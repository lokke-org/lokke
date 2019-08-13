;;; Copyright (C) 2015-2019 Rob Browning <rlb@defaultvalue.org>
;;;
;;; This project is free software; you can redistribute it and/or
;;; modify it under the terms of (at your option) either of the
;;; following two licences:
;;;
;;;   1) The GNU Lesser General Public License as published by the
;;;      Free Software Foundation; either version 2.1, or (at your
;;;      option) any later version
;;;
;;;   2) The Eclipse Public License; either version 1.0 or (at your
;;;      option) any later version.

(read-set! keywords 'postfix)  ;; srfi-88

(define-module (lokke scm atom)
  use-module: ((ice-9 atomic)
               select: (atomic-box?
                        atomic-box-compare-and-swap!
                        atomic-box-ref
                        make-atomic-box))
  use-module: ((srfi srfi-11) select: (let-values))
  use-module: ((lokke hash-map)
               select: (assoc contains? dissoc hash-map reduce-kv))
  export: (atom?
           atom
           atom-add-watch
           atom-compare-and-set!
           atom-deref
           atom-remove-watch
           atom-reset!
           atom-set-validator!
           atom-swap!))

;; Tried to keep this lightweight, i.e. just an atomic-box, but if we
;; decide we want to have metadata here too (intead of handling it in
;; a wrapper), might be time to switch to a record or object, in which
;; case the watchers may not need to be coordinated with the
;; value/validator.

;; Current approach does end up allocating an extra pair for every
;; update, and for some failed attempts.

;; FIXME: do we want any/all the yields?
;; FIXME: is it acceptable for watchers to run in the current thread for now?
;; FIXME: what should happen on watcher exception etc.?

(define (atom? x) (atomic-box? x))

;; Must never modify the state, always make a new one.
(define-syntax-rule (atom-state atom) (atomic-box-ref atom))
(define-syntax-rule (state-val state) (car state))
(define-syntax-rule (state-validator state)
  (if (pair? (cdr state)) (cadr state) (cdr state)))
(define-syntax-rule (state-watchers state)
  (and (pair? (cdr state))
       (cddr state)))

(define (make-state val validator watchers)
  (cons val (if watchers
                (cons validator watchers)
                validator)))

(define* (atom value optional: validator)
  (make-atomic-box (make-state value validator #f)))

(define (atom-deref atom) (state-val (atomic-box-ref atom)))

(define (atom-add-watch atom key f)
  ;; Don't worry about garbage generation; assume this rarely races.
  (let loop ((cur-state (atom-state atom)))
    (let* ((watchers (state-watchers cur-state))
           (new (make-state (state-val cur-state)
                            (state-validator cur-state)
                            (if watchers
                                (assoc watchers key f)
                                (hash-map key f)))))
      (unless (eq? cur-state (atomic-box-compare-and-swap! atom cur-state new))
        (yield)
        (loop (atom-state atom))))))

(define (atom-remove-watch atom key)
  ;; Don't worry about garbage generation; assume this rarely races.
  (let loop ((cur-state (atom-state atom)))
    (let ((watchers (state-watchers cur-state)))
      (when (and watchers (contains? watchers key))
        (let ((new (make-state (state-val cur-state)
                               (state-validator cur-state)
                               (dissoc watchers key))))
          (unless (eq? cur-state (atomic-box-compare-and-swap! atom cur-state new))
            (yield)
            (loop (atom-state atom))))))))

;; FIXME: is this the exception and error handling we want?
(define (validate-val atom validate value)
  (when validate
    (unless (catch #t
              (lambda () (validate value))
              (lambda args (throw 'invalid-ref-state atom value args)))
      (throw 'invalid-ref-state atom value))))

;; Double check behavior against docs...
(define (atom-set-validator! atom validator)
  ;; Don't worry about garbage generation; assume this rarely races.
  (let loop ((cur-state (atom-state atom)))
    (let ((old-validate (state-validator cur-state)))
      (or (eq? old-validate validator)
          (let ((val (state-val cur-state)))
            (validate-val atom validator val)
            (let ((new (make-state val validator (state-watchers cur-state))))
              (unless (eq? cur-state
                           (atomic-box-compare-and-swap! atom cur-state new))
                ;; (yield)?
                (loop (atom-state atom)))))))))

(define (notify-watchers atom oldval newval)
  (let ((watchers (state-watchers (atom-state atom))))
    (when watchers
      (reduce-kv (lambda (result key notice) (notice key atom oldval newval))
                 #f
                 watchers))))

(define (state-stale? existing newval validate watchers)
  (not (and existing
            (eq? newval (state-val existing))
            (eq? validate (state-validator existing))
            (eq? watchers (state-watchers existing)))))

(define (adjust-value! atom ignore-oldval? oldval get-newval)
  (let loop ((cur-state (atom-state atom))
             (new-state #f))
    (let ((val (state-val cur-state)))
      (if (not (or ignore-oldval? (eq? val oldval)))
          (values #f #f)
          (let ((newval (get-newval val)))
            (if (eq? val newval)
                (begin
                  (notify-watchers atom val newval)
                  (values #t newval))
                (let ((validate (state-validator cur-state)))
                  (validate-val atom validate newval)
                  (let* ((watchers (state-watchers cur-state))
                         ;; Generate less garbage during races.
                         (new (if (state-stale? new-state newval validate watchers)
                                  (make-state newval validate watchers)
                                  new-state))
                         (result (atomic-box-compare-and-swap! atom cur-state new)))
                    (if (eq? cur-state result)
                        (begin
                          (notify-watchers atom val newval)
                          (values #t newval))
                        ;; FIXME: is this acceptable?  Grey area?  Some
                        ;; component of state was changed.  For now,
                        ;; try again if anything but the value changed.
                        (if (or (not (eq? val (state-val result)))
                                (and (eq? validate (state-validator result))
                                     (eq? watchers (state-watchers result))))
                            (values #f #f)
                            (begin
                              ;; (yield)?
                              (loop (atom-state atom) new-state))))))))))))

(define (atom-compare-and-set! atom oldval newval)
  (let-values (((set? newval)
                (adjust-value! atom #f oldval (lambda (oldval) newval))))
    set?))

(define (atom-reset! atom newval)
  (let-values (((set? val)
                (adjust-value! atom #t #f (lambda (oldval) newval))))
    ;; For now, a sanity check
    (unless (eq? newval val)
      (error "Bug new value did not match expectation" val newval)))
  newval)

(define (atom-swap! atom f . args)
  (let-values (((set? val)
                (adjust-value! atom #t #f (lambda (oldval) (apply f oldval args)))))
    val))
