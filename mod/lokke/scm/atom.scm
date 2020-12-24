;;; Copyright (C) 2015-2020 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

(define-module (lokke scm atom)
  #:use-module ((ice-9 atomic)
                #:select (atomic-box?
                          atomic-box-compare-and-swap!
                          atomic-box-ref
                          make-atomic-box))
  #:use-module ((ice-9 threads) #:select (yield))
  #:use-module ((srfi srfi-9) #:select (define-record-type))
  #:use-module ((srfi srfi-11) #:select (let-values))
  #:use-module ((lokke hash-map)
                #:select (assoc contains? dissoc hash-map reduce-kv))
  #:export (<atom>
            atom?
            atom
            atom-add-watch
            atom-alter-meta!
            atom-compare-and-set!
            atom-deref
            atom-meta
            atom-remove-watch
            atom-reset!
            atom-reset-vals!
            atom-set-validator!
            atom-swap!
            atom-swap-vals!))

;; FIXME: is it acceptable for watchers to run in the current thread for now?
;; FIXME: what should happen on watcher exception etc.?

(define-record-type <atom>
  (%atom value infra)
  atom?
  (value %value)
  ;; A box containing a variable length vector of validator, watchers, meta.
  (infra %infra))

(define (atom-deref atom)
  (atomic-box-ref (%value atom)))

(define-inlinable (atom-infra atom)
  (atomic-box-ref (%infra atom)))

(define-inlinable (infra-validator infra)
  (and infra (> (vector-length infra) 0) (vector-ref infra 0)))

(define-inlinable (infra-watchers infra)
  (and infra (> (vector-length infra) 1) (vector-ref infra 1)))

(define-inlinable (infra-meta infra)
  (if (and infra (> (vector-length infra) 2))
      (vector-ref infra 2)
      #nil))

(define (make-infra validator watchers meta)
  (cond
   (meta (vector validator watchers meta))
   (watchers (vector validator watchers))
   (validator (vector validator))
   (else #nil)))

(define* (assoc-infra infra
                      #:key
                      (validator (infra-validator infra))
                      (watchers (infra-watchers infra))
                      (meta (infra-meta infra)))
  (make-infra validator watchers meta))

;; FIXME: is this the exception and error handling we want?
(define (validate-val validate value)
  (when validate
    (unless (validate value)
      (throw 'invalid-ref-state atom value))))

(define* (atom value #:key (validator #f) (meta #nil))
  (validate-val validator value)
  (%atom (make-atomic-box value)
         (make-atomic-box (make-infra validator #f meta))))

(define (atom-meta atom)
  (infra-meta (atom-infra atom)))

(define (atom-alter-meta! atom f . args)
  (let ((box (%infra atom)))
    (let loop ()
      (let* ((cur-infra (atomic-box-ref box))
             (meta (infra-meta cur-infra))
             (new-meta (apply f meta args))
             (new-infra (assoc-infra cur-infra #:meta new-meta)))
        (if (eq? cur-infra (atomic-box-compare-and-swap! box cur-infra new-infra))
            new-meta
            (loop))))))

(define (atom-add-watch atom key f)
  (let ((box (%infra atom)))
    (let loop ()
      (let* ((cur-infra (atom-infra atom))
             (watchers (infra-watchers cur-infra))
             (new-infra (assoc-infra cur-infra
                                     #:watchers (if watchers
                                                    (assoc watchers key f)
                                                    (hash-map key f)))))
        (unless (eq? cur-infra (atomic-box-compare-and-swap! box cur-infra new-infra))
          (loop))))))

(define (atom-remove-watch atom key)
  (let ((box (%infra atom)))
    (let loop ()
      (let* ((cur-infra (atom-infra atom))
             (watchers (infra-watchers cur-infra)))
        (when (and watchers (contains? watchers key))
          (let ((new-infra (assoc-infra cur-infra #:watchers (dissoc watchers key))))
            (unless (eq? cur-infra (atomic-box-compare-and-swap! box cur-infra new-infra))
              (loop))))))))

;; Double check behavior against docs...
(define (atom-set-validator! atom validator)
  (let ((box (%infra atom)))
    (let loop ()
      (let* ((cur-infra (atom-infra atom))
             (cur-validate (infra-validator cur-infra)))
        (or (eq? cur-validate validator)
            (begin
              (validate-val validator (atom-deref atom))
              (let ((new-infra (assoc-infra cur-infra #:validator validator)))
                (unless (eq? cur-infra
                             (atomic-box-compare-and-swap! box cur-infra new-infra))
                  (loop)))))))))

(define (notify-watchers atom oldval newval)
  (let ((watchers (infra-watchers (atom-infra atom))))
    (when watchers
      (reduce-kv (lambda (result key notice) (notice key atom oldval newval))
                 #f
                 watchers))))

(define (atom-compare-and-set! atom expected desired)
  (let* ((infra (atom-infra atom))
         (validate (infra-validator infra)))
    (validate-val validate desired)
    (let* ((watchers (infra-watchers infra))
           (result (atomic-box-compare-and-swap! (%value atom) expected desired)))
      (and (eq? result expected)
           (begin
             (notify-watchers atom expected desired)
             #t)))))

(define (atom-swap-vals! atom f . args)
  (let ((box (%value atom)))
    (let loop ()
      (let* ((infra (atom-infra atom))
             (prev (atomic-box-ref box))
             (new (apply f prev args)))
        (let ((validate (infra-validator infra)))
          (validate-val validate new)
          (let* ((watchers (infra-watchers infra))
                 (result (atomic-box-compare-and-swap! (%value atom) prev new)))
            (if (eq? prev result)
                (begin
                  (notify-watchers atom prev new)
                  (values prev new))
                (loop))))))))

(define (atom-swap! atom f . args)
  (let-values (((prev new) (apply atom-swap-vals! atom f args)))
    new))

(define (atom-reset-vals! atom newval)
  (atom-swap-vals! atom (lambda (prev) newval)))

(define (atom-reset! atom newval)
  (let-values (((prev new) (atom-swap-vals! atom (lambda (prev) newval))))
    new))

