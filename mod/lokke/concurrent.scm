;;; Copyright (C) 2019-2021 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

;; This module must not depend on (lokke collection) because because
;; it depends on (lokke base syntax) which depends on (lokke base
;; destructure) which depends on hash-map which depends on this.

(define-module (lokke concurrent)
  #:use-module ((ice-9 atomic)
                #:select (atomic-box-ref atomic-box-set! make-atomic-box))
  #:use-module ((ice-9 match) #:select (match-let*))
  #:use-module ((ice-9 threads) #:select (begin-thread join-thread))
  #:use-module ((oop goops) #:hide (<promise>))
  #:use-module ((lokke metadata) #:select (alter-meta! meta))
  #:use-module ((lokke scm atom)
                #:select (atom?
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
  #:use-module ((lokke scm promise)
                #:select (promise
                          promise-deliver
                          promise-deref))
  #:use-module ((lokke scm vector) #:select (lokke-vector))
  #:use-module ((lokke time) #:select (normalize-ts))
  #:use-module ((srfi srfi-11) #:select (let-values))
  #:export (<atom>
            <promise>
            alter-meta!
            add-watch
            compare-and-set!
            remove-watch
            deref
            future
            future-call
            reset!
            reset-vals!
            set-validator!
            swap!
            swap-vals!)
  #:re-export (atom atom? promise (promise-deliver . deliver))
  #:duplicates (merge-generics replace warn-override-core warn last))

(define-generic deref)

;; GOOPS doesn't define <variable>
(define var-class (class-of (module-variable (current-module) 'define)))

(define-method (deref (x var-class)) (variable-ref x))

;; For now, this <atom> is a class, while (lokke scm atom) <atom> is a record.
(define <atom> (class-of (atom #t)))

(define-method (deref (a <atom>)) (atom-deref a))
(define-method (reset! (a <atom>) newval) (atom-reset! a newval))
(define-method (swap! (a <atom>) . args) (apply atom-swap! a args))

(define-method (reset-vals! (a <atom>) newval)
  (let-values (((prev new) (atom-reset-vals! a newval)))
    (lokke-vector prev new)))

(define-method (swap-vals! (a <atom>) . args)
  (let-values (((prev new) (apply atom-swap-vals! a args)))
    (lokke-vector prev new)))

(define-method (compare-and-set! (a <atom>) oldval newval)
  (atom-compare-and-set! a oldval newval))

(define-method (add-watch (a <atom>) key fn) (atom-add-watch a key fn))
(define-method (remove-watch (a <atom>) key) (atom-remove-watch a key))
(define-method (set-validator! (a <atom>) validate)
  (atom-set-validator! a validate))

(define-method (meta (a <atom>)) (atom-meta a))
(define-method (alter-meta! (a <atom>) f . args)
  (apply atom-alter-meta! a f args))


(define-class <lokke-future> ()
  ;; An atomic-box containing either the thread, or (list result).
  (state #:init-keyword #:state))

(define* (deref-future f #:optional timeout-ms timeout-val)
  (let* ((box (slot-ref f 'state))
         (s (atomic-box-ref box)))
    (if (pair? s)
        (car s)
        (let ((r (if (not timeout-ms)
                     (join-thread s)
                     (join-thread s (match-let* (((sec . usec) (gettimeofday)))
                                      (normalize-ts sec (+ usec (* timeout-ms 1000))))
                                  timeout-val))))
          (begin
            (atomic-box-set! box (list r))
            r)))))

(define-method (deref (f <lokke-future>)) (deref-future f))
(define-method (deref (f <lokke-future>) timeout-ms timeout-val)
  (deref-future f timeout-ms timeout-val))

(define (future-call f)
  (make <lokke-future>
    ;; Provide our version of binding conveyance by transferring the state
    #:state (let ((bindings (current-dynamic-state)))
              (make-atomic-box (begin-thread (with-dynamic-state bindings f))))))

(define-syntax-rule (future exp ...)
  (future-call (lambda () exp ...)))


(define <promise> (class-of (promise)))

(define-method (deref (x <promise>))
  (promise-deref x))

(define-method (deref (x <promise>) timeout-ms timeout-val)
  (promise-deref x timeout-ms timeout-val))
