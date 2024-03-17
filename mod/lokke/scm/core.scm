;;; Copyright (C) 2015-2020 2022 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

(define-module (lokke scm core)
  #:use-module ((lokke base syntax)
                #:select (-> ->> cond-> cond->> some-> some->>))
  #:use-module ((lokke base util) #:select (string->keyword))
  #:use-module ((rnrs arithmetic fixnums) #:version (6) #:select (fixnum?))
  #:use-module ((srfi srfi-1) #:select (fold reduce first second third))
  #:use-module ((srfi srfi-28) #:select (format))
  #:re-export ((+ . +')
               (- . -')
               ->
               ->>
               (1+ . inc')
               (1+ . inc)
               (1- . dec')
               (1- . dec)
               cond->
               cond->>
               (exact->inexact . double)
               (exact->inexact . float)
               (eq? . identical?)
               (fixnum? . int?)
               (modulo . mod)
               (negative? . neg?)
               (positive? . pos?)
               (quotient . quot)
               (real? . double?)
               (real? . float?)
               rational?
               (remainder . rem)
               some->
               some->>
               (string->keyword . keyword) ; FIXME: restrictions?
               (string->symbol . symbol)   ; FIXME: restrictions?
               (substring/read-only . subs))
  #:export (comment
            doto
            true? false?
            comp complement constantly
            nat-int?
            neg-int?
            partial
            pos-int?
            rand rand-int
            ratio?
            str))

;; Are "Identifier Macros" felevant for any of our direct mappings?

(define-syntax comment
  (syntax-rules ()
    ((comment) #nil)
    ((comment body ...) #nil)))

(define (partial f . args)
  (cond
   ((null? args)
    (lambda later-args (apply f later-args)))
   ((null? (cdr args))
    (lambda later-args (apply f (car args) later-args)))
   ((null? (cddr args))
    (let ((x (first args)) (y (second args)))
      (lambda later-args (apply f x y later-args))))
   ((null? (cdddr args))
    (let ((x (first args)) (y (second args)) (z (third args)))
      (lambda later-args (apply f x y z later-args))))
   (else
    (lambda later-args (apply f (append args later-args))))))

(define (comp . funcs)
  (cond
   ((null? funcs) identity)
   ((null? (cdr funcs))
    (lambda later-args (apply (car funcs) later-args)))
   ((null? (cddr funcs))
    (let ((f (first funcs)) (g (second funcs)))
      (lambda later-args (f (apply g later-args)))))
   ((null? (cdddr funcs))
    (let ((f (first funcs)) (g (second funcs)) (h (third funcs)))
      (lambda later-args (f (g (apply h later-args))))))
   (else
    (lambda later-args
      (let ((funcs (reverse! funcs)))
        (fold (lambda (f result) (f result))
              (apply (car funcs) later-args)
              (cdr funcs)))))))

(define (complement f)
  (lambda args (not (apply f args))))

(define (true? x) (eq? #t x))
(define (false? x) (eq? #f x))

(define-syntax doto-body
  (syntax-rules ()
    ((doto-body x) x)
    ((doto-body x ()) (begin (x) x))
    ((doto-body x (y args ...)) (begin (y x args ...) x))
    ((doto-body x y) (begin (y x) x))
    ((doto-body x y rest ...) (begin (doto-body x y) (doto-body x rest ...)))))

(define-syntax doto
  (syntax-rules ()
    ((doto x) x)
    ((doto x y ...)
     (let ((result x))
       (doto-body result y ...)
       result))))

(define (constantly x)
  (lambda args x))

(define* (rand #:optional (n 1))
  (random n))

(define (rand-int n)
  (random (truncate n)))

(define (str . items)
  (string-concatenate (map (lambda (x) (format "~a" x)) items)))

(define (ratio? x)
  (and (not (exact-integer? x)) (rational? x) (exact? x)))

(define (pos-int? x) (and (fixnum? x) (positive? x)))
(define (neg-int? x) (and (fixnum? x) (negative? x)))
(define (nat-int? x) (and (fixnum? x) (not (negative? x))))
