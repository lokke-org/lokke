;;; Copyright (C) 2015-2019 Rob Browning <rlb@defaultvalue.org>
;;;
;;; This project is free software; you can redistribute it and/or
;;; modify it under the terms of (at your option) either of the
;;; following two licences:
;;;
;;;   1) The GNU Lesser General Public License as published by the
;;;      Free Software Foundation; either version 2.1, or (at your
;;;      option) any later version.
;;;
;;;   2) The Eclipse Public License; either version 1.0 or (at your
;;;      option) any later version.

(define-module (lokke scm core)
  #:use-module ((lokke base syntax) #:select (-> ->>))
  #:use-module ((srfi srfi-1) #:select (fold reduce first second third))
  #:use-module ((srfi srfi-88) #:select (string->keyword))
  #:re-export ((+ . +')
               (- . -')
               ->
               ->>
               (1+ . inc')
               (1+ . inc)
               (1- . dec')
               (1- . dec)
               (eq? . identical?)
               (modulo . mod)
               (negative? . neg?)
               (positive? . pos?)
               (quotient . quot)
               (real? . float?)
               (string->keyword . keyword) ; FIXME: restrictions?
               (string->symbol . symbol)   ; FIXME: restrictions?
               (substring/read-only . subs))
  #:export (comment
            doto
            true? false?
            comp complement constantly juxt partial
            rand rand-int
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
  (string-concatenate (map (lambda (x) (format #f "~a" x)) items)))
