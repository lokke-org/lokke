;;; Copyright (C) 2019 Rob Browning <rlb@defaultvalue.org>
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

(define-module (lokke base util)
  use-module: ((srfi srfi-1) select: (take))
  export: (pairify vec-tag?))

;; We use this as a syntax-pattern fender to detect reader-vectors.
(define (vec-tag? x) (eq? '/lokke/reader-vector (syntax->datum x)))

(define (pairify lst)
  ;; (a b c d) -> ((a b) (c d) ...)
  (let loop ((rest lst))
    (cond
     ((null? rest) '())
     ((null? (cdr rest)) (error "Can't pairify odd length list" lst))
     (else (cons (take rest 2) (loop (cddr rest)))))))
