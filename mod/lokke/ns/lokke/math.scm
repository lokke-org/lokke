;;; Copyright (C) 2020 Rob Browning <rlb@defaultvalue.org>
;;;
;;; This project is free software; you can redistribute it and/or modify
;;; it under the terms of (at your option) either of the following two
;;; licences:
;;;
;;;   1) The GNU Lesser General Public License as published by the Free
;;;      Software Foundation; either version 2.1, or (at your option) any
;;;      later version
;;;
;;;   2) The Eclipse Public License; either version 1.0 or (at your
;;;      option) any later version.

;; FIXME: check semantics against the jvm, and add tests

(define-module (lokke ns lokke math)
  #:version (0 0 0)
  ;;#:use-module (oop goops)
  #:duplicates (merge-generics replace warn-override-core warn last)
  #:export (E
            PI
            atan
            (atan . atan2)
            (round->integer . round)
            signum)
  #:re-export (abs
               acos
               asin
               (ceiling . ceil)
               cos
               cosh
               exp
               floor
               log
               log10
               (random:uniform . random)
               (round . rint)
               sin
               sinh
               sqrt
               tan
               tanh)
  #:replace (atan))

(define E (exp 1))
(define PI 3.14159265358979)

(define (round->integer x)
  (inexact->exact (round x)))

(define (signum x)
  (cond
   ((positive? x) 1.0)
   ((negative? x) -1.0)
   (else 0.0)))
