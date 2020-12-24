;;; Copyright (C) 2020 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

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
