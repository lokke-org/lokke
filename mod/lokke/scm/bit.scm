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

(define-module (lokke scm bit)
  #:use-module ((srfi srfi-1) #:select (reduce))
  #:use-module ((srfi srfi-60) #:select (copy-bit))
  #:re-export ((lognot . bit-not))
  #:export (bit-and
            bit-clear
            bit-flip
            bit-not
            bit-or
            bit-set
            bit-test
            bit-xor))

(define-syntax-rule (require-at-least-two-args name args)
  (when (or (null? args) (null? (cdr args)))
    (scm-error 'wrong-number-of-args name
               "Two or more arguments required: ~S" (list args) #f)))

(define (bit-and . integers)
  (require-at-least-two-args "bit-and" integers)
  (reduce logand #f integers))

(define (bit-or . integers)
  (require-at-least-two-args "bit-or" integers)
  (reduce logior #f integers))

(define (bit-xor . integers)
  (require-at-least-two-args "bit-xor" integers)
  (reduce logxor #f integers))

(define (bit-flip x n)
  ;; The result for a negative n is undefined.
  (logxor x (ash 1 n)))

(define (bit-set x n)
  ;; The result for a negative n is undefined.
  (copy-bit n x #t))

(define (bit-clear x n)
  ;; The result for a negative n is undefined.
  (copy-bit n x #f))

(define (bit-test x n)
  ;; The result for a negative n is undefined.
  (logbit? n x))

;; FIXME: bit-shift-right
;; FIXME: unsigned-bit-shift-right
;; FIXME: bit-shift-left
;; FIXME: bit-and-not
