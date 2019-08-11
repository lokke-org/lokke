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

(define-module (lokke invoke)
  use-module: (oop goops)
  export: (invoke)
  duplicates: (merge-generics replace warn-override-core warn last))

(define-method (invoke f . args)
  (error (format #f "No invoke method for ~s:" (class-of f))
         f))

(define-method (invoke (f <procedure>) . args)
  (apply f args))

(define-method (invoke (f <generic>) . args)
  (apply f args))

;; FIXME: do we want this?
(define parameter-class (class-of (make-parameter #f)))
(define-method (invoke (f parameter-class) . args)
  (apply f args))
