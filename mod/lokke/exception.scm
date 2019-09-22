;;; Copyright (C) 2019 Rob Browning <rlb@defaultvalue.org>
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

(read-set! keywords 'postfix)  ;; srfi-88

(define-module (lokke exception)
  version: (0 0 0)
  use-module: ((lokke base map) select: (map?))
  use-module: (oop goops)
  export: (ex-data ex-info str)
  duplicates: (merge-generics replace warn-override-core warn last))

(define-syntax-rule (validate-arg fn-name pred expected val)
  (unless (pred val)
    (scm-error 'wrong-type-arg fn-name
               "~A argument is not a ~A: ~A"
               (list 'val expected val) (list val))))

(define-class <exception-info> ()
  (msg init-keyword: msg:)
  (map init-keyword: map:)
  (cause init-keyword: cause: init-value: #nil))

(define* (ex-info msg map optional: cause)
  (validate-arg 'ex-info string? "string" msg)
  (validate-arg 'ex-info map? "map" map)
  (validate-arg 'ex-info
                (lambda (x) (or (not x) (is-a? x <exception-info>)))
                "ExceptionInfo" cause)
  (make <exception-info> msg: msg map: map cause: cause))

(define (ex-data ex)
  (validate-arg 'ex-info (lambda (x) (is-a? x <exception-info>))
                "ExceptionInfo" ex)
  (slot-ref ex 'map))
