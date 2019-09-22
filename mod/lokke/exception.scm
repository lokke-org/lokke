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
  use-module: ((guile) hide: (catch))  ;; to work as literal, can't be defined
  use-module: ((guile) select: ((throw . %scm-throw)))
  use-module: ((lokke base map) select: (map?))
  use-module: (oop goops)
  replace: (throw)
  export: (ex-data ex-info try)
  duplicates: (merge-generics replace warn-override-core warn last)
  pure:)

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

(define ex-info-tag (make-symbol "exception-catch-tag"))

(define (throw ex)
  (validate-arg 'ex-info (lambda (x) (is-a? x <exception-info>))
                "ExceptionInfo" ex)
  (%scm-throw ex-info-tag ex))

(define-syntax try
  (syntax-rules (ExceptionInfo catch finally)
    ;; Until we decide the semantics, e.g. for exceptions thrown from
    ;; within finally expressions, reject them.
    ((_ expr ... (finally finally-expr ...))
     (syntax-error "finally clauses are not supported yet"))
    ((_ expr ... (catch anything ...) (finally finally-expr ...))
     (syntax-error "finally clauses are not supported yet"))
    ((_ expr ... (catch ExceptionInfo ex catch-expr ...))
     ((@ (guile) catch)
       ex-info-tag
       (lambda () #nil expr ...)
       (lambda (key ex) #nil catch-expr ...)))
    ((_ expr ...)
     (begin #nil expr ...))))
