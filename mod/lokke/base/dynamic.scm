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

;; This is in a separate file for now so that pr can depend on it
;; without needing a full dependency on (lokke base syntax).

(define-module (lokke base dynamic)
  #:use-module ((ice-9 match) #:select (match-lambda))
  #:use-module ((lokke base util) #:select (pairify vec-tag?))
  #:export (binding defdyn defdynloc))


(define (secret-dynfluid-name context syn)
  (datum->syntax
   context
   (string->symbol (string-append "/lokke/dynamic-"
                                  (symbol->string (syntax->datum syn))))))

(define-syntax defdyn
  (lambda (x)
    (syntax-case x ()
      ((_ name value)
       (let ((fluid (secret-dynfluid-name x #'name)))
         #`(begin
             (define #,fluid (make-fluid value))
             (define-syntax name (identifier-syntax (fluid-ref #,fluid)))))))))

(define-syntax defdynloc
  (lambda (x)
    (syntax-case x ()
      ((_ name value)
       (let ((fluid (secret-dynfluid-name x #'name)))
         #`(begin
             (define #,fluid (make-thread-local-fluid value))
             (define-syntax name (identifier-syntax (fluid-ref #,fluid)))))))))

(define-syntax binding
  (lambda (x)
    (syntax-case x ()
      ((_ (vec-tag rest ...) body ...) (vec-tag? #'vec-tag)
       #'(binding (rest ...) body ...))
      ((_ () body ...) #'(begin #nil body ...))
      ((_ (name init rest ...) body ...)
       (let* ((bindings (map (match-lambda
                               ((n i) (list (secret-dynfluid-name x n) i)))
                             (pairify #'(name init rest ...)))))
         #`(with-fluids #,bindings
             body ...))))))
