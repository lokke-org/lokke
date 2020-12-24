;;; Copyright (C) 2019-2020 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

;; This is in a separate file for now so that pr can depend on it
;; without needing a full dependency on (lokke base syntax).

(define-module (lokke base dynamic)
  #:use-module ((guile) #:select ((set! . %set!)))
  #:use-module ((ice-9 match) #:select (match))
  #:use-module ((lokke base util) #:select (module-name->ns-str vec-tag?))
  #:use-module ((system syntax) #:select (syntax-module))
  #:export (binding defdyn defdynloc)
  #:replace (set!))


(define (secret-dynfluid-name context syn)
  (datum->syntax
   context
   (string->symbol (string-append "/lokke/dynamic-"
                                  (symbol->string (syntax->datum syn))))))

;; Stores (module-name name fluid-name fluid)
(define dynamic-fluid (make-object-property))

(define (remember-fluid! dyn-name fluid-name fluid)
  (let* ((mod (current-module))
         (var (module-variable mod dyn-name)))
    (%set! (dynamic-fluid var)
      (list (module-name mod) dyn-name fluid-name fluid))))

(define-syntax defdyn
  (lambda (x)
    (syntax-case x ()
      ((_ name value)
       (let ((fluid (secret-dynfluid-name x #'name)))
         #`(eval-when (expand load eval)
             (define #,fluid (make-fluid value))
             (define-syntax name (identifier-syntax (fluid-ref #,fluid)))
             (remember-fluid! 'name '#,fluid #,fluid)
             (export name)))))))

(define-syntax defdynloc
  (lambda (x)
    (syntax-case x ()
      ((_ name value)
       (let ((fluid (secret-dynfluid-name x #'name)))
         #`(eval-when (expand load eval)
             (define #,fluid (make-thread-local-fluid value))
             (define-syntax name (identifier-syntax (fluid-ref #,fluid)))
             (remember-fluid! 'name '#,fluid #,fluid)
             (export name)))))))

(define-syntax binding
  (lambda (x)
    (syntax-case x ()
      ((_ (vec-tag meta rest ...) body ...) (vec-tag? #'vec-tag)
       #'(binding (rest ...) body ...))
      ((_ () body ...) #'(begin #nil body ...))
      ((_ (name init rest ...) body ...)
       (let* ((mod (current-module))
              (name-sym (syntax->datum #'name))
              (var (module-variable mod name-sym))
              (info (dynamic-fluid var)))
         (unless info
           (error
            (format #f
                    "binding: unable to resolve dynamic variable ~a in ns ~a"
                    name-sym
                    (module-name->ns-str (module-name mod)))))
         (match info
           ((mod name fluid-name fluid)
            (with-syntax ((mod (datum->syntax x mod))
                          (fluid-name (datum->syntax x fluid-name)))
              #`(with-fluid*
                    (@@ mod fluid-name) init
                    (lambda () (binding (rest ...) body ...)))))))))))

(define-syntax set!
  ;; FIXME: duplication wrt binding
  (lambda (x)
    (syntax-case x ()
      ((_ name val)
       (let* ((mod (current-module))
              (name-sym (syntax->datum #'name))
              (var (module-variable mod name-sym))
              (info (dynamic-fluid var)))
         (unless info
           (error
            (format #f
                    "binding: unable to resolve dynamic variable ~a in ns ~a"
                    name-sym
                    (module-name->ns-str (module-name mod)))))
         (match info
           ((mod name fluid-name fluid)
            (with-syntax ((mod (datum->syntax x mod))
                          (fluid-name (datum->syntax x fluid-name)))
              #`(fluid-set! (@@ mod fluid-name) val)))))))))
