;;; Copyright (C) 2019-2020 Rob Browning <rlb@defaultvalue.org>
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

;; For now, we do things in a bit more primitive way than we otherwise
;; might, in order to limit the dependencies of these potentially
;; lower level error handling mechanisms.  We can always lighten up
;; later if that turns out to be fine.

(define-module (lokke exception)
  #:version (0 0 0)
  #:use-module ((guile) #:hide (catch)) ;; to work as literal, can't be defined
  #:use-module ((guile) #:select ((catch . %scm-catch) (throw . %scm-throw)))
  #:use-module ((lokke base map) #:select (map?))
  #:use-module ((lokke base util) #:select (vec-tag?))
  #:use-module ((lokke base syntax) #:select (let))
  #:use-module ((lokke scm vector)
                #:select (lokke-vector?
                          lokke-vector
                          lokke-vector-conj
                          lokke-vector-length
                          lokke-vector-ref))
  #:use-module (oop goops)
  #:use-module ((srfi srfi-1) :select (drop-while find first second))
  #:replace (close throw)
  #:export (Error
            Exception
            ExceptionInfo
            Throwable
            ex-cause
            ex-data
            ex-info
            ex-info?
            ex-message
            ex-suppressed
            ex-tag
            try
            with-final
            with-open)
  #:duplicates (merge-generics replace warn-override-core warn last)
  #:pure)

;; Currently we rely on Guile's more efficient "single direction",
;; catch/throw mechanism.  i.e. you can only throw "up" the stack.

;; FIXME: do we want/need any with-continuation-barrier wrappers,
;; and/or does catch/throw already provide a barrier?

(define-syntax-rule (validate-arg fn-name pred expected val)
  (unless (pred val)
    (scm-error 'wrong-type-arg fn-name
               "~A argument is not a ~A: ~A"
               (list 'val expected val) (list val))))

;; The content of Guile catch/throw exceptions is just the argument
;; list passed to throw, where the first argument must be a symbol
;; that a catch can be watching for.  So at the moment, ex-info
;; exceptions are just that list of arguments, and the exceptions
;; caught by Lokke's catch are also those Scheme argument lists,
;; i.e. (tag . args).

;; Provide *very* basic compatibility.  For the moment, there's no
;; extensibility, and if we add any, we might only do it for guile 3+
;; where we're likely to reimplement the handling in terms of
;; exception objects.

(define Throwable (make-symbol "lokke-throwable-catch-tag"))
(define Error (make-symbol "lokke-error-catch-tag"))
(define Exception (make-symbol "lokke-exception-catch-tag"))
(define ExceptionInfo (make-symbol "lokke-exception-info-catch-tag"))

;; This will of course be different when we support (or just switch
;; to) guile 3+ exception objects.  This is also very experiemntal,
;; i.e. not sure we'll want to preserve exactly this kind of interop.
;; The tentative plan is to switch to raise-exception,
;; with-exception-handler, and exception objects, and then Error might
;; map to &error, etc.

(define-method (ex-instance? kind ex)
  (validate-arg 'ex-instance? symbol? "kind" kind)
  (validate-arg 'ex-instance? maybe-exception? "ex" ex)
  (let* ((ex-kind (car ex)))
    (or
     (eq? kind ex-kind)
     (eq? kind Throwable)
     (cond
      ((eq? kind Exception) (or (eq? Exception ex-kind)
                                (eq? ExceptionInfo ex-kind)))
      ((eq? kind ExceptionInfo) (eq? ExceptionInfo ex-kind))
      ;; Based on information in (ice-9 exceptions)
      ((eq? kind Error) (memq ex-kind '(Error
                                        getaddrinfo-error
                                        goops-error
                                        host-not-found
                                        keyword-argument-error
                                        memory-allocation-error
                                        no-data
                                        no-recovery
                                        null-pointer-error
                                        numerical-overflow
                                        out-of-range
                                        program-error
                                        read-error
                                        regular-expression-syntax
                                        stack-overflow
                                        syntax-error
                                        system-error
                                        try-again
                                        unbound-variable
                                        wrong-number-of-args
                                        wrong-type-arg)))
      (else #f)))))

;; Exactly the args passed to throw
(define (ex-info? x)
  (and (pair? x) (= 5 (length x)) (eq? ExceptionInfo (car x))))

(define (maybe-exception? x)
  (and (pair? x) (symbol? (first x))))

(define* (ex-info msg map #:key (cause #nil) (suppressed #nil))
  (validate-arg 'ex-info string? "string" msg)
  (validate-arg 'ex-info map? "map" map)
  (validate-arg 'ex-info (lambda (x) (or (not x) (maybe-exception? x)))
                "ex-info" cause)
  (when suppressed
    (validate-arg 'ex-info (lambda (x) (lokke-vector? x)) "vector" suppressed)
    (let* ((n (lokke-vector-length suppressed)))
      (do ((i 0 (1+ i)))
          ((= i n))
        (let* ((x (lokke-vector-ref suppressed i)))
          (unless (maybe-exception? x)
            (scm-error 'wrong-type-arg 'ex-info
                       "suppressed item is not an exception: ~A"
                       (list x) (x)))))))
  (list ExceptionInfo msg map cause suppressed))

(define (ex-tag ex)
  (validate-arg 'ex-tag maybe-exception? "exception" ex)
  (list-ref ex 0))

(define (ex-message ex)
  (validate-arg 'ex-message ex-info? "ex-info" ex)
  (list-ref ex 1))

(define (ex-data ex)
  (validate-arg 'ex-data ex-info? "ex-info" ex)
  (list-ref ex 2))

(define (ex-cause ex)
  (validate-arg 'ex-cause ex-info? "ex-info" ex)
  (list-ref ex 3))

(define (ex-suppressed ex)
  (validate-arg 'ex-suppressed ex-info? "ex-info" ex)
  (list-ref ex 4))

(define (add-suppressed ex suppressed-ex)
  (validate-arg 'add-suppressed ex-info? "ex-info" ex)
  (validate-arg 'add-suppressed (lambda (x) (maybe-exception? x))
                "exception" suppressed-ex)
  (ex-info (ex-message ex)
           (ex-data ex)
           #:cause (ex-cause ex)
           #:suppressed (lokke-vector-conj (or (list-ref ex 4) (lokke-vector))
                                           suppressed-ex)))

(define (throw ex)
  (validate-arg 'throw (lambda (x) (maybe-exception? ex)) "exception" ex)
  (apply %scm-throw ex))

(define (call-with-exception-suppression ex thunk)
  (%scm-catch
   #t
   thunk
   (lambda suppressed
     ;; FIXME: can only add suppressed to ex-info exceptions...
     (if (ex-info? ex)
         (apply %scm-throw (add-suppressed ex suppressed))
         ;; Match the JVM for now -- until/unless we figure out
         ;; a way to handle suppressed exceptions universally.
         (apply %scm-throw suppressed)))))

;; Wonder about allowing an exception arg for a custom finally clause,
;; say (finally* ex ...), which would be nil unless an exception was
;; pending, so that it's possible to know when an exception is
;; pending.  If an exception were thrown from finally* it would still
;; be added to the original exception as a suppressed exception.

(define-syntax try
  ;; Arranges to catch any exceptions thrown by the body if the
  ;; exception's tag (as per Guile's catch/throw) matches the catch
  ;; clause's tag.  Suppresses any exceptions thrown by code in a
  ;; finally expression by calling add-suppressed on the pending
  ;; exception.  At the moment, only ex-info exceptions can carry
  ;; suppressed exceptions, so suppressed exceptions will be lost if
  ;; the pending exception isn't an ex-info exception.
  (lambda (x)

    (define (has-finally-clauses? exp)
      (find (lambda (x) (and (pair? x) (eq? 'finally (car x))))
            exp))

    (define (body-clauses syn)
      (syntax-case syn (catch)
        ((exp* ...) (has-finally-clauses? (syntax->datum #'(exp* ...)))
         (scm-error 'syntax-error 'try "finally clause must be last and unique in ~s"
                    (list (syntax->datum x)) #f))
        (((catch catch-exp* ...) exp* ...) '())
        ((exp exp* ...) #`(exp #,@(body-clauses #'(exp* ...))))
        (() '())))

    (define (drop-body syn)
      (drop-while (lambda (x)
                    (let* ((x (syntax->datum x)))
                      (or (not (list? x))
                          (not (eq? 'catch (car x))))))
                  syn))

    (define (catch-clauses caught syn)
      (syntax-case (drop-body syn) (catch)
        ((exp* ...) (has-finally-clauses? (syntax->datum #'(exp* ...)))
         (scm-error 'syntax-error 'try "finally clause must be last and unique in ~s"
                    (list (syntax->datum x)) #f))
        (((catch what ex catch-exp* ...) exp* ...)
         #`(((ex-instance? what #,caught)
             (let* ((ex #,caught))
               #nil
               catch-exp* ...))
            #,@(catch-clauses caught #'(exp* ...))))
        ((exp exp* ...)
         (scm-error 'syntax-error 'try
                    "expression ~s after first catch expression is not a catch or finally in ~s"
                    (list (syntax->datum #'exp) (syntax->datum x)) #f))
        ((exp exp* ...) #`(exp #,@(body-clauses #'(exp* ...))))
        (() '())))

    (syntax-case x (catch finally)
      ((_ exp* ... (finally finally-exp-1 ...) (finally finally-exp-2 ...))
       (scm-error 'syntax-error 'try "finally clause must be last and unique in ~s"
                  (list (syntax->datum x)) #f))

      ((_ exp* ... (finally finally-exp* ...))
       (let* ((caught (car (generate-temporaries '(#t))))
              (body (body-clauses #'(exp* ...)))
              (catches (catch-clauses caught #'(exp* ...))))
         #`(%scm-catch
            #t
            (lambda ()
              (let* ((result (%scm-catch
                              #t
                              (lambda () #nil #,@body)
                              ;; FIXME: compiler smart enough to elide?
                              (lambda #,caught
                                (cond
                                 #,@catches
                                 (else (apply %scm-throw #,caught)))))))
                finally-exp* ...
                result))
            (lambda ex
              (call-with-exception-suppression
               ex
               (lambda ()
                 finally-exp* ...
                 (apply %scm-throw ex)))))))

      ;; Assume no finally clause
      ((_ exp* ...)
       (let* ((caught (car (generate-temporaries '(#t))))
              (body (body-clauses #'(exp* ...)))
              (catches (catch-clauses caught #'(exp* ...))))
         #`(%scm-catch
            #t
            (lambda () #nil #,@body)
            (lambda #,caught
              (cond
               #,@catches
               (else (apply %scm-throw #,caught))))))))))

;; Because otherwise the replace: prevents it from being visible and
;; folded into the generic as the base case.
(define close (@ (guile) close))

;; Must import this close if you want to adjust it.
;; FIXME: do it and/or with-open belong in this module?
(define-generic close)

(define-syntax with-open
  ;; Bindings must be a vector of [name init ...] pairs.  Binds each
  ;; name to the value of the corresponding init, and behaves exactly
  ;; as if each subsequent name were guarded by a nested try form that
  ;; calls (close name) in its finally clause.  Suppresses any
  ;; exceptions thrown by the close calls by calling add-suppressed on
  ;; the pending exception.  At the moment, only ex-info exceptions
  ;; can carry suppressed exceptions, so suppressed exceptions will be
  ;; lost if the pending exception isn't an ex-info exception.
  ;;
  ;; Actually accepts either a list or lokke-vector of bindings.
  (lambda (x)
    (syntax-case x ()
      ((_ (vec-tag meta binding ...) body ...)  (vec-tag? #'vec-tag)
       #'(with-open (binding ...) body ...))
      ((_ (resource value binding ...) body ...)
       #'(let (resource value)
           (try
             (with-open (binding ...) body ...)
             (finally (close resource)))))
      ((_ () body ...)
       #'(begin body ...)))))

(define-syntax with-final
  ;; The bindings must be a vector of elements, each of which is
  ;; either "name init", "name init :always action", or "name init
  ;; :error action".  Binds each name to the value of the
  ;; corresponding init, and behaves exactly as if each subsequent
  ;; name were guarded by a nested try form that calls (action name)
  ;; in its finally clause when :always is specified, or (action name)
  ;; only on exception when :error is specified.  Suppresses any
  ;; exceptions thrown by the action calls by calling add-suppressed
  ;; on the pending exception.  At the moment, only ex-info exceptions
  ;; can carry suppressed exceptions, so suppressed exceptions will be
  ;; lost if the pending exception isn't an ex-info exception.
  ;;
  ;; Actually accepts either a list or lokke-vector of bindings.
  (lambda (x)
    (syntax-case x ()
      ((_ (vec-tag meta binding ...) body ...)  (vec-tag? #'vec-tag)
       #'(with-final (binding ...) body ...))
      ((_ (resource value #:always action binding ...) body ...)
       #'(let (resource value)
           (try
            (with-final (binding ...) body ...)
            (finally (action resource)))))
      ((_ (resource value #:error action binding ...) body ...)
       #'(let (resource value)
           (%scm-catch
            #t
            (lambda () (with-final (binding ...) body ...))
            (lambda ex
              (call-with-exception-suppression
               ex
               (lambda () (action resource)))))))
      ((_ () body ...)
       #'(begin body ...)))))
