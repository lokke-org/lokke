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
  #:use-module ((srfi srfi-1) :select (find first second))
  #:replace (close throw)
  #:export (ExceptionInfo
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

(define ExceptionInfo (make-symbol "lokke-exception-info-catch-tag"))

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
    (let (n (lokke-vector-length suppressed))
      (do ((i 0 (1+ i)))
          ((= i n))
        (let (x (lokke-vector-ref suppressed i))
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
    (define (has-finally-clauses? expr)
      (find (lambda (x) (and (pair? x) (eq? 'finally (car x))))
            expr))
    (syntax-case x (catch finally)
      ((_ expr ..
          (finally finally-expr-1 ...)
          (finally finally-expr-2 ...))
       (error "finally clause must be last and unique"))
      ((_ expr ...
          (finally finally-expr ...))
       #'(%scm-catch
          #t
          (lambda ()
            (let (result (try expr ...))
              finally-expr ...
              result))
          (lambda ex
            (call-with-exception-suppression
             ex
             (lambda ()
               finally-expr ...
               (apply %scm-throw ex))))))

      ;; Reverse the nesting so that tags are caught in code order
      ((_ expr ... (catch tag ex catch-expr ...))
       #'(%scm-catch
          tag
          (lambda () (try expr ...))
          (lambda ex catch-expr ...)))

      ((_ expr ...) (has-finally-clauses? (syntax->datum #'(expr ...)))
       (error "finally clause must be last and unique"))

      ((_ expr ...)
       #'(begin #nil expr ...)))))

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
