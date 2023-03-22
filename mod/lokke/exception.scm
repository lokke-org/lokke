;;; Copyright (C) 2019-2020 2023 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

;; For now, we do things in a bit more primitive way than we otherwise
;; might, in order to limit the dependencies of these potentially
;; lower level error handling mechanisms.  We can always lighten up
;; later if that turns out to be fine.

(define-module (lokke exception)
  #:version (0 0 0)
  #:use-module ((ice-9 match) #:select (match-lambda*))
  #:use-module ((ice-9 exceptions)
                #:select (define-exception-type
                          exception-message
                          exception-with-message?
                          make-error
                          make-exception-with-irritants
                          make-exception-with-message))
  #:use-module ((lokke base map) #:select (map?))
  #:use-module ((lokke base util) #:select (vec-tag?))
  #:use-module ((lokke base syntax) #:select (defdyn let when-not))
  #:use-module ((lokke pr) #:select (pr-str))
  #:use-module ((lokke scm vector) #:select (lokke-vector lokke-vector-conj))
  #:use-module (oop goops)
  #:use-module ((rnrs conditions)
                #:select (&assertion
                          make-assertion-violation
                          simple-conditions))
  #:use-module ((srfi srfi-1) :select (drop-while find))
  #:replace (close throw)
  #:export (AssertionError
            AssertionError.
            Error
            Error.
            Exception
            Exception.
            ExceptionInfo
            Throwable
            Throwable.
            assert
            ex-cause
            ex-cause?
            ex-data
            ex-info
            ex-info?
            ex-message
            ex-suppressed
            new
            try
            with-final
            with-open)
  #:duplicates (merge-generics replace warn-override-core warn last))

;; FIXME: do we want/need any with-continuation-barrier wrappers,
;; and/or does the infrastructure already provide a barrier?

(define-syntax-rule (validate-arg fn-name pred expected val)
  (unless (pred val)
    (scm-error 'wrong-type-arg fn-name
               "~A argument is not ~A: ~A"
               (list 'val expected val) (list val))))

;; This is very experimental, i.e. not sure we'll want to preserve
;; exactly this kind of interop.  For now, provide *very* basic
;; compatibility, and as yet, there's no extensibility.

(define-exception-type &cause &exception make-cause ex-cause? (cause ex-cause))
(define-exception-type &suppressed &exception make-suppressed
  exception-with-suppression?
  (suppressed suppressed-exceptions))

(define Throwable &error)
(define Error &error)
(define Exception &error)
(define AssertionError &assertion)

(define-exception-type ExceptionInfo &error make-ex-info ex-info?
  (data ex-data))

;; The Foo. constructors mostly match the JVM

(define (make-basic-constructor fn-name make-base)
  (match-lambda*
    (() (make-base))
    ((msg-or-cause)
     (cond
      ((string? msg-or-cause)
       (make-exception (make-base) (make-exception-with-message msg-or-cause)))
      ((exception? msg-or-cause)
       (make-exception (make-base) (make-cause msg-or-cause)))
      (else
       (scm-error 'wrong-type-arg fn-name
                  "argument is not a string message or exception cause: ~s"
                  (list msg-or-cause) (list msg-or-cause)))))
    ((msg cause)
     (validate-arg fn-name string? "a string" msg)
     (validate-arg fn-name exception? "an exception" cause)
     (make-exception (make-base)
                     (make-exception-with-message msg)
                     (make-cause cause)))))

(define Throwable. (make-basic-constructor 'Throwable. make-error))
(define Error. (make-basic-constructor 'Error. make-error))
(define Exception. (make-basic-constructor 'Exception. make-error))

(define AssertionError.
  (let* ((make-base (make-basic-constructor 'AssertionError. make-assertion-violation)))
    (match-lambda*
      (() (make-base))
      ((x)
       (make-exception (make-base (pr-str x))
                       (make-exception-with-irritants (list x))))
      ((msg cause)
       (validate-arg 'AssertionError. string? "a string" msg)
       (validate-arg 'AssertionError. exception? "an exception" cause)
       (make-exception (make-base msg) (make-cause cause))))))

(define* (make-assert-error what #:optional (msg #f))
  (let* ((msg (if msg
                  (string-append "Assert failed: " msg "\n" (pr-str what))
                  (string-append "Assert failed: " (pr-str what)))))
    (make-exception (make-assertion-violation)
                    (make-exception-with-message msg)
                    (make-exception-with-irritants (list what)))))

(define-syntax assert
  (syntax-rules ()
    ((_ x) (when-not x (raise-exception (make-assert-error 'x))))
    ((_ x message) (when-not x (raise-exception (make-assert-error 'x message))))))

(define ex-info
  (match-lambda*
    ((msg map)
     (validate-arg 'ex-info string? "a string" msg)
     (validate-arg 'ex-info map? "a map" map)
     (make-exception (make-error)
                     (make-exception-with-message msg)
                     (make-ex-info map)))
    ((msg map cause)
     (validate-arg 'ex-info string? "a string" msg)
     (validate-arg 'ex-info map? "a map" map)
     (validate-arg 'ex-info exception? "an exception" cause)
     (make-exception (make-error)
                     (make-exception-with-message msg)
                     (make-cause cause)
                     (make-ex-info map)))))

;; *If* we keep support for new, this is insufficient and wrong and
;; wouldn't go here, but for now, it allows some existing code to
;; work.  (Could also consider define-method.)

(define (new type . args)
  (cond
   ((eq? type Exception) (apply Exception. args))
   ((eq? type Throwable) (apply Throwable. args))
   ((eq? type Error) (apply Error. args))
   ((eq? type AssertionError) (apply AssertionError. args))
   (else
    (scm-error 'wrong-type-arg 'new "Unexpected type:: ~A"
               (list type) (list type)))))

(define (ex-message x)
  (validate-arg 'ex-message exception? "an exception" x)
  (if (exception-with-message? x) (exception-message x) #nil))

(define (ex-suppressed x)
  (validate-arg 'ex-suppressed exception? "an exception" x)
  (if (exception-with-suppression? x) (suppressed-exceptions x) #nil))

(define (add-suppressed ex suppressed-ex)
  (validate-arg 'add-suppressed exception? "an exception" ex)
  (validate-arg 'add-suppressed exception? "an exception" suppressed-ex)
  (if (exception-with-suppression? ex)
      (let* ((suppressed (lokke-vector-conj (suppressed-exceptions ex)
                                            suppressed-ex)))
        (apply make-exception (map (lambda (x)
                                     (if (exception-with-suppression? x)
                                         (make-suppressed suppressed)
                                         x))
                                   (simple-conditions ex))))
      (make-exception ex (make-suppressed (lokke-vector suppressed-ex)))))

(define (throw ex)
  (raise-exception ex))

(define (call-with-exception-suppression ex thunk)
  ;; FIXME: double-check semantics here, i.e. #nil ex expected/ok?
  (let* ((result (with-exception-handler
                     (lambda (suppressed)
                       (raise-exception (if ex
                                            (add-suppressed ex suppressed)
                                            suppressed)))
                   thunk
                   #:unwind? #t)))
    (when ex
      (raise-exception ex))
    result))

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
      (syntax-case syn ()
        ((exp* ...) (has-finally-clauses? (syntax->datum #'(exp* ...)))
         (scm-error 'syntax-error 'try "finally clause must be last and unique in ~s"
                    (list (syntax->datum x)) #f))
        (((maybe-catch catch-exp* ...) exp* ...)
         (eq? 'catch (syntax->datum #'maybe-catch))
         '())
        ((exp exp* ...) #`(exp #,@(body-clauses #'(exp* ...))))
        (() '())))

    (define (drop-body syn)
      (drop-while (lambda (x)
                    (let* ((x (syntax->datum x)))
                      (or (not (list? x))
                          (not (eq? 'catch (car x))))))
                  syn))

    (define (catch-clauses caught syn)
      (syntax-case (drop-body syn) ()
        ((exp* ...) (has-finally-clauses? (syntax->datum #'(exp* ...)))
         (scm-error 'syntax-error 'try "finally clause must be last and unique in ~s"
                    (list (syntax->datum x)) #f))
        (((maybe-catch what ex catch-exp* ...) exp* ...)
         (eq? 'catch (syntax->datum #'maybe-catch))
         #`((((exception-predicate what) #,caught)
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

    (syntax-case x ()
      ((_ exp* ... (finally-1? finally-exp-1 ...) (finally-2? finally-exp-2 ...))
       (and (eq? 'finally (syntax->datum #'finally-1?))
            (eq? 'finally (syntax->datum #'finally-2?)))
       (scm-error 'syntax-error 'try "finally clause must be last and unique in ~s"
                  (list (syntax->datum x)) #f))

      ((_ exp* ... (maybe-finally finally-exp* ...))
       (eq? 'finally (syntax->datum #'maybe-finally))
       (let* ((caught (car (generate-temporaries '(#t))))
              (body (body-clauses #'(exp* ...)))
              (catches (catch-clauses caught #'(exp* ...))))
         #`(let* ((result (with-exception-handler
                              (lambda (ex)
                                (call-with-exception-suppression
                                 ex
                                 (lambda () #nil finally-exp* ...)))
                            (lambda ()
                              (with-exception-handler
                                  ;; FIXME: compiler smart enough to elide?
                                  (lambda (#,caught)
                                    (cond
                                     #,@catches
                                     (else (throw #,caught))))
                                (lambda () #nil #,@body)
                                #:unwind? #t))
                            #:unwind? #t)))
             finally-exp* ...
             result)))

      ;; Assume no finally clause
      ((_ exp* ...)
       (let* ((caught (car (generate-temporaries '(#t))))
              (body (body-clauses #'(exp* ...)))
              (catches (catch-clauses caught #'(exp* ...))))
         #`(with-exception-handler
               (lambda (#,caught)
                 (cond
                  #,@catches
                  (else (throw #,caught))))
             (lambda () #nil #,@body)
             #:unwind? #t))))))


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
           (with-exception-handler
               (lambda (ex)
                 (call-with-exception-suppression
                  ex
                  (lambda () (action resource))))
             (lambda ()
               (with-final (binding ...) body ...))
             #:unwind? #t)))
      ((_ (var init binding ...) body ...)
       #'(let (var init)
           (with-final (binding ...) body ...)))
      ((_ () body ...)
       #'(begin #nil body ...)))))
