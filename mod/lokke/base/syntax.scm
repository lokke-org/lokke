;;; Copyright (C) 2015-2019 Rob Browning <rlb@defaultvalue.org>
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

;; Commonly useful code, including most notably, destructuring let and
;; fn.

;; For now, these allow #() in place of [] on the scheme side,
;; i.e. (let #(x 1 y 2) ...), and handle the reader's symbols
;; (/lokke/reader-vector ...), etc., but we might eventually want to
;; separate these, assuming we wan to keep both, and just have two
;; separate modules, say (lokke compile base) with the
;; /lokke/... versions and this module with the scheme friendly
;; versions.
;;
;; We might also want to improve the error handling, in part via more
;; liberal use of syntax pattern guards.

(define-module (lokke base syntax)
  #:use-module ((guile)
                #:select ((do . %scm-do)
                          (if . %scm-if)
                          (let . %scm-let)))
  #:use-module ((srfi srfi-1)
                #:select (append-map
                          concatenate
                          dotted-list?
                          drop-right
                          last
                          take-right))
  #:use-module ((lokke base collection) #:select (merge))
  #:use-module ((lokke base destructure) #:select (destructure-binding-syntax))
  #:use-module ((lokke base doc) #:select (clear-def-doc! maybe-set-def-doc!))
  #:use-module ((lokke base dynamic) #:select (binding defdyn defdynloc))
  #:use-module ((lokke base util)
                #:select (global-identifier? map-tag? pairify vec-tag?))
  #:use-module (oop goops)
  #:re-export (binding defdyn defdynloc)
  #:export (->
            ->>
            cond
            declare
            def
            defn
            dotimes
            fn
            if-let
            if-not
            letfn
            loop
            var
            when-let
            when-not)
  ;; We leave let as let within this module.
  #:replace (and or cond if (let** . let) when))

(eval-when (expand load eval)
  (define debug-fn? #f)
  (define debug-let? #f))

(define-syntax-rule (var name)
  (module-variable (current-module) 'name))

(define-syntax declare
  (syntax-rules ()
    ((_ name) (define name))
    ((_ name name* ...) (begin (define name) (declare name* ...)))))

(define-syntax ->
  (syntax-rules ()
    ((_ x) x)
    ((_ x (f args ...) expr ...) (-> (f x args ...) expr ...))
    ((_ x f expr ...) (-> (f x) expr ...))))

(define-syntax ->>
  (syntax-rules ()
    ((_ x) x)
    ((_ x (f args ...) expr ...) (->> (f args ... x) expr ...))
    ((_ x f expr ...) (->> (f x) expr ...))))

(define-syntax def
  (lambda (x)
    (syntax-case x ()
      ((_ sym doc value) (string? (syntax->datum #'doc))
       #`(begin
           (define sym value)
           (maybe-set-def-doc! (module-variable (current-module) 'sym)
                               sym
                               #,(global-identifier? #'sym) doc)
           (when (procedure? sym)
             (set-procedure-property! sym 'name 'sym))
           (export sym)
           (var sym)))
      ((_ sym value)
       #`(begin
           (define sym value)
           (clear-def-doc! (module-variable (current-module) 'sym)
                           #,(global-identifier? #'sym))
           (export sym)
           (var sym))))))

;; FIXME: think we might have a redundant expansion, i.e. not sure
;; let** needs to cons the initial extra binding.

(define-syntax dbglet
  (lambda (x)
    (syntax-case x ()
      ((_ arg ...) (%scm-if debug-let?
                            #'(format (current-error-port) arg ...)
                            #t)))))

(define-syntax let**
  (lambda (x)
    (define (destructured-bindings binding init body)
      (let ((var (car (generate-temporaries '(#t)))))
        (cons #`(#,var #,init)
              (destructure-binding-syntax x binding var))))
    (define (expand bindings body)
      (let* ((bindings (concatenate
                        (map (lambda (p)
                               (apply destructured-bindings (append p (list body))))
                             (pairify bindings))))
             (result #`(let* #,bindings #,@body)))
        (dbglet "expanded let:\n  ~s\n->\n  ~s\n->\n  ~s\n"
                (syntax->datum `(let ,bindings ,@body))
                result
                (syntax->datum `(let* ,bindings ,@body)))
         result))
    (syntax-case x ()
      ((_ (vec-tag binding ...) body ...)  (vec-tag? #'vec-tag)
       (expand #'(binding ...) #'(body ...)))
      ;; Lists of bindings
      ((_ ()) #nil)
      ((_ (binding ...) body ...) (expand #'(binding ...) #'(body ...))))))

(define-syntax if
  (syntax-rules ()
    ((_ test then) (%scm-if test then #nil))
    ((_ test then else) (%scm-if test then else))))

(define-syntax if-not
  (syntax-rules ()
    ((_ test then) (if (not test) then))
    ((_ test then else) (if (not test) then else))))

(define-syntax and
  (syntax-rules ()
    ((_) #t)
    ((_ x) x)
    ((_ x x* ...) (%scm-let ((result x)) (if result (and x* ...) result)))))

(define-syntax or
  (syntax-rules ()
    ((_) #nil)
    ((_ x) x)
    ((_ x x* ...) (%scm-let ((result x)) (if result result (or x* ...))))))

(define-syntax when
  (syntax-rules ()
    ((_ guard body ...) (if guard (begin #nil body ...)))))

(define-syntax when-not
  (syntax-rules ()
    ((_ guard body ...) (if-not guard (begin #nil body ...)))))

(warn "audit all the if-let-ish expansions for shadowing in else or how about some tests, hmmmmm?")

;; FIXME: tests
;; FIXME: vector vs list? (see DESIGN)
(define-syntax if-let
  (lambda (x)
    (syntax-case x ()
      ((_ (vec-tag var test) body ...) (vec-tag? #'vec-tag)
       #'(if-let (var test) body ...))
      ((_ (var test) then)
       #'(if-let (var test) then #nil))
      ((_ (var test) then else)
       #'(%scm-let ((outcome test))
           (if outcome
               (let** (var outcome) then)
               else))))))

(define-syntax when-let
  (lambda (x)
    (syntax-case x ()
      ((_ (vec-tag var test) body ...) (vec-tag? #'vec-tag)
       #'(when-let (var test) body ...))
      ((_ (var test)) #'(begin test #nil))
      ((_ (var test) body ...) #'(if-let (var test) (begin #nil body ...))))))

(define-syntax cond
  (syntax-rules ()
    ((_) #nil)
    ((_ guard expr expr* ...) (if guard expr (cond expr* ...)))))

(define-syntax loop
  (lambda (x)
    (define (expand context bindings body)
      (with-syntax ((((var val) ...) (pairify bindings))
                    (recur (datum->syntax context 'recur)))
        (let* ((shim-args (generate-temporaries #'((var val) ...)))
               (recur-args (map (lambda (shim var-val)
                                  (list shim (cadr var-val)))
                                shim-args
                                #'((var val) ...)))
               (let-args (append-map (lambda (shim var-val)
                                       (list (car var-val) shim))
                                     shim-args
                                     #'((var val) ...))))
          #`(%scm-let recur #,recur-args
                      (let** #,let-args
                        #,@body)))))
    ;; Note that both clauses have to have the recur literal -- was
    ;; very confusing before realizing that....
    (syntax-case x ()
      ((_ (vec-tag binding ...) body ...) (vec-tag? #'vec-tag)
       (expand x #'(binding ...) #'(body ...)))
      ((_ (binding ...) body ...)
       (expand x #'(binding ...) #'(body ...))))))

(define-syntax dotimes
  (lambda (x)
    (syntax-case x ()
      ((dotimes (vec-tag var n) body ...) (vec-tag? #'vec-tag)
       #'(dotimes (var n) body ...))
      ((dotimes (var n)) #nil)
      ((dotimes (var n) body ...)
       #'(when (positive? n)
           (%scm-let ((max (truncate n)))
             (%scm-do ((var 0 (1+ var)))
                      ((or (= var max) (> var 100)) #nil)
                      body ...)))))))

(define-syntax dbgfn
  (lambda (x)
    (syntax-case x ()
      ((_ arg ...) (%scm-if debug-fn?
                            #'(format (current-error-port) arg ...)
                            #t)))))

(define (undotted lst)
  (if (not (dotted-list? lst))
      lst
      (append (drop-right lst 0) (list (take-right lst 0)))))

(define (dotted lst)
  (if (dotted-list? lst)
      lst
      (append (drop-right lst 1) (last lst))))

;; FIXME: should recur be an identifier syntax (or similar),
;; i.e. read-only?

;; FIXME: can likely make this DRYer

(define-syntax fn
  ;; Deep breath - so we expand single arity functions into normal
  ;; lambdas, and we use letrec and we break hygene to make recur an
  ;; alternate name for named functions.  We handle multiple ariity
  ;; functions by creating a generic function in the base case and
  ;; then adding a method for each arity.  Finally, we rewrite every
  ;; arity with new argument identifiers that are then used as the
  ;; values in a new let wrapping the body (cf. loop above).  The
  ;; binding for each value is the original funcion argument.  As a
  ;; trivial example: (fn [[x]] ...) -> (fn [tmp] (let [[x] tmp] ...),
  ;; in Clojure parlance.

  ;; Always put & patterns first
  (lambda (x)

    (define (make-fn args body)
      (let* ((dot? (dotted-list? args))
             (proper-args (undotted args))
             (shim-args (generate-temporaries proper-args))
             (let-args (append-map (lambda (shim orig) (list orig shim))
                                   shim-args
                                   proper-args))
             (shim-args (if dot? (dotted shim-args) shim-args)))
        #`(lambda #,shim-args (let** #,let-args #nil #,@body))))

    (define (method-adder m args body)
      (let* ((dot? (dotted-list? args))
             (proper-args (undotted args))
             (dummy (dbgfn "proper: ~s\n" proper-args))
             (shim-args (generate-temporaries proper-args))
             (let-args (append-map (lambda (shim orig) (list orig shim))
                                   shim-args
                                   proper-args))
             (shim-args (if dot? (dotted shim-args) shim-args)))
        #`(add-method! #,m (method #,shim-args
                             (let** #,let-args #nil #,@body)))))

    ;; Single arity handlers (could be DRYer?)
    (define (single-arity? args-syn)
      (let* ((args (syntax->datum args-syn)))
        (or (vector? args)
            (and (list? args)
                 (not (null? args))
                 (eq? '/lokke/reader-vector (car args))))))
    (define (single-arity context args body)
      (define (expand args body)
        (if (null? body)
            (make-fn args body)
            ;; Do we need (car body), or could it be context?
            (with-syntax ((recur (datum->syntax (car body) 'recur)))
              #`(letrec ((recur #,(make-fn args body)))
                  recur))))
      (syntax-case args (&)
        ((vec-tag arg ... & rst) (vec-tag? #'vec-tag)
         (expand #'(arg ... . rst) body))
        ((vec-tag arg ...) (vec-tag? #'vec-tag)
         (expand #'(arg ...) body))
        (#(arg ... & rst) (expand #'(arg ... . rst) body))
        (#(arg ...) (expand #'(arg ...) body))))

    (define (named-single-arity template name args body)
      (define (expand args body)
        (if (null? body)
            (make-fn args body)
            (with-syntax ((recur (datum->syntax (car body) 'recur)))
              #`(letrec ((#,name (lambda args (apply recur args)))
                         (recur #,(make-fn args body)))
                  recur))))
      (syntax-case args (&)
        ((vec-tag arg ... & rst) (vec-tag? #'vec-tag)
         (expand #'(arg ... . rst) body))
        ((vec-tag arg ...) (vec-tag? #'vec-tag)
         (expand #'(arg ...) body))
        (#(arg ... & rst) (expand #'(arg ... . rst) body))
        (#(arg ...) (expand #'(arg ...) body))))

    ;; Multiple arity handlers.  Note that we don't yet verify that
    ;; there's only one variant with a rest arg).
    (define (named-multi-arity template name arities)
      ;; arities will never be null
      (with-syntax ((recur (datum->syntax (car arities) 'recur)))
        (define (add-method-for-arity arity)
          (syntax-case arity (&)
            ;; Always put & patterns first
            (((vec-tag arg ... & rst) body ...) (vec-tag? #'vec-tag)
             (method-adder #'recur #'(arg ... . rst) #'(body ...)))
            (((vec-tag arg ...) body ...) (vec-tag? #'vec-tag)
             (method-adder #'recur #'(arg ...) #'(body ...)))
            ((#(arg ... & rst) body ...)
             (method-adder #'recur #'(arg ... . rst) #'(body ...)))
            ((#(arg ...) body ...)
             (method-adder #'recur #'(arg ...) #'(body ...)))))
        #`(letrec ((#,name (lambda args (apply recur args)))
                   (recur (make-generic)))
            #,@(map add-method-for-arity arities)
            recur)))
    (define (multi-arity template arities)
      ;; arities will never be null
      (with-syntax ((recur (datum->syntax (car arities) 'recur)))
        (define (add-method-for-arity arity)  ;; FIXME: identical to above
          (syntax-case arity (&)
            ;; Always put & patterns first
            (((vec-tag arg ... & rst) body ...) (vec-tag? #'vec-tag)
             (method-adder #'recur #'(arg ... . rst) #'(body ...)))
            (((vec-tag arg ...) body ...) (vec-tag? #'vec-tag)
             (method-adder #'recur #'(arg ...) #'(body ...)))
            ((#(arg ... & rst) body ...)
             (method-adder #'recur #'(arg ... . rst) #'(body ...)))
            ((#(arg ...) body ...)
             (method-adder #'recur #'(arg ...) #'(body ...)))))
        #`(letrec ((recur (make-generic)))
            #,@(map add-method-for-arity arities)
            recur)))

    (dbgfn "fn:\n  syn: ~s\n  dat: ~s\n" x (syntax->datum x))
    (syntax-case x ()
      ;; FIXME: Multiple or single arity? (defn foo (#() #t))
      ;; For now we intercept the (fn ([] ...)) case as a degenerate
      ;; single arity.

      ;; Named single arity
      ((_ name (args body ...)) (and (identifier? #'name) (single-arity? #'args))
       (named-single-arity x #'name #'args #'(body ...)))
      ((_ name args body ...) (and (identifier? #'name) (single-arity? #'args))
       (named-single-arity x #'name #'args #'(body ...)))

      ;; Unnamed single arity
      ((_ args body ...) (single-arity? #'args)
       (single-arity x #'args #'(body ...)))
      ((_ (args body ...)) (single-arity? #'args)
       (single-arity x #'args #'(body ...)))

      ;; Better be multi-arity...

      ;; Named multiple arity
      ((_ name arity arities ...) (identifier? #'name)
       (named-multi-arity x #'name #'(arity arities ...)))

      ;; Unnamed multiple arity
      ((_ arity arities ...)
       (multi-arity x #'(arity arities ...))))))

(define-syntax defn
  (lambda (x)
    (syntax-case x ()
      ;; doc and attrs
      ((_ name doc (map-tag attr ...) expr ...)
       (and (string? (syntax->datum #'doc)) (map-tag? #'map-tag))
       #'(begin
           (def name doc (fn expr ...))
           (alter-meta! (module-variable (current-module) 'name)
                        (lambda (prev) (map-tag attr ...)))
           (var name)))
      ;; just attrs
      ((_ name (map-tag attr ...) expr ...) (map-tag? #'map-tag)
       #'(begin
           (def name (fn expr ...))
           (alter-meta! (module-variable (current-module) 'name)
                        (lambda (prev) (map-tag attr ...)))
           (var name)))
      ;; just doc
      ((_ name doc expr ...) (string? (syntax->datum #'doc))
       #'(def name doc (fn expr ...)))
      ;; none
      ((_ name expr ...)
       #'(def name (fn expr ...))))))

(define-syntax-rule (letfn ((fn-name fn-body ...) ...) body ...)
  (letrec ((fn-name (fn fn-name fn-body ...)) ...)
    #nil
    body ...))
