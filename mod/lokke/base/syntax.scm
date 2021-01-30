;;; Copyright (C) 2015-2020 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

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
                #:select ((and . %scm-and)
                          (cond . %scm-cond)
                          (do . %scm-do)
                          (if . %scm-if)
                          (let . %scm-let)
                          (or . %scm-or)))
  #:use-module ((srfi srfi-1)
                #:select (append-map!
                          dotted-list?
                          drop-right
                          find
                          last
                          remove
                          take-right))
  #:use-module ((lokke base collection)
                #:select ((cons . clj-cons)
                          concat
                          first
                          get
                          lazy-seq
                          rest
                          take-while
                          seq))
  #:use-module ((lokke base destructure) #:select (destructure-binding-syntax))
  #:use-module ((lokke base doc) #:select (clear-def-doc! maybe-set-def-doc!))
  #:use-module ((lokke base dynamic) #:select (binding defdyn defdynloc))
  #:use-module ((lokke base metadata) #:select (alter-meta!))
  #:use-module ((lokke base util)
                #:select (global-identifier?
                          map-tag?
                          meta-tag?
                          pairify
                          set-tag?
                          vec-tag?))
  #:use-module ((lokke compare) #:select (clj=))
  #:use-module ((lokke metadata) #:select (with-meta))
  #:use-module ((lokke reader literal)
                #:select (reader-hash-map? reader-meta? reader-vector?))
  #:use-module (oop goops)
  #:re-export (binding defdyn defdynloc)
  #:export (->
            ->>
            as->
            cond
            cond->
            cond->>
            condp
            declare
            def
            def-
            defn
            defn-
            doseq
            dotimes
            for
            fn
            if-let
            if-not
            if-some
            letfn
            loop
            some->
            some->>
            some?
            var
            when-first
            when-let
            when-not
            when-some)
  ;; We leave let as let within this module.
  #:replace (and or cond if (let** . let) nil? when))

(eval-when (expand load eval)
  (define debug-fn? #f)
  (define debug-let? #f)
  (define debug-loop? #f))

(define (nil? x) (eq? #nil x))
(define (some? x) (not (nil? x)))

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

(define (expand-def sym metadata doc value must-be-private?)
  (with-syntax ((sym sym)
                (metadata metadata)
                (doc doc)
                (value value)
                (must-be-private? must-be-private?))
    (%scm-if #'doc
             #`(begin
                 (define sym value)
                 (maybe-set-def-doc! (module-variable (current-module) 'sym)
                                     sym
                                     #,(global-identifier? #'sym) doc)
                 (when metadata
                   (alter-meta! (module-variable (current-module) 'sym)
                                (lambda (prev) metadata)))
                 (when (procedure? sym)
                   (set-procedure-property! sym 'name 'sym))
                 (unless (or must-be-private? (get metadata #:private))
                   (export sym))
                 (var sym))
             #`(begin
                 (define sym value)
                 (clear-def-doc! (module-variable (current-module) 'sym)
                                 #,(global-identifier? #'sym))
                 (when metadata
                   (alter-meta! (module-variable (current-module) 'sym)
                                (lambda (prev) metadata)))
                 ;; Appropriate to do this at runtime?
                 (when (and (not must-be-private?) (not (get metadata #:private)))
                   (export sym))
                 (var sym)))))

(define-syntax def
  (lambda (x)
    (syntax-case x ()
      ((_ (meta-tag mdata) sym doc value)
       (%scm-and (meta-tag? #'meta-tag) (string? (syntax->datum #'doc)))
       (expand-def #'sym #'mdata #'doc #'value #f))
      ((_ (meta-tag mdata) sym value) (meta-tag? #'meta-tag)
       (expand-def #'sym #'mdata #f #'value #f))
      ((_ sym doc value) (string? (syntax->datum #'doc))
       (expand-def #'sym #nil #'doc #'value #f))
      ((_ sym value)
       (expand-def #'sym #nil #f #'value #f)))))

(define-syntax def-
  (lambda (x)
    (syntax-case x ()
      ((_ (meta-tag mdata) sym doc value)
       (%scm-and (meta-tag? #'meta-tag) (string? (syntax->datum #'doc)))
       (expand-def #'sym #'mdata #'doc #'value #t))
      ((_ (meta-tag mdata) sym value) (meta-tag? #'meta-tag)
       (expand-def #'sym #'mdata #f #'value #t))
      ((_ sym doc value) (string? (syntax->datum #'doc))
       (expand-def #'sym #nil #'doc #'value #t))
      ((_ sym value)
       (expand-def #'sym #nil #f #'value #t)))))

;; FIXME: think we might have a redundant expansion, i.e. not sure
;; let** needs to cons the initial extra binding.

(define-syntax dbglet
  (lambda (x)
    (syntax-case x ()
      ((_ arg ...) (%scm-if debug-let?
                            #'(format (current-error-port) arg ...)
                            #t)))))

(define (strip-top-level-metadata forms)
  ;; e.g. (let [^Integer x 5] x)
  ;;
  ;; For now, just drop all the the metadata.  This does mean that
  ;; using metadata values for side effects won't work.
  (remove (lambda (x) (reader-meta? (syntax->datum x)))
          forms))

(define-syntax let**
  (lambda (x)
    (define (destructured-bindings binding init body)
      (let ((var (car (generate-temporaries '(#t)))))
        (cons #`(#,var #,init)
              (destructure-binding-syntax binding binding var))))
    (define (expand bindings body)
      (let* ((bindings (strip-top-level-metadata bindings))
             (destructured (append-map!
                            (lambda (p)
                              (apply destructured-bindings (append p (list body))))
                            (pairify bindings)))
             (result #`(let* #,destructured #,@body)))
        (dbglet "expanded let:\n  ~s\n->\n  ~s\n->\n  ~s\n"
                (syntax->datum `(let ,bindings ,@body))
                result
                (syntax->datum `(let* ,destructured ,@body)))
         result))
    (syntax-case x ()
      ((_ ()) #nil)
      ((_ (vec-tag meta) body ...)  (vec-tag? #'vec-tag) #nil)
      ((_ (vec-tag meta binding ...) body ...)  (vec-tag? #'vec-tag)
       (expand #'(binding ...) #'(body ...)))
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

;; FIXME: audit all the if-let-ish expansions for shadowing in else
;; FIXME: more tests

;; FIXME: vector vs list? (see DESIGN)
(define-syntax if-let
  (lambda (x)
    (syntax-case x ()
      ((_ (vec-tag meta var test) body ...) (vec-tag? #'vec-tag)
       #'(if-let (var test) body ...))
      ((_ (var test) then)
       #'(if-let (var test) then #nil))
      ((_ (var test) then else)
       #'(%scm-let ((outcome test))
           (if outcome
               (let** (var outcome) then)
               else))))))

(define-syntax if-some
  (lambda (x)
    (syntax-case x ()
      ((_ (vec-tag meta var test) body ...) (vec-tag? #'vec-tag)
       #'(if-some (var test) body ...))
      ((_ (var test) then)
       #'(if-some (var test) then #nil))
      ((_ (var test) then else)
       #'(%scm-let ((outcome test))
           (if (some? outcome)
               (let** (var outcome) then)
               else))))))

(define-syntax when-let
  (lambda (x)
    (syntax-case x ()
      ((_ (vec-tag meta var test) body ...) (vec-tag? #'vec-tag)
       #'(when-let (var test) body ...))
      ((_ (var test)) #'(begin test #nil))
      ((_ (var test) body ...) #'(if-let (var test) (begin #nil body ...))))))

(define-syntax when-some
  (lambda (x)
    (syntax-case x ()
      ((_ (vec-tag meta var test) body ...) (vec-tag? #'vec-tag)
       #'(when-some (var test) body ...))
      ((_ (var test)) #'(begin test #nil))
      ((_ (var test) body ...) #'(if-some (var test) (begin #nil body ...))))))

(define-syntax cond
  (syntax-rules ()
    ((_) #nil)
    ((_ guard expr expr* ...) (if guard expr (cond expr* ...)))))

(define-syntax loop
  (lambda (x)
    (define (expand context bindings body)
      (with-syntax ((((var val) ...) (pairify bindings))
                    (recur (datum->syntax context 'recur)))
        (let* ((var-vals #'((var val) ...))
               (shim-args (generate-temporaries var-vals))
               (recur-args (map (lambda (shim var-val)
                                  (list shim (cadr var-val)))
                                shim-args
                                var-vals))
               (let-args (append-map! (lambda (shim var-val)
                                        (list (car var-val) shim))
                                      shim-args
                                      var-vals)))
          #`(%scm-let recur #,recur-args
                      (let** #,let-args
                             #,@body)))))
    (syntax-case x ()
      ((loop (vec-tag meta binding ...) body ...) (vec-tag? #'vec-tag)
       (expand #'loop #'(binding ...) #'(body ...)))
      ((loop (binding ...) body ...)
       (expand #'loop #'(binding ...) #'(body ...))))))

(define-syntax dotimes
  (lambda (x)
    (syntax-case x ()
      ((dotimes (vec-tag meta var n) body ...) (vec-tag? #'vec-tag)
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

    (define (strip-condition-map body)
      ;; e.g. pre/post
      ;; FIXME: support pre/post
      ;; For now, we just drop the first map in any multi-form body.
      ;; We don't want to vet the map strictly unless upstream
      ;; specifies strict semantics, i.e. we assume we need to ignore
      ;; future keys, etc.  This does mean that using such a map for
      ;; evaluation side effects won't work.
      (let* ((body-dat (syntax->datum body)))
        (if (and (pair? body-dat)
                 (pair? (cdr body-dat))
                 (reader-hash-map? (car body-dat)))
            (cdr body)
            body)))

    (define (make-fn args body)
      (let* ((body (strip-condition-map body))
             (dot? (dotted-list? args))
             (proper-args (undotted args))
             (proper-args (strip-top-level-metadata proper-args))
             (shim-args (generate-temporaries proper-args))
             (let-args (append-map! (lambda (shim orig) (list orig shim))
                                    shim-args
                                    proper-args))
             (shim-args (if dot? (dotted shim-args) shim-args)))
        #`(lambda #,shim-args (let** #,let-args #nil #,@body))))

    (define (method-adder m args body)
      (let* ((body (strip-condition-map body))
             (dot? (dotted-list? args))
             (proper-args (undotted args))
             (dummy (dbgfn "proper: ~s\n" proper-args))
             (shim-args (generate-temporaries proper-args))
             (let-args (append-map! (lambda (shim orig) (list orig shim))
                                    shim-args
                                    proper-args))
             (shim-args (if dot? (dotted shim-args) shim-args)))
        #`(add-method! #,m (method #,shim-args
                             (let** #,let-args #nil #,@body)))))

    ;; Single arity handlers (could be DRYer?)
    (define (single-arity? args-syn)
      (let* ((args (syntax->datum args-syn)))
        (or (vector? args)
            (reader-vector? args))))
    (define (single-arity context args body)
      (define (expand args body)
        (if (null? body)
            (make-fn args body)
            (with-syntax ((recur (datum->syntax context 'recur)))
              #`(letrec ((recur #,(make-fn args body)))
                  recur))))
      (syntax-case args (&)
        ((vec-tag meta arg ... & rst) (vec-tag? #'vec-tag)
         (expand #'(arg ... . rst) body))
        ((vec-tag meta arg ...) (vec-tag? #'vec-tag)
         (expand #'(arg ...) body))
        (#(arg ... & rst) (expand #'(arg ... . rst) body))
        (#(arg ...) (expand #'(arg ...) body))))

    (define (named-single-arity context name args body)
      (define (expand args body)
        (if (null? body)
            (make-fn args body)
            (with-syntax ((recur (datum->syntax context 'recur)))
              #`(letrec ((#,name (lambda args (apply recur args)))
                         (recur #,(make-fn args body)))
                  recur))))
      (syntax-case args (&)
        ((vec-tag meta arg ... & rst) (vec-tag? #'vec-tag)
         (expand #'(arg ... . rst) body))
        ((vec-tag meta arg ...) (vec-tag? #'vec-tag)
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
            (((vec-tag meta arg ... & rst) body ...) (vec-tag? #'vec-tag)
             (method-adder #'recur #'(arg ... . rst) #'(body ...)))
            (((vec-tag meta arg ...) body ...) (vec-tag? #'vec-tag)
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
            (((vec-tag meta arg ... & rst) body ...) (vec-tag? #'vec-tag)
             (method-adder #'recur #'(arg ... . rst) #'(body ...)))
            (((vec-tag meta arg ...) body ...) (vec-tag? #'vec-tag)
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
       (named-single-arity #'args #'name #'args #'(body ...)))
      ((_ name args body ...) (and (identifier? #'name) (single-arity? #'args))
       (named-single-arity #'args #'name #'args #'(body ...)))

      ;; Unnamed single arity
      ((_ args body ...) (single-arity? #'args)
       (single-arity #'args #'args #'(body ...)))
      ((_ (args body ...)) (single-arity? #'args)
       (single-arity #'args #'args #'(body ...)))

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
      ;; Just discard metadata for now
      ;; FIXME: parse and rewrite :doc, etc.
      ((_ (meta-tag m ...) expr ...) (meta-tag? #'meta-tag)
       #'(defn expr ...))
      ;; doc and attrs
      ((_ name doc (map-tag meta attr ...) expr ...)
       (and (string? (syntax->datum #'doc)) (map-tag? #'map-tag))
       #'(begin
           (def name doc (fn expr ...))
           (alter-meta! (module-variable (current-module) 'name)
                        (lambda (prev) (map-tag meta attr ...)))
           (var name)))
      ;; just attrs
      ((_ name (map-tag meta attr ...) expr ...) (map-tag? #'map-tag)
       #'(begin
           (def name (fn expr ...))
           (alter-meta! (module-variable (current-module) 'name)
                        (lambda (prev) (map-tag meta attr ...)))
           (var name)))
      ;; just doc
      ((_ name doc expr ...) (string? (syntax->datum #'doc))
       #'(def name doc (fn expr ...)))
      ;; none
      ((_ name expr ...)
       #'(def name (fn expr ...))))))

(define-syntax defn-
  (lambda (x)
    (syntax-case x ()
      ;; Just discard metadata for now
      ;; FIXME: parse and rewrite :doc, etc.
      ((_ (meta-tag m ...) expr ...) (meta-tag? #'meta-tag)
       #'(defn- expr ...))
      ;; doc and attrs
      ((_ name doc (map-tag meta attr ...) expr ...)
       (and (string? (syntax->datum #'doc)) (map-tag? #'map-tag))
       #'(begin
           (def- name doc (fn expr ...))
           (alter-meta! (module-variable (current-module) 'name)
                        (lambda (prev) (map-tag meta attr ...)))
           (var name)))
      ;; just attrs
      ((_ name (map-tag meta attr ...) expr ...) (map-tag? #'map-tag)
       #'(begin
           (def- name (fn expr ...))
           (alter-meta! (module-variable (current-module) 'name)
                        (lambda (prev) (map-tag meta attr ...)))
           (var name)))
      ;; just doc
      ((_ name doc expr ...) (string? (syntax->datum #'doc))
       #'(def- name doc (fn expr ...)))
      ;; none
      ((_ name expr ...)
       #'(def- name (fn expr ...))))))

(define-syntax letfn
  (lambda (x)
    (syntax-case x ()
      ((_ (vec-tag meta exp ...) body ...)  (vec-tag? #'vec-tag)
       #'(letfn (exp ...) body ...))
      ((_ ((fn-name fn-body ...) ...) body ...)
       #'(letrec ((fn-name (fn fn-name fn-body ...)) ...)
           #nil
           body ...)))))

(define-syntax %doseq
  (lambda (x)
    (syntax-case x ()
      ((_ (vec-tag meta exp ...) body ...)  (vec-tag? #'vec-tag)
       #'(%doseq (exp ...) body ...))
      ((_ (#:let bindings exp ...) body ...)
       #'(let** bindings (%doseq (exp ...) body ...)))
      ((_ (#:when guard exp ...) body ...)
       #'(begin
           (when guard (%doseq (exp ...) body ...))
           #t))
      ((_ (#:while guard exp ...) body ...)
       #'(when guard (%doseq (exp ...) body ...)))
      ((_ () body ...) #'(begin #nil body ... #t))
      ((_ (bind init bindings ...) body ...)
       #'(let loop ((s init))
           (let ((s (seq s)))
             (if (not s)
                 #t
                 (let** (bind (first s))
                   (when (%doseq (bindings ...) body ...)
                     (loop (rest s)))
                   #t))))))))

(define-syntax doseq
  (lambda (x)
    (syntax-case x ()
      ((_ exp ...)
       #'(begin
           (%doseq exp ...)
           #nil)))))

(define-syntax %for
  (lambda (x)
    (syntax-case x ()
      ((_ terminator inner (vec-tag meta exp ...) body ...)  (vec-tag? #'vec-tag)
       #'(%for terminator inner (exp ...) body ...))
      ;; Just discard metadata for now
      ((_ terminator inner ((meta-tag m ...) exp ...) body ...)
       (meta-tag? #'meta-tag)
       #'(%for terminator inner (exp ...) body ...))
      ((_ terminator inner (#:let bindings exp ...) body ...)
       #'(let** bindings (%for terminator inner (exp ...) body ...)))
      ((_ terminator inner (#:when guard exp ...) body ...)
       #'(if guard
             (%for terminator inner (exp ...) body ...)
             (inner)))
      ((_ terminator inner (#:while guard exp ...) body ...)
       #'(if guard
             (%for terminator inner (exp ...) body ...)
             (clj-cons terminator #nil)))
      ((_ terminator inner () body ...)
       #'(clj-cons (begin #nil body ...) (inner)))
      ((_ terminator inner (bind init bindings ...) body ...)
       #`(let loop ((s init))
           (lazy-seq
            (let ((x (seq s)))
              (when x
                (let** (bind (first x))
                       (let ((new-inner (lambda () (loop (rest s)))))
                         (take-while (lambda (x) (not (eq? x terminator)))
                                     (concat
                                      (%for terminator inner
                                        (bindings ...) body ...)
                                      (loop (rest s))))))))))))))

(define-syntax for
  (lambda (x)
    (syntax-case x ()
      ((_ exp ...)
       #'(let ((terminator (cons #f #f)))
           (%for terminator (lambda () #nil) exp ...))))))

(define-syntax condp
  (lambda (x)
    (syntax-case x ()
      ((_ pred expr default) #'default)
      ((_ pred expr comparable #:>> result-fn clauses ...)
       #'(if-let (result (pred comparable expr))
           (result-fn result)
           (condp pred expr clauses ...)))
      ((_ pred expr comparable result clauses ...)
       #'(if (pred comparable expr)
             result
             (condp pred expr clauses ...)))
      ((_ pred expr)
       #'(error "No matching clause for" '(pred _ expr))))))

(define-syntax cond->
  (syntax-rules ()
    ((_ x) x)
    ((_ x pred exp exp* ...)
     (%scm-if (pred x)
              (cond-> (-> x exp) exp* ...)
              (cond-> x exp* ...)))))

(define-syntax cond->>
  (syntax-rules ()
    ((_ x) x)
    ((_ x pred exp exp* ...)
     (%scm-if (pred x)
              (cond->> (->> x exp) exp* ...)
              (cond->> x exp* ...)))))

(define-syntax some->
  (syntax-rules ()
    ((_ x) x)
    ((_ x exp exp* ...)
     (%scm-let ((y x))
       (%scm-if (nil? y)
                #nil
                (some-> (-> y exp) exp* ...))))))

(define-syntax some->>
  (syntax-rules ()
    ((_ x) x)
    ((_ x exp exp* ...)
     (%scm-let ((y x))
       (%scm-if (nil? y)
                #nil
                (some->> (->> y exp) exp* ...))))))

(define-syntax when-first
  (lambda (x)
    (syntax-case x ()
      ((_ (vec-tag meta binding exp) body ...)  (vec-tag? #'vec-tag)
       #'(when-first (binding exp) body ...))
      ((_ (binding exp) body ...)
       #'(let ((s (seq exp)))
           (when s
             (let** (binding (first s))
               body ...)))))))

(define-syntax as->
  (lambda (x)
    (syntax-case x ()
      ((_ exp name) #'exp)
      ((_ exp name form form* ...)
       (with-syntax ((x (datum->syntax #'form (syntax->datum #'name))))
         #'(let ((x exp))
             (as-> form name form* ...)))))))
