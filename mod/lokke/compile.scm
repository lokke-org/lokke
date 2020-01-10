;;; Copyright (C) 2019 Rob Browning <rlb@defaultvalue.org>
;;;
;;; This project is free software; you can redistribute it and/or
;;; modify it under the terms of (at your option) either of the
;;; following two licences:
;;;
;;;   1) The GNU Lesser General Public License as published by the
;;;      Free Software Foundation; either version 2.1, or (at your
;;;      option) any later version.
;;;
;;;   2) The Eclipse Public License; either version 1.0 or (at your
;;;      option) any later version.

(define-module (lokke compile)
  #:use-module ((ice-9 match) #:select (match))
  #:use-module ((ice-9 pretty-print) #:select (pretty-print))
  #:use-module ((ice-9 receive) #:select (receive))
  #:use-module ((ice-9 sandbox)
                #:select (all-pure-bindings
                          all-pure-and-impure-bindings
                          make-sandbox-module))
  #:use-module ((ice-9 vlist) #:select (alist->vhash vhash-assq))
  #:use-module ((language scheme compile-tree-il) #:prefix scheme/)
  #:use-module ((language tree-il) #:prefix tree-il/)
  #:use-module ((lokke hash-map) #:select (get))
  #:use-module ((lokke ns) #:select (ns-aliases))
  #:use-module ((lokke scm atom) #:select (atom-deref))
  #:use-module ((lokke symbol)
                #:select (ns-sym->mod-name
                          parse-symbol
                          parsed-sym-ns
                          parsed-sym-ref
                          simple-symbol?))
  #:use-module ((lokke transmogrify)
                #:select (clj-instances->literals
                          literals->clj-instances
                          literals->scm-instances
                          preserve-meta-if-new!))
  #:use-module (oop goops)
  #:use-module ((srfi srfi-1)
                #:select (any
                          every
                          find
                          first
                          fold
                          last
                          second))
  #:use-module ((srfi srfi-43) #:select (vector-map))
  #:use-module ((system base compile)
                #:select ((compile . base-compile)
                          compile-file
                          compiled-file-name))
  #:export (clj-defmacro
            load-file
            make-lokke-language
            tree->tree-il)
  #:duplicates (merge-generics replace warn-override-core warn last))

;; Right now the tree walkers in this code tend to be prescriptive,
;; i.e. they reject any type they don't recognize.  That's been
;; helpful with respect to debugging, but we could omit some of the
;; checks and just rely on a catch-all else clause (perhaps controlled
;; via a debug option) if we liked.

(define debug-compile? #f)
(define debug-il? (or debug-compile? #f))
(define enable-invoke? #t)

(define dbg
  (if debug-compile?
      (lambda args (apply format (current-error-port) args))
      (lambda args #t)))

(eval-when (expand load eval)
  (define (/lokke/prep-form-for-clj-macro form)
    (literals->clj-instances form)))

(eval-when (expand load eval)
  (define (/lokke/convert-form-from-clj-macro form)
    (clj-instances->literals form)))

(define (make-invoke-ref src)
  (tree-il/make-module-ref src '(lokke invoke) 'invoke #t))

(define (make-vector-fn-ref src)
  (tree-il/make-module-ref src '(lokke scm vector) 'lokke-vector #t))

(define (rewrite-il-call call)
  (define (add-invoke call)
    (tree-il/make-call (tree-il/call-src call)
                       (make-invoke-ref (tree-il/call-src call))
                       (cons (tree-il/call-proc call)
                             (tree-il/call-args call))))
  (if enable-invoke? (add-invoke call) call))

(define (resolved-ns-sym->mod-name ns-sym aliases)
  (if (not aliases)
      (ns-sym->mod-name ns-sym)
      (let ((mod (get aliases ns-sym)))
        (if mod
            (module-name mod)
            (ns-sym->mod-name ns-sym)))))

(define (rewrite-il-toplevel top ns-aliases)
  "Rewrites any <toplevel-ref> tree-il nodes containing a namespaced
Clojure reference like clojure.string/join to the corresponding Guile
<modulle-ref>."
  (let ((name (tree-il/toplevel-ref-name top)))
    (cond
     ((string-prefix? "/lokke/" (symbol->string name)) top)
     ((simple-symbol? name) top)
     (else
      (let* ((parsed (parse-symbol name))
             (ns-sym (parsed-sym-ns parsed)))
        (unless ns-sym
          ;; FIXME: appropriate?  They're class references for Clojure/JVM
          (error "Top-level x.y references are currently not allowed" name))
        (tree-il/make-module-ref (tree-il/toplevel-ref-src top)
                                 (resolved-ns-sym->mod-name ns-sym ns-aliases)
                                 (parsed-sym-ref parsed)
                                 #f))))))

(define il-count 0)

;; FIXME: do we want/need to look for and reject <lexical-ref>s that
;; are (not (simple-symbol? name))?

(define (rewrite-il-calls il ns-aliases)
  ;; FIXME: source-properties...
  (define (up tree)
    (let* ((count (begin (set! il-count (1+ il-count)) il-count))
           (_ (when debug-il? (format (current-error-port) "il[~a]: ~s\n" count tree)))
           (result (cond
                    ((tree-il/call? tree)
                     (let ((result (rewrite-il-call tree)))
                       (when debug-il?
                         (format (current-error-port) "il[~a]: ~s\n" count result))
                       result))
                    ((tree-il/toplevel-ref? tree)
                     (let ((result (rewrite-il-toplevel tree ns-aliases)))
                       (when debug-il?
                         (format (current-error-port) "il[~a]: ~s\n" count result))
                       result))
                    (else
                     (when debug-il?
                       (format (current-error-port) "il[~a]: <<unchanged>>\n" count))
                     tree))))
      result))
  (let ((result (tree-il/post-order up il)))
    (when debug-il?
      (format (current-error-port) "tree-il:\n")
      (pretty-print (tree-il/unparse-tree-il il) (current-error-port))
      (format (current-error-port) "  =>\n")
      (pretty-print (tree-il/unparse-tree-il result) (current-error-port)))
    result))

(define (tree->tree-il expr env opts)
  ;; FIXME: source-properties...
  (when debug-compile? (format (current-error-port) "compile: ~s\n" expr))
  ;; At the moment, env and cenv will be the same from the scheme compiler
  (receive (result env cenv)
      (scheme/compile-tree-il expr env opts)
    (when debug-compile?
      (format (current-error-port) "initial-tree-il: ~s\n" result))
    (let ((result (rewrite-il-calls result (ns-aliases env))))
      (when debug-compile? (format (current-error-port) "final-tree-il: ~s\n" result))
      (values result env env))))

(define (load-file path)
  (let ((compiled (compiled-file-name path)))
    (when (or (not (file-exists? compiled))
              (let ((st-src (stat path))
                    (st-com (stat compiled)))
                (or (<= (stat:mtime st-com) (stat:mtime st-src))
                    (and (= (stat:mtime st-src) (stat:mtime st-com))
                         (<= (stat:mtimensec st-com) (stat:mtimensec st-src))))))
      (compile-file path #:from 'lokke))
    (load-compiled compiled)))

(define (exported-bindings module-name)
  (cons module-name
        (module-map (lambda (name var) name)
                    (resolve-interface module-name))))

(define (compile-uninstantiated form env)
  (tree->tree-il form env '()))

(define (compile form env)
  (compile-uninstantiated (clj-instances->literals form) env))

;; FIXME: there's probably a better way to write this...
(define-syntax clj-defmacro
  (lambda (x)
    (syntax-case x ()
      ((_ name arity-or-arities ...)
       (with-syntax ((fn (datum->syntax x 'fn)))
         (dbg "clj-defmacro ~s\n" #'name)
         #'(begin
             (define-syntax name
               (lambda (x)
                 (syntax-case x ()
                   ((_ macro-args (... ...))
                    (let* ((dummy (dbg "expanding defmacro ~s\n" 'name))
                           (dummy (for-each (lambda (x) (dbg "  ~s\n" x))
                                            (list #'(macro-args (... ...)))))
                           (xform '(fn arity-or-arities ...))
                           (dummy (dbg "macro xform raw ~s\n" xform))
                           (xform (compile-uninstantiated xform (current-module)))
                           (dummy (dbg "macro xform compiled ~s\n" xform))
                           (xform (base-compile xform
                                                #:from 'tree-il
                                                #:to 'value
                                                #:env (current-module)))
                           (dummy (dbg "macro xform value ~s\n" xform))
                           (dummy (dbg "defmacro prep clj args\n"))
                           (clj-args (map (lambda (macro-arg)
                                            (/lokke/prep-form-for-clj-macro
                                             (syntax->datum macro-arg)))
                                          #'(macro-args (... ...))))
                           (dummy (dbg "defmacro clj-args\n"))
                           (dummy (for-each (lambda (x) (dbg "  ~s\n" x))
                                            clj-args))
                           (code (apply xform clj-args))
                           (dummy (dbg "defmacro generated code\n"))
                           (dummy (dbg "~s\n" code))
                           (code (/lokke/convert-form-from-clj-macro code))
                           (dummy (dbg "defmacro final scheme ~s\n" code))
                           (code (datum->syntax x code)))
                      code)))))
             (export name)))))))
