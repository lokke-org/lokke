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

;;; Namespace underpinnings

(read-set! keywords 'postfix)  ;; srfi-88

(define-module (lokke ns)
  use-module: ((lokke compile)
               select: (literals->scm-instances unexpand-symbols))
  use-module: ((lokke symbol)
               select: (parse-symbol
                        scoped-sym-ns
                        scoped-sym-ref
                        scoped-sym-symbol))
  use-module: ((lokke scm vector)
               select: (lokke-vector? lokke-vector->list))
  use-module: ((lokke symbol) select: (scoped-sym? simple-symbol?))
  use-module: ((srfi srfi-1) select: (every find proper-list? take drop))
  use-module: ((srfi srfi-9 gnu) select: (define-immutable-record-type))
  use-module: ((system base compile) select: (compile-file compiled-file-name))
  export: (*ns*
           create-ns
           default-environment
           find-ns
           fix-let
           in-ns
           ns
           ns-name
           re-export-all!
           refer
           refer-clojure
           require
           use))

;; FIXME: assuming we need to, does guile handle locking/blocking wrt
;; module operations, i.e. simultaneous resolve, etc.

(define debug-ns? #f)

(define dbgf
  (if debug-ns?
      (lambda args (apply format (current-error-port) args))
      (lambda args #t)))

(define dbgwn
  (if debug-ns?
      (lambda (item . items)
        (write item (current-error-port))
        (for-each (lambda (x)
                    (display " " (current-error-port))
                    (write x (current-error-port)))
                  items)
        (newline (current-error-port)))
      (lambda args #t)))

;; FIXME: doesn't seem like quite the right abstraction/name...
(define (scoped-sym-module s)
  (when (scoped-sym-ns s)
    (error "qualified symbol cannot be an ns reference" (scoped-sym-symbol s)))
  (let ((ns-sym (scoped-sym-ref s)))
    (unless ns-sym
      (error "invalid ns reference symbol" (scoped-sym-symbol s)))
    (let* ((ns (map string->symbol (string-split (symbol->string ns-sym) #\.))))
      (if (eq? 'guile (car ns))
          (cdr ns)
          (apply list 'lokke 'ns ns)))))

(define (re-export-all! module-name)
  (module-re-export! (current-module)
                     (module-map (lambda (name var) name)
                                 (resolve-interface module-name))))

(define (module-name->ns-name m)
  (string->symbol
   (string-join (map symbol->string
                     (if (and (> (length m) 2)
                              (equal? '(lokke ns) (take m 2)))
                         (drop m 2)
                         (cons 'guile m)))
                ".")))

(define (ns-name n)
  (module-name->ns-name (module-name n)))

(define (parse-ns-ref ref)
  (dbgf "parse-ns-ref: ~s\n" ref)
  (dbgf "parse-ns-ref: ~s\n" (parse-symbol ref))
  (cond
   ((symbol? ref) (parse-symbol ref))
   (else (error "Not a namespace reference:" ref))))

(define-syntax *ns*
  (identifier-syntax
   (var (current-module))
   ((set! var val) (set-current-module val))))

(define (core-module? name)
  (and (>= (length name) 4)
       (equal? '(lokke ns clojure core) (take name 4))))

(define (find-module-src mod-name)
  (dbgwn #:mod-name mod-name)
  (let* ((path-fragment (string-join (map symbol->string mod-name) "/"))
         (path (search-path %load-path path-fragment '(".clj" ".scm"))))
    (and path
         (let ((ext (substring/read-only path (- (string-length path) 4))))
           (cond
            ((string=? ".clj" ext) path)
            ((string=? ".scm" ext) path)
            (else #f))))))

(define (exports->import-set module-name)
  (cons module-name
        (module-map (lambda (name var) name)
                    (resolve-interface module-name))))

(define (bootstrap-environment)
  ;; Learned the hard way -- even though some of Guile's example
  ;; languages and/or the docs use a throwaway module for each call,
  ;; that doesn't work for us, i.e. the first compile/use will
  ;; succeed, but a second run (with a new heap) will crash looking
  ;; for the gensym-ish module name.  It would make sense that guile
  ;; might embed the module name in the compiled file for path lookup
  ;; on the next run, so we'll just root everything at '(lokke user)
  ;; for now.  However, I'm not certain that other problems weren't
  ;; interfering, so if it matters, we might want to investigate
  ;; again.

  ;; As an alternative, could we just have separate bootstrap and user
  ;; modules, either both scm or one scm and one clj, and would that
  ;; be any better?
  (resolve-module '(lokke user) #:ensure #f))

(define bootstrapped-compiler? #f)

(define (maybe-resolve-ns-module name)
  ;; For now, only handles list names, e.g. (lokke ns clojure core).

  (define (compile-it path lang env)
    (when (eq? 'lokke lang)
      (unless bootstrapped-compiler?
        ;; bootstrap - load spec so we can (compile ... #:from lokke)
        (unless (resolve-module '(language lokke spec) #:ensure #f)
          (error "Unable to define lokke language via (language lokke spec)"))
        (set! bootstrapped-compiler? #t)))
    (dbgf "lokke compiling as ~s ~s\n" lang path)
    (let ((result (if env
                      (compile-file path #:from lang #:env env)
                      (compile-file path #:from lang))))
      (dbgf "lokke compiled as ~s ~s\n" lang path)
      result))

  (dbgf "resolving: ~s\n" name)
  (when (scoped-sym? name)
    (error "Scoped sym?" name))
  (unless (pair? name)
    (error "Name must be a list, e.g. (lokke ns clojure core), not" name))
  (let ((env #f))
    (cond
     ;; Anything at or under clojure.core is part of the bootstrap set.
     ((core-module? name)
      (dbgf "bootstrapping: ~s\n" name)
      (set! env (bootstrap-environment)))
     ((and (> (length name) 2)
           (equal? '(lokke ns) (take name 2)))
      ;; If this function (resolve-ns-module) is effectively only
      ;; used by require/use, and they always refer-clojure then we
      ;; may not need this here.
      (dbgf "ensuring clojure.core for: ~s\n" name)
      (unless (resolve-ns-module '(lokke ns clojure core))
        (error "Unable to resolve clojure.core for" name))))
    (dbgf "find and load ~s\n" name)
    (let ((mod (resolve-module name #f #:ensure #f)))
      (if mod
          (begin
            (dbgf "resolved: ~s -> ~s\n" name mod)
            mod)
          (let ((path (find-module-src name)))
            (dbgf "path: ~s\n" path)
            (if (not path)
                (begin
                  (dbgf "resolved: ~s -> nil\n" name)
                  #nil)
                (let* ((compiled (compiled-file-name path)))
                  (dbgf "found src: ~s\n" path)
                  (when (or (not (file-exists? compiled))
                            (let* ((st-src (stat path))
                                   (st-com (stat compiled)))
                              (or (<= (stat:mtime st-com) (stat:mtime st-src))
                                  (and (= (stat:mtime st-src) (stat:mtime st-com))
                                       (<= (stat:mtimensec st-com) (stat:mtimensec st-src))))))
                    (dbgf "compiling in ~s: ~s\n" env path)
                    (let* ((ext (substring/read-only path (- (string-length path) 4)))
                           (lang (cond
                                  ((string=? ".clj" ext) 'lokke)
                                  ((string=? ".scm" ext) 'scheme)
                                  (else
                                   (error "Unexpected namespace source file extension:" path)))))
                      (compile-it path lang env)))
                  (dbgf "loading: ~s\n" compiled)
                  (save-module-excursion (lambda () (load-compiled compiled)))
                  (let ((mod (resolve-module name #f #:ensure #f)))
                    (unless mod
                      (error (format #f "Namespace ~s not defined by ~s"
                                     (module-name->ns-name name)
                                     compiled)))
                    (dbgf "resolved: ~s -> ~s\n" name mod)
                    mod))))))))

(define (resolve-ns-module name)
  (let ((m (maybe-resolve-ns-module name)))
    (unless m
      (error (format #f "Unable to find module ~s for namespace ~s"
                     name
                     (module-name->ns-name name))))
    m))

(define (find-ns ns-sym)
  (let ((ns (maybe-resolve-ns-module (scoped-sym-module (parse-ns-ref ns-sym)))))
    (if ns ns #nil)))


;; FIXME: add warning on collisions?  Docs claim exception will be
;; thrown, but jvm doesn't, just warns.

(define-immutable-record-type <ns-dep-spec>
  (make-ns-dep-spec module alias select hide)
  ns-dep-spec?
  (module ns-dep-spec-module)
  (alias ns-dep-spec-alias)
  (select ns-dep-spec-select)
  (hide ns-dep-spec-hide))

;; FIXME: unify these?

(define (dep-spec-coll->list x)
    (cond
     ((proper-list? x) x)
     ((lokke-vector? x) (lokke-vector->list x))
     (else #f)))


(define (refer-spec->list x)
  ;; FIXME: did we intend for this to support scm vectors?
  (let* ((lst (cond
               ((proper-list? x) (cond
                                  ((null? x) x)
                                  ((eq? '/lokke/reader-vector (car x)) (cdr x))
                                  (else x)))
               ((lokke-vector? x) (lokke-vector->list x))
               ((vector? x) (vector->list x))
               (else #f))))
    (and lst
         (every simple-symbol? lst)
         lst)))

(define (ref-item->interface-spec context item)
  (unless (memq context '(require use))
    (error "Invalid context:" context))
  ;; The suppressions here match observations on the jvm.
  ;; Docs claim exception on collision, but at leat from the repl,
  ;; there's only a warning.
  ;; Allows clojure or scheme vectors
  ;; FIXME: clj only allows vectors for libspecs, but lists or vectors for refer lists
  ;;        might want to require lokke or scm vector instead of list for libspec?
  ;;        i.e. make ambiguities less likely...
  ;; FIXME: handle shared prefixes
  ;; FIXME: handle :reload :reload-all :verbose
  ;; Some of this possibly easier with guile match?
  (let ((it (cond  ;; FIXME: similar to dep-sec-coll->list?  And is this what we want?
             ((symbol? item) (list item))
             ((list? item) item)
             ((lokke-vector? item) (lokke-vector->list item))
             ((vector? item) (vector->list item))
             (else
              (error "Unexpected ns reference type:" item)))))
    (dbgf "item it: ~s ~s\n" item it)
    (when (null? it)
      (error "Empty ns dependency reference:" item))
    (let loop ((specs (cdr it))
               (alias #f)
               (select-src #f)
               (select #f)
               (hide '()))
      (dbgf "loop: ~s ~s ~s ~s ~s\n" specs alias select-src select hide)
      (if (null? specs)
          (let ((name (car it)))
            (unless (symbol? name)
              (error "Unexpected namespace name:" item))
            (make-ns-dep-spec (scoped-sym-module (parse-ns-ref name)) alias select hide))
          (let ((kind (car specs)))
            (case kind
              ((#:as)
               (when (null? (cdr specs))
                 (error "No name for :as in" item))
               (let ((alias (cadr specs)))
                 (unless (simple-symbol? alias)
                   (error ":as alias is not a simple symbol:" alias))
                 (when alias
                   (error "require :as aliases are not yet supported"))
                 (loop (cddr specs) alias select-src select hide)))

              ((#:refer #:only)
               (when (and (eq? 'require context)
                          (eq? #:only kind))
                 (error "Encountered :only in require clause:" item))
               (when (null? (cdr specs))
                 (error (format #f "No values for ~a in" select-src) item))
               (when select-src
                 ;; select should only be false here for prev :refer :all
                 (warn (format #f "~a is suppressing" kind)
                       (cons select-src (or select #:all))))
               (let ((names (cadr specs)))
                 (if (eq? #:all names)
                     (begin
                       (when (eq? #:only kind)
                         (error ":only does not allow :all"))
                       (loop (cddr specs) alias kind #f hide))
                     (let ((names (refer-spec->list names)))
                       (unless names
                         (error "Invalid symbol selection in" item))
                       (loop (cddr specs) alias kind names hide)))))

              ((#:exclude)
               ;; docs make it sound like require doesn't support this,
               ;; but on the jvm it clobbers :refer.
               (when (null? (cdr specs))
                 (error "No :exclude values" item))
               (when select-src
                 ;; select should only be false here for prev :refer :all
                 (warn ":exclude is suppressing" (cons select-src (or select #:all))))
               (let ((names (cadr specs)))
                 (let ((names (dep-spec-coll->list names)))
                   (unless (and names (every simple-symbol? names))
                     (error "Invalid symbol selections in " item))
                   (loop (cddr specs) alias #f #f names))))

              ((#:rename)
               ;; This will likely need to add (name . alias) items to
               ;; the select list, but first we need to figure out how
               ;; occurrences interleaved with :refer and :only should
               ;; behave.
               (error ":rename not implemented yet")
               ;; Looks like the jvm silently ignores these.
               (when (eq? 'require context)
                 (error "Encountered :rename in require clause:" item)))
              (else
               (error "Unexpected require specification in" item))))))))

(define (debug-ref-item->interface-spec context item)
  (let ((result (ref-item->interface-spec context item)))
    (dbgf "interface:\n  ~s\n  ->\n  ~s\n" item result)
    result))

(define (dump-mod-info mod port)
  (format port "module: ~s\n" (module-name mod))
  (let ((pub (module-public-interface mod)))
    (when pub
      (format port "  public:\n")
      (module-map (lambda (name var) (format port "    ~s -> ~s\n" name var))
                  pub)))
  (format port "  private:\n")
  (module-map (lambda (name var)
                (format port "    ~s -> ~s\n" name var))
              mod)
  *unspecified*)


(define (incorporate-deps dep-specs)
  (define (use-spec-mod spec)
    (dbgf "mod-use: ~s ~s\n" (current-module) spec)
    (dbgf "mod-use: ~s\n" (ns-dep-spec-module spec))
    (let ((interface (resolve-interface (ns-dep-spec-module spec))))
      (dbgf "mod-use: ~s\n" interface)
      (dbgf "mod-use: ~s\n" (module-public-interface interface)))
    (module-use-interfaces! (current-module)
                            (list (resolve-interface (ns-dep-spec-module spec)
                                                     #:select (ns-dep-spec-select spec)
                                                     #:hide (ns-dep-spec-hide spec)))))
  (call-with-deferred-observers
   (lambda ()
     (for-each (lambda (s)
                 (resolve-ns-module (ns-dep-spec-module s)))
               dep-specs)
     ;; This *must* come after the resolve above so that the .clj
     ;; modules will be available as compiled .go files for
     ;; resolve-interface to find without autoloading.
     (for-each use-spec-mod dep-specs))))

(define (require . items)
  (dbgf "require -> ~s\n" items)
  (incorporate-deps (map (lambda (x) (debug-ref-item->interface-spec 'require x))
                         items)))

(define (use . items)
  (dbgf "use -> ~s\n" items)
  (incorporate-deps (map (lambda (x) (debug-ref-item->interface-spec 'use x))
                         items))
  (dbgf "used! -> ~s\n" items))

(define (refer ns-sym . filters)
  (dbgf "refer -> ~s ~s\n" ns-sym filters)
  (use (cons ns-sym filters)))

(define (refer-clojure . filters)
  (apply refer 'clojure.core filters))

(define (incorporate-refs-syntax refs core?)
  ;; Can currently expect scheme vectors for say (:require [foo ...])
  (let* ((quotify (lambda (lst) (map (lambda (x) (list 'quote x)) lst)))
         (cmds (map (lambda (ref)
                      (case (car ref)
                        ((#:require) `(require ,@(quotify (cdr ref))))
                        ((#:use) `(use ,@(quotify (cdr ref))))
                        ((#:refer-clojure) `(refer-clojure ,@(quotify (cdr ref))))
                        ;;((#:load) `(load ,@(quotify (cdr ref))))
                        ((#:import) (error ":import is not supported yet"))
                        (else (error "Unrecognized ns directive" ref))))
                    refs))
         (cmds (if (or core?
                       (find (lambda (x) (eq? #:refer-clojure (car x))) refs))
                   cmds
                   (cons '(refer 'clojure.core) cmds))))
    cmds))

(define (clj-syntax->scm syn)
  (literals->scm-instances (unexpand-symbols (syntax->datum syn))))

(define (create-ns name)
  (let* ((mod (scoped-sym-module (parse-ns-ref name)))
         (existing (resolve-ns-module mod)))
    (if existing
        existing
        (define-module* mod
          #:duplicates '(merge-generics replace warn-override-core warn last)
          #:pure))))

(define (in-ns name)
  (dbgf "in-ns: ~s\n" (create-ns name))
  (set-current-module (create-ns name)))


(define default-environment
  (let ((augmented? #f))
    (lambda ()
      (let ((m (bootstrap-environment)))
        (unless augmented?
          (save-module-excursion
           (lambda ()
             (set-current-module m)
             (refer-clojure)))
          (set! augmented? #t))
        m))))

;; For now, this only works for lokke since we can't use a let to
;; capture/restore the current language without hiding the
;; define-module from the compiler (assuming I understood what I saw
;; correctly).  (No longer sure the previous statments are
;; relevant/true...)
(define-syntax ns
  (lambda (x)
    (syntax-case x ()
      ((_ name specs ...)
       (let* ((mod (scoped-sym-module (parse-symbol (clj-syntax->scm #'name))))
              (core? (core-module? mod))
              (specs (clj-syntax->scm #'(specs ...)))
              (ref-cmds (datum->syntax x (incorporate-refs-syntax specs core?))))
         #`(eval-when (expand load eval)
             ;; Only define the module here -- needs to be top level,
             ;; and language will be lokke, so we don't want to let it
             ;; autocompile.  Rely on require/use instead which
             ;; autodetects.
             (define-module #,(datum->syntax x mod)
               #:duplicates (merge-generics replace warn-override-core warn last)
               #:pure)
             ;; FIXME: minimze this set
             (require 'guile.language.lokke.spec)  ;; FIXME: may not be needed
             (use 'guile.lokke.boot)
             (require '(guile.lokke.ns #:refer (refer refer-clojure require use)))
             ;; FIXME: drop after debugging...
             (require '(guile.guile #:refer (format current-error-port)))
             #,@ref-cmds
             *unspecified*))))))
