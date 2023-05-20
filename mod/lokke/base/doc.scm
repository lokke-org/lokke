;;; Copyright (C) 2019-2020 2023 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

(define-module (lokke base doc)
  #:use-module ((ice-9 control) #:select (call/ec))
  #:use-module ((ice-9 documentation) #:select (object-documentation))
  #:use-module ((lokke base util) #:select (global-identifier?))
  #:use-module ((lokke ns) #:select (find-ns ns-name ns-resolve))
  #:use-module ((lokke pr) #:select (print println))
  #:use-module ((lokke symbol)
                #:select (parse-symbol parsed-sym-ns parsed-sym-ref))
  #:export (clear-var-doc! doc set-fn-doc! set-var-doc!))

;; Currently def documentation strings are associated with the guile
;; variable itself (via guile's 'documentation property) for top-level
;; definitions (i.e. the top-level "box" holding the defined value).
;; Functions created via defn also have their procedure-documentation
;; set.

(define (set-fn-doc! f doc)
  (set-procedure-property! f 'documentation doc)
  f)

(define (set-var-doc! v doc)
  (set-object-property! v 'documentation doc))

(define (clear-var-doc! v)
  ;; FIXME: need guile to provide clear-object-property! or similar
  (set-object-property! v 'documentation #f))

(define (show-doc name doc)
  (println "-------------------------")
  (when name (println name))
  (when doc (println " " doc))
  #nil)

(define (find-source-module m var)
  "Returns a module where var is defined that is reachable from module
m (including m), or #f."
  (call/ec
   (lambda (exit)
     (define (find-var m _)
       (module-for-each (lambda (sym v) (when (eq? v var) (exit m))) m)
       #f)
     (module-search find-var m #f))))

;; Can't just rely on (ice-9 documentation object-documentation) right
;; now.  The main issue is that it always falls back to
;; guile-procedures.txt via search-documentation files if the
;; procedure has a 'name property, which is set by calls like
;; define-generic.  So you'll get (guile) docs for 'assoc even if your
;; assoc is completely different.  It sounded like the
;; guile-procedures.txt strings might now be in boot-9.go (as with
;; other .go files).  If so, then another option might be for guile to
;; rely on them instead.
;;
;; FIXME: propose changes upstream

(define (non-guile-proc-doc p)
  ;; If it has a name, guile may have cached the (incorrect) file text
  ;; as the documentation -- the (guile) docstring.
  (and (not (procedure-name p))
       (or (procedure-documentation p)
           (object-property p 'documentation))))

(define guile-module (resolve-module '(guile)))

(define (var-doc source-module var)
  ;; First check is fine because guile has no docs on vars right now
  (or (object-property var 'documentation)
      (let ((x (variable-ref var)))
        (if (eq? source-module guile-module)
          (object-documentation x)
          (cond
           ((procedure? x) (non-guile-proc-doc x))
           ((macro? x) (non-guile-proc-doc (macro-transformer x)))
           (else #f))))))

(define (show-def-doc sym)
  (let* ((parsed (parse-symbol sym))
         (ns (parsed-sym-ns parsed))
         (ref (parsed-sym-ref parsed))
         (m (if ns (find-ns ns) (current-module)))
         (nsn (ns-name m))
         (var (ns-resolve m ref))
         (sm (find-source-module m var)))
    (if (not sm)
        #nil
        (show-doc (string-append (symbol->string (ns-name sm))
                                 "/" (symbol->string ref))
                  (var-doc sm var)))))

(define-syntax doc
  (lambda (x)
    (syntax-case x ()
      ((_ what)
       (if (global-identifier? #'what)
           #`(show-def-doc 'what)
           #nil)))))
