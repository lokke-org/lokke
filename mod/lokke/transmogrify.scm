;;; Copyright (C) 2015-2021 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

(define-module (lokke transmogrify)
  #:use-module ((ice-9 match) #:select (match))
  #:use-module ((ice-9 pretty-print) #:select (pretty-print))
  #:use-module ((lokke reader literal)
                #:select (reader-hash-map
                          reader-hash-map-elts
                          reader-hash-map-meta
                          reader-hash-set
                          reader-hash-set-elts
                          reader-hash-set-meta
                          reader-tagged
                          reader-tagged?
                          reader-tagged-data
                          reader-tagged-tag
                          reader-vector
                          reader-vector-elts
                          reader-vector-meta))
  #:use-module ((lokke base collection) #:select (seq seq? seq->scm-list))
  #:use-module ((lokke base map-entry) #:select (key map-entry? val))
  #:use-module ((lokke base metadata) #:select (meta with-meta))
  #:use-module ((lokke hash-map) #:select (hash-map hash-map? kv-list))
  #:use-module ((lokke hash-set) #:select (hash-set? set))
  #:use-module ((lokke ns) #:select (find-ns))
  #:use-module ((lokke scm vector)
                #:select (lokke-vec lokke-vector? lokke-vector->list))
  #:use-module ((lokke symbol)
                #:select (ns-sym->mod-name
                          parse-symbol
                          parsed-sym-ns
                          parsed-sym-ref))
  #:use-module ((srfi srfi-1) #:select (find first second))
  #:use-module ((srfi srfi-69)
                #:select (hash-table-ref hash-table-set! make-hash-table))
  #:use-module (oop goops)
  #:export (/lokke/ref-scoped-sym-if-syntax
            add-tagged-element
            clj-instances->literals
            instantiate-tagged
            literals->clj-instances
            literals->scm-instances
            preserve-meta-if-new!
            quote-empty-lists)
  #:duplicates (merge-generics replace warn-override-core warn last))

;; Note that some of the null? checks here are doing dual duty for
;; both nil and ().

(define debug-transmogrify? #f)

(define (items->alist . alternating-keys-and-values)
  (let loop ((kvs alternating-keys-and-values)
             (result '()))
    (cond
     ((null? kvs) result)
     ((null? (cdr kvs)) (error "No value for key:" (car kvs)))
     (else (loop (cddr kvs)
                 (cons (cons (car kvs) (cadr kvs)) result))))))

(define (preserve-meta-if-new! orig maybe-new)
  (cond
   ((eq? orig maybe-new) orig)
   ((null? (meta orig)) maybe-new)  ;; FIXME: do we want to include '()?
   (else ((@@ (lokke metadata) with-meta) maybe-new (meta orig)))))

(define instantiators (make-fluid (make-hash-table eq?)))
(define uninstantiators (make-fluid '()))

(define (add-tagged-element tag pred instantiator uninstantiator)
  (hash-table-set! (fluid-ref instantiators) tag instantiator)
  (let* ((orig (fluid-ref uninstantiators))
         (new (assoc-set! orig (cons tag pred) uninstantiator)))
    (unless (eq? new orig)
      (fluid-set! uninstantiators new))))

(define (tag-instantiator tag)
  (hash-table-ref (fluid-ref instantiators) tag (lambda () #f)))

(define (instantiate-tagged tag data)
  (let ((instantiate (tag-instantiator tag)))
    (unless instantiate
      (error (string-append "Unknown tagged element #" (symbol->string tag))))
    (instantiate data)))

(define (uninstantiator x)
  ;; Traverse the uninstantiator info, checking (pred x) for a match
  (let ((info (find (lambda (entry) ((cdar entry) x))
                    (fluid-ref uninstantiators))))
    (and info info)))


(define-syntax /lokke/ref-scoped-sym-if-syntax
  (lambda (x)
    (syntax-case x ()
      ((_ sym ns-sym ref-sym)
       (let* ((ns-d (syntax->datum #'ns-sym))
              (n (find-ns ns-d)))
         (if n
             (let* ((v (module-variable n (syntax->datum #'ref-sym))))
               (if v
                   (if (macro? (variable-ref v))
                       (let* ((mod (ns-sym->mod-name ns-d))
                              (mod (datum->syntax #'sym mod)))
                         #`(@ #,mod ref-sym))
                       #'sym)
                   #'sym))
             #'sym))))))

(define (expose-scoped-refs-to-expander s)
  ;; cf. (lokke boot)
  (let* ((p (parse-symbol s))
         (n (parsed-sym-ns p))
         (r (parsed-sym-ref p)))
    (if (and n r)
        (list '/lokke/ref-scoped-sym-if-syntax s n r)
        s)))

(define (literals->clj-instances expr)
  (define (convert expr)
    (preserve-meta-if-new!
     expr
     (cond
      ((null? expr) expr)
      ((list? expr)
       (case (car expr)
         ((/lokke/ref-scoped-sym-if-syntax) (cadr expr))
         ((/lokke/reader-vector)
          (with-meta (lokke-vec (map convert (reader-vector-elts expr)))
                     (convert (reader-vector-meta expr))))
         ((/lokke/reader-hash-map)
          (with-meta (apply hash-map (map convert (reader-hash-map-elts expr)))
                     (convert (reader-hash-map-meta expr))))
         ((/lokke/reader-hash-set)
          (with-meta (set (map convert (reader-hash-set-elts expr)))
                     (convert (reader-hash-set-meta expr))))
         ((/lokke/reader-tagged)
          (instantiate-tagged (reader-tagged-tag expr)
                              (convert (reader-tagged-data expr))))
         (else (map convert expr))))
      (else expr))))
  (convert expr))

(define (literals->scm-instances expr)
  (define (convert expr)
    (preserve-meta-if-new!
     expr
     (cond
      ((null? expr) expr)
      ((list? expr)
       (case (car expr)
         ((/lokke/reader-vector)
          (apply vector (map convert (reader-vector-elts expr))))
         ((/lokke/reader-hash-map)
          (items->alist (map convert (reader-hash-map-elts expr))))
         ((/lokke/reader-hash-set)  ;; list for srfi-1
          (list (map convert (reader-hash-set-elts expr))))
         ((/lokke/reader-tagged)
          (instantiate-tagged (reader-tagged-tag expr)
                              (convert (reader-tagged-data expr))))
         (else (map convert expr))))
      (else expr))))
  (convert expr))

(define (clj-instances->literals expr)
  ;; This also converts seqs to scheme lists
  (define (convert expr)
    (preserve-meta-if-new!
     expr
     (cond
      ((symbol? expr) (expose-scoped-refs-to-expander expr))
      ((null? expr) expr)
      ((string? expr) expr)
      ((number? expr) expr)
      ((keyword? expr) expr)
      ((boolean? expr) expr)
      ((pair? expr)
       (if (eq? (car expr) '/lokke/ref-scoped-sym-if-syntax)
           expr
           (cons (convert (car expr)) (convert (cdr expr)))))
      ((lokke-vector? expr)
       (apply reader-vector (convert (meta expr))
              (map convert (lokke-vector->list expr))))
      ((hash-map? expr)
       (apply reader-hash-map (convert (meta expr))
              (map convert (kv-list expr))))
      ((map-entry? expr)
       (reader-vector (convert (meta expr))
                      (convert (key expr))
                      (convert (val expr))))
      ((hash-set? expr)
       (apply reader-hash-set (convert (meta expr))
              (map convert (seq->scm-list (seq expr)))))
      ((seq? expr)
       (map convert (seq->scm-list expr)))
      (else
       (match (uninstantiator expr)
         (((tag . pred) . uninstantiate)
          (reader-tagged tag (uninstantiate expr)))
         (_ (error "Unexpected expression while uninstantiating literals:"
                   expr (class-of expr) (list? expr))))))))
  (when debug-transmogrify?
    (display "uninstantiate:\n" (current-error-port))
    (pretty-print expr (current-error-port)))
  (let ((result (convert expr)))
    (when debug-transmogrify?
      (display "uninstantiated:\n" (current-error-port) )
      (pretty-print expr (current-error-port))
      (display "  =>\n" (current-error-port))
      (pretty-print result (current-error-port)))
    result))

(define (quote-empty-lists expr)
  ;; Handle clojure's ().  Convert it to '() when compiling so it's
  ;; not interpreted as an empty procedure call (i.e. syntax error).
  (define (convert expr)
    (preserve-meta-if-new!
     expr
     (cond
      ((eq? '() expr) '(quote ()))
      ;; FIXME: need syntax-quote recursion?  See DESIGN TODO.
      ((list? expr)
       (cond
        ((null? expr) expr)
        ((memq (car expr) '(quote syntax-quote)) expr)
        (else (map convert expr))))
      (else expr))))
  (convert expr))
