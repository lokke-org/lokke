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

(define-module (lokke transmogrify)
  #:use-module ((ice-9 pretty-print) #:select (pretty-print))
  #:use-module ((lokke collection) #:select (seq? seq->scm-list))
  #:use-module ((lokke hash-map) #:select (hash-map hash-map? kv-list))
  #:use-module ((lokke hash-set) #:select (hash-set? into set))
  #:use-module ((lokke metadata) #:select (meta))
  #:use-module ((lokke scm vector)
                #:select (lokke-vec lokke-vector? lokke-vector->list))
  #:use-module (oop goops)
  #:export (clj-instances->literals
            literals->clj-instances
            literals->scm-instances
            preserve-meta-if-new!
            quote-empty-lists)
  #:duplicates (merge-generics replace warn-override-core warn last))

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
   ((nil? (meta orig)) maybe-new)
   (else ((@@ (lokke metadata) with-meta) maybe-new (meta orig)))))

(define (literals->clj-instances expr)
  (define (convert expr)
    (preserve-meta-if-new!
     expr
     (cond
      ((null? expr) expr)
      ((list? expr)
       (case (car expr)
         ((/lokke/reader-hash-set) (set (map convert (cdr expr))))
         ((/lokke/reader-vector) (lokke-vec (map convert (cdr expr))))
         ((/lokke/reader-hash-map)
          (apply hash-map (map convert (cdr expr))))
         (else (map literals->clj-instances expr))))
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
         ((/lokke/reader-hash-set) (list (map convert (cdr expr))))  ;; for srfi-1
         ((/lokke/reader-vector) (apply vector (map convert (cdr expr))))
         ((/lokke/reader-hash-map) (items->alist (map convert (cdr expr))))
         (else (map literals->scm-instances expr))))
      (else expr))))
  (convert expr))

(define (clj-instances->literals expr)
  ;; This also converts seqs to scheme lists
  (define (convert expr)
    (preserve-meta-if-new!
     expr
     (cond
      ((symbol? expr) expr)
      ((null? expr) expr)
      ((string? expr) expr)
      ((number? expr) expr)
      ((keyword? expr) expr)
      ((boolean? expr) expr)
      ((pair? expr) (cons (convert (car expr)) (convert (cdr expr))))
      ;;((list? expr) (map convert expr))
      ((lokke-vector? expr) `(/lokke/reader-vector ,@(map convert (lokke-vector->list expr))))
      ((hash-map? expr) `(/lokke/reader-hash-map ,@(map convert (kv-list expr))))
      ((hash-set? expr) `(/lokke/reader-hash-set ,@(into '() (map convert expr))))
      ((seq? expr)
       (map convert (seq->scm-list expr)))
      (else (error "Unexpected expression while uninstantiating literals:"
                   expr (class-of expr) (list? expr))))))
  (when debug-transmogrify?
    (format (current-error-port) "uninstantiate:\n")
    (pretty-print expr (current-error-port)))
  (let ((result (convert expr)))
    (when debug-transmogrify?
      (format (current-error-port) "uninstantiated:\n")
      (pretty-print expr (current-error-port))
      (format (current-error-port) "  =>\n")
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
      ((list? expr) (if (nil? expr) expr (map convert expr)))
      (else expr))))
  (convert expr))
