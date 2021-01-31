;;; Copyright (C) 2019-2021 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

;;; A quote that can handle clojure literal values, e.g. [] #{} ...

(define-module (lokke base quote)
  #:duplicates (merge-generics replace warn-override-core warn last)
  #:use-module ((lokke base metadata) #:select (with-meta))
  #:use-module ((lokke reader literal)
                #:select (reader-hash-map-elts
                          reader-hash-set-elts
                          reader-vector-elts))
  #:use-module (oop goops)
  #:export (/lokke/reader-hash-map
            /lokke/reader-hash-set
            /lokke/reader-vector
            clj-quote
            synerr))

(define (convert-for-public-message expr)
  (define (convert expr)
    (cond
     ((null? expr) expr)
     ((list? expr)
      (case (car expr)
        ((/lokke/reader-vector)
         (cons (quote vector) (map convert (reader-vector-elts expr))))
        ((/lokke/reader-hash-map)
         (cons (quote hash-map) (map convert (reader-hash-map-elts expr))))
        ((/lokke/reader-hash-set)
         (cons (quote hash-set) (map convert (reader-hash-set-elts expr))))
        (else (map convert-for-public-message expr))))
     (else expr)))
  (convert expr))

(define-syntax synerr
  (syntax-rules ()
    ((_ name exp msg)
     (error (format #f "~s: ~a in form ~s" name msg
                    (convert-for-public-message (syntax->datum exp)))))))

;; If we eventually have a lower-level module for vector, hash-map,
;; and hash-set (to avoid circular references via ns, etc.), we could
;; just use-module above and avoid needing the direct @ refs here.

(define-syntax-rule (/lokke/reader-hash-map meta exp ...)
  (with-meta ((@ (lokke hash-map) hash-map) exp ...) meta))

(define-syntax-rule (/lokke/reader-hash-set meta exp ...)
  (with-meta ((@ (lokke hash-set) hash-set) exp ...) meta))

(define-syntax-rule (/lokke/reader-vector meta exp ...)
  (with-meta ((@ (lokke vector) vector) exp ...) meta))

(define-syntax-rule (/lokke/reader-meta x ...)
  (warn (format #f "Ignoring metadata in unsupported position: ~s"
                '(/lokke/reader-meta x ...))))

(define-syntax clj-quote
  ;; Note that ~ and ~@ (i.e. unquote and unquote-splicing) still
  ;; expand symbols inside clj-quoted forms, matching the JVM, but that's
  ;; handled by the reader.
  ;;
  ;; FIXME: could perhaps rewrite to scan, and just %scm-quote the whole value
  ;; if the form really is "const", i.e. has no internal maps/sets/vectors.
  (lambda (x)
    (syntax-case x (/lokke/reader-hash-map
                    /lokke/reader-hash-set
                    /lokke/reader-vector)

      ((_ (/lokke/reader-vector meta exp ...))
       #`(/lokke/reader-vector #,@(map (lambda (e) #`(clj-quote #,e)) #'(meta exp ...))))

      ((_ (/lokke/reader-hash-map meta exp ...))
       #`(/lokke/reader-hash-map #,@(map (lambda (e) #`(clj-quote #,e)) #'(meta exp ...))))

      ((_ (/lokke/reader-hash-set meta exp ...))
       #`(/lokke/reader-hash-set #,@(map (lambda (e) #`(clj-quote #,e)) #'(meta exp ...))))

      ((_ (/lokke/reader-vector))
       (synerr "clj-quote" x "internal error, reader vector missing argument"))
      ((_ (/lokke/reader-hash-map))
       (synerr "clj-quote" x "internal error, reader hash-map missing argument"))
      ((_ (/lokke/reader-hash-set))
       (synerr "clj-quote" x "internal error, reader hash-set missing argument"))

      ;; Explicitly match #nil, so it doesn't match (exp ...) below.
      ((_ nil) (eq? #nil (syntax->datum #'nil))
       #nil)

      ((_ (exp ...)) #`(list #,@(map (lambda (e) #`(clj-quote #,e)) #'(exp ...))))

      ;; "leaf" value, including ()
      ((_ x) #'(quote x)))))
