;;; Copyright (C) 2019-2020 Rob Browning <rlb@defaultvalue.org>
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

;; This is the lowest level, supporting *everything*, including
;; definitions required by code generated by the compiler, etc., and
;; providing bits needed to bootstrap the system by compiling
;; clojure.core, i.e. (lokke ns clojure core).

(define-module (lokke boot)
  #:use-module ((guile) #:select ((quote . %scm-quote)))
  #:use-module ((lokke ns) #:select (ns))
  #:export (/lokke/reader-hash-map
            /lokke/reader-hash-set
            /lokke/reader-vector
            syntax-quote)
  #:re-export (ns quasiquote unquote unquote-splicing)
  #:replace (quote)
  #:duplicates (merge-generics replace warn-override-core warn last))


(define-syntax-rule (syntax-quote form)
  (quasiquote form))


;; If we eventually have a lower-level module for vector, hash-map,
;; and hash-set (to avoid circular references via ns, etc.), we could
;; just use-module above and avoid needing the direct @ refs here.

(define-syntax-rule (/lokke/reader-hash-map x ...)
  ((@ (lokke hash-map) hash-map) x ...))

(define-syntax-rule (/lokke/reader-hash-set x ...)
  ((@ (lokke hash-set) hash-set) x ...))

(define-syntax-rule (/lokke/reader-vector x ...)
  ((@ (lokke vector) vector) x ...))


(define-syntax quote
  ;; FIXME: could perhaps rewrite to scan, and just %scm-quote the whole value
  ;; if the form really is "const", i.e. has no internal maps/sets/vectors.
  (lambda (x)
    (syntax-case x (/lokke/reader-hash-map
                    /lokke/reader-hash-set
                    /lokke/reader-vector)

      ((_ (/lokke/reader-vector exp ...))
       #`(/lokke/reader-vector #,@(map (lambda (e) #`(quote #,e)) #'(exp ...))))

      ((_ (/lokke/reader-hash-map exp ...))
       #`(/lokke/reader-hash-map #,@(map (lambda (e) #`(quote #,e)) #'(exp ...))))

      ((_ (/lokke/reader-hash-set exp ...))
       #`(/lokke/reader-hash-set #,@(map (lambda (e) #`(quote #,e)) #'(exp ...))))

      ((_ (exp ...)) #`(list #,@(map (lambda (e) #`(quote #,e)) #'(exp ...))))

      ;; "leaf" value, including ()
      ((_ x) #'(%scm-quote x)))))
