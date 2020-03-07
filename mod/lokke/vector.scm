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

(define-module (lokke vector)
  #:use-module (oop goops)
  #:use-module ((lokke collection)
                #:select (<coll>
                          <seq>
                          assoc
                          coll?
                          conj
                          const-nth?
                          contains?
                          count
                          counted?
                          empty
                          empty?
                          find
                          first
                          get
                          nth
                          reduce
                          rest
                          seq
                          update))
  #:use-module ((lokke base collection) #:select (define-nth-seq))
  #:use-module ((lokke base map-entry) #:select (map-entry))
  #:use-module ((lokke base util) #:select (require-nil))
  #:use-module ((lokke compare) #:select (clj= compare))
  #:use-module ((lokke compat) #:select (re-export-and-replace!))
  #:use-module ((lokke hash-map) #:select (<hash-map>))
  #:use-module ((lokke metadata) #:select (meta with-meta))
  #:use-module ((lokke pr) #:select (pr-on print-on))
  #:use-module ((lokke scm vector)
                #:select (<lokke-vector>
                          list->lokke-vector
                          lokke-vec
                          lokke-vector
                          lokke-vector->list
                          lokke-vector-assoc
                          lokke-vector-conj
                          lokke-vector-equal?
                          lokke-vector-length
                          lokke-vector-meta
                          lokke-vector-ref
                          lokke-vector-with-meta
                          lokke-vector?
                          vector->lokke-vector))
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module ((srfi srfi-67) #:select (vector-compare))
  #:replace (vector vector?)
  #:export (vec)
  #:re-export (clj=
               compare
               conj
               const-nth?
               contains?
               count
               counted?
               empty
               empty?
               find
               first
               get
               meta
               nth
               pr-on
               print-on
               rest
               seq
               with-meta
               update)
  #:duplicates (merge-generics replace warn-override-core warn last))

(re-export-and-replace! 'assoc)


;;; <lokke-vector>

(define vector? lokke-vector?)

(define-method (const-nth? (v <lokke-vector>)) #t)

(define-method (meta (v <lokke-vector>)) (lokke-vector-meta v))

(define-method (with-meta (m <lokke-vector>) (mdata <boolean>))
  (require-nil 'with-meta 2 mdata)
  (lokke-vector-with-meta m mdata))

(define-method (with-meta (m <lokke-vector>) (mdata <hash-map>))
  (lokke-vector-with-meta m mdata))

;; specialize this so that we'll bypass the generic <sequential> flavor
(define-method (clj= (v1 <lokke-vector>) (v2 <lokke-vector>))
  (lokke-vector-equal? v1 v2))

(define-method (compare (v1 <lokke-vector>) (v2 <lokke-vector>))
  (vector-compare compare v1 v2 lokke-vector-length lokke-vector-ref))

(define-method (conj (v <lokke-vector>) . xs)
  (fold (lambda (x result) (lokke-vector-conj result x))
        v
        xs))

(define empty-vector (@@ (lokke scm vector) lokke-empty-vector))

(define (vector . items) (list->lokke-vector items))
(define (vec coll) (reduce lokke-vector-conj empty-vector coll))

(define (show-vector v len nth emit port)
  (if (zero? len)
      (display "[]" port)
      (begin
        (display "[" port)
        (emit (nth v 0) port)
        (do ((i 1 (1+ i)))
            ((= i len))
          (display " " port)
          (emit (nth v i) port))
        (display "]" port))))

(define-method (pr-on (v <lokke-vector>) port)
  (show-vector v (lokke-vector-length v) lokke-vector-ref pr-on port))

(define-method (print-on (v <lokke-vector>) port)
  (show-vector v (lokke-vector-length v) lokke-vector-ref print-on port))

(define-method (counted? (v <lokke-vector>)) #t)
(define-method (count (v <lokke-vector>)) (lokke-vector-length v))
(define-method (empty (v <lokke-vector>)) empty-vector)
(define-method (empty? (v <lokke-vector>)) (zero? (lokke-vector-length v)))

(define-method (assoc (v <lokke-vector>) . indexes-and-values)
  (apply lokke-vector-assoc v indexes-and-values))

(define (valid-index? v i)
  (and (integer? i) (>= i 0) (< i (lokke-vector-length v))))

(define-method (contains? (v <lokke-vector>) i)
  (valid-index? v i))

(define-method (nth (v <lokke-vector>) i) (lokke-vector-ref v i))
(define-method (nth (v <lokke-vector>) i not-found)
  (lokke-vector-ref v i not-found))

(define-method (get (v <lokke-vector>) i) (lokke-vector-ref v i #nil))
(define-method (get (v <lokke-vector>) i not-found)
  (lokke-vector-ref v i not-found))

(define-method (find (v <lokke-vector>) i)
  (if (valid-index? v i)
      (map-entry i (lokke-vector-ref v i))
      #nil))

(define-method (update (v <lokke-vector>) i f . args)
  (lokke-vector-assoc v i (f (lokke-vector-ref v i #nil))))


(define-nth-seq <lokke-vector-seq>
  lokke-vector-length lokke-vector-ref)

(define-method (seq (v <lokke-vector>))
  (if (zero? (lokke-vector-length v))
      #nil
      (make <lokke-vector-seq> #:items v)))
