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

(define-module (lokke scm vector)
  #:use-module ((ice-9 format) #:select (format))
  #:use-module ((ice-9 match) #:select (match))
  #:use-module ((lokke base collection) #:select (<sequential>))
  #:use-module ((lokke hash-map) #:select (hash-map?))
  #:use-module ((lokke scm foreign-object) #:select (make-foreign-object-type*))
  #:use-module (oop goops)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module ((srfi srfi-43) #:select (vector-fold))
  #:export (<lokke-vector>
            list->lokke-vector
            lokke-vec
            lokke-vector
            lokke-vector->list
            lokke-vector-append
            lokke-vector-assoc
            lokke-vector-conj
            lokke-vector-equal?
            lokke-vector-length
            lokke-vector-meta
            lokke-vector-ref
            lokke-vector-with-meta
            lokke-vector?
            vector->lokke-vector)
  #:re-export (display equal? write)
  #:duplicates (merge-generics replace warn-override-core warn last))

;; FIXME: implement a "cursor" on the C side that can be used for
;; traversals, including seq.

;; FIXME: move some bulk construction to the C side?
;; FIXME: make-foreign-object-type* is an ugly potentially fragile hack.
(define <lokke-vector>
  ;; NOTE: this cannot be redefined since it's cached in the C code.
  (make-foreign-object-type* 'lokke-vector '(data) #:supers (list <sequential>)))

(load-extension "lokke-vector.so" "init_lokke_vector")

(define (lokke-vector? x) (is-a? x <lokke-vector>))

(define-inlinable (show v port emit)
  (format port "#<<lokke-vector> ~x [" (object-address v))
  (let ((len (lokke-vector-length v)))
    (when (> len 0)
      (emit (lokke-vector-ref v 0) port))
    (when (> len 1)
      (do ((i 1 (1+ i)))
          ((= i len))
        (display " " port)
        (emit (lokke-vector-ref v i) port))))
  (display "]>" port))

(define-method (write (v <lokke-vector>) port)
  (show v port write))

(define-method (display (v <lokke-vector>) port)
  (show v port display))

(define (lokke-vector-with-meta v m)
  (unless (or (hash-map? m) (eq? #nil m))
    (scm-error 'wrong-type-arg 'lokke-vector-with-meta
               "Wrong type argument in position 2: ~s"
               (list m) (list m)))
  (%lokke-vector-with-meta v m))

(define (lokke-vector-equal? v1 v2)
  ;; Note: this really is equal?, not clojure =
  (let ((n1 (lokke-vector-length v1))
        (n2 (lokke-vector-length v2)))
    (and (= n1 n2)
         (let loop ((i 0))
           (cond
            ((= i n1) #t)
            ((equal? (lokke-vector-ref v1 i) (lokke-vector-ref v2 i))
             (loop (1+ i)))
            (else #f))))))

(define-method (equal? (v1 <lokke-vector>) (v2 <lokke-vector>))
  (lokke-vector-equal? v1 v2))

(define (lokke-vector-conj v x . xs)
  (fold (lambda (x result) (lokke-vector-conj-1 result x))
        (lokke-vector-conj-1 v x)
        xs))

(define (lokke-vector-append v . vs)
  (fold (lambda (v result)
          (let ((len (lokke-vector-length v)))
            (let loop ((i 0)
                       (result result))
              (if (= i len)
                  result
                  (loop (1+ i)
                        (lokke-vector-conj-1 result (lokke-vector-ref v i)))))))
        v
        vs))

(define (lokke-vector-assoc v index val . index-vals)
  (let loop ((result (lokke-vector-assoc-1 v index val))
             (remaining index-vals))
    (if (null? remaining)
        result
        (match remaining
          ((index val . remaining)
           (loop (lokke-vector-assoc-1 result index val)
                 remaining))
          (_ (scm-error 'wrong-number-of-args
                        "lokke-vector-assoc"
                        "Wrong number of argumments" '() #f))))))

(define (list->lokke-vector items)
  (fold (lambda (item result)
          (lokke-vector-conj-1 result item))
        lokke-empty-vector
        items))

(define (vector->lokke-vector items)
  (vector-fold (lambda (i result item) (lokke-vector-conj-1 result item))
               lokke-empty-vector
               items))

(define (lokke-vec items)
  (cond
   ((list? items) (list->lokke-vector items))
   ((vector? items) (vector->lokke-vector items))
   ((lokke-vector? items) items)))

(define (lokke-vector . items)
  (lokke-vec items))

(define (lokke-vector->list v)
  (let ((len (lokke-vector-length v)))
    (let loop ((i 0)
               (result '()))
      (if (= i len)
          (reverse! result)
          (loop (1+ i) (cons (lokke-vector-ref v i) result))))))
