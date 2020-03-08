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
  #:use-module ((guile) #:hide (peek))
  #:use-module ((ice-9 match) #:select (match-lambda*))
  #:use-module (oop goops)
  #:use-module ((lokke base collection) #:select (define-nth-seq))
  #:use-module ((lokke base map-entry) #:select (map-entry))
  #:use-module ((lokke base util) #:select (require-nil))
  #:use-module ((lokke collection)
                #:select (<coll>
                          <seq>
                          <sequential>
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
                          peek
                          pop
                          reduce
                          rest
                          seq
                          seqable?
                          update))
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
  #:use-module ((srfi srfi-71) #:select (let let*))
  #:replace (vector vector?)
  #:export (subvec vec)
  #:re-export (clj=
               compare
               conj
               const-nth?
               contains?
               count
               counted?
               empty
               empty?
               equal?
               find
               first
               get
               meta
               nth
               peek
               pop
               pr-on
               print-on
               rest
               seq
               seqable?
               with-meta
               update)
  #:duplicates (merge-generics replace warn-override-core warn last))

(re-export-and-replace! 'assoc)


;;; <lokke-vector>

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

;; FIXME: here and update -- there is no zero arg version of either...
(define-method (assoc (v <lokke-vector>) . indexes-and-values)
  (apply lokke-vector-assoc v indexes-and-values))

(define (valid-index? v i)
  (and (integer? i) (>= i 0) (< i (lokke-vector-length v))))

(define-method (contains? (v <lokke-vector>) i)
  (valid-index? v i))

(define-method (nth (v <lokke-vector>) i) (lokke-vector-ref v i))
(define-method (nth (v <lokke-vector>) i not-found)
  (lokke-vector-ref v i not-found))

(define-method (get (v <lokke-vector>) i)
  (if (and (integer? i) (> i 0))
      (lokke-vector-ref v i #nil)
      #nil))

(define-method (get (v <lokke-vector>) i not-found)
  (if (and (integer? i) (> i 0))
      (lokke-vector-ref v i not-found)
      #nil))

(define-method (find (v <lokke-vector>) i)
  (if (valid-index? v i)
      (map-entry i (lokke-vector-ref v i))
      #nil))

(define-method (update (v <lokke-vector>) (i <integer>) f . args)
  (lokke-vector-assoc v i (apply f (lokke-vector-ref v i #nil) args)))


(define-nth-seq <lokke-vector-seq>
  lokke-vector-length lokke-vector-ref)

(define-method (seqable? (x <lokke-vector>)) #t)

(define-method (seq (v <lokke-vector>))
  (if (zero? (lokke-vector-length v))
      #nil
      (make <lokke-vector-seq> #:items v)))


;;; <subvec> - for now, a naive, disjoint implementation.

(define-class <subvec> (<sequential>)
  (vec #:init-keyword #:vec)
  (offset #:init-keyword #:offset)
  (length #:init-keyword #:length)
  (meta #:init-keyword #:meta #:init-value #nil))

(define (subvec? x)
  (is-a? x <subvec>))

(define* (clone-subvec v
                       #:key
                       (vec (slot-ref v 'vec))
                       (offset (slot-ref v 'offset))
                       (length (slot-ref v 'length))
                       (meta (slot-ref v 'meta)))
  (make <subvec> #:vec vec #:offset offset #:length length #:meta meta))

(define-method (meta (v <subvec>)) (slot-ref v 'meta))

(define-method (with-meta (v <subvec>) (mdata <boolean>))
  (require-nil 'with-meta 2 mdata)
  (clone-subvec v #:meta mdata))
(define-method (with-meta (v <subvec>) (mdata <hash-map>))
  (clone-subvec v #:meta mdata))

(define-inlinable (subvec-vec sv) (slot-ref sv 'vec))
(define-inlinable (subvec-length sv) (slot-ref sv 'length))
(define-inlinable (subvec-offset sv) (slot-ref sv 'offset))

(define-method (counted? (v <subvec>)) #t)
(define-method (count (v <subvec>)) (subvec-length v))
(define-method (empty (v <subvec>)) empty-vector)
(define-method (empty? (v <subvec>)) (zero? (subvec-length v)))


(define-inlinable (subvec-of orig-offset orig-len content-vec start end)
  (cond
   ((> start orig-len)
    (scm-error 'out-of-range 'subvec
               "start past end of vector: ~s" (list start) (list start)))
   ((and end (> end orig-len))
    (scm-error 'out-of-range 'subvec
               "end past end of vector: ~s" (list end) (list end)))
   ((zero? orig-len) empty-vector)
   (else (make <subvec>
           #:vec content-vec
           #:offset (+ orig-offset start)
           #:length (- (or end orig-len) start)))))

(define-method (subvec (v <lokke-vector>) (start <integer>))
  (subvec-of 0 (lokke-vector-length v) v start #f))

(define-method (subvec (v <lokke-vector>) (start <integer>) (end <integer>))
  (subvec-of 0 (lokke-vector-length v) v start end))

(define-method (subvec (v <subvec>) (start <integer>))
  (subvec-of (subvec-offset v) (subvec-length v) (subvec-vec v) start #f))

(define-method (subvec (v <subvec>) (start <integer>) (end <integer>))
  (subvec-of (subvec-offset v) (subvec-length v) (subvec-vec v) start end))


;; Define some equality specializations to avoid per-item generic
;; overhead, and for subvecs, per-item offset math.

(define (subvec-same? v1 v2 same?)
  (and (= (subvec-length v1) (subvec-length v2))
       (let* ((i1 (subvec-offset v1))
              (i-max (+ i1 (subvec-length v1)))
              (vec-1 (subvec-vec v1))
              (vec-2 (subvec-vec v2)))
         (let loop ((i1 i1)
                    (i2 (subvec-offset v2)))
           (cond
            ((= i1 i-max) #t)
            ((same? (lokke-vector-ref vec-1 i1) (lokke-vector-ref vec-2 i2))
             (loop (1+ i1) (1+ i2)))
            (else #f))))))

(define-method (clj= (v1 <subvec>) (v2 <subvec>)) (subvec-same? v2 v1 clj=))
(define-method (equal? (v1 <subvec>) (v2 <subvec>)) (subvec-same? v2 v1 equal?))

(define (subvec-vec-same? v1 v2 same?)
  (and (= (subvec-length v1)
          (lokke-vector-length v2))
       (let* ((i1 (subvec-offset v1))
              (i-max (+ i1 (subvec-length v1)))
              (vec-1 (subvec-vec v1)))
         (let loop ((i1 i1)
                    (i2 0))
           (cond
            ((= i1 i-max) #t)
            ((same? (lokke-vector-ref vec-1 i1) (lokke-vector-ref v2 i2))
             (loop (1+ i1) (1+ i2)))
            (else #f))))))

;; Avoid the per-item generic overhead for these
(define-method (clj= (v1 <subvec>) (v2 <lokke-vector>)) (subvec-vec-same? v1 v2 clj=))
(define-method (clj= (v1 <lokke-vector>) (v2 <subvec>)) (subvec-vec-same? v2 v1 clj=))
(define-method (equal? (v1 <subvec>) (v2 <lokke-vector>)) (subvec-vec-same? v1 v2 equal?))
(define-method (equal? (v1 <lokke-vector>) (v2 <subvec>)) (subvec-vec-same? v2 v1 equal?))

(define subvec-nth
  (match-lambda*
    ((v i)
     (let ((off (subvec-offset v))
           (len (subvec-length v))
           (vec (subvec-vec v)))
       (if (>= (+ off i) (lokke-vector-length vec))
           (scm-error 'out-of-range 'nth
                      "<subvec> index out of range: ~s" (list i) (list i))
           (lokke-vector-ref vec (+ off i)))))
    ((v i not-found)
     (let ((off (subvec-offset v))
           (len (subvec-length v))
           (vec (subvec-vec v)))
       (if (>= (+ off i) (lokke-vector-length vec))
           not-found
           (lokke-vector-ref vec (+ off i)))))))

(define-method (const-nth? (v <subvec>)) #t)
(define-method (nth (v <subvec>) i) (subvec-nth v i))
(define-method (nth (v <subvec>) i not-found) (subvec-nth v i not-found))
(define-method (get (v <subvec>) i) (subvec-nth v i #nil))
(define-method (get (v <subvec>) i not-found) (subvec-nth v i not-found))

(define-method (conj (v <subvec>) x . xs)
  (let* ((vec (subvec-vec v))
         (svlen (subvec-length v))
         (vlen (lokke-vector-length vec))
         ;; assoc until we run off the end of the underlying vec, then
         ;; bulk conj -- could do both in bulk if we partitioned the input
         (vec (let loop ((i (+ svlen (subvec-offset v)))
                         (rst (cons x xs))
                         (result vec))
                (cond
                 ((null? rst) result)
                 ((>= i vlen)
                  (apply lokke-vector-conj result rst))
                 (else
                  (loop (1+ i)
                        (cdr rst)
                        (lokke-vector-assoc result i (car rst))))))))
    (clone-subvec v
                  #:length (+ 1 (subvec-length v) (length xs))
                  #:vec vec)))

;; ;; FIXME: vec/subvec, etc.?  Do we have/need a generic clj=-like const-nth fallback?
;; (define-method (compare (v1 <subvec>) (v2 <subvec>))
;;   (vector-compare compare v1 v2 subvec-length subvec-nth))

(define-method (pr-on (v <subvec>) port)
  (show-vector v (subvec-length v) subvec-nth pr-on port))

(define-method (print-on (v <subvec>) port)
  (show-vector v (subvec-length v) subvec-nth pr-on port))

(define (fold-indexed f init . lists)
  (let ((i -1))
    (apply fold (lambda args
                  (set! i (1+ i))
                  (apply f i args))
           init lists)))

(define-method (assoc (v <subvec>) . indexes-and-values)
  ;; FIXME: optimize a bit?
  (let* ((svlen (subvec-length v))
         (vlen (lokke-vector-length (subvec-vec v)))
         (off (subvec-offset v))
         (appended 0)
         (ivs (fold-indexed (lambda (i x result)
                              (cons (if (odd? i)
                                        x
                                        (begin
                                          (when (>= x svlen)
                                            (set! appended (1+ appended)))
                                          (+ x off)))
                                    result))
                            '()
                            indexes-and-values)))
    (clone-subvec v
                  #:length (+ svlen appended)
                  #:vec (apply lokke-vector-assoc (subvec-vec v) (reverse! ivs)))))

(define-method (update (v <subvec>) (i <integer>) f . args)
  (let ((len (subvec-length v)))
    (unless (<= 0 i len)
      (scm-error 'out-of-range 'update "<subvec> index out of range: ~s"
                 (list i) (list i)))
    (let* ((vec (subvec-vec v))
           (vec-i (+ i (subvec-offset v)))
           (new-len prev (if (= i len)
                             (values (1+ len) #nil)
                             (values len (lokke-vector-ref vec vec-i)))))
      (clone-subvec v
                    #:length new-len
                    #:vec (lokke-vector-assoc vec vec-i (apply f prev args ))))))

(define-method (contains? (v <subvec>) i)
  (and (integer? i)
       (>= i 0)
       (< i (subvec-length v))))

(define-method (find (v <subvec>) i)
  (if (and (integer? i)
           (>= i 0)
           (< i (subvec-length v)))
      (map-entry i (lokke-vector-ref (subvec-vec v) (+ i (subvec-offset v))))
      #nil))

(define (vector? x)
  (or (lokke-vector? x)
      (subvec? x)))


(define-nth-seq <subvec-seq>
  subvec-length subvec-nth)

(define-method (seq (v <subvec>))
  (if (zero? (subvec-length v))
      #nil
      (make <subvec-seq> #:items v)))


(define-method (peek (v <lokke-vector>))
  (let ((len (lokke-vector-length v)))
    (if (positive? len)
        (lokke-vector-ref v (1- len))
        #nil)))

(define-method (pop (v <lokke-vector>))
  ;; FIXME: add real pop support to lokke-vectors, i.e. shrink tail, etc.
  (let ((len (lokke-vector-length v)))
    (when (zero? len)
      (error "cannot pop empty vector"))
    (subvec v 0 (1- len))))


(define-method (peek (v <subvec>))
  (subvec-nth v (1- (subvec-length v)) #nil))

(define-method (pop (v <subvec>))
  ;; FIXME: add real pop support, i.e. shrink tail, etc.
  (let ((len (subvec-length v)))
    (when (zero? len)
      (error "cannot pop empty vector"))
    (subvec-of (subvec-offset v) (subvec-length v) (subvec-vec v) 0 (1- len))))
