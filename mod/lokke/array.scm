;;; Copyright (C) 2021 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

(define-module (lokke array)
  #:use-module ((lokke base collection) #:select (define-nth-seq))
  #:use-module ((lokke base syntax) #:select ((if . clj-if)))
  #:use-module ((lokke collection)
                #:select (const-nth?
                          count
                          counted?
                          empty?
                          first
                          nth
                          rest
                          seq
                          seqable?
                          sequential?))
  #:use-module ((lokke compat) #:select (if-at-least-guile-version))
  #:use-module ((ice-9 arrays) #:select (array-copy))
  #:use-module ((ice-9 match) #:select (match-lambda*))
  #:use-module (oop goops)
  #:use-module (srfi srfi-4)
  #:use-module (srfi srfi-71)
  #:duplicates (merge-generics replace warn-override-core warn last)
  #:export (aget
            amap
            areduce
            aset
            aset-boolean
            boolean-array
            byte-array
            double-array
            float-array
            int-array
            long-array
            short-array)
  #:re-export ((array-copy . aclone)
               (array-ref . aget)
               (s8vector-ref . aget-byte)
               (s16vector-ref . aget-short)
               (s32vector-ref . aget-int)
               (s64vector-ref . aget-long)
               (f32vector-ref . aget-float)
               (f64vector-ref . aget-double)
               (array-length . alength)
               (s8vector-set! . aset-byte)
               (s16vector-set! . aset-short)
               (s32vector-set! . aset-int)
               (s64vector-set! . aset-long)
               (f32vector-set! . aset-float)
               (f64vector-set! . aset-double)
               const-nth?
               count
               counted?
               empty?
               first
               nth
               rest
               seq
               seqable?
               sequential?))

(if-at-least-guile-version
 (3 0 3)
 (re-export (bitvector-bit-set? . aget-boolean))
 (begin
   (define bitvector-bit-set? bitvector-ref)
   (define (bitvector-set-bit! vec i) (bitvector-set! vec i #t))
   (define (bitvector-clear-bit! vec i) (bitvector-set! vec i #f))
   (export (bitvector-bit-set? . aget-boolean))))

(define (aset array idx v)
  (array-set! array v idx))

(define (array-fns array)
  (cond
   ((s8vector? array) (values s8vector-length make-s8vector s8vector-ref s8vector-set!))
   ((s32vector? array) (values s32vector-length make-s32vector s32vector-ref s32vector-set!))
   ((s16vector? array) (values s16vector-length make-s16vector s16vector-ref s16vector-set!))
   ((s64vector? array) (values s64vector-length make-s64vector s64vector-ref s64vector-set!))
   ((f64vector? array) (values f64vector-length make-f64vector f64vector-ref f64vector-set!))
   ((f32vector? array) (values f32vector-length make-f32vector f32vector-ref f32vector-set!))
   ((bitvector? array) (values bitvector-length make-bitvector bitvector-bit-set? aset-boolean))
   (else (error "Not a known clojure array type" array))))

(define-nth-seq <lokke-uvec-seq>
  ;; Could be optimized - this implementation will have to dispatch on
  ;; type for every ref.
  array-length array-ref)

(define-method (const-nth? (x <uvec>)) #t)
(define-method (count (v <uvec>)) (array-length v))
(define-method (counted? (v <uvec>)) #t)
(define-method (empty? (v <uvec>)) (zero? (array-length v)))
(define-method (seqable? (x <uvec>)) #t)
(define-method (sequential? (x <uvec>)) #t)

(define-method (seq (v <uvec>))
  (if (zero? (array-length v))
      #nil
      (make <lokke-uvec-seq> #:items v)))

(define-method (nth (v <uvec>) i) (array-ref v i))
(define-method (nth (v <uvec>) i not-found)
  (cond
   ((negative? i) not-found)
   ((>= i (array-length v)) not-found)
   (else (array-ref v i))))

(define-nth-seq <lokke-bitvector-seq>
  bitvector-length bitvector-bit-set?)

(define-method (const-nth? (x <bitvector>)) #t)
(define-method (count (v <bitvector>)) (array-length v))
(define-method (counted? (v <bitvector>)) #t)
(define-method (empty? (v <bitvector>)) (zero? (bitvector-length v)))
(define-method (seqable? (x <bitvector>)) #t)
(define-method (sequential? (x <bitvector>)) #t)

(define-method (seq (v <bitvector>))
  (if (zero? (array-length v))
      #nil
      (make <lokke-bitvector-seq> #:items v)))

(define-method (nth (v <bitvector>) i) (array-ref v i))
(define-method (nth (v <bitvector>) i not-found)
  (cond
   ((negative? i) not-found)
   ((>= i (array-length v)) not-found)
   (else (array-ref v i))))

(define-syntax amap
  (lambda (x)
    (syntax-case x ()
      ((_  array idx ret expr)
       (and (symbol? (syntax->datum #'idx))
            (symbol? (syntax->datum #'ret)))
       (let ((i (datum->syntax #'expr (syntax->datum #'idx)))
             (ret (datum->syntax #'expr (syntax->datum #'ret))))
         #`(let* ((count _make ref set (array-fns array))
                  (len (count array))
                  (result (aclone array))
                  (#,ret result))
             (do ((#,i 0 (1+ #,i)))
                 ((= #,i len) result)
               (set result #,i expr))))))))

(define-syntax areduce
  (lambda (x)
    (syntax-case x ()
      ((_  array idx ret init expr)
       (and (symbol? (syntax->datum #'idx))
            (symbol? (syntax->datum #'ret)))
       (let ((i (datum->syntax #'expr (syntax->datum #'idx)))
             (ret (datum->syntax #'expr (syntax->datum #'ret))))
         #`(let* ((count _make ref set (array-fns array))
                  (len (count array)))
             (let loop ((#,ret init)
                        (#,i 0))
               (if (= #,i len)
                   #,ret
                   (loop expr (1+ #,i))))))))))

(define-syntax define-per-type-fns
  (lambda (x)
    (define (str->symsyn ctx s)
      (datum->syntax ctx (string->symbol s)))
    (syntax-case x ()
      ((_ clj-type scm-type)
       (let* ((cljt (symbol->string (syntax->datum #'clj-type)))
              (scmt (symbol->string (syntax->datum #'scm-type)))
              (make (str->symsyn x (string-append "make-" scmt "vector")))
              (ref (str->symsyn x (string-append scmt "vector-ref")))
              (set (str->symsyn x (string-append scmt "vector-set!")))
              (constructor (str->symsyn x (string-append cljt "-array"))))
         #`(begin
             ;; double-array
             (define #,constructor
               (match-lambda*
                 ((size-or-seq)
                  (if (not (seqable? size-or-seq))
                      (#,make size-or-seq 0)
                      (let* ((size (count size-or-seq))
                             (result (#,make size)))
                        (do ((i 0 (1+ i))
                             (remainder size-or-seq (rest remainder)))
                            ((= i size) result)
                          (#,set result i (first remainder))))))
                 ((size init-val-or-seq)
                  (if (not (seqable? init-val-or-seq))
                      (#,make size init-val-or-seq)
                      (do ((result (#,make size 0))
                           (i 0 (1+ i))
                           (remainder init-val-or-seq (rest remainder)))
                          ((or (= i size) (empty? remainder)) result)
                        (#,set result i (first remainder)))))))
             (export #,constructor)))))))

(define-per-type-fns byte s8)
(define-per-type-fns short s16)
(define-per-type-fns int s32)
(define-per-type-fns long s64)
(define-per-type-fns float f32)
(define-per-type-fns double f64)

(define (aset-boolean array i v)
  ((clj-if v bitvector-set-bit! bitvector-clear-bit!)
   array i))

(define boolean-array
  (match-lambda*
    ((size-or-seq)
     (if (not (seqable? size-or-seq))
         (make-bitvector size-or-seq #f)
         (let* ((size (count size-or-seq))
                (result (make-bitvector size)))
           (do ((i 0 (1+ i))
                (remainder size-or-seq (rest remainder)))
               ((= i size) result)
             (aset-boolean result i (first remainder))))))
    ((size init-val-or-seq)
     (if (not (seqable? init-val-or-seq))
         (make-bitvector size init-val-or-seq)
         (do ((result (make-bitvector size #f))
              (i 0 (1+ i))
              (remainder init-val-or-seq (rest remainder)))
             ((or (= i size) (empty? remainder)) result)
           (aset-boolean result i (first remainder)))))))
