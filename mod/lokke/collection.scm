;;; Copyright (C) 2015-2019 Rob Browning <rlb@defaultvalue.org>
;;;
;;; This project is free software; you can redistribute it and/or modify
;;; it under the terms of (at your option) either of the following two
;;; licences:
;;;
;;;   1) The GNU Lesser General Public License as published by the Free
;;;      Software Foundation; either version 2.1, or (at your option) any
;;;      later version
;;;
;;;   2) The Eclipse Public License; either version 1.0 or (at your
;;;      option) any later version.

(define-module (lokke collection)
  #:version (0 0 0)
  #:use-module ((ice-9 format) #:select (format))
  #:use-module ((guile) :select ((cons . %scm-cons)))
  #:use-module (oop goops)
  #:use-module ((ice-9 match) #:select (match-lambda match-lambda*))
  #:use-module ((srfi srfi-1) #:select (circular-list? proper-list?))
  #:use-module ((srfi srfi-43) #:select (vector-unfold))
  #:use-module ((lokke base syntax) #:select (if-let when when-let when-not))
  #:use-module ((lokke base util) #:select (require-nil))
  #:use-module ((lokke base collection)
                #:select (<coll>
                          <lazy-seq>
                          <pair-seq>
                          <seq>
                          <sequential>
                          <vector-seq>
                          assoc
                          bounded-count
                          coll?
                          conj
                          cons
                          contains?
                          count
                          counted?
                          drop
                          empty
                          every?
                          ffirst
                          find
                          first
                          fnext
                          get
                          get-in
                          lazy-seq
                          next
                          nfirst
                          nnext
                          nth
                          rest
                          second
                          seq
                          seq?
                          seqable?
                          sequential?
                          take
                          update))
  #:use-module ((lokke base map) #:select (<map> map? select-keys))
  #:use-module ((lokke base map-entry)
                #:select (<map-entry> key map-entry map-entry? val))
  #:use-module ((lokke scm vector) #:select (<lokke-vector>))
  #:use-module ((lokke compare) #:select (clj=))
  #:use-module ((lokke invoke) #:select (invoke))
  #:use-module ((lokke pr) #:select (pr-on print-on))
  #:export (doall
            dorun
            empty?
            into
            not-any?
            not-every?
            reduce
            repeat
            repeatedly
            seq->scm-list
            some)
  #:replace (merge)
  #:re-export (<coll>
               <map>
               <map-entry>
               <seq>
               <sequential>
               assoc
               bounded-count
               clj=
               coll?
               conj
               cons
               contains?
               count
               counted?
               drop
               empty
               every?
               ffirst
               find
               first
               fnext
               get
               get-in
               invoke
               key
               lazy-seq
               map?
               map-entry
               map-entry?
               next
               nfirst
               nnext
               nth
               pr-on
               print-on
               rest
               second
               select-keys
               seq
               seq?
               seqable?
               sequential?
               take
               update
               val)
  #:duplicates (merge-generics replace warn-override-core warn last))

(define-method (invoke (key <keyword>) map) (get map key))
(define-method (invoke (key <keyword>) map not-found) (get map key not-found))

(define-method (reversible? x) #f)

;; FIXME: match https://clojure.org/guides/equality#_summary as appropriate

(define-method (clj= (s1 <seq>) (s2 <seq>))
  ;; Could also be implemented via list-compare...
  (if-let [s1 (seq s1)]
    (if-let [s2 (seq s2)]
      (and (clj= (first s1) (first s2))
           (clj= (rest s1) (rest s2)))
      #f)
    (not (seq s2))))

;; We have to special case anything that's sequable?, but not <sequential>.
;; FIXME: improper lists?
(define-method (clj= (x <pair>) (y <pair>)) (equal? x y))

(define (clj-sequential= x y)
  (clj= (seq x) (seq y)))

(define-method (clj= (s <pair>) x)
  (when (sequential? x)
    (clj-sequential= s x)))

;; Empty list is <null> in goops
(define-method (clj= (s <null>) x)
  (when (sequential? x)
   (eq? #nil (seq x))))

(define-method (clj= (x <vector>) (y <vector>)) (equal? x y))
(define-method (clj= (s <vector>) x)
  (when (sequential? x)
    (clj-sequential= s x)))

(define-method (clj= (s <sequential>) x)
  (when (sequential? x)
    (clj-sequential= s x)))

(define (seq->scm-list s)
  (let loop ((s s)
             (result '()))
    (if-let [s (seq s)]
            (loop (next s) (%scm-cons (first s) result))
            (reverse! result))))

(define (show coll emit port open close)
  (display open port)
  (when-let (coll (seq coll))
    (emit (first coll) port)
    (do ((coll (next coll) (next coll)))
        ((nil? coll))
      (display " " port)
      (emit (first coll) port)))
  (display close port))

(define-method (pr-on (s <seq>) port)
  (show s pr-on port "(" ")"))

(define-method (print-on (s <seq>) port)
  (show s print-on port "(" ")"))

;; For now, just emulate the same output as Guile, assuming that
;; <class> is always correct.

(define-method (write (s <seq>) port)
  (show s
        (lambda (x) (write x port))
        (format #f "#<<class> ~s ~x ("
                (class-name (class-of s)) (object-address s))
        ")>"))

(define-method (display (s <seq>) port)
  (show s
        (lambda (x) (display x port))
        (format #f "#<<class> ~s ~x ("
                (class-name (class-of s)) (object-address s))
        ")>"))

;; FIXME: Implement rseq...
;;
;; (define-method (reversible? (vs <vector-seq>)) #t)

(define (last coll)
  (let ((n (next coll)))
    (if n (last n) (first n))))

(define (butlast coll)
  (let loop ((result '())
             (rst coll))
    (let ((n (next rst)))
      (if n
          (loop (cons (first n) result) (next rst))
          result))))

(define-method (empty? (coll <coll>))
  (if (counted? coll)
      (zero? (count coll))
      (not (seq coll))))

(define-method (empty? (v <vector>))
  (zero? (vector-length v)))

(define-method (empty? (v <list>))
  (null? v))

(define-method (empty? (v <sequential>))
  (eq? #nil (seq v)))

(define-method (empty? (v <null>))
  #t)

(define-method (empty? (v <pair>))
  #f)

(define-method (empty? (b <boolean>))
  (require-nil 'empty? b)
  #t)

;; Generic implementation -- may be able to do better for any given
;; class.

(define-method (reduce f val coll)
  (if-let (s (seq coll))
          (reduce f (f val (first s)) (next s))
          val))

(define-method (reduce f coll)
  (if-let (s (seq coll))
          (reduce f (first s) (next s))
          (f)))

(define-method (into to from)
  (reduce conj to from))

;; FIXME: these haven't been tested?

(define dorun
  (match-lambda*
    ((coll) (when-let (s (seq coll))
                      (dorun (next coll))))
    ((n coll) (when (positive? n)
                (when-let (s (seq coll))
                  (dorun (1- n) (next coll)))))))
(define doall
  (match-lambda*
    ((coll) (dorun coll) coll)
    ((n coll) (dorun n coll) coll)))

(define (some f coll)
  (let loop ((rst coll))
    (when-let (s (seq rst))
              (if-let (v (invoke f (first s)))
                      v
                      (loop (rest s))))))

(define (merge . xs)
  (when-not (null? xs)
    (let loop ((xs xs)
               (result (car xs)))
      (if (null? xs)
          result
          (loop (cdr xs)
                (reduce (lambda (result x) (conj result x))
                        result
                        (car xs)))))))

(define repeat
  (match-lambda*
    ((x) (let loop () (lazy-seq (cons x (loop)))))
    ((n x)
     (let loop ((n n))
       (lazy-seq
        (when-not (zero? n)
          (cons x (loop (1- n)))))))))

(define repeatedly
  (match-lambda*
    ((f) (let loop () (lazy-seq (cons (f) (loop)))))
    ((n f)
     (let loop ((n n))
       (lazy-seq
        (when-not (zero? n)
          (cons (f) (loop (1- n)))))))))

(define (not-every? pred coll)
  (not (every? pred coll)))

(define (not-any? pred coll)
  (not (some pred coll)))
