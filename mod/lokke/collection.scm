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

(read-set! keywords 'postfix)  ;; srfi-88

(define-module (lokke collection)
  version: (0 0 0)
  use-module: ((ice-9 format) select: (format))
  use-module: ((guile) :select ((cons . %scm-cons)))
  use-module: (oop goops)
  use-module: ((ice-9 match) select: (match-lambda match-lambda*))
  use-module: ((srfi srfi-1) select: (circular-list? proper-list?))
  use-module: ((srfi srfi-43) select: (vector-unfold))
  use-module: ((lokke base syntax) select: (if-let when when-let when-not))
  use-module: ((lokke base collection)
               select: (<coll>
                        <lazy-seq>
                        <pair-seq>
                        <seq>
                        <vector-seq>
                        assoc
                        coll?
                        conj
                        cons
                        contains?
                        count
                        counted?
                        drop
                        empty
                        ffirst
                        find
                        first
                        fnext
                        get
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
  use-module: ((lokke invoke) select: (invoke))
  use-module: ((lokke pr) select: (*out*  pr print))
  export: (doall
           dorun
           empty?
           every?
           into
           not-any?
           not-every?
           reduce
           repeat
           repeatedly
           seq->scm-list
           some)
  replace: (merge)
  re-export: (<coll>
              <seq>
              assoc
              coll?
              conj
              cons
              contains?
              count
              counted?
              drop
              empty
              ffirst
              find
              first
              fnext
              get
              invoke
              lazy-seq
              next
              nfirst
              nnext
              nth
              pr
              print
              rest
              second
              seq
              seq?
              seqable?
              sequential?
              take
              update)
  duplicates: (merge-generics replace warn-override-core warn last))

(define-method (invoke (key <keyword>) map) (get map key))
(define-method (invoke (key <keyword>) map not-found) (get map key not-found))

(define-method (reversible? x) #f)

(define (seq->scm-list s)
  (let loop ((s s)
             (result '()))
    (if-let [s (seq s)]
            (loop (next s) (%scm-cons (first s) result))
            (reverse! result))))

(define-method (equal? (s1 <seq>) (s2 <seq>))
  (if-let [s1 (seq s1)]
    (if-let [s2 (seq s2)]
      (and (equal? (first s1) (first s2))
           (equal? (rest s1) (rest s2)))
      #f)
    (not (seq s2))))

(define (show coll emit open close)
  (display open (*out*))
  (when-let (coll (seq coll))
    (emit (first coll))
    (do ((coll (next coll) (next coll)))
        ((nil? coll))
      (display " " (*out*))
      (emit (first coll))))
  (display close (*out*)))

(define-method (pr (s <seq>)) (show s pr "(" ")"))
(define-method (print (s <seq>)) (show s print "(" ")"))

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

(define-method (equal? (c1 <coll>) (c2 <coll>))
  (equal? (seq c1) (seq c2)))

(define-method (empty? (coll <coll>))
  (if (counted? coll)
      (zero? (count coll))
      (not (seq coll))))

(define-method (empty? (v <vector>))
  (zero? (vector-length v)))

(define-method (empty? (v <list>))
  (null? v))

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

(define (every? pred coll)
  (if-let (s (seq coll))
    (and (pred (first s))
         (every? pred (next coll)))
    #t))

(define (not-every? pred coll)
  (not (every? pred coll)))

(define (not-any? pred coll)
  (not (some pred coll)))
