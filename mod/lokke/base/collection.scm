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

;;; This is the core of the collection implementation.  It's in a
;;; separate base module because we want the base syntax module to be
;;; able to depend on it for destructuring, and we want to be able to
;;; use destructuring outside the base, i.e. in the main collections
;;; module.

;; Cannot rely on any of the (lokke base syntax)es in here, i.e.,
;; when, when-let, because they depend on let and let depends on this
;; module for destructuring.

(define-module (lokke base collection)
  #:version (0 0 0)
  #:use-module ((guile)
                :select ((apply . %scm-apply) (cons . %scm-cons) (list? . %scm-list?)))
  #:use-module ((lokke base invoke) #:select (invoke))
  #:use-module ((lokke base metadata) #:select (meta with-meta))
  #:use-module ((lokke base util) #:select (require-nil vec-tag?))
  #:use-module ((lokke compare) #:select (clj=))
  #:use-module ((lokke compat) #:select (re-export-and-replace!))
  #:use-module (oop goops)
  #:use-module ((srfi srfi-1) #:select (drop-right fold last proper-list?))
  #:use-module ((srfi srfi-43) #:select (vector-append))
  #:export (<coll>
            <lazy-seq>
            <pair-seq>
            <seq>
            <sequential>
            <vector-seq>
            assoc-in
            bounded-count
            coll?
            conj
            const-nth?
            contains?
            count
            counted?
            define-nth-seq
            dissoc
            doseq
            drop
            empty
            every?
            ffirst
            find
            first
            fnext
            get
            get-in
            into
            keys
            lazy-seq
            make-pair-seq
            next
            nfirst
            nnext
            not-empty
            nth
            reduce
            reduce-kv
            rest
            rseq
            second
            select-keys
            seq
            seq->scm-list
            seq?
            seqable?
            sequential?
            take
            take-while
            update
            update-in
            vals)
  #:re-export (clj= invoke)
  #:replace (apply assoc first list? merge)
  #:duplicates (merge-generics replace warn-override-core warn last))

(re-export-and-replace! 'cons)

;; FIXME: should these implmentations of rest actually be next?

(set! assoc #f)  ;; We do not want the guile assoc as the fallback
(define-generic assoc)
(define-generic dissoc)
(define-generic contains)
(define-generic empty)
(define-generic find)
(define-generic first)
(define-generic get)
(define-generic keys)
(define-generic into)
(define-generic next)
(define-generic not-empty)
(define-generic reduce-kv)
(define-generic rest)
(define-generic rseq)
(define-generic second)
(define-generic select-keys)
(define-generic seq)
(define-generic sequential?)
(define-generic update)
(define-generic vals)

;; FIXME: double-check, and re-evaluate cons handling more generally...
(define-generic cons)

(define-method (conj coll . xs)
  (if (null? xs)
      coll
      (fold (lambda (x result) (conj result x))
            coll
            xs)))

(define (seq->scm-list s)
  (let loop ((s s)
             (result '()))
    (let ((s (seq s)))
      (if s
          (loop (next s) (%scm-cons (first s) result))
          (reverse! result)))))

(define (apply f . args)
  ;; FIXME: tolerable?
  (if (null? args)
      (f)
      (let ((final (last args)))
        (if (%scm-list? final)
            (%scm-apply f (append (drop-right args 1)
                                  final))
            (%scm-apply f (append (drop-right args 1)
                                  (seq->scm-list final)))))))

;; e.g. (get nil :x)
(define-method (get (x <boolean>) key)
  (require-nil 'get x)
  #nil)

(define-method (get (x <boolean>) key not-found)
  (require-nil 'get x)
  not-found)

(define-method (cons obj1 obj2)
  (unless (proper-list? rest)
    (error "Second argument to cons is an improper list:" obj2))
  (%scm-cons obj1 obj2))

(define-method (count s)
  (let loop ((remaining s)
             (sum 0))
    (cond
     ((counted? remaining) (+ sum (count remaining)))
     ((not (seq remaining)) sum)
     (else (loop (next remaining) (1+ sum))))))

(define-method (bounded-count n coll)
  (if (counted? coll)
      (count coll)
      (let loop ((remaining coll)
                 (sum 0))
        (cond
         ((= sum n) n)
         ((counted? remaining) (min n (+ sum (count remaining))))
         ((not (seq remaining)) sum)
         (else (loop (next remaining) (1+ sum)))))))

(define (every? pred coll)
  (let ((s (seq coll)))
    (if s
        (and (pred (first s))
             (every? pred (next coll)))
        #t)))

;; Default behaviors
(define-method (counted? x) #f)
(define-method (empty x) #nil)
(define-method (first x) (first (seq x)))
(define-method (next x) (seq (rest x)))
(define-method (rest x) (rest (seq x)))
(define-method (second coll) (first (next coll)))
(define-method (seq? x) #f)
(define-method (seqable? x) #f)
(define-method (sequential? x) #f)

;; Note: suspect these can avoid being generics since their definition
;; is well specified in clojure, and guile interop should work fine
;; via the required internal seq conversions.

(define (ffirst coll) (first (first coll)))
(define (fnext coll) (first (next coll)))
(define (nfirst coll) (next (first coll)))
(define (nnext coll) (next (next coll)))

(define-class <coll> ())
(define-method (coll? x) #f)
(define-method (coll? (s <coll>)) #t)

(define-method (not-empty (coll <coll>))
  (if (seq coll)
      coll
      #nil))

(define-method (const-nth? x) #f)

(define-class <sequential> (<coll>))

(define-method (sequential? (s <sequential>)) #t)

(define (sequential-iterator s sentinel)
  ;; Returns a function that will return successive elements of s
  ;; until exhausted, and then the sentinel.  Returns elements via nth
  ;; when s is counted? and supports const-nth?.  Does not support
  ;; concurrent use.
  (if (and (counted? s) (const-nth? s))
      (let ((n (count s))
            (i 0))
        (lambda ()
          (if (= i n)
              sentinel
              (let ((result (nth s i)))
                (set! i (1+ i))
                result))))
      (let ((rst s))
        (lambda ()
          (let ((s (seq rst)))
            (if s
                (let ((result (first rst)))
                  (set! rst (rest rst))
                  result)
                sentinel))))))

(define (sequential= x y)
  (and (or (not (and (counted? x) (counted? y)))
           (= (count x) (count y)))
       (let* ((sentinel (make-symbol "sentinel"))
              (next-x (sequential-iterator x sentinel))
              (next-y (sequential-iterator y sentinel)))
         (let loop ((x (next-x))
                    (y (next-y)))
           (if (eq? x sentinel)
               (eq? y sentinel)
               (and (not (eq? y sentinel))
                    (equal? x y)
                    (loop (next-x) (next-y))))))))

(define-method (clj= (s <sequential>) (b <boolean>))
  (eq? (seq s) b))

(define-method (clj= (s <sequential>) (p <null>))
  (eq? #nil (seq s)))

;; FIXME: improper lists, etc.  See DESIGN <pair>s TODO.
(define-method (clj= (s <sequential>) (p <pair>))
  (sequential= s (seq p)))

(define-method (clj= (x <sequential>) (y <sequential>))
  (sequential= x y))

(define-method (equal? (x <sequential>) (y <sequential>))
  (sequential= x y))

(define-method (nth (coll <sequential>) (n <integer>))
  (when (negative? n)
    (scm-error 'out-of-range 'nth "Negative index: ~a"
               (list n) (list n)))
  (let loop ((i 0) (rst coll))
    (let ((s (seq rst)))
      (unless s
        (scm-error 'out-of-range 'nth "Vector index out of range: ~a"
                   (list i) (list i)))
      (if (= i n) (first s) (loop (1+ i) (next s))))))

(define-method (nth (coll <sequential>) (n <integer>) not-found)
  (when (negative? n)
    (scm-error 'out-of-range 'nth "Negative index: ~a"
               (list n) (list n)))
  (let loop ((i 0) (rst coll))
    (let ((s (seq rst)))
      (if s
          (if (= i n) (first s) (loop (1+ i) (next s)))
          not-found))))

(define-method (invoke (s <sequential>) (i <integer>))
  (nth s i))


(define-class <seq> (<sequential>))
(define-method (conj (s <seq>) x) (make-pair-seq x s (meta s)))
(define-method (cons x (s <seq>)) (make-pair-seq x s #nil))
(define-method (empty x (s <seq>)) #nil)
(define-method (seq (s <seq>)) s)
(define-method (seq? (s <seq>)) #t)
(define-method (seqable? (s <seq>)) #t)

;;; <pair-seq>

;; This is also the persistent list implementation.  All pair-seq
;; lists should terminate with '()
(define-class <pair-seq> (<seq>)
  (rfirst #:getter pair-seq-first #:init-keyword #:first)
  (rrrest #:getter pair-seq-rest #:init-keyword #:rest)
  (meta #:getter pair-seq-meta #:init-keyword #:meta))

(eval-when (eval load)
  (define (make-pair-seq first rest meta)
    (make <pair-seq> #:first first #:rest rest #:meta meta)))

(eval-when (expand compile)
  (define (make-pair-seq first rest meta)
    (%scm-cons first rest)))

(define-method (list? x) #f)
(define-method (list? (s <pair-seq>)) #t)
(define-method (list? (s <null>)) #t)
(define-method (list? (s <pair>)) (proper-list? s))

(define-method (first (s <pair-seq>)) (pair-seq-first s))
(define-method (rest (s <pair-seq>)) (pair-seq-rest s))
(define-method (meta (s <pair-seq>)) (pair-seq-meta s))

;; FIXME: split (lokke hash-map) so we can use <hash-map> here
(define-method (with-meta (s <pair-seq>) m)
  (make-pair-seq (pair-seq-first m) (pair-seq-rest m) m))

(define-method (conj (b <boolean>) x) (require-nil 'conj b) (cons x b))
(define-method (cons x (b <boolean>)) (require-nil 'cons b) (make-pair-seq x '() #nil))
(define-method (contains? (b <boolean>) x) (require-nil 'contains? b) #f)
(define-method (count (b <boolean>)) (require-nil 'count b) 0)
(define-method (empty (b <boolean>)) (require-nil 'empty b) #nil)
(define-method (first (b <boolean>)) (require-nil 'first b) #nil)
(define-method (next (b <boolean>)) (require-nil 'next b) #nil)
(define-method (nth (b <boolean>) (i <integer>)) (require-nil 'nth b) #nil)
(define-method (nth (b <boolean>) (i <integer>) not-found) (require-nil 'nth b) not-found)
(define-method (rest (b <boolean>)) (require-nil 'rest b) '())
(define-method (seq (b <boolean>)) (require-nil 'seq b) #nil)
(define-method (seq? (b <boolean>)) (require-nil 'seq b) #f)
(define-method (seqable? (b <boolean>)) (eq? b #nil))

;; FIXME: replace <null> and <pair> with <list>, particularly when
;; they're identical?

(define-method (coll? (s <null>)) #t)
(define-method (conj (s <null>) x) (cons x s))
(define-method (cons x (s <null>)) (make-pair-seq x s #nil))
(define-method (count (s <null>)) 0)
(define-method (counted? (s <null>)) #t)
(define-method (empty (s <null>)) #nil)
(define-method (first (s <null>)) #nil)
(define-method (next (s <null>)) #nil)
(define-method (nth (s <null>) (i <integer>)) #nil)
(define-method (nth (s <null>) (i <integer>) not-found) not-found)
(define-method (rest (s <null>)) '())
(define-method (seq (s <null>)) #nil)
(define-method (seq? (s <null>)) #t)
(define-method (seqable? (s <null>)) #t)
(define-method (sequential? (s <null>)) #t)

(define-method (coll? (x <pair>)) #t)
(define-method (count (x <pair>)) (length x))
(define-method (counted? (s <pair>)) #f)
(define-method (first (x <pair>)) (car x))
(define-method (nth (p <pair>) (i <integer>)) (nth p i #nil))
(define-method (nth (p <pair>) (i <integer>) not-found)
  (when (negative? i)
    (scm-error 'out-of-range 'nth "Negative index: ~a" (list i) (list i)))
  (let loop ((i i) (more p))
    (if (null? more)
        not-found
        (if (zero? i) (car more) (loop (1- i) (cdr more))))))
(define-method (rest (x <pair>)) (cdr x))
(define-method (seq (x <pair>)) (make-pair-seq (first x) (rest x) #nil))
(define-method (seq? (x <pair>)) #t)
(define-method (seqable? (x <pair>)) #t)
(define-method (sequential? (x <pair>)) #t)

(define-method (conj (p <list>) x) (cons x p))

;;; <vector>

(define-method (contains? (v <vector>) key)
  (and (>= key 0)
       (< key (vector-length v))))

(define-method (count (v <vector>))
  (vector-length v))

(define-method (not-empty (v <vector>))
  (if (zero? (vector-length v))
      #nil
      v))

(define-method (const-nth? (v <vector>)) #t)
(define-method (nth (v <vector>) (i <integer>)) (vector-ref v i))
(define-method (nth (v <vector>) (i <integer>) not-found)
  (cond
   ((negative? i) not-found)
   ((>= i (vector-length v)) not-found)
   (else (vector-ref v i))))


;; Does not define the (seq <type>) constructor

(define-syntax-rule (define-nth-seq name get-count get-nth)
  (begin
    (define-class name (<seq>)
      (items #:init-keyword #:items)
      (i #:init-keyword #:i #:init-value 0))

    (define-method (seq (x name))
      (if (< (slot-ref x 'i) (get-count (slot-ref x 'items)))
          x
          #nil))

    (define-method (first (x name))
      (let ((i (slot-ref x 'i))
            (items (slot-ref x 'items)))
        (if (< i (get-count items))
            (get-nth items i)
            #nil)))

    (define-method (rest (x name))
      (make name
        #:items (slot-ref x 'items)
        #:i (1+ (slot-ref x 'i))))

    (define-method (counted? (x name))
      #t)

    (define-method (count (x name))
      (- (get-count (slot-ref x 'items))
         (slot-ref x 'i)))

    (define-method (const-nth (x name)) #t)

    (define-method (nth (x name) (i <integer>))
      (when (negative? i)
        (scm-error 'out-of-range 'nth "Negative index: ~a"
                   (list i) (list i)))
      (let ((items (slot-ref x 'items))
            (i (+ i (slot-ref x 'i))))
        (when (>= i (get-count items))
          (scm-error 'out-of-range 'nth "Vector index out of range: ~a"
                     (list i) (list i)))
        (get-nth items i)))

    (define-method (nth (x name) (i <integer>) not-found)
      (when (negative? i)
        (scm-error 'out-of-range 'nth "Negative index: ~a"
                   (list i) (list i)))
      (let ((items (slot-ref x 'items))
            (i (+ i (slot-ref x 'i))))
        (if (>= i (get-count items))
            not-found
            (get-nth items i))))))


;;; <vector-seq>

(define-nth-seq <vector-seq> vector-length vector-ref)

(define-method (seq (v <vector>))
  (if (zero? (vector-length v))
      #nil
      (make <vector-seq> #:items v)))

(define-method (seqable? (x <vector>)) #t)


;;; <vector-rseq>

(define-nth-seq <vector-rseq>
  vector-length
  (lambda (x i) (vector-ref x (- (vector-length x) i))))

(define-method (rseq (v <vector>))
  (if (zero? (vector-length v))
      #nil
      (make <vector-rseq> #:items v)))


;;; <lazy-seq>

(define-class <lazy-seq> (<seq>)
  (s #:init-keyword #:s))

(define-syntax lazy-seq
  (syntax-rules ()
    ((lazy-seq) '())
    ((lazy-seq body ...) (make <lazy-seq> #:s (delay (begin body ...))))))

(define-method (first (ls <lazy-seq>)) (first (seq ls)))

(define-method (rest (ls <lazy-seq>))
  (rest (seq ls)))

(define-method (seq (ls <lazy-seq>))
  (seq (force (slot-ref ls 's))))


(define (take n coll)
  (lazy-seq
   (if (not (positive? n))
       #nil
       (let ((s (seq coll)))
         (if (nil? s)
             #nil
             (cons (first s)
                   (take (1- n) (next s))))))))

(define (take-while pred coll)
  (lazy-seq
   (let ((s (seq coll)))
     (if (nil? s)
         #nil
         (let ((x (first s)))
           (if (pred x)
               (cons x (take-while pred (rest s)))
               #nil))))))

(define (drop n coll)
  (lazy-seq
   (let ((s (seq coll)))
     (if (not (positive? n))
         s
         (if (nil? s)
             s
             (drop (1- n) (next s)))))))

(define* (get-in associative keys #:optional (not-found #nil))
  (let loop ((src associative) (keys keys))
    (let ((keys (seq keys)))
      (if (not keys)
          src
          (loop (get src (first keys) not-found) (next keys))))))

(define-method (assoc associative k1 v1 k2 . v2-kvs)
  ;; Fall back for cases where there's no advantage to a bulk method
  (when (null? v2-kvs)
    (error "No value for key:" k2))
  (let loop ((kvs (cdr v2-kvs))
             (result (assoc (assoc associative k1 v1) k2 (car v2-kvs))))
    (cond
     ((null? kvs) result)
     ((null? (cdr kvs)) (error "No value for key:" (car kvs)))
     (else (loop (cddr kvs) (assoc result (car kvs) (cadr kvs)))))))

(define (assoc-in associative ks v)
  ;; Match jvm semantics for now
  (let* ((s (seq ks))
         (k (first s))
         (ks (next s)))
    (if ks
        (assoc associative k (assoc-in (get associative k) ks v))
        (assoc associative k v))))

(define (update-in associative ks f . args)
  (let* ((s (seq ks))
         (k (first s))
         (ks (next s)))
    (if ks
        (assoc associative k (apply update-in (get associative k) ks f args))
        (assoc associative k (apply f (get associative k) args)))))

;; Generic implementation -- may be able to do better for any given
;; class.

(define-method (reduce f val coll)
  (let ((s (seq coll)))
    (if s
        (reduce f (f val (first s)) (rest s))
        val)))

(define-method (reduce f coll)
  (let ((s (seq coll)))
    (if s
        (reduce f (first s) (rest s))
        (f))))

(define (merge . xs)
  (if (null? xs)
      #nil
      (let loop ((xs xs)
                 (result (car xs)))
        (if (null? xs)
            result
            (loop (cdr xs)
                  (reduce (lambda (result x) (conj result x))
                          result
                          (car xs)))))))

(define-syntax doseq
  (lambda (x)
    (syntax-case x ()
      ((_ (vec-tag var init) body ...)  (vec-tag? #'vec-tag)
       #'(doseq (var init) body ...))
      ((_ (var init) body ...)
       #'(let loop ((s init))
           (let ((s (seq s)))
             (if (not s)
                 #nil
                 (let ((var (first s)))
                   body ...
                   (loop (rest s))))))))))
