;;; Copyright (C) 2019 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

(define-module (lokke hash-set)
  #:use-module ((lokke fash)
                #:select (make-fash
                          fash-fold
                          fash-ref
                          fash-update))
  #:use-module ((ice-9 control) #:select (call/ec))
  #:use-module ((ice-9 format) #:select (format))
  #:use-module ((lokke base invoke) #:select (invoke))
  #:use-module ((lokke base metadata) #:select (meta with-meta))
  #:use-module ((lokke base util) #:select (require-nil))
  #:use-module ((lokke base collection)
                #:select (conj
                          cons
                          contains?
                          count
                          counted?
                          empty
                          get
                          into
                          lazy-seq
                          reduce
                          rest
                          seq
                          seqable?))
  #:use-module ((lokke compare) #:select (clj= hash))
  #:use-module ((lokke hash-map) #:select (<hash-map>))
  #:use-module ((lokke pr) #:select (pr-approachable pr-readable))
  #:use-module ((lokke set) #:select (<set>))
  #:use-module (oop goops)
  #:use-module ((rnrs sorting) #:select (vector-sort!))
  #:use-module ((srfi srfi-43) #:select (vector-fold))
  #:export (<hash-set>
            disj
            foo
            hash-set
            hash-set?
            set)
  #:re-export (clj=
               conj
               contains?
               count
               empty
               get
               hash
               into
               meta
               pr-approachable
               pr-readable
               seq
               seqable?
               with-meta)
  #:duplicates (merge-generics replace warn-override-core warn last))

;; FIXME: implement (lokke set) operations, here, or more generically
;; when appropriate, there.

;; Until/unless fash has a delete method, also store this for dissocs
(define not-found (make-symbol "not-found"))

(define* (ref m key #:optional (nope #nil))
  (let ((result (fash-ref m key (lambda (k) not-found))))
    (if (eq? result not-found)
        nope
        result)))

(define-class <hash-set> (<set>)
  (internals #:init-keyword #:internals)
  ;; FIXME: remove once fash supports disj
  (count #:init-keyword #:count)
  (meta #:init-keyword #:meta))

(define-inlinable (make-set fm n meta)
  (make <hash-set> #:internals fm #:count n #:meta meta))

(define-inlinable (set-fm s) (slot-ref s 'internals))
(define-inlinable (set-count s) (slot-ref s 'count))
(define-inlinable (set-meta s) (slot-ref s 'meta))

(define-method (meta (s <hash-set>)) (set-meta s))

(define-method (with-meta (s <hash-set>) (mdata <boolean>))
  (require-nil 'with-meta 2 mdata)
  (make-set (set-fm s) (set-count s) mdata))

(define-method (with-meta (s <hash-set>) (mdata <hash-map>))
  (make-set (set-fm s) (set-count s) mdata))

(define-method (get (s <hash-set>) x)
  (ref (set-fm s) x))

(define-method (get (s <hash-set>) x not-found)
  (ref (set-fm s) x not-found))

(define (show s emit port)
  (display "#{" port)
  (let ((first? #t))
    (fash-fold (lambda (k v result)
                 (unless (eq? v not-found)
                   (if first?
                       (set! first? #f)
                       (display " " port))
                   (emit k port)))
               (set-fm s)
               #t))
  (display "}" port))

(define-method (pr-readable (s <hash-set>) port)
  (show s pr-readable port))

(define-method (pr-approachable (s <hash-set>) port)
  (show s pr-approachable port))

(define-method (write (m <hash-set>) port)
  (format port "#<~s ~x " (class-name (class-of m)) (object-address m))
  (display (set-fm m) port)
  (display ">" port))

(define (hash-set? x) (is-a? x <hash-set>))

(define empty-fash (make-fash #:hash hash #:equal clj=))
(define empty-hash-set (make-set empty-fash 0 #nil))
(define (empty-hash-set-w-meta data)
  (if (eq? #nil data)
      empty-hash-set
      (make-set empty-fash 0 data)))

(define (set coll)
  (let* ((n 0)
         (fm (reduce (lambda (result x)
                       (fash-update result x
                                    (lambda (existing)
                                      (when (eq? existing not-found)
                                        (set! n (1+ n)))
                                      x)
                                    not-found))
                     (set-fm empty-hash-set)
                     coll)))
    (make-set fm n #nil)))

(define (hash-set . xs) (set xs))

(define-method (conj (s <hash-set>) . xs)
  (let* ((n (set-count s))
         (fm (reduce (lambda (result x)
                       (fash-update result x
                                    (lambda (existing)
                                      (when (eq? existing not-found)
                                        (set! n (1+ n)))
                                      x)
                                    not-found))
                     (set-fm s)
                     xs)))
    (make-set fm n (set-meta s))))

(define-method (disj (s <hash-set>) . xs)
  (let* ((n (set-count s))
         (fm (reduce (lambda (result x)
                       (fash-update result x
                                    (lambda (existing)
                                      (unless (eq? existing not-found)
                                        (set! n (1- n)))
                                     not-found)
                                    not-found))
                     (set-fm s)
                     xs)))
    (make-set fm n (set-meta s))))

(define-method (count (x <hash-set>)) (set-count x))
(define-method (counted? (x <hash-set>)) #t)

(define-method (empty (x <hash-set>))
  (empty-hash-set-w-meta (set-meta x)))

;; not-empty - generic default is correct

(define-method (contains? (s <hash-set>) x)
  (not (eq? not-found (ref (set-fm s) x not-found))))

(define-method (get (s <hash-set>) x)
  (ref (set-fm s) x))

(define-method (get (s <hash-set>) x not-found)
  (ref (set-fm s) x not-found))

(define (hash-set-= s1 s2)
  (let* ((h1 (set-fm s1))
         (h2 (set-fm s2))
         (exit (make-symbol "exit"))
         (subset? (lambda (m of-m)
                    (fash-fold (lambda (k v result)
                                 (let ((v2 (ref of-m k not-found)))
                                   (unless (clj= v v2)
                                     (throw exit #f)))
                                 #t)
                               m
                               #t))))
    ;; Can't use the size check until fash supports delete and we
    ;; don't have to ignore the not-found tokens.
    ;; (= (fash-size h1) (fash-size h2))
    (catch exit
      (lambda () (and (subset? h1 h2) (subset? h2 h1)))
      (lambda args #f))))

;; specialize this so that we'll bypass the generic <sequential> flavor
(define-method (clj= (s1 <hash-set>) (s2 <hash-set>)) (hash-set-= s1 s2))

;; FIXME: add iterator to map?

(define (some-item fm)
  (call/ec
   (lambda (return)
     (fash-fold (lambda (k v result)
                  (unless (eq? v not-found)
                    (return k)))
                fm #t)
     not-found)))

(define-method (seq (s <hash-set>))
  (let loop ((s s))
    (let ((item (some-item (set-fm s))))
      (if (eq? item not-found)
          #nil
          (cons item (lazy-seq (loop (disj s item))))))))

(define-method (seqable? (b <hash-set>)) #t)

(define-method (rest (s <hash-set>)) (rest (seq s)))

;; FIXME: depth diminishing tree-structured sampling like guile's hash.c?

(define-method (hash (x <hash-set>))
  (let* ((m (set-fm x))
         (n (set-count x))
         (hashes (make-vector n)))
    (fash-fold (lambda (k v i)
                 (unless (eq? v not-found)
                   (vector-set! hashes i (hash k))
                   (1+ i)))
               m
               0)
    (vector-sort! < hashes)
    (vector-fold (lambda (i result x) (logxor result (hash x)))
                 (hash n)
                 hashes)))

;; FIXME: custom merge?
