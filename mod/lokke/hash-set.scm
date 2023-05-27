;;; Copyright (C) 2019-2022 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

(define-module (lokke hash-set)
  #:use-module ((ice-9 atomic)
                #:select (atomic-box-ref atomic-box-set! make-atomic-box))
  #:use-module ((lokke fash)
                #:select (make-fash
                          fash-fold
                          fash-ref
                          fash-set!
                          fash-update
                          make-transient-fash
                          persistent-fash
                          transient-fash))
  #:use-module ((ice-9 control) #:select (let/ec))
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
                          empty?
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
  #:use-module ((lokke set) #:select (<set> difference intersection union))
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
               difference
               empty
               get
               hash
               intersection
               into
               meta
               pr-approachable
               pr-readable
               seq
               seqable?
               union
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
  (hash #:init-thunk (lambda () (make-atomic-box #nil)))
  (meta #:init-keyword #:meta))

(define-inlinable (make-set fm n meta)
  (make <hash-set> #:internals fm #:count n #:meta meta))

(define-inlinable (set-fm s) (slot-ref s 'internals))
(define-inlinable (set-count s) (slot-ref s 'count))
(define-inlinable (set-hash m) (slot-ref m 'hash))
(define-inlinable (set-meta s) (slot-ref s 'meta))

(define-method (meta (s <hash-set>)) (set-meta s))

(define-method (with-meta (s <hash-set>) (mdata <boolean>))
  (require-nil 'with-meta 2 mdata)
  (if (nil? (set-meta s))
      s
      (make-set (set-fm s) (set-count s) mdata)))

(define-method (with-meta (s <hash-set>) (mdata <hash-map>))
  (if (eq? mdata (set-meta s))
      s
      (make-set (set-fm s) (set-count s) mdata)))

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
  (simple-format port "#<~s ~a "
                 (class-name (class-of m))
                 (number->string (object-address m) 16))
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
  (let/ec exit
    (let* ((h1 (set-fm s1))
           (h2 (set-fm s2))
           (subset? (lambda (m of-m)
                      (fash-fold (lambda (k v result)
                                   (let ((v2 (ref of-m k not-found)))
                                     (unless (clj= v v2)
                                       (exit #f)))
                                   #t)
                                 m
                                 #t))))
      ;; Can't use the size check until fash supports delete and we
      ;; don't have to ignore the not-found tokens.
      ;; (= (fash-size h1) (fash-size h2))
      (and (subset? h1 h2) (subset? h2 h1)))))

;; specialize this so that we'll bypass the generic <sequential> flavor
(define-method (clj= (s1 <hash-set>) (s2 <hash-set>)) (hash-set-= s1 s2))

;; FIXME: add iterator to map?

(define (some-item fm)
  (let/ec return
   (fash-fold (lambda (k v result)
                (unless (eq? v not-found)
                  (return k)))
              fm #t)
   not-found))

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
  (let* ((box (set-hash x))
         (h (atomic-box-ref box)))
    (or h
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
          (let ((h (vector-fold (lambda (i result x) (logxor result (hash x)))
                                (hash n)
                                hashes)))
            (atomic-box-set! box h)
            h)))))

;; FIXME: custom merge?

(define-method (union) empty-hash-set)

(define-method (union (s1 <hash-set>) (s2 <set>))
  (cond
   ((empty? s1) s2)
   ((empty? s2) s1)
   (else
    (let* ((n (set-count s1))
           (h1 (set-fm s1))
           (fm (persistent-fash
                (reduce (lambda (result x)
                          (if (eq? not-found
                                   (fash-ref h1 x (lambda (_) not-found)))
                              (begin
                                (set! n (1+ n))
                                (fash-set! result x x))
                              result))
                        (transient-fash (set-fm s1))
                        s2))))
      (make-set fm n (meta s1))))))


(define (intersect-via-get s1 s2 s2-get)
  (let* ((n 0)
         (fm (persistent-fash
              (fash-fold (lambda (k v result)
                           (if (or (eq? not-found v)
                                   (eq? not-found (s2-get s2 k not-found)))
                               result
                               (begin
                                 (set! n (1+ n))
                                 (fash-set! result k v))))
                         (set-fm s1)
                         (make-transient-fash #:hash hash #:equal clj=)))))
    (make-set fm n (meta s1))))

(define-method (intersection (s1 <hash-set>) (s2 <hash-set>))
  (intersect-via-get s1 (set-fm s2) (lambda (s k not-found)
                                      (fash-ref s k (lambda (_) not-found)))))

(define-method (intersection (s1 <hash-set>) (s2 <set>))
  (intersect-via-get s1 s2 get))

;; FIXME: need "update if exists"

(define (difference-via-get s1 s1-get s1-n s2 m)
  (let* ((n s1-n)
         (fm (persistent-fash
              (fash-fold (lambda (k v result)
                           (cond
                            ((eq? not-found v) result)
                            ((eq? not-found (s1-get s1 k not-found)) result)
                            (else
                             (set! n (1- n))
                             (fash-set! result k not-found))))
                         (set-fm s2)
                         (transient-fash s1)))))
    (make-set fm n m)))

(define-method (difference (s1 <hash-set>) (s2 <hash-set>))
  (difference-via-get (set-fm s1)
                      (lambda (s k not-found)
                        (fash-ref s k (lambda (_) not-found)))
                      (set-count s1)
                      s2
                      (meta s1)))

(define-method (difference (s1 <hash-set>) (s2 <set>))
  (difference-via-get s1 get (set-count s1) s2 (meta s1)))
