;;; Copyright (C) 2015-2022 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

;; This module must not depend on (lokke collection) because it
;; depends on (lokke base syntax) which depends on (lokke base
;; destructure) which depends on hash-map.

(define-module (lokke hash-map)
  ;; FIXME: first and second should be together
  #:use-module ((ice-9 atomic)
                #:select (atomic-box-ref atomic-box-set! make-atomic-box))
  #:use-module ((ice-9 control) #:select (call/ec))
  #:use-module ((ice-9 format) #:select (format))
  #:use-module ((lokke base collection)
                #:select (<coll>
                          assoc
                          conj
                          cons
                          contains?
                          count
                          counted?
                          dissoc
                          empty
                          find
                          first
                          get
                          keys
                          lazy-seq
                          reduce
                          reduce-kv
                          second
                          select-keys
                          seq
                          seqable?
                          update
                          vals))
  #:use-module ((lokke base metadata) #:select (meta with-meta))
  #:use-module ((lokke base map)
                #:select (<map> map-invert update-keys update-vals))
  #:use-module ((lokke fash)
                #:select (fash-fold
                          fash-ref
                          fash-set!
                          fash-update
                          make-fash
                          make-transient-fash
                          persistent-fash))
  #:use-module ((lokke base map-entry) #:select (map-entry))
  #:use-module ((lokke base util) #:select (require-nil))
  #:use-module ((lokke compare) #:select (clj= hash))
  #:use-module ((lokke compat) #:select (re-export-and-replace!))
  #:use-module ((lokke pr) #:select (pr-approachable pr-readable))
  #:use-module (oop goops)
  #:use-module ((rnrs sorting) #:select (vector-sort!))
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module ((srfi srfi-43) #:select (vector-fold))
  #:export (<hash-map>
            hash-map
            hash-map?
            kv-list)
  #:re-export (clj=
               conj
               cons
               contains?
               count
               counted?
               dissoc
               empty
               get
               keys
               map-invert
               meta
               pr-approachable
               pr-readable
               reduce-kv
               select-keys
               seq
               seqable?
               with-meta
               update
               update-keys
               update-vals
               vals)
  #:duplicates (merge-generics replace warn-override-core warn last))

(re-export-and-replace! 'assoc 'hash)

;; FIXME: move rest of common map generics to (lokke map)

;; Q: wonder if we might want to match clojure in clojure.lang,
;; i.e. PersistentArrayMap

;; Until/unless fash has a delete method, also store this for dissocs
(define not-found (make-symbol "not-found"))

(define* (ref m key #:optional (nope #nil))
  (let ((result (fash-ref m key (lambda (k) not-found))))
    (if (eq? result not-found)
        nope
        result)))

(define-class <hash-map> (<map>)
  (internals #:init-keyword #:internals)
  (count #:init-keyword #:count)
  (hash #:init-thunk (lambda () (make-atomic-box #nil)))
  (meta #:init-keyword #:meta))

(define-inlinable (make-map fm n meta)
  (make <hash-map> #:internals fm #:count n #:meta meta))

(define-inlinable (map-fm m) (slot-ref m 'internals))
(define-inlinable (map-count m) (slot-ref m 'count))
(define-inlinable (map-hash m) (slot-ref m 'hash))
(define-inlinable (map-meta m) (slot-ref m 'meta))

(define-method (meta (m <hash-map>)) (map-meta m))

(define-method (with-meta (m <hash-map>) (mdata <boolean>))
  (require-nil 'with-meta 2 mdata)
  (if (nil? (map-meta m))
      m
      (make-map (map-fm m) (map-count m) mdata)))

(define-method (with-meta (m <hash-map>) (mdata <hash-map>))
  (if (eq? mdata (map-meta m))
      m
      (make-map (map-fm m) (map-count m) mdata)))

(define (show m emit port)
  (display "{" port)
  (let ((first? #t))
    (fash-fold (lambda (k v result)
                 (unless (eq? v not-found)
                   (if first?
                       (set! first? #f)
                       (display ", " port))
                   (emit k port)) (display #\space port) (emit v port))
               (map-fm m)
               #t))
  (display "}" port))

(define-method (pr-readable (m <hash-map>) port)
  (show m pr-readable port))

(define-method (pr-approachable (m <hash-map>) port)
  (show m pr-approachable port))

(define-method (write (m <hash-map>) port)
  (format port "#<~s ~x " (class-name (class-of m)) (object-address m))
  (display (map-fm m) port)
  (display ">" port))

(define (hash-map? x) (is-a? x <hash-map>))

(define-method (assoc (m <hash-map>) k v)
  (let* ((n (map-count m))
         (fm (fash-update (map-fm m) k
                          (lambda (existing)
                            (when (eq? existing not-found)
                              (set! n (1+ n)))
                            v)
                          not-found)))
    (make-map fm n (map-meta m))))

(define empty-fash (make-fash #:hash hash #:equal clj=))
(define (empty-transient-fash) (make-transient-fash #:hash hash #:equal clj=))

(define empty-hash-map (make-map empty-fash 0 #nil))
(define (empty-hash-map-w-meta data)
  (if (eq? #nil data)
      empty-hash-map
      (make-map empty-fash 0 data)))

(define (hash-map . alternating-keys-and-values)
  (if (null? alternating-keys-and-values)
      empty-hash-map
      (apply assoc
             empty-hash-map
             alternating-keys-and-values)))

(define-method (kv-list (m <hash-map>))
  (fash-fold (lambda (k v result)
               (if (eq? v not-found)
                   result
                   (cons k (cons v result))))
             (map-fm m)
             '()))

(define-method (conj (m1 <hash-map>) (m2 <hash-map>))
  ;; FIXME: should this just be in terms of assoc or an %assoc?
  (let* ((n (map-count m1))
         (fm (fash-fold (lambda (k v result)
                          (if (eq? v not-found)
                              result
                              (fash-update result k
                                           (lambda (existing)
                                             (when (eq? existing not-found)
                                               (set! n (1+ n)))
                                             v)
                                           not-found)))
                        (map-fm m2)
                        (map-fm m1))))
    (make-map fm n (map-meta m1))))

(define-method (dissoc (m <hash-map>) . xs)
  (let* ((n (map-count m))
         (fm (fold (lambda (x result)
                     (fash-update result x
                                  (lambda (existing)
                                      (unless (eq? existing not-found)
                                        (set! n (1- n)))
                                      not-found)
                                  not-found))
                   (map-fm m)
                   xs)))
    (make-map fm n (map-meta m))))

(define-method (counted? (m <hash-map>)) #t)
(define-method (count (m <hash-map>)) (map-count m))

(define-method (empty (m <hash-map>)) (empty-hash-map-w-meta (map-meta m)))
;; not-empty - generic default is correct

(define-method (contains? (m <hash-map>) x)
  (not (eq? not-found (ref (map-fm m) x not-found))))

(define-method (get (m <hash-map>) x)
  (ref (map-fm m) x))

(define-method (get (m <hash-map>) x not-found)
  (ref (map-fm m) x not-found))

(define (hash-map-= m1 m2)
  (let* ((h1 (map-fm m1))
         (h2 (map-fm m2))
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
(define-method (clj= (m1 <hash-map>) (m2 <hash-map>)) (hash-map-= m1 m2))

;; FIXME: we could probably augment the data structure to support
;; incremental traversal.  For now, we're just not lazy.

;; FIXME: not lazy
(define-method (seq (m <hash-map>))
  ;; Should produce the same ordering as keys
  (if (zero? (map-count m))
      #nil
      (fash-fold (lambda (k v result)
                   (if (eq? v not-found)
                       result
                       (cons (map-entry k v) result)))
                 (map-fm m)
                 '())))

(define-method (seqable? (b <hash-map>)) #t)

;; FIXME: not lazy
(define-method (keys (m <hash-map>))
  ;; Should produce the same ordering as seq
  (if (zero? (map-count m))
      #nil
      (fash-fold (lambda (k v result)
                   (if (eq? v not-found)
                       result
                       (cons k result)))
                 (map-fm m)
                 '())))

(define-method (vals (m <hash-map>))
  (fash-fold (lambda (k v result)
               (if (eq? v not-found)
                   result
                   (cons v result)))
             (map-fm m)
             '()))

(define-method (find (m <hash-map>) k)
  (let ((v (get map k)))
    (when v
      (map-entry k v))))

(define-method (update (m <hash-map>) k f . args)
  (make-map (apply fash-update (map-fm m) k f #nil args)
            (map-count m)
            (map-meta m)))

(define-method (reduce-kv f init (m <hash-map>))
  (fash-fold (lambda (k v result)
               (if (eq? v not-found)
                   result
                   (f result k v)))
             (map-fm m)
             init))

(define-method (select-keys (m <hash-map>) keys)
  (let ((fm (map-fm m))
        (n 0))
    (make-map (persistent-fash
               (reduce (lambda (result k)
                         (let ((v (fash-ref fm k (lambda (_) not-found))))
                           (if (eq? v not-found)
                               result
                               (begin
                                 (set! n (1+ n))
                                 (fash-set! result k v)))))
                       (empty-transient-fash)
                       keys))
              n
              (map-meta m))))

(define-method (hash (x <hash-map>))
  (let* ((box (map-hash x))
         (h (atomic-box-ref box)))
    (or h
        (let* ((m (map-fm x))
               (n (map-count x))
               (hashes (make-vector n)))
          (fash-fold (lambda (k v i)
                       (unless (eq? v not-found)
                         (vector-set! hashes i (logxor (hash k) (hash v)))
                         (1+ i)))
                     m
                     0)
          (vector-sort! < hashes)
          (let ((h (vector-fold (lambda (i result x) (logxor result (hash x)))
                                (hash n)
                                hashes)))
            (atomic-box-set! box h)
            h)))))

(define-method (map-invert (m <hash-map>))
  (make-map (persistent-fash
             (fash-fold (lambda (k v result) (fash-set! result v k))
                        (map-fm m)
                        (empty-transient-fash)))
            (map-count m)
            #nil))

(define-method (update-keys m f)
  (make-map (persistent-fash
             (fash-fold (lambda (k v result) (fash-set! result (f k) v))
                        (map-fm m)
                        (empty-transient-fash)))
            (map-count m)
            #nil))

(define-method (update-vals m f)
  (make-map (persistent-fash
             (fash-fold (lambda (k v result) (fash-set! result k (f v)))
                        (map-fm m)
                        (empty-transient-fash)))
            (map-count m)
            #nil))

;; FIXME: custom merge?
