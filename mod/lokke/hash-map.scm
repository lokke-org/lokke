;;; Copyright (C) 2015-2019 Rob Browning <rlb@defaultvalue.org>
;;;
;;; This project is free software; you can redistribute it and/or
;;; modify it under the terms of (at your option) either of the
;;; following two licences:
;;;
;;;   1) The GNU Lesser General Public License as published by the
;;;      Free Software Foundation; either version 2.1, or (at your
;;;      option) any later version
;;;
;;;   2) The Eclipse Public License; either version 1.0 or (at your
;;;      option) any later version.

(read-set! keywords 'postfix)  ;; srfi-88

;; This module must not depend on (lokke collection) because it
;; depends on (lokke base syntax) which depends on (lokke base
;; destructure) which depends on hash-map.

(define-module (lokke hash-map)
  ;; FIXME: first and second should be together
  use-module: ((ice-9 control) select: (call/ec))
  use-module: ((lokke base collection)
               select: (<coll>
                        assoc
                        conj
                        cons
                        contains?
                        count
                        counted?
                        empty
                        find
                        first
                        get
                        lazy-seq
                        second
                        seq
                        update))
  use-module: ((lokke base map) select: (<map> select-keys))
  use-module: ((lokke base map-entry) select: (key map-entry val))
  use-module: ((lokke compare) select: (clj=))
  use-module: ((lokke pr) select: (*out* pr pr-str print print-str))
  use-module: (oop goops)
  use-module: ((pfds hamts) prefix: hamts/)
  use-module: ((srfi srfi-1) select: (fold))
  use-module: ((srfi srfi-69) prefix: hash/)
  export: (<hash-map>
           dissoc
           hash-map
           hash-map?
           keys
           kv-list
           merge
           reduce-kv
           vals)
  re-export: (assoc
              clj=
              conj
              cons
              contains?
              count
              counted?
              empty
              equal?
              get
              pr
              pr-str
              print
              print-str
              select-keys
              seq
              update)
  duplicates: (merge-generics replace warn-override-core warn last))

;; FIXME: move rest of common map generics to (lokke map)

;; Q: wonder if we might want to match clojure in clojure.lang,
;; i.e. PersistentArrayMap

(define not-found (make-symbol "not-found"))

(define-class <hash-map> (<map>)
  (internals init-keyword: internals:))

(define-syntax-rule (make-map hamt) (make <hash-map> internals: hamt))
(define-syntax-rule (map-hamt m) (slot-ref m 'internals))

(define (read-only-str s) (substring/read-only s 0))

(define (render-str m render)
  (let* ((first? #t)
         (content (hamts/hamt-fold (lambda (k v result)
                                     (when first? (set! first? #f))
                                     (cons (string-append (if first? "" ", ")
                                                          (render k) " " (render v))
                                           result))
                                   '()
                                   (map-hamt m)))
         (content (reverse! content)))
    (read-only-str
     (apply string-append "{" (append content '("}"))))))

(define-method (pr-str (m <hash-map>)) (render-str m pr-str))
(define-method (print-str (m <hash-map>)) (render-str m print-str))

(define (show m emit)
  (display "{" (*out*))
  (let ((first? #t))
    (hamts/hamt-fold (lambda (k v result)
                       (if first?
                           (set! first? #f)
                           (display ", " (*out*)))
                       (emit k) (display #\space (*out*)) (emit v))
                     #t
                     (map-hamt m)))
  (display "}" (*out*)))

(define-method (pr (m <hash-map>)) (show m pr))
(define-method (print (m <hash-map>)) (show m print))

(define (hash-map? x) (is-a? x <hash-map>))

(define-method (assoc (m <hash-map>) . alternating-keys-and-values)
  (let loop ((kvs alternating-keys-and-values)
             (result (map-hamt m)))
    (cond
     ((null? kvs) (make-map result))
     ((null? (cdr kvs)) (error "No value for key:" (car kvs)))
     (else (loop (cddr kvs)
                 (hamts/hamt-set result (car kvs) (cadr kvs)))))))

(define empty-hash-map (make-map (hamts/make-hamt hash/hash eqv?)))

(define (hash-map . alternating-keys-and-values)
  (if (null? alternating-keys-and-values)
      empty-hash-map
      (apply assoc
             empty-hash-map
             alternating-keys-and-values)))

(define-method (kv-list (m <hash-map>))
  (hamts/hamt-fold (lambda (k v result) (cons k (cons v result)))
                   '()
                   (map-hamt m)))

(define-method (conj (m <hash-map>) . kvs)
  (make-map (fold (lambda (kv result)
                    (hamts/hamt-set result (first kv) (second kv)))
                  (map-hamt m)
                  kvs)))

(define-method (dissoc (m <hash-map>) . xs)
  (make-map (fold (lambda (x result) (hamts/hamt-delete result x))
                  (map-hamt m)
                  xs)))

(define-method (count (m <hash-map>))
  (hamts/hamt-size (map-hamt m)))

(define-method (counted? (m <hash-map>)) #t)
(define-method (empty (m <hash-map>)) empty-hash-map)
;; not-empty - generic default is correct

(define-method (contains? (m <hash-map>) x)
  (hamts/hamt-contains? (map-hamt m) x))

(define-method (get (m <hash-map>) x)
  (hamts/hamt-ref (map-hamt m) x #nil))

(define-method (get (m <hash-map>) x not-found)
  (hamts/hamt-ref (map-hamt m) x not-found))

(define (hash-map-equal? m1 m2)
  (let ((h1 (map-hamt m1))
        (h2 (map-hamt m2))
        (exit (make-symbol "exit")))
    (and (= (hamts/hamt-size h1) (hamts/hamt-size h2))
         (catch exit
           (lambda ()
             (hamts/hamt-fold (lambda (k v result)
                                (let ((v2 (hamts/hamt-ref h2 k not-found)))
                                  (when (or (eq? not-found v2)
                                            (not (equal? v v2)))
                                    (throw exit #f)))
                                #t)
                              #t
                              h1))
           (lambda args #f)))))

(define-method (equal? (m1 <hash-map>) (m2 <hash-map>)) (hash-map-equal? m1 m2))

;; specialize this so that we'll bypass the generic <sequential> flavor
(define-method (clj= (m1 <hash-map>) (m2 <hash-map>)) (hash-map-equal? m1 m2))

;; FIXME: we might need to augment the data structure, but we should
;; be able to do better with respect to seq traversal.  Need similar
;; changes for keys, etc.

(define (some-item hamt not-found)
  (call/ec
   (lambda (return)
     ;; FIXME: vector...
     (hamts/hamt-fold (lambda (k v result) (return (map-entry k v))) #t hamt)
     not-found)))

(let ((not-found (make-symbol "hash-map-not-found")))
  (define-method (seq (m <hash-map>))
    (let ((item (some-item (map-hamt m) not-found)))
      (if (eq? item not-found)
          #nil
          (lazy-seq (cons item (dissoc m (first item))))))))

;; FIXME: not lazy
(define-method (keys (m <hash-map>))
  (hamts/hamt-fold (lambda (k v result) (cons k result))
                   '()
                   (map-hamt m)))

(define-method (vals (m <hash-map>))
  (hamts/hamt-fold (lambda (k v result) (cons v result))
                   '()
                   (map-hamt m)))

(define-method (find (m <hash-map>) k)
  (let ((v (get map k)))
    (when v
      (map-entry k v))))

(define (update map m k f . args)
  (make-map (hamts/hamt-update (map-hamt m)
                               k
                               (lambda (x) (apply x args))
                               #nil)))

(define-method (reduce-kv f init (m <hash-map>))
  (hamts/hamt-fold (lambda (k v result) (f result k v))
                   init
                   (map-hamt m)))

(define-method (select-keys (m <hash-map>) keys)
  (hamts/hamt-fold (lambda (k v result) (assoc result k v))
                   empty-hash-map
                   (map-hamt m)))

;; FIXME: custom merge?
