;;; Copyright (C) 2015-2020 Rob Browning <rlb@defaultvalue.org>
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

;; This module must not depend on (lokke collection) because it
;; depends on (lokke base syntax) which depends on (lokke base
;; destructure) which depends on hash-map.

(define-module (lokke hash-map)
  ;; FIXME: first and second should be together
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
                          merge
                          reduce-kv
                          second
                          select-keys
                          seq
                          update
                          vals))
  #:use-module ((lokke base metadata) #:select (meta with-meta))
  #:use-module ((lokke base map) #:select (<map>))
  #:use-module ((fash)
                #:select (make-fash
                          fash-fold
                          fash-ref
                          fash-set
                          fash-size))
  #:use-module ((lokke base map-entry) #:select (map-entry))
  #:use-module ((lokke base util) #:select (require-nil))
  #:use-module ((lokke compare) #:select (clj=))
  #:use-module ((lokke compat) #:select (re-export-and-replace!))
  #:use-module ((lokke pr) #:select (pr-on print-on))
  #:use-module (oop goops)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module ((srfi srfi-69) #:prefix hash/)
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
               equal?
               get
               keys
               meta
               pr-on
               print-on
               reduce-kv
               select-keys
               seq
               with-meta
               update
               vals)
  #:duplicates (merge-generics replace warn-override-core warn last))

(re-export-and-replace! 'apply 'assoc 'merge)

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
  (meta #:init-keyword #:meta))

(define-syntax-rule (make-map fm meta)
  (make <hash-map> #:internals fm #:meta meta))

(define-syntax-rule (map-fm m) (slot-ref m 'internals))
(define-syntax-rule (map-meta m) (slot-ref m 'meta))

(define-method (meta (m <hash-map>)) (map-meta m))

(define-method (with-meta (m <hash-map>) (mdata <boolean>))
  (require-nil 'with-meta 2 mdata)
  (make-map (map-fm m) mdata))

(define-method (with-meta (m <hash-map>) (mdata <hash-map>))
  (make-map (map-fm m) mdata))

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

(define-method (pr-on (m <hash-map>) port)
  (show m pr-on port))

(define-method (print-on (m <hash-map>) port)
  (show m print-on port))

(define-method (write (m <hash-map>) port)
  (format port "#<~s ~x " (class-name (class-of m)) (object-address m))
  (display (map-fm m) port)
  (display ">" port))

(define (hash-map? x) (is-a? x <hash-map>))

(define-method (assoc (m <hash-map>) k v)
  (make-map (fash-set (map-fm m) k v)
            (map-meta m)))

(define empty-fash (make-fash #:hash hash/hash #:equal clj=))
(define empty-hash-map (make-map empty-fash #nil))
(define (empty-hash-map-w-meta data)
  (if (eq? #nil data)
      empty-hash-map
      (make-map empty-fash data)))

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
  (make-map (fash-fold (lambda (k v result)
                         (if (eq? v not-found)
                             result
                             (fash-set result k v)))
                       (map-fm m2)
                       (map-fm m1))
            (map-meta m1)))

(define-method (dissoc (m <hash-map>) . xs)
  (make-map (fold (lambda (x result)
                    (fash-set result x not-found))
                  (map-fm m)
                  xs)
            (map-meta m)))

(define-method (count (m <hash-map>))
  (fash-fold (lambda (k v size) (if (eq? v not-found) size (1+ size)))
             (map-fm m)
             0))

(define-method (counted? (m <hash-map>)) #t)
(define-method (empty (m <hash-map>)) (empty-hash-map-w-meta (map-meta m)))
;; not-empty - generic default is correct

(define-method (contains? (m <hash-map>) x)
  (not (eq? not-found (ref (map-fm m) x not-found))))

(define-method (get (m <hash-map>) x)
  (ref (map-fm m) x))

(define-method (get (m <hash-map>) x not-found)
  (ref (map-fm m) x not-found))

(define (hash-map-equal? m1 m2)
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

(define-method (equal? (m1 <hash-map>) (m2 <hash-map>)) (hash-map-equal? m1 m2))

;; specialize this so that we'll bypass the generic <sequential> flavor
(define-method (clj= (m1 <hash-map>) (m2 <hash-map>)) (hash-map-equal? m1 m2))

;; FIXME: we might need to augment the data structure, but we should
;; be able to do better with respect to seq traversal.  Need similar
;; changes for keys, etc.

(define (some-item fm)
  (call/ec
   (lambda (return)
     ;; FIXME: vector...
     (fash-fold (lambda (k v result)
                  (unless (eq? v not-found)
                    (return (map-entry k v))))
                fm #t)
     not-found)))

(define-method (seq (m <hash-map>))
  (let ((item (some-item (map-fm m))))
    (if (eq? item not-found)
        #nil
        (lazy-seq (cons item (dissoc m (first item)))))))

;; FIXME: not lazy
(define-method (keys (m <hash-map>))
  (fash-fold (lambda (k v result)
               (if (eq? v not-found)
                   result
                   (cons k result)))
             (map-fm m)
             '()))

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

(define (update m k f . args)
  (let* ((fm (map-fm m))
         (v (ref fm k)))
    (make-map (fash-set fm k (apply v args))
              (map-meta m))))

(define-method (reduce-kv f init (m <hash-map>))
  (fash-fold (lambda (k v result)
               (if (eq? v not-found)
                   result
                   (f result k v)))
             (map-fm m)
             init))

(define-method (select-keys (m <hash-map>) keys)
  (fash-fold (lambda (k v result)
               (if (eq? v not-found)
                   result
                   (assoc result k v)))
             (map-fm m)
             (empty-hash-map-w-meta (map-meta m))))

;; FIXME: custom merge?
