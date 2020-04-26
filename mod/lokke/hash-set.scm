;;; Copyright (C) 2019 Rob Browning <rlb@defaultvalue.org>
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

(define-module (lokke hash-set)
  #:use-module ((fash)
                #:select (make-fash
                          fash-fold
                          fash-ref
                          fash-set
                          fash-size))
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
  #:use-module ((lokke compare) #:select (clj=))
  #:use-module ((lokke hash-map) #:select (<hash-map>))
  #:use-module ((lokke pr) #:select (pr-on print-on))
  #:use-module ((lokke set) #:select (<set>))
  #:use-module (oop goops)
  #:use-module ((srfi srfi-41) #:select (stream-lambda))
  #:use-module ((srfi srfi-69) #:prefix hash/)
  #:use-module ((rnrs) :prefix rnrs/)
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
               into
               meta
               pr-on
               print-on
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
  (meta #:init-keyword #:meta))

(define-inlinable (make-set fm meta)
  (make <hash-set> #:internals fm #:meta meta))

(define-inlinable (set-fm s) (slot-ref s 'internals))
(define-inlinable (set-meta s) (slot-ref s 'meta))

(define-method (meta (s <hash-set>)) (set-meta s))

(define-method (with-meta (s <hash-set>) (mdata <boolean>))
  (require-nil 'with-meta 2 mdata)
  (make-set (set-fm s) mdata))

(define-method (with-meta (s <hash-set>) (mdata <hash-map>))
  (make-set (set-fm s) mdata))

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

(define-method (pr-on (s <hash-set>) port)
  (show s pr-on port))

(define-method (print-on (s <hash-set>) port)
  (show s print-on port))

(define-method (write (m <hash-set>) port)
  (format port "#<~s ~x " (class-name (class-of m)) (object-address m))
  (display (set-fm m) port)
  (display ">" port))

(define (hash-set? x) (is-a? x <hash-set>))

(define empty-fash (make-fash #:hash hash/hash #:equal clj=))
(define empty-hash-set (make-set empty-fash #nil))
(define (empty-hash-set-w-meta data)
  (if (eq? #nil data)
      empty-hash-set
      (make-set empty-fash data)))

(define (set coll)
  (make-set (reduce (lambda (result x) (fash-set result x x))
                    (set-fm empty-hash-set)
                    coll)
            #nil))

(define (hash-set . xs) (set xs))

(define-method (conj (s <hash-set>) . xs)
  (make-set (reduce (lambda (result x) (fash-set result x x))
                    (set-fm s)
                    xs)
            (set-meta s)))

(define-method (disj (s <hash-set>) . xs)
  (make-set (reduce (lambda (result x) (fash-set result x not-found))
                    (set-fm s)
                    xs)
            (set-meta s)))

(define-method (count (x <hash-set>))
  (fash-fold (lambda (k v size) (if (eq? v not-found) size (1+ size)))
             (set-fm x)
             0))

(define-method (counted? (x <hash-set>))
  #t)

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
  (let ((item (some-item (set-fm s))))
    (if (eq? item not-found)
        #nil
        (lazy-seq (cons item (disj s item))))))

(define-method (seqable? (b <hash-set>)) #t)

(define-method (rest (s <hash-set>)) (rest (seq s)))

;; FIXME: custom merge?
