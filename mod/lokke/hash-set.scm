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
  #:use-module ((lokke base invoke) #:select (invoke))
  #:use-module ((ice-9 control) #:select (call/ec))
  #:use-module ((lokke collection)
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
                          rest seq))
  #:use-module ((lokke compare) #:select (clj=))
  #:use-module ((lokke pr) #:select (pr-on print-on))
  #:use-module ((lokke set) #:select (<set>))
  #:use-module (oop goops)
  #:use-module ((pfds hamts) #:prefix hamts/)
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
               pr-on
               print-on)
  #:duplicates (merge-generics replace warn-override-core warn last))

;; FIXME: implement (lokke set) operations, here, or more generically
;; when appropriate, there.

(define-class <hash-set> (<set>)
  (internals #:init-keyword #:internals))

(define-syntax-rule (set-hamt s) (slot-ref s 'internals))

(define (read-only-str s) (substring/read-only s 0))

(define (render-str s render)
  (let* ((first? #t)
         (content (hamts/hamt-fold (lambda (x _ result)
                                     (when first? (set! first? #f))
                                     (cons (string-append (if first? "" " ")
                                                          (render x))
                                           result))
                                   '()
                                   (set-hamt s)))
         (content (reverse! content)))
    (read-only-str (apply string-append "#{" (append content '("}"))))))

(define (show s emit port)
  (display "#{" port)
  (let ((first? #t))
    (hamts/hamt-fold (lambda (k _ result)
                       (if first?
                           (set! first? #f)
                           (display " " port))
                       (emit k port))
                     #t
                     (set-hamt s)))
  (display "}" port))

(define-method (pr-on (s <hash-set>) port)
  (show s pr-on port))

(define-method (print-on (s <hash-set>) port)
  (show s print-on port))

(define (hash-set? x) (is-a? x <hash-set>))

(define (set coll)
  (make <hash-set>
    #:internals (reduce (lambda (result x)
                          (hamts/hamt-set result x x))
                        (hamts/make-hamt hash/hash eqv?)
                        coll)))

(define (hash-set . xs) (set xs))

(define-method (conj (s <hash-set>) . xs)
  (make <hash-set>
    #:internals (reduce (lambda (result x) (hamts/hamt-set result x x))
                        (set-hamt s)
                        xs)))

(define-method (disj (s <hash-set>) . xs)
  (make <hash-set>
    #:internals (reduce (lambda (result x) (hamts/hamt-delete result x))
                        (set-hamt s)
                        xs)))

(define-method (count (x <hash-set>))
  (hamts/hamt-size (set-hamt x)))

(define-method (counted? (x <hash-set>))
  #t)

(define-method (empty (x <hash-set>))
  (hash-set))

;; not-empty - generic default is correct

(define-method (contains? (s <hash-set>) x)
  (hamts/hamt-contains? (set-hamt s) x))

(define-method (get (s <hash-set>) x)
  (hamts/hamt-ref (set-hamt s) x #nil))

(define-method (get (s <hash-set>) x not-found)
  (hamts/hamt-ref (set-hamt s) x not-found))

(define (hash-set-equal? s1 s2)
  (let ((h1 (set-hamt s1))
        (h2 (set-hamt s2))
        (exit (make-symbol "exit")))
    (and (= (hamts/hamt-size h1) (hamts/hamt-size h2))
         (catch exit
           (lambda ()
             (hamts/hamt-fold (lambda (k _ result)
                                (unless (hamts/hamt-contains? h2 k)
                                  (throw exit #f))
                                #t)
                              #t
                              h1))
           (lambda args
             #f)))))

(define-method (equal? (s1 <hash-set>) (s2 <hash-set>)) (hash-set-equal? s1 s2))

;; specialize this so that we'll bypass the generic <sequential> flavor
(define-method (clj= (s1 <hash-set>) (s2 <hash-set>)) (hash-set-equal? s1 s2))

;; FIXME: adapt hamt-fold or move to some other data structure...

(define (some-item hamt not-found)
  (call/ec
   (lambda (return)
     (hamts/hamt-fold (lambda (k _ result) (return k)) #t hamt)
     not-found)))

(let ((not-found (make-symbol "hash-set-not-found")))
  (define-method (seq (s <hash-set>))
    (let ((item (some-item (set-hamt s) not-found)))
      (if (eq? item not-found)
          #nil
          (lazy-seq (cons item (disj s item)))))))

(define-method (rest (s <hash-set>)) (rest (seq s)))

;; FIXME: custom merge?
