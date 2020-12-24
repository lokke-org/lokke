;;; Copyright (C) 2019 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

;; This module must not depend on (lokke collection) because it
;; depends on (lokke base syntax) which depends on (lokke base
;; destructure) which depends on hash-map which depends on this.

(define-module (lokke base map-entry)
  #:use-module (oop goops)
  #:use-module ((lokke base collection)
                #:select (<sequential>
                          assoc
                          cons
                          first
                          get
                          nth
                          rest
                          second
                          seq))
  #:use-module ((lokke compare) #:select (clj=))
  #:use-module ((lokke pr) #:select (pr-readable pr-approachable))
  #:export (<map-entry> key map-entry map-entry? val)
  #:re-export (assoc
               clj=
               first
               get
               nth
               pr-approachable
               pr-readable
               rest
               second
               seq)
  #:duplicates (merge-generics replace warn-override-core warn last))

(define-class <map-entry> (<sequential>)
  (k #:init-keyword #:k)
  (v #:init-keyword #:v))

(define (map-entry k v)
  (make <map-entry> #:k k #:v v))

(define-method (first (entry <map-entry>)) (slot-ref entry 'k))
(define-method (second (entry <map-entry>)) (slot-ref entry 'v))
(define-method (key (entry <map-entry>)) (slot-ref entry 'k))
(define-method (val (entry <map-entry>)) (slot-ref entry 'v))

(define (show m emit port)
  (display "[" port)
  (emit (key m) port)
  (display " " port)
  (emit (val m) port)
  (display "]" port))

(define-method (pr-readable (v <map-entry>) port)
  (show v pr-readable port))

(define-method (pr-approachable (v <map-entry>) port)
  (show v pr-approachable port))

(define (map-entry? x) (is-a? x <map-entry>))

(define-method (count (entry <map-entry>)) 2)
(define-method (counted? (entry <map-entry>)) #t)
(define-method (not-empty (entry <map-entry>)) entry)
(define-method (contains? (entry <map-entry>) i) (or (= i 0) (= i 1)))

(define-method (assoc (entry <map-entry>) i x)
  (case i
    ((0) (map-entry x (slot-ref entry 'v)))
    ((1) (map-entry (slot-ref entry 'k) x))
    (else (scm-error 'out-of-range 'get "<map-entry> index out of range: ~a"
                     (list i) (list i)))))

(define-method (get (entry <map-entry>) i)
  (case i
    ((0) (slot-ref entry 'k))
    ((1) (slot-ref entry 'v))
    (else (scm-error 'out-of-range 'get "<map-entry> index out of range: ~a"
                     (list i) (list i)))))

(define-method (get (entry <map-entry>) i not-found)
  (case i
    ((0) (slot-ref entry 'k))
    ((1) (slot-ref entry 'v))
    (else not-found)))

(define nth get)  ;; efficient, but has the wrong name when printed?

(define-method (clj= (e1 <map-entry>) (e2 <map-entry>))
  (and (clj= (slot-ref e1 'k) (slot-ref e2 'k))
       (clj= (slot-ref e1 'v) (slot-ref e2 'v))))

;; FIXME?
(define-method (seq (entry <map-entry>))
  (cons (slot-ref entry 'k)
        (cons (slot-ref entry 'v) #nil)))

(define-method (rest (entry <map-entry>))
  (cons (slot-ref entry 'v) #nil))
