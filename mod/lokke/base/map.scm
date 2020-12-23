;;; Copyright (C) 2019-2020 Rob Browning <rlb@defaultvalue.org>
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

(define-module (lokke base map)
  #:use-module ((ice-9 match) #:select (match))
  #:use-module ((lokke base collection) #:select (<coll> count every? get seq))
  #:use-module ((lokke base invoke) #:select (apply invoke))
  #:use-module ((lokke base map-entry) #:select (key val))
  #:use-module ((lokke compare) #:select (clj=))
  #:use-module ((lokke compat) #:select (re-export-and-replace!))
  #:use-module (oop goops)
  #:export (<map> map?)
  #:re-export (clj= get invoke)
  #:replace (assoc)
  #:duplicates (merge-generics replace warn-override-core warn last))

(re-export-and-replace! 'apply)

;; Define the <map> basics here.  They can't go in anything that
;; depends on (lokke collection) because it depends on (lokke base
;; syntax) which depends on (lokke base destructure) which depends on
;; hash-map.

(define sentinel (cons #f #f))  ;; Globally eq?/equal? unique token

(define-class <map> (<coll>))
(define (map? x) (is-a? x <map>))

(define-method (clj= (x <map>) (y <map>))
  ;; Fallback for heterogeneous comparisons.  Assume for now they're
  ;; both counted so this will be fast.
  (and (= (count x) (count y))
       (every? (lambda (entry)
                 (clj= (get y (key entry) sentinel)
                         (val entry)))
               (seq x))))

(define-method (invoke (s <map>) item) (get s item))
(define-method (invoke (s <map>) item not-found) (get s item not-found))

(define-method (apply (s <map>) . args)
  (match args
    (((item)) (get s item))
    ((item (not-found)) (get s item not-found))))
