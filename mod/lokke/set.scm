;;; Copyright (C) 2019 Rob Browning <rlb@defaultvalue.org>
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

(define-module (lokke set)
  #:version (0 0 0)
  #:use-module ((lokke base collection) #:select (<coll> get))
  #:use-module ((lokke base invoke) #:select (invoke))
  #:use-module ((lokke collection) #:select (contains? count every? seq))
  #:use-module (oop goops)
  #:export (<set>
            difference
            index
            intersection
            join
            map-invert
            project
            rename
            rename-keys
            select
            set?
            subset?
            superset?
            union)
  #:re-export (invoke)
  #:duplicates (merge-generics replace warn-override-core warn last))

(define-class <set> (<coll>))
(define-generic difference)
(define-generic index)
(define-generic intersection)
(define-generic join)
(define-generic map-invert)
(define-generic project)
(define-generic rename)
(define-generic rename-keys)
(define-generic select)
(define-method (set? x) #f)
(define-method (set? (s <set>)) #t)
(define-generic subset?)
(define-generic superset?)
(define-generic union)

(define-method (equal? (x <set>) (y <set>))
  ;; Fallback for heterogeneous comparisons.  Assume for now they're
  ;; both counted so this will be fast.
  (and (= (count x) (count y))
       (every? (lambda (elt) (contains? y elt))
               (seq x))))

(define-method (invoke (s <set>) item)
  (get s item))
