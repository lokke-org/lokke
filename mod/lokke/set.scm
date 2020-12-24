;;; Copyright (C) 2019-2020 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

(define-module (lokke set)
  #:version (0 0 0)
  #:use-module ((ice-9 match) #:select (match))
  #:use-module ((lokke base collection)
                #:select (<coll>
                          contains?
                          count
                          every?
                          get
                          seq))
  #:use-module ((lokke base invoke) #:select (apply invoke))
  #:use-module ((lokke compare) #:select (clj=))
  #:use-module ((lokke compat) #:select (re-export-and-replace!))
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
  #:re-export (clj= invoke)
  #:duplicates (merge-generics replace warn-override-core warn last))

(re-export-and-replace! 'apply)

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

(define-method (clj= (x <set>) (y <set>))
  ;; Fallback for heterogeneous comparisons.  Assume for now they're
  ;; both counted so this will be fast.
  (and (= (count x) (count y))
       (every? (lambda (elt) (contains? y elt))
               (seq x))))

(define-method (invoke (s <set>) item) (get s item))
(define-method (invoke (s <set>) item not-found) (get s item not-found))

(define-method (apply (s <set>) . args)
  (match args
    (((item)) (get s item))
    ((item (not-found)) (get s item not-found))))
