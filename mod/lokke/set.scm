;;; Copyright (C) 2019-2022 Rob Browning <rlb@defaultvalue.org>
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
  #:use-module ((lokke base map) #:select (<map>))
  #:use-module ((lokke base util) #:select (require-nil))
  #:use-module ((lokke compare) #:select (clj=))
  #:use-module (oop goops)
  #:export (<set>
            difference
            index
            intersection
            join
            project
            rename
            rename-keys
            select
            set?
            subset?
            superset?
            union)
  #:re-export (clj= invoke)
  #:re-export-and-replace (apply)
  #:duplicates (merge-generics replace warn-override-core warn last))

(define-class <set> (<coll>))
(define-generic difference)
(define-generic index)
(define-generic intersection)
(define-generic join)
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

(define-method (union (s <set>)) s)

(define-method (union (s1 <set>) (s2 <set>) . more)
  (apply union (union s1 s2) more))

(define-method (intersection (s <set>)) s)

(define-method (intersection (s1 <set>) (s2 <set>) . more)
  (apply intersection (intersection s1 s2) more))

(define-method (difference (s <set>)) s)

(define-method (difference (s1 <set>) (s2 <set>) . more)
  (apply difference (difference s1 s2) more))

;; nil

(define-method (union (b <boolean>)) (require-nil 'union b) #nil)
(define-method (union (b <boolean>) (s <set>)) (require-nil 'union b) s)
(define-method (union (s <set>) (b <boolean>)) (require-nil 'union b) s)
(define-method (union (b1 <boolean>) (b2 <boolean>))
  (require-nil 'union b1)
  (require-nil 'union b2)
  #nil)


(define-method (intersection (b <boolean>) (s <set>))
  (require-nil 'intersection b)
  #nil)

(define-method (intersection (s <set>) (b <boolean>))
  (require-nil 'intersection b)
  #nil)

(define-method (intersection (b1 <boolean>) (b2 <boolean>))
  (require-nil 'intersection b1)
  (require-nil 'intersection b2)
  #nil)


(define-method (difference (b <boolean>) (s <set>))
  (require-nil 'difference b)
  #nil)

(define-method (difference (s <set>) (b <boolean>))
  (require-nil 'difference b)
  s)

(define-method (difference (b1 <boolean>) (b2 <boolean>))
  (require-nil 'difference b1)
  (require-nil 'difference b2)
  #nil)

(define-method (rename-keys (m <boolean>) (new-names <map>))
  (require-nil 'rename-keys m)
  #nil)

(define-method (rename-keys (m <map>) (new-names <boolean>))
  (require-nil 'rename-keys new-names)
  #nil)
(define-method (rename-keys (m <boolean>) (new-names <boolean>))
  (require-nil 'rename-keys m)
  (require-nil 'rename-keys new-names)
  #nil)
