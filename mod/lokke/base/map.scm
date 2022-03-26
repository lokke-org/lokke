;;; Copyright (C) 2019-2020 2022 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

(define-module (lokke base map)
  #:use-module ((ice-9 match) #:select (match))
  #:use-module ((lokke base collection)
                #:select (<coll> conj count every? get reduce seq))
  #:use-module ((lokke base invoke) #:select (apply invoke))
  #:use-module ((lokke base map-entry) #:select (key val))
  #:use-module ((lokke base util) #:select (require-nil))
  #:use-module ((lokke compare) #:select (clj=))
  #:use-module ((lokke compat) #:select (re-export-and-replace!))
  #:use-module (oop goops)
  #:export (<map> map-invert map? update-keys update-vals)
  #:re-export (clj= get invoke)
  #:replace (assoc merge)
  #:duplicates (merge-generics replace warn-override-core warn last))

(re-export-and-replace! 'apply)

;; Define the <map> basics here.  They can't go in anything that
;; depends on (lokke collection) because it depends on (lokke base
;; syntax) which depends on (lokke base destructure) which depends on
;; hash-map.

(define-generic map-invert)
(define-generic update-keys)
(define-generic update-vals)

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

(define-method (conj (m1 <map>) (m2 <boolean>))
  (require-nil 'conj m2)
  m1)

(define (merge . xs)
  (if (null? xs)
      #nil
      (let loop ((xs xs)
                 (result (car xs)))
        (if (null? xs)
            result
            (let ((x (car xs)))
              (loop (cdr xs)
                    (if (eq? x #nil)
                        result
                        (reduce conj result x))))))))
