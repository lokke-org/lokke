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

(define-module (lokke compare)
  #:version (0 0 0)
  #:use-module ((lokke base util) #:select (keyword->string))
  #:use-module (oop goops)
  #:use-module ((srfi srfi-67) #:select (boolean-compare
                                         char-compare
                                         number-compare
                                         (string-compare . string-compare-67)
                                         symbol-compare
                                         vector-compare))
  #:export (== clj= compare)
  #:duplicates (merge-generics replace warn-override-core warn last))

;; FIXME: what do we want with respect to improper lists?
;; FIXME: do we want equal? for vector vs pair that are more efficient?

(define == =)

;; FIXME: match https://clojure.org/guides/equality#_summary as appropriate
(define-generic clj=)
(define-method (clj= x) #t)
(define-method (clj= x y) (equal? x y))
(define-method (clj= x y . rest)
  (and (clj= x y)
       (let loop ((x y)
                  (rest rest))
         (or (null? rest)
             (and (clj= x (car rest))
                  (loop (car rest) (cdr rest)))))))

(define-method (clj= (x <real>) (y <real>))
  (and (not (or (nan? x) (nan? y)))
       (equal? x y)))

;; compare results are compatible with SRFI-67 #{-1 0 1}, stricter
;; than Clojure's.

(define-generic compare)

(define-method (compare (x <boolean>) (y <boolean>))
  (if (eq? x #nil)
      (if (eq? y #nil) 0 -1)
      (if (eq? y #nil)
          1
          (boolean-compare x y))))

(define-method (compare (x <number>) (y <number>))
  (if (or (nan? x) (nan? y))
      0
      (number-compare x y)))

;; FIXME: namespaces, or maybe only on the clojure side
(define-method (compare (x <symbol>) (y <symbol>)) (symbol-compare x y))
(define-method (compare (x <keyword>) (y <keyword>))
  (string-compare-67 (keyword->string x) (keyword->string y)))

(define-method (compare (x <char>) (y <char>)) (char-compare x y))
(define-method (compare (x <string>) (y <string>)) (string-compare-67 x y))
(define-method (compare (x <vector>) (y <vector>)) (vector-compare compare x y))
