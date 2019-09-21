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

(read-set! keywords 'postfix)  ;; srfi-88

(define-module (lokke compare)
  version: (0 0 0)
  use-module: (oop goops)
  export: (== clj=)
  duplicates: (merge-generics replace warn-override-core warn last))

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
