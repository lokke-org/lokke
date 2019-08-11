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

;; This module must not depend on (lokke collection) because because
;; it depends on (lokke base syntax) which depends on (lokke base
;; destructure) which depends on hash-map which depends on this.

(read-set! keywords 'postfix)  ;; srfi-88

(define-module (lokke map-entry)
  use-module: (oop goops)
  use-module: ((lokke base collection)
               select: (<coll> cons first nth rest second seq))
  use-module: ((lokke pr) select: (*out* pr pr-str print print-str))
  export: (<map-entry> key map-entry map-entry? val)
  re-export: (first nth second)
  duplicates: (merge-generics replace warn-override-core warn last))

(define-class <map-entry> (<coll>)
  (k init-keyword: k:)
  (v init-keyword: v:))

(define (map-entry k v)
  (make <map-entry> k: k v: v))

(define-method (first (entry <map-entry>)) (slot-ref entry 'k))
(define-method (second (entry <map-entry>)) (slot-ref entry 'v))
(define-method (key (entry <map-entry>)) (slot-ref entry 'k))
(define-method (val (entry <map-entry>)) (slot-ref entry 'v))

(define (read-only-str s) (substring/read-only s 0))

(define (render-str m render)
  (read-only-str (string-append "[" (render (key m)) " " (render (val m)) "]")))

(define (show m emit)
  (display "[" (*out*))
  (emit (key m))
  (display " " (*out*))
  (emit (val m))
  (display "]" (*out*)))

(define-method (pr-str (v <map-entry>)) (render-str v pr))
(define-method (print (v <map-entry>)) (render-str v print))
(define-method (pr (v <map-entry>)) (show v pr))
(define-method (print (v <map-entry>)) (show v print))

(define (map-entry? x) (is-a? x <map-entry>))

(define-method (count (entry <map-entry>)) 2)
(define-method (counted? (entry <map-entry>)) #t)
(define-method (not-empty (entry <map-entry>)) entry)
(define-method (contains? (entry <map-entry>) i) (or (= i 0) (= i 1)))

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

(define-method (equal? (e1 <map-entry>) (e2 <map-entry>))
  (and (equal? (slot-ref e1 'k) (slot-ref e2 'k))
       (equal? (slot-ref e1 'v) (slot-ref e2 'v))))

;; FIXME?
(define-method (seq (entry <map-entry>))
  (cons (slot-ref entry 'k)
        (cons (slot-ref entry 'v) #nil)))

(define-method (rest (entry <map-entry>))
  (cons (slot-ref entry 'v) #nil))
