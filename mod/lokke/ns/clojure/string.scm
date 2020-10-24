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

(define-module (lokke ns clojure string)
  #:use-module ((ice-9 i18n)
                #:select (string-locale-downcase string-locale-upcase))
  #:use-module ((lokke base syntax) #:select (if-let))
  #:use-module ((lokke collection) #:select (first next seq))
  #:use-module ((lokke scm vector)
                #:select (lokke-vector
                          lokke-vector-conj
                          lokke-vector-length
                          lokke-vector-pop
                          lokke-vector-ref))
  #:use-module ((lokke pr) #:select (str))
  #:use-module ((lokke regex)
                #:select (matcher-end
                          matcher-start
                          re-find
                          re-matcher
                          re-pattern))
  #:use-module (oop goops)
  #:use-module ((srfi srfi-1) #:select (every proper-list?))
  #:export (blank?
            capitalize
            ends-with?
            includes?
            join
            lower-case
            split
            split-lines
            starts-with?
            trim
            trim-newline
            triml
            trimr
            upper-case)
  #:duplicates (merge-generics replace warn-override-core warn last))


(define (read-only s) (substring/read-only s 0))
(define crnl (char-set #\return #\newline))

(define (blank? s) (string-every char-set:whitespace s))
(define (capitalize s) (read-only (string-capitalize s)))
(define (ends-with? s substr) (string-suffix? substr s))
(define (includes? s substr) (string-contains s substr))
(define (lower-case s) (read-only (string-locale-downcase s)))
(define (starts-with? s substr) (string-prefix? substr s))
(define (trim s) (read-only (string-trim-both s)))
(define (trim-newline s) (read-only (string-trim-right s crnl)))
(define (triml s) (read-only (string-trim s)))
(define (trimr s) (read-only (string-trim-right s)))
(define (upper-case s) (read-only (string-locale-upcase s)))

(define-method (join separator coll)
  ;; Does this short-circuit help?
  (let ((ls (if (and (proper-list? coll) (every string? coll))
                coll
                ;; Bit more efficient than seq->scm-list and map str.
                (let loop ((s coll)
                           (result '()))
                  (if-let (s (seq s))
                          (loop (next s) (cons (str (first s)) result))
                          (reverse! result))))))
    (string-join ls (str separator))))

(define-method (join coll) (join "" coll))

(define* (split s re #:optional (limit 0))
  ;; This somewhat matches the JVM version, i.e. a zero-width match at
  ;; the beginning of the string does not produce an empty initial
  ;; string in the result (changed in JVM 8). e.g. (split "x" #""), a
  ;; limit of 0 drops trailing empty strings, and a negative limit is
  ;; treated as infinite.
  (let* ((substr substring/read-only)
         (trim-end? (zero? limit))
         (limit (and (positive? limit) (1- limit)))
         (m (re-matcher re s))
         (v (let loop ((result (lokke-vector))
                       (prev-end 0)
                       (find 0))
              ;; Negative limits ignored as per the JVM
              (if (and limit (= (lokke-vector-length result) limit))
                  (lokke-vector-conj result (substr s prev-end))
                  (if (re-find m)
                      (let* ((start (matcher-start m))
                             (end (matcher-end m)))
                        ;; see zero-width comment above
                        (loop (if (and (= find 0) (= 0 start end))
                                  result
                                  (lokke-vector-conj result
                                                     (substr s prev-end start)))
                              end (1+ find)))
                      (lokke-vector-conj result (substr s prev-end))))))
         (trim-end (lambda (v)
                     (let loop ((v v))
                       (let ((n (lokke-vector-length v)))
                         (cond
                          ((zero? n) v)
                          ((zero? (string-length (lokke-vector-ref v (1- n))))
                           (loop (lokke-vector-pop v)))
                          (else v)))))))
    (if trim-end?
        (trim-end v)
        v)))

(define split-lines
  (let ((rx (re-pattern "(?:\n|\r\n)")))
    (lambda [s]
      (split s rx))))
