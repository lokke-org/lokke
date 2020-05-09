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
  #:use-module ((lokke scm vector) #:select (lokke-vector lokke-vector-conj))
  #:use-module ((lokke pr) #:select (str))
  #:use-module ((lokke regex)
                #:select (matcher-end matcher-start re-find re-matcher))
  #:use-module ((srfi srfi-1) #:select (every proper-list?))
  #:export (blank?
            capitalize
            ends-with?
            includes?
            join
            lower-case
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

(define (join separator coll)
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

(define* (split s re #:optional limit)
  (let ((m (re-matcher re s)))
    (let loop ((result (lokke-vector))
               (prev-end 0)
               (limit limit))
      (if (and limit (zero? limit))
          result
          (let ((next (re-find m)))
            (cond
             (next
              (let* ((part (substring/read-only s prev-end (matcher-start m))))
                (loop (lokke-vector-conj result part)
                      (matcher-end m)
                      (and limit (1- limit)))))
             ((< prev-end (string-length s))
              (lokke-vector-conj (substring/read-only s prev-end)
                                 result))
             (else result)))))))
