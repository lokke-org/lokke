;;; Copyright (C) 2019-2020 Rob Browning <rlb@defaultvalue.org>
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

(define-module (lokke regex)
  #:version (0 0 0)
  #:use-module ((ice-9 match) #:select (match-lambda*))
  #:use-module (oop goops)
  #:use-module ((lokke collection) #:select (lazy-seq))
  #:use-module ((lokke exception) #:select (ex-info throw))
  #:use-module ((lokke hash-map) #:select (hash-map))
  #:use-module ((lokke pcre2)
                #:select (PCRE2_ERROR_NOMATCH
                          PCRE2_ERROR_PARTIAL
                          PCRE2_JIT_COMPLETE
                          PCRE2_UNSET
                          pcre2-compile-utf
                          pcre2-get-error-message
                          pcre2-jit-compile
                          pcre2-match-data-for-code-32
                          pcre2-match-data-for-code-8
                          pcre2-match-utf
                          pcre2-match-ovector))
  #:use-module ((lokke pr) #:select (pr-on pr-str print-on))
  #:use-module ((lokke vector) #:select (conj vector))
  #:export (matcher-end
            matcher-start
            re-find
            re-groups
            re-matcher
            re-pattern
            re-seq)
  #:re-export (pr-on print-on)
  #:duplicates (merge-generics replace warn-override-core warn last))

;; This does not try to be thread safe, and relies on unguarded
;; mutation for now, matching the jvm.

;; FIXME: check/improve error checking...

(define-inlinable (string-width x)
  (case (string-bytes-per-char x)
    ((1) 8)
    ((4) 32)
    (else => (lambda (x) (error "unexpected bytes per char:" x)))))

(define-class <re-pattern> ()
  (source #:init-keyword #:source)
  (code-8 #:init-keyword #:code-8 #:init-value #nil)
  (code-32 #:init-keyword #:code-32 #:init-value #nil))

(define-class <re-matcher> ()
  (pattern #:init-keyword #:pattern)
  (string #:init-keyword #:string)
  (start #:init-keyword #:start #:init-value 0)
  (end #:init-keyword #:end #:init-value 0)
  (data #:init-keyword #:data)
  (match? #:init-keyword #:match? #:init-value #f))

(define (matcher-end m)
  (unless (slot-ref m 'match?)
    (error "no match"))
  (slot-ref m 'end))

(define (matcher-start m)
  (unless (slot-ref m 'match?)
    (error "no match"))
  (slot-ref m 'start))

(define (require-code! re width)
  (let* ((slot (case width
                 ((8) 'code-8)
                 ((32) 'code-32)
                 (else (error "invalid width"))))
         (code (slot-ref re slot)))
    (if code
        code
        (let ((c (pcre2-compile-utf (slot-ref re 'source) 0 width)))
          (when (pair? c)
            (throw (ex-info (pcre2-get-error-message (car c))
                            (hash-map #:kind #:lokke.regex/compile-error
                                      #:stage #:compile
                                      #:code (car c)
                                      #:offset (cdr c)))))
          (let ((c (pcre2-jit-compile c PCRE2_JIT_COMPLETE)))
            (unless (zero? c)
              (throw (ex-info (pcre2-get-error-message (car c))
                              (hash-map #:kind #:lokke.regex/compile-error
                                        #:stage #:jit
                                        #:code (car c))))))
          (slot-set! re slot c)
          c))))

(define (re-pattern s)
  (make <re-pattern> #:source s))

(define-method (pr-on (re <re-pattern>) port)
  (display (string-append "#" (pr-str (slot-ref re 'source))) port))

(define-method (print-on (re <re-pattern>) port)
  (display (string-append "#" (pr-str (slot-ref re 'source))) port))

(define (re-matcher re s)
  (let* ((width (string-width s))
         (code (require-code! re width)))
    (make <re-matcher>
      #:pattern re
      #:string s
      ;; The data content is mutable and shared across all matches.
      #:data (if (= width 8)
                 (pcre2-match-data-for-code-8 code)
                 (pcre2-match-data-for-code-32 code)))))

(define (groups-for-ovector string ovector)
  ;; FIXME: add ovector accessors and avoid creating intermediate vec?
  (let ((len (array-length ovector))
        (ovec-str (lambda (offset)
                    (let ((start (array-ref ovector offset)))
                      (if (= start PCRE2_UNSET)
                          #nil
                          (substring/read-only string
                                               start
                                               (array-ref ovector
                                                          (1+ offset))))))))
    (cond
     ((= len 2) (ovec-str 0))
     ((> len 2)
      (let loop ((i 0)
                 (result (vector)))
        (if (= i len)
            result
            (loop (+ i 2) (conj result (ovec-str i))))))
     ((zero? len) #nil)  ;; Should be impossible.
     (else (error "invalid ovector length" len)))))

(define (re-groups matcher)
  (unless (slot-ref matcher 'match?)
    (throw (ex-info "No match for which to return groups"
                    (hash-map #:kind #:lokke.regex/no-match
                              #:matcher matcher))))
  (groups-for-ovector (slot-ref matcher 'string)
                      (pcre2-match-ovector (slot-ref matcher 'data))))

(define re-find
  (match-lambda*
    ((re string) (re-find (re-matcher re string)))
    ((matcher)
     (let* ((width (string-width (slot-ref matcher 'string)))
            (pattern (slot-ref matcher 'pattern))
            (code-8 (and (= width 8) (slot-ref pattern 'code-8)))
            (code-32 (and (= width 32) (slot-ref pattern 'code-32)))
            (string (slot-ref matcher 'string))
            ;; matcher constructor ensures code will be there
            (rc (pcre2-match-utf code-8 code-32
                                 #f ;; coerce
                                 string
                                 (slot-ref matcher 'end)
                                 0 ;; opts
                                 (slot-ref matcher 'data))))
       (cond
        ((= rc PCRE2_ERROR_NOMATCH) (slot-set! matcher 'match? #f) #nil)
        ((= rc PCRE2_ERROR_PARTIAL) (slot-set! matcher 'match? #f) #nil) ;; for now?
        ((positive? rc)
         (let ((ov (pcre2-match-ovector (slot-ref matcher 'data))))
           (slot-set! matcher 'start (array-ref ov 0))
           (slot-set! matcher 'end (array-ref ov 1))
           (slot-set! matcher 'match? #t)
           (groups-for-ovector string ov)))
        ((zero? rc)
         ;; This should be impossible
         (throw (ex-info "Not enough space to store match substrings"
                         (hash-map #:kind #:lokke.regex/match-error
                                   #:matcher matcher
                                   #:code rc))))
        (else
         (throw (ex-info (pcre2-get-error-message rc)
                         (hash-map #:kind #:lokke.regex/match-error
                                   #:matcher matcher
                                   #:code rc)))))))))

(define (re-seq re string)
  (let ((matcher (re-matcher re string)))
    (let loop ()
      (lazy-seq
       (let ((groups (re-find matcher)))
         (if groups
             (cons groups (loop))
             #nil))))))
