;;; Copyright (C) 2021 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

(define-module (lokke scm edn)
  ;; We could replace this with lookahead-char for full scheme compliance
  #:use-module ((ice-9 textual-ports) #:select (unget-char))
  #:use-module ((lokke time) #:select (tagged-data->instant))
  #:use-module ((lokke uuid) #:select (tagged-data->uuid))
  #:use-module ((rnrs io ports)
                #:select (eof-object get-char get-string-n lookahead-char))
  #:use-module ((srfi srfi-1) #:select (delete-duplicates!))
  #:use-module ((srfi srfi-9) #:select (define-record-type))
  #:use-module ((srfi srfi-28) #:select (format))
  #:use-module ((srfi srfi-43) #:select (reverse-list->vector))
  #:use-module ((srfi srfi-88) #:select (string->keyword))
  ;; Expect imports to use a #:prefix edn/ or similar
  #:export (read constructors reader read-string string-reader))

;; Aside from the exceptions specifically mentioned below and in the
;; test suite, we intend to match the edn specification:
;; https://github.com/edn-format/edn

;; Note: the public functions are a bit different from what Clojure
;; specifies both in order to be more idiomatic with respect to
;; Scheme, and to support the common "fixed?" case.  i.e. it doesn't
;; seem likely you'd often want to change the tag handlers for each
;; call to read, for example.

;; FIXME: see FIXMEs in test suite
;; FIXME: support rnrs exceptions
;; FIXME: improve eof messages (include file/line and/or partial content)
;; FIXME: forbid unscoped user tags somewhere?
;; FIXME: add strict roundtripping string and port writer functions
;; FIXME: if we add print-length, make sure it's always avoidable for data writes
;; FIXME: ensure any writers always generate valid output or crash
;; FIXME: never dynamically follow *out* in writers

(define-record-type <edn-read-constructors>
  (%read-constructors list-init list-add list-finish
                      vector-init vector-add vector-finish
                      set-init set-add set-finish
                      map-init map-add map-finish)
  constructors?
  (list-init list-init)
  (list-add list-add)
  (list-finish list-finish)
  (vector-init vector-init)
  (vector-add vector-add)
  (vector-finish vector-finish)
  (set-init set-init)
  (set-add set-add)
  (set-finish set-finish)
  (map-init map-init)
  (map-add map-add)
  (map-finish map-finish))

;; Keep clojure.edn in mind whenever changing any of these
(define* (constructors #:key
                       (list-init list)
                       (list-add cons)
                       (list-finish reverse!)
                       (vector-init list)
                       (vector-add cons)
                       (vector-finish reverse-list->vector)
                       (set-init list)
                       (set-add cons)
                       (set-finish (lambda (x) (delete-duplicates! (reverse! x))))
                       (map-init list)
                       (map-add (lambda (s k v) (assoc-set! s k v)))
                       (map-finish identity))
  (%read-constructors list-init list-add list-finish
                      vector-init vector-add vector-finish
                      set-init set-add set-finish
                      map-init map-add map-finish))

(define scheme-constructors (constructors))

(define (scheme-tagged-element-readers tag)
  (case tag
    ((inst) (lambda (tag data) (tagged-data->instant data)))
    ((uuid) (lambda (tag data) (tagged-data->uuid data)))
    (else #f)))

(define edn-digit-set (char-set #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
(define edn-blank-set (char-set-adjoin char-set:whitespace #\,))
(define edn-nosep-delimiter-set (char-set #\( #\) #\[ #\] #\{ #\}))
(define symbol-first-set (char-set #\. #\* #\+ #\! #\- #\_ #\? #\$ #\% #\& #\= #\< #\>))
(define symbol-set (char-set-adjoin symbol-first-set #\# #\:))
(define maybe-likely-symbol-letter-set
  (char-set
   #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
   #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))
(define remaining-symbol-letter-set
  (char-set-difference char-set:letter maybe-likely-symbol-letter-set))

(define-inlinable (blank? c) (char-set-contains? edn-blank-set c))
(define-inlinable (delimiter? c) (char-set-contains? edn-nosep-delimiter-set c))

(define-inlinable (must-end port message)
  (let ((c (lookahead-char port)))
    (unless (or (eof-object? c)
                (blank? c)
                (delimiter? c))
      (error (message c)))))

(define (require-char port name remaining result)
  (let ((s (get-string-n port (string-length remaining))))
    (cond
     ((eof-object? s)
      (error (string-append "Unexpected end of file while reading " name)))
     ((< (string-length s) (string-length remaining))
      (error (string-append "Unexpected end of file while reading " name)))
     ((string=? s remaining)
      (must-end port (lambda (c)
                       (format "Unrecognized edn character ~a~a..." name c)))
      result))))

(define (read-unicode-point port)
  ;; For now, we only support 4 hex digits, matching clojure/jvm
  (let ((s (get-string-n port 4)))
    (cond
     ((or (eof-object? s) (not (= 4 (string-length s))))
      (error "Unexpected end of file while reading \\uNNNN character"))
     (else
      (must-end port (lambda (c)
                       (format "Unrecognized edn character \\u~a~a..." s c)))
      (integer->char (string->number s 16))))))

(define (read-character-remainder port)
  (let ((c (get-char port)))
    (cond
     ((eof-object? c)
      (error "Unexpected end of file while reading edn character"))
     (else
      (case c
        ((#\n) (require-char port "\\newline" "ewline" #\newline))
        ((#\t) (require-char port "\\tab" "ab" #\tab))
        ((#\s) (require-char port "\\space" "pace" #\space))
        ((#\r) (require-char port "\\return" "eturn" #\return))
        ((#\u) (read-unicode-point port))
        (else c))))))

(define (read-string-remainder port pending backslashed?)
  (let ((c (get-char port)))
    (cond
     ((eof-object? c) (error "Unexpected end of file while reading edn string"))
     (backslashed?
      (case c
        ((#\n) (read-string-remainder port (cons #\newline pending) #f))
        ((#\" #\\) (read-string-remainder port (cons c pending) #f))
        ((#\t) (read-string-remainder port (cons #\tab pending) #f))
        ((#\r) (read-string-remainder port (cons #\return pending) #f))
        (else
         (error "Invalid edn string escape:" (string #\\ c)))))
     (else
      (case c
        ((#\") (reverse-list->string pending))
        ((#\\) (read-string-remainder port pending #t))
        (else (read-string-remainder port (cons c pending) #f)))))))

(define (read-symbolic-str-remainder port pending name slash?)
  ;; By this point, we know it can't be a number, and has at least one char
  (define (validate-symbol-name s slash?)
    (unless (or (not slash?)
                (string=? "/" s)
                (and (> (string-length s) 2)
                     (not (string-prefix? "/" s))
                     (not (string-suffix? "/" s))))
      (error (string-append "Invalid " name " name:") s))
    s)
  (let ((c (get-char port)))
    (cond
     ((or (eof-object? c) (blank? c))
      (validate-symbol-name (reverse-list->string pending) slash?))
     ((delimiter? c)
      (unget-char port c)
      (validate-symbol-name (reverse-list->string pending) slash?))
     ((char-set-contains? char-set:letter+digit c)
      (read-symbolic-str-remainder port (cons c pending) name slash?))
     (else
      (case c
        ((#\/)
         (let ((pending (cons c pending)))
           (when slash?
             (error (string-append "Encountered multiple / characters in edn "
                                   name ":")
                    (reverse-list->string pending)))
           (read-symbolic-str-remainder port pending name #t)))
        ;; Assumes that : and # were already forbidden as the first character
        ((#\. #\* #\+ #\! #\- #\_ #\? #\$ #\% #\& #\= #\< #\> #\# #\:)
         (read-symbolic-str-remainder port (cons c pending) name slash?))
        (else
         (error (string-append "Invalid " name " prefix:")
                (reverse-list->string (cons c pending)))))))))

(define-inlinable (read-symbolic-remainder port pending name slash?)
  (let ((x (read-symbolic-str-remainder port pending name slash?)))
    (if (string? x)
        (string->symbol x)
        x)))

(define (read-symbol-or-literal-remainder port pending expect literal)
  ;; Try to match the given literal, and at the first mismatched char,
  ;; defer to the normal symbol reader.

  (let loop ((pending pending)
             (i (1- (vector-length expect))))
    (let ((c (get-char port)))
      (cond
       ((or (eof-object? c) (blank? c))
        (let ((s (reverse-list->string pending)))
          (if (= -1 i)
              literal
              ;; Must be a prefix of one of the literals, which is a valid symbol
              (string->symbol (reverse-list->string pending)))))
       ((delimiter? c)
        (unget-char port c)
        (if (= -1 i)
            literal
            ;; Must be a prefix of one of the literals, which is a valid symbol
            (string->symbol (reverse-list->string pending))))
       ((>= i 0)
        (if (eqv? c (vector-ref expect i))
            (loop (cons c pending) (1- i))
            (begin
              (unget-char port c)
              (read-symbolic-remainder port pending "symbol" #f))))
       (else
        (unget-char port c)
        (read-symbolic-remainder port pending "symbol" #f))))))

(define (parse-float s)
  (let ((n (string->number s)))
    (unless n
      (error "Invalid edn floating point syntax" s))
    n))

(define (read-exp-magnitude port pending valid?)
  (let ((c (get-char port)))
    (cond
     ((eof-object? c)
      (unless valid?
        (error "Invalid edn floating point syntax" (reverse-list->string pending)))
      (parse-float (reverse-list->string pending)))
     ((char-set-contains? edn-digit-set c)
      (read-exp-magnitude port (cons c pending) #t))
     ((or (blank? c) (delimiter? c))
      (unless valid?
        (error "Invalid edn floating point syntax" (reverse-list->string pending)))
      (parse-float (reverse-list->string pending)))
     ((eqv? c #\M)
      (unless valid?
        (error (string-append "Invalid edn floating point syntax")
               (reverse-list->string pending) "M"))
      (must-end port
                (lambda (c)
                  (format "Invalid edn floating point syntax ~aM~a..."
                          (reverse-list->string pending) c)))
      (parse-float (string-append "#e" (reverse-list->string pending))))
     (else
      (error "Invalid edn floating point syntax"
             (reverse-list->string (cons c pending)))))))

(define (read-exp-remainder port pending)
  (let ((c (get-char port)))
    (cond
     ((eof-object? c)
      (error "Invalid edn floating point syntax" (reverse-list->string pending)))
     (else
      (case c
        ((#\- #\+) (read-exp-magnitude port (cons c pending) #f))
        (else
         (unget-char port c)
         (read-exp-magnitude port pending #f)))))))

(define (read-float-remainder port pending dot?)
  ;; Possibilities upon first entry:
  ;;   dot? \.[0-9]
  ;;   dot? [0-9]+\.
  ;;
  ;; Note that the edn spec's "grammar" doesn't appear to allow M
  ;; after anything but [0-9]+, but the prose says it can follow any
  ;; float, so support that.
  (let ((c (get-char port)))
    (cond
     ((or (eof-object? c) (blank? c) (delimiter? c))
      (parse-float (reverse-list->string pending)))
     ((char-set-contains? edn-digit-set c)
      (read-float-remainder port (cons c pending) dot?))
     (else
      (case c
        ((#\.)
         (let ((pending (cons c pending)))
           (when dot?
             (error "Invalid edn floating point syntax"
                    (reverse-list->string pending)))
           (read-float-remainder port pending #t)))
        ((#\e #\E)
         (read-exp-remainder port (cons c pending)))
        ((#\M)
         (must-end port
                   (lambda (c)
                     (format "Invalid edn floating point syntax ~aM~a..."
                             (reverse-list->string pending) c)))
         (parse-float (string-append "#e" (reverse-list->string pending))))
        (else
         (error "Invalid edn floating point syntax"
                (reverse-list->string (cons c pending)))))))))

(define (read-numeric-remainder port pending n?)
  ;; pending already has at least one [0-9]
  (let ((c (get-char port)))
    (cond
     ((or (eof-object? c) (blank? c)) ;; could only be integer
      (let ((s (reverse-list->string pending)))
        ;; Treat leading zero as octal for now, matching the JVM, even
        ;; though the edn spec forbids it.
        (case (string-ref s 0)
          ((#\- #\+) (case (string-ref s 1)
                       ((#\0) (string->number (reverse-list->string pending) 8))
                       (else (string->number (reverse-list->string pending)))))
          ((#\0) (string->number (reverse-list->string pending) 8))
          (else (string->number (reverse-list->string pending))))))
     ((delimiter? c) ;; could only be integer
      (unget-char port c)
      (string->number (reverse-list->string pending)))
     ((char-set-contains? edn-digit-set c)
      (read-numeric-remainder port (cons c pending) n?))
     ((eqv? c #\.)
      (let ((pending (cons c pending)))
        (when n?
          (error "Invalid edn number prefix:" (reverse-list->string pending)))
        (read-float-remainder port pending #t)))
     ;; char set?
     ((memv c '(#\e #\E))
      (let ((pending (cons c pending)))
        (when n?
          (error "Invalid edn number prefix:" (reverse-list->string pending)))
        (read-exp-remainder port pending)))
     ((eqv? c #\N)
      (when n?
        (error "Invalid edn number prefix:"
               (reverse-list->string (cons c pending))))
      (read-numeric-remainder port pending #t))
     ((eqv? c #\M)
      (must-end port
                (lambda (c)
                  (format "Invalid edn nummeric syntax ~aM~a..."
                          (reverse-list->string pending) c)))
      (string->number (reverse-list->string pending)))
     ((char-set-contains? char-set:digit c)
      (error "Invalid edn numeric digit" c))
     (else
      (error "Invalid edn numeric prefix:" (reverse-list->string pending))))))

(define (read-symbol-or-number port pending frac?)
  ;; The JVM implementation treats anything starting with a "digit"
  ;; (Character/isDigit) as a number, but then only allows [0-9].
  ;; However, anything that starts with [-.+] is taken as a symbol
  ;; unless the second char is [0-9].  Match that behavior for now.
  (let ((c (get-char port)))
    (cond
     ((or (eof-object? c) (blank? c))  ;; must be + - .
      (string->symbol (reverse-list->string pending)))
     ((delimiter? c)  ;; must be + - .
      (unget-char port c)
      (string->symbol (reverse-list->string pending)))
     ((char-set-contains? edn-digit-set c)
      (let ((pending (cons c pending)))
        (if frac?
            (read-float-remainder port pending #t)
            (read-numeric-remainder port pending #f))))
     ((char-set-contains? char-set:letter c)
      (read-symbolic-remainder port (cons c pending) "symbol" #f))
     ((char-set-contains? symbol-set c)
      (read-symbolic-remainder port (cons c pending) "symbol" #f))
     ((eqv? c #\/)
      (read-symbolic-remainder port (cons c pending) "symbol" #t))
     ((char-set-contains? char-set:digit c) ; Should only match non [0-9] digits
      (read-symbolic-remainder port (cons c pending) "symbol" #f))
     (else
      (error "Invalid edn element prefix:" (reverse-list->string pending))))))

(define (read-keyword-remainder port)
  (let ((c (get-char port)))
    (cond
     ((eof-object? c)
      (error "Unexpected end of file while reading edn keyword"))
     ((blank? c)
      (error "Unexpected blank space after start of edn keyword"))
     ((delimiter? c)
      (error (format "Unexpected delimiter ~s after start of edn keyword"
                     (string c))))
     ((eqv? c #\:) (error "Read :: at start of edn keyword"))
     (else
      (let ((s (read-symbolic-str-remainder port (list c) "keyword"
                                            (eqv? c #\/))))
        (if (equal? s "/")
            (error "Invalid edn keyword :/")
            (string->keyword s)))))))

(define (read-container-content port name terminator pending
                                tag-rdr default-tag-rdr constructors
                                add finish)
  (let ((c (get-char port)))
    (cond
     ((eof-object? c)
      (error (string-append "Unexpected end of file while reading " name)))
     ((eqv? c terminator) (finish pending))
     (else
      (unget-char port c)
      (let ((x (read* port tag-rdr default-tag-rdr constructors)))
        (when (eof-object? x)
          (error (string-append "Unexpected end of file while reading edn "
                                name)))
        (read-container-content port name terminator
                                (add x pending)
                                tag-rdr default-tag-rdr constructors
                                add finish))))))

(define (read-map-content port pending tag-rdr default-tag-rdr constructors)
  (let ((c (get-char port)))
    (cond
     ((eof-object? c)
      (error (string-append "Unexpected end of file while map")))
     ((eqv? c #\}) ((map-finish constructors) pending))
     (else
      (unget-char port c)
      (let* ((k (read* port tag-rdr default-tag-rdr constructors))
             (_ (when (eof-object? k)
                  (error "Unexpected end of file while reading edn map")))
             (v (read* port tag-rdr default-tag-rdr constructors))
             (_ (when (eof-object? v)
                  (error "Unexpected end of file after edn map key:" k))))
        (read-map-content port ((map-add constructors) pending k v)
                          tag-rdr default-tag-rdr constructors))))))

(define (read-tagged-element port tag-rdr default-tag-rdr constructors)
  (let ((tag (read* port tag-rdr default-tag-rdr constructors)))
    (cond
     ((eof-object? tag)
      (error "Unexpected end of file in edn tagged element"))
     ((not (symbol? tag))
      (error "Found non-symbol at start of edn tagged element:" tag))
     (else
      (let ((data (read* port tag-rdr default-tag-rdr constructors)))
        (when (eof-object? data)
          (error "End of file in edn tagged element data" tag))
        (let ((read-elt (or (and tag-rdr (tag-rdr tag))
                            default-tag-rdr)))
          (unless read-elt
            (error "Unrecognized edn tagged element type:" tag))
          (read-elt tag data)))))))

(define (read-#-remainder port tag-rdr default-tag-rdr constructors)
  (let ((c (get-char port)))
    (cond
     ((eof-object? c) (error "Unexpected end of file while reading # form"))
     (else
      (case c
        ((#\{)
         (read-container-content port "set" #\}
                                 ((set-init constructors))
                                 tag-rdr default-tag-rdr constructors
                                 (set-add constructors)
                                 (set-finish constructors)))
        ;; Match the (1.9+) JVM behavior, even though these are not
        ;; (yet?) in the spec.
        ((#\#)
         (let ((x (read* port tag-rdr default-tag-rdr constructors)))
           (unless (symbol? x)
             (error "Value read after ## was not a symbol:" x))
           (case x
             ((Inf) +inf.0)
             ((-Inf) -inf.0)
             ((NaN) +nan.0)
             (else
              (error "Unrecognized ## literal:"
                     (string-append "##" (symbol->string x)))))))
        ((#\_)
         (read* port tag-rdr default-tag-rdr constructors)
         (read* port tag-rdr default-tag-rdr constructors))
        (else
         (unget-char port c)
         (read-tagged-element port tag-rdr default-tag-rdr constructors)))))))

(define* (read* port tag-rdr default-tag-rdr constructors)
  (let ((c (get-char port)))
    (cond
     ((eof-object? c) c)
     ((blank? c) (read* port tag-rdr default-tag-rdr constructors))
     (else
      (case c
        ((#\() (read-container-content port "list" #\)
                                       ((list-init constructors))
                                       tag-rdr default-tag-rdr constructors
                                       (list-add constructors)
                                       (list-finish constructors)))
        ((#\[) (read-container-content port "vector" #\]
                                       ((vector-init constructors))
                                       tag-rdr default-tag-rdr constructors
                                       (vector-add constructors)
                                       (vector-finish constructors)))
        ((#\{) (read-map-content port ((map-init constructors))
                                 tag-rdr default-tag-rdr constructors))
        ((#\") (read-string-remainder port '() #f))
        ((#\:) (read-keyword-remainder port))
        ((#\n) (read-symbol-or-literal-remainder port (list c) #(#\l #\i) #nil))
        ((#\t) (read-symbol-or-literal-remainder port (list c) #(#\e #\u #\r) #t))
        ((#\f) (read-symbol-or-literal-remainder port (list c) #(#\e #\s #\l #\a) #f))
        ((#\-) (read-symbol-or-number port (list c) #f))
        ((#\\) (read-character-remainder port))
        ((#\.) (read-symbol-or-number port (list c) #t))
        ((#\+) (read-symbol-or-number port (list c) #f))
        ((#\#) (read-#-remainder port tag-rdr default-tag-rdr constructors))
        (else
         (cond
          ((char-set-contains? edn-digit-set c)
           (read-numeric-remainder port (list c) #f))
          ((eqv? c #\/) (read-symbolic-remainder port (list c) "symbol" #t))
          ((char-set-contains? symbol-first-set c)
           (read-symbolic-remainder port (list c) "symbol" #f))
          ((char-set-contains? maybe-likely-symbol-letter-set c)
           (read-symbolic-remainder port (list c) "symbol" #f))
          ((char-set-contains? remaining-symbol-letter-set c)
           (read-symbolic-remainder port (list c) "symbol" #f))
          ((char-set-contains? char-set:digit c)
           ;; Match the JVM reader for now
           (error "Number begins with digit other than 0-9" c))
          (else
           (error "Unrecognized edn construct:" c)))))))))

(define* (read-top port on-eof tag-rdr default-tag-rdr constructors)
  (let ((c (lookahead-char port)))
    (if (eof-object? c)
        (on-eof)
        (read* port tag-rdr default-tag-rdr constructors))))

(define* (reader #:key
                 (on-eof eof-object)
                 (tag-reader scheme-tagged-element-readers)
                 (default-tag-reader #f)
                 (constructors scheme-constructors)
                 (fixed? #t))
  (if fixed?
      (case-lambda
        (()
         (read-top (current-input-port) on-eof
                   tag-reader default-tag-reader
                   constructors))
        ((port)
         (read-top port on-eof
                   tag-reader default-tag-reader
                   constructors)))
      (lambda* (#:optional
                (port (current-input-port))
                #:key
                (on-eof on-eof)
                (tag-reader tag-reader)
                (default-tag-reader default-tag-reader)
                (constructors constructors))
        (read-top port on-eof tag-reader default-tag-reader constructors))))

(define (string-reader . args)
  (let ((read (apply reader args)))
    (lambda (s . opts)
      (call-with-input-string s
        (lambda (port) (apply read port opts))))))

(define read (reader))
(define read-string (string-reader))
