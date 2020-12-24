;;; Copyright (C) 2019 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

(define-module (lokke symbol)
  #:use-module ((ice-9 receive) #:select (receive))
  #:use-module ((lokke base util) #:select (string->keyword))
  #:use-module ((lokke pr) #:select (str))
  #:use-module ((lokke scm core) #:select (->>))
  #:use-module (oop goops)
  #:use-module ((srfi srfi-1) #:select (any))
  #:export (ident?
            keyword
            name
            namespace
            ns-sym->mod-name
            parse-symbol
            parsed-sym-ns
            parsed-sym-ref
            require-ns-sym
            simple-symbol?)
  #:replace (gensym symbol))

;; FIXME: we may over-validate here, i.e. run too much through the
;; full parse-object gauntlet too often.

;; FIXME: namepsace support is ... incomplete, to put it generously.

(define (ident? x)
  (or (symbol? x) (keyword? x)))

(define* (gensym #:optional prefix)
  ;; Generate something obscure that's a valid clj symbol
  (if prefix
      ((@ (guile) gensym) (string-append "__<?!?>__" (str prefix)))
      ((@ (guile) gensym) "__<?!?>__")))

(define simple-symbol?
  (let ((rx (make-regexp "[./]")))
    (lambda (s)
      (and (symbol? s)
           (not (regexp-exec rx (symbol->string s)))))))

(define (make-parsed-sym ns ref) (cons ns ref))

(define (parsed-sym-ns s)
  "Returns the namespace symbol for s, if any, otherwise a false value.
Note that like (namespace sym), this will be false for any symbol that
doesn't contain a slash like, for example, clojure.string."
  (car s))

(define (parsed-sym-ref s)
  "Returns the reference component of s, if any, otherwise a false
value, 'join for example, for the symbol clojure.string/join.
Similar to (name sym), except that it returns a symbol."
  (cdr s))

(define (ns-sym->mod-name ref) ;; -> (lokke ns foo)
  "Returns the Guile module name for the given ref, e.g. (lokke ns
clojure string) for clojure.string."
  (let* ((split (string-split (symbol->string ref) #\.))
         (ns (if (= 1 (length split))
                 (list ref)
                 (map string->symbol split))))
    (if (eq? 'guile (car ns))
        (cdr ns)
        (apply list 'lokke 'ns ns))))


;; Looks like multiple interior slashes are legal in a quoted symbol,
;; regardless of what the reader docs say, and there are some extra
;; valid characters character too, but leading/trailing slashes are
;; invalid.  Extra valid chars determined via experimentation: &%

(define sym-component-first-chars "-\\*\\+!_'\\?<>=a-z&%")

(define sym-component-other-chars
  (string-append sym-component-first-chars "0-9"))

(define valid-symbol-component-name?
  (let ((rx (make-regexp (format #f "^[~a][~a]*$"
                                 sym-component-first-chars
                                 sym-component-other-chars)
                         regexp/icase)))
    (lambda (s) (regexp-exec rx s))))

(define* (parse-symbol sym)
  ;; Ensures valid clojure symbol syntax and then returns a
  ;; parsed-sym.
  ;; FIXME: more tests
  ;; FIXME: unicode?
  ;; FIXME: we allow anything if no "/"?
  (if (eq? '/ sym)
      (make-parsed-sym #f sym)
      (let ((s (symbol->string sym)))
        (if (not (string-index s #\/))
            (make-parsed-sym #f sym)
            ;; Otherwise, must be a valid clojure symbol
            (let ((ns-and-ref (string-split s #\/)))
              (when (any (lambda (x) (zero? (string-length x))) ns-and-ref)
                (error "Improper \"/\" placement in symbol:" sym))
              (when (> (length ns-and-ref) 2)
                (error "More than one \"/\" ns terminator in:" sym))
              (receive (ns ref)
                  (let ((ns (string-split (car ns-and-ref) #\.)))
                    (case (length ns-and-ref)
                      ((1) (values ns #f))
                      ((2) (values ns (cadr ns-and-ref)))
                      (else (error "Unexpected decomposition of" sym))))
                (when (any (lambda (x) (zero? (string-length x))) ns)
                  (error "Improper \".\" placement in symbol's namespace:" sym))
                (when (and ref (string-index ref #\.))
                  (error "Found \".\" character in symbol's var ref:" sym))
                (for-each (lambda (x)
                            (unless (valid-symbol-component-name? x)
                              (error (format #f "Invalid ns component ~s in symbol:" x)
                                     sym)))
                          ns)
                (unless (or (not ref) (valid-symbol-component-name? ref))
                  (if (pair? ns)
                      (error (format #f "Invalid var ref component ~s in symbol:"
                                     ref)
                             sym)
                      (error "Invalid symbol syntax:" sym)))
                (if (pair? ns)
                    (make-parsed-sym (string->symbol (string-join ns "."))
                                     (and ref (string->symbol ref)))
                    ;; FIXME: how could this ever be reached, given the
                    ;; check for "/" in the top-level "if" above?
                    (make-parsed-sym (string->symbol sym) #f))))))))

(define (require-ns-sym ref)
  ;; FIXME: doesn't need full complexity of parse-symbol
  (unless (symbol? ref)
    (error "Namespace reference is not a symbol:" ref))
  (let* ((parsed (parse-symbol ref))
         (n (parsed-sym-ns parsed))
         (r (parsed-sym-ref parsed)))
    (when (and n r)
      (error "Symbol is not just a namespace reference:" ref))
    (let ((ns (or n r)))
      (unless ns
        (error "Unable to find namespace or name in symbol:" ref))
      ns)))

(define (validate-ns-str s)
  (let* ((ns (string-split s #\.)))
    (when (any (lambda (x) (zero? (string-length x))) ns)
      (error "Improper \".\" placement in symbol's namespace:" s))
    (for-each (lambda (x)
                (unless (valid-symbol-component-name? x)
                  (error (format #f "Invalid ns component ~s in symbol:" x)
                         s)))
              ns))
  s)

;; For the most part we treat keywords as symbolish...

;; FIXME: *always* validate?
;; FIXME: method?
;; For now we just funnel everything to a <symbol> for parse-symbol
(define* (symbol ns-or-name #:optional (name #nil))
  (if (not name)
      (let ((sym (cond
                  ((string? ns-or-name) (string->symbol ns-or-name))
                  ((symbol? ns-or-name) ns-or-name)
                  ((keyword? ns-or-name) (keyword->symbol ns-or-name))
                  (else (error "Unexpected symbol argument" ns-or-name)))))
        ;; FIXME: clojure/jvm actually allows "invalid" symbols like
        ;; (symbol "@#$@#$%///"); they just can't be read.  What do we
        ;; want?
        (parse-symbol sym)
        sym)
      (let ((ns ns-or-name))
        (unless (or (string? ns) (eq? #nil ns))
          (error "namespace must be a string or nil" ns))
        (unless (string? name)
          (error "name must be a string" name))
        (let* ((sym (string->symbol name))
               (parsed (parse-symbol sym)))
          (if (eq? #nil ns)
              sym
              (begin
                (when (parsed-sym-ns parsed)
                  (error (format #f
                                 "Can't construct symbol with ns ~s; name already has one: ~s" ns name)))
                (validate-ns-str ns)
                (string->symbol (string-append ns "/" name))))))))

;; FIXME: method?
(define (name x)
  (if (symbol? x)
      (symbol->string (parsed-sym-ref (parse-symbol x)))
      (name (symbol x))))

;; FIXME: may not be quite right yet.
;; FIXME: method?
(define (namespace x)
  (if (symbol? x)
      (let* ((parsed (parse-symbol x))
             (ns (parsed-sym-ns parsed)))
        (if (not ns)
            #nil
            (symbol->string ns)))
      (namespace (symbol x))))

(define* (keyword name-or-ns #:optional (name #nil))
  (symbol->keyword (symbol name-or-ns name)))
