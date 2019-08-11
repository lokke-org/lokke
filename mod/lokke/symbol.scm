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

(read-set! keywords 'postfix)  ;; srfi-88

(define-module (lokke symbol)
  use-module: ((ice-9 receive) select: (receive))
  use-module: ((lokke pr) select: (str))
  use-module: ((lokke scm core) select: (->>))
  use-module: (oop goops)
  use-module: ((srfi srfi-1) select: (any))
  use-module: ((srfi srfi-88) select: (string->keyword))
  export: (ident?
           keyword
           name
           namespace
           parse-symbol
           scoped-sym?
           scoped-sym-symbol
           scoped-sym-ns
           scoped-sym-ref
           scoped-sym->validated-ns-ref
           simple-symbol?)
  replace: (gensym symbol))

;; FIXME: we may over-validate here, i.e. run too much through the
;; full parse-object gauntlet too often.

;; FIXME: namepsace support is ... incomplete, to put it generously.

(define (ident? x)
  (or (symbol? x) (keyword? x)))

(define* (gensym optional: prefix)
  ;; Generate something obscure that's a valid clj symbol
  (if prefix
      ((@ (guile) gensym) (string-append "__<?!?>__" (str prefix)))
      ((@ (guile) gensym) "__<?!?>__")))

(define simple-symbol?
  (let ((rx (make-regexp "[./]")))
    (lambda (s)
      (and (symbol? s)
           (not (regexp-exec rx (symbol->string s)))))))

;; At the moment, scoped syms are used as both a representation for
;; parsed symbols, and by the compiler during expansion, so they need
;; to be something that the macroexpander can match/traverse.  Below
;; "ref" means the reference within a namespace, i.e. the foo in
;; bar/foo.

(define (make-scoped-sym sym ns ref) (list '/lokke/scoped-sym `',sym `',ns `',ref))
(define (scoped-sym? s) (and (pair? s) (eq? '/lokke/scoped-sym (car s))))
(define (scoped-sym-symbol s) (cadr (list-ref s 1)))
(define (scoped-sym-ns s) (cadr (list-ref s 2)))
(define (scoped-sym-ref s) (cadr (list-ref s 3)))

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

(define maybe-valid-symbol-name?
  (let* ((others (string-append sym-component-first-chars "0-9./"))
         (rx (make-regexp (format #f "^[~a][~a]*$"
                                  sym-component-first-chars
                                  others)
                          regexp/icase)))
    ;; Apparently the symbol can't start or end with a slash unless it
    ;; *is* slash.
    (lambda (s)
      (and (regexp-exec rx s)
           (let ((len (string-length s)))
             (or (= 1 len)
                 (not (char=? #\/ (string-ref s (1- len))))))))))

(define (parse-symbol sym)
  ;; Ensures valid clojure symbol syntax and then returns a
  ;; scoped-sym.
  ;; FIXME: unicode?
  (let ((s (symbol->string sym)))
    (if (not (string-index s #\/))
        (make-scoped-sym sym #f sym)
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
                (make-scoped-sym sym
                                 (map string->symbol ns)
                                 (and ref (string->symbol ref)))
                (make-scoped-sym sym (list sym) #f)))))))

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
(define* (symbol ns-or-name optional: (name #nil))
  (if (not name)
      (let ((sym (cond
                  ((string? ns-or-name) (string->symbol ns-or-name))
                  ((symbol? ns-or-name) ns-or-name)
                  ((keyword? ns-or-name) (keyword->symbol ns-or-name))
                  (else (error "Unexpected symbol argument" ns-or-name)))))
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
                (when (scoped-sym-ns parsed)
                  (error (format #f
                                 "Can't construct symbol with ns ~s; name already has one: ~s" ns name)))
                (validate-ns-str ns)
                (string->symbol (string-append ns "/" name))))))))

;; FIXME: method?
(define (name x)
  (if (symbol? x)
      (symbol->string (scoped-sym-ref (parse-symbol x)))
      (name (symbol x))))

;; FIXME: may not be quite right yet.
;; FIXME: method?
(define (namespace x)
  (if (symbol? x)
      (let* ((parsed (parse-symbol x))
             (ns (scoped-sym-ns parsed)))
        (if (not ns)
            #nil
            (string-join (map symbol->string ns) ".")))
      (namespace (symbol x))))

(define* (keyword name-or-ns optional: (name #nil))
  (symbol->keyword (symbol name-or-ns name)))
