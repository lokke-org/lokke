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

(define-module (lokke base doc)
  use-module: ((ice-9 threads) select: (with-mutex))
  use-module: ((lokke base util) select: (global-identifier?))
  use-module: ((srfi srfi-1) select: (drop take))
  use-module: ((texinfo string-utils) select: (make-text-wrapper))
  export: (clear-def-doc! doc maybe-set-def-doc!))

;; Currently def (and by extension defn) documentation strings are
;; associated with the guile variable itself for top-level definitions
;; (i.e. the top-level "box" holding the defined value) via a
;; hash-table, are also attached to any procedures via as guile's
;; procedure-documentation, and are ignored in any other situations.
;; The /lokke/doc* bindings are established with the namespace.

;; For now we just use a hash-table so that we don't have to break
;; hash-map and atom up into (lokke base ...) components that don't
;; depend on (lokke base syntax).

(define (call-with-locked-doc-table f)
  (let* ((mod (current-module))
         (lock (module-variable mod '/lokke/doc-lock)))
    (if lock
        (let* ((doc-map (module-ref mod '/lokke/doc)))
          (with-mutex (variable-ref lock)
            (f doc-map)))
        #nil)))

(define (maybe-set-def-doc! var value top-level? doc)
  (when (procedure? value)
    (set-procedure-property! value 'documentation doc))
  (if top-level?
      (call-with-locked-doc-table
       (lambda (t) (hashq-set! t var doc) var))
      #nil))

(define (clear-def-doc! var top-level?)
  (if top-level?
      (call-with-locked-doc-table
       (lambda (t) (hashq-remove! t var) var))
      #nil))

(define (find-doc name value top-level?)
  "Returns the first of the following found: documentation associated
with the variable referred to by name if this is a top-level
reference, documentation assocated with the value, or
procedure-documentation if the value is a procedure."
  (if top-level?
      (let* ((mod (current-module))
             (var (module-variable mod name)))
        (call-with-locked-doc-table
         (lambda (t)
           (let ((doc (or (hashq-ref t var) (hashq-ref t value))))
             (if doc
                 doc
                 (find-doc name value #f))))))
      (if (procedure? value)
          (or (procedure-documentation value)
              #nil)
          #nil)))

;; FIXME: duplicated with pr
(define (module-name->ns-str m)
  (string-join (map symbol->string
                    (if (and (> (length m) 2)
                             (equal? '(lokke ns) (take m 2)))
                        (drop m 2)
                        (cons 'guile m)))
               "."))

(define show-doc
  (let ((wrap (make-text-wrapper initial-indent: "  " subsequent-indent: "  ")))
    (lambda (name doc)
      (display "-------------------------\n")
      (when name (display name) (newline))
      (when doc (display (string-join (wrap doc) "\n" 'suffix)))
      #nil)))

(define-syntax doc
  (lambda (x)
    (syntax-case x ()
      ((_ what)
       ;; For now, assume it's acceptable to always evaluate what
       (if (global-identifier? #'what)
           #'(show-doc (string-append
                        (module-name->ns-str (module-name (current-module)))
                        "/"
                        (symbol->string 'what))
                       (find-doc 'what what #t))
           #'(show-doc (string-append "/" (symbol->string 'what))
                       (find-doc 'what what #f)))))))
