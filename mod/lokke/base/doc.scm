;;; Copyright (C) 2019 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

(define-module (lokke base doc)
  #:use-module ((ice-9 threads) #:select (with-mutex))
  #:use-module ((lokke base util)
                #:select (global-identifier?
                          macro-identifier?
                          module-name->ns-str
                          module-filename->ns-str))
  #:use-module ((srfi srfi-1) #:select (drop take))
  #:use-module ((system vm program) #:select (source:file program-source))
  #:use-module ((texinfo string-utils) #:select (make-text-wrapper))
  #:export (clear-def-doc! doc maybe-set-def-doc!))

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
      (or (cond ((procedure? value)
                 (procedure-documentation value))
                ((macro? value)
                 (and=> (macro-transformer value) procedure-documentation))
                (else #f))
          #nil)))

(define show-doc
  (let ((wrap (make-text-wrapper #:initial-indent "  " #:subsequent-indent "  ")))
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
       (cond ((global-identifier? #'what)
              #'(show-doc (string-append
                           (module-name->ns-str (module-name (current-module)))
                           "/"
                           (symbol->string 'what))
                          (find-doc 'what what #t)))
             ((macro-identifier? #'what)
              #'(let ((what* (module-ref (current-module) 'what)))
                  (show-doc (string-append (module-filename->ns-str
                                            (source:file
                                             (program-source
                                              (macro-transformer what*) 0)))
                                           "/" (symbol->string 'what))
                            (find-doc 'what what* #f))))
             (else #'(show-doc (string-append "/" (symbol->string 'what))
                               (find-doc 'what what #f))))))))
