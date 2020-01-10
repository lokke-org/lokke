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

;; Lokke, a Clojure dialect

(define-module (language lokke spec)
  #:use-module ((system base compile) #:select (compile-file compiled-file-name))
  #:use-module ((system base language)
                #:select (invalidate-compilation-cache!
                          make-language
                          define-language))
  #:use-module ((lokke compile) #:select (tree->tree-il))
  #:use-module ((lokke ns) #:select (default-environment))
  #:use-module ((lokke reader) #:select (read-for-compiler)))

(define debug-lang? #f)

(define (debug-read port env)
  (let ((result (read-for-compiler port env)))
    (when debug-lang?
      (write (list #:read result) (current-error-port))
      (newline (current-error-port)))
    result))

(define (debug-compile expr env opts)
  (when debug-lang? (write (list #:expr expr))) (newline)
  (let ((result (tree->tree-il expr env opts)))
    (when debug-lang?
      (write (list #:scm result) (current-error-port))
      (newline (current-error-port)))
    result))

(define-language lokke
  #:title "Lokke, a Clojure dialect"
  #:reader debug-read
  ;; #:printer pr-to  ;; This isn't for the repl; see (system repl common)
  #:printer write
  #:make-default-environment default-environment
  #:evaluator (lambda (x module) (primitive-eval x))
  #:compilers `((tree-il . ,tree->tree-il)))
