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

;; FIXME: not positive it wasn't an artifact of other problems, but
;; may have observed that --language=lokke causes everything, even
;; scheme modules to pass through the lokke reader.  For now we'll
;; avoid setting the lanugage.

(define-module (lokke main)
  #:use-module ((ice-9 eval-string) #:select (eval-string))
  #:use-module ((ice-9 rdelim) #:select (read-delimited))
  #:use-module ((ice-9 textual-ports) #:select (get-string-n))
  #:use-module ((lokke core) #:select (println))
  #:use-module ((lokke compile) #:select (load-file))
  #:use-module ((lokke ns) #:select (default-environment))
  #:use-module ((lokke reader) #:select ((read . read-edn)))  ; FIXME: real edn reader
  #:use-module ((srfi srfi-1) #:select (append-map))
  #:use-module ((srfi srfi-69)
                #:select (make-hash-table
                          hash-table->alist
                          hash-table-ref
                          hash-table-set!
                          hash-table-update!
                          hash-table-update!/default))
  #:export (main))

(define not-even-started 254)

(define err current-error-port)
(define str string-append)

(define usage
  (string-append
   "Usage:\n"
   "  lokke\n"
   "  lokke {--help | -h | -?}\n"
   "  lokke OPTS\n"
   "  lokke OPTS -- CLOJURE_PROGRAM_ARG...\n"
   "  lokke --#!\n"
   "OPTS:\n"
   "  -i, --init PATH    load path\n"
   "  -e, --eval CODE    evaluate code, printing any values that are\n"
   "                     not nil or unspecified\n"))

(define* (quit msg status)
  (display msg (err))
  (exit status))

(define (quit-early msg . args)
  (apply format (err) msg args)
  (exit not-even-started))

(define (misuse)
  (quit-early usage))

(define (make-options-hash)
  (let ((result (make-hash-table)))
    (hash-table-set! result 'actions '())
    (hash-table-set! result 'args '())
    result))

(define (parse-args args)
  (define (clean-up result)
    (hash-table-update! result 'actions (lambda (x) (reverse! x)))
    result)
  (define (add-loader path)
    (lambda (actions)
      (cons (lambda () (load-file path)) actions)))
  (define (add-evaluator code)
    (lambda (actions)
      (cons (lambda ()
              (let ((result (eval-string code #:lang 'lokke #:compile? #t)))
                (unless (or (nil? result) (eq? *unspecified* result))
                  (println result))))
            actions)))
  (let loop ((args (cdr args))
             (result (make-options-hash)))
    (if (null? args)
        result
        (let ((arg (car args)))
          (cond
           ((equal? "--help" arg) (quit usage 0))
           ((member arg '("-i" "--init"))
            (when (null? (cdr args))
              (quit-early "lokke: no argument for ~a\n" arg))
            (hash-table-update! result 'actions (add-loader (cadr args)))
            (loop (cddr args) result))
           ((member arg '("-e" "--eval"))
            (when (null? (cdr args))
              (quit-early "lokke: no argument for ~a\n" arg))
            (hash-table-update! result 'actions (add-evaluator (cadr args)))
            (loop (cddr args) result))
           ((member arg '("-?" "-h" "--help")) (quit usage 0))
           ((equal? "--" arg)
            (hash-table-set! result 'args (cdr args))
            result)
           (else
            (format (err) "lokke: unrecognized argument: ~s\n" arg)
            (quit-early usage)))))))

(define (process-arguments args)
  (let ((opts (parse-args args)))
    ;; FIXME: is this what we want or should *command-line-args* just
    ;; be a subset?  This is also a fluid...
    (set-program-arguments (cons (car (program-arguments))
                                 (hash-table-ref opts 'args)))
    (set-current-module (default-environment))
    (for-each (lambda (action) (action))
              (hash-table-ref opts 'actions))
    0))

(define (configure-history)
  (setenv "GUILE_HISTORY"
          (or (getenv "LOKKE_HISTORY")
              (string-append (getenv "HOME") "/.lokke_history"))))

;; load-user-init adapted from the version in Guile 2.2.6 (LGPL 3)
;; FIXME: propose accommodations upstream
(define (load-user-init)
  (let* ((home (or (getenv "HOME")
                   (false-if-exception (passwd:dir (getpwuid (getuid))))
                   file-name-separator-string))  ;; fallback for cygwin etc.
         (init-file (in-vicinity home ".lokke_guile")))
    ;; FIXME: add support for suppressing the init file (e.g. Guile's -q)
    ;; FIXME: either add support for loading a clj ~/.lokke after
    ;; this, or if feasible, drop .lokke_guile entirely in favor of
    ;; that.  We'll need a primitive_load equivalent, etc.
    (if (file-exists? init-file)
        (primitive-load init-file))))

(define (read-string-until port terminator)
  "Reads from port until reaching a match for the terminator string.
Returns everything read except the terminator as a string, and leaves
the port positioned at the first character after the
terminator."
  ;; For now, just do this the easy, more expensive way...
  (let ((last-term (string-take-right terminator 1)))
    (let loop ((result ""))
      (let* ((chunk (read-delimited last-term port 'concat))
             (result (string-append/shared result chunk)))
        (when (eof-object? result)
          (quit-early "lokke: file ~s ended without preamble terminator ~s"
                      (port-filename port) terminator))
        (if (string-suffix? terminator result)
            (substring/read-only result
                                 0
                                 (- (string-length result)
                                    (string-length terminator)))
            (loop result))))))

(define (read-preamble-forms port)
  "Reads and returns all of the --#! edn preamble forms after
converting all but the keywords to strings."
  (define (parse exp)
    (cond
     ((string? exp) exp)
     ((symbol? exp) (symbol->string exp))
     ((number? exp) (number->string exp))
     ((eq? exp #nil) "nil")
     ((eq? #t exp) "true")
     ((eq? #f exp) "false")
     ((eq? #:this exp) exp)
     ((eq? #:args exp) exp)
     (else
      (quit-early "lokke: unrecognized form in ~s preamble: ~s\n"
                  (port-filename port) exp))))
  (let loop ((exp (read-edn port))
             (result '()))
    (if (eof-object? exp)
        (reverse! result)
        (loop (read-edn port) (cons (parse exp) result)))))

(define (read-preamble port terminator)
  ;; Skip past the preamble indicator
  (read-string-until port " --#!")
  ;; For now, just read the preamble into a string and parse that...
  (let* ((preamble (read-string-until port terminator)))
    (call-with-input-string preamble
      (lambda (in) (read-preamble-forms in)))))

;; FIXME: do we want to adopt guile's guess-encoding?

(define (run-script args)
  (let* ((interpreter (car args))
         (script (caddr args))
         (script-args (cdddr args))
         (terminator "!#"))
    (let* ((src (open-input-file script))
           (preamble (read-preamble src terminator))
           (final-args (cons interpreter
                             (append-map (lambda (x)
                                           (case x
                                             ((#:args) script-args)
                                             ((#:this) (list script))
                                             (else (list x))))
                                         preamble))))
      (process-arguments final-args))))

(define (main args)
  ;; FIXME: which args does clj (or should we) allow you to use with the repl?
  (let ((len (length args)))
    (if (= len 1)
        (begin
          (configure-history)
          (load-user-init)
          ((@ (lokke repl) repl)))
        (if (string-prefix? "--#!" (cadr args))
            (begin
              (when (= len 2)
                (display usage (err))
                (quit-early "lokke: script argument ~s not followed by path\n"
                            (cadr args)))
              (run-script args))
            (process-arguments args)))))
