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
  #:use-module ((ice-9 format) #:select (format))
  #:use-module ((ice-9 rdelim) #:select (read-delimited))
  #:use-module ((ice-9 textual-ports) #:select (get-string-n))
  #:use-module ((lokke base dynamic) #:select (binding))
  #:use-module ((lokke config) #:select (ensure-config-dir))
  #:use-module ((lokke core) #:select (*command-line-args* prn))
  #:use-module ((lokke compile) #:select (load-file))
  #:use-module ((lokke ns) #:select (default-environment resolve-ns))
  #:use-module ((lokke reader) #:select ((read . read-edn)))  ; FIXME: real edn reader
  #:use-module ((lokke symbol) #:prefix sym/)
  #:use-module ((srfi srfi-1) #:select (append-map))
  #:use-module ((srfi srfi-69)
                #:select (make-hash-table
                          hash-table->alist
                          hash-table-ref
                          hash-table-set!
                          hash-table-update!
                          hash-table-update!/default))
  #:use-module ((srfi srfi-88) #:select (keyword->string string->keyword))
  #:export (lok-main lokke-main))

(define not-even-started 254)

(define err current-error-port)
(define str string-append)

(define (run-opts)
  (string-append
   "RUN_OPT:\n"
   "  -l, --load FILE    execute code in FILE\n"
   "  -e, --eval CODE    evaluate CODE, printing any values that are\n"
   "                     not nil or unspecified\n"
   "  -a, --apply REF    apply REF to the *command-line-args*\n"
   "  FILE               execute code in FILE (name must not start with -)\n"
   "  -                  execute any code provided on standard input\n"
   "  --                 Make all subsequent arguments *command-line-args*\n"))

(define (lokke-usage)
  (string-append
   "Usage:\n"
   "  lokke (help | --help | -h | -?)\n"
   "  lokke -0\n"
   "  lokke [repl]\n"
   "  lokke run [RUN_OPT ...] [-- COMMAND_LINE_ARG ...]\n"
   (run-opts)))

(define (lok-usage)
  (string-append
   "Usage:\n"
   "  lok (--help | -h | -?)\n"
   "  lok [RUN_OPT ...] [-- COMMAND_LINE_ARG ...]\n"
   (run-opts)))

(define* (quit msg status)
  (display msg (err))
  (exit status))

(define (quit-early msg . args)
  (apply format (err) msg args)
  (exit not-even-started))

(define (misuse usage)
  (quit-early (usage)))

(define (make-options-hash)
  (let ((result (make-hash-table)))
    (hash-table-set! result 'actions '())
    (hash-table-set! result 'args '())
    result))

(define (parse-run-args args usage)
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
                  (prn result))))
            actions)))
  (define (add-apply what)
    (lambda (actions)
      actions
      (cons (lambda ()
              ;; FIXME: error checking
              (let* ((what-sym (string->symbol what))
                     (ns (string->symbol (sym/namespace what-sym)))
                     (n (string->symbol (sym/name what-sym))))
                (apply (module-ref (resolve-ns ns) n) args)))
            actions)))
  (let loop ((args args)
             (result (make-options-hash)))
    (if (null? args)
        (clean-up result)
        (let ((arg (car args)))
          (cond
           ((member arg '("-?" "-h" "--help")) (quit (usage) 0))
           ((member arg '("-e" "--eval"))
            (when (null? (cdr args))
              (quit-early "lokke: no argument for ~a\n" arg))
            (hash-table-update! result 'actions (add-evaluator (cadr args)))
            (loop (cddr args) result))
           ((member arg '("-l" "--load"))
            (when (null? (cdr args))
              (quit-early "lokke: no argument for ~a\n" arg))
            (hash-table-update! result 'actions (add-loader (cadr args)))
            (loop (cddr args) result))
           ((member arg '("-a" "--apply"))
            (when (null? (cdr args))
              (quit-early "lokke: no argument for ~a\n" arg))
            (hash-table-update! result 'actions (add-apply (cadr args)))
            (loop (cddr args) result))
           ((equal? "--" arg)
            (hash-table-set! result 'args (cdr args))
            (clean-up result))
           (else
            (format (err) "lokke: unrecognized argument: ~s\n" arg)
            (quit-early (usage))))))))

(define (lokke-run args usage)
  (let ((opts (parse-run-args args usage)))
    (let ((actions (hash-table-ref opts 'actions)))
      (if (null? actions)
          (present-repl '()) ;; Might get here with args via --strip
          (begin
            (binding (*command-line-args* (hash-table-ref opts 'args))
                     (set-current-module (default-environment))
                     (for-each (lambda (action) (action)) actions))
            0)))))

(define (configure-history)
  (setenv "GUILE_HISTORY"
          (or (getenv "LOKKE_HISTORY")
              (string-append (ensure-config-dir) "/history"))))

;; load-user-init adapted from the version in Guile 2.2.6 (LGPL 3)
;; FIXME: propose accommodations upstream
(define (load-user-init)
  (let* ((home (or (getenv "HOME")
                   (false-if-exception (passwd:dir (getpwuid (getuid))))
                   file-name-separator-string)) ;; fallback for cygwin etc.
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

;; FIXME: do we want to adopt guile's guess-encoding?
;; FIXME: add all subcommands to the preamble module (and more cleanly)

(define (cli-run . args)
  (lokke-run args lok-usage))

(define (run-script args usage)
  (when (< (length args) 3)
    (display (usage) (err))
    (quit-early "lokke: script argument ~s not followed by path\n" (cadr args)))
  (let* ((interpreter (car args))
         (script (caddr args))
         (script-args (cdddr args))
         (terminator "!#"))
    ;; FIXME: error handling, i.e. make sure there was a terminator?
    (let* ((preamble-str (call-with-input-file script
                           (lambda (port)
                             ;; Skip past the preamble indicator
                             (read-string-until port interpreter)
                             (read-string-until port " -0")
                             ;; For now, just read the preamble into a
                             ;; string and parse that...
                             (read-string-until port terminator))))
           (preamble (call-with-input-string preamble-str
                       (lambda (port)
                         (let loop ((expr (read port))
                                    (result '()))
                           (if (eof-object? expr)
                               (reverse! result)
                               (loop (read port) (cons expr result)))))))
           (preamble (if (null? preamble)
                         '(apply run "-l" %0 "--" %&)
                         (cons 'begin preamble)))
           (preamble-mod (let ((m (make-fresh-user-module)))
                           (module-define! m '%0 (caddr (program-arguments)))
                           (module-define! m '%& (cdddr (program-arguments)))
                           (do ((args (cdddr (program-arguments))
                                      (cdr args))
                                (i 1 (1+ i)))
                               ((null? args))
                             (module-define! m
                                             (string->symbol (format #f "%~d" i))
                                             (car args)))
                           (module-define! m 'run cli-run)
                           m)))
      (set-program-arguments (cddr (program-arguments)))
      (eval preamble preamble-mod))))

(define (present-repl args)
  (unless (null? args)
    (quit-early "lokke: repl invocation currently accepts no arguments\n"))
  (configure-history)
  (load-user-init)
  ((@ (lokke repl) repl)))

(define (lok-main args)
  (lokke-run (cdr args) lok-usage))

(define (lokke-main args)
  (let ((len (length args)))
    (if (< len 2)
        (present-repl '())
        (let ((cmd (cadr args)))
          (cond
           ((member cmd '("-?" "-h" "--help" "help")) (quit (lokke-usage) 0))
           ((string-prefix? "-0" cmd) (run-script args lokke-usage))
           ((string=? "repl" cmd) (present-repl (cddr args)))
           ((string=? "run" cmd) (lokke-run (cddr args) lokke-usage))
           (else
            (display (lokke-usage) (err))
            (quit-early "lokke: unrecognized subcommand ~s\n" cmd)))))))
