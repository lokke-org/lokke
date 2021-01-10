;;; Copyright (C) 2019-2020 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

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
  #:use-module ((lokke io) #:select (slurp))
  #:use-module ((lokke ns) #:select (default-environment resolve-ns))
  #:use-module ((lokke reader) #:select ((read . read-edn)))  ; FIXME: real edn reader
  #:use-module ((lokke symbol) #:prefix sym/)
  #:use-module ((srfi srfi-1)
                #:select (append-map append-reverse last span take-while))
  #:use-module ((srfi srfi-11) #:select (let-values))
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
   "  -m, --main NS      same as -a NS/-main"
   "  --seed sys         set the random state to (random-state-from-platform)\n"
   "  --seed INTEGER     seed the random state with INTEGER\n"
   "  --no-seed          suppress the default, implicit --seed sys.  The rightmost\n"
   "                     seed related argument determines the behavior.\n"
   "  --deps FILE        acquire depenencies as specified by EDN FILE (deps.edn)\n"
   "  FILE               execute code in FILE (name must not start with -)\n"
   "  -                  execute any code provided on standard input\n"
   "  --                 Make all subsequent arguments *command-line-args*\n"))

(define (lokke-usage)
  (string-append
   "Usage:\n"
   "  lokke [help | --help | -h | -?]\n"
   "  lokke run [RUN_OPT ...] [-- COMMAND_LINE_ARG ...]\n"
   "  lokke -0\n"
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

(define arabic-digit-set
  (char-set #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(define (parse-integer s)
  (and (string-every arabic-digit-set s)
       (string->number s)))

(define (maybe-randomize-state args)
  ;; Find any the rightmost seed-related argument and do what it requests.
  (let-values (((last-opt prefix)
                (span (lambda (x)
                        (not (or (string=? "--no-seed" x)
                                 (string=? "--seed" x))))
                      (reverse (take-while (lambda (x) (not (string=? x "--")))
                                           args)))))
    (cond
     ;; no seed-related option
     ((null? prefix)
      (set! *random-state* (random-state-from-platform)))
     ((string=? "--no-seed" (car prefix)) #f)
     ((string=? "--seed" (car prefix))
      (when (null? last-opt)
        (quit-early "lokke: no --seed argument\n"))
      (let ((mode (last last-opt)))
        (if (string=? "sys" mode)
            (begin
              (set! *random-state* (random-state-from-platform))
              #:sys)
            (let ((n (parse-integer mode)))
              (unless n
                (quit-early "lokke: --seed argument is not \"sys\" or an integer\n"))
              (set! *random-state* (seed->random-state n))
              n)))))))

(define (remove-seed-args args)
  ;; Assumes the seed-related arguments have already been vetted.
  (let loop ((result '())
             (args args))
    (if (null? args)
        (reverse result)
        (cond
         ((string=? "--" (car args)) (append-reverse result args))
         ((string=? "--seed" (car args)) (loop result (cddr args)))
         ((string=? "--no-seed" (car args)) (loop result (cdr args)))
         (else (loop (cons (car args) result) (cdr args)))))))

(define (make-options-hash)
  (let ((result (make-hash-table)))
    (hash-table-set! result 'deps '())
    (hash-table-set! result 'actions '())
    (hash-table-set! result 'args '())
    result))

(define (parse-run-args args usage)
  (define (clean-up result)
    (hash-table-update! result 'deps (lambda (x) (reverse! x)))
    (hash-table-update! result 'actions (lambda (x) (reverse! x)))
    result)
  (define (add-loader path)
    ;; FIXME: friendly error messages for missing files
    (lambda (actions)
      (cons (lambda () (load-file path)) actions)))
  (define (add-evaluator code)
    (lambda (actions)
      (cons (lambda ()
              (let ((result (eval-string code #:lang 'lokke #:compile? #t)))
                (unless (or (eq? #nil result) (eq? *unspecified* result))
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
                (apply (module-ref (resolve-ns ns) n) *command-line-args*)))
            actions)))
  (let loop ((args args)
             (add-repl? #t)
             (result (make-options-hash)))
    (if (null? args)
        (begin
          (when add-repl?
            (hash-table-update! result 'actions (add-apply "clojure.main/repl")))
          (clean-up result))
        (let ((arg (car args)))
          (cond
           ((member arg '("-?" "-h" "--help")) (quit (usage) 0))
           ((equal? arg "-p")
            (loop (cons* "--deps" "deps.edn" (cdr args)) add-repl? result))
           ((equal? arg "--deps")
            (when (null? (cdr args))
              (quit-early "lokke: no argument for ~a\n" arg))
            (hash-table-update! result 'deps (lambda (x) (cons (cadr args) x)))
            (loop (cddr args) add-repl? result))
           ((member arg '("-e" "--eval"))
            (when (null? (cdr args))
              (quit-early "lokke: no argument for ~a\n" arg))
            (hash-table-update! result 'actions (add-evaluator (cadr args)))
            (loop (cddr args) #f result))
           ((member arg '("-l" "--load"))
            (when (null? (cdr args))
              (quit-early "lokke: no argument for ~a\n" arg))
            (hash-table-update! result 'actions (add-loader (cadr args)))
            (loop (cddr args) #f result))
           ((member arg '("-a" "--apply"))
            (when (null? (cdr args))
              (quit-early "lokke: no argument for ~a\n" arg))
            (hash-table-update! result 'actions (add-apply (cadr args)))
            (loop (cddr args) #f result))
           ((member arg '("-m" "--main"))
            (when (null? (cdr args))
              (quit-early "lokke: no argument for ~a\n" arg))
            (loop (cons* "-a" (string-append (cadr args) "/-main")
                         (cddr args))
                  add-repl? result))
           ((string=? "-" arg)
            (hash-table-update! result 'actions
                                (add-evaluator (slurp (current-input-port))))
            (loop (cdr args) #f result))
           ((not (eqv? #\- (string-ref arg 0)))
            (hash-table-update! result 'actions (add-loader arg))
            (loop (cdr args) #f result))
           ((equal? "--" arg)
            (hash-table-set! result 'args (cdr args))
            (clean-up result))
           (else
            (format (err) "lokke: unrecognized argument: ~s\n" arg)
            (quit-early (usage))))))))

(define (lokke-run args usage)
  (maybe-randomize-state args)
  (let ((opts (parse-run-args (remove-seed-args args) usage)))
    ((module-ref (resolve-ns 'lokke.deps) 'load-deps-files)
     (hash-table-ref opts 'deps))
    (let ((actions (hash-table-ref opts 'actions)))
      (begin
        (binding (*command-line-args* (hash-table-ref opts 'args))
                 (set-current-module (default-environment))
                 (for-each (lambda (action) (action)) actions))
        0))))

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

(define (lok-main args)
  (lokke-run (cdr args) lok-usage))

(define (lokke-main args)
  (let* ((len (length args))
         (args (if (< len 2) (cons* (car args) "run" (cdr args)) args))
         (cmd (cadr args)))
    (cond
     ((member cmd '("-?" "-h" "--help" "help")) (quit (lokke-usage) 0))
     ((string-prefix? "-0" cmd)
      (unless (string=? "-0" cmd)
        (quit-early "lokke: -0 must be the only #! argument\n"))
      (run-script args lokke-usage))
     ((string=? "run" cmd) (lokke-run (cddr args) lokke-usage))
     (else
      (display (lokke-usage) (err))
      (quit-early "lokke: unrecognized subcommand ~s\n" cmd)))))
