;;; Copyright (C) 2015-2019 Rob Browning <rlb@defaultvalue.org>
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

(define-module (lokke repl)
  use-module: ((lokke ns) select: (default-environment))
  use-module: ((system repl repl) select: (start-repl))
  export: (repl)
  duplicates: (merge-generics replace warn-override-core warn last))

;; If upstream is willing to add an initial-module argument to repl,
;; or equivalent, then we won't need all this.  For now, just do
;; exactly what the Guile top-repl does, and then changes the module
;; to the default-environment, i.e. (lokke user).

;; call-with-sigint taken from the version in Guile 2.2.6 (LGPL 3)
;; FIXME: propose accommodations upstream
(define call-with-sigint
  (if (not (provided? 'posix))
      (lambda (thunk) (thunk))
      (lambda (thunk)
        (let ((handler #f))
          (dynamic-wind
            (lambda ()
              (set! handler
                    (sigaction SIGINT
                      (lambda (sig)
                        (scm-error 'signal #f "User interrupt" '()
                                   (list sig))))))
            thunk
            (lambda ()
              (if handler
                  ;; restore Scheme handler, SIG_IGN or SIG_DFL.
                  (sigaction SIGINT (car handler) (cdr handler))
                  ;; restore original C handler.
                  (sigaction SIGINT #f))))))))

(define setlocale
  (if (defined? 'setlocale (resolve-module '(guile)))
      (@ (guile) setlocale)
      #f))

;; repl-for-current-module adapted from the version in Guile 2.2.6 (LGPL 3)
;; FIXME: propose accommodations upstream
(define (repl-for-current-module)
  (save-module-excursion
   (lambda ()
     (let ((guile-user-module (resolve-module '(guile-user))))
       ;; Use some convenient modules (in reverse order)
       (set-current-module guile-user-module)
       (process-use-modules
        (append
         '(((ice-9 r5rs))
           ((ice-9 session)))
         (if (provided? 'regex)
             '(((ice-9 regex)))
             '())
         (if (provided? 'threads)
             '(((ice-9 threads)))
             '()))))))
  (call-with-sigint
   (lambda ()
     (and setlocale
          (catch 'system-error
            (lambda ()
              (setlocale LC_ALL ""))
            (lambda (key subr fmt args errno)
                (format (current-error-port)
                        "warning: failed to install locale: ~a~%"
                        (strerror (car errno))))))
       (let ((status (start-repl (current-language))))
         (run-hook exit-hook)
         status))))

(define (repl)
  (set-current-module (default-environment))
  (current-language 'lokke)
  (repl-for-current-module))
