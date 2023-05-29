;;; Copyright (C) 2019-2020 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

(define-module (lokke ns clojure main)
  #:use-module ((lokke config) #:select (ensure-config-dir))
  #:export (repl))

(define history-configured? #f)

(define (configure-history)
  (unless history-configured?
    (setenv "GUILE_HISTORY"
            (or (getenv "LOKKE_HISTORY")
                (string-append (ensure-config-dir) "/history")))
    (set! history-configured? #t)))


(define user-init-loaded? #f)  ;; Should this be per-module?

;; load-user-init adapted from the version in Guile 2.2.6 (LGPL 3)
;; FIXME: propose accommodations upstream
(define (load-user-init)
  (unless user-init-loaded?
    (let* ((home (or (getenv "HOME")
                     (false-if-exception (passwd:dir (getpwuid (getuid))))
                     file-name-separator-string)) ;; fallback for cygwin etc.
           (init-file (string-append (ensure-config-dir) "/interactive.scm")))
      ;; FIXME: add support for suppressing the init file (e.g. Guile's -q)
      ;; FIXME: instead of or in addition, support loading an interactive.clj?
      ;; We'll need a primitive-load equivalent, etc.
      (if (file-exists? init-file)
          (save-module-excursion
           (lambda ()
             (set-current-module (resolve-module '(guile-user)))
             (primitive-load init-file)))))
    (set! user-init-loaded? #t)))

(define (repl)
  (configure-history)
  (load-user-init)
  ((@ (lokke repl) repl))
  #nil)
