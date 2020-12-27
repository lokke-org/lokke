;;; Copyright (C) 2019-2020 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

(define-module (lokke ns clojure main)
  #:use-module ((ice-9 format) #:select (format))
  #:use-module ((lokke config) #:select (ensure-config-dir))
  #:export (repl))

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
        (save-module-excursion
         (lambda ()
           (set-current-module (resolve-module '(guile-user)))
           (primitive-load init-file))))))

(define (repl)
  (configure-history)
  (load-user-init)
  ((@ (lokke repl) repl))
  #nil)
