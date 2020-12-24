;;; Copyright (C) 2020 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

(define-module (lokke sysenv)
  #:export (prepare-environment))

(define (prepare-environment lib-dir mod-dir comp-dir)
  (unless (string-prefix? "/" lib-dir)
    (error "error: library dir must be absolute" lib-dir))
  (unless (string-prefix? "/" mod-dir)
    (error "error: module dir must be absolute" mod-dir))
  (unless (string-prefix? "/" comp-dir)
    (error "error: compilation dir must be absolute" comp-dir))
  ;; Until we fix it ourselves, or guile provides better options, Just
  ;; have to hope the sys-ctype returned will work right when passed
  ;; to setlocale from the C locale.
  (let ((sys-ctype (getenv "LC_CTYPE")))
    ;; Can we (portably) do better than C if C isn't strictly defined to
    ;; provide pass-through.
    (setlocale LC_CTYPE "C")
    (let ((env-load (getenv "GUILE_LOAD_PATH"))
          (env-comp (getenv "GUILE_LOAD_COMPILED_PATH"))
          (env-lib (getenv "LTDL_LIBRARY_PATH")))
      (setlocale LC_CTYPE (or sys-ctype ""))
      (set! %load-path (cons mod-dir %load-path))
      (when env-load
        (set! %load-path (cons env-load %load-path)))
      (set! %load-compiled-path (cons comp-dir %load-compiled-path))
      (when env-comp
        (set! %load-compiled-path (cons env-comp %load-compiled-path)))
      (if env-lib
          (setenv "LD_LIBRARY_PATH" (string-append env-lib ":" lib-dir))
          (setenv "LD_LIBRARY_PATH" lib-dir)))))
