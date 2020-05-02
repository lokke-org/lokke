;;; Copyright (C) 2020 Rob Browning <rlb@defaultvalue.org>
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

(define-module (lokke sysenv)
  #:export (prepare-environment))

(define (prepare-environment lib-dir mod-dir)
  (unless (string-prefix? "/" lib-dir)
    (error "error: library dir must be absolute" lib-dir))
  (unless (string-prefix? "/" mod-dir)
    (error "error: module dir must be absolute" mod-dir))
  ;; Until we fix it ourselves, or guile provides better options, Just
  ;; have to hope the sys-ctype returned will work right when passed
  ;; to setlocale from the C locale.
  (let ((sys-ctype (getenv "LC_CTYPE")))
    ;; Can we (portably) do better than C if C isn't strictly defined to
    ;; provide pass-through.
    (setlocale LC_CTYPE "C")
    (let ((env-load (getenv "GUILE_LOAD_PATH"))
          (env-lib (getenv "LTDL_LIBRARY_PATH")))
      (setlocale LC_CTYPE (or sys-ctype ""))
      (set! %load-path (cons mod-dir %load-path))
      (when env-load
        (set! %load-path (cons env-load %load-path)))
      (if env-lib
          (setenv "LD_LIBRARY_PATH" (string-append env-lib ":" lib-dir))
          (setenv "LD_LIBRARY_PATH" lib-dir)))))
