;;; Copyright (C) 2020 Rob Browning <rlb@defaultvalue.org>
;;;
;;; This project is free software; you can redistribute it and/or modify
;;; it under the terms of (at your option) either of the following two
;;; licences:
;;;
;;;   1) The GNU Lesser General Public License as published by the Free
;;;      Software Foundation; either version 2.1, or (at your option) any
;;;      later version
;;;
;;;   2) The Eclipse Public License; either version 1.0 or (at your
;;;      option) any later version.

(define-module (lokke pcre2)
  #:version (0 0 0)
  ;; document u32 arrays must be host byte order (does not handle BOM)
  #:export (
            <pcre2-code-u32>
            <pcre2-code-u8>
            <pcre2-code-utf32>
            <pcre2-code-utf8>
            <pcre2-match-32>
            <pcre2-match-8>
            PCRE2_ERROR_NOMATCH
            PCRE2_ERROR_PARTIAL
            PCRE2_JIT_COMPLETE
            pcre2-compile-u32 ;; quashes PCRE2_NO_UTF_CHECK if PCRE2_UTF or PCRE2_MATCH_INVALID_UTF
            pcre2-compile-u8  ;; quashes PCRE2_NO_UTF_CHECK if PCRE2_UTF or PCRE2_MATCH_INVALID_UTF
            pcre2-compile-utf
            pcre2-get-error-message
            pcre2-jit-compile
            pcre2-major
            pcre2-make-match-data-32
            pcre2-make-match-data-8
            pcre2-match-data-for-code-32
            pcre2-match-data-for-code-8
            pcre2-match-u32
            pcre2-match-u8
            pcre2-match-utf
            pcre2-minor

            ;;pcre2-match-ovector-length
            ;;pcre2-match-ovector-ref
            pcre2-match-ovector

            ;; Compile a matcher for the internal bytes...
            ;;%compile-string-units
            ;;   compiles string as is by pcre2-compile-u8 or pcre2-compile-u32

            ;; %match-string-units
            ;;   requires code and match-data matching str 8/32
            ;;   matches string as is by pcre2-match-u8 or pcre2-match-u32
            ))

(load-extension "lokke-pcre2.so" "init_pcre2")
