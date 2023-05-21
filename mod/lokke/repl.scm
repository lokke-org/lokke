;;; Copyright (C) 2015-2019 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

(define-module (lokke repl)
  #:use-module ((lokke borrowed repl) #:select (repl-for-current-module))
  #:use-module ((lokke ns) #:select (default-environment))
  #:export (repl)
  #:duplicates (merge-generics replace warn-override-core warn last))

(define setlocale
  (if (defined? 'setlocale (resolve-module '(guile)))
      (@ (guile) setlocale)
      #f))

(define (repl)
  (set-current-module (default-environment))
  (current-language 'lokke)
  (repl-for-current-module))
