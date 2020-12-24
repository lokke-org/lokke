;;; Copyright (C) 2019 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

;; At the moment, this module is just a convenience so that these
;; functions will be available via lokke.exception.

(define-module (lokke ns lokke exception)
  #:version (0 0 0)
  #:use-module ((lokke exception)
                #:select (ex-info? ex-suppressed ex-tag with-final))
  #:re-export (ex-info? ex-suppressed ex-tag with-final)
  #:duplicates (merge-generics replace warn-override-core warn last)
  #:pure)
