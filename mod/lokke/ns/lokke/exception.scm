;;; Copyright (C) 2019 Rob Browning <rlb@defaultvalue.org>
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

;; At the moment, this module is just a convenience so that these
;; functions will be available via lokke.exception.

(define-module (lokke ns lokke exception)
  #:version (0 0 0)
  #:use-module ((lokke exception)
                #:select (ex-info? ex-suppressed ex-tag with-final))
  #:re-export (ex-info? ex-suppressed ex-tag with-final)
  #:duplicates (merge-generics replace warn-override-core warn last)
  #:pure)
