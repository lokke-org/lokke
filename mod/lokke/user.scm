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

;; This is the default module, i.e. the normal enviornment for the
;; REPL and command-line -e/-i evaluation.  We define it in Scheme so
;; that you can more easily switch to lokke from the guile repl.  See
;; the README for details.

((@@ (lokke ns) ns) guile.lokke.user)
