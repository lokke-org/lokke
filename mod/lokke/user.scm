;;; Copyright (C) 2015-2019 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

;; This is the default module, i.e. the normal enviornment for the
;; REPL and command-line -e/-i evaluation.  We define it in Scheme so
;; that you can more easily switch to lokke from the guile repl.  See
;; the README for details.

((@@ (lokke ns) ns) guile.lokke.user)
