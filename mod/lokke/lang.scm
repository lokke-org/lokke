;;; Copyright (C) 2019 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

;; Support functions for clojure.core (at least).

(define-module (lokke lang)
  #:export (named?))

(define (named? x)
  (or (symbol? x) (keyword? x)))
