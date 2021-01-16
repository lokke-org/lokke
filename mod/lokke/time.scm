;;; Copyright (C) 2021 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

(define-module (lokke time)
  #:export (normalize-ts)
  #:duplicates (merge-generics replace warn-override-core warn last))

(define-inlinable (normalize-ts s us)
  (let ((n (+ (* s 1000000) us)))
    (cons (quotient n 1000000) (remainder n 1000000))))
