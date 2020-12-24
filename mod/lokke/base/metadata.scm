;;; Copyright (C) 2019-2020 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

(define-module (lokke base metadata)
  #:use-module (oop goops)
  #:export (alter-meta! meta vary-meta with-meta)
  #:duplicates (merge-generics replace warn-override-core warn last))

(define-generic alter-meta!)
(define-generic meta)
(define-generic vary-meta)
(define-generic with-meta)
