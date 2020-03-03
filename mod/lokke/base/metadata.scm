;;; Copyright (C) 2019-2020 Rob Browning <rlb@defaultvalue.org>
;;;
;;; This project is free software; you can redistribute it and/or
;;; modify it under the terms of (at your option) either of the
;;; following two licences:
;;;
;;;   1) The GNU Lesser General Public License as published by the
;;;      Free Software Foundation; either version 2.1, or (at your
;;;      option) any later version.
;;;
;;;   2) The Eclipse Public License; either version 1.0 or (at your
;;;      option) any later version.

(define-module (lokke base metadata)
  #:use-module (oop goops)
  #:export (alter-meta! meta vary-meta with-meta)
  #:duplicates (merge-generics replace warn-override-core warn last))

(define-generic alter-meta!)
(define-generic meta)
(define-generic vary-meta)
(define-generic with-meta)
