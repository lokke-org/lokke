;;; Copyright (C) 2019-2020 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

(define-module (lokke base invoke)
  #:use-module ((guile) #:select ((apply . %scm-apply)))
  #:use-module (oop goops)
  #:export (invoke)
  #:replace (apply)
  #:duplicates (merge-generics replace warn-override-core warn last))

;; FIXME: this base case will be clobbered later by (lokke base collection)
(define apply %scm-apply)

(define-generic apply)

(define-method (invoke (f <procedure>) . args)
  (apply f args))

(define-method (invoke (f <generic>) . args)
  (apply f args))

(define pws-class (class-of (make-procedure-with-setter identity identity)))
(define-method (invoke (f pws-class) . args)
  (apply f args))

;; FIXME: do we want this?
(define parameter-class (class-of (make-parameter #f)))
(define-method (invoke (f parameter-class) . args)
  (apply f args))
