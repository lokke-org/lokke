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

(read-set! keywords 'postfix)  ;; srfi-88

(define-module (lokke user)
  ;; Loading the spec may not be necessary, but shouldn't hurt either.
  use-module: ((language lokke spec) select: ())
  use-module: (lokke boot)
  use-module: (lokke base syntax)
  ;; FIXME: so we still need/want this?
  use-module: ((guile) select: (@@))
  pure:)

;; This module is used by top-level evaluation, compilation,
;; bootstrapping, etc.  The bindings above are the bootstrap set, and
;; after bootstrapping, this module will include all of 'clojure.core
;; (does it?).

;; FIXME: not sure this is really how we want to handle the environment.
