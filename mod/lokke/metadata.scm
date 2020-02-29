;;; Copyright (C) 2019 Rob Browning <rlb@defaultvalue.org>
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

(define-module (lokke metadata)
  #:use-module (oop goops)
  #:export (*print-meta*
            alter-meta!
            meta
            vary-meta
            with-meta)
  #:duplicates (merge-generics replace warn-override-core warn last))

;; We've just let metadata be broken for now, i.e. some of it may
;; kinda work, but we need to review the whole thing, and add a bunch
;; of tests once we decide exactly where we want to start.  So take
;; everything here with a very large grain of salt.

;; We're also leaning toward omitting symbol metadata if we can get
;; away with it.  See the DESIGN doc.

;; Types with immutable metadata implement with-meta, and types with
;; mutable metadata (like Clojure's ref, atom, etc.) implement
;; alter-meta!.

;; For now, types with immutable metadata will typically need to
;; override both equal? and with-meta.  Any GOOPS classes representing
;; Clojure objects (i.e. they're "immutable") can base the latter on
;; shallow-clone if nothing else.

(define *print-meta* (make-parameter #nil))

(define-method (meta obj) #nil)

(define-method (with-meta obj map)
  (scm-error 'wrong-type-arg
             "with-meta"
             "Persistent metadata not supported for ~a: ~s"
             (list (class-of obj) obj)
             (list obj)))

(define (vary-meta obj f . args)
  (with-meta obj (apply f (meta obj) args)))

(define-method (alter-meta! obj f . args)
  (error "Mutable metadata not supported for " (class-of obj)))
