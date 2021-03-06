;;; Copyright (C) 2019 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

(define-module (lokke metadata)
  #:use-module ((lokke base metadata)
                #:select (alter-meta! meta vary-meta with-meta))
  #:use-module ((lokke base dynamic) #:select (defdyn))
  #:use-module ((lokke hash-map) #:select (assoc get hash-map hash-map?))
  #:use-module ((lokke pr) #:select (pr-str))
  #:use-module ((lokke scm atom) #:select (atom atom-deref atom-swap!))
  #:use-module (oop goops)
  #:export (*print-meta*)
  #:re-export (alter-meta! meta vary-meta with-meta)
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

(defdyn *print-meta* #nil)

(define-method (meta obj) #nil)

(define (vary-meta obj f . args)
  (with-meta obj (apply f (meta obj) args)))


;; module variables (var)

;; As an optimization, we might be able to subclass guile variables to
;; add a metadata atom containing a hash map, and then we'd need to
;; create a suitable define derivative to create them, and then use it
;; "everywhere".

;; For now, assume that it's acceptable to keep the metadata forever
;; (variables aren't "dropped" often enough to matter, etc., i.e. we
;; don't need a weak mapping.

(define variable-metadata
  (atom (hash-map)))

;; GOOPS doesn't define <variable>
(define var-class (class-of (module-variable (current-module) 'define)))

(define-method (meta (v var-class))
  (get (atom-deref variable-metadata) v #nil))

(define-method (alter-meta! (v var-class) f . args)
  (atom-swap! variable-metadata
              (lambda [prev-vars-meta]
                (let ((new-v-meta (apply f (get prev-vars-meta v (hash-map)) args)))
                  (unless (or (hash-map? new-v-meta) (eq? #nil new-v-meta))
                    (scm-error 'wrong-type-arg 'alter-meta!
                               "New metadata is not a map: ~a"
                               (list (pr-str new-v-meta)) (list new-v-meta)))
                  (assoc prev-vars-meta v new-v-meta)))))
