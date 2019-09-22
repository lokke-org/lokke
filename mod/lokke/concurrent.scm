;;; Copyright (C) 2019 Rob Browning <rlb@defaultvalue.org>
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

;; This module must not depend on (lokke collection) because because
;; it depends on (lokke base syntax) which depends on (lokke base
;; destructure) which depends on hash-map which depends on this.

(read-set! keywords 'postfix)  ;; srfi-88

(define-module (lokke concurrent)
  use-module: ((ice-9 atomic) select: (make-atomic-box))
  use-module: (oop goops)
  use-module: ((lokke scm atom)
               select: (atom?
                        atom
                        atom-add-watch
                        atom-compare-and-set!
                        atom-deref
                        atom-remove-watch
                        atom-reset!
                        atom-set-validator!
                        atom-swap!))
  export: (<atom>
           atom?
           add-watch
           remove-watch
           deref
           reset!
           set-validator!
           swap!)
  re-export: (atom)
  duplicates: (merge-generics replace warn-override-core warn last))

(define <atom> (class-of (make-atomic-box #t)))
(define (atom? x) (is-a? x <atom>))
(define-method (deref (a <atom>)) (atom-deref a))
(define (reset! a newval) (atom-reset! a newval))
(define (swap! a . args) (apply atom-swap! a args))
(define (compare-and-set! a oldval newval)
  (apply atom-compare-and-set! a oldval newval))

(define-method (add-watch (a <atom>) key fn) (atom-add-watch a key fn))
(define-method (remove-watch (a <atom>) key) (atom-remove-watch a key))
(define-method (set-validator! (a <atom>) validate)
  (atom-set-validator! a validate))
