;;; Copyright (C) 2014, 2015 Free Software Foundation, Inc.
;;; Copyright (C) 2019 Rob Browning <rlb@defaultvalue.org>
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; This is an ugly hack of Guile's make-foreign-object-type to
;;; support the specification of superclasses, so that we can just
;;; make <lokke-vector> a <sequential> from the start.  In the long
;;; run, we should either change our approach, or see if the change
;;; might be acceptable upstream.  The current code was taken from and
;;; so is compabitle with at least Guile 2.2.6.

;; FIXME: badger upstream?  ^

(define-module (lokke scm foreign-object)
  #:use-module (oop goops)
  #:export (make-foreign-object-type*))

(define <foreign-class>
  (@@ (system foreign-object) <foreign-class>))
(define <foreign-class-with-finalizer>
  (@@ (system foreign-object) <foreign-class-with-finalizer>))

(define* (make-foreign-object-type* name slots
                                    #:key
                                    finalizer
                                    (getters (map (const #f) slots))
                                    (supers '()))
  (unless (symbol? name)
    (error "type name should be a symbol" name))
  (unless (or (not finalizer) (procedure? finalizer))
    (error "finalizer should be a procedure" finalizer))
  (let ((dslots (map (lambda (slot getter)
                       (unless (symbol? slot)
                         (error "slot name should be a symbol" slot))
                       (cons* slot #:class <foreign-slot>
                              #:init-keyword (symbol->keyword slot)
                              #:init-value 0
                              (if getter (list #:getter getter) '())))
                     slots
                     getters)))
    (if finalizer
        (make-class supers dslots #:name name
                    #:finalizer finalizer
                    #:static-slot-allocation? #t
                    #:metaclass <foreign-class-with-finalizer>)
        (make-class supers dslots #:name name
                    #:static-slot-allocation? #t
                    #:metaclass <foreign-class>))))
