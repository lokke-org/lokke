;;; Copyright (C) 2021 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

(define-module (lokke uuid)
  #:duplicates (merge-generics replace warn-override-core warn last)
  #:use-module ((ice-9 format) #:select (format))
  #:use-module ((lokke pr) #:select (pr-readable to-string))
  #:use-module (oop goops)
  #:re-export (pr-readable to-string)
  #:export (nil-uuid
            random-uuid
            tagged-data->uuid
            uuid->integer
            uuid->tagged-data
            uuid-version
            uuid?))

;; For now, we only provide a lokke-level implementation, but we could
;; easily provide a pure scheme basis later.
;;
;; At the moment, when reading a value, any UUID version (random,
;; node-based, etc.) is accepted, even versions that aren't yet
;; defined in a standard.  We assume that all of the versions can be
;; converted to an integer by reading the serialized string as a
;; network-byte-order hexadecimal encoding.  Currently, the only
;; allowed variant is the the RFC 4122 (Leach-Salz) variant.

(define-class <uuid> () (%value #:init-keyword #:%value))

(define-inlinable (uuid-value uuid) (slot-ref uuid '%value))
(define (uuid->integer uuid) (uuid-value uuid))

(define (make-uuid n) (make <uuid> #:%value n))

(define (uuid? x) (is-a? x <uuid>))

(define nil-uuid (make-uuid 0))

(define-method (equal? (x <uuid>) (y <uuid>))
  (= (uuid-value x) (uuid-value y)))

(define-inlinable (uuid-version uuid)
  (bit-extract (uuid-value uuid) 40 44))

(define (uuid-int->string n)
  (format #f "~8,'0x-~4,'0x-~4,'0x-~4,'0x-~12,'0x"
          (bit-extract n 96 128)
          (bit-extract n 80 96)
          (bit-extract n 64 80)
          (bit-extract n 48 64)
          (bit-extract n 0 48)))

(define-inlinable (hex-substr->int s start end)
  (let ((u (substring/shared s start end)))
    (unless (string-every char-set:hex-digit u)
      (error "Invalid UUID syntax:" s u))
    (string->number u 16)))

(define (string->uuid-int s)
  (unless (= 36 (string-length s))
    (error "UUID string is not 36 characters long:" s))
  (for-each (lambda (i) (unless (eqv? #\- (string-ref s i))
                          (error "Invalid UUID syntax:" s)))
            '(8 13 18 23))
  (let ((n (logior (ash (hex-substr->int s 0 8) 96)
                   (ash (hex-substr->int s 9 13) 80)
                   (ash (hex-substr->int s 14 18) 64)
                   (ash (hex-substr->int s 19 23) 48)
                   (hex-substr->int s 24 36))))
    (unless (= 2 (bit-extract n 62 64))
      (error "Only RFC 4122 (Leach-Salz) UUID variants are currently supported"
             s))
    n))

(define (tagged-data->uuid data)
  (unless (string? data)
    (error "Invalid #uuid data:" data))
  (make-uuid (string->uuid-int data)))

(define (uuid->tagged-data uuid)
  (uuid-int->string (uuid-value uuid)))

(define-method (write (uuid <uuid>) port)
  (format port "#<~s ~x " (class-name (class-of uuid)) (object-address uuid))
  (display (uuid->tagged-data uuid) port)
  (display ">" port))

(define-method (to-string (uuid <uuid>))
  (string-append "#uuid \"" (uuid->tagged-data uuid) "\""))

(define-method (pr-readable (uuid <uuid>) port)
  (display (to-string uuid) port))

(define (random-uuid)
  "Returns a random UUID (RFC 4122 (Leach-Salz) version 4 variant 1)."
  (make-uuid (logior (logand (random #x100000000000000000000000000000000)
                             #xffffffffffff0fff3fffffffffffffff)
                     #x0000000000004000c000000000000000)))
