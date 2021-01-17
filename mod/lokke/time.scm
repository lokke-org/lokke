;;; Copyright (C) 2021 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

(define-module (lokke time)
  #:use-module ((ice-9 format) #:select (format))
  #:use-module ((ice-9 match) #:select (match))
  #:use-module ((ice-9 regex) #:select (match:substring))
  #:use-module ((lokke pr) #:select (pr-readable to-string))
  #:use-module (oop goops)
  #:use-module ((srfi srfi-11) #:select (let-values))
  #:re-export (pr-readable to-string)
  #:export (instant
            instant?
            instant->tagged-data
            instant-ns
            normalize-ts
            tagged-data->instant
            write-instant)
  #:duplicates (merge-generics replace warn-override-core warn last))

(define-inlinable (normalize-ts s us)
  (let ((n (+ (* s 1000000) us)))
    (cons (quotient n 1000000) (remainder n 1000000))))

(define-class <instant> () (ns #:init-keyword #:ns))

(define-inlinable (instant-ns i) (slot-ref i 'ns))

(define (instant ns)
  (make <instant> #:ns ns))

(define (instant? x)
  (is-a? x <instant>))

(define-method (equal? (x <instant>) (y <instant>))
  (= (instant-ns x) (instant-ns y)))

(define remainder-rx (make-regexp "^(\\.[0-9]+)?((([-+][0-9]{2}):([0-9]{2}))|Z)$"))

(define (tagged-data->instant data)
  ;; Currently only handles (with or without fractional seconds):
  ;;   "1970-01-01T00:00:00.100Z"
  ;;   "1970-01-01T00:00:00.100-06:00"
  (unless (string? data)
    (error "Invalid #inst content:" data))
  (match (strptime "%Y-%m-%dT%T" data)
    ((tm . n)
     (let* ((m (regexp-exec remainder-rx data n))
            (_ (unless m
                 (error "Invalid #inst content:" data)))
            (frac-sec (or (match:substring m 1) "0"))
            (frac-sec (string->number frac-sec))
            (frac-ns (inexact->exact (round (* frac-sec 1000000))))
            (offset (if (string=? "Z" (match:substring m 2))
                        0
                        (* 60
                           (+ (* 60 (string->number (match:substring m 4)))
                              (string->number (match:substring m 5)))))))
       (set-tm:gmtoff tm offset)
       (match (mktime tm "UTC")
         ((s . tm) (instant (+ (* s 1000000) frac-ns))))))))

(define (instant->tagged-data inst)
  (let-values (((s ns) (floor/ (instant-ns inst) 1000000)))
    (let* ((tm (gmtime s))
           (off (tm:gmtoff tm)))
      (string-append
       "\""
       (strftime "%FT%T" tm)
       (if (zero? ns) "" (format #f ".~d" ns))
       (if (zero? off) "Z" (let-values (((h s) (truncate/ off (* 60 60))))
                             (format #f "~d:~d" h (/ (abs s) 60))))
       "\""))))

(define-method (write (inst <instant>) port)
  (format port "#<~s ~x " (class-name (class-of inst)) (object-address inst))
  (display (instant->tagged-data inst) port)
  (display ">" port))

(define-method (to-string (inst <instant>))
  (string-append "#inst " (instant->tagged-data inst)))

(define-method (pr-readable (inst <instant>) port)
  (display (to-string inst) port))
