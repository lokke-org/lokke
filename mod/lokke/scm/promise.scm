;;; Copyright (C) 2020 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

(define-module (lokke scm promise)
  #:use-module ((ice-9 atomic)
                #:select (atomic-box-compare-and-swap!
                          atomic-box-ref
                          atomic-box-set!
                          make-atomic-box))
  #:use-module ((ice-9 match) #:select (match-lambda* match-let*))
  #:use-module ((ice-9 threads) #:select (lock-mutex make-mutex unlock-mutex))
  #:use-module ((srfi srfi-9) #:select (define-record-type))
  #:export (<promise>
            promise?
            promise
            promise-deliver
            promise-deref)
  #:duplicates (merge-generics replace warn-override-core warn last))

(define-record-type <promise>
  (%promise state)
  promise?
  ;; state is (#f . mutex) until delivery
  ;; state is (value) after delivery
  (state %state))

(define (promise)
  (let ((m (make-mutex 'allow-external-unlock)))
    (lock-mutex m)
    (%promise (make-atomic-box (cons #f m)))))

(define (promise-deliver p val)
  (let* ((box (%state p))
         (state (atomic-box-ref box))
         (m (cdr state)))
    (unless (null? m)
      (let* ((prev (atomic-box-compare-and-swap! box state (list val))))
        (when (eq? prev state)
          (unlock-mutex m))))))

(define-inlinable (normalize-ts s us)
  (let ((n (+ (* s 1000000) us)))
    (cons (quotient n 1000000) (remainder n 1000000))))

(define promise-deref
  (match-lambda*
    ((p)
     (let* ((box (%state p))
            (state (atomic-box-ref box))
            (m (cdr state)))
       (if (null? m)
           (car state)
           (begin
             (lock-mutex m)
             (unlock-mutex m)
             (car (atomic-box-ref box))))))
    ((p timeout-ms timeout-val)
     (let* ((box (%state p))
            (state (atomic-box-ref box))
            (m (cdr state)))
       (if (null? m)
           (car state)
           (match-let* (((sec . usec) (gettimeofday))
                        (deadline (normalize-ts sec (+ usec (* timeout-ms 1000))))
                        (w (lock-mutex m deadline)))
             (if (not w)
                 timeout-val
                 (begin
                   (unlock-mutex m)
                   (car (atomic-box-ref box))))))))))
