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

(define-module (lokke scm test-anything)
  #:use-module ((ice-9 regex)
                #:select (regexp-substitute/global))
  #:use-module ((srfi srfi-28))
  #:use-module ((srfi srfi-64)
                #:select (test-result-ref
                          test-result-kind
                          test-runner-factory
                          test-runner-null
                          test-runner-on-test-end!
                          test-runner-on-bad-count!
                          test-runner-on-bad-end-name!
                          test-runner-on-final!
                          test-runner-test-name
                          test-runner-fail-count
                          test-runner-skip-count
                          test-runner-pass-count))
  #:export (tap-test-runner))

(define tap-test-name
  (let ((quashes (make-regexp "[\n\r\f]+")))
    (lambda (runner)
      (let* ((name (test-runner-test-name runner))
             (name (if (positive? (string-length name))
                       name
                       (format "~s" (test-result-ref runner 'source-form "")))))
        (regexp-substitute/global #f quashes name 'pre " " 'post)))))

(define (on-test-end runner)
  (let ((kind (test-result-kind runner))
        (name (tap-test-name runner)))
    (display (case kind
               ((pass) (format "ok ~a~%" name))
               ((fail) (format "not ok ~a~%" name))
               ((xpass) (format "ok ~a # todo~%" name))
               ((xfail) (format "not ok ~a # todo~%" name))
               ((skip) (format "ok ~a # skip~%" name))
               (else (error "Unexpected test result:" kind))))))

(define (on-bad-count runner actual expected)
  (display (format "# ERROR: expected ~a tests but found ~a~%"
                   expected actual)))

(define (on-bad-end-name runner begin-name end-name)
  (display (format "# ERROR: test-end's name ~s does not match begin's ~s~%"
                   end-name begin-name)))

(define (on-final runner)
  (display "1..")
  (display (+ (test-runner-pass-count runner)
              (test-runner-fail-count runner)
              (test-runner-skip-count runner)))
  (newline))

(define (tap-test-runner)
  (let ((r (test-runner-null)))
    ;;(test-runner-aux-value! r (alist->hash-table '(count . 0)))
    (test-runner-on-test-end! r on-test-end)
    (test-runner-on-bad-count! r on-bad-count)
    (test-runner-on-bad-end-name! r on-bad-end-name)
    (test-runner-on-final! r on-final)
    r))
