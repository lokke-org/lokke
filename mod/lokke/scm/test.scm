;;; Copyright (C) 2021 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

(define-module (lokke scm test)
  #:use-module ((srfi srfi-64)
                #:select (test-end test-runner-current test-runner-fail-count))
  #:export (test-end-and-exit))

(define* (test-end-and-exit status #:optional name)
  (let ((failures (test-runner-fail-count (test-runner-current))))
    (if name
        (test-end name)
        (test-end))
    (unless (zero? failures)
      (exit status))))
