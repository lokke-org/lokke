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

(read-set! keywords 'postfix)  ; srfi-88

(define-module (lokke ns clojure test)
  use-module: ((lokke scm test-anything) select: (tap-test-runner))
  use-module: ((srfi srfi-64)
               select: (test-assert
                        test-begin
                        test-end
                        test-equal
                        test-group
                        test-runner-current
                        test-runner-fail-count))
  export: (begin-tests
           end-tests
           is
           testing))

(define* (begin-tests suite-name)
  (when (equal? "tap" (getenv "LOKKE_TEST_PROTOCOL"))
    (test-runner-current (tap-test-runner)))
  (test-begin (if (symbol? suite-name)
                  (symbol->string suite-name)
                  suite-name)))

(define* (end-tests optional: suite-name key: exit?)
  (if suite-name
      (test-end (if (symbol? suite-name)
                    (symbol->string suite-name)
                    suite-name))
      (test-end))
  (when exit?
    (exit (if (zero? (test-runner-fail-count (test-runner-current)))
              0
              2))))

;; For now just supports (is x) and (is (= x y)), since that's easy to
;; do with srfi-64, but this will almost certainly need an overhaul,
;; and/or moving away from the srfi.

(define-syntax is
  (syntax-rules (=)
    ((_ (= expected expression)) (test-equal expected expression))
    ((_ (= expected expression) msg) (test-equal msg expected expression))
    ((_ expression) (test-assert expression))
    ((_ expression msg) (test-assert msg expression))))

(define-syntax testing
  (syntax-rules ()
    ((_ what test ...) (test-group what test ...))))
