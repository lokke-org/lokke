;;; Copyright (C) 2020 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

(define-module (lokke compat)
  #:export (if-at-least-guile-version))

(define-syntax if-at-least-guile-version
  (lambda (x)
    (syntax-case x ()
      ((_ (maj min mic) then else)
       (let ((maj (syntax->datum #'maj))
             (min (syntax->datum #'min))
             (mic (syntax->datum #'mic)))
         (if (and (>= (string->number (major-version)) maj)
                  (>= (string->number (minor-version)) min)
                  (>= (string->number (micro-version)) mic))
             #'then
             #'else))))))
