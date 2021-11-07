;;; Copyright (C) 2020 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

(define-module (lokke compat)
  #:export (if-at-least-guile-version re-export-and-replace!))

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

(define (re-export-and-replace! . names)
  ;; Guile 3.0.0 expands guile-2 and guile-2.2
  (cond-expand
    (guile-3.0
     (module-re-export! (current-module) names #:replace? #t))
    (guile-2.2
     (module-re-export! (current-module) names))
    (else
     (module-re-export! (current-module) names #:replace? #t))))
