;;; Copyright (C) 2020 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

(define-module (lokke compat)
  #:export (re-export-and-replace!))

(define (re-export-and-replace! . names)
  ;; Guile 3.0.0 expands guile-2 and guile-2.2
  (cond-expand
    (guile-3.0
     (module-re-export! (current-module) names #:replace? #t))
    (guile-2.2
     (module-re-export! (current-module) names))
    (else
     (module-re-export! (current-module) names #:replace? #t))))
