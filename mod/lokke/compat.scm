;;; Copyright (C) 2020 Rob Browning <rlb@defaultvalue.org>
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
