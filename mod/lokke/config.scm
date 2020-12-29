;;; Copyright (C) 2020 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

(define-module (lokke config)
  #:export (cache-dir config-dir ensure-cache-dir ensure-config-dir))

(define (ensure-dir path)
  (catch 'system-error
    (lambda ()
      (mkdir path)
      path)
    (lambda (key fname fmt-pattern fmt-args info)
      (unless (= EEXIST (car info))
        (throw key fname fmt-pattern fmt-args info))
      path)))

(define (xdg-dir env-name dot-name)
  (let ((xdg (getenv env-name)))
    (if (and xdg (not (string=? "" xdg)))
        (string-append xdg "/lokke")
        (string-append (getenv "HOME") "/" dot-name "/lokke"))))

(define (cache-dir) (xdg-dir "XDG_CACHE_HOME" ".cache"))
(define (config-dir) (xdg-dir "XDG_CONFIG_HOME" ".config"))
(define (ensure-cache-dir) (ensure-dir (cache-dir)))
(define (ensure-config-dir) (ensure-dir (config-dir)))
