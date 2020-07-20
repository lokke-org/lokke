;;; Copyright (C) 2020 Rob Browning <rlb@defaultvalue.org>
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
    (if xdg
        (if (string=? "" xdg)
            (string-append (getenv "HOME") "/" dot-name "/lokke")
            xdg)
        (string-append (getenv "HOME") "/" dot-name "/lokke"))))

(define (cache-dir) (xdg-dir "XDG_CACHE_HOME" ".cache"))
(define (config-dir) (xdg-dir "XDG_CONFIG_HOME" ".config"))
(define (ensure-cache-dir) (ensure-dir (cache-dir)))
(define (ensure-config-dir) (ensure-dir (config-dir)))
