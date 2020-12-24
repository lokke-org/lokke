;;; Copyright (C) 2019 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

(define-module (lokke ns lokke shell)
  #:use-module ((ice-9 atomic)
                #:select (atomic-box-ref atomic-box-set! make-atomic-box))
  #:use-module ((ice-9 futures) #:select (future touch))
  #:use-module ((ice-9 iconv) #:select (string->bytevector bytevector->string))
  #:use-module ((oop goops) #:select (class-of))
  #:use-module ((ice-9 i18n) #:select (locale-encoding))
  #:use-module ((ice-9 popen) #:select (open-pipe* close-pipe))
  #:use-module ((ice-9 receive) #:select (receive))
  #:use-module ((ice-9 binary-ports) #:select (get-bytevector-all))
  #:use-module ((lokke base dynamic) #:select (binding defdyn))
  #:use-module ((lokke hash-map) #:select (hash-map reduce-kv))
  #:use-module ((rnrs bytevectors)
                #:select (bytevector?
                          bytevector-length))
  #:use-module ((rnrs io ports) #:select (open-bytevector-output-port))
  #:use-module ((srfi srfi-1) #:select (append-map break))
  #:export (*sh-dir* *sh-env* sh double-quote with-sh-dir with-sh-env
                     test-err))

(defdyn *sh-dir* #nil)
(defdyn *sh-env* #nil)

(define-syntax-rule (with-sh-dir dir exp ...)
  (binding (*sh-dir* dir) exp ...))

(define-syntax-rule (with-sh-env env exp ...)
  (binding (*sh-env* env) exp ...))

(define (double-quote s)
  (list->string
   (append '(#\")
           (append-map (lambda (c)
                         (case c
                           ((#\" #\\ #\` #\$) (list #\\ c))
                           (else (list c))))
                       (string->list s))
           '(#\"))))

(define (sh . args)
  ;; FIXME: non-string args
  ;; Suppose we could add :locale as a convenience enc value.
  ;; We could allow specification of guile conversion strategies.
  ;; Alternately, could just allow ports for :in, etc.
  (define* (get-opts #:key in in-enc
                     (out-enc #:bytes)
                     (err-enc (locale-encoding))
                     (dir *sh-dir*)
                     (env *sh-env*))
    `((#:in . ,in)
      (#:in-enc . ,in-enc)
      (#:out-enc . ,out-enc)
      (#:err-enc . ,err-enc)
      (#:env . ,env)
      (#:dir . ,dir)))
  (receive (argv opts) (break keyword? args)
    (let* ((opts (apply get-opts opts))
           (mode (if (assq-ref opts #:in) OPEN_BOTH OPEN_READ))
           (in (assq-ref opts #:in))
           (in-enc (assq-ref opts #:in-enc))
           (_ (if (string? in)
                  (unless in-enc
                    (error "Encoding not specified for sh input string"))
                  (when in-enc
                    (error "Encoding provided for sh input bytes"))))
           (out-enc (assq-ref opts #:out-enc))
           (err-enc (assq-ref opts #:err-enc))
           (env (assq-ref opts #:env))
           (argv (if env
                     (append '("env")
                             (reduce-kv (lambda (result k v)
                                          (cons (string-append k "=" v)
                                                result))
                                        '()
                                        env)
                             argv)
                     argv))
           (dir (assq-ref opts #:dir))
           (argv (if (not dir)
                     argv
                     (list "/bin/sh" "-c"
                           (string-append
                            "cd " (double-quote dir)
                            " && "
                            (string-join (map double-quote argv)
                                         " ")))))
           (err-pipe (pipe))
           (err (future (let ((bytes (get-bytevector-all (car err-pipe))))
                          (if (eq? #:bytes err-enc)
                              (if (eof-object? bytes)
                                  #vu8()
                                  bytes)
                              (if (eof-object? bytes)
                                  ""
                                  (bytevector->string bytes err-enc))))))
           (pipe (with-error-to-port (cdr err-pipe)
                   (lambda ()
                     (apply open-pipe* mode argv)))))
      (when in
        (cond
         ((bytevector? in)
          (if (> (bytevector-length in) PIPE_BUF)
              (future (begin (write in pipe) (force-output pipe)))
              (begin (write in pipe) (force-output pipe))))
         ((string? in)
          (let ((b (string->bytevector in in-enc)))
            (if (> (bytevector-length in) PIPE_BUF)
                (future (begin (write in pipe) (force-output pipe)))
                (begin (write in pipe) (force-output pipe)))))
         (else (error "sh input not bytevector or string:" (class-of in)))))

      (let* ((out (let ((bytes (get-bytevector-all pipe)))
                    (if (eq? #:bytes out-enc)
                        (if (eof-object? bytes)
                            #vu8()
                            bytes)
                        (if (eof-object? bytes)
                            ""
                            (bytevector->string bytes out-enc)))))
             (status (close-pipe pipe))
             (_ (close (cdr err-pipe)))
             (err (touch err)))
        (hash-map #:status status
                  #:exit (status:exit-val status)
                  #:out out
                  #:err err)))))
