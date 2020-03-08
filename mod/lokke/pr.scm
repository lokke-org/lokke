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

;; FIXME: consider falling back to write/display (customized via
;; GOOPS) when the reps are the same.

;; FIXME: check scheme write vs clojure more carefully

;; FIXME: *out* *err*, etc.

(define-module (lokke pr)
  #:use-module ((ice-9 format) #:select ((format . %scm-format)))
  #:use-module ((language tree-il) #:prefix tree-il/)
  #:use-module ((lokke base dynamic) #:select (binding defdyn))
  #:use-module ((lokke base util)
                #:select (keyword->string
                          module-name->ns-str
                          module-name->ns-sym))
  #:use-module (oop goops)
  #:replace (format)
  #:export (*err*
            *in*
            *out*
            pr
            pr-on
            pr-str
            print
            printf
            print-on
            print-str
            println
            prn
            str
            with-out-str)
  #:duplicates (merge-generics replace warn-override-core warn last))

(defdyn *in* (current-input-port))
(defdyn *out* (current-output-port))
(defdyn *err* (current-error-port))

;; For now, guile doesn't have one...
(define (tree-il? x)
  (or
   (tree-il/void? x)
   (tree-il/const? x)
   (tree-il/primitive-ref? x)
   (tree-il/lexical-ref? x)
   (tree-il/lexical-set? x)
   (tree-il/module-ref? x)
   (tree-il/module-set? x)
   (tree-il/toplevel-ref? x)
   (tree-il/toplevel-set? x)
   (tree-il/toplevel-define? x)
   (tree-il/conditional? x)
   (tree-il/call? x)
   (tree-il/primcall? x)
   (tree-il/seq? x)
   (tree-il/lambda? x)
   (tree-il/lambda-case? x)
   (tree-il/let? x)
   (tree-il/letrec? x)))

(define-syntax-rule (with-out-str e ...)
  (call-with-output-string
      (lambda (port)
        (binding (*out* port) e ...))))

(define (format . args) (apply %scm-format #f args))

(define (read-only-str s) (substring/read-only s 0))


(define (str-somehow x details)
  (cond
   ((eq? x *unspecified*) "")
   ((eq? x #nil) "nil")
   ((null? x) "()")
   (else
    (read-only-str
     (string-append
      "#object["
      (if (tree-il? x)
          (%scm-format #f "~s" x)
          ;; This may not be the preferred rep for structs/records/etc.
          (apply %scm-format #f "~s 0x~x~a~a"
                 (class-name (class-of x))
                 (object-address x)
                 (if details
                     (list " " details)
                     '("" ""))))
      "]")))))

(define-method (pr-on x port)
  (display (str-somehow x #f) port) #nil)

(define-method (print-on x port)
  (pr-on x port) #nil)


(define-method (pr-on (x <class>) port)
  (display (class-name x) port)
  #nil)


(define (boolean->string x)
  (case x
    ((#t) "true")
    ((#f) "false")
    ((#nil) "nil")
    (else (error "impossible boolean" x))))

(define-inlinable (show-boolean x port)
  (display (boolean->string x) port)
  #nil)

(define-method (pr-on (x <boolean>) port)
  (show-boolean x port))

(define-method (print-on (x <boolean>) port)
  (show-boolean x port))


(define-inlinable (show-null x port)
  (display "()" port) #nil)

(define-method (pr-on (x <null>) port)
  (show-null x port))

(define-method (print-on (x <null>) port)
  (show-null x port))


(define-inlinable (show-num x port)
  (display x port) #nil)

(define-method (pr-on (x <number>) port)
  (show-num x port))

(define-method (print-on (x <number>) port)
  (show-num x port))


;; FIXME: ensure correct clj format
(define-inlinable (show-sym x port)
  (display (symbol->string x) port) #nil)

(define-method (pr-on (x <symbol>) port)
  (show-sym x port))

(define-method (print-on (x <symbol>) port)
  (show-sym x port))


;; FIXME: ensure correct clj format
(define-inlinable (show-keyword x port)
  (display #\: port)
  (display (keyword->string x) port)
  #nil)

(define-method (pr-on (x <keyword>) port)
  (show-keyword x port))

(define-method (print-on (x <keyword>) port)
  (show-keyword x port))


;; FIXME: ensure correct clj format
(define-method (pr-on (c <char>) port)
  (case c
    ((#\newline) (display "\\newline" port))
    ((#\space) (display "\\space" port))
    ((#\tab) (display "\\tab" port))
    ((#\page) (display "\\formfeed" port))
    ((#\backspace) (display "\\backspace" port))
    ((#\return) (display "\\return" port))
    ;; FIXME: adjust to match the jvm (or edn) more precisely
    (else (display (display (string #\\ c) port)))))

(define-method (print-on (x <char>) port)
  (display x port) #nil)


;; FIXME: ensure correct clj format
(define-method (pr-on (x <string>) port)
  (write x port) #nil)

(define-method (print-on (x <string>) port)
  (display x port) #nil)

(define-method (pr-on (x <module>) port)
  (display (str-somehow x (pr-str (module-name->ns-str (module-name x)))) port)
  #nil)

(define-method (print-on (x <module>) port)
  (display (module-name->ns-str (module-name x)) port)
  #nil)


;; GOOPS doesn't define <variable>
(define var-class (class-of (module-variable (current-module) 'define)))

(define-inlinable (var-desc v)
  (read-only-str
   (%scm-format #f "#object[<variable> 0x~x]" (object-address v))))

(define-method (pr-str (x var-class))
  (var-desc x))

(define-method (pr-on (x var-class) port)
  (display (var-desc x) port)
  #nil)

(define-method (print-on (x var-class) port)
  (display (var-desc x) port)
  #nil)


(define-method (pr-on (x <syntax>) port)
  (write x port) #nil)


(define (show-all items emit port)
  (if (pair? items)
    (let ((x (car items))
          (more (cdr items)))
      (emit x port)
      (let loop ((rest more))
        (when (pair? rest)
          (display #\space port)
          (emit (car rest) port)
          (loop (cdr rest)))
        #nil))))

(define (pr . items)
  (show-all items pr-on *out*))

(define (print . items)
  (show-all items print-on *out*))

(define (printf fmt . args)
  (print (apply format fmt args)))


(define (prn . items)
  (apply pr items) (newline *out*) #nil)

(define (println . items)
  (apply print items) (newline *out*) #nil)


(define-method (pr-str . args)
  (with-out-str (apply pr args)))

(define (print-str . args)
  (with-out-str (apply print args)))

(define (str . args)
  (string-concatenate (map print-str args)))
