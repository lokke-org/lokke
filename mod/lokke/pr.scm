;;; Copyright (C) 2019-2020 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

(define-module (lokke pr)
  #:use-module ((guile) #:select ((newline . %scm-newline)))
  #:use-module ((ice-9 match) #:select (match-lambda*))
  #:use-module ((ice-9 format) #:select ((format . %scm-format)))
  #:use-module ((language tree-il) #:prefix tree-il/)
  #:use-module ((lokke base dynamic) #:select (binding defdyn))
  #:use-module ((lokke base util)
                #:select (keyword->string
                          module-name->ns-str
                          module-name->ns-sym))
  #:use-module (oop goops)
  #:replace (format newline)
  #:export (*err*
            *in*
            *out*
            *print-readably*
            pr
            pr-approachable
            pr-readable
            pr-str
            print
            printf
            print-str
            println
            prn
            str
            to-string
            with-in-str
            with-out-str)
  #:duplicates (merge-generics replace warn-override-core warn last))

(defdyn *in* (current-input-port))
(defdyn *out* (current-output-port))
(defdyn *err* (current-error-port))
(defdyn *print-readably* #t)

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

(define-syntax-rule (with-in-str s e ...)
  (call-with-input-string s
    (lambda (port)
      (binding (*in* port) e ...))))

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

(define-method (pr-readable x port)
  (display (str-somehow x #f) port) #nil)

(define-method (pr-approachable x port)
  (pr-readable x port) #nil)


(define-method (pr-readable (x <class>) port)
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

(define-method (pr-readable (x <boolean>) port)
  (show-boolean x port))

(define-method (pr-approachable (x <boolean>) port)
  (show-boolean x port))


(define-inlinable (show-null x port)
  (display "()" port) #nil)

(define-method (pr-readable (x <null>) port)
  (show-null x port))

(define-method (pr-approachable (x <null>) port)
  (show-null x port))


(define-inlinable (show-num x port)
  (display x port) #nil)

(define-method (pr-readable (x <number>) port)
  (show-num x port))

(define-method (pr-approachable (x <number>) port)
  (show-num x port))


;; FIXME: ensure correct clj format
(define-inlinable (show-sym x port)
  (display (symbol->string x) port) #nil)

(define-method (pr-readable (x <symbol>) port)
  (show-sym x port))

(define-method (pr-approachable (x <symbol>) port)
  (show-sym x port))


;; FIXME: ensure correct clj format
(define-inlinable (show-keyword x port)
  (display #\: port)
  (display (keyword->string x) port)
  #nil)

(define-method (pr-readable (x <keyword>) port)
  (show-keyword x port))

(define-method (pr-approachable (x <keyword>) port)
  (show-keyword x port))


;; FIXME: ensure correct clj format
(define-method (pr-readable (c <char>) port)
  (case c
    ((#\newline) (display "\\newline" port))
    ((#\space) (display "\\space" port))
    ((#\tab) (display "\\tab" port))
    ((#\page) (display "\\formfeed" port))
    ((#\backspace) (display "\\backspace" port))
    ((#\return) (display "\\return" port))
    ;; FIXME: adjust to match the jvm (or edn) more precisely
    (else (display (string #\\ c) port))))

(define-method (pr-approachable (x <char>) port)
  (display x port) #nil)


;; FIXME: ensure correct clj format
(define-method (pr-readable (x <string>) port)
  (write x port) #nil)

(define-method (pr-approachable (x <string>) port)
  (display x port) #nil)

(define-method (pr-readable (x <module>) port)
  (display (str-somehow x (pr-str (module-name->ns-str (module-name x)))) port)
  #nil)

(define-method (pr-approachable (x <module>) port)
  (display (module-name->ns-str (module-name x)) port)
  #nil)


;; GOOPS doesn't define <variable>
(define var-class (class-of (module-variable (current-module) 'define)))

(define-inlinable (var-desc v)
  (read-only-str
   (%scm-format #f "#object[<variable> 0x~x]" (object-address v))))

(define-method (pr-str (x var-class))
  (var-desc x))

(define-method (pr-readable (x var-class) port)
  (display (var-desc x) port)
  #nil)

(define-method (pr-approachable (x var-class) port)
  (display (var-desc x) port)
  #nil)

(define (newline)
  (%scm-newline *out*))

(define-method (pr-readable (x <syntax>) port)
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
  (if *print-readably*
      (show-all items pr-readable *out*)
      (show-all items pr-approachable *out*)))

(define (print . items)
  (show-all items pr-approachable *out*))

(define (printf fmt . args)
  (print (apply format fmt args)))

(define (prn . items)
  (apply pr items) (%scm-newline *out*) #nil)

(define (println . items)
  (apply print items) (%scm-newline *out*) #nil)


(define-method (pr-str . args)
  (with-out-str (apply pr args)))

(define (print-str . args)
  (with-out-str (apply print args)))

(define-method (to-string x)
  (with-out-str (print x)))

(define str
  (match-lambda*
    (() "")
    ((x) (to-string x))
    (xs (string-concatenate (map to-string xs)))))
