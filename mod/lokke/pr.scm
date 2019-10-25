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

(read-set! keywords 'postfix)  ;; srfi-88

;; FIXME: consider falling back to write/display (customized via
;; GOOPS) when the reps are the same.

;; FIXME: check scheme write vs clojure more carefully

;; FIXME: *out* *err*, etc.

(define-module (lokke pr)
  use-module: ((ice-9 format) select: ((format . %scm-format)))
  use-module: ((language tree-il) prefix: tree-il/)
  use-module: (oop goops)
  use-module: ((srfi srfi-88) select: (keyword->string))
  replace: (format)
  export: (*out* pr pr-str print print-str prn println str))

;; FIXME: just using (*out*) to it'll be easy to find/fix later.
(define (*out*) (current-error-port))

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

(define (format . args) (apply %scm-format #f args))

(define (read-only-str s) (substring/read-only s 0))

(define (str-somehow x details)
  (if (eq? x *unspecified*)
      ""
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
        "]"))))

;; No args
(define-method (pr-str) "")
(define-method (pr) #nil)
(define-method (prn) #nil)
(define-method (print-str) "")
(define-method (print) #nil)
(define-method (println) #nil)

;; Single arg (fallback)
(define-method (pr-str x) (str-somehow x #f))
(define-method (print-str x) (str-somehow x #f))
(define-method (pr x) (display (pr-str x) (*out*)))
(define-method (print x) (display (print-str x) (*out*)))

;; Multiple items

(define-method (pr-str x y . rest)
  (string-join (map pr-str (cons x (cons y rest))) " "))

(define-method (print-str x y . rest)
  (string-join (map print-str (cons x (cons y rest))) " "))

(define-method (pr x y . rest)
  (pr x)
  (for-each (lambda (z) (display " " (*out*)) (pr z))
            (cons y rest)))

(define-method (print x y . rest)
  (print x)
  (for-each (lambda (z) (display " " (*out*)) (print z))
            (cons y rest)))

(define (prn . items)
  (apply pr items)
  (newline (*out*)))

(define (println . items)
  (apply print items)
  (newline (*out*)))

(define (str . args)
  (string-concatenate (map print-str args)))

(define-method (pr-str (x <class>)) (class-name x))
(define-method (print-str (x <class>)) (class-name x))
(define-method (pr (x <class>)) (display (class-name x) (*out*)))
(define-method (print (x <class>)) (display (class-name x) (*out*)))

(define (boolean->string x)
  (case x
    ((#t) "true")
    ((#f) "false")
    ((#nil) "nil")
    (else (error "impossible boolean" x))))

(define-method (pr-str (x <null>)) "()")
(define-method (print-str (x <null>)) "()")
(define-method (pr (x <null>)) (display "()" (*out*)))
(define-method (print (x <null>)) (display "()" (*out*)))

(define-method (pr-str (x <boolean>)) (boolean->string x))
(define-method (print-str (x <boolean>)) (boolean->string x))
(define-method (pr (x <boolean>)) (display (boolean->string x) (*out*)))
(define-method (print (x <boolean>)) (display (boolean->string x) (*out*)))

(define-method (pr-str (x <number>)) (number->string x))
(define-method (print-str (x <number>)) (number->string x))
(define-method (pr (x <number>)) (write x (*out*)))
(define-method (print (x <number>)) (display x (*out*)))

;; ;; FIXME: symbols...
(define-method (pr-str (x <symbol>)) (symbol->string x))
(define-method (print-str (x <symbol>)) (symbol->string x))
(define-method (pr (x <symbol>)) (write x (*out*)))
(define-method (print (x <symbol>)) (display x (*out*)))

;; FIXME: keywords...
(define-method (pr-str (x <keyword>))
  (string-append ":" (keyword->string x)))

(define-method (print-str (x <keyword>))
  (string-append ":" (keyword->string x)))

(define-method (pr (x <keyword>))
  (display #\: (*out*))
  (display (keyword->string x) (*out*)))

(define-method (print (x <keyword>))
  (display #\: (*out*))
  (display (keyword->string x) (*out*)))

(define-method (pr-str (x <char>)) (string #\\ x))
(define-method (print-str (x <char>)) (string x))
(define-method (pr (x <char>)) (display (pr-str x) (*out*)))
(define-method (print (x <char>)) (display x (*out*)))

(define-method (pr-str (x <string>)) (with-output-to-string (lambda () (write x))))
(define-method (print-str (x <string>)) x)
(define-method (pr (x <string>)) (write x (*out*)))
(define-method (print (x <string>)) (display x (*out*)))


(define-method (pr-str (x <syntax>)) (with-output-to-string (lambda () (write x))))
(define-method (print-str (x <syntax>)) (with-output-to-string (lambda () (write x))))
(define-method (pr (x <syntax>)) (write x (*out*)))
(define-method (print (x <syntax>)) (display x (*out*)))

(define-method (pr-str (x <module>)) (str-somehow x (module-name x)))
(define-method (print-str (x <module>)) (str-somehow x (module-name x)))
