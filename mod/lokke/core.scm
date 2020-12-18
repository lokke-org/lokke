;;; Copyright (C) 2015-2020 Rob Browning <rlb@defaultvalue.org>
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

(define-module (lokke core)
  #:use-module ((guile)
                #:select ((apply . %scm-apply)
                          (begin . %scm-begin)
                          (let . %scm-let)
                          (format . %scm-format)))
  #:use-module ((ice-9 match) #:select (match-lambda*))
  #:use-module (oop goops)
  #:use-module ((srfi srfi-1) #:select (iota))
  #:use-module ((srfi srfi-43) #:select (vector-unfold))
  #:use-module ((lokke base doc) #:select (doc))
  #:use-module ((lokke base map) #:select (<map>))
  #:use-module ((lokke base syntax)
                #:select (->
                          ->>
                          as->
                          and
                          binding
                          (cond . clj-cond)
                          cond->
                          cond->>
                          condp
                          declare
                          def
                          def-
                          defn
                          defn-
                          defdyn
                          defdynloc
                          doseq
                          dotimes
                          fn
                          for
                          if
                          if-let
                          if-not
                          if-some
                          let
                          letfn
                          loop
                          nil?
                          or
                          some->
                          some->>
                          some?
                          var
                          when
                          when-first
                          when-let
                          when-not
                          when-some))
  #:use-module ((lokke boot) #:select (quote))
  #:use-module ((lokke base collection) #:select (define-nth-seq))
  #:use-module (lokke collection)
  #:use-module ((lokke compare) #:select (== clj= compare))
  #:use-module ((lokke compat) #:select (re-export-and-replace!))
  #:use-module ((lokke compile) #:select (clj-defmacro load-file))
  #:use-module ((lokke concurrent)
                #:select (<atom>
                          atom
                          atom?
                          add-watch
                          deliver
                          deref
                          future
                          promise
                          remove-watch
                          reset!
                          reset-vals!
                          set-validator!
                          swap!
                          swap-vals!))
  #:use-module ((lokke exception)
                #:select (Exception
                          ExceptionInfo
                          Throwable
                          close
                          ex-cause
                          ex-data
                          ex-info
                          ex-message
                          throw
                          try
                          with-open))
  #:use-module ((lokke hash-map) #:select (hash-map hash-map?))
  #:use-module (lokke hash-set)
  #:use-module ((lokke ns)
                #:select (*ns*
                          alias
                          find-ns
                          in-ns
                          ns
                          ns-aliases
                          ns-name
                          refer
                          refer-clojure
                          require
                          use))
  #:use-module ((lokke base invoke) #:select (invoke))
  #:use-module ((lokke ns lokke io)
                #:select (flush line-seq read-line slurp spit))
  #:use-module ((lokke pr)
                #:select (*err*
                          *in*
                          *out*
                          format
                          newline
                          pr
                          pr-str
                          print
                          printf
                          print-str
                          println
                          prn
                          str
                          with-out-str))
  #:use-module ((lokke reader) #:select (read read-string))
  #:use-module ((lokke set) #:select (<set>))
  #:use-module (lokke vector) ;; #:FIXME select
  #:use-module ((lokke metadata)
                #:select (*print-meta* alter-meta! meta vary-meta with-meta))
  #:use-module ((lokke regex)
                #:select (re-find
                          re-groups
                          re-matcher
                          re-matches
                          re-pattern
                          re-seq))
  #:use-module ((lokke scm bit)
                #:select (bit-and
                          bit-clear
                          bit-flip
                          bit-not
                          bit-or
                          bit-set
                          bit-test
                          bit-xor))
  #:use-module ((lokke scm core)
                ;; Notably not str
                #:select (+'
                          -'
                          -'
                          comment
                          comp
                          complement
                          constantly
                          dec
                          dec'
                          doto
                          double
                          double?
                          false?
                          float
                          float?
                          identical?
                          inc
                          inc'
                          int?
                          mod
                          neg?
                          nat-int?
                          neg-int?
                          partial
                          pos?
                          pos-int?
                          quot
                          rand
                          rand-int
                          rem
                          subs
                          true?))
  #:use-module ((lokke symbol)
                #:select (gensym ident? keyword name namespace symbol))
  #:use-module ((srfi srfi-19)
                #:select (current-time
                          time-difference
                          time-monotonic
                          time-nanosecond
                          time-second))
  #:use-module ((system base compile)
                #:select (compile-file compiled-file-name))
  #:export (*assert*
            *command-line-args* ;; this is wrong...
            *file*
            assert
            byte
            (do . %scm-do)
            distinct?
            fnil
            instance?
            int
            integer
            juxt
            long
            max-key
            min-key
            not=
            num
            short
            time
            trampoline)
  #:replace (= do instance?)
  #:re-export (*
               *err*
               *in*
               *ns*
               *out*
               *print-meta*
               +
               -
               ->
               ->>
               /
               <
               <=
               <atom>
               <coll>
               <map>
               <map-entry>
               <seq>
               <set>
               ==
               >
               >=
               Exception
               ExceptionInfo
               Throwable
               as->
               add-watch
               alias
               alter-meta!
               and
               assoc-in
               atom
               atom?
               (begin . %scm-begin)
               (begin . do)
               binding
               bit-and
               bit-clear
               bit-flip
               bit-not
               bit-or
               bit-set
               bit-test
               bit-xor
               boolean?
               bounded-count
               butlast
               char?
               (class-of . class)
               (clj-cond . cond)
               cond->
               cond->>
               (clj-defmacro . defmacro)
               close
               coll?
               comment
               comp
               compare
               complement
               concat
               condp
               conj
               constantly
               contains?
               count
               counted?
               dec
               dec'
               declare
               def
               def-
               defdyn
               defdynloc
               defn
               defn-
               deref
               disj
               dissoc
               deliver
               doall
               doc
               dorun
               doseq
               dotimes
               doto
               double
               double?
               drop
               drop-while
               empty
               empty?
               eval-when
               even?
               every?
               ex-cause
               ex-data
               ex-info
               ex-message
               false?
               ffirst
               filterv
               find
               find-ns
               first
               float
               float?
               flush
               fn
               (procedure? . fn?)
               fnext
               for
               future
               gensym
               get
               get-in
               hash-map
               hash-set
               ident?
               identical?
               identity
               if
               if-let
               if-not
               if-some
               in-ns
               inc
               inc'
               int?
               integer?
               interleave
               interpose
               into
               into-array
               invoke
               iterate
               key
               keys
               keyword
               keyword?
               last
               lazy-seq
               let
               letfn
               line-seq
               list*
               load-file
               loop
               macroexpand
               map?
               map-entry
               map-entry?
               mapv
               max
               meta
               min
               mod
               name
               namespace
               neg?
               nat-int?
               neg-int?
               next
               nfirst
               nnext
               not
               not-any?
               not-every?
               ns
               ns-aliases
               ns-name
               nth
               odd?
               or
               partial
               pop
               pos?
               pos-int?
               pr
               pr-str
               print
               printf
               print-str
               println
               prn
               promise
               quot
               rand
               rand-int
               random-sample
               range
               re-find
               re-groups
               re-matcher
               re-matches
               re-pattern
               re-seq
               read-line
               read-string
               reduce
               reduce-kv
               refer
               refer-clojure
               rem
               remove-watch
               repeat
               repeatedly
               require
               reset!
               reset-vals!
               rest
               reverse
               second
               select-keys
               seq
               seq->scm-list
               seq?
               seqable?
               sequential?
               set
               set-validator!
               shuffle
               slurp
               spit
               split-with
               some
               some->
               some->>
               some?
               str
               string?
               subs
               subvec
               swap!
               swap-vals!
               symbol
               symbol?
               take
               take-last
               take-nth
               take-while
               throw
               true?
               try
               update
               update-in
               use
               val
               vals
               var
               vary-meta
               vec
               vector
               vector?
               when
               when-first
               when-let
               when-not
               when-some
               with-open
               with-out-str
               with-meta
               zero?
               zipmap)
  #:duplicates (merge-generics replace warn-override-core warn last))

(re-export-and-replace! 'apply
                        'assoc
                        'cons
                        'format
                        'list
                        'list?
                        'merge
                        'newline
                        'nil?
                        'peek
                        'quote
                        'read
                        'sort)

(defdyn *assert* #t)

(define-syntax assert
  (syntax-rules ()
    ((_ x) (when-not x (error "Assertion failed:" (pr-str x))))
    ((_ x message)
     (when-not x (error (str "Assertion failed: " message "\n" (pr-str x)))))))

(define-syntax *file*
  (identifier-syntax (or (current-filename) #nil)))

(define = clj=)
(define (not= . args) (not (%scm-apply = args)))

(defdyn *command-line-args* (cdr (program-arguments)))

(define (instance? c x) (is-a? x c))

(define (as-num-type x name min max)
  (let (x (inexact->exact (truncate x)))
    (when (> x max)
      (scm-error 'out-of-range name "~a value ~a greater than ~a"
                 (list name x max) (list x)))
    (when (< x min)
      (scm-error 'out-of-range name "~a value ~a less than ~a"
                 (list name x min) (list x)))
    x))

(define-method (byte (c <char>))
  (as-num-type (char->integer c) "byte" -128 127))
(define-method (byte (c <integer>))
  (as-num-type c "byte" -128 127))
(define-method (byte (r <number>))
  (as-num-type (inexact->exact (truncate r)) "byte" -128 127))

(define-method (short (c <char>))
  (as-num-type (char->integer c) "short" -32768 32767))
(define-method (short (c <integer>))
  (as-num-type c "short" -32768 32767))
(define-method (short (r <number>))
  (as-num-type (inexact->exact (truncate r)) "short" -32768 32767))

(define-method (int (c <char>))
  (as-num-type (char->integer c) "int" -2147483648  2147483647))
(define-method (int (c <integer>))
  (as-num-type c "int" -2147483648  2147483647))
(define-method (int (r <number>))
  (as-num-type (inexact->exact (truncate r)) "int" -2147483648  2147483647))

(define-method (long (c <char>))
  (as-num-type (char->integer c) "long" -9223372036854775808 9223372036854775807))
(define-method (long (c <integer>))
  (as-num-type c "long" -9223372036854775808 9223372036854775807))
(define-method (long (r <number>))
  (as-num-type (inexact->exact (truncate r)) "long" -9223372036854775808 9223372036854775807))

(define-method (num (c <char>)) (char->integer c))
(define-method (num (n <number>)) n)

(define-method (integer (c <char>)) (char->integer c))
(define-method (integer (r <real>)) (inexact->exact (truncate r)))

(define bigint integer)
(define biginteger integer)

(define distinct?
  (match-lambda*
    ((x) #t)
    ((x ...)
     (%scm-let recur ((rst x)
                      (seen (hash-set)))
       (if (null? rst)
           #t
           (let (item (car rst))
             (if (contains? seen item)
                 #f
                 (recur (cdr rst) (conj seen item)))))))))

(define-method (count (s <string>))
  (string-length s))

(define-nth-seq <string-seq> string-length string-ref)

(define-method (seq (s <string>))
  (if (zero? (string-length s))
      #nil
      (make <string-seq> #:items s)))

(define (juxt f . fs)
  (lambda args
    (apply vector
           (apply f args)
           (map (lambda (f) (apply f args)) fs))))

(define-syntax-rule (time exp)
  (let* ((start (current-time time-monotonic))
         (result exp)
         (elapsed (time-difference (current-time time-monotonic) start)))
    (%scm-format *out* "\"Elapsed time: ~s msecs\"\n"
                 (+ (* 1000 (time-second elapsed))
                    (/ (time-nanosecond elapsed) 1000.0)))
    result))

(define (trampoline f . args)
  (%scm-let loop ((result (apply f args)))
    (if (procedure? result)
        (loop (result))
        result)))

(define-inlinable (extreme-key k order x xs)
  (%scm-let loop ((best-x x)
                  (best-k (k x))
                  (xs xs))
    (if (null? xs)
        best-x
        (let* ((x (first xs))
               (kx (k x)))
          (if (or (order kx best-k) (clj= kx best-k))
              (loop x kx (rest xs))
              (loop best-x best-k (rest xs)))))))

(define (min-key k x . xs) (extreme-key k < x xs))
(define (max-key k x . xs) (extreme-key k > x xs))

(define fnil
  (match-lambda*
    ((f x)
     (lambda (a . args)
       (apply f (if (nil? a) x a) args)))
    ((f x y)
     (lambda (a b . args)
       (apply f (if (nil? a) x a) (if (nil? b) y b) args)))
    ((f x y z)
     (lambda (a b c . args)
       (apply f (if (nil? a) x a) (if (nil? b) y b) (if (nil? c) z c) args)))
    ((f x ...)
     (lambda args
       (apply f (%scm-let loop ((args args)
                                (patches x))
                  (if (null? args)
                      (if (null? patches)
                          '()
                          ;; FIXME: fix function name handling
                          (scm-error 'wrong-number-of-args #f
                                     "Wrong number of arguments" '() #f))
                      (if (null? patches)
                          (scm-error 'wrong-number-of-args #f
                                     "Wrong number of arguments" '() #f)
                          (cons (if (nil? (car args)) (car patches) (car args))
                                (loop (cdr args) (cdr patches)))))))))))
