;;; Copyright (C) 2015-2019 Rob Browning <rlb@defaultvalue.org>
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
  #:use-module ((guile) #:select ((begin . %scm-begin) (let . %scm-let)))
  #:use-module ((ice-9 match) #:select (match-lambda*))
  #:use-module (oop goops)
  #:use-module ((srfi srfi-1) #:select (iota))
  #:use-module ((lokke base doc) #:select (doc))
  #:use-module ((lokke base syntax)
                #:select (and
                          binding
                          (cond . clj-cond)
                          declare
                          def
                          defn
                          defdyn
                          defdynloc
                          dotimes
                          fn
                          if
                          if-let
                          if-not
                          let
                          letfn
                          loop
                          or
                          when
                          when-let
                          when-not))
  #:use-module (lokke collection)
  #:use-module ((lokke compare) #:select (== clj= compare))
  #:use-module ((lokke compile) #:select (clj-defmacro load-file))
  #:use-module ((lokke concurrent)
                #:select (<atom>
                          atom
                          atom?
                          add-watch
                          remove-watch
                          deref
                          future
                          reset!
                          set-validator!
                          swap!))
  #:use-module ((lokke exception)
                #:select (ExceptionInfo
                          close
                          ex-cause
                          ex-data
                          ex-info
                          ex-message
                          throw
                          try
                          with-open))
  #:use-module (lokke hash-map)
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
  #:use-module ((lokke ns lokke io) #:select (line-seq read-line slurp spit))
  #:use-module ((lokke pr)
                #:select (*err*
                          *in*
                          *out*
                          format
                          pr
                          pr-str
                          print
                          printf
                          print-str
                          println
                          prn
                          str
                          with-out-str))
  #:use-module ((lokke set) #:select (<set>))
  #:use-module (lokke vector) ;; #:FIXME select
  #:use-module ((lokke metadata)
                #:select (*print-meta* meta set-meta! vary-meta with-meta))
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
                          ->
                          ->>
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
                          juxt
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
                          subs
                          true?))
  #:use-module ((lokke symbol)
                #:select (gensym ident? keyword name namespace symbol))
  #:use-module ((system base compile)
                #:select (compile-file compiled-file-name))
  #:export (*command-line-args* ;; this is wrong...
            byte
            (do . %scm-do)
            distinct?
            instance?
            int
            integer
            long
            not=
            num
            short
            some?)
  #:replace (= instance? nil? do)
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
               <atom>
               <coll>
               <map>
               <map-entry>
               <seq>
               <set>
               ==
               >
               ExceptionInfo
               add-watch
               alias
               and
               apply
               assoc
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
               char?
               (class-of . class)
               (clj-cond . cond)
               (clj-defmacro . defmacro)
               close
               coll?
               comment
               comp
               compare
               complement
               conj
               cons
               constantly
               contains?
               count
               counted?
               dec
               dec'
               declare
               def
               defdyn
               defdynloc
               defn
               deref
               disj
               dissoc
               doall
               doc
               dorun
               dotimes
               doto
               double
               double?
               drop
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
               fn
               fnext
               format
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
               in-ns
               inc
               inc'
               int?
               into
               invoke
               key
               keys
               keyword
               keyword?
               lazy-seq
               let
               letfn
               line-seq
               list
               load-file
               loop
               macroexpand
               map?
               map-entry
               map-entry?
               mapv
               max
               merge
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
               pos?
               pos-int?
               pr
               pr-str
               print
               printf
               print-str
               println
               prn
               quot
               quote
               rand
               rand-int
               read-line
               reduce
               reduce-kv
               refer
               refer-clojure
               remove-watch
               repeat
               repeatedly
               require
               reset!
               rest
               second
               select-keys
               seq
               seq->scm-list
               seq?
               seqable?
               sequential?
               set
               set-meta!
               set-validator!
               slurp
               spit
               some
               str
               string?
               subs
               swap!
               symbol
               symbol?
               take
               take-while
               throw
               true?
               try
               update
               use
               val
               vals
               vary-meta
               vec
               vector
               vector?
               when
               when-let
               when-not
               with-open
               with-out-str
               with-meta
               zero?)
  #:duplicates (merge-generics replace warn-override-core warn last))

(define = clj=)
(define (not= . args) (not (apply = args)))

(define *command-line-args* (cdr (program-arguments)))

(define (nil? x) (eq? #nil x))
(define some? (complement nil?))

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
