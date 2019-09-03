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

(read-set! keywords 'postfix)  ;; srfi-88

(define-module (lokke core)
  use-module: ((guile)
               select: ((apply . %scm-apply)
                        (begin . %scm-begin)
                        (let . %scm-let)))
  use-module: ((ice-9 match) select: (match-lambda*))
  use-module: (oop goops)
  use-module: ((srfi srfi-1) select: (drop-right iota last))
  use-module: ((srfi srfi-67) select: (boolean-compare
                                       char-compare
                                       number-compare
                                       (string-compare . string-compare-67)
                                       symbol-compare))
  use-module: ((lokke base syntax)
               select: (and
                        (cond . clj-cond)
                        def
                        defn
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
  use-module: (lokke collection)
  use-module: ((lokke compile)
               select: (clj-defmacro
                        expand-symbol
                        expand-symbols
                        load-file))
  use-module: ((lokke concurrent)
               select: (<atom>
                        atom?
                        add-watch
                        remove-watch
                        deref
                        reset!
                        set-validator!
                        swap!))
  use-module: ((lokke map) select: (select-keys))
  use-module: (lokke hash-map)
  use-module: (lokke hash-set)
  use-module: ((lokke map-entry)
               select: (<map-entry> key map-entry map-entry? val))
  use-module: ((lokke ns)
               select: (*ns*
                        find-ns
                        in-ns
                        ns
                        ns-name
                        refer
                        refer-clojure
                        require
                        use))
  use-module: ((lokke invoke) select: (invoke))
  use-module: ((lokke pr)
               select: (format pr pr-str print print-str println prn str))
  use-module: (lokke vector)
  use-module: ((lokke metadata)
               select: (*print-meta* meta set-meta! vary-meta with-meta))
  use-module: ((lokke scm core)
               ;; Notably not str
               select: (+'
                        -'
                        -'
                        ->
                        ->>
                        bit-and
                        bit-not
                        bit-or
                        bit-xor
                        comment
                        comp
                        complement
                        constantly
                        dec
                        dec'
                        doto
                        false?
                        float?
                        identical?
                        inc
                        inc'
                        juxt
                        mod
                        neg?
                        partial
                        pos?
                        quot
                        rand
                        rand-int
                        subs
                        true?))
  use-module: ((lokke symbol)
               select: (gensym ident? keyword name namespace symbol))
  use-module: ((system base compile)
               select: (compile-file compiled-file-name))
  export: (*command-line-args*  ;; this is wrong...
           *err*
           *out*
           ==
           byte
           compare
           (do . %scm-do)
           doc
           distinct?
           instance?
           int
           integer
           long
           map?
           not=
           num
           short
           some?)
  replace: (apply instance? nil? do)
  re-export: (*
              *ns*
              *print-meta*
              +
              -
              ->
              /
              <atom>
              <coll>
              <map-entry>
              <seq>
              =
              add-watch
              and
              assoc
              atom?
              (begin . %scm-begin)
              (begin . do)
              bit-and
              bit-not
              bit-or
              bit-xor
              boolean?
              char?
              (class-of . class)
              (clj-cond . cond)
              (clj-defmacro . defmacro)
              coll?
              comment
              comp
              complement
              conj
              cons
              constantly
              contains?
              count
              counted?
              dec
              dec'
              def
              defn
              deref
              dissoc
              doall
              dorun
              dotimes
              doto
              drop
              empty
              empty?
              eval-when
              even?
              every?
              false?
              ffirst
              find
              find-ns
              first
              float?
              fn
              fnext
              format
              gensym
              get
              get-in
              ident?
              identical?
              identity
              if
              if-let
              if-not
              in-ns
              inc
              inc'
              into
              invoke
              key
              keys
              keyword
              keyword?
              lazy-seq
              let
              letfn
              list
              load-file
              loop
              macroexpand
              map-entry
              map-entry?
              max
              merge
              meta
              min
              mod
              name
              namespace
              neg?
              next
              nfirst
              nnext
              not
              not-any?
              not-every?
              ns
              ns-name
              nth
              or
              partial
              pos?
              pr
              pr-str
              print
              print-str
              println
              prn
              quot
              quote
              rand
              rand-int
              reduce
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
              set-meta!
              set-validator!
              some
              str
              string?
              subs
              swap!
              symbol
              symbol?
              take
              true?
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
              with-meta
              zero?)
  duplicates: (merge-generics replace warn-override-core warn last))

(define (apply f . args)
  ;; FIXME: tolerable?
  (if (null? args)
      (f)
      (let (final (last args))
        (if (list? final)
            (%scm-apply f (append (drop-right args 1)
                                  final))
            (%scm-apply f (append (drop-right args 1)
                                  (seq->scm-list final)))))))

;; FIXME: docstrings

(define-generic =)
(define-method (= x y) (equal? x y))

;; (define-syntax *err*
;;   (make-variable-transformer
;;    (lambda (x)
;;      (syntax-case x ()
;;        (_ #'(current-error-port))))))

;; (define-syntax *out*
;;   (make-variable-transformer
;;    (lambda (x)
;;      (syntax-case x ()
;;        (_ #'(current-output-port))))))

;; FIXME: wrong
(define *err* (current-error-port))
(define *out* (current-output-port))

(define *command-line-args* (cdr (program-arguments)))

(define-method (map? x) #f)
(define-method (map? (x <hash-map>)) #t)

(define == =)

(define (nil? x) (eq? #nil x))
(define some? (complement nil?))

(define not= (comp not =))

(define (instance? c x) (is-a? x c))

;; compare mostly follows srfi-67, except for '(), pair, and vector,
;; which are treated as sequences.  Also currently more flexible than
;; clojure/jvm with respect to collections.  i.e. (compare [1 2 3] '(1
;; 2 3)) works.

;; compare results are compatible with SRFI-67 #{-1 0 1}, stricter
;; than Clojure's

(define (seq-compare x y)
  (if (identical? x y)
      0
      (let (x (seq x)
            y (seq y))
        (cond
         ((and (not x) (not y)) 0)
         ((not x) -1)
         ((not y) 1)
         (else (let (c (compare (first x) (first y)))
                 (if (zero? c)
                     (seq-compare (next x) (next y))
                     c)))))))

(define-method (compare (x <boolean>) y) -1)
(define-method (compare (x <char>) y) -1)
(define-method (compare (x <string>) y) -1)
(define-method (compare (x <symbol>) y) -1)
(define-method (compare (x <number>) y) -1)
(define-method (compare (x <null>) y) -1)
(define-method (compare (x <pair>) y) -1)
(define-method (compare (x <vector>) y) -1)

(define-method (compare (x <boolean>) (y <boolean>)) (boolean-compare x y))

(define-method (compare (x <char>) (y <boolean>)) 1)
(define-method (compare (x <char>) (y <char>)) (char-compare x y))

(define-method (compare (x <string>) (y <boolean>)) 1)
(define-method (compare (x <string>) (y <char>)) 1)
(define-method (compare (x <string>) (y <string>)) (string-compare-67 x y))

(define-method (compare (x <symbol>) (y <boolean>)) 1)
(define-method (compare (x <symbol>) (y <char>)) 1)
(define-method (compare (x <symbol>) (y <string>)) 1)
(define-method (compare (x <symbol>) (y <symbol>)) (symbol-compare x y))

(define-method (compare (x <number>) (y <boolean>)) 1)
(define-method (compare (x <number>) (y <char>)) 1)
(define-method (compare (x <number>) (y <string>)) 1)
(define-method (compare (x <number>) (y <symbol>)) 1)
(define-method (compare (x <number>) (y <number>)) (number-compare x y))

(define-method (compare (x <null>) (y <boolean>)) 1)
(define-method (compare (x <null>) (y <char>)) 1)
(define-method (compare (x <null>) (y <string>)) 1)
(define-method (compare (x <null>) (y <symbol>)) 1)
(define-method (compare (x <null>) (y <number>)) 1)
(define-method (compare (x <null>) (y <null>)) 0)
(define-method (compare (x <null>) (y <pair>)) (seq-compare x y))
(define-method (compare (x <null>) (y <vector>)) (seq-compare x y))

(define-method (compare (x <pair>) (y <boolean>)) 1)
(define-method (compare (x <pair>) (y <char>)) 1)
(define-method (compare (x <pair>) (y <string>)) 1)
(define-method (compare (x <pair>) (y <symbol>)) 1)
(define-method (compare (x <pair>) (y <number>)) 1)
(define-method (compare (x <pair>) (y <null>)) (seq-compare x y))
(define-method (compare (x <pair>) (y <pair>)) (seq-compare x y))
(define-method (compare (x <pair>) (y <vector>)) (seq-compare x y))

(define-method (compare (x <vector>) (y <boolean>)) 1)
(define-method (compare (x <vector>) (y <char>)) 1)
(define-method (compare (x <vector>) (y <string>)) 1)
(define-method (compare (x <vector>) (y <symbol>)) 1)
(define-method (compare (x <vector>) (y <number>)) 1)
(define-method (compare (x <vector>) (y <null>)) (seq-compare x y))
(define-method (compare (x <vector>) (y <pair>)) (seq-compare x y))
(define-method (compare (x <vector>) (y <vector>)) (seq-compare x y))

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

(define-method (doc (x <procedure>))
  (let (s (procedure-documentation x))
    (when s
      (display "-------------------------\n")
      (display s)
      (newline))))
