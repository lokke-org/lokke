;;; Copyright (C) 2015-2023 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

(define-module (lokke core)
  #:use-module ((guile)
                #:select ((apply . %scm-apply)
                          (begin . %scm-begin)
                          (if . %scm-if)
                          (let . %scm-let)
                          (format . %scm-format)))
  #:use-module ((ice-9 match) #:select (match-lambda*))
  #:use-module (oop goops)
  #:use-module ((srfi srfi-1) #:select (iota))
  #:use-module ((lokke array)
                #:select (aclone
                          aget
                          aget-boolean
                          aget-byte
                          aget-double
                          aget-float
                          aget-int
                          aget-long
                          aget-short
                          alength
                          amap
                          areduce
                          aset
                          aset-boolean
                          aset-byte
                          aset-double
                          aset-float
                          aset-int
                          aset-long
                          aset-short
                          boolean-array
                          byte-array
                          double-array
                          float-array
                          int-array
                          long-array
                          short-array))
  #:use-module ((lokke base collection) #:select (define-nth-seq))
  #:use-module ((lokke base doc) #:select (doc))
  #:use-module ((lokke base dynamic) #:select (set!))
  #:use-module ((lokke base map)
                #:select (<map> map-invert map? update-keys update-vals))
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
  #:use-module ((lokke base util) #:select (map-tag? set-tag? vec-tag?))
  #:use-module ((lokke base version) #:prefix ver/)
  #:use-module ((lokke base quote) #:select (clj-quote))
  #:use-module (lokke collection)
  #:use-module ((lokke compare) #:select (== clj= compare hash))
  #:use-module ((lokke compat) #:select (re-export-and-replace!))
  #:use-module ((lokke compile) #:select (clj-defmacro load-file))
  #:use-module ((lokke concurrent)
                #:select (<atom>
                          atom
                          atom?
                          add-watch
                          compare-and-set!
                          deliver
                          deref
                          future
                          future-call
                          promise
                          remove-watch
                          reset!
                          reset-vals!
                          set-validator!
                          swap!
                          swap-vals!))
  #:use-module ((lokke datatype)
                #:select (defprotocol defrecord extend-type))
  #:use-module ((lokke exception)
                #:select (AssertionError
                          AssertionError.
                          Error
                          Error.
                          Exception
                          Exception.
                          ExceptionInfo
                          Throwable
                          Throwable.
                          assert
                          close
                          ex-cause
                          ex-data
                          ex-info
                          ex-message
                          new
                          throw
                          try
                          with-open))
  #:use-module ((lokke hash-map) #:select (hash-map hash-map?))
  #:use-module (lokke hash-set)
  #:use-module ((lokke ns)
                #:select (*ns*
                          alias
                          find-ns
                          find-var
                          in-ns
                          ns
                          ns-aliases
                          ns-name
                          refer
                          refer-clojure
                          require
                          use))
  #:use-module ((lokke base invoke) #:select (invoke))
  #:use-module ((lokke io) #:select (flush line-seq read-line slurp spit))
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
                          with-in-str
                          with-out-str))
  #:use-module ((lokke reader) #:select (read read-string string->float))
  #:use-module ((lokke set) #:select (<set> set?))
  #:use-module ((lokke time) #:select (instant?))
  #:use-module ((lokke uuid) #:select (parse-uuid random-uuid uuid?))
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
                          ratio?
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
  #:export ((do . %scm-do)
            *clojure-version*
            *command-line-args* ;; this is wrong...
            *file*
            *warn-on-reflection*
            boolean
            byte
            complement
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
            parse-boolean
            parse-double
            parse-long
            short
            time
            trampoline)
  #:replace (= boolean? case do instance?)
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
               AssertionError
               AssertionError.
               Error
               Error.
               Exception
               Exception.
               ExceptionInfo
               Throwable
               Throwable.
               abs
               aclone
               add-watch
               aget
               aget-boolean
               aget-byte
               aget-double
               aget-float
               aget-int
               aget-long
               aget-short
               alength
               alias
               alter-meta!
               amap
               and
               areduce
               as->
               aset
               aset-boolean
               aset-byte
               aset-double
               aset-float
               aset-int
               aset-long
               aset-short
               assert
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
               boolean-array
               bounded-count
               butlast
               byte-array
               char?
               (class-of . class)
               (ver/version . clojure-version)
               compare-and-set!
               (clj-cond . cond)
               cond->
               cond->>
               (clj-defmacro . defmacro)
               close
               coll?
               comment
               comp
               compare
               concat
               condp
               conj
               constantly
               contains?
               count
               counted?
               cycle
               dec
               dec'
               declare
               def
               def-
               defdyn
               defdynloc
               defn
               defn-
               defprotocol
               defrecord
               deliver
               deref
               disj
               dissoc
               doall
               doc
               dorun
               doseq
               dotimes
               doto
               double
               double-array
               double?
               drop
               drop-last
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
               extend-type
               false?
               ffirst
               filterv
               find
               find-ns
               find-var
               first
               float
               float-array
               float?
               flush
               fn
               (procedure? . fn?)
               fnext
               for
               future
               future-call
               gensym
               get
               get-in
               hash
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
               (inf? . infinite?)
               (instant? . inst?)
               int-array
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
               long-array
               loop
               macroexpand
               make-array
               map-invert
               map-entry
               map-entry?
               map?
               mapv
               max
               meta
               min
               mod
               name
               namespace
               (nan? . NaN?)
               nat-int?
               neg?
               neg-int?
               new
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
               number?
               odd?
               or
               parse-uuid
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
               random-uuid
               range
               ratio?
               rational?
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
               reductions
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
               set?
               short-array
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
               update-keys
               update-vals
               use
               uuid?
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
               with-in-str
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
                        '(clj-quote . quote)
                        'read
                        'set!
                        'sort)

(defdyn *clojure-version*
  (hash-map #:major ver/major
            #:minor ver/minor
            #:increment ver/increment
            #:qualifier ver/qualifier))

(defdyn *warn-on-reflection* #f)

(define (boolean? x)
  (or (eq? x #t) (eq? x #f)))

(define (boolean x)
  (not (or (eq? x #f) (eq? x #nil))))

(define (complement f)
  (lambda args (not (apply f args))))

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

(define-syntax case
  (lambda (x)
    (syntax-case x ()
      ((_ target () action exp ...) #'(case target exp ...))

      ;; Literal adapter, so expressions like (quote x) and
      ;; (/lokke/reader-vector #nil 1) aren't taken as multiple
      ;; symbol selectors by (candidate ...)  below.
      ((_ target (tag meta x ...) exp ...)
       (or (eq? 'quote (syntax->datum #'tag))
           (vec-tag? #'tag) (map-tag? #'tag) (set-tag? #'tag))
       #'(case target ((tag meta x ...)) exp ...))

      ((_ target-exp (candidate candidate* ...) action exp ...)
       #'(%scm-let ((target target-exp))
           (%scm-if (clj= (clj-quote candidate) target)
                    action
                    (case target (candidate* ...) action exp ...))))

      ((_ target candidate action exp ...)
       #'(case target (candidate) action exp ...))

      ((_ target action) #'action)
      ((_ target) #'(error "No matching clause for" target)))))

(define (parse-boolean s)
  (cond
   ((string=? s "true") #t)
   ((string=? s "false") #f)
   (else #nil)))

(define (parse-double s)
  (%scm-let ((s (string-trim-both s)))
    (cond
     ((string=? s "NaN") +nan.0)
     ((string=? s "+NaN") +nan.0)
     ((string=? s "-NaN") +nan.0)
     ((string=? s "Infinity") +inf.0)
     ((string=? s "+Infinity") +inf.0)
     ((string=? s "-Infinity") -inf.0)
     (else
      (or (string->float s)
          #nil)))))

(define (parse-long s)
  (let (n (string->number s))
    (when (exact-integer? n)
        n)))
