;;; Copyright (C) 2019 Rob Browning <rlb@defaultvalue.org>
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

(define-module (lokke reader)
  use-module: ((ice-9 pretty-print) select: (pretty-print))
  use-module: ((ice-9 receive) select: (receive))
  use-module: ((lokke base util) select: (pairify))
  use-module: ((lokke collection) select: (empty? merge))
  use-module: ((lokke compile)
               select: (literals->clj-instances preserve-meta-if-new!))
  use-module: ((lokke hash-map) select: (assoc get hash-map hash-map?))
  use-module: ((lokke hash-set) select: (hash-set))
  use-module: ((lokke metadata) select: (with-meta set-meta!))
  use-module: (oop goops)
  use-module: ((srfi srfi-1) select: (concatenate iota fold))
  export: (read read-for-compiler read-string read-string-for-compiler)
  duplicates: (merge-generics replace warn-override-core warn last))

;; FIXME: perhaps we can improve some of the tree traversals via walk
;; function.

;; FIXME: metadata is currently broken.

(define debug-reader? #f)
(define debug-conditionals? (or debug-reader? #f))

;; Avoid undefined warnings
(define read-primitively #f)
(load-extension "lokke-reader.so" "init_lokke_reader")

(define (rewrite-anon-fns expr)
  ;; We don't have to handle literal collections here because we
  ;; expand them later.

  (define anon-arg?
    ;; Clojure/JVM at least ignores %0 and does recognize %04, etc.
    (let* ((int-or-rest (make-regexp "^%([0-9]+|&)?$"))
           (zero (make-regexp "^%0+$")))
      (lambda (s)
        (let* ((s (symbol->string s)))
          (and (regexp-exec int-or-rest s)
               (not (regexp-exec zero s)))))))

  (define (arg-pos sym)
    (case sym
      ((%) 1)
      ((%&) sym)
      (else (string->number (substring/read-only (symbol->string sym) 1)))))

  (define (make-binding pos)
    (cond
     ((integer? pos) (gensym (format #f "fn%~a_" pos)))
     ((eq? '%& pos) (gensym "fn%&_"))
     (else (error "Unexpected #() argument position type:" pos))))

  (define (rewrite-anon-fn top)
    (let* ((pos-syms (make-hash-table 31))
           (max-pos 0))

      (define (expand-anon-args expr)
        (let* ((argsyms (make-hash-table 31)))
          (cond
           ((keyword? expr) expr)
           ((symbol? expr)
            (if (not (anon-arg? expr))
                expr
                (let* ((pos (arg-pos expr))
                       (existing (hashv-ref pos-syms pos)))
                  (or existing
                      (let* ((sym (make-binding pos)))
                        (if (and (integer? pos) (> pos max-pos))
                            (set! max-pos pos))
                        (hashv-set! pos-syms pos sym)
                        sym)))))
           ((string? expr) expr)
           ((number? expr) expr)
           ((boolean? expr) expr)
           ((char? expr) expr)
           ((null? expr) expr)
           ((list? expr)
            (if (eq? '/lokke/reader-anon-fn (car expr))
                ;; FIXME: recursively (/lokke/reader-anon-fn ...) as
                ;; #() for error
                (error (format #f "Encountered nested #() form: #~s" top))
                (map expand-anon-args expr)))
           (else
            (error (format #f "Unexpected type ~s for ~s in #() form:"
                           (class-of expr) expr)
                   top)))))

      (let* ((expanded (expand-anon-args top))
             (arglist (map (lambda (i)
                             (or (hashv-ref pos-syms i)
                                 (make-binding i)))
                           (iota max-pos 1)))
             (arglist (let* ((rst (hashv-ref pos-syms '%&)))
                        (if rst
                            (append arglist (list '& rst))
                            arglist))))
        `(fn (/lokke/reader-vector ,@arglist) ,expanded))))

  (preserve-meta-if-new!  ;; currently unnecessary
   expr
   (cond
    ((symbol? expr) expr)
    ((null? expr) expr)
    ((list? expr)
     (if (eq? '/lokke/reader-anon-fn (car expr))
         (begin
           (unless (= 2 (length expr))
             (error "#() expansion should have only one item:" (cdr expr)))
           (rewrite-anon-fn (cadr expr)))
         (map rewrite-anon-fns expr)))
    ((string? expr) expr)
    ((number? expr) expr)
    ((keyword? expr) expr)
    ((boolean? expr) expr)
    ((char? expr) expr)
    (else (error (format #f "Unexpected ~s while rewriting #():" (class-of expr))
                 expr)))))

;;; syntax-quote (i.e. quasiquote)

(define synquote-gensym?
  ;; FIXME: stricter syntax, or can we assume it's already a valid
  ;; clojure sym?
  (let* ((rx (make-regexp "^[^/]+#$")))
    (lambda (s) (regexp-exec rx s))))

(define* (expand-synquote-gensyms expr #:optional bindings)
  ;; We don't have to handle literal collections here because we
  ;; expand them later.
  ;; FIXME: can we use uninterned symbols?

  (define (expand-unquoted expr)
    ;; Do what clojure/jvm does and expand everything, even inside
    ;; quoted regions.
    (cond
     ((symbol? expr) expr)
     ((null? expr) expr)
     ((list? expr)
      (if (eq? 'syntax-quote (car expr))
          (receive (result _)
              (expand-synquote-gensyms expr (hash-map))
            result)
          (map (lambda (x) (expand-unquoted x)) expr)))
     ((string? expr) expr)
     ((number? expr) expr)
     ((keyword? expr) expr)
     ((boolean? expr) expr)
     ((char? expr) expr)
     (else (error (format #f "Unexpected unquoted ~s:" (class-of expr)) expr))))

  (if (not bindings)
      (expand-unquoted expr)
      ;; Inside syntax-quoted region
      (cond
       ((symbol? expr)
        (let ((syms (symbol->string expr)))
          (if (not (synquote-gensym? syms))
              (values expr bindings)
              (let ((b (get bindings expr #f)))
                (if b
                    (values b bindings)
                    (let ((b (gensym (string-append (substring/read-only syms 0 (1- (string-length syms))) "_"))))
                      (values b (assoc bindings expr b))))))))
       ((null? expr)
        (values expr bindings))
       ((list? expr)
        (case (car expr)
          ((syntax-quote)
           (receive (result _)
               (expand-synquote-gensyms (cdr expr) (hash-map))
             (values (cons 'syntax-quote result) bindings)))
          ((unquote unquote-splicing)
           (values (expand-unquoted expr) bindings))
          (else ;; Normal list
           (let ((result-and-bindings (fold (lambda (x prev-r-and-b)
                                              (receive (result bindings)
                                                  (expand-synquote-gensyms x (cdr prev-r-and-b))
                                                (cons (cons result (car prev-r-and-b))
                                                      bindings)))
                                            (cons '() bindings)
                                            expr)))
             (values (reverse! (car result-and-bindings))
                     (cdr result-and-bindings))))))
       ((string? expr) (values expr bindings))
       ((number? expr) (values expr bindings))
       ((keyword? expr) (values expr bindings))
       (else (error "Unexpected expression:" expr)))))

(define (merge-metadata-into-items lst)
  ;; Does not need to handle literals because we instantiate them
  ;; later.
  (let loop ((lst lst)
             (pending-meta (hash-map))
             (result '()))
    (cond
     ((null? lst)
      (unless (empty? pending-meta)
        (error "metadata pending at end of list:" pending-meta
               (reverse! result)))
      (reverse! result))
     ((pair? lst)
      (let ((expr (car lst)))
        (if (and (pair? expr) (eq? '/lokke/reader-meta (car expr)))
            ;; For now, assume the meta expr is valid
            (let ((m (cdr expr)))
              (cond
               ((keyword? m)
                (loop (cdr lst) (merge (hash-map m #t) pending-meta) result))
               ((or (symbol? m) (string? m))
                ;; FIXME: why does tag: here and further below cause this:
                ;;   In procedure module-lookup: Unbound variable: #{tag:}#
                ;; wonder if lokke-reader.c has some remaining
                ;; overlapping state with guile's reader...  (Still a
                ;; problem?)
                (loop (cdr lst) (merge (hash-map #:tag m) pending-meta) result))
               ((and (pair? m) (eq? '/lokke/reader-hash-map (car m)))
                (loop (cdr lst) (merge m pending-meta) result))
               (else
                (error (format #f "Unexpected metadata type ~s for:"
                               (class-of m))
                       m))))
            ;; Not (/lokke/reader-meta ...)
            (let ((x (apply-internal-metadata expr (hash-map))))
              (loop (cdr lst)
                    (hash-map)
                    (cons (if (empty? pending-meta)
                              x
                              (with-meta pending-meta x))
                          result))))))
     (else
      (error "Argument must be a list:" lst)))))

(define (apply-internal-metadata expr pending-meta)
  ;; Does not need to handle literals because we instantiate them
  ;; later.
  (cond
   ((symbol? expr) expr)
   ((string? expr) expr)
   ((number? expr) expr)
   ((keyword? expr) expr)
   ((null? expr) expr)
   ((list? expr) (merge-metadata-into-items expr))
   ((boolean? expr) expr)
   ((char? expr) expr)
   (else (error "Unexpected expression from primitive reader:" expr))))

(define (expand-reader-conditionals expr)
  ;; FIXME: suspect we're missing some preserve-metadata calls in here.
  ;; Returns a list containing the top-level expansion(s), if any.
  (define (splice selected reader-cond)
    (cond
     ((null? selected) selected)
     ((list? selected)
      (case (car selected)
        ;; Clojure/JVM only allows implementers of List interface
        ((/lokke/reader-vector) (cdr selected))
        (else selected)))
     (else
      (error (format #f "Improper splice in #?@~s" reader-cond)))))
  (define (select-for-dialect reader-cond splice?)
    ;; Returns a list of expressions (if any) to be spliced into the
    ;; parent.
    (unless (even? (length reader-cond))
      (error (format #f "Improper reader conditional: #?~a~s"
                     (if splice? "@" "") reader-cond)))
    (let* ((cases (pairify reader-cond))
           (code (or (assq-ref cases cljl:)
                     (assq-ref cases default:))))
      (if code
          (if splice?
              (splice (car code) reader-cond)
              (list (car code)))
          '())))
  (define (expand expr)
    ;; Expand all reader conditionals, and return the result wrapped
    ;; in a list unless it's an unsplicing, so that the caller can
    ;; build the correct result via unconditional concatenation.
    (preserve-meta-if-new!
     expr
     (cond
      ((null? expr) (list expr))
      ((list? expr)
       (case (car expr)
         ((/lokke/reader-cond)
          (select-for-dialect (concatenate (expand (cdr expr))) #f))
         ((/lokke/reader-cond-splice)
          (select-for-dialect (concatenate (expand (cdr expr))) #t))
         (else
          (list (concatenate (map expand expr))))))
      (else (list expr)))))
  (when debug-conditionals?
    (format (current-error-port) "expand reader conditional:\n")
    (pretty-print expr (current-error-port)))
  (let* ((result (expand expr)))
    (when debug-conditionals?
      (format (current-error-port) "reader conditional expanded:\n")
      (pretty-print expr (current-error-port))
      (format (current-error-port) "  =>\n")
      (pretty-print result (current-error-port)))
    result))

(define (read-conditionally port)
  (let ((expr (read-primitively port)))
    (if (eof-object? expr)
        expr
        (let ((result (expand-reader-conditionals expr)))
          (cond
           ((null? result) (read-conditionally port))  ;; #?() #?(foo 1) etc.
           ;; FIXME: arrange for read-string to return multiple values
           ;; and for read to return multuple top level values one at a
           ;; time so we can remove this error.
           ((not (null? (cdr result)))
            (when (eq? '/lokke/reader-cond (car expr))
              (error (format #f "#?(...) produced multiple top level values (BUG): #?~s"
                             (cdr expr))))
            (unless (eq? '/lokke/reader-cond-splice (car expr))
              (error "Unexpected reader conditional marker:" expr))
            (error
             (format #f "#?@(...) cannot unsplice multiple top level values yet: #?@~s"
                     (cdr expr))))
           (else (car result)))))))

(define (uninstantiated-read port)
  (let loop ((expr (read-conditionally port))
             (pending-meta (hash-map)))
    (cond
     ((eof-object? expr)
      (unless (empty? pending-meta)
        (error "metadata pending at end of file:" pending-meta))
      expr)
     ((and (pair? expr) (eq? '/lokke/reader-meta (car expr)))
      (unless (null? (cddr expr))
        (error "Expected only one argument to /lokke/reader-meta:" expr))
      ;; For now, assume the meta expr is valid
      (let ((m (cadr expr)))
        (cond
         ((keyword? m)
          (loop (read-conditionally port)
                (merge (hash-map m #t) pending-meta)))
         ((or (symbol? m) (string? m))
          (loop (read-conditionally port)
                (merge (hash-map #:tag m) pending-meta)))
         ((and (pair? m) (eq? '/lokke/reader-hash-map (car m)))
          (loop (read-conditionally port)
                (merge (apply hash-map (cdr m)) pending-meta)))
         (else
          (error (format #f "Unexpected metadata type ~s for:" (class-of m))
                 m)))))
     (else  ;; Not (/lokke/reader-meta ...)
      (when debug-reader?
        (format (current-error-port) "reader rewriting #(): ~s\n" expr))
      (let* ((result (rewrite-anon-fns expr))
             (_ (when debug-reader?
                  (format (current-error-port) "reader expanding syntax-quote: ~s\n" result)))
             (result (expand-synquote-gensyms result))
             (_ (when debug-reader?
                  (format (current-error-port) "reader compiling metadata: ~s\n" result)))
             (result (apply-internal-metadata result (hash-map)))
             (_ (when debug-reader?
                  (format (current-error-port) "reader finishing up: ~s\n" result)))
             (result (if (empty? pending-meta)
                         result
                         (set-meta! result pending-meta))))
        (when debug-reader?
          (format (current-error-port) "reader returning: ~s\n" result))
        result)))))

(define (read-for-compiler port)
  (uninstantiated-read port))

(define (read-string-for-compiler s)
  (call-with-input-string s
    (lambda (port) (read-for-compiler port))))

(define (read port)
  (let ((expr (uninstantiated-read port)))
    (if (eof-object? expr)
        expr
        (literals->clj-instances expr))))

(define (read-string s)
  (call-with-input-string s
    (lambda (port) (read port))))
