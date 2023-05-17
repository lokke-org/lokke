;;; Copyright (C) 2019-2020 2023 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

(define-module (lokke base destructure)
  ;;((language tree-il) #:prefix tree-il/)
  #:use-module ((lokke base collection) #:select (drop get nth seq seq?))
  #:use-module ((lokke base util) #:select (pairify))
  #:use-module ((lokke reader literal)
                #:select (reader-hash-map?
                          reader-hash-map-elts
                          reader-vector?
                          reader-vector-elts))
  #:use-module ((lokke hash-map) #:select (hash-map))
  #:use-module (oop goops)
  #:use-module ((srfi srfi-1)
                #:select (break
                          concatenate
                          every
                          find-tail
                          proper-list?
                          take))
  #:use-module ((srfi srfi-11) #:select (let-values))
  #:export (destructure-binding-syntax))

(eval-when (expand load eval)
  (define debug? #f))

(define-syntax dbg
  (lambda (x)
    (syntax-case x ()
      ((_ arg ...) (if debug?
                       #'(format (current-error-port) arg ...)
                       #t)))))

;; For now, we don't bother to try to be smart about dropping unnecessary
;; bindings somehow.

;; FIXME: what about duplicate names?  keys: [x y x], etc.  If nothing
;; else, perhaps just add a distinct on :keys, etc.

(define (syn-vecish? syn)
  (let ((x (syntax->datum syn)))
    (or (vector? x) (reader-vector? x))))

(define (syn-vecish->list syn)
  (let ((x (syntax->datum syn)))
    (cond
     ((vector? x) (map (lambda (d) (datum->syntax syn d))
                       (vector->list x)))
     ((reader-vector? x)
      (map (lambda (d) (datum->syntax syn d)) (reader-vector-elts x)))
     (else
      (scm-error 'wrong-type-arg 'syn-vecish->list
                 "Wrong type argument in position 1: ~S"
                 (list syn) (list syn))))))

(define (syn-map? syn)
  (reader-hash-map? (syntax->datum syn)))

(define (syn-map->list syn)
  (let ((x (syntax->datum syn)))
    (if (reader-hash-map? x)
        (map (lambda (d) (datum->syntax syn d)) (reader-hash-map-elts x))
        (scm-error 'wrong-type-arg 'syn-map->list
                   "Wrong type argument in position 1: ~S"
                   (list syn) (list syn)))))

(define (syn-map-refq syn key not-found)
  (if (null? (syntax->datum syn))
      not-found
      (let loop ((more (syn-map->list syn)))
        (cond
         ((null? more) not-found)
         ((eq? key (syntax->datum (car more)))
          (when (null? (cdr more))
            (error "Odd number of items in reader map" syn))
          (cadr more))
         (else (loop (cddr more)))))))

(define (syn-symbol? syn) (symbol? (syntax->datum syn)))

(define (syn-symbol syn)
  (let ((d (syntax->datum syn)))
    (and (symbol? d) d)))
(define (syn-keyword syn)
  (let ((d (syntax->datum syn)))
    (and (keyword? d) d)))

(define (only-aliases? lst)
  (and (even? (length lst))
       (let ((pairs (pairify lst)))
         (and (every (lambda (x) (eq? #:as x)) (map car pairs)))
         (and (every (lambda (x) (symbol? x)) (map cadr pairs))))))

(define (valid-or-content? lst)
  (and (syn-map? lst)
       (and (every symbol?
                   (syntax->datum (map car (pairify (syn-map->list lst))))))))

(define (extract-map-defaults lst)
  ;; FIXME: support alists or other scm literal too?
  (let-values (((head or-spec) (break (lambda (x) (eq? #:or (syntax->datum x)))
                                      lst)))
    (if (null? or-spec)
        (values head #nil)
        (begin
          (when (null? (cdr or-spec))
            (error ":or has no value" lst))  ;; FIXME: include more context
          (unless (valid-or-content? (cadr or-spec))
            (error ":or is not a mapping of symbols to values:" lst))
          (let ((another (find-tail (lambda (x) (eq? #:or (syntax->datum x)))
                                    (cddr or-spec))))
            (when another
              (error "Multiple :or specifications:" lst))
            (values (append head (cddr or-spec)) (cadr or-spec)))))))

(define (destructure-binding-syntax context binding init)

  (define (destruct-vec enclosing-binding i rst result)
    (if (null? rst)
        result
        (let* ((item (car rst))
               (sym (syn-symbol item)))
          (cond
           ((eq? sym '&)
            (when (null? (cdr rst)) (error "Nothing after &" rst))
            (let ((x (cadr rst)))
              (unless (only-aliases? (syntax->datum (cddr rst)))
                (error "Only :as aliases may follow rest argument" rst))
              (destruct-vec enclosing-binding
                            (1+ i)
                            (cddr rst)
                            (destructure (cadr rst) #`(seq (drop #,i #,enclosing-binding))
                                         result))))
           ((eq? #:as (syn-keyword item))
            (if (null? (cdr rst))
                (error "No value for :as" rst)
                (destruct-vec enclosing-binding
                              i
                              (cddr rst)
                              (cons (list (cadr rst) enclosing-binding)
                                    result))))
           (else
            (destruct-vec enclosing-binding
                          (1+ i)
                          (cdr rst)
                          (destructure item #`(nth #,enclosing-binding #,i #nil)
                                       result)))))))

  (define (destruct-map container-id defaults rst result)
    (if (null? rst)
        result
        (let* ((item (car rst))
               (sym (syn-symbol item))
               (kwd (syn-keyword item)))
          (cond
           ((member kwd '(#:keys #:strs #:syms))
            (when (null? (cdr rst))
              (error (format #f "No value for ~a in" kwd) rst))
            (let ((names (cadr rst)))
              (unless (syn-vecish? names)
                (error (format #f "~a value is not a vector" kwd) rst))
              (let ((names (syn-vecish->list names)))
                (unless (every syn-symbol? names)
                  (error (format #f "~a names must all be symbols" kwd) rst))
                (let* ((syms (reverse (map syntax->datum names)))
                       (dummy (dbg "syms: ~s\n" syms))
                       (names (case kwd
                                ((#:keys) (map symbol->keyword syms))
                                ((#:strs) (map symbol->string syms))
                                (else (map (lambda (x) `(quote ,x)) syms)))))
                  (destruct-map container-id
                                defaults
                                (cddr rst)
                                (append (map (lambda (sym name)
                                               (dbg "symname: ~s ~s\n" sym name)
                                               (let ((var (datum->syntax context sym))
                                                     (default-form (syn-map-refq defaults sym #nil)))
                                                 #`(#,var
                                                    (get #,container-id
                                                         #,(datum->syntax context name)
                                                         #,default-form))))
                                             syms names)
                                        result))))))
           ((eq? #:as (syn-keyword item))
            (if (null? (cdr rst))
                (error "No value for :as" rst)
                (destruct-map container-id
                              defaults
                              (cddr rst)
                              (cons (list (cadr rst) container-id)
                                    result))))
           (else
            (when (null? (cdr rst))
              (error "No keyword for map destructuring:" rst))
            (let ((k (syntax->datum (cadr rst))))
              (unless (or (keyword? k) (string? k) (symbol? k))
                (error "Invalid map destructuring key type:" rst)))
            (let ((default-form (syn-map-refq defaults sym #nil)))
              (destruct-map container-id
                            defaults
                            (cddr rst)
                            (destructure item #`(get #,container-id
                                                     (quote #,(cadr rst))
                                                     #,default-form)
                                         result))))))))

  (define (destructure binding init result)
    (let* ((tmp (car (generate-temporaries '(#t)))))
      (dbg "destructure: ~s\n" (list tmp binding init result))
      (cond
       ((syn-symbol? binding) (cons #`(#,binding #,init) result))
       ((syn-vecish? binding)
        (destruct-vec tmp 0 (syn-vecish->list binding)
                      (cons (list tmp init) result)))
       ((syn-map? binding)
        (let ((specs (syn-map->list binding))
              ;; For destructuring say (defn foo [x y {:keys
              ;; [enable?]} ...)  called as (foo 1 2 :enable true).
              ;; See the Clojure docs.
              (init #`(let ((v #,init))
                        (if (seq? v) (apply hash-map v) v))))
          (let-values (((specs defaults) (extract-map-defaults specs)))
            (destruct-map tmp defaults specs
                          (cons (list tmp init) result)))))
       (else
        (error "Unsupported destructuring form" binding)))))

  (reverse! (destructure binding init '())))
