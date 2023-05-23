;;; Copyright (C) 2022 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

(define-module (lokke datatype)
  #:use-module ((ice-9 atomic)
                #:select (atomic-box-ref atomic-box-set! make-atomic-box))
  #:use-module ((ice-9 match) #:select (match))
  #:use-module ((lokke base map) #:select (<map>))
  #:use-module ((lokke base util) #:select (vec-tag?))
  #:use-module ((lokke base syntax) #:select (fn))
  #:use-module ((lokke base collection)
                #:select (assoc
                          count
                          dissoc
                          get
                          keys
                          lazy-seq
                          reduce-kv
                          seq
                          vals))
  #:use-module ((lokke base map-entry) #:select (map-entry))
  #:use-module ((lokke base metadata) #:select (meta with-meta))
  #:use-module ((lokke base util) #:select (require-nil))
  #:use-module ((lokke collection) #:select (some))
  #:use-module ((lokke compare) #:select (clj= hash))
  #:use-module ((lokke hash-map) #:select (hash-map hash-map?))
  #:use-module ((lokke reader literal)
                #:select (reader-vector? reader-vector-elts))
  #:use-module ((lokke scm vector)
                #:select (lokke-vec lokke-vector lokke-vector-length))
  #:use-module (oop goops)
  #:use-module ((srfi srfi-1) #:select (delete-duplicates every partition))
  #:use-module ((srfi srfi-69) #:select ((hash . srfi-hash)))
  #:use-module ((srfi srfi-71))
  #:export (defprotocol defrecord extend-type)
  #:re-export (count dissoc get keys seq vals)
  #:re-export-and-replace (assoc)
  #:duplicates (merge-generics replace warn-override-core warn last))

;; FIXME: avoid method as binding name?
;; FIXME: add extends?
;; FIXME: need to strip arg metadata?

(define (error* fmt . args)
  (error (apply format #f fmt args)))

(define (arglist? x)
  (and (reader-vector? x)
       (every symbol? (reader-vector-elts x))))

(eval-when (expand load eval)
  (define (compile-protocol-method method-sig)

    ;; FIXME: warn when clobbering generic

    (define (convert-sigs sigs)
      (let* ((argvecs (map reader-vector-elts sigs))
             (arities (map length argvecs)))
        (unless (= (length argvecs) (length (delete-duplicates arities)))
          (error "Duplicate arities for protocol method:"
                 (syntax->datum method-sig)))
        (unless (every positive? arities)
          (error "All methods must accept at least one argument:"
                 (syntax->datum method-sig)))
        argvecs))

    (let ((sig (syntax->datum method-sig)))
      (match sig
        (((? symbol? name)
          (? arglist? sigs) ...
          (? string? doc))
         (hash-map #:name name
                   #:sigs (convert-sigs sigs)
                   #:doc doc))
        (((? symbol? name)
          (? arglist? sigs) ...)
         (hash-map #:name name
                   #:sigs (convert-sigs sigs)))
        (_ (error "Invalid protocol method signature:" sig))))))

(eval-when (expand load eval)
  (define (compile-protocol name methods)
    (let* ((name-sym (syntax->datum name)))
      (unless (symbol? name-sym)
        (error "defprotocol name is not a symbol" name-sym))
      (hash-map
       ;; FIXME: fully qualified name?
       #:name name-sym
       #:methods (let loop ((methods methods)
                            (result (hash-map)))
                   (if (null? methods)
                       result
                       (let* ((m (compile-protocol-method (car methods)))
                              (m-name (get m #:name)))
                         (when (get result m-name)
                           (error* "Duplicate method ~a for protocol ~a"
                                   m-name name))
                         (loop (cdr methods) (assoc result m-name m)))))))))

(define-syntax defprotocol
  (lambda (x)

    (define (method-map->syntax ctx m)
      #`(hash-map #:name #,(datum->syntax ctx `(quote ,(get m #:name)))
                  #:sigs (lokke-vector
                          #,@(map (lambda (sig)
                                    (let ((qargs (map (lambda (arg) arg) sig)))
                                      (datum->syntax ctx #`(lokke-vec (quote #,qargs)))))
                                  (get m #:sigs)))))

    (syntax-case x ()
      ((_ name method-sig ...)
       (begin
         (let* ((protocol (compile-protocol #'name #'(method-sig ...)))
                (methods (get protocol #:methods))
                (method-kvs (reduce-kv (lambda (result m-name m-map)
                                         (cons* (datum->syntax x `(quote ,m-name))
                                                (method-map->syntax x m-map)
                                                result))
                                       '()
                                       methods))
                (method-map #`(hash-map #,@method-kvs))
                (m-names (reduce-kv (lambda (result name _) (cons name result))
                                    '()
                                    methods)))
           #`(eval-when (expand load eval)
               (begin
                 ;; Define the protocol as a map describing it
                 (define name
                   (hash-map #:lokke.datatype/protocol #t
                             #:name (quote name)
                             #:methods #,method-map))
                 ;; Define a generic for each method
                 #,@(map (lambda (n)
                           #`(define-generic #,(datum->syntax x n)))
                         m-names)
                 ;; Export
                 (export name #,@(map (lambda (n) (datum->syntax x n))
                                      m-names))))))))))

(define (protocol? x)
  (and (hash-map? x)
       (get x #:lokke.datatype/protocol)))

;; FIXME: add protocol definitions to defrecord via extend-protocol
;; FIXME: add extend-protocol
;; FIXME: extend protocol on nil

(define-syntax extend-type

  ;; Looks like method bodies don't support pre/post condition maps,
  ;; i.e. the map is just a map.

  ;; recur inside methods can be shadowed, unlike the jvm (still true?)
  ;; Always put & patterns first
  ;; Enforce at least one arg for each protocol, i.e. "this".

  (lambda (extend-type-stx)

    (define (find-protocol name-syn)
      (let* ((mod (current-module))
             (name (syntax->datum name-syn))
             (_ (unless (symbol? name)
                  (error "Protocol name is not a symbol:" name)))
             (var (module-variable mod name))
             (_ (unless var (error "Protocol name is undefined" name)))
             (proto (variable-ref var)))
        (unless (protocol? proto)
          (error* "~s refers to ~s, not a protocol" name proto))
        proto))

    ;; dotted args (see lokke syntax)?
    (define (method-adder type method-name args-meta args body)
      (let* ((method-args (generate-temporaries args))
             ;; Change (this x y ...) to ((this <some-type) x y ...)
             (specialized (cons (list (car method-args) type)
                                (cdr method-args)))
             (args (cons* (datum->syntax extend-type-stx '/lokke/reader-vector)
                          args-meta args)))
        #`(let ((f (fn #,args #,@body)))
            (add-method! #,method-name (method #,specialized (f #,@method-args))))))

    (define (expand-type-extensions type extensions proto seen expansion)
      (syntax-case extensions ()
        ;; FIXME: OK to do nothing for protocols with no methods?
        ;; FIXME: reject duplicate protocols

        ((protocol exp ...) (identifier? #'protocol)
         (expand-type-extensions type #'(exp ...) (find-protocol #'protocol)
                                 seen expansion))

        ;; FIXME: catch (method-name) here as an error, or just fall through...?

        (((method-name ((vec-tag meta arg ...) body ...)
                       other-arity ...)
          exp ...)
         (vec-tag? #'vec-tag)
         ;; FIXME: meta?
         (let* ((arity (length #'(arg ...)))
                (method-sym (syntax->datum #'method-name)))
           (unless proto
             (error "Protocol missing before methods"))
           (unless (symbol? method-sym)
             (error "extend-type method name is not a symbol:" method-sym))

           ;; Q: should the method keys be keywords as with clj/jvm extend?

           (let* ((proto-name (get proto #:name))
                  (methods (get proto #:methods))
                  (mth (get methods method-sym))
                  (sigs (get mth #:sigs)))
             (unless mth
               (error* "Protocol ~s extension has no ~s method" proto-name method-sym))

             ;; Why "and sigs"? (and test that case)
             (unless (and sigs (some (lambda (x) (= arity (lokke-vector-length x)))
                                     sigs))
               (error* "There is no ~a argument method ~a in protocol ~a"
                       arity method-sym proto-name))
             (when (member (cons method-sym arity) seen)
               (error* "Duplicate definitions for ~a argument ~a ~a method"
                       arity proto-name method-sym)))

           (let* ((remaining (if (null? #'(other-arity ...))
                                 #'(exp ...)
                                 #'((method-name other-arity ...) exp ...)))
                  (expansion (cons (method-adder type
                                                 #'method-name
                                                 #'meta
                                                 #'(arg ...)
                                                 #'(body ...))
                                   expansion)))
             (expand-type-extensions type remaining proto
                                     (cons (cons method-sym arity) seen)
                                     expansion))))

        ;; Rewrite single arity as multiple arity
        (((method-name (vec-tag meta arg ...) body ...)
          exp ...)
         (vec-tag? #'vec-tag)
         (expand-type-extensions type
                                 #'((method-name ((vec-tag meta arg ...) body ...))
                                    exp ...)
                                 proto seen expansion))

        (() #`(begin #,@(reverse! expansion) #nil))
        (_ (error "Expected protocol or method, not" (syntax->datum #'methods)))))

    (syntax-case extend-type-stx ()
      ((_ type exp ...) (expand-type-extensions #'type #'(exp ...) #f '() '()))
      ((_) (error "extend-type expression with no arguments")))))

;; FIXME: are duplicate slot names rejected?
;; FIXME: explicitly reject 0-*?
;; FIXME: replace shallow-clone

(define (generic-record-assoc r slot-key? kvs)
  (when (null? kvs)
    (scm-error 'wrong-number-of-args
               "assoc"
               "Wrong number of arguments" '() #f))
  (let ((result (shallow-clone r))
        (orig-others (slot-ref r '0-others)))
    (slot-set! result '0-hash (make-atomic-box #nil))
    (let loop ((kvs kvs) (others orig-others))
      (match kvs
        ((k v kvs ...)
         (if (slot-key? k)
             (begin
               (slot-set! result (keyword->symbol k) v)
               (loop kvs others))
             (loop kvs (assoc others k v))))
        (()
         (unless (eq? others orig-others)
           (slot-set! result '0-others others))
         result)
        ((k) (error "No value for key:" k))))))

(define (generic-record-dissoc r slot-keys ks)
  ;; Assumes r is a record
  (let ((slots others (partition (lambda (k) (memq k slot-keys)) ks)))
    (if (null? slots)
        (if (null? others)
            r
            (let ((r (shallow-clone r)))
              (slot-set! r '0-hash (make-atomic-box #nil))
              (slot-set! r '0-others (apply dissoc (slot-ref r '0-others) others))
              r))
        ;; Field dissoc; demote to map
        (let loop ((slot-keys slot-keys)
                   (result (let ((o (slot-ref r '0-others)))
                             ;; Do this for now so we don't have to
                             ;; worry about trying to include core
                             ;; for (assoc nil ...) below.
                             (or (and o (apply dissoc
                                               (with-meta o (slot-ref r '0-meta))
                                               others))
                                 (with-meta (hash-map)
                                            (slot-ref r '0-meta))))))
          (match slot-keys
            (() result)
            ((k . ks)
             (if (memq k slots)
                 (loop ks result)
                 (loop ks (assoc result k (slot-ref r (keyword->symbol k)))))))))))

(define-syntax defrecord
  (lambda (x)
    (syntax-case x ()
      ((_ name (vec-tag vec-meta field ...)
          protocol-def ...)
       (vec-tag? #'vec-tag)
       (let* ((sym-name (symbol->string (syntax->datum #'name)))
              (symsyn (lambda (s) (datum->syntax x (string->symbol s))))
              (->name (symsyn (string-append "->" sym-name)))
              (map->name (symsyn (string-append "map->" sym-name)))
              (field-keys
               (map (lambda (f)
                      (datum->syntax x (symbol->keyword (syntax->datum f))))
                    #'(field ...))))

         (with-syntax (((field-key ...) field-keys))
           #`(begin
               ;; Do we still need this?
               (use-modules
                ((lokke base collection) #:select (assoc count dissoc get seq))
                ((lokke base map) #:select (<map> map?))
                ((lokke base metadata) #:select (meta with-meta)))

               ;; eval-when causes "sequence of zero expressions in
               ;; form (begin)" error
               (eval-when (expand load eval)
                 (define-class name (<map>)
                   (field #:init-value #nil) ...
                   (0-hash #:init-thunk (lambda () (make-atomic-box #nil)))
                   (0-others #:init-value #nil)
                   (0-meta #:init-value #nil)))

               (define-method (with-meta (r name) (mdata <boolean>))
                 (require-nil 'with-meta 2 mdata)
                 (if (nil? (slot-ref r '0-meta))
                     r
                     (let ((r (shallow-clone r)))
                       (slot-set! r '0-meta mdata)
                       r)))

               (define-method (with-meta (r name) (mdata <map>))
                 (if (eq? mdata (slot-ref r '0-meta))
                     r
                     (let ((r (shallow-clone r)))
                       (slot-set! r '0-meta mdata)
                       r)))

               (define-method (meta (r name))
                 (slot-ref r '0-meta))

               (define (#,->name field ...)
                 (let ((r (make name)))
                   (slot-set! r (quote field) field) ...
                   r))

               (define (#,map->name m)
                 (let* ((r (make name))
                        (others (reduce-kv (lambda (others k v)
                                             (if (memq k '(field-key ...))
                                                 (begin
                                                   (slot-set! r (keyword->symbol k) v)
                                                   others)
                                                 (cons* k v others)))
                                           '()
                                           m)))
                   (unless (null? others)
                     (slot-set! r '0-others (apply hash-map others)))
                   r))

               (define-method (clj= (r1 name) (r2 name))
                 (and (clj= (slot-ref r1 'field) (slot-ref r2 'field)) ...
                      (clj= (slot-ref r1 '0-others) (slot-ref r2 '0-others))))

               (define-method (count (r name))
                 (+ #,(length field-keys)
                    (count (slot-ref r '0-others))))

               ;; FIXME: Support (.x record) and/or (. record x)?

               (let ((get* (lambda (x key not-found)
                             (case key
                               ((field-key) (slot-ref x 'field)) ...
                               (else (get (slot-ref x '0-others) key not-found))))))

                 (define-method (get (x name) key not-found)
                   (get* x key not-found))

                 (define-method (get (x name) key)
                   (get* x key #nil)))

               (let ((class-hash (srfi-hash name)))
                 (define-method (hash (r name))
                   (let* ((box (slot-ref r '0-hash))
                          (h (atomic-box-ref box)))
                     (or h
                         (let ((h (logxor class-hash
                                          (hash (slot-ref r '0-others))
                                          (hash (slot-ref r 'field)) ...)))
                           (atomic-box-set! box h)
                           h)))))

               (define-method (keys (r name))
                 '(#,@field-keys))

               (define-method (vals (r name))
                 (map (lambda (x) (get r x)) '(#,@field-keys)))

               (define-method (assoc (r name) . kvs)
                 (generic-record-assoc r
                                       (lambda (k) (memq k '(field-key ...)))
                                       kvs))

               (define-method (dissoc (r name) . ks)
                 (generic-record-dissoc r
                                        '(field-key ...)
                                        ks))

               (define-method (seq (r name))
                 (let loop ((syms '(field ...))
                            (keys '(field-key ...)))
                   (lazy-seq
                    (if (null? syms)
                        (seq (slot-ref r '0-others))
                        (cons (map-entry (car keys) (slot-ref r (car syms)))
                              (loop (cdr syms) (cdr keys)))))))

               (extend-type name protocol-def ...)

               (module-export! (current-module) '(name #,->name #,map->name))
               (module-re-export! (current-module)
                                  '(assoc
                                    dissoc
                                    get
                                    hash
                                    map?
                                    meta
                                    seq
                                    with-meta)))))))))
