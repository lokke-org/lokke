;; Copyright (C) 2015 Free Software Foundation, Inc.

;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Commentary:
;;;
;;; A fash is a functional hash map.
;;;
;;; Code:

(define-module (fash)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 match)
  #:export (fash?
            make-fash
            make-transient-fash
            transient-fash
            persistent-fash
            fash-size
            fash-ref
            fash-set
            fash-set!
            fash-fold
            fash-unfold
            fash->alist
            alist->fash
            fash
            fashq
            fashv))

(define-syntax-rule (define-inline name val)
  (define-syntax name (identifier-syntax val)))

;; FIXME: This should make an actual atomic reference.
(define-inlinable (make-atomic-reference value)
  (list value))
(define-inlinable (get-atomic-reference reference)
  (car reference))
(define-inlinable (set-atomic-reference! reference value)
  (set-car! reference value))

(define-syntax compile-time-cond
  (lambda (x)
    (syntax-case x (else)
      ((_ (else body ...))
       #'(begin body ...))
      ((_ (exp body ...) clause ...)
       (if (eval (syntax->datum #'exp) (current-module))
           #'(begin body ...)
           #'(compile-time-cond clause ...))))))

(compile-time-cond
 ((>= (logcount most-positive-fixnum) 32)
  (define-inline *branch-bits* 5))
 (else
  (define-inline *branch-bits* 4)))
(define-inline *branch-size* (ash 1 *branch-bits*))
(define-inline *branch-mask* (1- *branch-size*))

(define-record-type <fash>
  (%make-fash size root hash equal)
  fash?
  (size %fash-size)
  (root fash-root)
  (hash fash-hash)
  (equal fash-equal))

(define-record-type <transient-fash>
  (%make-transient-fash size root hash equal edit)
  transient-fash?
  (size transient-fash-size set-transient-fash-size!)
  (root transient-fash-root set-transient-fash-root!)
  (hash transient-fash-hash)
  (equal transient-fash-equal)
  (edit transient-fash-edit set-transient-fash-edit!))

(define (clone-and-set branch i elt)
  (let ((new (vector-copy branch)))
    (vector-set! new i elt)
    new))

(define (raw-hashq x)
  (hashq x most-positive-fixnum))

(define (raw-hashv x)
  (hashv x most-positive-fixnum))

(define (raw-hash x)
  (hash x most-positive-fixnum))

(define* (make-fash #:key (hash raw-hash) (equal equal?))
  (%make-fash 0 #f hash equal))

(define* (make-transient-fash #:key (hash raw-hash) (equal equal?))
  (let ((edit (make-atomic-reference (current-thread))))
    (%make-transient-fash 0 #f hash equal edit)))

(define-inlinable (assert-readable! root-edit)
  (unless (eq? (get-atomic-reference root-edit) (current-thread))
    (error "Transient fash owned by another thread" root-edit)))

(define (transient-fash source)
  (match source
    (($ <transient-fash> size root hash equal edit)
     (assert-readable! edit)
     source)
    (($ <fash> size root hash equal)
     (let ((edit (make-atomic-reference (current-thread))))
       (%make-transient-fash size root hash equal edit)))))

(define (persistent-fash source)
  (match source
    (($ <transient-fash> size root hash equal edit)
     (assert-readable! edit)
     ;; Make a fresh reference, causing any further operations on this
     ;; transient to clone its root afresh.
     (set-transient-fash-edit! source
                               (make-atomic-reference (current-thread)))
     ;; Clear the reference to the current thread, causing our edited
     ;; data structures to be persistent again.
     (set-atomic-reference! edit #f)
     (let ((size (or size (compute-transient-fash-size! source))))
       (%make-fash size root hash equal)))
    (($ <fash>)
     source)))

(define (fash-size fash)
  (match fash
    (($ <fash> size) size)
    (($ <transient-fash> size root hash equal edit)
     (assert-readable! edit)
     (or size (compute-transient-fash-size! fash)))))

(define *fash-sentinel* (list 'sentinel))

(define-inlinable (mask h shift)
  (logand (ash h (- shift)) *branch-mask*))
(define-inlinable (bitpos h shift)
  (ash 1 (mask h shift)))
(define-inlinable (index bit bitmap)
  (logcount (logand bitmap (1- bit))))

(define-inlinable (sparse-branch bitmap array edit)
  (vector 'sparse bitmap array edit))
(define-inlinable (packed-branch array edit)
  (vector 'packed array edit))

(define-inlinable (collision-branch array edit)
  (vector 'collision array edit))

(define (compute-transient-fash-size! fash)
  (define (compute-size root)
    (match root
      (#('sparse bitmap array edit)
       (let ((len (* 2 (logcount bitmap))))
         (let lp ((i 0) (count 0))
           (if (< i len)
               (let ((k (vector-ref array i)))
                 (lp (+ i 2)
                     (if (eq? k *fash-sentinel*)
                         (let ((v (vector-ref array (1+ i))))
                           (+ count (compute-size v)))
                         (1+ count))))
               count))))
      (#('packed array edit)
       (let ((len (vector-length array)))
         (let lp ((i 0) (count 0))
           (if (< i len)
               (lp (1+ i)
                   (+ count (compute-size (vector-ref array i))))
               count))))
      (#('collision array edit)
       (ash (vector-length array) -1))
      (#f 0)))
  (match fash
    (($ <transient-fash> size root hash equal edit)
     (assert-readable! edit)
     (let ((size (compute-size root)))
       (set-transient-fash-size! fash size)
       size))))

(define* (fash-ref fash k #:optional (not-found (lambda (k) #f)))
  (define (find-value shift root h equal)
    (let recurse ((shift shift) (root root))
      (match root
        (#('sparse bitmap array edit)
         (let ((bit (bitpos h shift)))
           (if (logtest bitmap bit)
               (let* ((idx (index bit bitmap))
                      (k* (vector-ref array (* 2 idx)))
                      (v* (vector-ref array (1+ (* 2 idx)))))
                 (cond
                  ((eq? k* *fash-sentinel*)
                   (recurse (+ shift *branch-bits*) v*))
                  ((or (eq? k k*) (equal k k*)) v*)
                  (else (not-found k))))
               (not-found k))))
        (#('packed array edit)
         (let ((node (vector-ref array (mask h shift))))
           (if node
               (recurse (+ shift *branch-bits*) node)
               (not-found k))))
        (#('collision array edit)
         (let ((len (vector-length array)))
           (let lp ((i 0))
             (if (< i len)
                 (if (equal (vector-ref array i) k)
                     (vector-ref array (1+ i))
                     (lp (+ i 2)))
                 (not-found k)))))
        (#f (not-found k)))))
  (match fash
    (($ <fash> size root hash equal)
     (find-value 0 root (hash k) equal))
    (($ <transient-fash> size root hash equal edit)
     (assert-readable! edit)
     (find-value 0 root (hash k) equal))))

(define* (fash-set fash k v)
  (define (update shift root h equal)
    (let recurse ((shift shift) (root root))
      (match root
        (#('sparse bitmap array edit)
         (let* ((bit (bitpos h shift))
                (idx (* 2 (index bit bitmap))))
           (define (update-branch array)
             (sparse-branch bitmap array #f))
           (define (sparse-leaf shift h k v)
             (sparse-branch (bitpos h shift) (vector k v) #f))
           (define (collision-leaf k v)
             (collision-branch (vector k v) #f))
           (if (logtest bitmap bit)
               (let* ((k* (vector-ref array idx))
                      (v* (vector-ref array (1+ idx))))
                 (cond
                  ((eq? k* *fash-sentinel*)
                   ;; Update subnode.
                   (call-with-values (lambda ()
                                       (recurse (+ shift *branch-bits*) v*))
                     (lambda (v** added?)
                       (if (eq? v* v**)
                           (values root #f)
                           (values (update-branch
                                    (clone-and-set array (1+ idx) v**))
                                   added?)))))
                  ((or (eq? k k*) (equal k k*))
                   ;; Replace value.
                   (if (eq? v v*)
                       (values root #f)
                       (values (update-branch
                                (clone-and-set array (1+ idx) v))
                               #f)))
                  (else
                   ;; Replace leaf with subnode.
                   (let* ((h* ((fash-hash fash) k*))
                          (shift* (+ shift *branch-bits*))
                          (array* (clone-and-set array idx *fash-sentinel*))
                          (subnode (if (eqv? h h*)
                                       (collision-leaf k* v*)
                                       (sparse-leaf shift* h* k* v*))))
                     (vector-set! array* (1+ idx) (recurse shift* subnode))
                     (values (update-branch array*) #t)))))
               (let* ((old-length (* 2 (logcount bitmap))))
                 (if (< old-length *branch-size*)
                     ;; Add leaf to sparse.
                     (let ((array* (make-vector (+ old-length 2))))
                       (vector-move-left! array 0 idx array* 0)
                       (vector-set! array* idx k)
                       (vector-set! array* (1+ idx) v)
                       (vector-move-left! array idx old-length array* (+ idx 2))
                       (values (sparse-branch (logior bitmap bit) array* #f)
                               #t))
                     ;; Promote sparse to packed.
                     (let ((array* (make-vector *branch-size* #f))
                           (hash (fash-hash fash))
                           (shift* (+ shift *branch-bits*)))
                       (vector-set! array* (mask h shift)
                                    (sparse-leaf shift* h k v))
                       (let lp ((i 0) (idx 0))
                         (when (< i *branch-size*)
                           (if (logbit? i bitmap)
                               (let* ((k* (vector-ref array idx))
                                      (v* (vector-ref array (1+ idx)))
                                      (node (if (eq? k* *fash-sentinel*)
                                                v*
                                                (sparse-leaf shift*
                                                             (hash k*) k* v*))))
                                 (vector-set! array* i node)
                                 (lp (1+ i) (+ idx 2)))
                               (lp (1+ i) idx))))
                       (values (packed-branch array* #f) #t)))))))
        (#('packed array edit)
         (let* ((idx (mask h shift))
                (node (vector-ref array idx)))
           (call-with-values (lambda ()
                               (recurse (+ shift *branch-bits*) node))
             (lambda (node* added?)
               (if (eq? node node*)
                   (values root #f)
                   (values (packed-branch (clone-and-set array idx node*) #f)
                           added?))))))
        (#('collision array edit)
         (let ((len (vector-length array)))
           (let lp ((i 0))
             (if (< i len)
                 (if (equal (vector-ref array i) k)
                     (if (eq? (vector-ref array (1+ i)) v)
                         root
                         (let ((array* (vector-copy array)))
                           (vector-set! array* (1+ i) v)
                           (collision-branch array* #f)))
                     (lp (+ i 2)))
                 (let ((array* (make-vector (+ len 2) #f)))
                   (vector-move-left! array 0 len array* 0)
                   (vector-set! array* len k)
                   (vector-set! array* (1+ len) v)
                   (collision-branch array* #f))))))
        (#f (values (sparse-branch (bitpos h shift) (vector k v) #f)
                    #t)))))
  (match fash
    (($ <fash> size root hash equal)
     (let-values (((root* added?) (update 0 root (hash k) equal)))
       (if (eq? root root*)
           fash
           (%make-fash (if added? (1+ size) size) root* hash equal))))
    (($ <transient-fash>)
     (fash-set (persistent-fash fash) k v))))

(define* (fash-set! fash k v)
  (define (update shift root h equal root-edit)
    (let recurse ((shift shift) (root root))
      (match root
        (#('sparse bitmap array edit)
         (let* ((bit (bitpos h shift))
                (idx (* 2 (index bit bitmap))))
           (define (sparse-leaf shift h k v)
             (sparse-branch (bitpos h shift) (vector k v) root-edit))
           (define (collision-leaf k v)
             (collision-branch (vector k v) root-edit))
           (if (logtest bitmap bit)
               (let* ((array* (if (eq? edit root-edit)
                                  array
                                  (vector-copy array)))
                      (k* (vector-ref array* idx))
                      (v* (vector-ref array* (1+ idx))))
                 (cond
                  ((eq? k* *fash-sentinel*)
                   ;; Update subnode.
                   (vector-set! array* (1+ idx)
                                (recurse (+ shift *branch-bits*) v*)))
                  ((or (eq? k k*) (equal k k*))
                   ;; Replace value.
                   (vector-set! array* (1+ idx) v))
                  (else
                   ;; Replace leaf with subnode.
                   (let* ((h* ((transient-fash-hash fash) k*))
                          (shift* (+ shift *branch-bits*))
                          (subnode (if (eqv? h h*)
                                       (collision-leaf k* v*)
                                       (sparse-leaf shift* h* k* v*))))
                     (vector-set! array* idx *fash-sentinel*)
                     (vector-set! array* (1+ idx) (recurse shift* subnode)))))
                 (if (eq? array array*)
                     root
                     (sparse-branch bitmap array* root-edit)))
               (let* ((old-length (* 2 (logcount bitmap))))
                 (if (< old-length *branch-size*)
                     ;; Add leaf to sparse.
                     (let ((array* (if (and (eq? edit root-edit)
                                            (<= (+ old-length 2)
                                                (vector-length array)))
                                       array
                                       (make-vector (+ old-length 2) #f))))
                       (vector-move-left! array 0 idx array* 0)
                       (vector-move-left! array idx old-length array* (+ idx 2))
                       (vector-set! array* idx k)
                       (vector-set! array* (1+ idx) v)
                       (cond
                        ((not (eq? edit root-edit))
                         (sparse-branch (logior bitmap bit) array* root-edit))
                        (else
                         (vector-set! root 1 (logior bitmap bit))
                         (vector-set! root 2 array*)
                         root)))
                     ;; Promote sparse to packed.
                     (let ((array* (make-vector *branch-size* #f))
                           (hash (transient-fash-hash fash))
                           (shift* (+ shift *branch-bits*)))
                       (vector-set! array* (mask h shift)
                                    (sparse-leaf shift* h k v))
                       (let lp ((i 0) (idx 0))
                         (when (< i *branch-size*)
                           (if (logbit? i bitmap)
                               (let* ((k* (vector-ref array idx))
                                      (v* (vector-ref array (1+ idx)))
                                      (node (if (eq? k* *fash-sentinel*)
                                                v*
                                                (sparse-leaf shift*
                                                             (hash k*) k* v*))))
                                 (vector-set! array* i node)
                                 (lp (1+ i) (+ idx 2)))
                               (lp (1+ i) idx))))
                       (packed-branch array* root-edit)))))))
        (#('packed array edit)
         (if (eq? edit root-edit)
             (let ((idx (mask h shift)))
               (vector-set! array idx (recurse (+ shift *branch-bits*)
                                               (vector-ref array idx)))
               root)
             (recurse shift (packed-branch (vector-copy array) root-edit))))
        (#('collision array edit)
         (let ((len (vector-length array)))
           (let lp ((i 0))
             (if (< i len)
                 (if (equal (vector-ref array i) k)
                     (let ((array* (if (eq? edit root-edit)
                                       array
                                       (vector-copy array))))
                       (vector-set! array* (1+ i) v)
                       (if (eq? array array*)
                           root
                           (collision-branch array* root-edit)))
                     (lp (+ i 2)))
                 (let ((array* (make-vector (+ len 2) #f)))
                   (vector-move-left! array 0 len array* 0)
                   (vector-set! array* len k)
                   (vector-set! array* (1+ len) v)
                   (collision-branch array* root-edit))))))
        (#f (sparse-branch (bitpos h shift) (vector k v) root-edit)))))
  (match fash
    (($ <transient-fash> size root hash equal edit)
     (assert-readable! edit)
     (set-transient-fash-size! fash #f)
     (set-transient-fash-root! fash (update 0 root (hash k) equal edit))
     fash)
    (($ <fash>)
     (fash-set! (transient-fash fash) k v))))

(define (fash-fold f fash seed)
  (define (fold root seed)
    (match root
      (#('sparse bitmap array edit)
       (let ((len (vector-length array)))
         (let lp ((i 0) (seed seed))
           (if (< i len)
               (let ((k (vector-ref array i))
                     (v (vector-ref array (1+ i))))
                 (lp (+ i 2)
                     (if (eq? k *fash-sentinel*)
                         (fold v seed)
                         (f k v seed))))
               seed))))
      (#('packed array edit)
       (let ((len (vector-length array)))
         (let lp ((i 0) (seed seed))
           (if (< i len)
               (lp (1+ i)
                   (fold (vector-ref array i) seed))
               seed))))
      (#('collision array edit)
       (let ((len (vector-length array)))
         (let lp ((i 0) (seed seed))
           (if (< i len)
               (let ((k (vector-ref array i))
                     (v (vector-ref array (1+ i))))
                 (lp (+ i 2) (f k v seed)))
               seed))))
      (#f seed)))
  (match fash
    (($ <fash> size root hash equal)
     (fold root seed))
    (($ <transient-fash>)
     (fash-fold f (persistent-fash fash) seed))))

(define* (fash-unfold pred map next seed
                      #:optional (tail (make-transient-fash)))
  (if (transient-fash? tail)
      (let lp ((seed seed) (tail tail))
        (if (pred seed)
            (persistent-fash tail)
            (lp (next seed)
                (call-with-values (lambda () (map seed))
                  (lambda (k v)
                    (fash-set! tail k v))))))
      (let lp ((seed seed) (tail tail))
        (if (pred seed)
            tail
            (lp (next seed)
                (call-with-values (lambda () (map seed))
                  (lambda (k v)
                    (fash-set tail k v))))))))

(define (fash->alist fash)
  (fash-fold acons fash '()))

(define* (alist->fash alist #:key (hash raw-hashq) (equal equal?))
  (fash-unfold null?
               (match-lambda (((k . v) . _) (values k v)))
               (match-lambda ((_ . alist) alist))
               alist
               (make-transient-fash #:hash hash #:equal equal)))

(define (print-fash fash port)
  (format port "#<fash ~a>" (fash->alist fash)))
(define (print-transient-fash fash port)
  (format port "#<transient-fash ~a>" (fash->alist fash)))

(set-record-type-printer! <fash> print-fash)
(set-record-type-printer! <transient-fash> print-transient-fash)

(define-syntax-rule (fash (k v) ...)
  (let ((fash (make-transient-fash)))
    (fash-set! fash k v)
    ...
    (persistent-fash fash)))

(define-syntax-rule (fashq (k v) ...)
  (let ((fash (make-transient-fash #:hash raw-hashq #:equal eq?)))
    (fash-set! fash k v)
    ...
    (persistent-fash fash)))

(define-syntax-rule (fashv (k v) ...)
  (let ((fash (make-transient-fash #:hash raw-hashv #:equal eqv?)))
    (fash-set! fash k v)
    ...
    (persistent-fash fash)))

(define (test)
  (define (fash-integers n)
    (fash-unfold (lambda (seed) (>= seed n))
                 (lambda (seed) (values seed seed))
                 (lambda (seed) (1+ seed))
                 0))
  (let ((integers (fash-integers #e1e5)))
    (let lp ((n 0))
      (when (< n #e1e5)
        (unless (eqv? (fash-ref integers n) n)
          (error "ref failed" n))))
    (unless (equal? (sort (fash->alist integers)
                          (match-lambda* (((k1 . v1) (k2 . v2))
                                          (< k1 k2))))
                    (map (lambda (n) (cons n n))
                         (iota #e1e5)))
      (error "keys and values did not match"))
    (unless (eqv? #e1e5) (fash-size integers)))
  (let ((fash (make-fash))
        (k1 (string #\a))
        (k2 (string #\a)))
    (let ((fash (fash-set (fash-set fash k1 1) k2 2)))
      (unless (eqv? (fash-ref fash k1) 2)
        (error "ref k1 failed"))
      (unless (eqv? (fash-ref fash k2) 2)
        (error "ref k2 failed"))))
  (let ((fashq (make-fash #:hash raw-hashq #:equal eq?))
        (k1 (string #\a))
        (k2 (string #\a)))
    (let ((fashq (fash-set (fash-set fashq k1 1) k2 2)))
      (unless (eqv? (fash-ref fashq k1) 1)
        (error "refq k1 failed"))
      (unless (eqv? (fash-ref fashq k2) 2)
        (error "refq k2 failed"))))
  (let ((fash0 (make-fash #:hash (lambda (x) 0) #:equal eq?))
        (k1 (string #\a))
        (k2 (string #\a)))
    (let ((fash0 (fash-set (fash-set fash0 k1 1) k2 2)))
      (unless (eqv? (fash-ref fash0 k1) 1)
        (error "refq k1 failed"))
      (unless (eqv? (fash-ref fash0 k2) 2)
        (error "refq k2 failed"))
      (unless (equal? '(("a" . 1) ("a" . 2))
                      (sort (fash->alist fash0)
                            (match-lambda* (((k1 . v1) (k2 . v2))
                                            (< v1 v2)))))
        (error "keys and values did not match"))
      (unless (eqv? 2) (fash-size fash0)))))
