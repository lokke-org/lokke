;   Copyright (c) Rich Hickey. All rights reserved.
;   Copyright (c) 2019-2020 Rob Browning <rlb@defaultvalue.org>
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;; License: EPL-1.0

;; All of this code is adapted from Clojure/JVM's starting around
;; 1.10.1-beta3.

;; If anything here is completely rewritten it should be moved to
;; another file with the normal dual license.

;; This file is currently a big mess, in part because we don't support
;; metadata much yet, and in part because it was hacked as quickly as
;; possible to get things even partially working.  It needs
;; substantial help.

(ns clojure.core.epl
  (:use
   [guile.lokke.core]
   [guile.lokke.lang]))

;; This is a canary; leave it for now (proves imported syntax expanders work).
(lazy-seq '(1 2 3))

(eval-when
 (expand load eval)
 ;; FIXME
 (defn chunk-append [x y] false)
 (defn chunked-seq? [x] false)
 (defn chunk [x] false)
 (defn chunk-first [x] false)
 (defn chunk-rest [x] false)
 (defn chunk-buffer [x] false)
 (defn chunk-cons [x y] false))

(eval-when
 (expand load eval)
 (defn map
   ;; "Returns a lazy sequence consisting of the result of applying f to
   ;; the set of first items of each coll, followed by applying f to the
   ;; set of second items in each coll, until any one of the colls is
   ;; exhausted.  Any remaining items in other colls are ignored. Function
   ;; f should accept number-of-colls arguments. Returns a transducer when
   ;; no collection is provided."
   ;; {:added "1.0"
   ;;  :static true}
   ([f]
    (fn [rf]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (rf result (f input)))
        ([result input & inputs]
         (rf result (apply f input inputs))))))
   ([f coll]
    (lazy-seq
     (when-let [s (seq coll)]
       (if (chunked-seq? s)
         (let [c (chunk-first s)
               size (int (count c))
               b (chunk-buffer size)]
           (dotimes [i size]
             (chunk-append b (f (nth c i)))) ;; was (.nth c i)
           (chunk-cons (chunk b) (map f (chunk-rest s))))
         (cons (f (first s)) (map f (rest s)))))))
   ([f c1 c2]
    (lazy-seq
     (let [s1 (seq c1) s2 (seq c2)]
       (when (and s1 s2)
         (cons (f (first s1) (first s2))
               (map f (rest s1) (rest s2)))))))
   ([f c1 c2 c3]
    (lazy-seq
     (let [s1 (seq c1) s2 (seq c2) s3 (seq c3)]
       (when (and  s1 s2 s3)
         (cons (f (first s1) (first s2) (first s3))
               (map f (rest s1) (rest s2) (rest s3)))))))
   ([f c1 c2 c3 & colls]
    (let [step (fn step [cs]
                 (lazy-seq
                  (let [ss (map seq cs)]
                    (when (every? identity ss)
                      (cons (map first ss) (step (map rest ss)))))))]
      (map #(apply f %) (step (conj colls c3 c2 c1)))))))

(defn mapcat
  ;; "Returns the result of applying concat to the result of applying map
  ;; to f and colls.  Thus function f should return a collection. Returns
  ;; a transducer when no collections are provided"
  ;; {:added "1.0"
  ;;  :static true}
  ;; ([f] (comp (map f) cat))
  ([f & colls]
     (apply concat (apply map f colls))))

(defn filter
  ;; "Returns a lazy sequence of the items in coll for which
  ;; (pred item) returns logical true. pred must be free of side-effects.
  ;; Returns a transducer when no collection is provided."
  ;; {:added "1.0"
  ;;  :static true}
  ([pred]
    (fn [rf]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
           (if (pred input)
             (rf result input)
             result)))))
  ([pred coll]
   (lazy-seq
    (when-let [s (seq coll)]
      (if (chunked-seq? s)
        (let [c (chunk-first s)
              size (count c)
              b (chunk-buffer size)]
          (dotimes [i size]
              (let [v (nth c i)] ;; was (.nth c i)
                (when (pred v)
                  (chunk-append b v))))
          (chunk-cons (chunk b) (filter pred (chunk-rest s))))
        (let [f (first s) r (rest s)]
          (if (pred f)
            (cons f (filter pred r))
            (filter pred r))))))))

(defn remove
  ;; "Returns a lazy sequence of the items in coll for which
  ;; (pred item) returns logical false. pred must be free of side-effects.
  ;; Returns a transducer when no collection is provided."
  ;; {:added "1.0"
  ;;  :static true}
  ([pred] (filter (complement pred)))
  ([pred coll]
     (filter (complement pred) coll)))

(defn merge-with
  ;; "Returns a map that consists of the rest of the maps conj-ed onto
  ;; the first.  If a key occurs in more than one map, the mapping(s)
  ;; from the latter (left-to-right) will be combined with the mapping in
  ;; the result by calling (f val-in-result val-in-latter)."
  ;; {:added "1.0"
  ;;  :static true}
  [f & maps]
  (when (some identity maps)
    (let [merge-entry (fn [m e]
			(let [k (key e) v (val e)]
			  (if (contains? m k)
			    (assoc m k (f (get m k) v))
			    (assoc m k v))))
          merge2 (fn [m1 m2]
		   (reduce merge-entry (or m1 {}) (seq m2)))]
      (reduce merge2 maps))))

(defn nthnext
  ;; "Returns the nth next of coll, (seq coll) when n is 0."
  ;; {:added "1.0"
  ;;  :static true}
  [coll n]
  (loop [n n xs (seq coll)]
      (if (and xs (pos? n))
        (recur (dec n) (next xs))
        xs)))

(eval-when
 (expand load eval)
 (defn nthrest
   ;; "Returns the nth rest of coll, coll when n is 0."
   ;; {:added "1.3"
   ;;  :static true}
   [coll n]
   (loop [n n xs coll]
     (let [result
           (if-let [xs (and (pos? n) (seq xs))]
             (recur (dec n) (rest xs))
             (do
               xs))]
       result))))

(eval-when
 (expand load eval)
 (defn partition
   ;; "Returns a lazy sequence of lists of n items each, at offsets step
   ;; apart. If step is not supplied, defaults to n, i.e. the partitions
   ;; do not overlap. If a pad collection is supplied, use its elements as
   ;; necessary to complete last partition upto n items. In case there are
   ;; not enough padding elements, return a partition with less than n items."
   ;; {:added "1.0"
   ;;  :static true}
   ([n coll]
    (partition n n coll))
   ([n step coll]
    (lazy-seq
     (when-let [s (seq coll)]
       (let [p (doall (take n s))]
         (when (= n (count p))
           (cons p (partition n step (nthrest s step))))))))
   ([n step pad coll]
    (lazy-seq
     (when-let [s (seq coll)]
       (let [p (doall (take n s))]
         (if (= n (count p))
           (cons p (partition n step pad (nthrest s step)))
           (list (take n (concat p pad))))))))))

(defn tree-seq
  "Returns a lazy sequence of the nodes in a tree, via a depth-first walk.
   branch? must be a fn of one arg that returns true if passed a node
   that can have children (but may not).  children must be a fn of one
   arg that returns a sequence of the children. Will only be called on
   nodes for which branch? returns true. Root is the root node of the
  tree."
  {:added "1.0"
   :static true}
  [branch? children root]
  (let [walk (fn walk [node]
               (lazy-seq
                (cons node
                      (when (branch? node)
                        (mapcat walk (children node))))))]
    (walk root)))

(defn flatten
  "Takes any nested combination of sequential things (lists, vectors,
  etc.) and returns their contents as a single, flat lazy sequence.
  (flatten nil) returns an empty sequence."
  {:added "1.2"
   :static true}
  [x]
  (filter (complement sequential?)
          (rest (tree-seq sequential? seq x))))

(defn group-by
  "Returns a map of the elements of coll keyed by the result of
  f on each element. The value at each key will be a vector of the
  corresponding elements, in the order they appeared in coll."
  {:added "1.2"
   :static true}
  [f coll]
  (reduce (fn [ret x]
            (let [k (f x)]
              (assoc ret k (conj (get ret k []) x))))
          {}
          coll))

(defn partition-by
  "Applies f to each value in coll, splitting it each time f returns a
   new value.  Returns a lazy seq of partitions.  Returns a stateful
   transducer when no collection is provided."
  {:added "1.2"
   :static true}
  ([f coll]
   (lazy-seq
    (when-let [s (seq coll)]
      (let [fst (first s)
            fv (f fst)
            run (cons fst (take-while #(= fv (f %)) (next s)))]
        (cons run (partition-by f (lazy-seq (drop (count run) s)))))))))

(defn frequencies
  "Returns a map from distinct items in coll to the number of times
  they appear."
  {:added "1.2"
   :static true}
  [coll]
  (reduce (fn [counts x]
            (assoc counts x (inc (get counts x 0))))
          {}
          coll))

(defn partition-all
  "Returns a lazy sequence of lists like partition, but may include
  partitions with fewer than n items at the end.  Returns a stateful
  transducer when no collection is provided."
  {:added "1.2"
   :static true}
  ([n coll]
   (partition-all n n coll))
  ([n step coll]
   (lazy-seq
    (when-let [s (seq coll)]
      (let [seg (doall (take n s))]
        (cons seg (partition-all n step (nthrest s step))))))))

(defn keep
  "Returns a lazy sequence of the non-nil results of (f item). Note,
  this means false return values will be included.  f must be free of
  side-effects.  Returns a transducer when no collection is provided."
  {:added "1.2"
   :static true}
  ([f coll]
   (lazy-seq
    (when-let [s (seq coll)]
      (let [x (f (first s))]
        (if (nil? x)
          (keep f (rest s))
          (cons x (keep f (rest s)))))))))

(defn keep-indexed
  "Returns a lazy sequence of the non-nil results of (f index item). Note,
  this means false return values will be included.  f must be free of
  side-effects.  Returns a stateful transducer when no collection is
  provided."
  {:added "1.2"
   :static true}
  ([f coll]
   (letfn [(keepi [idx coll]
             (lazy-seq
              (when-let [s (seq coll)]
                (let [x (f idx (first s))]
                  (if (nil? x)
                    (keepi (inc idx) (rest s))
                    (cons x (keepi (inc idx) (rest s))))))))]
     (keepi 0 coll))))

(defn every-pred
  "Takes a set of predicates and returns a function f that returns true
  if all of its composing predicates return a logical true value
  against all of its arguments, else it returns false. Note that f is
  short-circuiting in that it will stop execution on the first
  argument that triggers a logical false result against the original
  predicates."
  {:added "1.3"}
  ([p]
   (fn ep1
     ([] true)
     ([x] (boolean (p x)))
     ([x y] (boolean (and (p x) (p y))))
     ([x y z] (boolean (and (p x) (p y) (p z))))
     ([x y z & args] (boolean (and (ep1 x y z)
                                   (every? p args))))))
  ([p1 p2]
   (fn ep2
     ([] true)
     ([x] (boolean (and (p1 x) (p2 x))))
     ([x y] (boolean (and (p1 x) (p1 y) (p2 x) (p2 y))))
     ([x y z] (boolean (and (p1 x) (p1 y) (p1 z) (p2 x) (p2 y) (p2 z))))
     ([x y z & args] (boolean (and (ep2 x y z)
                                   (every? #(and (p1 %) (p2 %)) args))))))
  ([p1 p2 p3]
   (fn ep3
     ([] true)
     ([x] (boolean (and (p1 x) (p2 x) (p3 x))))
     ([x y] (boolean (and (p1 x) (p2 x) (p3 x) (p1 y) (p2 y) (p3 y))))
     ([x y z] (boolean (and (p1 x) (p2 x) (p3 x) (p1 y) (p2 y) (p3 y) (p1 z) (p2 z) (p3 z))))
     ([x y z & args] (boolean (and (ep3 x y z)
                                   (every? #(and (p1 %) (p2 %) (p3 %)) args))))))
  ([p1 p2 p3 & ps]
   (let [ps (list* p1 p2 p3 ps)]
     (fn epn
       ([] true)
       ([x] (every? #(% x) ps))
       ([x y] (every? #(and (% x) (% y)) ps))
       ([x y z] (every? #(and (% x) (% y) (% z)) ps))
       ([x y z & args] (boolean (and (epn x y z)
                                     (every? #(every? % args) ps))))))))

(defn some-fn
  "Takes a set of predicates and returns a function f that returns the
  first logical true value returned by one of its composing predicates
  against any of its arguments, else it returns logical false. Note
  that f is short-circuiting in that it will stop execution on the
  first argument that triggers a logical true result against the
  original predicates."
  {:added "1.3"}
  ([p]
   (fn sp1
     ([] nil)
     ([x] (p x))
     ([x y] (or (p x) (p y)))
     ([x y z] (or (p x) (p y) (p z)))
     ([x y z & args] (or (sp1 x y z)
                         (some p args)))))
  ([p1 p2]
   (fn sp2
     ([] nil)
     ([x] (or (p1 x) (p2 x)))
     ([x y] (or (p1 x) (p1 y) (p2 x) (p2 y)))
     ([x y z] (or (p1 x) (p1 y) (p1 z) (p2 x) (p2 y) (p2 z)))
     ([x y z & args] (or (sp2 x y z)
                         (some #(or (p1 %) (p2 %)) args)))))
  ([p1 p2 p3]
   (fn sp3
     ([] nil)
     ([x] (or (p1 x) (p2 x) (p3 x)))
     ([x y] (or (p1 x) (p2 x) (p3 x) (p1 y) (p2 y) (p3 y)))
     ([x y z] (or (p1 x) (p2 x) (p3 x) (p1 y) (p2 y) (p3 y) (p1 z) (p2 z) (p3 z)))
     ([x y z & args] (or (sp3 x y z)
                         (some #(or (p1 %) (p2 %) (p3 %)) args)))))
  ([p1 p2 p3 & ps]
   (let [ps (list* p1 p2 p3 ps)]
     (fn spn
       ([] nil)
       ([x] (some #(% x) ps))
       ([x y] (some #(or (% x) (% y)) ps))
       ([x y z] (some #(or (% x) (% y) (% z)) ps))
       ([x y z & args] (or (spn x y z)
                           (some #(some % args) ps)))))))
