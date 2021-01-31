;;; Copyright (C) 2019 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

(ns clojure.core
  (:use
   [clojure.core.epl]
   [guile.lokke.core]))

;; After loading epl, let, etc. should finally be full-featured,
;; including destructuring, etc.

(guile.lokke.ns/re-export-all! '(lokke core))
(guile.lokke.ns/re-export-all! '(lokke ns clojure core epl))

(defn replace [smap coll]
  (into (empty coll)
        (map #(get smap % %) coll)))

(defn map-indexed [f coll]
  (map f (range) coll))

(defdyn unquote
  (fn [exp]
    (guile/error "unquote invocation outside syntax-quote")))

(defdyn unquote-splicing
  (fn [exp]
    (guile/error "unquote-splicing invocation outside syntax-quote")))

(defmacro syntax-quote
  [exp]
  (defn container? [x] (or (list? x) (vector? x) (map? x) (set? x)))
  (defn unquote-splicing? [x] (and (list? x) (= 'unquote-splicing (first x))))
  (defn insist-unary [name exp]
    (when-not (= 2 (count exp))
      (guile/error (str "multiple expressions in " name " form"))))
  (defn expand [exp splicing-parent?]
    (let [splices? (and (container? exp) (some unquote-splicing? exp))]
      (defn maybe-wrap [exp]
        (if splicing-parent?
          (list 'clojure.core/list exp)
          exp))
      (defn maybe-splice [exps]
        (if-not splices?
          (cons 'clojure.core/list exps)
          (list 'clojure.core/apply 'clojure.core/concat
                (cons 'clojure.core/list exps))))
      (cond
        (list? exp)
        (if (empty? exp)
          ;; FIXME: (list 'quote ...)?
          (quote (quote ()))
          (case (first exp)
            (unquote)
            (do
              (insist-unary 'unquote exp)
              (maybe-wrap (second exp)))
            (unquote-splicing)
            (do
              (insist-unary 'unquote-splicing exp)
              (second exp))
            (maybe-wrap (maybe-splice (map #(expand % splices?) exp)))))

        ;; Q: could we use empty here?
        (vector? exp)
        (maybe-wrap
         (list 'clojure.core/vec (maybe-splice (map #(expand % splices?) exp))))

        (map? exp)
        (maybe-wrap
         (list 'clojure.core/into {}
               (maybe-splice (map #(expand % splices?) exp))))

        (set? exp)
        (maybe-wrap
         (list 'clojure.core/into #{}
               (maybe-splice (map #(expand % splices?) exp))))

        :else (maybe-wrap (list 'quote exp)))))

  (when (unquote-splicing? exp)
    (guile/error "unquote-splicing at top level"))
  (expand exp false))
