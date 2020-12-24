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
