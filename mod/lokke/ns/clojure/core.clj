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
