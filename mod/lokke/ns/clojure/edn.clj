;;; Copyright (C) 2021 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

(ns clojure.edn
  (:refer-clojure
   :exclude [read read-string])
  (:require
   [guile.lokke.scm.edn :as scm-edn]))

(def- read-constructors
  (scm-edn/constructors :vector-init vector
                        :vector-add #(conj %2 %1)
                        :vector-finish identity
                        :set-init hash-set
                        :set-add #(conj %2 %1)
                        :set-finish identity
                        :map-init hash-map
                        :map-add assoc
                        :map-finish identity))

(def- inflexible-read
  (scm-edn/reader :constructors read-constructors))

(def- flexible-read
  (scm-edn/reader :fixed? false :constructors read-constructors))

(defn read
  ([] (inflexible-read *in*))
  ([stream] (inflexible-read stream))
  ([{:keys [eof readers default] :as opts} stream]
   (apply flexible-read
          stream
          (cond->> ()
            (contains? opts :eof) (guile/cons* :on-eof (constantly eof))
            (contains? opts :readers) (guile/cons* :tag-reader #(get readers %1))
            (contains? opts :default) (guile/cons* :default-tag-reader default)))))

(def- inflexible-read-string
  (scm-edn/string-reader :constructors read-constructors))

(def- flexible-read-string
  (scm-edn/string-reader :fixed? false :constructors read-constructors))

(defn read-string
  ([] (inflexible-read-string *in*))
  ([s] (inflexible-read-string s))
  ([{:keys [eof readers default] :as opts} s]
   (apply flexible-read-string
          s
          (cond->> ()
            (contains? opts :eof) (guile/cons* :on-eof (constantly eof))
            (contains? opts :readers) (guile/cons* :tag-reader #(get readers %1))
            (contains? opts :default) (guile/cons* :default-tag-reader default)))))
