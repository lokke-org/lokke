;;; Copyright (C) 2020 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

(ns lokke.io
  (:require
   [guile.guile
    :refer [closedir eof-object? opendir readdir stat stat:mode stat:type]]
   [guile.lokke.io
    :refer [copy
            delete-file
            flush
            line-seq
            mkstemp
            read-line
            reader
            slurp
            slurp-bytes
            spit
            spit-bytes
            writer]]
   [guile.lokke.ns :refer [re-export]]
   [lokke.exception :refer [with-final]]))

(re-export copy
           delete-file
           flush
           line-seq
           mkstemp
           read-line
           reader
           slurp
           slurp-bytes
           spit
           spit-bytes
           writer)

(defn path-seq [dir]
  (tree-seq
   #(= 'directory (-> % stat stat:type))
   (fn [dir]
     (with-final [d (opendir dir) :always closedir]
       (doall
        (->> (repeatedly #(readdir d))
             (take-while #(not (eof-object? %)))
             (remove #{"." ".."})
             (map #(str dir "/" %))))))
   dir))
