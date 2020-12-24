;;; Copyright (C) 2020 Rob Browning <rlb@defaultvalue.org>
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
