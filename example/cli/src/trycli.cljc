
(ns trycli
  (:require
   [clojure.tools.cli :refer [parse-opts]]
   #?(:cljl [guile.guile :refer [string->number]])))

(def opt-spec
  [["-i" "--integer N" "An integer"
    :default 0
    :parse-fn #?(:cljc #(Integer/parseInt %)
                 :cljl #(string->number %))
    :validate [integer? "Must be a number"]]
   ["-v" nil "Verbosity level"
    :id :verbosity
    :default 0
    :update-fn inc]
   ["-h" "--help"]])

(defn -main [& args]
  (let [{:keys [options]} (parse-opts *command-line-args* opt-spec)]
    (println "integer:" (:integer options))
    (println "verbosity:" (:verbosity options))))
