;;; Copyright (C) 2020 Rob Browning <rlb@defaultvalue.org>
;;; SPDX-License-Identifier: LGPL-2.1-or-later OR EPL-1.0+

(ns lokke.deps
  (:require
   [clojure.string :as str]
   [guile.ice-9.iconv :refer [bytevector->string]]
   [guile
    :refer [dirname
            chdir
            closedir
            getcwd
            opendir
            readdir
            rename-file
            stat
            stat:type
            symlink]]
   [guile.lokke.config :refer [add-to-guile-load-path ensure-cache-dir]]
   [lokke.exception :refer [with-final]]
   [lokke.shell :refer [sh]]))

;; Store dependencies in XDG_CACHE_DIR/lokke (referred to below as
;; CACHE).  Since lokke namespaces must be located underneath (lokke
;; ns) in the guile module hierarchy, use symlinks to create paths
;; with the lokke/ns prefix (suitable for the guile %load-path) that
;; correspond to those listed as deps.edn :paths.
;;
;; Clone git/url: repos to CACHE/repo/COORDINATES.
;; For example: CACHE/repo/org.clojure/tools.cli/
;;
;; So given this dep:
;;
;;   {:paths ["src"]  ;; For this example (it's actually src/main/clojure)
;;    :deps
;;    {org.clojure/tools.cli
;;     {:git/url "https://github.com/clojure/tools.cli.git"
;;      :sha "d633ef9b028ab452fa2c35b6520d7aed6cf7958e"
;;      :tag "tools.cli-1.0.194"}}}
;;
;; Clone the URL to CACHE/repo/org.clojure/tools.cli/ as a bare
;; repositoriy.
;;
;; Create a git worktree for d633ef9b028ab452fa2c35b6520d7aed6cf7958e
;; as CACHE/tree/d633ef9b028ab452fa2c35b6520d7aed6cf7958e/root.
;;
;; Create the necessary load path alias for src in :paths as
;;
;;   CACHE/tree/d633ef9b028ab452fa2c35b65/path/0/lokke/ns -> ../../../root/src
;;
;; Then CACHE/tree/d633ef9b028ab452fa2c35b65/path/0 can be added to
;; the path so that a reference to clojure.tools.cli can find 
;;
;;   .../d633ef9b028ab452fa2c35b65/path/0/lokke/ns/clojure/tools/cli.cljc
;;
;; via the symlink.
;;
;; When there are multiple :paths, create one symlink for each path as
;; HASH/path/{0,1,2,3}/lokke/ns.  The 0, 1, 2, 3 paths can then be
;; added to the %load-path in numerical order to provide access to the
;; namespaces under the correct (lokke ns) subtree in the guile module
;; path.

;; FIXME: error handling...
;; FIXME: edn/read-string, not read-string
;; FIXME: better edn content error checking

;; FIXME: support deps.edn system/user/etc. hierarchy
;; https://clojure.org/reference/deps_and_cli#_deps_edn_sources
;; Perhaps only when enabled (maybe #! and ./lok differ)?

;; FIXME: should we always generate load path from an edn traversal,
;; or could we get away with just scanning/trusting the HASH dirs?

(defn note [f & args]
  (binding [*out* *err*] (apply f args)))

(defn exc [& args]
  (let [result (apply sh args)]
    (when-not (zero? (:exit result))
      (note print (:err result))
      (throw (ex-info (str "Non-zero exit (" (:exit result) ") for "
                           (pr-str args))
                      {:kind ::non-zero-exit
                       ;; FIXME: perhaps not what we really want
                       :err (:err result)
                       :result result})))
    result))

(defn mkdir [& args]
  (apply exc "mkdir" args))

(defn rm [& args]
  (apply exc "rm" args))

(defn rm-rf [& args]
  (apply exc "rm" "-rf" args))

(defn mktemp [& args]
  ;; until guile has mktemp -d
  (let [p (-> (apply exc "mktemp" args)
              :out
              (bytevector->string "ascii"))]
    (subs p 0 (dec (count p)))))

(defn append-to-load-path [dir]
  (assert (str/starts-with? dir "/"))
  (add-to-guile-load-path dir))

(defn subdirs [path]
  (with-final [dir (opendir path) :always closedir]
    (doall (take-while #(not (guile/eof-object? %))
                       (repeatedly #(readdir dir))))))

(def all-digit-dir-rx (re-pattern "/\\d+$"))

(defn all-digit-dir? [path]
  (and (re-find all-digit-dir-rx path)
       (= 'directory (stat:type (stat path)))))

(defn git-dep-load-dirs [[spec info :as dep] cache]
  ;; FIXME: binary paths
  (let [paths (str cache "/tree/" (:sha info) "/path")
        edn (str cache "/tree/" (:sha info) "/root/deps.edn")]
    (into (vec (for [dir (subdirs paths)
                     :let [full (str paths "/" dir)]
                     :when (all-digit-dir? full)]
                 full))
          (when-let [edn (and (guile/file-exists? edn)
                              (read-string (slurp edn)))]
            (derive-load-path edn cache)))))

(defn derive-load-path [deps-edn cache]
  (reduce (fn [result [spec info :as dep]]
            (cond
              (:git/url info) (into result (git-dep-load-dirs dep cache))
              :else
              (do
                (note println "Unrecognized deps.edn entry:" (prn dep))
                (guile/exit 2))))
          ;; FIXME: why did a {} here cause (I think) an infloop?
          []
          (:deps deps-edn)))

(defn augment-load-path [deps-edn top]
  ;; FIXME: don't recompute everything, cache on disk like clj
  ;; FIXME: binary paths
  (doseq [path (derive-load-path deps-edn (ensure-cache-dir))]
    (append-to-load-path path))
  (when (guile/file-exists? ".lokke/path")
    (doseq [path (for [dir (subdirs ".lokke/path")
                       :let [full (str top "/.lokke/path/" dir)]
                       :when (all-digit-dir? full)]
                   full)]
      (append-to-load-path path))))

(defn link-dep-paths
  "For each path in paths, ensures that
  path-dir/ZERO-PADDED-INDEX/lokke/ns is symlinked to it, so that
  path-dir/* can be added to the guile %load-path, in numerical order
  to provide access to the paths under the (lokke ns) module subree.
  Assumes path-dir is empty."
  [paths path-dir relative-root]  

  ;; FIXME: is this right, i.e. does clj actually default to "."?
  (when (seq paths)
    (let [top (getcwd)]
      (when (some #(or (= % "..")
                       (str/starts-with? % "../")
                       (str/ends-with? % "/..")
                       (str/includes? % "/../"))
                  paths)
        ;; FIXME: include the dep name and path in the error
        (throw (ex-info "For now, dependency paths cannot contain ..\n")
               {:kind ::exit
                :status 2}))
      (let [i-width (count (str (count paths)))]
        ;; Not thread safe
        (dorun
         (map-indexed (fn [i path]
                        (let [parent (str path-dir
                                          (format "/~v,'0d/lokke" i-width i))]
                          (mkdir "-p" parent)
                          (chdir parent)
                          (try
                            (symlink (str relative-root path) "ns")
                            (finally (chdir top)))))
                      paths))))))

(defn split-pathstr [s]
  (str/split s #"/+"))

;; FIXME: better error handling, e.g. report whole dep, friendlier, etc.

(defn validate-sha-syntax [s]
  (when-not (re-find #"^[0-9A-Fa-f]{40}$" s)
    (throw (ex-info (str "Invalid or incomplete SHA: " s) {:sha s}))))

(defn validate-spec [spec]
  ;; strict for now
  (when-not (symbol? spec)
    (throw (ex-info "git dependency library is not a symbol" spec)))
  (let [s (str spec)]
    (when (str/starts-with? s "/")
      (throw (ex-info "git dependency library name starts with /" spec)))
    (when (str/includes? s "//")
      (throw (ex-info "git dependency library name includes \"//\"" spec)))
    (when (or (str/includes? s "/../")
              (str/starts-with? s "../")
              (str/ends-with? s "/.."))
      (throw (ex-info "git dependency library name includes \"..\"" spec)))))


(defn require-cached-git-tree
  [spec {url :git/url oidx :sha tag :tag}]
  ;; See the top of this file for an overview.
  ;; Providing the advisory tag is recommended since it may make the
  ;; clone much faster.
  ;; FIXME: option to avoid shallow clones (e.g. for offline uses)
  (note print "dep:" url tag)
  (validate-spec spec)
  (validate-sha-syntax oidx)
  (with-final [cache (ensure-cache-dir)
               repo (str cache "/repo/" spec)
               spec-parts (split-pathstr (str spec))
               ;; FIXME: not sure git actually preserves the relative
               ;; structure.  If not, this may be pointless.
               relative-root (str/join "/" (concat (repeat (inc (count spec-parts))
                                                           "..")
                                                   ["tree" oidx "root"]))
               tree (str cache "/tree/" oidx)
               root (str tree "/root")
               path (str tree "/path")
               edn (str tree "/root/deps.edn")
               _ (mkdir "-p" (dirname repo))
               repo-tmp (mktemp "-d" (str repo "-XXXXXX-tmp")) :always rm-rf
               _ (mkdir "-p" (dirname path))
               path-tmp (mktemp "-d" (str path "-XXXXXX-tmp")) :always rm-rf]
    ;; FIXME: unify somehow with (clojure/jvm) ~/.gitlibs/ _repo or libs?
    (when-not (guile/file-exists? path)
      (when-not (guile/file-exists? root)
        (when-not (guile/file-exists? repo)
          (if-not tag
            (exc "git" "clone" "--bare" url repo-tmp)
            (exc "git" "clone" "--bare" "--depth" "1"
                 "--single-branch" "--branch" tag url repo-tmp)))
        ;; FIXME: do something else about 'syatem-error EEXIST?
        (rm-rf repo)
        (rename-file repo-tmp repo)
        (rm-rf root)
        (exc "git" "-C" repo "worktree" "add" relative-root oidx))
      (link-dep-paths (when (guile/file-exists? edn)
                        (:paths (read-string (slurp edn))))
                      path-tmp "../../../root/")
      (rm-rf path)
      (rename-file path-tmp path))
    ;; (exc "chmod" "-R" "a-w" tree)?
    (note println)))

(defn read-dep-source [path]
  (let [deps (read-string (slurp path))]
    (assert (:deps deps))
    deps))

(defn ensure-top-level-paths [deps top]
  ;; FIXME: ok to assume we're still in the top-level dir?
  (when (seq (:paths deps))
    (with-final [dot (str top "/.lokke")
                 _ (mkdir "-p" dot)
                 path (str dot "/path")
                 path-tmp (mktemp "-d" (str dot "/path-XXXXXX-tmp")) :always rm-rf]
      (rm-rf path)
      (link-dep-paths (:paths deps) path-tmp "../../../../")
      (rename-file path-tmp path))))

(defn load-deps-files [sources]
  ;; Each source is [path deps:-dep?]
  (when (seq sources)
    (let [top (getcwd)
          deps-forms (map read-dep-source sources)
          deps (apply merge-with merge deps-forms)
          ;; Last :paths wins
          deps (if-let [p (->> deps-forms reverse (keep :paths) first)]
                 (assoc deps :paths p)
                 deps)]
      (try
        (doseq [[lib info :as dep] (:deps deps)]
          (cond
            (:git/url info) (require-cached-git-tree lib info)
            :else
            (do
              (note println "Unrecognized deps.edn entry:" (prn dep))
              (guile/exit 2))))
        ;; FIXME: unify?
        (ensure-top-level-paths deps top)
        (augment-load-path deps top)
        (catch ExceptionInfo ex
          (let [data (ex-data ex)]
            (when (= ::exit (:kind ex))
              (note println (ex-message ex))
              (guile.exit/exit (:status ex))))
          (throw ex))))))
