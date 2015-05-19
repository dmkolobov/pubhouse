(ns pubhouse.site
  (:require [me.raynes.fs :as fs]))

(defn relative-path
  "Returns the sub-string of path relative to the current-value of *cwd*.
  Assumes that path is a equal to or a child of *cwd*"
  [path]
  (->> (fs/split path)
       (drop (count (fs/split fs/*cwd*)))
       (clojure.string/join "/")))

(defn strip-extensions
  "Strips all extensions from the path string."
  [path]
  (if-let [ext (fs/extension path)]
    (recur (clojure.string/replace path ext ""))
    path))

(defn canonical-url
  "Given some url, ensures that it is an absolute url. Also, if
   the url has the form path/to/category/index, then /path/to/category/
   will be returned."
  [url]
  (str "/" (clojure.string/replace url "index" "")))

(defn path->url
  "Convert a path to a url, with paths relative to the current value of *cwd*"
  [path]
  (-> path (relative-path) (strip-extension) (canonical-url)))

(defn file-mapping
  [root file]
  (let [path (.getPath file)]
    (fs/with-cwd root [path (path->url path)])))

(defn site-mapping
  [source-file? path]
  (let [root (fs/with-cwd path (fs/with-cwd "content" (fs/file ".")))]
    (->> (file-seq root)
         (filter source-file?)
         (map (partial file-mapping root)))))
