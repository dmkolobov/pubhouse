(ns pubhouse.core
  (:require [clojure.string :refer [join]]
            [clojure.java.io :as io]
            [me.raynes.fs :as fs]))

(defn relative-path
  "Returns the sub-string of path relative to the current-value of *cwd*.
  Assumes that path is a equal to or a child of *cwd*"
  [path]
  (->> (fs/split path) (drop (count (fs/split fs/*cwd*))) (join "/")))

(defn strip-extensions
  "Strips all extensions from the path string."
  [path]
  (if-let [ext (fs/extension path)]
    (recur (clojure.string/replace path ext ""))
    path))

(defn path->url
  "Convert a path to a url, with paths relative to the current value of *cwd*"
  [path]
  (-> path (relative-path) (strip-extensions) (str ".resource")))

(defn file-mapping
  [analyze-page root file]
  (let [path (.getPath file)]
    (fs/with-cwd root
      [path (merge {:url (path->url path)} (analyze-page file))])))

(defn site-mapping
  "Creates a lazy sequence of [path url] pairs for each file in the directory
  root-path which matches the predicate page-file?. The url returned is relative 
  to the root url, whereas the path is absolute."
  [page-file? analyze-page root-path]
  (let [root (fs/file root-path)]
    (->> (file-seq root)
         (filter page-file?)
         (map #(file-mapping analyze-page root %)))))

(defn page-key
  "Convert a url relative to the root of the site into a sequence of its path parts
  ,omitting any extensions in the url."
  [url]
  (-> url (strip-extensions) (fs/split)))

(defn canonical-url
  "Given some url, ensures that it is an absolute url. If url is a path to
  some index.html, return a url to the parent directory."
  [url]
  (str "/" (-> url (clojure.string/replace "index.resource" "") (strip-extensions))))

(defn add-page
  [site page-info]
  (assoc-in site (page-key (:url page-info)) (update-in page-info [:url] canonical-url)))

(defn mapping->site
  "Convert a sequence of [path url] pairs to a map structure representing
  the directory layout of the site."
  [mapping]
  (reduce #(add-page %1 (last %2)) {} mapping))

(defn navigate
  [site url]
  (assoc site :current-page (get-in site (page-key url))))

(defn output-file
  "Given the path to the build directory and url relative to the site root,
  open a writer to the resulting file. Ensures that parent directories of 
  the site exist."
  [build-root file-type url]
  (let [file (fs/with-cwd build-root
               (fs/file
                (str (strip-extensions url) "." file-type)))
        parent (fs/parent file)]
    (when (not (fs/exists? parent)) (fs/mkdirs parent))
    file))

(defn run-compiler
  [site-map compile mk-output]
  (let [site (mapping->site site-map)]
    (doseq [[path {:keys [url]}] site-map]
      (compile (navigate site url) (fs/file path) (mk-output url)))))

(defn compiler
  [file-type page-file? analyze-page compile-page]
  (fn [site-root build-root]
    (run-compiler (site-mapping page-file? analyze-page site-root)
                  compile-page
                  (partial output-file build-root file-type))))


