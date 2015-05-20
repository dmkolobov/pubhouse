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
  (-> path (relative-path) (strip-extensions) (str ".html")))

(defn file-mapping
  [info root file]
  (let [path (.getPath file)]
    (fs/with-cwd root
      [path (merge {:url (path->url path)} (info file))])))

(defn site-mapping
  "Creates a lazy sequence of [path url] pairs for each file in the directory
  root-path which matches the predicate source-file?. The url returned is relative 
  to the root url, whereas the path is absolute."
  [source-file? info root-path]
  (let [root (fs/file root-path)]
    (->> (file-seq root)
         (filter source-file?)
         (map #(file-mapping info root %)))))

(defn site-nav
  "Convert a url relative to the root of the site into a sequence of its path parts
  ,omitting any extensions in the url."
  [url]
  (-> url (strip-extensions) (fs/split)))

(defn canonical-url
  "Given some url, ensures that it is an absolute url. If url is a path to
  some index.html, return a url to the parent directory."
  [url]
  (str "/" (-> url (clojure.string/replace "index.html" "") (strip-extensions))))

(defn mapping->site
  "Convert a sequence of [path url] pairs to a map structure representing
  the directory layout of the site."
  [mapping]
  (reduce (fn [site [path inf]]
            (assoc-in site
                      (site-nav (:url inf))
                      (update-in inf [:url] canonical-url)))
          {}
          mapping))

(defn html-writer
  "Given the path to the build directory and url relative to the site root,
  open a writer to the resulting file. Ensures that parent directories of 
  the site exist."
  [build-root url]
  (let [file (fs/with-cwd build-root (fs/file url))
        parent (fs/parent file)]
    (when (not (fs/exists? parent)) (fs/mkdirs parent))
    (io/writer file)))

(defn compile-site
  [f {:keys [site-root build-root source-file? info]}]
  (let [site-map (site-mapping source-file? info site-root)
        site (mapping->site site-map)]
    (doseq [[path {:keys [url]}] site-map]
      (with-open [reader (io/reader (fs/file path))
                  writer (html-writer build-root url)]
        (f (assoc site
                  :current-page
                  (get-in site (site-nav url)))
           reader
           writer)))))
      
