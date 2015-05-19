(ns pubhouse.core
  (:require [clojure.string :refer [join]]
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

(defn canonical-url
  "Given some url, ensures that it is an absolute url. If url is a path to
  some index.html, return a url to the parent directory."
  [url]
  (str "/" (-> url (clojure.string/replace "index.html" "") (strip-extensions))))

(defn file-mapping
  [root file]
  (let [path (.getPath file)]
    (fs/with-cwd root [path (path->url path)])))

(defn site-mapping
  [source-file? root-path]
  (let [root (fs/file root-path)]
    (->> (file-seq root)
         (filter source-file?)
         (map (partial file-mapping root)))))

(defn read-info
  "Reads the lines preceding the info block separator '===' in as a 
  Clojure data structure."
  [reader]
  (let [not-sep #(not= % "===")]
    (->> (line-seq reader) (take-while not-sep) (join "\n") (read-string))))

(defn make-page
  [path url]
  {:url (canonical-url url)
   :info (with-open [reader (clojure.java.io/reader (fs/file path))]
           (read-info reader))})

(defn resource-key
  [url]
  (-> url (strip-extensions) (fs/split)))

(defn mapping->site
  [mapping]
  (reduce (fn [site [path url]]
            (assoc-in site (resource-key url) (make-page path url)))
          {}
          mapping))

(defn html-writer
  [build-root url]
  (let [file (fs/with-cwd build-root (fs/file url))
        parent (fs/parent file)]
    (when (not (fs/exists? parent)) (fs/mkdirs parent))
    (clojure.java.io/writer file)))

(defn compile-site
  [f {:keys [site-root build-root source-file?]}]
  (let [site-map (site-mapping source-file? site-root)
        site (mapping->site site-map)]
    (doseq [[path url] site-map]
      (with-open [reader (clojure.java.io/reader (fs/file path))
                  writer (html-writer build-root url)]
        (f (assoc site
                  :current-page
                  (get-in site (resource-key url)))
           reader
           writer)))))
      
