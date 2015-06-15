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
  [root path]
  (fs/with-cwd root
    (-> path (relative-path) (strip-extensions) (str ".html"))))

(defn mk-page-record
  [analyze-page root file]
  (let [path (.getPath file)]
    [path (merge {:url (path->url root path)} (analyze-page file))]))

(defn site-mapping
  "Creates a lazy sequence of [path url] pairs for each file in the directory
  root-path which matches the predicate page-file?. The url returned is relative 
  to the root url, whereas the path is absolute."
  [page-file? analyze-page root-path]
  (let [root (fs/file root-path)]
    (->> (file-seq root)
         (filter page-file?)
         (map #(mk-page-record analyze-page root %)))))

(defn- page-key
  "Convert a url relative to the root of the site into a sequence of its path parts
  ,omitting any extensions in the url. For use with Clojure's *-in functions."
  [url]
  (-> url (strip-extensions) (fs/split)))

(defn canonical-url
  "Given some url, ensures that it is an absolute url. If url is a path to
  some index.html, return a url to the parent directory."
  [url]
  (str "/" (-> url (clojure.string/replace "index.html" "") (strip-extensions))))

(defn add-page
  "Helper for adding a page map to the site map."
  [site page-info]
  (assoc-in site
            (page-key (:url page-info))
            (update-in page-info [:url] canonical-url)))

(defn page?
  "Helper predicate for testing whether an object is a map representing a page."
  [x] (contains? x :url))

(declare fixup-site)

(defn add-all-pages
  "Helper for adding a vector of child pages to a directory map entry under the
  key :all."
  [[name val]]
  [name
   (if (and (map? val) (not (page? val)))
     (fixup-site (assoc val :all (->> val (map last) (filter page?))))
     val)])

(defn fixup-site
  [x]
  (clojure.walk/walk add-all-pages identity x))

(defn mapping->site
  "Convert a sequence of [path url] pairs to a map structure representing
  the directory layout of the site."
  [mapping]
  (fixup-site (reduce #(add-page %1 (last %2)) {} mapping)))

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
  "Helper function which actually compiles the content."
  [site-map compile mk-output]
  (let [site (mapping->site site-map)]
    (doseq [[path {:keys [url]}] site-map]
      (compile (navigate site url) (fs/file path) (mk-output url)))))

(defn compiler
  "Creates a static site compiles.

  The function 'page-file?' is a predicate which takes a file as an argument, and 
  returns true if the file is a content file. 

  The function 'analyze-page' takes as an argument a content file, and should 
  nil or a map of data associated with the file. Useful for reading Jekyll style
  headers. 

  The function 'compile-page' takes as argument a hash representing the site 
  structure , the input content file, and the output file with an extension of
  'file-type'."
  [file-type page-file? analyze-page compile-page]
  (fn [site-root build-root]
    (run-compiler (site-mapping page-file? analyze-page site-root)
                  compile-page
                  (partial output-file build-root file-type))))


