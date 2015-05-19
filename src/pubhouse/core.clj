(ns pubhouse.core
  (:require [clojure.java.io :refer [as-file]]
            [clojure.string :refer [join]]
            [markdown.core :refer [md-to-html-string]]
            [hiccup.core :as hiccup]
            [me.raynes.fs :as fs]))

(def ^:dynamic *source-file?* nil)
(def ^:dynamic *render* nil)
(def ^:dynamic *meta-sep*  "===")

(defn relative-path
  "Takes a path as the sole argument and returns path with any parts
  matching the current *cwd* removed. Requires path to be a child of 
  *cwd*."
  [path]
  (->> (fs/split path)
       (drop (count (fs/split fs/*cwd*)))
       (clojure.string/join "/")))

(defn ensure-parent-dir
  "Invoke the function f, ensuring that the parent directory of the file at path
  exists. Presumabely, f has side-effects involving writing to the file at path."
  [path f]
  (let [parent (fs/parent path)]
    (if (fs/exists? parent)
      (f)
      (do (fs/mkdirs parent) (f)))))

(def content-file?
  "Any files for which this predicate returns true will be compiled."
  (complement fs/directory?))

(defn get-file-info
  [file]
  {:path (relative-path (.getPath file))
   :mod-time (fs/mod-time file)})

(defn get-page-info
  "Reads the lines preceding the *meta-sep* line as a persistent map."
  [lines]
  (->> lines (take-while #(not= % *meta-sep*)) (join "\n") (read-string)))

(defn content-section
  "Returns a lazy sequence of lines following the *meta-sep* line."
  [lines]
  (->> lines (drop-while #(not= % *meta-sep*)) (drop 1)))

(defn strip-extension
  [s]
  (if-let [e (fs/extension s)] (clojure.string/replace s e "") s))

(defn with-io
  "Creates a reader and writer using the input and output files, and invokes the 
  f with the arguments (reader writer). Ensures that the parent directory of the output
  file exists."
  [input output f]
  (ensure-parent-dir output
    (fn []
      (with-open [reader (clojure.java.io/reader input)
                  writer (clojure.java.io/writer output)]
        (f reader writer)))))

(defn input-file
  "Give a path to the root of the site directory and the relative path
  of a resource source file, return that resource source file."
  [site-root rel-path]
  (fs/with-cwd site-root
    (fs/with-cwd "content"
      (fs/file rel-path))))

(defn output-file
  "Given a path to the root of the build directory and the URL of
  a resource, return thefile to be used for output of resource compilation."
  [build-path url]
  (fs/with-cwd build-path
    (fs/file (str url ".html"))))

(defn canonical-url
  [url]
  (let [url (str "/" url)]
    (clojure.string/replace url "index" "")))

(defn make-page
  [file-info page-info]
  (merge page-info
         {:url (canonical-url
                (strip-extension
                 (:path file-info)))}))

(defn compile-file!
  [{:keys [site-path build-path]} site [file-info page-info]]
  (let [path (:path file-info)
        input (input-file site-path path)
        output (output-file build-path (strip-extension path))]
    (with-io input output
      (fn [reader writer]
        (.write writer
                (*render* file-info
                          (assoc site
                                 :current-page
                                 (assoc (make-page file-info page-info)
                                        :content-lines
                                        (content-section (line-seq reader))))))))))

(defn build-site-map!
  [options]
  (fs/with-cwd (:site-path options)
    (fs/with-cwd "content"
      (doall
       (for [file (->> (fs/file ".") (file-seq) (filter *source-file?*))]
         (with-open [reader (clojure.java.io/reader file)]
           [(get-file-info file) (get-page-info (line-seq reader))]))))))

(defn content->site
  [site-map]
  (reduce (fn [site [file-info page-info]]
            (assoc-in site
                      (fs/split
                       (strip-extension (:path file-info)))
                      (make-page file-info page-info)))
          {}
          site-map))
  
(defn compile-content!
  [options site-map]
  (let [site (content->site site-map)]
    (doseq [content-map site-map]
      (compile-file! options site content-map))))

(defn build-site
  [options source-file? render]
  (let [options (assoc options :root fs/*cwd*)]
    (binding [*source-file?* source-file?
              *render* render]
      (compile-content! options (build-site-map! options)))))

