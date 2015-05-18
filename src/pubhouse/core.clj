(ns pubhouse.core
  (:require [clojure.java.io :refer [as-file]]
            [clojure.string :refer [join]]
            [markdown.core :refer [md-to-html-string]]
            [hiccup.core :as hiccup]
            [me.raynes.fs :as fs]))

(def ^:dynamic *site-map* nil)

(def ^:dynamic site nil)

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

(defn build-site-map!
  []
  (reset! *site-map*
          (fs/with-cwd "content"
            (doall
             (for [file (->> (fs/file ".") (file-seq) (filter content-file?))]
               (with-open [reader (clojure.java.io/reader file)]
                 [(get-file-info file) (get-page-info (line-seq reader))]))))))

(defn strip-extension
  [s]
  (if-let [e (fs/extension s)] (clojure.string/replace s e "") s))

(defn make-page
  [file-info page-info]
  (merge page-info
         {:url (strip-extension (:path file-info))}))

(defmulti render-file (fn [ext _ _] ext))

(defmethod render-file ".md"
  [_ page-record content-lines]
  (hiccup/html
   [:html
    [:head [:title (:title page-record)]]
    [:body
     (md-to-html-string (join "\n" content-lines))
     [:div (:url page-record)]
     [:div (get-in site [:current-page :url])]]]))

(defn pages->site
  [pages]
  (reduce #(assoc-in %1 (fs/split (:url %2)) %2) {} pages))

(defn with-file-ends
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
  (fs/with-cwd (str (fs/normalized site-root) "/content")
    (fs/file rel-path)))

(defn output-file
  "Given a path to the root of the build directory and the URL of
  a resource, return thefile to be used for output of resource compilation."
  [build-path url]
  (fs/with-cwd build-path (fs/file (str url ".html"))))

(defn compile-content!
  [site-path site-map build-path]
  (let [site (pages->site (map (partial apply make-page) site-map))]
    (doseq [[file-info page-info] site-map]
      (let [page (make-page file-info page-info)
            input (input-file site-path (:path file-info))
            output (output-file build-path (:url page))]
        (with-file-ends input output
          (fn [reader writer]
            (binding [site (assoc site :current-page page)]
              (.write writer
                      (render-file (fs/extension (:path file-info))
                                   page
                                   (content-section (line-seq reader)))))))))))

(defn compile-site!
  [site-path build-root]
  (binding [*site-map* (atom {})]
    (fs/with-cwd site-path (build-site-map!))
    (compile-content! site-path @*site-map* build-root)))
