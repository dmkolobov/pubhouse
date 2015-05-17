(ns pubhouse.core
  (:require [pubhouse.files :refer [with-parent-dir map-directory! do-directory!]]
            [clojure.java.io :refer [as-file]]
            [clojure.string :refer [join]]
            [markdown.core :refer [md-to-html-string]]
            [hiccup.core :as hiccup]
            [me.raynes.fs :as fs]))

(def ^:dynamic *site-map* nil)

(def ^:dynamic site nil)

(def ^:dynamic *meta-sep*  "===")

(def ^:dynamic *block-sep* "$$$")

(def content-file? (complement fs/directory?))

(defn mk-page-record
  [file lines]
  (merge (->> lines (take-while #(not= % *meta-sep*)) (join " ") (read-string))
         {:path (.getPath file) :mod-time (fs/mod-time file)}))

(defn content-section
  [lines]
  (->> lines (drop-while #(not= % *meta-sep*)) (drop 1)))

(defn page-record? [x] (and (coll? x) (contains? x :mod-time)))

(defn build-file!
  [file lines]
  [(mk-page-record file lines)
   (content-section lines)])

(defn build-files!
  [root-dir]
  (map-directory! build-file! content-file? root-dir))

(defn add-page
  [site-map page-record]
  (assoc-in site-map
            (map keyword (fs/split (:path page-record)))
            page-record))

(defn build-site-map!
  [root-dir]
  (reset! *site-map*
          (let [root-part (first (fs/split root-dir))]
            (get (reduce #(add-page %1 (first %2))
                         {}
                         (build-files! root-dir))
                 (keyword root-part)))))

(defn strip-extension
  [s]
  (if-let [e (fs/extension s)] (clojure.string/replace s e "") s))

(defn strip-file-info
  [page-record]
  (dissoc page-record :path :mod-time))

(defn humane-site-map
  [site-map]
  (clojure.walk/postwalk (fn [form]
                           (cond (page-record? form)
                                 (strip-file-info form)
                                 
                                 (keyword? form)
                                 (-> form name strip-extension keyword)

                                 :default form))
                         site-map))

(defmulti render-file
  (comp fs/extension :path))

(defmethod render-file ".md"
  [page-record content-lines]
  (hiccup/html
   [:html
    [:head [:title (:title page-record)]]
    [:body (md-to-html-string (join "\n" content-lines))]]))

(defn build-path
  [root path]
  (clojure.java.io/as-file
   (str (join "/" (cons root (drop 2 (fs/split (strip-extension path)))))
        ".html")))

(defn navigate-site-map
  [site-map page-record]
  (assoc (humane-site-map site-map)
         :current-page
         (strip-file-info page-record)))

(defn compile-file!
  [site-map build-root [page-record lines]]
  (let [output (build-path build-root (:path page-record))]
    (with-parent-dir output
      (fn []
        (with-open [writer (clojure.java.io/writer output)]
          (binding [site (navigate-site-map site-map page-record)]
            (.write writer (render-file page-record lines))))))))

(defn compile-content!
  [site-map content-root build-root]
  (do-directory! (comp (partial compile-file! site-map build-root)
                       build-file!)
                 content-file?
                 content-root))

(defn compile-site!
  [root-dir build-root]
  (binding [*site-map* (atom {})]
    (let [content-root (clojure.java.io/as-file
                        (join "/" (concat (fs/split root-dir)
                                          (list "content"))))]
      (build-site-map! content-root)
      (compile-content! @*site-map* content-root build-root))))
