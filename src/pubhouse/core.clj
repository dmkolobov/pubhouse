(ns pubhouse.core
  (:require [pubhouse.files :refer [rel-path with-parent-dir map-directory! do-directory!]]
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

(defn get-file-info
  [file]
  {:path (rel-path (.getPath file))
   :mod-time (fs/mod-time file)})

(defn meta-section
  [lines]
  (->> lines (take-while #(not= % *meta-sep*))))

(defn content-section
  [lines]
  (->> lines (drop-while #(not= % *meta-sep*)) (drop 1)))

(defn build-site-map!
  []
  (reset! *site-map*
          (fs/with-cwd "content"
            (doall
             (for [file (->> (fs/file ".") (file-seq) (filter content-file?))]
               (with-open [reader (clojure.java.io/reader file)]
                 [(get-file-info file)
                  (->> (meta-section (line-seq reader))
                       (join "\n")
                       (read-string))]))))))

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
  [input output f]
  (with-parent-dir output
    (fn []
      (with-open [reader (clojure.java.io/reader input)
                  writer (clojure.java.io/writer output)]
        (f reader writer)))))

(defn compile-content!
  [site-map build-path]
  (let [site (pages->site (map (partial apply make-page) site-map))]
    (doseq [[file-info page-info] site-map]
      (let [page (make-page file-info page-info)
            input (fs/with-cwd "content" (fs/file (:path file-info)))
            output (fs/with-cwd build-path (fs/file (str (:url page) ".html")))]
        (with-file-ends input output
          (fn [reader writer]
            (binding [site (assoc site :current-page page)]
              (.write writer
                      (render-file (fs/extension (:path file-info))
                                   page
                                   (content-section (line-seq reader)))))))))))

(defn compile-site!
  [root-path build-root]
  (binding [*site-map* (atom {})]  
    (let [content-root (content-path root-path)]
      (build-site-map!)
      (compile-content! @*site-map* build-root))))
