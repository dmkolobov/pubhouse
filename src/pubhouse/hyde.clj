(ns pubhouse.hyde
  (:require [pubhouse.core :as pub]
            [me.raynes.fs :as fs]
            [markdown.core :as md]
            [selmer.parser :as selmer]
            [clojure.java.io :as io]))

(def ^:dynamic *info-sep*
  "A line which will be used to split page files into info and content sections."
  nil)

(def ^:dynamic *page-extensions*
  "A vector containing the list of allowed page file types."
  nil)

;;;; Templates
;;;;;;;;;;;;;;

(defn current-template
  [site]
  (get-in site [:current-page :template]))

;;;; Page Files
;;;;;;;;;;;;;;;

(defn page-file?
  "A predicate returns true for all non-hidden files whose extensions are listed in
  *page-extensions*."
  [file]
  (and (not (fs/hidden? file))
       (some (hash-set (fs/extension file)) *page-extensions*)))

(defn page-info
  "Reads the lines preceding the info separator from file as a Clojure hash."
  [file]
  (with-open [reader (io/reader file)]
    (->> (line-seq reader)
         (take-while #(not= % *info-sep*))
         (clojure.string/join "\n")
         (read-string))))

(defn- page-source
  "Reads the lines following the info separator and returns them as a string."
  [reader]
  (->> (line-seq reader)
       (drop-while #(not= % *info-sep*))
       (drop 1)
       (clojure.string/join "\n")))

(defmulti render-source
  "Renders the source from file as HTML. Dispatches on the extension of the file,
  should return an HTML string."
  (fn [file source]
    (fs/extension file)))

(defn- render-page
  "First renders the source from file as HTML, and then renders the resulting 
  HTML as a Selmer template, with the site hash passed in as the data object.
  Returns an updated site hash with the :content key of the :current-page map
  set to the results.  

  Default renderers for the contents of the HTML and Markdown files are provided.
  See the 'render-content' multimethod for extension."
  [file source site]
  (assoc-in site
            [:current-page :content]
            (selmer/render (render-source file source) site)))

(defn- current-template
  "Returns the Selmer template to be used in rendering the current page."
  [site]
  (get-in site [:current-page :template]))

(defn- compile-page
  "Compile a page."
  [site in out]
  (with-open [reader (io/reader in)
              writer (io/writer out)]
    (.write writer
            (selmer/render-file (current-template site)
                                (render-page in (page-source reader) site)))))

(defn- set-template-root!
  "Setup the Selmer resource path. The template path should be specified relative
  to the site path."
  [site-path template-path]
  (selmer/set-resource-path!
   (str
    (fs/with-cwd site-path
      (fs/file template-path)))))

(defn make-hyde-compiler
  [{:keys [info-sep content-path template-path page-extensions]}]
  (let [compiler (pub/compiler "html" page-file? page-info compile-page)]
    (fn [site-root build-path]
      (binding [*info-sep* info-sep
                *page-extensions* page-extensions]
        (set-template-root! site-root template-path)
        (compiler (fs/with-cwd site-root (fs/file content-path))
                  build-path)))))

(defmethod render-source ".md"
  [_ source]
  (md/md-to-html-string source))

(defmethod render-source ".html"
  [_ source]
  source)

(defn start
  []
  (make-hyde-compiler {:info-sep "==="
                       :page-extensions [".md" ".html"]
                       :content-path "content"
                       :template-path "templates"}))
