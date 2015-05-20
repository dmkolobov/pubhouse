(ns pubhouse.play
  (:require [hiccup.core :as hiccup]
            [markdown.core :as md]
            [me.raynes.fs :as fs]
            [clojure.string :refer [join]]
            [pubhouse.core :as pubhouse]))

(defn nav-item
  [current-url url label]
  [:div
   [:a {:href url}
    (when (= current-url url) ">")
    label]])

(defn top-nav
  [current-url site]
  [:ul.navigation
   (->> (seq site)
        (map (fn [[name data]]
               (if-let [url (:url data)]
                 [:li (nav-item current-url url (get-in data [:title]))]
                 (when-let [index (get data "index")]
                   [:li
                    (nav-item current-url (:url index) (get-in index [:title]))
                    (top-nav current-url (dissoc data "index"))])))))])

(defn play-template
  [site content]
  (let [{:keys [url title content]} (:current-page site)]
    (hiccup/html
     [:html
      [:head
       [:title title]
       [:link {:rel "stylesheet" :href "/style.css"}]]
      [:body
       (top-nav url (dissoc site :current-page))
       [:h1 title]
       content
       [:div.development
        [:pre (with-out-str (clojure.pprint/pprint site))]]]])))

(defn md-file? [file] (= ".md" (fs/extension file)))

(defn analyze-md-page
  "Reads the lines preceding the info block separator '===' in as a 
  Clojure data structure."
  [file]
  (with-open [reader (clojure.java.io/reader file)]
    (let [not-sep #(not= % "===")]
      (->> (line-seq reader) (take-while not-sep) (join "\n") (read-string)))))

(defn compile-page
  [site in out]
  (with-open [reader (clojure.java.io/reader in)
              writer (clojure.java.io/writer out)]
    (.write writer
            (play-template site
                           (->> (line-seq reader)
                                (drop-while #(not= % "==="))
                                (drop 1)
                                (clojure.string/join "\n")
                                (md/md-to-html-string))))))

(def md-compiler
  (pubhouse/compiler "html" md-file? analyze-md-page compile-page))

(def css-compiler
  (pubhouse/compiler "css"
                     (fn [f] (= ".css" (fs/extension f)))
                     (constantly {})
                     (fn [_ in out]
                       (spit out (slurp in)))))

(defn jekyll-compile
  [site-root build-root]
  (md-compiler (fs/with-cwd site-root (fs/file "content")) build-root)
  (css-compiler (fs/with-cwd site-root (fs/file "resources")) build-root))

