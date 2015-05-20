(ns pubhouse.play
  (:require [hiccup.core :as hiccup]
            [markdown.core :as md]
            [me.raynes.fs :as fs]
            [clojure.string :refer [join]]
            [pubhouse.core :refer [compile-site]]))

(defn nav-item
  [current-url url label]
  [:div
   [:a {:href url}
    (when (= current-url url) ">")
    label]])

(defn top-nav
  [current-url site]
  [:ul
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
       [:link {:rel "stylesheet" :src "style.css"}]]
      [:body
       (top-nav url (dissoc site :current-page))
       [:h1 title]
       content
       [:div.development
        [:pre (with-out-str (clojure.pprint/pprint site))]]]])))

(defn read-info
  "Reads the lines preceding the info block separator '===' in as a 
  Clojure data structure."
  [file]
  (with-open [reader (clojure.java.io/reader file)]
    (let [not-sep #(not= % "===")]
      (->> (line-seq reader) (take-while not-sep) (join "\n") (read-string)))))

(defn read-content
  [reader]
  (->> (line-seq reader)
       (drop-while #(not= % "==="))
       (drop 1)
       (clojure.string/join "\n")
       (md/md-to-html-string)))

(defn compile-page
  [site in out]
  (with-open [reader (clojure.java.io/reader in)
              writer (clojure.java.io/writer out)]
    (.write writer (play-template site (read-content reader)))))

(defn play
  []
  (compile-site compile-page
                {:site-root "example/content"
                 :info read-info
                 :build-root "example-build"
                 :source-file? (fn [f] (= ".md" (fs/extension f)))}))
