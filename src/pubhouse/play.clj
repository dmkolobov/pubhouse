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
  (hiccup/html
   [:html
    [:head
     [:title (get-in site [:current-page :title])]
     [:link {:rel "stylesheet" :src "style.css"}]]
    [:body
     (top-nav (get-in site [:current-page :url]) (dissoc site :current-page))
     [:h1 (get-in site [:current-page :title])]
     content
     [:div.development
      [:pre {:style "font-family: Courier;line-height: 1.5em;"}
       (with-out-str (clojure.pprint/pprint site))]]]]))

(defn read-info
  "Reads the lines preceding the info block separator '===' in as a 
  Clojure data structure."
  [reader]
  (let [not-sep #(not= % "===")]
    (->> (line-seq reader) (take-while not-sep) (join "\n") (read-string))))

(defn read-content
  [reader]
  (->> (line-seq reader)
       (drop-while #(not= % "==="))
       (drop 1)
       (clojure.string/join "\n")
       (md/md-to-html-string)))

(defn play
  []
  (compile-site (fn [site reader writer]
                  (.write writer
                          (play-template site (read-content reader))))
                {:site-root "example/content"
                 :info read-info
                 :build-root "example-build"
                 :source-file? (fn [f] (= ".md" (fs/extension f)))}))
