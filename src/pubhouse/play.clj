(ns pubhouse.play
  (:require [hiccup.core :as hiccup]
            [markdown.core :as md]
            [me.raynes.fs :as fs]
            [pubhouse.core :as pb]))

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
                 [:li (nav-item current-url url (:title data))]
                 (when-let [index (get data "index")]
                   [:li
                    (nav-item current-url (:url index) (:title index))
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

(defn md-site->html
  [site]
  (md/md-to-html-string
   (clojure.string/join "\n" (get-in site [:current-page :content-lines]))))

(defn play
  []
  (pb/build-site
   {:site-path "example"
    :build-path "example-build"}
   (fn [file]
     (= ".md" (fs/extension file)))
   (fn [file-info site]
     (when (= ".md" (fs/extension (:path file-info)))
       (play-template site (md-site->html site))))))
