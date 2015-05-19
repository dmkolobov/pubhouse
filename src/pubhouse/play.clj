(ns pubhouse.play
  (:require [hiccup.core :as hiccup]
            [markdown.core :as md]
            [me.raynes.fs :as fs]
            [pubhouse.core :as pb]))

(defn nav-item
  [site url label]
  [:li
   (when (= (get-in site [:current-page :url]) url) ">")
   [:a {:href (str "/" url)} label]])  

(defn top-nav
  [site]
  [:ul
   (->> (seq (dissoc site :current-page))
        (map (fn [[name data]]
               (if-let [url (:url data)]
                 (nav-item site url (:title data))
                 (nav-item site name
                           (clojure.string/capitalize name))))))])
(defn play-template
  [site content]
  (hiccup/html
   [:html
    [:head
     [:title (get-in site [:current-page :title])]
     [:link {:rel "stylesheet" :src "style.css"}]]
    [:body
     (top-nav site)
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
