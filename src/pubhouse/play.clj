(ns pubhouse.play
  (:require [hiccup.core :as hiccup]
            [markdown.core :as md]
            [me.raynes.fs :as fs]
            [pubhouse.core :as pb]))

(defn nav-item
  [site url label]
  [:li
   (when (= (get-in site [:current-page :url]) url) ">")
   [:a {:href url} label]])  

(defn top-nav
  [site]
  [:ul
   (->> (seq site)
        (filter #(not (contains? (last %) :url)))
        (map (fn [[name _]]
               (nav-item site (str "/" name) (clojure.string/capitalize name))))
        (cons (nav-item site "/index" "Home")))])

(defn play-template
  [site page content]
  (hiccup/html
   [:html
    [:head
     [:title (:title page)]
     [:link {:rel "stylesheet" :src "style.css"}]]
    [:body
     (:url (:current-page site))
     (top-nav site)
     content
     [:div.development
      [:pre (with-out-str (clojure.pprint/pprint site))]
      [:div (:url page)]
      [:div (get-in site [:current-page :url])]]]]))

(defn md-lines->html
  [lines]
  (md/md-to-html-string (clojure.string/join "\n" lines)))

(pb/build-site
 {:site-path "example"
  :build-path "example-build"}
  (fn [site file-info page content-lines]
    (when (= ".md" (fs/extension (:path file-info)))
      (play-template site page (md-lines->html content-lines)))))
