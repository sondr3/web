(ns web.app
  (:require [cljs-node-io.core :as io]
            [clojure.pprint :refer [pprint]]
            [rum.core :as rum]))

(rum/defc hello []
  [:div "Hello, world!"])

(defn files []
  (io/file-seq "site/content/pages"))

(defn pages []
  (->> (io/file-seq "site/content/pages")
       (filter #(= (.getExt %) ".dj"))
       (map #(io/slurp %))))

(defn -main []
  (prn "Hello, world!")
  (prn (files))
  (pprint (pages))
  (prn (rum/render-static-markup (hello))))
