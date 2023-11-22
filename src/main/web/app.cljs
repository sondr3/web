(ns web.app
  (:require [cljs-node-io.core :as io]
            [clojure.pprint :refer [pprint]]
            [rum.core :as rum]
            [web.templates :as templates]))

(defn files []
  (io/file-seq "site/content/pages"))

(defn file-ext [^File f]
  (.getExt f))

(defn pages []
  (->> (io/file-seq "site/content/pages")
       (filter #(= (file-ext %) ".dj"))
       (map #(io/slurp %))))

(defn -main []
  (prn "Hello, world!")
  (prn (files))
  (pprint (pages))
  (prn (rum/render-static-markup (templates/hello)))
  (io/spit "test.html" (rum/render-static-markup (templates/layout))))
