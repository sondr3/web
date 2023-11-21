(ns web.app
  (:require [rum.core :as rum]))

(rum/defc hello []
  [:div "Hello, world!"])

(defn -main []
  (prn "Hello, world!")
  (prn (rum/render-static-markup (hello))))
