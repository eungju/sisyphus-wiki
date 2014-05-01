(ns sisyphus-wiki.core-test
  (:use midje.sweet ring.util.response)
  (:require [sisyphus-wiki.core :refer :all]))

(fact "Error responses show code, reason and description."
      ((error-response 400) {}) => {:status 400 :headers {"Content-Type" "text/html"} :body "<h1>400 Bad request</h1><p></p>" })

(facts "a route rule returns a handler if it matches the request; otherwise it returns nil."
       (let [handler (fn [request] (response "Ok."))]
         (fact "matches any URI if the URI pattern is :else."
               (route-match [:else handler] "/" :get) => handler
               (route-match [:else handler] "/help" :get) => handler)
         (fact "matches specified URI if the URI pattern is a string."
               (route-match ["/" handler] "/" :get) => handler
               (route-match ["/" handler] "/help" :get) => nil)
         (fact "matches any method if the handler is not a map."
               (route-match ["/" handler] "/" :get) => handler
               (route-match ["/" handler] "/" :post) => handler)
         (fact "matches specified methods if the handler is a map."
               (route-match ["/" {:get handler}] "/" :get) => handler
               (route-match ["/" {:get handler}] "/" :post) => nil)))
