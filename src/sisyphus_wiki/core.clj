(ns sisyphus-wiki.core
  (:use ring.util.response
        ring.util.codec
        ring.middleware.params
        ring.middleware.lint
        hiccup.core hiccup.page
        [org.httpkit.server :only [run-server]])
  (:require [clojure.string :as string])
  (:gen-class))

(def http-status {400 ["Bad request" ""]
                  401 ["Unauthorized" ""]
                  403 ["Forbidden" ""]
                  405 ["Method not allowed" ""]
                  404 ["Not found" "The resource could not be found."]})

(defn error-response [code]
  (let [[reason description] (http-status code)]
    (fn [request] (-> (response (format "<h1>%d %s</h1><p>%s</p>" code reason description))
                      (status code)
                      (content-type "text/html")))))

(def error-404 (error-response 404))

(defn route-match [rule uri method]
  (let [[p hs] rule multi? (map? hs)]
    (if (and (or (= p :else) (= p uri))
             (or (not multi?) (hs method)))
      (if multi? (hs method) hs)
      nil)))

(defn route [rules request]
  (if rules
    (if-let [h (route-match (first rules) (:uri request) (:request-method request))]
      (h request)
      (recur (next rules) request))
    (throw (IllegalArgumentException. "No matching rule"))))

(defn router [rules]
  (fn [request] (route rules request)))

(defn home-uri []
  "/")

(defn file-uri
  ([path] (str "/" ))
  ([path action] (str ({:edit "/edit" :history "/history"} action) "/" path)))

(defn wrap-app-context [handler app-context]
  (fn [request] (handler (assoc request :app-context app-context))))

(defn home [request]
  (response (-> request :app-context :message)))

(defn drop-action [uri]
  (let [m (re-matcher #"/(edit|history)/(.+)" uri)]
    (if (.matches m)
      (.group m 2)
      (.substring uri 1))))

(defn route-backward [handler & args]
  (cond
   (= handler :file-view) {:uri (str "/" (first args)) :request-method :get}
   (= handler :file-edit) {:uri (str "/edit/" (first args)) :request-method :get}
   (= handler :file-history) {:uri (str "/history/" (first args)) :request-method :get}))

(defn layout [app-context body]
  (list
   [:head [:title (:name app-context)]]
   [:body
    [:div.header]
    [:div.repository
     [:h1.name [:a {:href ""} (:name app-context)]]
     [:div#wrapper body]]]))

(defn file-view-page [app-context file-path]
  (html5
   (layout
    app-context
    [:div.file
     [:h1.path [:a {:href file-path} file-path]]
     [:div.actions
      [:a.view {:href (:uri (route-backward :file-view file-path))} "View"]
      [:a.edit {:href (:uri (route-backward :file-edit file-path))} "Edit"]
      [:a.history {:href (:uri (route-backward :file-history file-path))} "History"]]
     [:div.content "asdfasdfasdf"]])))

(defn file-view [request]
  (let [file-path (drop-action (:uri request))]
    (-> (response (file-view-page (:app-context request) file-path))
        (content-type "text/html; charset=utf-8"))))

(defn file-edit [request]
  (let [file-path (drop-action (:uri request))]
    (-> (response (file-view-page (:app-context request) file-path))
        (content-type "text/html; charset=utf-8"))))

(defn file-history [request]
  (let [file-path (drop-action (:uri request))]
    (-> (response (file-view-page (:app-context request) file-path))
        (content-type "text/html; charset=utf-8"))))

(defn make-app []
  (let [app-context {:name "Sisyphus Wiki"}]
    (-> (router [["/edit/" {:get file-edit}]
                 ["/history/" {:get file-history}]
                 [:else {:get file-view}]])
        (wrap-app-context app-context)
        wrap-params)))

(def app (make-app))

(defn -main
  "Sisyphus Wiki."
  [& args]
    (run-server (wrap-lint app) {:port 8080}))
