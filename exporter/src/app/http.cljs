;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.http
  (:require
   [app.common.exceptions :as ex]
   [app.common.logging :as l]
   [app.config :as cf]
   [app.http.resources :as rsc]
   [app.http.export :refer [export-handler]]
   ;; [app.http.export-frames :refer [export-frames-handler]]
   [app.http.impl :as impl]
   [app.util.transit :as t]
   [cuerdas.core :as str]
   [promesa.core :as p]
   [reitit.core :as r]))

(l/set-level! :info)

;; (def routes
;;   [["/export-frames" {:handler export-frames-handler}]
;;    ["/export" {:handler export-handler}]])

(def instance (atom nil))

(defn- on-error
  [error exchange]
  (let [{:keys [type message code] :as data} (ex-data error)]
    (cond
      (= :validation type)
      (-> exchange
          (assoc :response/status 400)
          (assoc :response/body (t/encode data))
          (assoc :response/headers {"content-type" "application/transit+json"}))

      (and (= :internal type)
           (= :browser-not-ready code))
      (-> exchange
          (assoc :response/status 503)
          (assoc :response/body (t/encode data))
          (assoc :response/headers {"content-type" "application/transit+json"}))

      :else
      (do
        (l/error :msg "Unexpected error" :cause error)
        ;; (js/console.error error)
        (-> exchange
            (assoc :response/status 500)
            (assoc :response/body (t/encode data))
            (assoc :response/headers {"content-type" "application/transit+json"}))))))

(defn- handler
  [{:keys [:request/params] :as exchange}]
  (case (:cmd params)
    "get-resource"    (rsc/retrieve-handler exchange)
    "export-single"   (export-handler exchange)
    ;; :export-multiple (export-multiple-handler exchange)
    ;; :export-frames   (export-frames-handler exchange)
    (ex/raise :type :internal
              :code :method-not-implemented
              :hint "method not implemented")))

(defn init
  []
  (let [server  (impl/server handler on-error)
        port    (cf/get :http-server-port 6061)]
    (.listen server port)
    (l/info :msg "welcome to penpot"
            :module "exporter"
            :version (:full @cf/version))
    (l/info :msg "starting http server" :port port)
    (reset! instance server)))

(defn stop
  []
  (if-let [server @instance]
    (p/create (fn [resolve]
                (.close server (fn []
                                 (l/info :msg "shutdown http server")
                                 (resolve)))))
    (p/resolved nil)))
