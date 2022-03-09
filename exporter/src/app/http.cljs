;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.http
  (:require
   [app.common.data.macros :as dm]
   [app.common.exceptions :as ex]
   [app.common.logging :as l]
   [app.common.spec :as us]
   [app.config :as cf]
   [app.http.export :as export]
   [app.http.export-frames :as export-frames]
   ;; [app.http.export-frames :refer [export-frames-handler]]
   [app.http.impl :as impl]
   [app.http.resources :as rsc]
   [app.util.transit :as t]
   [clojure.spec.alpha :as s]
   [cuerdas.core :as str]
   [promesa.core :as p]
   [reitit.core :as r]))

(l/set-level! :info)

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

(defmulti command-spec :cmd)

(s/def ::id ::us/string)
(s/def ::uri ::us/string)
(s/def ::wait ::us/boolean)
(s/def ::cmd ::us/keyword)

(defmethod command-spec :export-shapes [_] ::export/params)
(defmethod command-spec :export-frames [_] ::export-frames/params)
(defmethod command-spec :get-resource [_] (s/keys :req-un [::id]))

(s/def ::params
  (s/and (s/keys :req-un [::cmd]
                 :opt-un [::wait ::uri])
         (s/multi-spec command-spec :cmd)))

(defn- handler
  [{:keys [:request/params] :as exchange}]
  (let [{:keys [cmd] :as params} (us/conform ::params params)]
    (case cmd
      :get-resource  (rsc/retrieve-handler exchange)
      :export-shapes (export/handler exchange params)
      :export-frames (export-frames/handler exchange params)
      (ex/raise :type :internal
                :code :method-not-implemented
                :hint (dm/fmt "method % not implemented" cmd)))))

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
