;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.http.export
  (:require
   [app.common.exceptions :as exc :include-macros true]
   [app.common.spec :as us]
   [app.redis :as redis]
   [app.http.resources :as rsc]
   [app.renderer.bitmap :as rb]
   [app.renderer.pdf :as rp]
   [app.renderer.svg :as rs]
   [cljs.spec.alpha :as s]
   [cuerdas.core :as str]
   [promesa.core :as p]))

(declare ^:private handle-exports)
(declare ^:private handle-single-export)
(declare ^:private handle-multiple-export)
(declare ^:private run-export)
(declare ^:private assign-file-name)

(s/def ::name ::us/string)
(s/def ::page-id ::us/uuid)
(s/def ::file-id ::us/uuid)
(s/def ::object-id ::us/uuid)
(s/def ::scale ::us/number)
(s/def ::suffix ::us/string)
(s/def ::type ::us/keyword)
(s/def ::suffix string?)
(s/def ::scale number?)

(s/def ::export
  (s/keys :req-un [::page-id ::file-id ::object-id
                   ::type ::suffix ::scale]))
(s/def ::exports
  (s/coll-of ::export :kind vector? :min-count 1))

(s/def ::params
  (s/keys :req-un [::exports]))

(defn handler
  [{:keys [:request/auth-token] :as exchange} {:keys [exports] :as params}]
  (let [xform   (comp
                 (map #(assoc % :token auth-token))
                 (assign-file-name))
        exports (into [] xform exports)]
    (if (= 1 (count exports))
      (handle-single-export exchange (assoc params :export (first exports)))
      (handle-multiple-export exchange (assoc params :exports exports)))))

(defn- handle-single-export
  [exchange {:keys [export wait uri] :as params}]
  (-> (run-export export)
      (p/then rsc/create-simple)
      (p/then (fn [resource]
                (assoc exchange :response/body (dissoc resource :path))))))

(defn- handle-multiple-export
  [exchange {:keys [exports wait uri] :as params}]
  (let [items       (map #(fn [] (run-export %)) exports)
        topic       (-> exports first :file-id str)
        resource    (rsc/create :zip)

        on-progress (fn [data]
                      (let [data (assoc data :resource-id (:id resource))]
                        (redis/pub! topic data)))

        on-complete (fn [resource]
                      (js/console.log "complete"))

        on-error    (fn [cause]
                      (js/console.error cause))

        proc        (-> (rsc/create-zip :resource resource
                                        :items items
                                        :on-progress on-progress)
                        (p/finally (fn [_ cause]
                                     (when cause (on-error cause)))))]

    (if wait
      (p/then proc #(assoc exchange :response/body (dissoc % :path)))
      (assoc exchange :response/body (dissoc resource :path)))))

(defn- run-export
  [{:keys [type] :as params}]
  (p/let [res (case type
                :png  (rb/render params)
                :jpeg (rb/render params)
                :svg  (rs/render params)
                :pdf  (rp/render params))]
    (assoc res :type type)))

(defn- assign-file-name
  "A transducer that assocs a candidate filename and avoid duplicates."
  []
  (letfn [(find-candidate [params used]
            (loop [index 0]
              (let [candidate (str (:name params)
                                   (:suffix params "")
                                   (when (pos? index)
                                     (str "-" (inc index)))
                                   (case (:type params)
                                     :png  ".png"
                                     :jpeg ".jpg"
                                     :svg  ".svg"
                                     :pdf  ".pdf"))]
                (if (contains? used candidate)
                  (recur (inc index))
                  candidate))))]
    (fn [rf]
      (let [used (volatile! #{})]
        (fn
          ([] (rf))
          ([result] (rf result))
          ([result params]
           (let [candidate (find-candidate params @used)
                 params    (assoc params :filename candidate)]
             (vswap! used conj candidate)
             (rf result params))))))))
