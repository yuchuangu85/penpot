;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.http.export
  (:require
   [app.common.exceptions :as exc :include-macros true]
   [app.common.spec :as us]
   [app.http.resources :as rsc]
   [app.renderer.bitmap :as rb]
   [app.renderer.pdf :as rp]
   [app.renderer.svg :as rs]
   [cljs.spec.alpha :as s]
   [cuerdas.core :as str]
   [promesa.core :as p]))

(s/def ::name ::us/string)
(s/def ::page-id ::us/uuid)
(s/def ::file-id ::us/uuid)
(s/def ::object-id ::us/uuid)
(s/def ::scale ::us/number)
(s/def ::suffix ::us/string)
(s/def ::type ::us/keyword)
(s/def ::suffix string?)
(s/def ::scale number?)
(s/def ::export  (s/keys :req-un [::type ::suffix ::scale]))
(s/def ::exports (s/coll-of ::export :kind vector? :min-count 1))

(s/def ::params
  (s/keys :req-un [::page-id ::file-id ::object-id ::name ::exports]))

(declare handle-single-export)
(declare handle-multiple-export)
(declare perform-export)
(declare attach-filename)

(defn export-handler
  [{:keys [:request/params :request/cookies :request/auth-token] :as exchange}]
  (let [{:keys [exports page-id file-id object-id name]} (us/conform ::params params)]
    (if (= 1 (count exports))
      (-> (first exports)
          (assoc :name name)
          (assoc :token auth-token)
          (assoc :file-id file-id)
          (assoc :page-id page-id)
          (assoc :object-id object-id)
          (handle-single-export exchange))

      (let [exports (->> exports
                         (map (fn [item]
                                (-> item
                                    (assoc :name name)
                                    (assoc :token auth-token)
                                    (assoc :file-id file-id)
                                    (assoc :page-id page-id)
                                    (assoc :object-id object-id))))
                         (attach-filename))]

        (handle-multiple-export exports exchange)))))

(defn- handle-single-export
  [params exchange]
  (-> (perform-export params)
      (p/then rsc/create-simple)
      (p/then (fn [resource]
                (assoc exchange :response/body (dissoc resource :path))))))

(defn- handle-multiple-export
  [exports exchange]
  (let [items (map #(fn [] (perform-export %)) exports)]
    (-> (rsc/create-zip items)
        (p/then (fn [resource]
                  (assoc exchange :response/body (dissoc resource :path)))))))

(defn- perform-export
  [{:keys [type] :as params}]
  (p/let [res (case type
                :png  (rb/render params)
                :jpeg (rb/render params)
                :svg  (rs/render params)
                :pdf  (rp/render params))]
    (assoc res :type type)))

(defn- find-filename-candidate
  [params used]
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
        candidate))))

(defn- attach-filename
  [exports]
  (loop [exports (seq exports)
         used   #{}
         result  []]
    (if (nil? exports)
      result
      (let [export    (first exports)
            candidate (find-filename-candidate export used)
            export    (assoc export :filename candidate)]
        (recur (next exports)
               (conj used candidate)
               (conj result export))))))
