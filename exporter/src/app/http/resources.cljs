;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.http.resources
  "Temporal resouces management."
  (:require
   ["archiver" :as arc]
   ["fs" :as fs]
   ["os" :as os]
   ["path" :as path]
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.exceptions :as ex]
   [app.common.uuid :as uuid]
   [cuerdas.core :as str]
   [promesa.core :as p]))

(defn- get-path
  [type id]
  (path/join (os/tmpdir) (dm/str  "exporter." (d/name type) "." id)))

(defn- get-mtype
  [type]
  (case (d/name type)
    "zip" "application/zip"
    "jpeg" "image/jpeg"
    "png"  "image/png"
    "pdf"  "application/pdf"))

(defn- fs-stat
  [path]
  (-> (.stat fs/promises path)
      (p/then (fn [data]
                {:created-at (inst-ms (.-ctime ^js data))
                 :size (.-size data)}))
      (p/catch (constantly nil))))

(defn create
  "Generates ephimeral resource object."
  [type]
  (let [task-id (uuid/next)]
    {:path (get-path type task-id)
     :mtype (get-mtype type)
     :id (dm/str (name type) "." task-id)}))

(defn- write-as-zip!
  [{:keys [id path]} items on-progress on-error]
  (let [^js zip  (arc/create "zip")
        ^js out  (fs/createWriteStream path)
        append!  (fn [{:keys [data name] :as result}]
                  (.append zip data #js {:name name}))
        progress (atom 0)]
    (p/create
     (fn [resolve reject]
       (.on zip "error" #(reject %))
       (.on zip "end" resolve)
       (.on zip "entry" (fn [data]
                          (let [name (unchecked-get data "name")
                                num  (swap! progress inc)]
                            (on-progress
                             {:total (count items)
                              :id id
                              :done num
                              :name name}))))
       #_(.on zip "progress" (fn [data]
                               (on-progress data)
                               (js/console.log "on-progress" data)))
       (.pipe zip out)
       (-> (reduce (fn [res export-fn]
                     (p/then res (fn [_] (-> (export-fn) (p/then append!)))))
                   (p/resolved 1)
                   items)
           (p/then #(.finalize zip))
           (p/catch reject))))))

(defn create-simple
  [{:keys [type data] :as params}]
  (let [{:keys [path] :as resource} (create type)]
    (-> (.writeFile fs/promises path data)
        (p/then #(fs-stat path))
        (p/then #(merge resource %)))))

(defn create-zip
  "Creates a resource with multiple files merget into a single zip file."
  [& {:keys [resource items on-error on-progress]
      :or {on-error identity
           on-progress identity}}]
  (let [{:keys [path id] :as resource} (or resource (create :zip))]
    (-> (write-as-zip! resource items on-progress on-error)
        (p/then #(fs-stat path))
        (p/then #(merge resource %)))))

(defn- lookup
  [id]
  (p/let [[type task-id] (str/split id "." 2)
          path  (get-path type task-id)
          mtype (get-mtype type)
          stat  (fs-stat path)]

    (when-not stat
      (ex/raise :type :not-found))

    {:stream (fs/createReadStream path)
     :headers {"content-type" mtype
               "content-length" (:size stat)}}))

(defn retrieve-handler
  [{:keys [:request/params response] :as exchange}]
  (when-not (contains? params :id)
    (ex/raise :type :validation
              :code :missing-id))
  (p/then
   (lookup (get params :id))
   (fn [{:keys [stream headers] :as resource}]
     (-> exchange
         (assoc :response/status 200)
         (assoc :response/body stream)
         (assoc :response/headers headers)))))

