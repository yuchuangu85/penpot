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
   [app.common.data.macros :as dm]
   [app.common.exceptions :as ex]
   [app.common.uuid :as uuid]
   [cuerdas.core :as str]
   [promesa.core :as p]))

(defn- get-mtype
  [type]
  (let [type (if (keyword? type) (name type) type)]
    (case type
      "zip" "application/zip"
      "jpeg" "image/jpeg"
      "png"  "image/png"
      "pdf"  "application/pdf")))

(defn- fs-stat
  [path]
  (-> (.stat fs/promises path)
      (p/then (fn [data]
                {:created-at (inst-ms (.-ctime ^js data))
                 :size (.-size data)}))
      (p/catch (constantly nil))))

(defn- lookup
  [id]
  (p/let [[type task-id] (str/split id "." 2)
          path  (path/join (os/tmpdir) (dm/str  "exporter." type "." task-id))
          mtype (get-mtype type)
          stat  (fs-stat path)]
    (when-not stat
      (ex/raise :type :not-found))

    {:stream (fs/createReadStream path)
     :headers {"content-type" mtype
               "content-length" (:size stat)}}))

(defn- create
  "Generates ephimeral resource object."
  [type]
  (let [task-id (uuid/next)]
    {:path (path/join (os/tmpdir) (dm/str "exporter." (name type) "." task-id))
     :mtype (get-mtype type)
     :id (dm/str (name type) "." task-id)}))

(defn- write-as-zip!
  [path items on-progress]
  (let [^js zip (arc/create "zip")
        ^js out (fs/createWriteStream path)
        append! (fn [{:keys [data name] :as result}]
                  (.append zip data #js {:name name}))]
    (p/create
     (fn [resolve reject]
       (.on zip "error" #(reject %))
       (.on zip "end" resolve)
       (.on zip "entry" (fn [data]
                          (js/console.log "on-entry" data)))
       (.on zip "progress" (fn [data]
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
  ([items] (create-zip items {}))
  ([items {:keys [on-progress] :or {on-progress identity}}]
   (let [{:keys [path] :as resource} (create :zip)]
     (-> (write-as-zip! path items on-progress)
         (p/then #(fs-stat path))
         (p/then #(merge resource %))))))

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

