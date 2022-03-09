;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.http.export-frames
  (:require
   [app.common.data.macros :as dm]
   ["path" :as path]
   [app.common.exceptions :as exc :include-macros true]
   [app.common.spec :as us]
   [app.http.resources :as rsc]
   [app.redis :as redis]
   [app.renderer.pdf :as rp]
   [app.util.shell :as sh]
   [cljs.spec.alpha :as s]
   [cuerdas.core :as str]
   [promesa.core :as p]))

(declare ^:private handle-export)
(declare ^:private create-pdf)
(declare ^:private export-frame)
(declare ^:private join-pdf)
(declare ^:private move-file)
(declare ^:private clean-tmp)


(s/def ::name ::us/string)
(s/def ::file-id ::us/uuid)
(s/def ::page-id ::us/uuid)
(s/def ::frame-id ::us/uuid)

(s/def ::export
  (s/keys :req-un [::file-id ::page-id ::frame-id]))

(s/def ::exports
  (s/every ::export :kind vector? :min-count 1))

(s/def ::params
  (s/keys :req-un [::exports]))

(defn handler
  [{:keys [:request/auth-token] :as exchange} {:keys [exports uri] :as params}]
  (let [xform    (map #(assoc % :token auth-token :uri uri))
        exports  (into [] xform exports)]
    (handle-export exchange (assoc params :exports exports))))

(defn handle-export
  [exchange {:keys [exports wait uri] :as params}]
  (let [topic       (-> exports first :file-id str)
        resource    (rsc/create :pdf)

        on-progress (fn [progress]
                      (let [data {:type :export-update
                                  :resource-id (:id resource)
                                  :status "running"
                                  :progress progress}]
                        (redis/pub! topic data)))

        on-complete (fn [resource]
                      (let [data {:type :export-update
                                  :resource-id (:id resource)
                                  :size (:size resource)
                                  :status "ended"}]
                        (redis/pub! topic data)))

        on-error    (fn [cause]
                      (let [data {:type :export-update
                                  :resource-id (:id resource)
                                  :status "error"
                                  :cause (ex-message cause)}]
                        (redis/pub! topic data)))

        file-id     (-> exports first :file-id)
        proc        (create-pdf :resource resource
                                :items exports
                                :on-progress on-progress
                                :on-complete on-complete
                                :on-error on-error)]
    (if wait
      (p/then proc #(assoc exchange :response/body (dissoc % :path)))
      (assoc exchange :response/body (dissoc resource :path)))))

(defn create-pdf
  [& {:keys [resource items on-progress on-complete on-error]
      :or {on-progress identity
           on-complete identity
           on-error identity}}]
  (let [progress (atom 0)
        tmpdir   (rsc/create-tmpdir! "pdfexport")
        file-id  (-> items first :file-id)
        items    (into [] (map #(partial export-frame tmpdir %)) items)
        xform    (map (fn [export-fn]
                        #(p/finally
                           (export-fn)
                           (fn [result _]
                             (on-progress {:total (count items)
                                           :done (swap! progress inc)
                                           :name (:name result)})))))]
    (-> (reduce (fn [res export-fn]
                  (p/let [res res
                          out (export-fn)]
                    (cons res (:path out))))
                (p/resolved nil)

                ;; We use list here instead of vector because we want the
                ;; items in reversed order.
                (into '() xform items))
        (p/then (partial join-pdf tmpdir file-id))
        (p/then (partial move-file resource))
        (p/then rsc/fs-stat)
        (p/then #(merge resource %))
        (p/finally (fn [result cause]
                     (if cause
                       (on-error cause)
                       (on-complete result)))))))

(defn- export-frame
  [tmpdir {:keys [file-id page-id frame-id token]}]
  (let [file-name (dm/fmt "%.pdf" frame-id)
        save-path (path/join tmpdir file-name)]
    (-> (rp/render {:name (dm/str frame-id)
                    :suffix ""
                    :token token
                    :file-id file-id
                    :page-id page-id
                    :object-id frame-id
                    :scale 1
                    :save-path save-path})
        (p/then (fn [_]
                  {:name file-name
                   :path save-path})))))

(defn- join-pdf
  [tmpdir file-id paths]
  (let [output-path (path/join tmpdir (str file-id ".pdf"))
        paths-str   (str/join " " paths)]
    (-> (sh/run-cmd! (str "pdfunite " paths-str " " output-path))
        (p/then (constantly output-path)))))

(defn- move-file
  [{:keys [path] :as resource} output-path]
  (rsc/move! output-path path))

(defn- clean-tmp
  [tdpath data]
  (p/do!
    (sh/rmdir! tdpath)
    data))


  ;; (let [

  ;;           data (-> (reduce (fn [promise frame-id]
  ;;                                (p/then promise (partial export-frame tdpath file-id page-id token frame-id)))
  ;;                              (p/future [])
  ;;                              (reverse frame-ids))
  ;;                      (p/then  (partial join-files tdpath file-id))
  ;;                      (p/then  sh/read-file)
  ;;                      (p/then  (partial clean-tmp-data tdpath)))]
  ;;       {:status 200
  ;;        :body data
  ;;        :headers {"content-type" "application/pdf"
  ;;                  "content-length" (.-length data)}})
  ;;     {:status 204
  ;;      :body ""
  ;;      :headers {"content-type" "text/plain"}})))

;; (defn- export-frame
;;   [tdpath file-id page-id token frame-id spaths]
;;   (p/let [spath  (path/join tdpath (str frame-id ".pdf"))
;;           result (rp/render {:name (str frame-id)
;;                              :suffix ""
;;                              :token token
;;                              :file-id file-id
;;                              :page-id page-id
;;                              :object-id frame-id
;;                              :scale 1
;;                              :save-path spath})]
;;     (conj spaths spath)))

;; (defn- join-files
;;   [tdpath file-id paths]
;;   (let [output-path (path/join tdpath (str file-id ".pdf"))
;;         paths-str   (str/join " " paths)]
;;     (-> (sh/run-cmd! (str "pdfunite " paths-str " " output-path))
;;         (p/then (constantly output-path)))))

;; (defn- clean-tmp-data
;;   [tdpath data]
;;   (p/do!
;;     (sh/rmdir! tdpath)
;;     data))

;; (defn export-frames-handler
;;   [{:keys [params cookies] :as request}]
;;   (let [{:keys [name file-id page-id frame-ids]} (us/conform ::handler-params params)
;;         token  (.get ^js cookies "auth-token")]
;;     (if (seq frame-ids)
;;       (p/let [tdpath (sh/create-tmpdir! "pdfexport-")
;;               data (-> (reduce (fn [promise frame-id]
;;                                  (p/then promise (partial export-frame tdpath file-id page-id token frame-id)))
;;                                (p/future [])
;;                                (reverse frame-ids))
;;                        (p/then  (partial join-files tdpath file-id))
;;                        (p/then  sh/read-file)
;;                        (p/then  (partial clean-tmp-data tdpath)))]
;;         {:status 200
;;          :body data
;;          :headers {"content-type" "application/pdf"
;;                    "content-length" (.-length data)}})
;;       {:status 204
;;        :body ""
;;        :headers {"content-type" "text/plain"}})))

