;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.data.exports
  (:require
   [app.main.data.messages :as msg]
   [app.main.repo :as rp]
   [app.main.store :as st]
   [app.util.i18n :as i18n :refer  [tr]]
   [app.util.dom :as dom]
   ;; [app.main.data.websocket :as ws]
   [app.util.websocket :as ws]
   [beicon.core :as rx]
   [potok.core :as ptk]))

(defn toggle-export-detail-visibililty
  []
  (ptk/reify ::toggle-export-detail-visibililty
    ptk/UpdateEvent
    (update [_ state]
      (let [visibility (get-in state [:export :export-detail-visibililty] false)]
        (-> state
            (assoc-in [:export :export-detail-visibililty] (not visibility)))))))

(defn set-export-detail-visibililty
  [visibility]
  (ptk/reify ::set-export-detail-visibililty
    ptk/UpdateEvent
    (update [_ state]
      (-> state
          (assoc-in [:export :export-detail-visibililty] visibility)))))

(defn set-export-widget-visibililty
  [visibility]
  (ptk/reify ::set-export-widget-visibililty
    ptk/UpdateEvent
    (update [_ state]
      (-> state
          (assoc-in [:export :export-widget-visibililty] visibility)))))

(defn update-export-status
  [status]
  (ptk/reify ::update-export-status
    ptk/UpdateEvent
    (update [_ state]
      (-> state
          (assoc-in [:export :export-in-progress?] status)))))

(defn store-export-task-id
  [id exports filename]
  (ptk/reify ::store-export-task-id
    ptk/UpdateEvent
    (update [_ state]
      (-> state
          (assoc :export {;; TODO quitar sufjo
                          :export-in-progress? true
                          :export-health "OK"
                          :export-error? false
                          :export-widget-visibililty true
                          :export-detail-visibililty true
                          :exports exports
                          :export-progress 0
                          ;; TODO: rename to resource id
                          :export-task-id id
                          :export-filename filename})))))

(defn retry-last-export
  []
  (ptk/reify ::retry-last-export
    ptk/WatchEvent
    (watch [_ state _]
      (let [exports (get-in state [:export :exports])
            filename (get-in state [:export :export-filename])]
        (->> (rp/query! :export-shapes-multiple exports)
             (rx/subs
              (fn [body]
                (st/emit! (store-export-task-id (:id body) exports filename)))
              (fn [_error]
                (st/emit! (msg/error (tr "errors.unexpected-error"))))))))))

(defn- update-export-status2
  [{:keys [progress status resource-id name] :as data}]
  (prn "update-export-status2" data)
  (ptk/reify ::update-export-status2
    ptk/UpdateEvent
    (update [_ state]
      (cond-> state
        (= status "running")
        (update :export assoc :progress (:done progress))

        (= status "ended")
        (update :export assoc :in-progress false)))

    ptk/WatchEvent
    (watch [_ state _]
      (when (= status "ended")
        (let [filename (-> state :export :export-filename)]
          (->> (rp/query! :download-export-resource resource-id)
               (rx/map #(dom/trigger-download name %))))))))

(defn request-simple-export
  [{:keys [export filename]}]
  (ptk/reify ::request-simple-export
    ptk/EffectEvent
    (effect [_ state _]
      (let [profile-id (:profile-id state)
            params     {:exports [export]
                        :profile-id profile-id}]

        (->> (rp/query! :export-shapes-simple params)
             (rx/subs (fn [data]
                        (dom/trigger-download filename data))))))))

;; (rx/catch (fn [_cause]
;; (rx/of (dm/error (tr "errors.unexpected-error"))))

(defn request-multiple-export
  [{:keys [filename exports] :as params}]
  (ptk/reify ::request-multiple-export
    ptk/UpdateEvent
    (update [_ state]
      (assoc state :export {:in-progress true
                            :health "OK"
                            :error false
                            :widget-visible true
                            :detail-visible true
                            :exports exports
                            :progress 0
                            ;; TODO: rename to resource id
                            ;; :export-task-id id
                            ;; :export-filename filename
                            }))

    ptk/WatchEvent
    (watch [_ state stream]
      (let [{:keys [on-error on-success]
             :or {on-error rx/throw
                  on-success identity}} (meta params)
            resource-id (volatile! nil)
            profile-id  (:profile-id state)
            ws-conn     (:ws-conn state)
            params      {:exports exports
                         :name filename
                         :profile-id profile-id
                         :wait false}]

        (rx/merge
         (->> (rp/query! :export-shapes-multiple params)
              (rx/tap (fn [{:keys [id]}]
                        (vreset! resource-id id)))
              (rx/map (fn [{:keys [id]}]
                        #(update % :export assoc :export-task-id id)))
              (rx/catch (fn [cause]
                          (on-error cause)
                          (rx/throw cause))))
         (->> (ws/get-rcv-stream ws-conn)
              (rx/filter ws/message-event?)
              (rx/map :payload)
              (rx/filter #(= :export-update (:type %)))
              (rx/tap (fn [v] (prn "EXPORT UPDATE" v)))
              (rx/filter #(= @resource-id (:resource-id %)))
              (rx/map update-export-status2)))))))

;; (defn request-export
;;   [{:keys [exports]}]
;;   (ptk/reify ::request-export
;;     ptk/WatchEvent
;;     (watch [_ state _]



;;     (swap! st/ongoing-tasks conj :export)
;;     (rp/query! :export-shapes-simple exports)))




