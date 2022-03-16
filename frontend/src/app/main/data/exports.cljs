;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.data.exports
  (:require
   [app.main.repo :as rp]
   [app.main.store :as st]
   [app.main.data.workspace.persistence :as dwp]
   [app.util.dom :as dom]
   [app.util.websocket :as ws]
   [beicon.core :as rx]
   [potok.core :as ptk]))

(defn toggle-detail-visibililty
  []
  (ptk/reify ::toggle-detail-visibililty
    ptk/UpdateEvent
    (update [_ state]
      (update-in state [:export :detail-visible] not))))

(defn toggle-widget-visibililty
  []
  (ptk/reify ::toggle-widget-visibility
    ptk/UpdateEvent
    (update [_ state]
      (update-in state [:export :widget-visible] not))))

(defn clear-export-state
  []
  (ptk/reify ::clear-export-state
    ptk/UpdateEvent
    (update [_ state]
      (dissoc state :export))))

(defn- update-export-status
  [{:keys [progress status resource-id name] :as data}]
  (ptk/reify ::update-export-status
    ptk/UpdateEvent
    (update [_ state]
      (cond-> state
        (= status "running")
        (update :export assoc :progress (:done progress))

        (= status "error")
        (update :export assoc :error (:cause data))

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
    ptk/WatchEvent
    (watch [_ _ _]
      (rx/of ::dwp/force-persist))

    ptk/EffectEvent
    (effect [_ state _]
      (let [profile-id (:profile-id state)
            params     {:exports [export]
                        :profile-id profile-id}]
        (->> (rp/query! :export-shapes-simple params)
             (rx/subs (fn [data]
                        (dom/trigger-download filename data))))))))

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
                            :filename filename
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
                         :wait false}

            progress-stream
            (->> (ws/get-rcv-stream ws-conn)
                 (rx/filter ws/message-event?)
                 (rx/map :payload)
                 (rx/filter #(= :export-update (:type %)))
                 (rx/filter #(= @resource-id (:resource-id %)))
                 (rx/share))

            stoper
            (->> progress-stream
                 (rx/filter #(or (= "ended" (:status %))
                                 (= "error" (:status %))))
                 (rx/delay 200))]

        (swap! st/ongoing-tasks conj :export)

        (rx/merge
         (rx/of ::dwp/force-persist))
         (->> (rp/query! :export-shapes-multiple params)
              (rx/delay 200)
              (rx/tap (fn [{:keys [id]}]
                        (vreset! resource-id id)))
              (rx/map (fn [{:keys [id]}]
                        #(update % :export assoc :export-task-id id)))
              (rx/catch (fn [cause]
                          (rx/of #(update :export assoc :error cause)))))
         (->> progress-stream
              (rx/map update-export-status)
              (rx/take-until stoper)
              (rx/finalize (fn []
                             (swap! st/ongoing-tasks disj :export))))))))

(defn retry-last-export
  []
  (ptk/reify ::retry-last-export
    ptk/WatchEvent
    (watch [_ state _]
      (let [{:keys [exports filename]} (:export state)]
        (rx/of (request-multiple-export {:exports exports :filename filename}))))))

