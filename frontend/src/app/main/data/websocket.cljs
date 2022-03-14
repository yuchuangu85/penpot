;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.data.websocket
  (:require
   [app.common.data :as d]
   [app.common.spec :as us]
   [app.common.transit :as t]
   [app.common.uri :as u]
   [app.config :as cf]
   [app.main.repo :as rp]
   [app.main.store :as st]
   [app.main.streams :as ms]
   [app.util.object :as obj]
   [app.util.time :as dt]
   [app.util.timers :as ts]
   [app.util.websocket :as ws]
   [beicon.core :as rx]
   [cljs.spec.alpha :as s]
   [clojure.set :as set]
   [potok.core :as ptk]))

(defn- prepare-uri
  [params]
  (let [base (-> (u/join cf/public-uri "ws/notifications")
                 (assoc :query (u/map->query-string params)))]
    (cond-> base
      (= "https" (:scheme base))
      (assoc :scheme "wss")

      (= "http" (:scheme base))
      (assoc :scheme "ws"))))

;; (defn sync-state
;;   "Event responsible of syncronize the state values with the
;;   subscriptions sent to the websocket server."
;;   []
;;   (let [stream (rx/subject)]
;;     (prn "sync-state" "constructor")
;;     (ptk/reify ::sync-state
;;       ;; ptk/UpdateEvent
;;       ;; (update [_ state]
;;       ;;   ;; (if (ws/open? (:ws-conn state))

;;       ptk/WatchEvent
;;       (watch [_ state _]
;;         (prn ::sync-state "watch" (:ws-data state))
;;         stream)

;;       ptk/EffectEvent
;;       (effect [_ state _]
;;         (prn ::sync-state "effect" (:ws-data state))
;;         (let [ws-conn (:ws-conn state)
;;               ws-data (:ws-data state)]
;;           (when (ws/open? (:ws-conn state))
;;             (when (not= @ws-conn ws-data)
;;               (when (not= (:team-id @ws-conn) (:team-id ws-data))
;;                 (if-let [team-id (:team-id ws-data)]
;;                   (ws/send! ws-conn {:type :join-team-room :team-id team-id})))

;;               (when (not= (:file-id @ws-data) (:file-id ws-data))
;;                 (if-let [file-id (:file-id ws-data)]
;;                   (ws/send! ws-conn {:type :join-file-room :file-id file-id})
;;                   (ws/send! ws-conn {:type :leave-file-room})))




;;         )))

(defn initialize
  []
  (ptk/reify ::initialize
    ptk/UpdateEvent
    (update [_ state]
      (prn ::initialize "update")
      (let [sid (:session-id state)
            uri (prepare-uri {:session-id sid})]
        (-> state
            (assoc :ws-conn (ws/create uri))
            (assoc :ws-data {}))))

    ptk/WatchEvent
    (watch [_ state stream]
      (prn ::initialize "watch")
      (let [ws-conn (:ws-conn state)
            ws-data (:ws-data state)
            stoper  (rx/merge
                     (rx/filter (ptk/type? ::finalize) stream)
                     (rx/filter (ptk/type? ::initialize) stream))]

        (->> (rx/merge
              ;; (->> stream
              ;;      (rx/filter (ptk/type? ::update-state))
              ;;      (rx/tap (fn [o] (prn "FFFF2" o)))
              ;;      (rx/map #(sync-state))
              ;;      (rx/tap (fn [o] (js/console.log "FFFF2" o)))
              ;;      #_(rx/observe-on :async))
              (->> (ws/get-rcv-stream ws-conn)
                   (rx/filter ws/message-event?)
                   (rx/map :payload)
                   (rx/map #(ptk/data-event ::message %)))
              (->> (ws/get-rcv-stream ws-conn)
                   (rx/filter ws/opened-event?)
                   (rx/map (fn [_]
                             (prn "OPENED EVENT")))
                   (rx/ignore)))
             (rx/take-until stoper))))))

;; (defn- send-keepalive
;;   [file-id]
;;   (ptk/reify ::send-keepalive
;;     ptk/EffectEvent
;;     (effect [_ state _]
;;       (when-let [ws (get-in state [:ws file-id])]
;;         (ws/send! ws {:type :keepalive})))))

;; --- Finalize Websocket

(defn finalize
  []
  (ptk/reify ::finalize
    ptk/EffectEvent
    (effect [_ state _]
      (some-> (:ws state) ws/close!))))

;; (defn join-team-room
;;   [team-id]
;;   (ptk/reify ::join-team-room
;;     ptk/EffectEvent
;;     (effect [_ state _]
;;       (let [ws-data (:ws-data state)]
;;         (swap!

;;         (prn "KAKAK" (.isOpen ^js @conn))
;;         (ws/send! conn {:type :join-team-room
;;                         :team-id team-id})))))


(defn update-state
  [params]
  (ptk/reify ::update-state
    ptk/UpdateEvent
    (update [_ state]
      (prn ::update-state params)
      (update state :ws-data
              (fn [data]
                (reduce-kv (fn [data k v]
                             (if (nil? v)
                               (dissoc data k)
                               (assoc data k v)))
                           data
                           params))))))
