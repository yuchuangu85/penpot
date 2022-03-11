;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.data.workspace.exports
  (:require
   [app.main.data.messages :as dm]
   [app.main.repo :as rp]
   [app.main.store :as st]
   [app.util.i18n :as i18n :refer  [tr]]
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
  (ptk/reify ::toggle-export-detail-visibililty
    ptk/UpdateEvent
    (update [_ state]
      (-> state
          (assoc-in [:export :export-detail-visibililty] visibility)))))

(defn update-export-status
  [status]
  (ptk/reify ::update-export-status
    ptk/UpdateEvent
    (update [_ state]
      (-> state
          (update state :export #(assoc % :export-in-progress? status))))))

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

(defn retry-export
  []
  (ptk/reify ::retry-export
    ptk/WatchEvent
    (watch [_ state _]
      (let [exports (get-in state [:export :exports])
            filename (get-in state [:export :export-filename])]
        (->> (rp/query! :export-shapes-multiple exports)
             (rx/subs
              (fn [body]
                (st/emit! (store-export-task-id (:id body) exports filename)))
              (fn [_error]
                (st/emit! (dm/error (tr "errors.unexpected-error"))))))))))
