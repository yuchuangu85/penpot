;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.data.workspace.exports
  (:require
   [potok.core :as ptk]))

(defn toggle-export-detail-visibililty
  ;; [&]
  []
  (ptk/reify ::toggle-export-detail-visibililty
    ptk/UpdateEvent
    (update [_ state]
      (let [visibility (get-in state [:export :export-detail-visibililty] false)]
        (-> state
            (assoc-in [:export :export-detail-visibililty] (not visibility)))))))

(defn set-export-detail-visibililty
  ;; [&]
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
  [id total filename]
  (ptk/reify ::store-export-task-id
    ptk/UpdateEvent
    (update [_ state]
      (-> state
          (assoc :export {:export-in-progress? true
                          :export-widget-visibililty true
                          :export-detail-visibililty true
                          :export-total total
                          :export-progress 0
                          :export-task-id id
                          :export-filename filename})))))
