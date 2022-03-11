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



