;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.ui.workspace.sidebar.options.menus.exports
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.main.data.messages :as msg]
   [app.main.data.modal :as modal]
   [app.main.data.exports :as dwe]
   [app.main.data.workspace.changes :as dch]
   [app.main.data.workspace.persistence :as dwp]
   [app.main.data.workspace.state-helpers :as wsh]
   [app.main.refs :as refs]
   [app.main.ui.workspace.shapes :refer [shape-wrapper]]
   [app.main.repo :as rp]
   [app.main.store :as st]
   [app.main.ui.icons :as i]
   [app.util.dom :as dom]
   [app.util.i18n :as i18n :refer  [tr, c]]
   [app.util.object :as obj]
   [cuerdas.core :as str]
   [app.util.timers :as ts]
   [beicon.core :as rx]
   [potok.core :as ptk]
   [rumext.alpha :as mf]))

(def exports-attrs
  "Shape attrs that corresponds to exports. Used in other namespaces."
  [:exports])

;; (defn prepare-exports-data
;;   [shapes file-id page-id]
;;   (into []
;;         (mapcat (fn [{:keys [id exports name] :as shape}]
;;                   (map (fn [export]
;;                          (-> export
;;                              (assoc :enabled true)
;;                              (assoc :page-id page-id)
;;                              (assoc :file-id file-id)
;;                              (assoc :object-id id)
;;                              (assoc :name name)))
;;                        exports)))
;;         shapes))



(defn prepare-exports-data
  [shapes file-id page-id]
  (letfn [(process-shape [{:keys [id name] :as shape}]
            (update shape :exports (fn [exports]
                                     (mapv (fn [export]
                                             (-> export
                                                 (assoc :enabled true)
                                                 (assoc :page-id page-id)
                                                 (assoc :file-id file-id)
                                                 (assoc :object-id id)
                                                 (assoc :name name)))
                                           exports))))]
    (sequence (map process-shape) shapes)))


(mf/defc export-shapes-dialog
  {::mf/register modal/components
   ::mf/register-as :export-shapes}
  [{:keys [shape-ids page-id file-id]}]
  (let [lstate          (mf/deref refs/export)
        in-progress?    (:in-progress lstate)

        selected        (wsh/lookup-selected @st/state)
        shapes          (cond
                          (some? shape-ids)      (wsh/lookup-shapes @st/state shape-ids)
                          (> (count selected) 0) (wsh/lookup-selected @st/state selected)
                          :else                  (wsh/filter-shapes @st/state #(pos? (count (:exports %)))))

        exports         (mf/use-state (prepare-exports-data shapes file-id page-id))

        all-exports     (into [] (mapcat :exports) @exports)
        all-checked?    (every? :enabled all-exports)
        all-unchecked?  (every? (complement :enabled) all-exports)


        enabled-exports (into []
                              (comp (mapcat :exports)
                                    (filter :enabled))
                              @exports)


        ;; checked         (->> (map :exports @exports)
        ;;                      (flatten)
        ;;                      (filter #(get % :enabled)))

        cancel-fn
        (mf/use-fn
         (fn [event]
           (dom/prevent-default event)
           (st/emit! (modal/hide))))

        accept-fn
        (mf/use-callback
         (mf/deps @all-exports)
         (fn [event]
           (dom/prevent-default event)
           (swap! st/ongoing-tasks conj :export)
           #_(->> (rp/query! :export-shapes-multiple enabled-exports)
                (rx/subs
                 (fn [body]
                   (st/emit! (modal/hide))
                   (st/emit! (modal/show {:type :export-progress-dialog}))
                   #_(st/emit! (dwe/store-export-task-id (:id body) enabled-exports (:name page))))
                 (fn [_error]
                   (st/emit! (msg/error (tr "errors.unexpected-error"))))))))

        on-toggle-enabled
        (fn [_ shape-index export-index]
          (swap! exports update-in [shape-index :exports export-index :enabled] not))

        change-all
        (fn [_]
          (reset! exports (mapv (fn [shape] (assoc shape :exports (mapv #(assoc % :enabled (not all-checked?)) (:exports shape)))) shapes)))]

    [:div.modal-overlay
     [:div.modal-container.export-shapes-dialog
      {:class (when (empty? all-exports) "no-shapes")}

      [:div.modal-header
       [:div.modal-header-title
        [:h2 (tr "dashboard.export-shapes.title")]]

       [:div.modal-close-button
        {:on-click cancel-fn} i/close]]

      [:*
       [:div.modal-content
        (if (> (count all-exports) 0)
          [:*
           [:div.header
            [:div.field.check {:on-click change-all}
             (cond
               all-checked? [:span i/checkbox-checked]
               all-unchecked? [:span i/checkbox-unchecked]
               :else [:span i/checkbox-intermediate])]
            [:div.field.title (tr "dashboard.export-shapes.selected"
                                  (c (count enabled-exports))
                                  (c (count @all-exports)))]]

           [:div.body
            (for [[shape-index shape] (d/enumerate shapes)]
              (for [[export-index export] (d/enumerate (:exports shape))]
                (let [{:keys [x y width height]} (:selrect shape)
                      shape-name    (:name shape)
                      export-suffix (:suffix export)]
                  [:div.row
                   [:div.field.check {:on-click #(on-toggle-enabled % shape-index export-index)}
                    (if (get-in @exports [shape-index :exports export-index :enabled])
                      [:span i/checkbox-checked]
                      [:span i/checkbox-unchecked])]

                   [:div.field.image
                    [:svg {:view-box (dm/str x " " y " " width " " height)
                           :width 24
                           :height 20
                           :version "1.1"
                           :xmlns "http://www.w3.org/2000/svg"
                           :xmlnsXlink "http://www.w3.org/1999/xlink"
                           ;; Fix Chromium bug about color of html texts
                           ;; https://bugs.chromium.org/p/chromium/issues/detail?id=1244560#c5
                           :style {:-webkit-print-color-adjust :exact}}

                     [:& shape-wrapper {:shape shape}]]]

                   [:div.field.name (cond-> shape-name export-suffix (str export-suffix))]
                   [:div.field.scale (dm/str (* width (:scale export)) "x"
                                             (* height (:scale export)) "px ")]
                   [:div.field.extension (-> export :type d/name str/upper)]])))]

           [:div.modal-footer
            [:div.action-buttons
             [:input.cancel-button
              {:type "button"
               :value (tr "labels.cancel")
               :on-click cancel-fn}]

             [:input.accept-button.primary
              {:class (dom/classnames
                       :btn-disabled in-progress?)
               :disabled in-progress?
               :type "button"
               :value (if in-progress?
                        (tr "workspace.options.exporting-object")
                        (tr "labels.export"))
               :on-click (when-not in-progress? accept-fn)}]]]]

          [:div.no-selection
           [:img {:src "images/export-no-shapes.png" :border "0"}]
           [:p (tr "dashboard.export-shapes.no-elements")]
           [:p (tr "dashboard.export-shapes.how-to")]
           [:p (tr "dashboard.export-shapes.how-to-link")]])]]]]))


(mf/defc exports-menu
  {::mf/wrap [#(mf/memo' % (mf/check-props ["ids" "values" "type" "page-id" "file-id"]))]}
  [{:keys [ids type values page-id file-id] :as props}]
  (let [exports      (:exports values [])


        _    (prn ids type values)

        state        (mf/deref refs/export)
        in-progress? (:in-progress state)

        filename     (when (seqable? exports)
                       (let [shapes (wsh/lookup-shapes @st/state ids)
                             sname  (-> shapes first :name)
                             suffix (-> exports first :suffix)]
                         (cond-> sname
                           (and (= 1 (count exports)) (some? suffix))
                           (str suffix))))

        scale-enabled?
        (mf/use-callback
         (fn [export]
           (#{:png :jpeg} (:type export))))

        ;; ;; This only can be caused when
        ;; on-download-error
        ;; (fn [cause]
        ;;   (st/emit! (msg/error (tr "errors.unexpected-error"))))

        on-download-success
        (fn [_]
          (ts/schedule 5000 #(st/emit! (dwe/clear-export-state))))

        on-download
        (mf/use-fn
         (mf/deps ids page-id file-id exports)
         (fn [event]
           (dom/prevent-default event)
           (if (= :multiple exports)
             (st/emit! (modal/show {:type :export-shapes
                                    :page-id page-id
                                    :file-id file-id
                                    :shape-ids ids}))

             ;; in other all cases we only allowed to have a single
             ;; shape-id because multiple shape-ids are handled
             ;; separatelly by the export-modal.
             (let [defaults {:page-id page-id
                             :file-id file-id
                             :name filename
                             :object-id (first ids)}
                   exports  (mapv #(merge % defaults) exports)]
               (if (= 1 (count exports))
                 (st/emit! (dwe/request-simple-export
                            (with-meta {:export (first exports)
                                        :filename filename}
                              {:on-success on-download-success
                               ;; :on-error on-download-error
                               })))
                 (st/emit! (dwe/request-multiple-export
                            (with-meta {:exports exports
                                        :filename filename}
                              {:on-success on-download-success
                               ;; :on-error on-download-error
                               }))))))))


        ;; TODO: maybe move to specific events for avoid to have this logic here?
        add-export
        (mf/use-callback
         (mf/deps ids)
         (fn []
           (let [xspec {:type :png :suffix "" :scale 1}]
             (st/emit! (dch/update-shapes ids
                                          (fn [shape]
                                            (assoc shape :exports (into [xspec] (:exports shape)))))))))

        delete-export
        (mf/use-callback
         (mf/deps ids)
         (fn [position]
           (let [remove-fill-by-index (fn [values index] (->> (d/enumerate values)
                                                              (filterv (fn [[idx _]] (not= idx index)))
                                                              (mapv second)))

                 remove (fn [shape] (update shape :exports remove-fill-by-index position))]
             (st/emit! (dch/update-shapes ids remove)))))

        on-scale-change
        (mf/use-callback
         (mf/deps ids)
         (fn [index event]
           (let [target  (dom/get-target event)
                 value   (dom/get-value target)
                 value   (d/parse-double value)]
             (st/emit! (dch/update-shapes ids
                                          (fn [shape]
                                            (assoc-in shape [:exports index :scale] value)))))))

        on-suffix-change
        (mf/use-callback
         (mf/deps ids)
         (fn [index event]
           (let [target  (dom/get-target event)
                 value   (dom/get-value target)]
             (st/emit! (dch/update-shapes ids
                                          (fn [shape]
                                            (assoc-in shape [:exports index :suffix] value)))))))

        on-type-change
        (mf/use-callback
         (mf/deps ids)
         (fn [index event]
           (let [target  (dom/get-target event)
                 value   (dom/get-value target)
                 value   (keyword value)]
             (st/emit! (dch/update-shapes ids
                                          (fn [shape]
                                            (assoc-in shape [:exports index :type] value)))))))

        on-remove-all
        (mf/use-callback
         (mf/deps ids)
         (fn []
           (st/emit! (dch/update-shapes ids
                                        (fn [shape]
                                          (assoc shape :exports []))))))]

    [:div.element-set.exports-options
     [:div.element-set-title
      [:span (tr (if (> (count ids) 1) "workspace.options.export-multiple" "workspace.options.export"))]
      (when (not (= :multiple exports))
        [:div.add-page {:on-click add-export} i/close])]

     (cond
       (= :multiple exports)
       [:div.element-set-options-group
        [:div.element-set-label (tr "settings.multiple")]
        [:div.element-set-actions
         [:div.element-set-actions-button {:on-click on-remove-all}
          i/minus]]]


       (seq exports)
       [:div.element-set-content
        (for [[index export] (d/enumerate exports)]
          [:div.element-set-options-group
           {:key index}
           (when (scale-enabled? export)
             [:select.input-select {:on-change (partial on-scale-change index)
                                    :value (:scale export)}
              [:option {:value "0.5"}  "0.5x"]
              [:option {:value "0.75"} "0.75x"]
              [:option {:value "1"} "1x"]
              [:option {:value "1.5"} "1.5x"]
              [:option {:value "2"} "2x"]
              [:option {:value "4"} "4x"]
              [:option {:value "6"} "6x"]])
           [:input.input-text {:value (:suffix export)
                               :placeholder (tr "workspace.options.export.suffix")
                               :on-change (partial on-suffix-change index)}]
           [:select.input-select {:value (name (:type export))
                                  :on-change (partial on-type-change index)}
            [:option {:value "png"} "PNG"]
            [:option {:value "jpeg"} "JPEG"]
            [:option {:value "svg"} "SVG"]
            [:option {:value "pdf"} "PDF"]]
           [:div.delete-icon {:on-click (partial delete-export index)}
            i/minus]])

        [:div.btn-icon-dark.download-button
         {:on-click (when-not in-progress? on-download)
          :class (dom/classnames
                  :btn-disabled in-progress?)
          :disabled in-progress?}
         (if in-progress?
           (tr "workspace.options.exporting-object")
           (tr "workspace.options.export-object" (c (count ids))))]])]))
