;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.ui.workspace.sidebar.options.menus.exports
  (:require
   [app.common.data :as d]
   [app.main.data.messages :as dm]
   [app.main.data.modal :as modal]
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
   [beicon.core :as rx]
   [potok.core :as ptk]
   [rumext.alpha :as mf]))

(def exports-attrs [:exports])

;; TODO: move somewhere?
(defn update-export-status
  [status]
  (ptk/reify ::update-export-status
    ptk/UpdateEvent
    (update [_ state]
      (-> state
          (assoc-in [:export :export-in-progress?] status)))))

;; TODO: move somewhere?
(defn store-export-task-id
  [id total filename]
  (ptk/reify ::store-export-task-id
    ptk/UpdateEvent
    (update [_ state]
      (let [_ (println "-------------->" (:export state))]
        ;; cambiar por un update
        ;; evitar assoc-in y update-in
        (-> state
            (assoc-in [:export :export-in-progress?] true)
            (assoc-in [:export :export-widget-visibililty] true)
            (assoc-in [:export :export-detail-visibililty] true)
            (assoc-in [:export :export-total] total)
            (assoc-in [:export :export-progress] 0)
            (assoc-in [:export :export-task-id] id)
            (assoc-in [:export :export-filename] filename))))))

(defn request-export
  [object-id page-id file-id name exports]
  ;; Force a persist before exporting otherwise the exported shape could be outdated
  (st/emit! ::dwp/force-persist)
  (let [exports (mapv (fn [export]
                        (assoc export
                               :page-id page-id
                               :file-id file-id
                               :object-id object-id
                               :name name))
                      exports)]
    (rp/query! :export-shapes-simple exports)))

(defn use-download-export
  [shapes filename page-id file-id exports]
  (mf/use-callback
   (mf/deps shapes filename page-id file-id exports)
   (cond
     (and (= (count shapes) 1) (= (count exports) 1))
     (fn [event]
       (dom/prevent-default event)
       (st/emit! (update-export-status true))
       (->> (request-export (:id (first shapes)) page-id file-id filename exports)
            (rx/subs
             (fn [body]
               (dom/trigger-download filename body)
               (st/emit! (update-export-status false)))
             (fn [_error]
               (st/emit! (dm/error (tr "errors.unexpected-error")))
               (st/emit! (update-export-status false))))))

     (and (= (count shapes) 1) (> (count exports) 1))
     (fn [event]
       (let [shape (first shapes)
             exports (-> (mapv #(assoc %
                                       :page-id page-id
                                       :file-id file-id
                                       :object-id (:id shape)
                                       :name (:name shape))
                               exports)
                         flatten
                         vec)]
         (dom/prevent-default event)
         (->> (rp/query! :export-shapes-multiple exports)
              (rx/subs
               (fn [body]
                 (st/emit!
                  (modal/show
                   {:type :export-progress-dialog}))
                 (st/emit! (store-export-task-id (:id body) (count exports) filename)))
               (fn [_error]
                 ;; TODO error en export múltiple
                 (st/emit! (dm/error (tr "errors.unexpected-error"))))))))
     :else
     (fn [event]
       (dom/prevent-default event)
       (st/emit!
        (modal/show
         {:type :export-shapes
          :shapes shapes}))))))

(defn use-download-export-by-id
  [ids filename page-id file-id exports]
  (let [objects (deref (refs/objects-by-id ids))]
    (use-download-export objects filename page-id file-id exports)))

(mf/defc export-shapes-dialog
  {::mf/register modal/components
   ::mf/register-as :export-shapes}
  [{:keys [shapes]}]
  (let [export-in-progress? (mf/deref refs/export-in-progress?)
        page-id (:current-page-id @st/state)
        page (wsh/lookup-page @st/state)
        file-id (:current-file-id @st/state)
        selected (wsh/lookup-selected @st/state)
        shapes (if (some? shapes)
                 shapes
                 (if (> (count selected) 0)
                   (deref (refs/objects-by-id selected))
                   (->> (wsh/lookup-page-objects @st/state)
                        vals
                        (filter #(> (count (:exports %)) 0)))))
        exports (mf/use-state (mapv (fn [shape]
                                      (assoc shape :exports (mapv #(assoc %
                                                                          :enabled true
                                                                          :page-id page-id
                                                                          :file-id file-id
                                                                          :object-id (:id shape)
                                                                          :name (:name shape))
                                                                  (:exports shape)))) shapes))

        enabled-exports (->> (map :exports @exports)
                             (flatten)
                             (filterv #(get % :enabled)))

        checked (->> (map :exports @exports)
                     (flatten)
                     (filter #(get % :enabled)))

        all (->> (map :exports @exports)
                 (flatten))

        all-checked? (->> all
                          (every? #(get % :enabled)))

        all-unchecked? (->> all
                            (every? #(not (get % :enabled))))

        cancel-fn
        (mf/use-callback
         (fn [event]
           (dom/prevent-default event)
           (st/emit! (modal/hide))))

        accept-fn
        (mf/use-callback
         (mf/deps @exports)
         (fn [event]
           (dom/prevent-default event)
           (->> (rp/query! :export-shapes-multiple enabled-exports)
                (rx/subs
                 (fn [body]
                   (st/emit! (modal/hide))
                   (st/emit!
                    (modal/show
                     {:type :export-progress-dialog}))
                   (st/emit! (store-export-task-id (:id body) (count enabled-exports) (:name page))))
                 (fn [_error]
                   (st/emit! (dm/error (tr "errors.unexpected-error"))))))))

        on-change-handler
        (fn [_ shape-index export-index]
          (swap! exports update-in [shape-index :exports export-index :enabled] not))

        change-all
        (fn [_]
          (reset! exports (mapv (fn [shape] (assoc shape :exports (mapv #(assoc % :enabled (not all-checked?)) (:exports shape)))) shapes)))]

    [:div.modal-overlay
     [:div.modal-container.export-shapes-dialog {:class (when (empty? all) "no-shapes")}

      [:div.modal-header
       [:div.modal-header-title
        [:h2 (tr "dashboard.export-shapes.title")]]

       [:div.modal-close-button
        {:on-click cancel-fn} i/close]]

      [:*
       [:div.modal-content
        (if (> (count all) 0)
          [:*
           [:div.header
            [:div.field.check {:on-click change-all}
             (cond
               all-checked? [:span i/checkbox-checked]
               all-unchecked? [:span i/checkbox-unchecked]
               :else [:span i/checkbox-intermediate])]
            [:div.field.title (tr "dashboard.export-shapes.selected" (c (count checked)) (c (count all)))]]

           [:div.body
            (for [[shape-index shape] (d/enumerate shapes)]
              (for [[export-index export] (d/enumerate (:exports shape))]
                [:div.row
                 [:div.field.check {:on-click #(on-change-handler % shape-index export-index)}
                  (if (get-in @exports [shape-index :exports export-index :enabled])
                    [:span i/checkbox-checked]
                    [:span i/checkbox-unchecked])]

                 [:div.field.image
                  [:svg {:view-box (str (get-in shape[:selrect :x]) " " (get-in shape [:selrect :y]) " " (get-in shape [:selrect :width]) " " (get-in shape [:selrect :height]))
                         :width 24
                         :height 20
                         :version "1.1"
                         :xmlns "http://www.w3.org/2000/svg"
                         :xmlnsXlink "http://www.w3.org/1999/xlink"
                         ;; Fix Chromium bug about color of html texts
                         ;; https://bugs.chromium.org/p/chromium/issues/detail?id=1244560#c5
                         :style {:-webkit-print-color-adjust :exact}}

                   [:& shape-wrapper {:shape shape}]]]

                 [:div.field.name (str (cond-> (:name shape)
                                         (:suffix export) (str (:suffix export))))]

                 [:div.field.scale (str (* (int (get-in shape [:selrect :width])) (:scale export)) "x" (* (int (get-in shape [:selrect :height])) (:scale export)) "px ")]
                 [:div.field.extension (-> (name (:type export)) clojure.string/upper-case)]]))]

           [:div.modal-footer
            [:div.action-buttons
             [:input.cancel-button
              {:type "button"
               :value (tr "labels.cancel")
               :on-click cancel-fn}]

             [:input.accept-button.primary
              {:class (dom/classnames
                       :btn-disabled export-in-progress?)
               :disabled export-in-progress?
               :type "button"
               :value (if export-in-progress?
                        (tr "workspace.options.exporting-object")
                        (tr "labels.export"))
               :on-click (when-not export-in-progress? accept-fn)}]]]]

          [:div.no-selection
           [:img {:src "images/export-no-shapes.png" :border "0"}]
           [:p (tr "dashboard.export-shapes.no-elements")]
           [:p (tr "dashboard.export-shapes.how-to")]
           [:p (tr "dashboard.export-shapes.how-to-link")]])]]]]))

(mf/defc exports-menu
  {::mf/wrap [#(mf/memo' % (mf/check-props ["ids" "values" "type" "page-id" "file-id"]))]}
  [{:keys [ids type values page-id file-id] :as props}]
  (let [exports  (:exports values [])

        scale-enabled?
        (mf/use-callback
         (fn [export]
           (#{:png :jpeg} (:type export))))

        page (wsh/lookup-page @st/state page-id)
        first-object-name (-> (:objects page)
                              (get (first ids))
                              :name)

        filename (cond
                   ;; one export from one shape
                   (and (= (count ids) 1)
                        (= (count exports) 1)
                        (not (empty (:suffix (first exports)))))
                   (str
                    first-object-name
                    (:suffix (first exports)))

                   ;; multiple exports from one shape
                   (and (= (count ids) 1)
                        (> (count exports) 1))
                   first-object-name

                   :else
                   (:name page))

        export-in-progress? (mf/deref refs/export-in-progress?)
        on-download (use-download-export-by-id ids filename page-id file-id exports)

        add-export
        (mf/use-callback
         (mf/deps ids)
         (fn []
           (let [xspec {:type :png
                        :suffix ""
                        :scale 1}]
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
             (st/emit! (dch/update-shapes
                        ids
                        #(remove %))))))

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
         {:on-click (when-not export-in-progress? on-download)
          :class (dom/classnames
                  :btn-disabled export-in-progress?)
          :disabled export-in-progress?}
         (if export-in-progress?
           (tr "workspace.options.exporting-object")
           (tr "workspace.options.export-object" (c (count ids))))]])]))
