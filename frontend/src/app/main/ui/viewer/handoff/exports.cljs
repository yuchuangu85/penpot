;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.ui.viewer.handoff.exports
  (:require
   [app.common.data :as d]
   [app.main.refs :as refs]
   [app.main.ui.icons :as i]
   ;; [app.main.ui.workspace.sidebar.options.menus.exports :as we]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr, c]]
   [rumext.alpha :as mf]))

(mf/defc exports
  [{:keys [shapes name page-id file-id] :as props}]
  (let [exports  (mf/use-state [])
        first-object-name (-> (first shapes) :name)
        viewer-state (mf/deref refs/viewer-data)
        page (get-in viewer-state [:pages page-id])
        filename (cond
                   ;; one export from one shape
                   (and (= (count shapes) 1)
                        (= (count @exports) 1)
                        (not (empty (:suffix (first @exports)))))
                   (str
                    first-object-name
                    (:suffix (first @exports)))

                   ;; multiple exports from one shape
                   (and (= (count shapes) 1)
                        (> (count @exports) 1))
                   first-object-name

                   :else
                   (:name page))


        ;; TODO: fix this
        export-in-progress? (mf/deref refs/export-in-progress?)
        ;; on-download (we/use-download-export shapes filename page-id file-id @exports)
        on-download (constantly nil)

        add-export
        (mf/use-callback
         (mf/deps shapes)
         (fn []
           (let [xspec {:type :png
                        :suffix ""
                        :scale 1}]
             (swap! exports conj xspec))))

        delete-export
        (mf/use-callback
         (mf/deps shapes)
         (fn [index]
           (swap! exports (fn [exports]
                            (let [[before after] (split-at index exports)]
                              (d/concat-vec before (rest after)))))))

        on-scale-change
        (mf/use-callback
         (mf/deps shapes)
         (fn [index event]
           (let [target  (dom/get-target event)
                 value   (dom/get-value target)
                 value   (d/parse-double value)]
             (swap! exports assoc-in [index :scale] value))))

        on-suffix-change
        (mf/use-callback
         (mf/deps shapes)
         (fn [index event]
           (let [target  (dom/get-target event)
                 value   (dom/get-value target)]
             (swap! exports assoc-in [index :suffix] value))))

        on-type-change
        (mf/use-callback
         (mf/deps shapes)
         (fn [index event]
           (let [target  (dom/get-target event)
                 value   (dom/get-value target)
                 value   (keyword value)]
             (swap! exports assoc-in [index :type] value))))]

    (mf/use-effect
     (mf/deps shapes)
     (fn []
       (reset! exports (-> (mapv #(:exports % []) shapes)
                           flatten
                           distinct
                           vec))))

    [:div.element-set.exports-options
     [:div.element-set-title
      [:span (tr "workspace.options.export")]
      [:div.add-page {:on-click add-export} i/close]]

     (when (seq @exports)
       [:div.element-set-content
        (for [[index export] (d/enumerate @exports)]
          [:div.element-set-options-group
           {:key index}
           [:select.input-select {:on-change (partial on-scale-change index)
                                  :value (:scale export)}
            [:option {:value "0.5"}  "0.5x"]
            [:option {:value "0.75"} "0.75x"]
            [:option {:value "1"} "1x"]
            [:option {:value "1.5"} "1.5x"]
            [:option {:value "2"} "2x"]
            [:option {:value "4"} "4x"]
            [:option {:value "6"} "6x"]]

           [:input.input-text {:on-change (partial on-suffix-change index)
                               :value (:suffix export)}]
           [:select.input-select {:on-change (partial on-type-change index)
                                  :value (d/name (:type export))}
            [:option {:value "png"} "PNG"]
            [:option {:value "jpeg"} "JPEG"]
            [:option {:value "svg"} "SVG"]]

           [:div.delete-icon {:on-click (partial delete-export index)}
            i/minus]])

        [:div.btn-icon-dark.download-button
         {:on-click (when-not export-in-progress? on-download)
          :class (dom/classnames :btn-disabled export-in-progress?)
          :disabled export-in-progress?}
         (if export-in-progress?
           (tr "workspace.options.exporting-object")
           (tr "workspace.options.export-object" (c (count shapes))))]])]))

