;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.ui.workspace.sidebar.options.menus.fill
  (:require
   [app.common.attrs :as attrs]
   [app.common.colors :as clr]
   [app.common.data :as d]
   [app.common.pages :as cp]
   [app.main.data.workspace.colors :as dc]
   [app.main.store :as st]
   [app.main.ui.icons :as i]
   [app.main.ui.workspace.sidebar.options.rows.color-row :refer [color-row]]
   [app.util.color :as uc]
   [app.util.dom :as dom]
   [app.util.i18n :as i18n :refer [tr]]
   [cuerdas.core :as str]
   [rumext.alpha :as mf]))

(def fill-attrs
  [:fills
   :fill-color
   :fill-opacity
   :fill-color-ref-id
   :fill-color-ref-file
   :fill-color-gradient
   :hide-fill-on-export])

(def fill-attrs-shape
  (conj fill-attrs :hide-fill-on-export))

;; (defn create-fill []
;;   (let [id (uuid/next)]
;;     {:id id
;;      :fill-color cp/default-color
;;      :fill-color-gradient nil
;;      ::fill-color-ref-file nil
;;      :fill-color-ref-id nil
;;      :fill-opacity 1}))

(defn color-values
  [color]
  {:color (:fill-color color)
   :opacity (:fill-opacity color)
   :id (:fill-color-ref-id color)
   :file-id (:fill-color-ref-file color)
   :gradient (:fill-color-gradient color)})

(mf/defc fill-menu
  {::mf/wrap [#(mf/memo' % (mf/check-props ["ids" "values"]))]}
  [{:keys [ids type values disable-remove?] :as props}]
  (let [label (case type
                :multiple (tr "workspace.options.selection-fill")
                :group (tr "workspace.options.group-fill")
                (tr "workspace.options.fill"))

        ;; Excluding nil values
        values (->> values
                    (remove (comp nil? val))
                    (into {}))

        only-shapes? (and (contains? values :fills)
                          ;; texts have :fill-* attributes, the rest of the shapes have :fills
                          (= (count (filter #(str/starts-with? (str %) ":fill-") (keys values))) 0))

        k1 (if (vector? (:fills values))
             (concat (:fills values) [(dissoc values :fills)])
             values)
        kk (attrs/get-attrs-multi k1 [:fill-color :fill-opacity :fill-color-ref-id :fill-color-ref-file :fill-color-gradient])

        kk (if (empty? kk)
             values
             kk)

        hide-fill-on-export? (:hide-fill-on-export values false)

        checkbox-ref (mf/use-ref)

        on-add
        (mf/use-callback
         (mf/deps ids)
         (fn [_]
           (st/emit! (dc/add-fill ids {:color cp/default-color
                                       :opacity 1}))))

        on-change
        (mf/use-callback
         (mf/deps ids)
         (fn [index]
           (fn [color]
             (st/emit! (dc/change-fill ids color index)))))

        on-change-mixed-shapes
        (mf/use-callback
         (mf/deps ids)
         (fn [color]
           (st/emit! (dc/change-fill-and-clear ids color))))

        on-remove
        (fn [index]
          (fn []
            (st/emit! (dc/remove-fill ids {:color cp/default-color
                                           :opacity 1} index))))
        on-remove-all
        (fn [_]
          (st/emit! (dc/remove-all-fills ids {:color clr/black
                                              :opacity 1})))

        ;; TODO Fix
        ;; on-detach
        ;; (mf/use-callback
        ;;  (mf/deps ids)
        ;;  (fn []
        ;;    (let [remove-multiple (fn [[_ value]] (not= value :multiple))
        ;;          color (-> (into {} (filter remove-multiple) color)
        ;;                    (assoc :id nil :file-id nil))]
        ;;      (st/emit! (dc/change-fill ids color)))))

        on-change-show-fill-on-export
        (mf/use-callback
         (mf/deps ids)
         (fn [event]
           (let [value (-> event dom/get-target dom/checked?)]
             (st/emit! (dc/change-hide-fill-on-export ids (not value))))))]

    (mf/use-layout-effect
      (mf/deps hide-fill-on-export?)
      #(let [checkbox (mf/ref-val checkbox-ref)]
         (when checkbox
           ;; Note that the "indeterminate" attribute only may be set by code, not as a static attribute.
           ;; See https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/checkbox#attr-indeterminate
           (if (= hide-fill-on-export? :multiple)
             (dom/set-attribute checkbox "indeterminate" true)
             (dom/remove-attribute checkbox "indeterminate")))))

      [:div.element-set
       [:div.element-set-title
        [:span label]
        (when (and (not disable-remove?) (not (= :multiple (:fills values))) only-shapes?)
          [:div.add-page {:on-click on-add} i/close])]

       [:div.element-set-content

        (if only-shapes?
          (cond
            (= :multiple (:fills values))
              ;; varios borrar
              ;; (contains? (set (vals values)) :multiple)
            [:div.element-set-options-group
             [:div.element-set-label (tr "settings.multiple")]
             [:div.element-set-actions
              [:div.element-set-actions-button {:on-click on-remove-all}
               i/minus]]]

            (seq (:fills values))
            (for [[index value] (d/enumerate (:fills values []))]
              [:div
               [:& color-row {:color {:color (:fill-color value)
                                      :opacity (:fill-opacity value)
                                      :id (:fill-color-ref-id value)
                                      :file-id (:fill-color-ref-file value)
                                      :gradient (:fill-color-gradient value)}
                              :title (tr "workspace.options.fill")
                              :on-change (on-change index)
                          ;; :on-detach on-detach
                              :on-remove (on-remove index)}]]))
          ;; varios editar
          ;;:else
          [:& color-row {:color {:color (:fill-color kk)
                                 :opacity (:fill-opacity kk)
                                 :id (:fill-color-ref-id kk)
                                 :file-id (:fill-color-ref-file kk)
                                 :gradient (:fill-color-gradient kk)}
                         :title (tr "workspace.options.fill")
                         :on-change on-change-mixed-shapes
                          ;; :on-detach on-detach
                         }])


        (when (or (= type :frame)
                  (and (= type :multiple) (some? hide-fill-on-export?)))
          [:div.input-checkbox
           [:input {:type "checkbox"
                    :id "show-fill-on-export"
                    :ref checkbox-ref
                    :checked (not hide-fill-on-export?)
                    :on-change on-change-show-fill-on-export}]

           [:label {:for "show-fill-on-export"}
            (tr "workspace.options.show-fill-on-export")]])]]
      ))
