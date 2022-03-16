(ns app.components-sync-test
  (:require
    [debug :as debug]
    [app.common.colors :as clr]
    [app.common.data :as d]
    [app.common.geom.point :as gpt]
    [app.main.data.workspace.changes :as dwc]
    [app.main.data.workspace.libraries :as dwl]
    [app.main.data.workspace.libraries-helpers :as dwlh]
    [app.main.data.workspace.state-helpers :as wsh]
    [app.test-helpers.events :as the]
    [app.test-helpers.libraries :as thl]
    [app.test-helpers.pages :as thp]
    [beicon.core :as rx]
    [cljs.pprint :refer [pprint]]
    [cljs.test :as t :include-macros true]
    [linked.core :as lks]
    [potok.core :as ptk]))

(t/use-fixtures :each
  {:before thp/reset-idmap!})

(t/deftest test-touched
  (t/async done
    (try
      (let [state (-> thp/initial-state
                      (thp/sample-page)
                      (thp/sample-shape :shape1 :rect
                                        {:name "Rect 1"
                                         :fill-color clr/white
                                         :fill-opacity 1})
                      (thp/make-component :instance1 :component-1
                                          [(thp/id :shape1)]))

            shape1    (thp/get-shape state :shape1)

            update-fn (fn [shape]
                        (merge shape {:fill-color clr/test
                                      :fill-opacity 0.5}))

            store (the/prepare-store state done
             (fn [new-state]
               ; Expected shape tree:
               ;
               ; [Page]
               ; Root Frame
               ;   Rect 1-1            #--> Rect 1-1
               ;     Rect 1*           ---> Rect 1
               ;         #{:fill-group}
               ;
               ; [Rect 1]
               ; Rect 1-1
               ;   Rect 1
               ;
               (let [instance1 (thp/get-shape new-state :instance1)
                     shape1 (thp/get-shape new-state :shape1)

                     [[group shape1] [c-group c-shape1] component]
                     (thl/resolve-instance-and-main
                       new-state
                       (:id instance1))]

                 (t/is (= (:fill-color shape1) clr/test))
                 (t/is (= (:fill-opacity shape1) 0.5))
                 (t/is (= (:touched shape1) #{:fill-group}))
                 (t/is (= (:fill-color c-shape1) clr/white))
                 (t/is (= (:fill-opacity c-shape1) 1))
                 (t/is (= (:touched c-shape1) nil)))))]

        (ptk/emit!
          store
          (dwc/update-shapes [(:id shape1)] update-fn)
          :the/end)))))

(t/deftest test-reset-changes
  (t/async done
    (try
      (let [state (-> thp/initial-state
                      (thp/sample-page)
                      (thp/sample-shape :shape1 :rect
                                        {:name "Rect 1"
                                         :fill-color clr/white
                                         :fill-opacity 1})
                      (thp/make-component :instance1 :component-1
                                          [(thp/id :shape1)]))

            shape1    (thp/get-shape state :shape1)
            instance1 (thp/get-shape state :instance1)

            update-fn (fn [shape]
                        (merge shape {:fill-color clr/test
                                      :fill-opacity 0.5}))

            store (the/prepare-store state done
              (fn [new-state]
                ; Expected shape tree:
                ;
                ; [Page]
                ; Root Frame
                ;   Rect 1-1            #--> Rect 1-1
                ;     Rect 1            ---> Rect 1
                ; 
                ; [Rect 1]
                ; Rect 1-1
                ;   Rect 1
                ;
                (let [shape1 (thp/get-shape new-state :shape1)

                      [[group shape1] [c-group c-shape1] component]
                      (thl/resolve-instance-and-main
                        new-state
                        (:id instance1))]

                  (t/is (= (:fill-color shape1) clr/white))
                  (t/is (= (:fill-opacity shape1) 1))
                  (t/is (= (:touched shape1) nil))
                  (t/is (= (:fill-color c-shape1) clr/white))
                  (t/is (= (:fill-opacity c-shape1) 1))
                  (t/is (= (:touched c-shape1) nil)))))]

        (ptk/emit!
          store
          (dwc/update-shapes [(:id shape1)] update-fn)
          (dwl/reset-component (:id instance1))
          :the/end)))))

(t/deftest test-update-component
  (t/async done
    (try
      (let [state (-> thp/initial-state
                      (thp/sample-page)
                      (thp/sample-shape :shape1 :rect
                                        {:name "Rect 1"
                                         :fill-color clr/white
                                         :fill-opacity 1})
                      (thp/make-component :instance1 :component-1
                                          [(thp/id :shape1)]))

            shape1    (thp/get-shape state :shape1)
            instance1 (thp/get-shape state :instance1)

            update-fn (fn [shape]
                        (merge shape {:fill-color clr/test
                                      :fill-opacity 0.5}))

            store (the/prepare-store state done
              (fn [new-state]
                ; Expected shape tree:
                ;
                ; [Page]
                ; Root Frame
                ;   Rect 1-1            #--> Rect 1-1
                ;     Rect 1            ---> Rect 1
                ; 
                ; [Rect 1]
                ; Rect 1-1
                ;   Rect 1
                ;
                (let [[[group shape1] [c-group c-shape1] component]
                      (thl/resolve-instance-and-main
                        new-state
                        (:id instance1))]

                  (t/is (= (:fill-color shape1) clr/test))
                  (t/is (= (:fill-opacity shape1) 0.5))
                  (t/is (= (:touched shape1) nil))
                  (t/is (= (:fill-color c-shape1) clr/test))
                  (t/is (= (:fill-opacity c-shape1) 0.5))
                  (t/is (= (:touched c-shape1) nil)))))]

        (ptk/emit!
          store
          (dwc/update-shapes [(:id shape1)] update-fn)
          (dwl/update-component (:id instance1))
          :the/end)))))

(t/deftest test-update-component-and-sync
  (t/async done
    (try
      (let [state (-> thp/initial-state
                      (thp/sample-page)
                      (thp/sample-shape :shape1 :rect
                                        {:name "Rect 1"
                                         :fill-color clr/white
                                         :fill-opacity 1})
                      (thp/make-component :instance1 :component-1
                                          [(thp/id :shape1)])
                      (thp/instantiate-component :instance2
                                                 (thp/id :component-1)))

            file      (wsh/get-local-file state)

            shape1    (thp/get-shape state :shape1)
            instance1 (thp/get-shape state :instance1)

            update-fn (fn [shape]
                        (merge shape {:fill-color clr/test
                                      :fill-opacity 0.5}))

            store (the/prepare-store state done
              (fn [new-state]
                ; Expected shape tree:
                ;
                ; [Page]
                ; Root Frame
                ;   Rect 1-1            #--> Rect 1-1
                ;     Rect 1            ---> Rect 1
                ;   Rect 1-2            #--> Rect 1-1
                ;     Rect 1            ---> Rect 1
                ;
                ; [Rect 1]
                ; Rect 1-1
                ;   Rect 1
                ;
                (let [instance2 (thp/get-shape state :instance2)

                      [[group shape2] [c-group c-shape2] component]
                      (thl/resolve-instance-and-main
                        new-state
                        (:id instance2))]

                  (t/is (= (:fill-color shape2) clr/test))
                  (t/is (= (:fill-opacity shape2) 0.5))
                  (t/is (= (:touched shape2) nil))
                  (t/is (= (:fill-color c-shape2) clr/test))
                  (t/is (= (:fill-opacity c-shape2) 0.5))
                  (t/is (= (:touched c-shape2) nil)))))]

        (ptk/emit!
          store
          (dwc/update-shapes [(:id shape1)] update-fn)
          (dwl/update-component-sync (:id instance1) (:id file))
          :the/end)))))

