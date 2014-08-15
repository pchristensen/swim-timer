(ns swim_timer.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(def app-state
  (atom {:intervals [{:desc "5x100@3:00"}
                     {:desc "1x200@6:00"}]}))

(re-find #"x(\d+)@" "1x200@6:00")

(js/parseInt (second (re-find #"^(\d+)x" "45x200@6:00")))

(defn parse-interval-string [interval-string]
  (let [[_ num-s dist-s min-s sec-s] (re-matches #"^(\d+)x(\d+)@(\d+):(\d+)$" interval-string)
        num (js/parseInt (second (re-find #"^(\d+)x" interval-string)))
        dist (js/parseInt (second (re-find #"x(\d+)@" interval-string)))
        min (js/parseInt (second (re-find #"POOP" interval-string)))
        sec (js/parseInt (second (re-find #"POOP" interval-string)))
        total-sec (+ (* 60 min) sec)]
    {:num num
     :dist dist
     :time sec
     :desc interval-string}))

(parse-interval-string "7x100@3:00")


(defn interval-view [interval owner]
  (reify
    om/IInitState
    (init-state [_]
      {:num 0 :dist 0 :time sec :desc ""})
    om/IRender
    (render [_]
      (dom/li nil (:desc interval)))))

(defn create-interval [intervals owner]
  (let [new-interval-string-el (om/get-node owner "new-interval")
        new-interval-string (.-value new-interval-string-el)
        new-interval (parse-interval-string new-interval-string)]
    (om/transact! intervals [] #(conj % new-interval))
    (set! (.-value new-interval-string-el) "")))

(defn create-interval-view [intervals owner]
  (reify
    om/IInitState
    (init-state [_]
      {:new-interval {:num 0
                      :dist 0
                      :time 0
                      :desc ""}})
    om/IRenderState
    (render-state [_ {:keys [new-interval]}]
      (dom/div #js {:id "add-interval"}
        (dom/div nil
          (dom/input #js {:ref "new-interval"})
          (dom/button
            #js {:onClick (fn [e] (create-interval intervals owner))}
            "Save"))
        (dom/div #js {:id "add-interval-preview"}
          (str (:num new-interval)))))))

(defn app-view [app owner]
  (reify
    om/IRender
    (render [_]
      (dom/div nil
        (dom/h1 nil "Intervals")
        (om/build create-interval-view (:intervals app))
        (apply dom/ul nil
               (map
                 (fn [i]
                   (om/build interval-view i))
                 (:intervals app)))))))

(om/root
  app-view
  app-state
  {:target (. js/document (getElementById "app"))})
