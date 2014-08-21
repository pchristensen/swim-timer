(ns swim_timer.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(defn extract-int [s regex]
  (if-let [num-str (second (re-find regex s))]
    (js/parseInt num-str)))

(defn parse-interval-string [interval-string]
  (let [[_ num-s dist-s min-s sec-s] (re-matches #"^(\d+)x(\d+)@(\d+):(\d+)$" interval-string)
        num  (extract-int interval-string #"^(\d+)x")
        dist (extract-int interval-string #"x(\d+)@")
        min  (extract-int interval-string #"@(\d+):")
        sec  (extract-int interval-string #":(\d+)$")
        total-sec (and min sec (+ (* 60 min) sec))]
    {:num num
     :dist dist
     :time total-sec
     :desc interval-string}))

(defn validate-interval [{:keys [num dist time] :as i}]
  (and num dist time))

(defn get-time
  "current time as a map"
  []
  (let [d (js/Date.)]
    {:hours (.getHours d)
     :minutes (.getMinutes d)
     :seconds (.getSeconds d)}))

(def app-state
  (atom {:intervals []
         :current-time (get-time)}))

(defn interval-view [i owner]
  (reify
    om/IRender
    (render [_]
      (dom/li nil (str (:dist i) " @ " (:time i))))))

(defn create-interval [intervals owner]
  (let [new-interval-string-el (om/get-node owner "new-interval")
        new-interval-string (.-value new-interval-string-el)
        new-interval (parse-interval-string new-interval-string)
        valid? (validate-interval new-interval)]
    (if valid?
      (do
        (om/set-state! owner [:text] "")
        (dotimes [n (:num new-interval)]
          (om/transact! intervals #(conj % (dissoc new-interval :num))))))))

(defn create-interval-view [{:keys [intervals current-time]} owner]
  (reify
    om/IInitState
    (init-state [_]
      {:text ""})
    om/IWillMount
    (will-mount [_]
      (js/setInterval
        (fn [] (om/update! current-time (get-time)))
        1000))
    om/IRenderState
    (render-state [_ {:keys [text]}]
      (let [new-interval (parse-interval-string text)]
        (dom/div #js {:id "add-interval"}
          (dom/div nil
            (dom/input
              #js {:ref "new-interval"
                   :value text
                   :onChange (fn [e]
                               (om/set-state! owner [:text] (.. e -target -value)))
                   :onKeyPress #(when (== (.-keyCode %) 13)
                                  (create-interval intervals owner))})
            (dom/button
              #js {:disabled (not (validate-interval new-interval))
                   :onClick (fn [e] (create-interval intervals owner))}
              "Save"))
          (dom/div #js {:id "instructions"}
                   "Use format '<number of reps>x<distance>@<minutes>:<seconds>'")
          (println-str current-time)
          (dom/div #js {:id "preview"}
            (str (get new-interval :num)
                 " x "
                 (get new-interval :dist)
                 " @ "
                 (get new-interval :time))))))))

(defn app-view [app owner]
  (reify
    om/IRender
    (render [_]
      (dom/div nil
        (dom/h1 nil "Intervals")
        (om/build create-interval-view app)
        (apply dom/ul
               nil
               (om/build-all interval-view
                             (:intervals app)))))))

(om/root
  app-view
  app-state
  {:target (. js/document (getElementById "app"))})
