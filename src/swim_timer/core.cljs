(ns swim_timer.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(def state-transitions
  {:waiting :timing
   :timing  :resting
   :resting :complete})

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

(defn split-time [total-sec]
  (let [min (quot total-sec 60)
        sec (rem total-sec 60)]
    {:min min :sec sec}))

(defn time-str [{:keys [min sec]}]
  (str min
       ":"
       (if (< sec 10) "0") ; ghetto zero-padding for seconds
       sec))

(defn start-timer [app]
  (om/update! app :timer-state :running)
  (om/update! app :timer-id (js/setInterval
                               (fn [] (om/transact! app :elapsed-seconds #(inc %)))
                               1000)))

(defn stop-timer [app]
  (om/update! app :timer-state :stopped)
  (js/clearInterval (:timer-id @app))
  (om/update! app :timer-id nil))

(def app-state
  (atom {:intervals []
         :elapsed-seconds 0
         :timer-id nil
         :timer-state :stopped}))

(defn interval-view [i owner]
  (reify
    om/IRender
    (render [_]
      (dom/li nil (str (:dist i) " @ " (time-str (split-time (:time i))) " - " (:state i))))))

(defn create-interval [intervals owner]
  (let [interval-string-el (om/get-node owner "new-interval")
        interval-string (.-value interval-string-el)
        raw-interval (parse-interval-string interval-string)
        valid? (validate-interval raw-interval)
        num-to-create (:num raw-interval)
        new-interval (assoc (dissoc raw-interval :num)
                            :state :waiting)]
    (if valid?
      (do
        (om/set-state! owner [:text] "")
        (dotimes [n num-to-create]
          (om/transact! intervals #(conj % new-interval)))))))

(defn create-interval-view [{:keys [intervals]} owner]
  (reify
    om/IInitState
    (init-state [_]
      {:text ""})
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
          (dom/div #js {:id "preview"}
            (str (get new-interval :num)
                 " x "
                 (get new-interval :dist)
                 " @ "
                 (get new-interval :time))))))))

(defn app-view [{:keys [intervals timer-state] :as app} owner]
  (reify
    om/IRender
    (render [_]
      (dom/div nil
        (dom/h1 nil "Intervals")
        (dom/h3 nil (str "Timer ID: " (:timer-id app)))
        (dom/h3 nil (str "Elapsed seconds: " (:elapsed-seconds app)))
        (om/build create-interval-view app)
        (dom/div #js {:id "start-stop"}
          (dom/button
            #js {:disabled (not= :stopped timer-state)
                 :onClick (fn [e] (start-timer app))}
            "Start")
          (dom/button
            #js {:disabled (= :stopped (:timer-state app))
                 :onClick (fn [e] (stop-timer app))}
            "Stop"))
        (apply dom/ul
               nil
               (om/build-all interval-view
                             intervals))
        (let [total-sec (reduce + (map :time intervals))]
          (dom/div #js {:id "total-time"}
                  (str "Total time: " (time-str (split-time total-sec)))))))))

(om/root
  app-view
  app-state
  {:target (. js/document (getElementById "app"))})
