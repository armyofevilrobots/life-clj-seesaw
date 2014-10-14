;(use 'clojure.repl)
(ns life_seesaw.main (:gen-class))

(import '(javax.swing JFrame JPanel)
        '(java.awt Color Graphics)
        '(java.awt.image BufferedImage))
;TODO: Replace all this with imports
(use 'seesaw.core)
(use 'seesaw.color)
(use 'seesaw.graphics)

(def dim-board   [ 90   90])
(def dim-screen  [600  600])
(def dim-scale   (vec (map / dim-screen dim-board)))
(def loops-atom (atom 0))
(def last-loop-time-atom (atom 0))
(def avg-fps (atom 0))
 
(defn fmap [f coll] (doall (map f coll)))

(defn now-millis 
  "Get the current unix time in milliseconds"
  []
  (-> (java.util.Date.) .getTime))


(defn render-cell [#^Graphics g cell]
  (let [[state x y] cell
        x  (inc (* x (dim-scale 0)))
        y  (inc (* y (dim-scale 1)))]
    (doto g
      (.setColor (if (= state :dying) (color "gray") (color "white")))
      (.fillRect x y (dec (dim-scale 0)) (dec (dim-scale 1))))))


(defn last-fps
  "Get the last FPS"
  []
  (/ 1000.0 
     (Math/abs (- (deref last-loop-time-atom) 
                  (swap! last-loop-time-atom 
                         (fn [_] (now-millis))
                         )
                  )))
  )

(defn avgfps 
  "Calc the average via an accumulator"
  [fps]
  (/ 
    (swap! avg-fps (fn [oldfps] 
                     (- 
                       (+ oldfps fps) 
                       (/ oldfps 20.0)))) 
    20.0))

(defn render [g img bg stage]
  "Given a graphics g, img img, the bg, and the stage,
  render the stage onto the graphics after updating
  the stage with a new loop of conways game."
  (doto bg
    (.setColor (color "black"))
    (.fillRect 0 0 (dim-screen 0) (dim-screen 1)))
  (fmap (fn [col]
          (fmap #(when (not= :off (% 0))
                   (render-cell bg %)) col)) stage)
  (swap! loops-atom inc)
  (.drawImage g img 0 0 nil)
  (.setColor g (color "white"))
  (let [fps (last-fps)
        avg-fps (avgfps fps)]
    (.drawString g (str (deref loops-atom)) 10 15)
    (.drawString g (str 
                     (format "%3.2f" fps) 
                     "(" 
                     (format "%3.0f" avg-fps)
                     ") fps") 10 30)
    )
  )


(defn active-neighbors [above [left _ right] below]
  (count
   (filter #(= :on (% 0))
           (concat above [left right] below))))
 
(defn torus-window [coll]
  (partition 3 1 (concat [(last coll)] coll [(first coll)])))
 
(defn rules [above current below]
  (let [[self x y]  (second current)]
    (cond
      (= :on    self)                              [:dying x y]
      (= :dying self)                              [:off   x y]
      (= 2 (active-neighbors above current below)) [:on    x y]
      :else                                        [:off   x y])))


(defn step [board]
  (doall
   (pmap (fn [window]
          (apply #(doall (apply map rules %&))
                 (doall (map torus-window window))))
        (torus-window board))))

(def board
     (for [x (range (dim-board 0))]
       (for [y (range (dim-board 1))]
         [(if (< 50 (rand-int 100)) :on :off) x y])))


(defn activity-loop 
  "The activity loop that runs for-evs"
  [surface stage]
    (while true
      (swap! stage step)
      (.repaint surface)))

(defn -main []
  (let [stage (atom board)
        img   (buffered-image (dim-screen 0) (dim-screen 1))
        bg    (.createGraphics img)
        panel (canvas :paint #(render %2 img bg @stage))
        caframe (frame :title "SeeSawCA")
        ]
    (native!)
    (doto caframe (config! :content panel)
      (.setDefaultCloseOperation javax.swing.JFrame/EXIT_ON_CLOSE)
      (pack!)
      (.setSize (dim-screen 0) (dim-screen 1))
      (show!) )
    (future (activity-loop panel stage))
    ))
