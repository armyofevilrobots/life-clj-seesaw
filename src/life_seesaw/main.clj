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
 
(defn fmap [f coll] (doall (map f coll)))

(defn render-cell [#^Graphics g cell]
  (let [[state x y] cell
        x  (inc (* x (dim-scale 0)))
        y  (inc (* y (dim-scale 1)))]
    (doto g
      (.setColor (if (= state :dying) (color "gray") (color "white")))
      (.fillRect x y (dec (dim-scale 0)) (dec (dim-scale 1))))))

(defn render [g img bg stage]
  (doto bg
    (.setColor (color "black"))
    (.fillRect 0 0 (dim-screen 0) (dim-screen 1)))
  (fmap (fn [col]
          (fmap #(when (not= :off (% 0))
                   (render-cell bg %)) col)) stage)
  (.drawImage g img 0 0 nil)
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

(defn activity-loop [surface stage]
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
