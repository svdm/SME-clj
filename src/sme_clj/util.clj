
(ns mmm.util
  "Utility/helper functions"
  (:use mmm.defs

        [clojure.contrib.def :only [defvar]]))

(defvar shape? #{:circle :rectangle :triangle}
  "Set of valid shapes. Use as function to verify if given value is a valid
  shape keyword.")

(defn goal-spec?
  "A goal-spec is a map with :sender and :receiver entries, each specifying in a
  map their :shape and the :pos of their goal."
  [gs]
  (and (map? gs)
       ;; Currently only the receiver's shape is used, so we do not care about
       ;; the other entries.
       (get (get gs :receiver) :shape)))

(defn location?
  "A location is an x,y coordinate on the TCG board"
  [loc]
  (and (map? loc)
       (let [{:keys [x y]} loc]
         (and (number? x) (number? y)
              (>= x 0) (< x board-size)
              (>= y 0) (< y board-size)))))

(defn loc
  "Returns a proper location from a raw x,y coordinate."
  [x y]
  {:x x :y y})

(defvar PI2 (* 2 Math/PI)
  "2Pi")

(defn rotation?
  "A rotation is a positive number representing degrees rotated from the
  starting position in which the piece faces north (0 deg). Must be a multiple
  of the single rotation step size."
  [rot]
  (and (number? rot)
       (not (neg? rot))
       (< rot PI2)
       (zero? (mod rot rotation-step))))

(defn rot
  "Returns the result of rotating a piece that is currently in rotation 'start
  by an amount 'delta. The result will always be in the proper 0-2Pi range."
  [start delta]
  (let [r (+ start delta)]
    (cond
      (neg? r) (+ r PI2)
      (>= r PI2) (- r PI2)
      :else r)))

(defn rad2deg [x] (Math/round (Math/toDegrees x)))

(defn manhattan-distance
  "Returns the Manhattan distance between the two given locations."
  [{x1 :x y1 :y}
   {x2 :x y2 :y}]
  (+ (Math/abs (- x1 x2))
     (Math/abs (- y1 y2))))


(defn rotate-point
  "Rotate a point represented as '[x y] clockwise by the given angle (in
  radians)."
  [ang [x y]]
  (let [theta (- ang)] ; clockwise
    [(- (* x (Math/cos theta)) (* y (Math/sin theta)))
     (+ (* x (Math/sin theta)) (* y (Math/cos theta)))]))

;; The 8 different orientations a piece can be in, radians
(def rotations
     (for [r (range 0 360 45)]
       (Math/toRadians r)))             ; I know, I know...

(defn find-val
  "Linearly walks given map to find the map entry with the given value."
  [map val]
  (first (filter (fn [[k v]] (= val v)) map)))

(defn classname
  "Returns only the class name part from a class identifier."
  [cls]
  (-> (re-seq #"\.?(\w*)$" (str cls))
      first second))

(defmacro defmake
  "Defines a 'make-RecordName factory function for the given record/class."
  [recname [& fields]]
  `(defn ~(symbol (.concat "make-" (classname recname)))
     [~@fields]
     (new ~recname ~@fields)))
