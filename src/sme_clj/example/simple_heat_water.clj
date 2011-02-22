
(ns sme-clj.example.simple-heat-water
  (:use sme-clj.typedef))

(defpredicate flow
  :type :relation
  :arity 4)

(defpredicate greater
  :type :relation
  :arity 2)

(defpredicate cause
  :type :relation
  :arity 2)

(defpredicate temperature
  :type :function)

(defpredicate flat-top
  :type :function)

(defpredicate pressure
  :type :function)

(defpredicate diameter
  :type :function)

(defpredicate liquid
  :type :attribute)

(defpredicate clear
  :type :attribute)

(defentity Coffee [])
(defentity Icecube [])
(defentity Bar [])
(defentity Heat [])

(defentity Water [])
(defentity Beaker [])
(defentity Vial [])
(defentity Pipe [])


;; TODO:
;; - document this example a bit, refer to falkenhainer
;;   - match gives three gmaps, third one is the correct one IIRC

(def simple-heat-flow
  (make-concept-graph "simple heat flow" e

                      (e flow Coffee Icecube Bar Heat)
                      (e greater (e temperature Coffee) (e temperature Icecube))
                      (e flat-top Coffee)
                      (e liquid Coffee)))

(def simple-water-flow
  (make-concept-graph "simple water flow" e

                      (e cause
                         (e greater (e pressure Beaker) (e pressure Vial))
                         (e flow Beaker Vial Water Pipe))
                      (e greater (e diameter Beaker) (e diameter Vial))
                      (e clear Beaker)
                      (e flat-top Water)
                      (e liquid Water)))
