
(ns sme-clj.example.simple-heat-water
  "Example adapted from SME literature [1], of an analogy between the flow of
   water from a large beaker through a pipe to a small vial, and the flow of
   heat from a cup of coffee through a bar into an ice cube.

   This is the running example Falkenhainer et al. use in their description of
   the SME algorithm.

    [1] Falkenhainer, Forbus & Gentner (1989). The structure-mapping engine:
          algorithm and examples. Artificial Intelligence, 41, 1-62.
  "
  (:use sme-clj.typedef)
  (:require [sme-clj.core   :as sme]
            [clojure.pprint :as pp]))

;; Predicate definitions

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

;; Entities
(defentity Coffee []) ; [] -> no value slots in entity
(defentity Icecube [])
(defentity Bar [])
(defentity Heat [])

(defentity Water [])
(defentity Beaker [])
(defentity Vial [])
(defentity Pipe [])


;; Concept graph definitions

(def simple-heat-flow
  (make-concept-graph "simple heat flow" e

                      (e flow Coffee Icecube Heat Bar)
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

;; Commented out example
#_(do
    ;; Water flow is the base, heat flow the target
    (def result (sme/match simple-water-flow simple-heat-flow))
    (def gmaps (:gmaps result))

    ;; Should show the cause relation between the greater temperature
    ;; relation and the heat flow relation. This relation has been inferred
    ;; based on the analogical cause relation in the water flow graph.
    (pp/write (:transferred (first gmaps)) :suppress-namespaces true)

    ;; For other keys like :transferred that are stored in a gmap and might be
    ;; interesting to examine, see the docstring for 'sme-clj.core/match
    )