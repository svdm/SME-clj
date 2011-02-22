
(ns sme-clj.typedef
  "Data type definitions and protocols for SME, as well as facilities to create
   instances of types and manipulate them."
  (:use [sme-clj.util :only [defmake is-type?]]))

;;; ENTITY AND PREDICATE

;; Only used to identify Entities defined with defentity, ie. a marker interface
(defprotocol AEntity
  (entity? [_]))

(extend-protocol AEntity
  Object
  (entity? [_] false))

;; Concept graph node that can be shown as prettier string, for display
(defprotocol ADisplayableNode
  (title-for-node [this] "Returns pretty string for this node."))

;; This base Entity is primarily here for testing mapping mechanics. Real
;; concept graphs will have specialised data types with value slots.
(defrecord Entity
  [name]
  AEntity
  (entity? [_] true))

(defprotocol APredicate
  (predicate-type? [this t] "Is this a Predicate of a given type?")
  (function? [this] "Is this a :function Predicate?")
  (attribute? [this] "Is this an :attribute Predicate?"))

(defrecord Predicate
  [name ptype arity ordered?]

  APredicate
  (predicate-type? [this t] (= t ptype))
  (function?  [this] (predicate-type? this :function))
  (attribute? [this] (predicate-type? this :attribute))

  ADisplayableNode
  (title-for-node [this]
    (clojure.core/name name)))

(defn predicate? [x] (is-type? x Predicate))


;;; EXPRESSION

;; Originally expressions were simply seqs, with as first element the
;; functor. However, this meant two relations with the same predicate and same
;; entities were equal as per value equality. Though this problem is not
;; detrimental in every situation, they are sufficiently common to change the
;; representation to a uniquely identifiable one including a unique id.
;;
;; An Expression's id is not guaranteed to be related to any implicit ordering
;; of the expressions within a graph, but in practice often is.


(defprotocol AExpression
  (expression-functor [this] "Returns this expression's functor.")
  (expression-args    [this] "Returns this expression's arguments."))

(defrecord Expression
  [id functor args]

  AExpression
  (expression-functor [_] functor)
  (expression-args    [_] args)

  AEntity
  (entity? [_] false))

(defn expression? [x] (is-type? x Expression))

(defmake Expression [id f a])

(def make-expr make-Expression)

(defn ancestor?
  "Returns true if a given expression is an ancestor of one of the expressions
  in the base set."
  [base-set expr]
  (and (expression? expr)
       (or (contains? base-set expr)
           (some #(ancestor? base-set %) (expression-args expr)))))

(defn get-descendants
  "Returns the seq of descendants of the given expression."
  [expr]
  (tree-seq expression? expression-args expr))


(comment
  (defn predicate-type? [t x] (and (predicate? x) (= t (:ptype x))))
  (defn function? [x] (predicate-type? :function x))
  (defn attribute? [x] (predicate-type? :attribute x)))

;;; CONCEPT GRAPH

(defrecord ConceptGraph
  [name graph spec id-counter])

(defn wrap-graph
  [exprs id-state]
  (ConceptGraph. nil exprs nil id-state))

(defmake ConceptGraph [name graph spec id])

(defn concept-graph? [x] (is-type? x ConceptGraph))

(defn unfold-expression
  "Given an expression, 'unfolds' it into a seq of the expression and all
  subexpressions (ie. predicates).

  This is necessary to fulfil the SM requirement that all predicate clauses
  exist separately in the concept graph's list of clauses. Useful when adding
  clauses to a graph."
  [expr]
  (filter expression? (tree-seq expression? expression-args expr)))

(defmacro make-concept-graph
  "Helper macro for defining a concept graph. The symbol given as 'expsym will
  be let-bound to a function that creates an Expression out of a given functor +
  arguments. It is followed by one or more forms that define expressions using
  that function.

  Ex.:
    (make-concept-graph \"example\" expr
      (expr predicate-foo entity-a entity-b)
      (expr predicate-bar (expr predicate-baz entity-c) entity-d))"
  [name expsym & spec]
  `(let [id-state# (atom -1)
         id# (fn [] (swap! id-state# inc))
         ~expsym  (fn [f# & a#] (make-expr (id#) f# (vec a#)))]
     (make-ConceptGraph ~name
                        (doall (mapcat unfold-expression [~@spec]))
                        (quote ~spec)
                        (deref id-state#))))

;;; MATCH HYPOTHESIS

(defprotocol AMatchHypothesis
  (is-expression? [mh] "Is this MH an expression?")
  (is-emap?       [mh] "Is this MH an emap?"))

(defrecord MatchHypothesis
  [base target]
  
  AMatchHypothesis
  (is-expression? [_] (expression? base))
  (is-emap?       [_] (and (entity? base) (entity? target))))

(defmake MatchHypothesis [base target])

(def make-MH make-MatchHypothesis)

;;; GMAP

(defrecord GMap
  [mhs structure])

(defmake GMap [m s])

(defn matched-goal
  [gmap]
  (:base (:mapping gmap)))

(defn matched-goals
  "Returns the set of distinct goals that are mapped in the given collection of
  gmaps."
  [gmaps]
  (set (map matched-goal gmaps)))

(defn filter-predicates
  "Returns a seq of relations for which the root predicate matches the given
  predicate."
  [predicate coll]
  (filter #(= predicate (expression-functor %)) coll))

(defn some-predicate
  "Returns the first relation for which the root predicate matches the given
  predicate, or nil if none is found."
  [predicate coll]
  (some #{predicate} (map expression-functor coll)))

;;; Def'ing

(defmacro defpredicate
  "Defines a named Predicate."
  [pname & kwargs]
  (let [{:keys [type arity ordered?]
         :or {type :relation, arity 2, ordered? true}} (apply hash-map kwargs)]
    `(def ~pname
          (Predicate. ~(keyword pname)
                      ~type ~(if (not= type :relation) 1 arity) ~ordered?))))


(defmacro defentity
  "Defines a named entity (implements the AEntity protocol)."
  [ename slots & impls]
  `(do
     (defrecord ~ename
       ~slots
       ~@impls)

     (extend ~ename
             AEntity
             {:entity? (fn [_#] true)})
     
     (defmake ~ename ~slots)))
