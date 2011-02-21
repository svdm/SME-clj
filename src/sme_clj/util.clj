
(ns sme-clj.util
  "Utility/helper functions")

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


(defmacro is-type? [x t] `(isa? (type ~x) ~t))
