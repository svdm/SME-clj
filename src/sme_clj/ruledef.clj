
(ns mmm.sm.ruledef
  "Structure mapping matching rule definitions."
  (:use mmm.sm.typedef))

;;; Rule definition helpers
(defmacro rule
  "Matching rule specification. Generates {type rule-fn} where rule-fn is a
  function taking [base target] as arguments."
  [name type body]
  `{~type
    (fn ~name
      (~'[base target parent]           ; unhygienic convenience
       ~body)
      ([b# t#]
         (~name b# t# nil))
      ([mh#]
         (~name (:base mh#) (:target mh#) mh#))
      )})

(defmacro defrules
  [name & rules]
  `(def ~name
        (merge-with conj
                    {:intern []
                     :filter []}
                    ~@rules)))

;; As in SME
(defrules literal-similarity
  (rule same-functor :filter
        (when (= (expression-functor base) (expression-functor target))
          (make-MH base target)))

  (rule compatible-args :intern
        (when (and (expression? base) (expression? target)
                   (:ordered? (expression-functor base))
                   (:ordered? (expression-functor target)))
          (map (fn [bchild tchild]
                 (when (or
                        (not (or (expression? bchild) (expression? tchild)))
                        (and (expression? bchild) (expression? tchild)
                             (function? (expression-functor bchild))
                             (function? (expression-functor tchild))))
                   (make-MH bchild tchild)))
               (expression-args base)
               (expression-args target))))

  ;; this rule not tested much yet
  (rule commutative-args :intern
        (when (and (expression? base) (expression? target)
                   (not (:ordered? (expression-functor base)))
                   (not (:ordered? (expression-functor target))))
          (for [bchild (expression-args base)
                tchild (expression-args target)]
            (when (or
                   (not (or (expression? bchild) (expression? tchild)))
                   (and (expression? bchild) (expression? tchild)
                        (function? (expression-functor bchild))
                        (function? (expression-functor tchild))))
              (make-MH bchild tchild))))))


