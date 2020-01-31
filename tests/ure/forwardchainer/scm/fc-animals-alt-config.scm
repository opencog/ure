;;
;; Alternate configuration file for FC animals unit test. Uses custom
;; conditional instantiations instead of the generic one.

(define if-croaks-and-eats-flies-then-frog-rule
  (BindLink
    (Variable "$X")
    (Present
      (Evaluation
        (Predicate "croaks")
        (Variable "$X")
      )
      (Evaluation
        (Predicate "eats_flies")
        (Variable "$X")
      )
    )
    (Inheritance
      (Variable "$X")
      (Concept "frog")
    )
  )
)

(define if-frog-then-green-rule
  (Bind
    (Variable "$X")
    (Inheritance
      (Variable "$X")
      (Concept "frog")
    )
    (Inheritance
      (Variable "$X")
      (Concept "green")
    )
  )
)


(define if-croaks-and-eats-flies-then-frog-rule-name
  (DefinedSchema "if-croaks-and-eats-flies-then-frog-rule"))
(Define if-croaks-and-eats-flies-then-frog-rule-name
  if-croaks-and-eats-flies-then-frog-rule)

(define if-frog-then-green-rule-name
  (DefinedSchema "if-frog-then-green-rule"))
(Define if-frog-then-green-rule-name
  if-frog-then-green-rule)

;; Associate the rules to the rule base (with weights, their semantics
;; is currently undefined, we might settled with probabilities but it's
;; not sure)
(define rbs (Concept "URE"))
(ure-add-rules rbs
               (list
                (cons if-croaks-and-eats-flies-then-frog-rule-name (stv 0.9 1))
                (cons if-frog-then-green-rule-name (stv 0.5 1))))
