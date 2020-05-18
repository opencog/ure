;; Generator for the rule of replacement. We use
;;
;; Evaluation
;;   Predicate "synonymous"
;;   Set X Y
;;
;; to express syntactical equivalence. We don't use the Equivalence
;; link to not bring confusion about its PLN semantics.
(define (gen-replacement-rule TYPE)
  (define (synonymous A B)
    ;; TODO: use Set instead of List
    (Evaluation (Predicate "synonymous") (List A B)))
  (let* ([semi-open (Interval (Number 0) (Number -1))]
	 [LG (Glob "$LG")]
	 [RG (Glob "$RG")]
	 [A (Variable "$A")]
	 [B (Variable "$B")])
    (Bind
      (VariableList
        (TypedVariable LG semi-open)
	(TypedVariable RG semi-open)
	A
	B)
      (Present
        (synonymous A B)
	(TYPE LG A RG))
      (ExecutionOutput
        (GroundedSchema "scm: replacement")
        (List
	  ;; Conclusion
	  (synonymous
            (TYPE LG A RG)
	    (TYPE LG B RG))
	  ;; Premises
	  (synonymous A B)
	  (TYPE LG A RG))))))

;; Dummy formula to please the backward chainer
(define (replacement conclusion . premises)
  conclusion)

;; Instantiate rules
(define evaluation-replacement-rule-name
  (DefinedSchema "evaluation-replacement-rule"))
(Define
  (DefinedSchema "evaluation-replacement-rule")
  (gen-replacement-rule EvaluationLink))
(define similarity-replacement-rule-name
  (DefinedSchema "similarity-replacement-rule"))
(Define
  (DefinedSchema "similarity-replacement-rule")
  (gen-replacement-rule SimilarityLink))
(define list-replacement-rule-name
  (DefinedSchema "list-replacement-rule"))
(Define
  (DefinedSchema "list-replacement-rule")
  (gen-replacement-rule ListLink))
(define set-replacement-rule-name
  (DefinedSchema "set-replacement-rule"))
(Define
  (DefinedSchema "set-replacement-rule")
  (gen-replacement-rule SetLink))

;; Rule-base
(define rrb (Concept "replacement-rb"))
(ure-add-rules rrb (list evaluation-replacement-rule-name
			 similarity-replacement-rule-name
			 list-replacement-rule-name
			 set-replacement-rule-name))
