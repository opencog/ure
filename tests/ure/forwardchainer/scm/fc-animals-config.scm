;;
;; Configuration file for FC animals unit test.
;;
;; To be loaded first

;; Load the rules (use load for relative path w.r.t. to that file)
(load-from-path "tests/ure/meta-rules/conditional-full-instantiation-meta-rule.scm")
(load-from-path "tests/ure/rules/fuzzy-conjunction-introduction-rule.scm")

(define rbs (ConceptNode "URE"))

;; Associate the rules to the rule base
(ure-add-rules rbs (list conditional-full-instantiation-meta-rule-name
			 fuzzy-conjunction-introduction-2ary-rule-name))

;; termination criteria parameters
(ure-set-maximum-iterations rbs 20)
