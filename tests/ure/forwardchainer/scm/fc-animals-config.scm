;;
;; Configuration file for FC animals unit test.
;;
;; To be loaded first

;; Load the rules (use load for relative path w.r.t. to that file)
(define cep (current-error-port))

(load-from-path "tests/ure/meta-rules/conditional-full-instantiation-meta-rule.scm")
(format cep "duuude post cony\n")
(load-from-path "tests/ure/rules/fuzzy-conjunction-introduction-rule.scm")
(format cep "duuude post intro\n")

(define rbs (ConceptNode "URE"))

;; Associate the rules to the rule base
(ure-add-rules rbs (list conditional-full-instantiation-meta-rule-name
			 fuzzy-conjunction-introduction-2ary-rule-name))
(format cep "duuude post add rules\n")

;; termination criteria parameters
(ure-set-maximum-iterations rbs 20)
