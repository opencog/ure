;; Experimenting with rule of replacement
;; https://en.wikipedia.org/wiki/Rule_of_replacement

;; Load opencog
(use-modules (opencog) (opencog ure))

(ure-logger-set-level! "debug")

;; Load knowledge base
(load "kb.scm")

;; Load rule-base
(load "rb.scm")

;; Run backward chainer
(define X (Variable "$X"))
(define target (synonymous nil-believes-max-believes-universe-is-math X))
(define results (cog-bc rrb target))
