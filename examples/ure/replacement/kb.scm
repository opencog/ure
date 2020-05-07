;; A believes B
(define (believe A B)
  (Evaluation
    (Predicate "believe")
    (List A B)))

;; Max Tegmark believes the universe is made of math.
(define max-believes-universe-is-math
  (believe
    (Concept "Max Tegmark")
    (Similarity
      (Concept "Universe")
      (Concept "Mathematics"))))

;; Nil believes that Max Tegmark believes the universe is made of
;; math.
(define nil-believes-max-believes-universe-is-math
  (believe
    (Concept "Nil")
    max-believes-universe-is-math))

;; Mathematics is synonymous to consciousness, supposedly.
(define (synonymous A B)
  (Evaluation
    (Predicate "synonymous")
    ;; TODO: use Set instead of List
    (List A B)))
(synonymous (Concept "Mathematics") (Concept "Consciousness"))
