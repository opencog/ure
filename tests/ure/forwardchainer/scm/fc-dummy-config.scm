
(define rule
  (BindLink
    (And
      (Member
        (Variable "X")
        (Variable "Y")
      )
      (Member
        (Variable "Y")
        (Variable "Z")
      )
    )
    (Member
      (Variable "X")
      (Variable "Z")
    )
  )
)

(define XY
  (Member
    (Concept "terrier")
    (Concept "dog")
  )
)
(define YZ
  (Member
    (Concept "dog")
    (Concept "animal")
  )
)

(define rule-name
  (DefinedSchema "rule"))
(Define rule-name
  rule)

(define rbs (Concept "URE"))
(ure-add-rules rbs
               (list
                (cons rule-name (stv 1 1))))


