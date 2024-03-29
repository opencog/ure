; =============================================================================
; Crisp Modus Ponens Rule.
;
; A->B
; A
; |-
; B
;
; See examples/ure/README.md for more details.
; -----------------------------------------------------------------------------

(define crisp-modus-ponens-rule
    (BindLink
        (VariableSet
            (TypedVariable
                (VariableNode "$A")
                (TypeNode "PredicateNode"))
            (TypedVariable
                (VariableNode "$B")
                (TypeNode "PredicateNode")))
        (PresentLink
            (ImplicationLink
                (VariableNode "$A")
                (VariableNode "$B"))
            (VariableNode "$A"))
        (ExecutionOutputLink
            (GroundedSchemaNode "scm: crisp-modus-ponens")
            (ListLink
                (VariableNode "$B")
                (ImplicationLink
                    (VariableNode "$A")
                    (VariableNode "$B"))
                (VariableNode "$A")
                ))))

; -----------------------------------------------------------------------------
; Crisp Modus Ponens Formula
;
; If both confidence and strength of A->B and A are above 0.5 then set
; the TV of B to (stv 1 1)
; -----------------------------------------------------------------------------

(define (crisp-modus-ponens B AB A)
    (let
        ((sA (cog-mean A))
         (cA (cog-confidence A))
         (sAB (cog-mean AB))
         (cAB (cog-confidence AB)))
      (if (and (>= sA 0.5) (>= cA 0.5) (>= sAB 0.5) (>= cAB 0.5))
          (cog-set-tv! B (stv 1 1)))))

; Associate a name to the rule
(define crisp-modus-ponens-rule-name
  (DefinedSchemaNode "crisp-modus-ponens-rule"))
(DefineLink
  crisp-modus-ponens-rule-name
  crisp-modus-ponens-rule)
