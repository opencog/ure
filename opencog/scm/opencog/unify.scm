;
; OpenCog Unification module
;
(define-module (opencog unify))

(use-modules (opencog ure-config))
(use-modules (opencog logger))

(load-extension (string-append opencog-ext-path-unify "libunifySCM") "opencog_unify_init")
