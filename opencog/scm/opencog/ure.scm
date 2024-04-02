;
; OpenCog Unified Rule Engine module
;
(define-module (opencog ure)
 #:use-module (opencog)
 #:use-module (opencog ure-config)
)

(load-extension
   (string-append opencog-ext-path-ure-types "libure-types")
   "ure_types_init")

(load-extension
	(string-append opencog-ext-path-ure "libure")
	"opencog_ure_init")

(include-from-path "opencog/ure/types/ure_types.scm")
(load-from-path "opencog/ure/ure-utils.scm")
(export-ure-utils)
