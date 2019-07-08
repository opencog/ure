ADD_GUILE_MODULE (FILES
	opencog/ure.scm
	opencog/rule-engine.scm
	MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog"
)

ADD_GUILE_MODULE (FILES
	opencog/ure/ure-utils.scm
	MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog/ure"
)
