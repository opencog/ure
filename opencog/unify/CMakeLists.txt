ADD_LIBRARY (unify
	Unify
	UniVars
)

TARGET_LINK_LIBRARIES(unify
	${ATOMSPACE_LIBRARIES}
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS unify
    EXPORT URETargets
    DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")

INSTALL (FILES
	Unify.h
	UniVars.h
	DESTINATION "include/opencog/unify"
)
