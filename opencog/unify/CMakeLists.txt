ADD_LIBRARY (unify
	Unify
)

TARGET_LINK_LIBRARIES(unify
	${ATOMSPACE_LIBRARIES}
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS unify
    EXPORT AtomSpaceTargets 
    DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")

INSTALL (FILES
	Unify.h
	DESTINATION "include/opencog/unify"
)
