
OPENCOG_ADD_ATOM_TYPES(atom_types.script
	atom_types.h
	atom_types.definitions
	atom_types.inheritance
	ure_types.scm
	ure_types.pyx
)
ADD_CUSTOM_TARGET(ure_atom_types DEPENDS atom_types.h)

# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR})

ADD_LIBRARY (ure-types SHARED
	atom_types_init.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(ure-types ure_atom_types)

TARGET_LINK_LIBRARIES(ure-types
	${ATOMSPACE_atomtypes_LIBRARY}
)

INSTALL (TARGETS ure-types
	EXPORT URETargets
	LIBRARY DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

ADD_GUILE_EXTENSION(SCM_CONFIG ure-types "opencog-ext-path-ure-types")

INSTALL (FILES
	${CMAKE_CURRENT_BINARY_DIR}/atom_types.h
	${CMAKE_CURRENT_BINARY_DIR}/atom_names.h
	DESTINATION "include/opencog/ure/types"
)

INSTALL (FILES
	${CMAKE_CURRENT_BINARY_DIR}/ure_types.scm
	DESTINATION "${GUILE_SITE_DIR}/opencog/ure/types"
)
