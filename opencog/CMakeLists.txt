
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR})

DECLARE_GUILE_CONFIG_TARGET(SCM_CONFIG "opencog ure-config" "URE_TEST")

# The build order used here is loosely in terms of 
# dependencies: the later parts depend on, or may
# someday depend on the earlier parts.
#
IF (HAVE_UNIFY)
	ADD_SUBDIRECTORY (unify)
ENDIF (HAVE_UNIFY)
IF (HAVE_URE)
	ADD_SUBDIRECTORY (ure)
ENDIF (HAVE_URE)

# Extension language support
IF (HAVE_GUILE)
	ADD_SUBDIRECTORY (scm)
ENDIF (HAVE_GUILE)

IF (HAVE_CYTHON)
	ADD_SUBDIRECTORY (cython)
ENDIF (HAVE_CYTHON)

WRITE_GUILE_CONFIG(${GUILE_BIN_DIR}/opencog/ure-config.scm SCM_CONFIG TRUE)
WRITE_GUILE_CONFIG(${GUILE_BIN_DIR}/opencog/ure-config-installable.scm SCM_CONFIG FALSE)
INSTALL(FILES ${GUILE_BIN_DIR}/opencog/ure-config-installable.scm DESTINATION ${GUILE_SITE_DIR}/opencog RENAME ure-config.scm)
