ENABLE_TESTING()
INCLUDE(AddCxxtest)

IF (WIN32)
	ADD_DEFINITIONS(-D_CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES
	                -D_CRT_SECURE_NO_WARNINGS)
ENDIF (WIN32)

ADD_DEFINITIONS(-DPROJECT_SOURCE_DIR="${CMAKE_SOURCE_DIR}"
                -DPROJECT_BINARY_DIR="${CMAKE_BINARY_DIR}")

# All tests should load the atomspace scm from the build dir, unless the scm
# file is specific to the test (this variable is used by ADD_CXXTEST)
SET(GUILE_LOAD_PATH "${PROJECT_BINARY_DIR}/opencog/scm")

# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR})

# Perform tests in component-dependency order, as much as possible.
# For example, since most things depends on the atomspace,
# its is tested first.
IF (CXXTEST_FOUND)

	ADD_SUBDIRECTORY (unify)
	ADD_SUBDIRECTORY (ure)

	IF (HAVE_CYTHON AND HAVE_NOSETESTS)
		MESSAGE(STATUS "found cython and nosetest, enabling python unit tests")
		ADD_SUBDIRECTORY (cython)
	ENDIF (HAVE_CYTHON AND HAVE_NOSETESTS)

ENDIF (CXXTEST_FOUND)
