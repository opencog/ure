INCLUDE_DIRECTORIES(
	${Python3_INCLUDE_DIRS}
	${PROJECT_SOURCE_DIR}/opencog/atomspace
	${PROJECT_SOURCE_DIR}/opencog/guile
	${PROJECT_SOURCE_DIR}/opencog/util
)

LINK_DIRECTORIES(
	${PROJECT_BINARY_DIR}/opencog/atomspace
	${PROJECT_BINARY_DIR}/opencog/guile
	${PROJECT_BINARY_DIR}/opencog/util
)

LINK_LIBRARIES(${ATOMSPACE_LIBRARIES} ure-types)

IF (HAVE_NOSETESTS)

	IF (HAVE_URE_THESE_TESTS_FAIL_XXX)

		ADD_TEST(CythonForwardchainer ${NOSETESTS_EXECUTABLE} -vs
			${CMAKE_SOURCE_DIR}/tests/cython/forwardchainer/)
		SET_PROPERTY(TEST CythonForwardchainer
			APPEND PROPERTY ENVIRONMENT
				"GUILE_LOAD_PATH=${GUILE_LOAD_PATH}"
				"PYTHONDONTWRITEBYTECODE=1"
				"PROJECT_SOURCE_DIR=${PROJECT_SOURCE_DIR}"
				"PYTHONPATH=${PROJECT_BINARY_DIR}/opencog/cython/:${PROJECT_SOURCE_DIR}/opencog/python/:${PROJECT_SOURCE_DIR}/tests/cython/forwardchainer"
				)

		ADD_TEST(CythonBackwardchainer ${NOSETESTS_EXECUTABLE} -vs
			${CMAKE_SOURCE_DIR}/tests/cython/backwardchainer/)
		SET_PROPERTY(TEST CythonBackwardchainer
			PROPERTY ENVIRONMENT
				"GUILE_LOAD_PATH=${GUILE_LOAD_PATH}"
				"PYTHONDONTWRITEBYTECODE=1"
				"PROJECT_SOURCE_DIR=${PROJECT_SOURCE_DIR}"
				"PYTHONPATH=${PROJECT_BINARY_DIR}/opencog/cython/:${PROJECT_SOURCE_DIR}/opencog/python/:${PROJECT_SOURCE_DIR}/tests/cython/backwardchainer"
				)
	ENDIF (HAVE_URE_THESE_TESTS_FAIL_XXX)

ENDIF (HAVE_NOSETESTS)
