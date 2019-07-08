#
# Need to use -fno-strict-aliasing when compiling cython code, in order
# to avoid nasty compiler warnings about aliasing.  Cython explicitly
# performs aliasing, in order to emulate python object inheritance.
# See, for example,
# https://groups.google.com/forum/#!topic/cython-users/JV1-KvIUeIg
#
SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-strict-aliasing")

MESSAGE(STATUS "Setting python RPATH to ${PYTHON_DEST}")
SET(CMAKE_SHARED_LINKER_FLAGS
	"${CMAKE_SHARED_LINKER_FLAGS} -Wl,-rpath -Wl,${PYTHON_DEST}")
SET(CMAKE_SHARED_LIBRARY_SUFFIX .so)

INCLUDE_DIRECTORIES(
	${ATOMSPACE_INCLUDE_DIR}
	${PYTHON_INCLUDE_DIRS}
	${CMAKE_CURRENT_SOURCE_DIR}
	${CMAKE_CURRENT_BINARY_DIR}
)

SET(CYTHON_FLAGS "-2" "-f" "-I" "${ATOMSPACE_INCLUDE_DIR}/opencog/cython")

# Use this as a guide:
# https://github.com/OpenKinect/libfreenect/blob/master/wrappers/python/CMakeLists.txt

##################### URE #####################

CYTHON_ADD_MODULE_PYX(ure
	"ure.pyx" "forwardchainer.pyx" "backwardchainer.pyx"
	"../../ure/forwardchainer/ForwardChainer.h"
	"../../ure/backwardchainer/BackwardChainer.h"
	ure
)

# opencog.ure Python bindings
ADD_LIBRARY(ure_cython
	ure.cpp
)

TARGET_LINK_LIBRARIES(ure_cython
	ure
	${ATOMSPACE_LIBRARIES}
	${PYTHON_LIBRARIES}
)

SET_TARGET_PROPERTIES(ure_cython PROPERTIES
	PREFIX ""
	OUTPUT_NAME ure)

INSTALL (TARGETS
	ure_cython
	DESTINATION "${PYTHON_DEST}")

##################################################
# Tell python where to find modules.

INSTALL (FILES
	__init__.py
	DESTINATION "${PYTHON_DEST}")
