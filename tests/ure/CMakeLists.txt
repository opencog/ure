LINK_LIBRARIES(
	ure-types
	ure
	atomspace
	logger
)

INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR})

# Run the tests in logical order, not alphabetical order:
# The URE reader has to work, else the chainers will fail
# ADD_CXXTEST(UREConfigUTest)
ADD_CXXTEST(BetaDistributionUTest)
ADD_CXXTEST(ActionSelectionUTest)
# ADD_CXXTEST(RuleUTest)
ADD_CXXTEST(UtilsUTest)

ADD_SUBDIRECTORY (forwardchainer)

# Backward chainer code has bit-rotted and unit tests no longer work.
# ADD_SUBDIRECTORY (backwardchainer)
