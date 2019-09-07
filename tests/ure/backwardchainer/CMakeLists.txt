LINK_LIBRARIES(
	ure
	atomspace
	clearbox
)

ADD_CXXTEST(BackwardChainerUTest)
ADD_CXXTEST(ControlPolicyUTest)
ADD_CXXTEST(BITUTest)
#Rewrite this Test in Scheme and Update to proper URE Rule Format
#ADD_CXXTEST(GradientUTest)
