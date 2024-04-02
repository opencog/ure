LINK_LIBRARIES(
	ure
	atomspace
	clearbox
)

# ForwardChainerUTest runs fine and passes for me, but
# it segfaults after 15 seconds when it runs in circleci.
# ADD_CXXTEST(ForwardChainerUTest)
