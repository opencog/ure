#
# Rule engine lib build
#
ADD_LIBRARY(ure
	backwardchainer/BackwardChainer.cc
	backwardchainer/TraceRecorder.cc
	backwardchainer/ControlPolicy.cc
	backwardchainer/BIT.cc
	backwardchainer/Fitness.cc
	forwardchainer/FCStat.cc
	forwardchainer/ForwardChainer.cc
	forwardchainer/SourceSet.cc
	forwardchainer/SourceRuleSet.cc
	URELogger.cc
	URESCM.cc
	Rule.cc
	UREConfig.cc
	Utils.cc
	MixtureModel.cc
	ActionSelection.cc
	BetaDistribution.cc
	ThompsonSampling.cc
)

TARGET_LINK_LIBRARIES(ure
	unify
)
IF (HAVE_GUILE)
	TARGET_LINK_LIBRARIES(ure ${GUILE_LIBRARIES})
	ADD_GUILE_EXTENSION(SCM_CONFIG ure "opencog-ext-path-ure")
ENDIF (HAVE_GUILE)

INSTALL (TARGETS ure 
    EXPORT URETargets
    DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")

INSTALL (FILES
	UREConfig.h
	URELogger.h
	Rule.h
	UREConfig.h
	Utils.h
	MixtureModel.h
	ActionSelection.h
	BetaDistribution.h
	ThompsonSampling.h
	DESTINATION "include/opencog/ure"
)

ADD_SUBDIRECTORY(forwardchainer)
ADD_SUBDIRECTORY(backwardchainer)
