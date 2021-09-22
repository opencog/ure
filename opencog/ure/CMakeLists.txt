#
# Rule engine lib build
#
ADD_LIBRARY(ure
	backwardchainer/BackwardChainer
	backwardchainer/TraceRecorder
	backwardchainer/ControlPolicy
	backwardchainer/BIT
	backwardchainer/Fitness
	forwardchainer/FCStat
	forwardchainer/ForwardChainer
	forwardchainer/SourceSet
	forwardchainer/SourceRuleSet
	URELogger
	URESCM
	Rule
	UREConfig
	Utils
	MixtureModel
	ActionSelection
	BetaDistribution
	ThompsonSampling
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
