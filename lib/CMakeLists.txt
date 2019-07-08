# build_type is used in UREConfig.cmake.in
string(TOUPPER "${CMAKE_BUILD_TYPE}" build_type )

export(EXPORT URETargets
	FILE "${CMAKE_CURRENT_BINARY_DIR}/URE/URETargets.cmake"
)

set(ConfigPackageLocation lib/cmake/URE)

install(EXPORT URETargets
	FILE URETargets.cmake
	DESTINATION ${ConfigPackageLocation}
)

include(CMakePackageConfigHelpers)

configure_package_config_file(UREConfig.cmake.in
	${CMAKE_CURRENT_BINARY_DIR}/UREConfig.cmake
	INSTALL_DESTINATION lib/URE/cmake
	PATH_VARS CMAKE_INSTALL_PREFIX
)

write_basic_package_version_file(
	"${CMAKE_CURRENT_BINARY_DIR}/UREConfigVersion.cmake"
	VERSION ${SEMANTIC_VERSION}
	COMPATIBILITY SameMajorVersion
)

INSTALL(FILES
	${CMAKE_CURRENT_BINARY_DIR}/UREConfigVersion.cmake
	${CMAKE_CURRENT_BINARY_DIR}/UREConfig.cmake
	DESTINATION ${ConfigPackageLocation}
)
