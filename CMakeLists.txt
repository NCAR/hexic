cmake_minimum_required(VERSION 2.8)
cmake_policy(SET CMP0017 NEW)
if (POLICY CMP0042)
  cmake_policy(SET CMP0042 NEW)
endif ()

project(hexic Fortran C)

set(VERSION_MAJOR "0")
set(VERSION_MINOR "1")
set(VERSION_PATCH "0")
set(VERSION "${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH}")

set(CMAKE_MODULE_PATH
    ${CMAKE_SOURCE_DIR}/cmake/Modules
    ${CMAKE_ROOT}/Modules
    ${CMAKE_MODULE_PATH}
)

include(${CMAKE_SOURCE_DIR}/cmake/GetDate.cmake)
LONG_TODAY(hexic_BUILD_DATE)
message(STATUS "Build date = ${hexic_BUILD_DATE}")

option(USE_OPENMP "build application with OpenMP" OFF)

include_directories(${CMAKE_SOURCE_DIR}/include)
include_directories(${CMAKE_BINARY_DIR}/include)

set(CMAKE_SKIP_BUILD_RPATH FALSE)
set(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)
set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
list(FIND CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES "${CMAKE_INSTALL_PREFIX}/lib" isSystemDir)
if ("${isSystemDir}" STREQUAL "-1")
  set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
endif ()

add_subdirectory(include)
add_subdirectory(src)

add_subdirectory(IDL)
# TODO: turn off Python bindings while we focus on IDL bindings
#add_subdirectory(Python)
