cmake_minimum_required(VERSION 3.11)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(BUILD_SHARED_LIBS ON)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

project(
  ts-lang-module
  VERSION 1.0
  LANGUAGES C CXX)

include(CheckIncludeFiles)
check_include_files("tree_sitter/api.h" HAVE_TREESITTER)

if(NOT HAVE_TREESITTER)
  message(FATAL_ERROR "Missing required header: tree_sitter/api.h")
endif()

add_compile_options(-Wall -ggdb3)

add_library(ts-lang-module SHARED src/ts-lang-module.cc)
set_target_properties(
  ts-lang-module
  PROPERTIES VERSION ${PROJECT_VERSION}
             PREFIX ""
             POSITION_INDEPENDENT_CODE ON
             C_VISIBILITY_PRESET "hidden"
             LIBRARY_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR})

if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "No build type selected, defaulting to RelWithDebInfo")
  set(CMAKE_BUILD_TYPE
      "RelWithDebInfo"
      CACHE STRING "Build type (default RelWithDebInfo)" FORCE)
endif()

add_custom_target(
  emacs
  COMMAND
    emacs -Q --module-assertions -L ${CMAKE_SOURCE_DIR} -L ${CMAKE_BINARY_DIR}
    --eval "\\(require \\'ts-lang-module\\)" --eval "\\(ielm\\)"
  DEPENDS ts-lang-module)
