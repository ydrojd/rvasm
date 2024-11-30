cmake_minimum_required(VERSION 3.20)
project(rvasm)

option(USE_ASAN "Use address sanitizer if available" OFF)
option(USE_UBSAN "Use undefined behavior sanitizer if available" OFF)
option(USE_CLANGTIDY "Use clang-tidy if available" OFF)
option(USE_CPPCHECK "Use Cppcheck if available" OFF)

set(EXTRA_WARNINGS false)
set(CMAKE_BUILD_TYPE "Release")
set(GNU_COMPATIBLE true)

set(CMAKE_EXPORT_COMPILE_COMMANDS true)

add_executable(rvasm)

find_package(FLEX REQUIRED)

flex_target(lexer ./src/F.l ./F.cpp DEFINES_FILE ./F.h)

include_directories(SYSTEM ./elfio/)

include_directories(
  ./incl/
  ./
)

target_sources(rvasm PRIVATE
  ./rvasm.cpp
  ./F.cpp ./F.h
  ./incl/lexer.hpp
  ./src/parser.cpp ./incl/parser.hpp
  ./src/preprocessor.cpp ./incl/preprocessor.hpp
  ./src/assembler.cpp ./incl/assembler.hpp
  ./src/rvlib.cpp ./incl/rvlib.hpp
  ./src/elf_file.cpp ./incl/elf_file.hpp
  ./src/error_logger.cpp ./incl/error_logger.hpp
  ./incl/program_options.hpp
)

set(target "rvasm")

set_property(TARGET ${target} PROPERTY CXX_STANDARD 20)

if(DEFINED CMAKE_BUILD_TYPE)
  if(GNU_COMPATIBLE
      AND NOT ${CMAKE_BUILD_TYPE} MATCHES "Debug"
      AND NOT ${CMAKE_BUILD_TYPE} MATCHES "Release")
    message(FATAL_ERROR "Build type for GCC-like compiler is not set to Debug or Release")
  endif()
else()
  set(CMAKE_BUILD_TYPE "Debug")
  message("Build type set to Debug")
endif()

if(GNU_COMPATIBLE)
  if(EXTRA_WARNINGS)
    target_compile_options(rvasm PRIVATE -Wall -pedantic -Wextra -Wzero-as-null-pointer-constant)
  endif()
  if(${CMAKE_BUILD_TYPE} MATCHES "Debug")
    target_compile_options(rvasm PRIVATE -Og -g)
  elseif(${CMAKE_BUILD_TYPE} MATCHES "Release")
    target_compile_options(rvasm PRIVATE -O3)
  endif()
endif()

if (USE_ASAN)
    execute_process(COMMAND "${CMAKE_CXX_COMPILER}" -print-file-name=libasan.so
            OUTPUT_VARIABLE LIBASAN_PATH
            RESULT_VARIABLE ASAN_RESULT
            OUTPUT_STRIP_TRAILING_WHITESPACE)

    set(HAS_ASAN FALSE)

    if (USE_ASAN AND (${ASAN_RESULT} EQUAL 0) AND (NOT ${LIBASAN_PATH} STREQUAL ""))
        message("libasan found @${LIBASAN_PATH}")
        message("To disable the address sanitizer set USE_ASAN to OFF.\n")
        set(HAS_ASAN TRUE)
    endif ()

    if (HAS_ASAN AND USE_ASAN)
        target_compile_options(${target} PRIVATE -fsanitize=address -fsanitize-recover=address)
        target_link_options(${target} PRIVATE -fsanitize=address)
    endif ()
endif()

if (USE_UBSAN)
    execute_process(COMMAND "${CMAKE_CXX_COMPILER}" -print-file-name=libubsan.so
            OUTPUT_VARIABLE LIBUBSAN_PATH
            RESULT_VARIABLE UBSAN_RESULT
            OUTPUT_STRIP_TRAILING_WHITESPACE)

    set(HAS_UBSAN FALSE)

    if (USE_UBSAN AND (${UBSAN_RESULT} EQUAL 0) AND (NOT ${LIBUBSAN_PATH} STREQUAL ""))
        message("libubsan found @${LIBUBSAN_PATH}")
        message("To disable the undefined behavior sanitizer set USE_UBSAN to OFF.\n")
        set(HAS_UBSAN TRUE)
    endif ()

    if (HAS_UBSAN AND USE_UBSAN)
        target_compile_options(${target} PRIVATE -fsanitize=undefined -fsanitize-recover=undefined)
        target_link_options(${target} PRIVATE -fsanitize=undefined)
    endif ()
endif()

if (USE_CPPCHECK)
    find_program(CPPCHECK NAMES cppcheck HINTS $ENV{PROGRAMFILES}/cppcheck)
    if(CPPCHECK AND NOT (CPPCHECK MATCHES "NOTFOUND"))
        message(STATUS "which cppcheck: ${CPPCHECK}")

        set(CPPCHECK_CMD "${CPPCHECK}"
                "--enable=all"
                "--inconclusive"
                "--inline-suppr"
		"--suppress=missingIncludeSystem"
		"--suppress=missingInclude"
                "--suppress=unmatchedSuppression"
                "--template='{file}:{line}: warning: {id} ({severity}): {message}'")

        set_target_properties(${target} PROPERTIES
                CXX_CPPCHECK "${CPPCHECK_CMD}"
                )
    endif()
endif(USE_CPPCHECK)

if (USE_CLANGTIDY)
    find_program(CLANGTIDY NAMES clang-tidy clang-tidy-14 clang-tidy-13 clang-tidy-12 clang-tidy-11)

    if(CLANGTIDY AND NOT (CLANGTIDY MATCHES "NOTFOUND"))
        message(STATUS "which clang-tidy: ${CLANGTIDY}")
        set_target_properties(${target} PROPERTIES
                CXX_CLANG_TIDY "${CLANGTIDY};-checks=-*,,clang-analyzer-*,cppcoreguidelines-*,portability-*"
                )
    endif()
endif(USE_CLANGTIDY)
