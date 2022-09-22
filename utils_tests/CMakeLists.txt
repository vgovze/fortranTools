cmake_minimum_required(VERSION 3.16)

project(tests_misc)
enable_language(Fortran)

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    set(options "-ffree-line-length-none\
    -ffree-form -std=f2008 -fimplicit-none -cpp")
    set(bounds "-fbounds-check")
else()
    message(FATAL_ERROR "GNU compiler not found.")
endif()

include_directories(..)

set(CMAKE_Fortran_FLAGS ${CMAKE_Fortran_FLAGS} ${options})
set(CMAKE_Fortran_FLAGS_DEBUG ${CMAKE_Fortran_FLAGS} ${bounds})

file(GLOB_RECURSE sources *.f90 ../*.h)
add_executable(test_macros ${sources})