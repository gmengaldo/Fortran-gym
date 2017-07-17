# Minimum CMake version required
CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

# Project name
PROJECT(shape)

# Enable Fortran language 
ENABLE_LANGUAGE(Fortran)

# Enable testing framework
INCLUDE(CTest)

# Add support for CMAKE_DEPENDENT_OPTION
INCLUDE(CMakeDependentOption)



# Make sure default compilation is RELEASE
# ------------------------------------------------------------------------------
IF (NOT CMAKE_BUILD_TYPE)
  SET(CMAKE_BUILD_TYPE RELEASE CACHE STRING
      "Choose the type of build, options are: None Debug Release." FORCE)
ENDIF(NOT CMAKE_BUILD_TYPE)
# ------------------------------------------------------------------------------



# Define path where to search for customised "Find.cmake" files
# ------------------------------------------------------------------------------
SET(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
# ------------------------------------------------------------------------------


# Set default install path if not provided on the command-line
# ------------------------------------------------------------------------------
IF (CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
    SET(CMAKE_INSTALL_PREFIX ${CMAKE_BINARY_DIR}/dist CACHE PATH "" FORCE)
ENDIF()
# ------------------------------------------------------------------------------



# Set various ThirdParty locations
# ------------------------------------------------------------------------------
SET(TPURL   http://www.iblgf.caltech.edu/thirdparty)
SET(TPSRC   ${CMAKE_SOURCE_DIR}/ThirdParty)
SET(TPBUILD ${CMAKE_BINARY_DIR}/ThirdParty)
SET(TPDIST  ${CMAKE_BINARY_DIR}/ThirdParty/dist)
SET(TPINC   ${CMAKE_INSTALL_PREFIX}/include/ThirdParty)
# ------------------------------------------------------------------------------



# Path to Fortran module files
# ------------------------------------------------------------------------------
SET(CMAKE_Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/modules"
    CACHE STRING "Path to Fortran modules")
# ------------------------------------------------------------------------------



# PRIORITIZE INTEL COMPILER OVER GNU
# ------------------------------------------------------------------------------
SET(INTEL_SEARCH_PATHS $ENV{LD_LIBRARY_PATH} $ENV{PATH})

FIND_PROGRAM(C_COMPILER NAMES icc PATHS ${INTEL_SEARCH_PATHS})
FIND_PROGRAM(CXX_COMPILER NAMES icpc PATHS ${INTEL_SEARCH_PATHS})
FIND_PROGRAM(Fortran_COMPILER NAMES ifort PATHS ${INTEL_SEARCH_PATHS})
FIND_PROGRAM(AR NAMES xiar PATHS ${INTEL_SEARCH_PATHS})
FIND_PROGRAM(LINKER NAMES xild PATHS ${INTEL_SEARCH_PATHS})
GET_FILENAME_COMPONENT(Fortran_COMPILER_PATH ${Fortran_COMPILER} PATH)

IF (Fortran_COMPILER)
    SET(FC ifort)
    SET(CMAKE_Fortran_COMPILER "${Fortran_COMPILER}" CACHE STRING "" FORCE)
    SET(CMAKE_C_COMPILER "${C_COMPILER}" CACHE STRING "" FORCE)
    SET(CMAKE_CXX_COMPILER "${CXX_COMPILER}" CACHE STRING "" FORCE)
    SET(CMAKE_AR "${AR}" CACHE STRING "" FORCE)
    SET(CMAKE_LINKER "${LINKER}" CACHE STRING "" FORCE)
ELSE()
ENDIF()
# ------------------------------------------------------------------------------



# Flags for different compilers
# ------------------------------------------------------------------------------
IF (CMAKE_Fortran_COMPILER_ID MATCHES Intel OR Fortran_COMPILER)
    MESSAGE(STATUS "INTEL COMPILER")
    SET(CMAKE_Fortran_FLAGS         
        "-warn -fpp" 
        CACHE STRING "" FORCE)
    SET(CMAKE_Fortran_FLAGS_DEBUG   
        "-g -fpp -debug extended -warn all -traceback -check all" 
       CACHE STRING "" FORCE)
    SET(CMAKE_Fortran_FLAGS_RELEASE 
        "-O3 -fpp" CACHE STRING "" FORCE)
    #SET(CMAKE_EXE_LINKER_FLAGS  
    #    "-L$ENV{LD_LIBRARY_PATH}" 
    #    CACHE STRING "" FORCE)
    SET(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS 
        "-dynamic-intel" 
        CACHE STRING "" FORCE)
    SET(CMAKE_MODULE_LINKER_FLAGS 
        "-I${PROJECT_BINARY_DIR}/modules"
          "${PROJECT_BINARY_DIR}"
          "${PROJECT_BINARY_DIR}/ThirdParty/dist/lib"
        CACHE STRING "" FORCE)
ELSEIF (CMAKE_Fortran_COMPILER_ID MATCHES GNU)
    MESSAGE(STATUS "GNU COMPILER")
    SET(CMAKE_Fortran_FLAGS         
        "-cpp -Wunused-dummy-argument -Warray-bounds -Waliasing -Wampersand -Wcoverage-mismatch -Wline-truncation -Wno-align-commons -Wno-tabs -Wreal-q-constant -Wunderflow -Wunused-parameter -Wrealloc-lhs -Wrealloc-lhs-all"
        CACHE STRING "" FORCE)
    SET(CMAKE_Fortran_FLAGS_DEBUG   
        "-O0 -g3 -cpp -ffpe-trap=invalid,zero,overflow"
        CACHE STRING "" FORCE)
    SET(CMAKE_Fortran_FLAGS_RELEASE 
        "-Ofast -march=native"
        CACHE STRING "" FORCE)
    SET(CMAKE_MODULE_LINKER_FLAGS 
        "-I${PROJECT_BINARY_DIR}/modules"
          "${PROJECT_BINARY_DIR}"
          "${PROJECT_BINARY_DIR}/ThirdParty/dist/lib"
        CACHE STRING "" FORCE)
ENDIF()
# ------------------------------------------------------------------------------



# Add executable and modules
# ------------------------------------------------------------------------------
add_executable(shape 	    shape.f90)
add_library   (shape_module modules/shape_module.f90)

target_link_libraries(shape shape_module)

INSTALL(TARGETS shape DESTINATION ${CMAKE_INSTALL_PREFIX}/bin)
# ------------------------------------------------------------------------------



# Test
# ----------------------------------------------------------------------------
# area of a rectangle of base 20 and width 10
# ----------------------------------------------------------------------------
ADD_TEST(NAME area_1 
         COMMAND mpiexec -np 1 ${CMAKE_BINARY_DIR}/dist/bin/shape
	 WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}")

SET_TESTS_PROPERTIES(area_1
                     PROPERTIES 
                         PASS_REGULAR_EXPRESSION " area =    200.000000"
			 PROCESSORS "1")
# ------------------------------------------------------------------------------

