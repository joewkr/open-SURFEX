cmake_minimum_required(VERSION 3.10)
set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

project(surfex)
enable_language(Fortran)

option(INSPECT_MASTER_SOURCES "check main source files tree for top-level routines" OFF)

option(ENABLE_MPI "enable MPI" OFF)
option(ENABLE_OMP "enable OpenMP" OFF)
option(ENABLE_XIOS "use XIOS library for parallel IO" OFF)
if(ENABLE_XIOS)
  message(FATAL_ERROR "Not implemented yet")
endif()

option(USE_LOCAL_GRIB_API "use grib_api library provided with SURFEX" ON)
option(USE_LOCAL_NETCDF "use netcdf library provided with SURFEX" ON)
option(USE_LOCAL_XIOS "use XIOS io server library provided with SURFEX" ON)

set(USER_SOURCES_DIR "" CACHE PATH "Path to directory with user-provided source files")

set(CONFIG_FILE "${CMAKE_CURRENT_SOURCE_DIR}/conf/config.${CMAKE_Fortran_COMPILER_ID}.cmake")
if(EXISTS ${CONFIG_FILE} AND NOT IS_DIRECTORY ${CONFIG_FILE})
  message(STATUS "Using ${CONFIG_FILE}")
  include(${CONFIG_FILE})
else()
  message(FATAL_ERROR "Unable to load config ${CONFIG_FILE}")
endif()

include(Dependencies)
include(GenInterface)

function(list_sources dir sources)
  set(endings "*.F90" "*.f90" "*.F" "*.f")

  set(result ${${sources}})
  foreach(item ${endings})
    file(GLOB sources_chunk "${dir}/${item}")
    list(APPEND result ${sources_chunk})
  endforeach(item)
  set(${sources} ${result} PARENT_SCOPE)
endfunction(list_sources)

function(build_sources_master sources programs)
  set(ALL_FORTRAN_SOURCES "")
  list_sources("src/ASSIM"             ALL_FORTRAN_SOURCES)
  list_sources("src/SURFEX"            ALL_FORTRAN_SOURCES)
  list_sources("src/LIB/TOPD"          ALL_FORTRAN_SOURCES)
  list_sources("src/LIB/GELATO"        ALL_FORTRAN_SOURCES)
  list_sources("src/LIB/DRHOOK/BYPASS" ALL_FORTRAN_SOURCES)

  list_sources("src/OFFLIN" ALL_FORTRAN_SOURCES)

  set(MAIN_PROGRAMS
    "oi_main"
    "soda"
    "varassim"
    "ncpost"
    "offline"
    "pgd"
    "prep"
    "sxpost" )

  set(FORTRAN_SOURCES "")
  set(FORTRAN_PROGRAMS "")
  foreach(item ${ALL_FORTRAN_SOURCES})
    get_filename_component(current "${item}" NAME_WE)
    list(FIND MAIN_PROGRAMS "${current}" found)

    if(found EQUAL -1)
      list(APPEND FORTRAN_SOURCES "${item}")
    else()
      list(APPEND FORTRAN_PROGRAMS "${item}")
    endif()
  endforeach(item)

  set(${sources} ${FORTRAN_SOURCES} PARENT_SCOPE)
  set(${programs} ${FORTRAN_PROGRAMS} PARENT_SCOPE)
endfunction(build_sources_master)

function(merge_user_sources master user)
  set(master_sources ${${master}})
  set(user_sources ${${user}})
  set(result "")
  foreach(item ${master_sources})
    get_filename_component(current_master "${item}" NAME)
    list(FIND user_sources "${USER_SOURCES_DIR}/${current_master}" found)

    if(found EQUAL -1)
      list(APPEND result "${item}")
    else()
      list(APPEND result "${USER_SOURCES_DIR}/${current_master}")
      list(REMOVE_ITEM user_sources "${USER_SOURCES_DIR}/${current_master}")
    endif()
  endforeach()
  list(APPEND result "${user_sources}")
  set(${master} "${result}" PARENT_SCOPE)
endfunction()

build_sources_master(FORTRAN_SOURCES FORTRAN_PROGRAMS)

# Check source files for top-level routines in the master tree
if(INSPECT_MASTER_SOURCES)
  find_top_level_routines("${FORTRAN_SOURCES}")
endif()

if(USER_SOURCES_DIR)
  get_filename_component(
    USER_SOURCES_DIR "${USER_SOURCES_DIR}" ABSOLUTE
    BASE_DIR ${CMAKE_CURRENT_BINARY_DIR})
  if(IS_DIRECTORY "${USER_SOURCES_DIR}")
    list_sources(${USER_SOURCES_DIR} USER_SOURCES)
    find_top_level_routines("${USER_SOURCES}" USER_PROGRAMS)
    list(REMOVE_ITEM USER_SOURCES ${USER_PROGRAMS})

    merge_user_sources(FORTRAN_SOURCES USER_SOURCES)
    merge_user_sources(FORTRAN_PROGRAMS USER_PROGRAMS)
  endif()
endif()

add_library( surfex-static STATIC ${FORTRAN_SOURCES})
set_target_properties(surfex-static PROPERTIES OUTPUT_NAME "surfex")

if(${USE_LOCAL_NETCDF})
  add_dependencies(surfex-static ${LOCAL_NETCDF})
endif(${USE_LOCAL_NETCDF})

if(${USE_LOCAL_GRIB_API})
  add_dependencies(surfex-static ${LOCAL_GRIB_API})
endif(${USE_LOCAL_GRIB_API})

add_definitions(-Din_surfex)
add_definitions(-DSFX_BIN)
add_definitions(-DSFX_TXT)
add_definitions(-DSFX_OL)
add_definitions(-DSFX_NC)
add_definitions(-DSFX_ASC)
add_definitions(-DUSE_SODA)

set_target_properties(surfex-static PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

target_include_directories(surfex-static PUBLIC ${CMAKE_BINARY_DIR}/mod)

if(ENABLE_OMP)
  target_link_libraries(surfex-static PUBLIC OpenMP::OpenMP_Fortran)
endif()
if(ENABLE_MPI)
  target_compile_definitions(surfex-static PUBLIC "-DSFX_MPI")
  target_link_libraries(surfex-static PUBLIC MPI::MPI_Fortran)
endif()
target_link_libraries(surfex-static PUBLIC NetCDF::NetCDF_Fortran)
target_link_libraries(surfex-static PUBLIC grib_api::grib_api_Fortran)

foreach(item ${FORTRAN_PROGRAMS})
  get_filename_component(current "${item}" NAME_WE)
  string(TOUPPER "${current}" program_name)

  add_executable(${current} "${item}")
  set_target_properties(${current} PROPERTIES OUTPUT_NAME "${program_name}")
  target_link_libraries(${current} surfex-static)

endforeach(item)