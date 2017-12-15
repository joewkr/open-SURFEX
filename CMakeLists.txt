cmake_minimum_required(VERSION 3.10)
set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

project(surfex)
enable_language(Fortran)

option(ENABLE_MPI "enable MPI" OFF)
option(ENABLE_OMP "enable OpenMP" OFF)
option(SKIP_INSPECTIONS "do not check source files for top-level routines" OFF)
option(ENABLE_XIOS "use XIOS library for parallel IO" OFF)
if(ENABLE_XIOS)
  message(FATAL_ERROR "Not implemented yet")
endif()

option(USE_LOCAL_GRIB_API "use grib_api library provided with SURFEX" ON)
option(USE_LOCAL_NETCDF "use netcdf library provided with SURFEX" ON)
option(USE_LOCAL_XIOS "use XIOS io server library provided with SURFEX" ON)

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

build_sources_master(FORTRAN_SOURCES FORTRAN_PROGRAMS)

message('${FORTRAN_PROGRAMS}')

set (fort_flags "-convert big_endian -r8 -g -assume nosource_include -assume byterecl -fpic -traceback -fp-model source")
set (CMAKE_Fortran_FLAGS "-O2 -fpe0 -ftz ${fort_flags}")

# Check source files for top-level routines
if(NOT SKIP_INSPECTIONS)
  find_top_level_routines(${GENERATED_ROOT}/interfaces.cmake "${FORTRAN_SOURCES}")
endif()

# Define a target shared object library
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