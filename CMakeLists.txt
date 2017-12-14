cmake_minimum_required(VERSION 3.10)
set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

project(surfex)
enable_language(Fortran)

option(ENABLE_MPI "enable MPI" OFF)
option(ENABLE_OMP "enable OpenMP" OFF)

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
set(GENERATED_ROOT ${CMAKE_BINARY_DIR}/generated)

message('${FORTRAN_PROGRAMS}')

set (fort_flags "-convert big_endian -r8 -g -assume nosource_include -assume byterecl -fpic -traceback -fp-model source")
set (CMAKE_Fortran_FLAGS "-O2 -fpe0 -ftz ${fort_flags}")

# Initially create generated files, if they don't yet exist
IF( NOT EXISTS ${GENERATED_ROOT}/gen_ifaces )
  file(MAKE_DIRECTORY ${GENERATED_ROOT})
  MESSAGE( "*** Create list of MODI files to be generated ***" )

  FIND_TOP_LEVEL_ROUTINES(${GENERATED_ROOT}/interfaces.cmake "${FORTRAN_SOURCES}")
  file(WRITE ${GENERATED_ROOT}/gen_ifaces "#generated")
ENDIF()

#[=[
INCLUDE( ${GENERATED_ROOT}/interfaces.cmake )

set (GENERATED_MODULES "")
foreach(source_string ${GENERATED_SRC})
  string(REPLACE " " ";" source_list ${source_string})

  list(GET source_list 0 input)
  list(REMOVE_AT source_list 0)

  set(generated ${source_list})

  add_custom_command(
    OUTPUT  ${generated}
    COMMAND ${CMAKE_COMMAND}
      -DSTANDALONE=TRUE
      -DCMAKE_MODULE_PATH=${CMAKE_MODULE_PATH}
      -DFILE_NAME=${input}
      -DGENERATED_ROOT=${GENERATED_ROOT}
      -P ${CMAKE_CURRENT_SOURCE_DIR}/cmake/GenInterface.cmake
    DEPENDS ${input}
    WORKING_DIRECTORY ${GENERATED_ROOT}
    VERBATIM
  )

  list(APPEND GENERATED_MODULES ${generated})
endforeach(source_string)
#]=]

# Define a target shared object library
ADD_LIBRARY( surfex-static STATIC ${FORTRAN_SOURCES})
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

set_target_properties( surfex-static PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod )

target_include_directories(surfex-static PUBLIC ${CMAKE_BINARY_DIR}/mod )
target_include_directories(surfex-static PUBLIC ${GRIB_API_INCLUDE_DIRS})
target_include_directories(surfex-static PUBLIC ${NETCDF_F90_INCLUDE_DIRS})

if(ENABLE_OMP)
  target_compile_options(surfex-static PUBLIC "${OpenMP_Fortran_FLAGS}")
  target_link_libraries(surfex-static INTERFACE OpenMP::OpenMP_Fortran)
endif()
if(ENABLE_MPI)
  target_compile_definitions(surfex-static PUBLIC "-DSFX_MPI")
  target_compile_options(surfex-static PUBLIC "${MPI_Fortran_COMPILE_FLAGS}")
  target_include_directories(surfex-static PUBLIC ${MPI_Fortran_INCLUDE_DIRS})
  target_link_libraries(surfex-static INTERFACE MPI::MPI_Fortran)
endif()

foreach(item ${FORTRAN_PROGRAMS})
  get_filename_component(current "${item}" NAME_WE)
  string(TOUPPER "${current}" program_name)

  add_executable(${current} "${item}")
  set_target_properties(${current} PROPERTIES OUTPUT_NAME "${program_name}")
  target_link_libraries(${current} surfex-static)

  target_link_libraries(${current} "${NETCDF_F90_LIBRARIES}")
  target_link_libraries(${current} "${GRIB_API_LIBRARIES}")
endforeach(item)