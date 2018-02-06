# - Find NetCDF
# Find the native NetCDF includes and library
# Original version of this module was written by Kitware, Inc. for the VTK
# project and available under the OSI-approved BSD 3-clause License.
#
#  NETCDF_INCLUDE_DIR  - user modifiable choice of where netcdf headers are
#  NETCDF_LIBRARY      - user modifiable choice of where netcdf libraries are
#
# Your package can require certain interfaces to be FOUND by setting these
#
#  NETCDF_CXX         - require the C++ interface and link the C++ library
#  NETCDF_F77         - require the F77 interface and link the fortran library
#  NETCDF_F90         - require the F90 interface and link the fortran library
#
# Or equivalently by calling FindNetCDF with a COMPONENTS argument containing one or
# more of "CXX;F77;F90".
#
# When interfaces are requested the user has access to interface specific hints:
#
#  NETCDF_${LANG}_INCLUDE_DIR - where to search for interface header files
#  NETCDF_${LANG}_LIBRARY     - where to search for interface libraries
#
# This module returns these variables for the rest of the project to use.
#
#  NETCDF_FOUND          - True if NetCDF found including required interfaces (see below)
#  NETCDF_LIBRARIES      - All netcdf related libraries.
#  NETCDF_INCLUDE_DIRS   - All directories to include.
#  NETCDF_HAS_INTERFACES - Whether requested interfaces were found or not.
#  NETCDF_${LANG}_INCLUDE_DIRS/NETCDF_${LANG}_LIBRARIES - C/C++/F70/F90 only interface
#
# Additionally, the following IMPORTED targets are defined:
#  NetCDF::NetCDF - target for using NetCDF C library
#  NetCDF::NetCDF_CXX - target for using NetCDF C++ library
#  NetCDF::NetCDF_Fortran - target for using NetCDF F90 library
#  NetCDF::NetCDF_F77 - target for using NetCDF F77 library
#
# Normal usage would be:
#  set (NETCDF_F90 "YES")
#  find_package (NetCDF REQUIRED)
#  target_link_libraries (uses_everthing ${NETCDF_LIBRARIES})
#  target_link_libraries (only_uses_f90 ${NETCDF_F90_LIBRARIES})

#search starting from user editable cache var
if (NETCDF_INCLUDE_DIR AND NETCDF_LIBRARY)
  # Already in cache, be silent
  set (NETCDF_FIND_QUIETLY TRUE)
endif ()

set(USE_DEFAULT_PATHS "NO_DEFAULT_PATH")
if(NETCDF_USE_DEFAULT_PATHS)
  set(USE_DEFAULT_PATHS "")
endif()

find_path (NETCDF_INCLUDE_DIR netcdf.h
  HINTS "${NETCDF_DIR}/include")
mark_as_advanced (NETCDF_INCLUDE_DIR)
set (NETCDF_C_INCLUDE_DIRS ${NETCDF_INCLUDE_DIR})

find_library (NETCDF_LIBRARY NAMES netcdf
  HINTS "${NETCDF_DIR}/lib")
mark_as_advanced (NETCDF_LIBRARY)

set (NETCDF_C_LIBRARIES ${NETCDF_LIBRARY})

#start finding requested language components
set (NetCDF_libs "")
set (NetCDF_includes "${NETCDF_INCLUDE_DIR}")

if(NETCDF_C_LIBRARIES AND NOT TARGET NetCDF::NetCDF)
  add_library(NetCDF::NetCDF INTERFACE IMPORTED)
  set_property(TARGET NetCDF::NetCDF PROPERTY
    INTERFACE_INCLUDE_DIRECTORIES "${NETCDF_C_INCLUDE_DIR}")
  set_property(TARGET NetCDF::NetCDF PROPERTY
    INTERFACE_LINK_LIBRARIES "${NETCDF_C_LIBRARIES}")
endif()

get_filename_component (NetCDF_lib_dirs "${NETCDF_LIBRARY}" PATH)
set (NETCDF_HAS_INTERFACES "YES") # will be set to NO if we're missing any interfaces

macro (NetCDF_check_interface lang header libs)
    #search starting from user modifiable cache var
    find_path (NETCDF_${lang}_INCLUDE_DIR NAMES ${header}
      HINTS "${NETCDF_INCLUDE_DIR}"
      HINTS "${NETCDF_${lang}_ROOT}/include"
      ${USE_DEFAULT_PATHS})

    find_library (NETCDF_${lang}_LIBRARY NAMES ${libs}
      HINTS "${NetCDF_lib_dirs}"
      HINTS "${NETCDF_${lang}_ROOT}/lib"
      ${USE_DEFAULT_PATHS})

    mark_as_advanced (NETCDF_${lang}_INCLUDE_DIR NETCDF_${lang}_LIBRARY)

    #export to internal varS that rest of project can use directly
    set (NETCDF_${lang}_LIBRARIES ${NETCDF_${lang}_LIBRARY})
    set (NETCDF_${lang}_INCLUDE_DIRS ${NETCDF_${lang}_INCLUDE_DIR})

    if (NETCDF_${lang}_INCLUDE_DIR AND NETCDF_${lang}_LIBRARY)
      list (APPEND NetCDF_libs ${NETCDF_${lang}_LIBRARY})
      list (APPEND NetCDF_includes ${NETCDF_${lang}_INCLUDE_DIR})

      if(NOT TARGET NetCDF::NetCDF_${lang})
        add_library(NetCDF::NetCDF_${lang} INTERFACE IMPORTED)
        set_property(TARGET NetCDF::NetCDF_${lang} PROPERTY
          INTERFACE_INCLUDE_DIRECTORIES "${NETCDF_${lang}_INCLUDE_DIR}")

        set_property(TARGET NetCDF::NetCDF_${lang} PROPERTY
          INTERFACE_LINK_LIBRARIES
            ${NETCDF_${lang}_LIBRARY})
        try_compile(_netcdf_test_compiled
          ${CMAKE_BINARY_DIR}
          ${CMAKE_CURRENT_SOURCE_DIR}/cmake/test_netcdf.${lang}
          LINK_LIBRARIES NetCDF::NetCDF_${lang}
          OUTPUT_VARIABLE out)

        if(NOT _netcdf_test_compiled)
          message(STATUS "NetCDF ${lang} requires C NetCDF library for linking")
          set_property(TARGET NetCDF::NetCDF_${lang} APPEND PROPERTY
            INTERFACE_LINK_LIBRARIES
              NetCDF::NetCDF)
        endif()
      endif()
    else ()
      set (NETCDF_HAS_INTERFACES "NO")
      message (STATUS "Failed to find NetCDF interface for ${lang}")
    endif ()
endmacro ()

list (FIND NetCDF_FIND_COMPONENTS "CXX" _nextcomp)
if (_nextcomp GREATER -1)
  NetCDF_check_interface (CXX netcdfcpp.h netcdf_c++)
endif ()
list (FIND NetCDF_FIND_COMPONENTS "F77" _nextcomp)
if (_nextcomp GREATER -1)
  NetCDF_check_interface (F77 netcdf.inc  netcdff)
endif ()
list (FIND NetCDF_FIND_COMPONENTS "F90" _nextcomp)
if (_nextcomp GREATER -1)
  NetCDF_check_interface (F90 netcdf.mod  netcdff)
  if(TARGET NetCDF::NetCDF_F90 AND NOT TARGET NetCDF::NetCDF_Fortran)
    add_library(NetCDF::NetCDF_Fortran INTERFACE IMPORTED)
    set_property(TARGET NetCDF::NetCDF_Fortran PROPERTY
      INTERFACE_LINK_LIBRARIES NetCDF::NetCDF_F90)
  endif()
endif ()

#export accumulated results to internal varS that rest of project can depend on
list (APPEND NetCDF_libs "${NETCDF_C_LIBRARIES}")
set (NETCDF_LIBRARIES ${NetCDF_libs})
set (NETCDF_INCLUDE_DIRS ${NetCDF_includes})

# handle the QUIETLY and REQUIRED arguments and set NETCDF_FOUND to TRUE if
# all listed variables are TRUE
include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (NetCDF
  DEFAULT_MSG NETCDF_LIBRARIES NETCDF_INCLUDE_DIRS NETCDF_HAS_INTERFACES)
