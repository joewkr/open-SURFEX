
include(ExternalProject)

set(shared_lib_dir "lib")
if(CYGWIN)
    set(shared_lib_dir "bin")
endif()

function(generate_install_targets)
    cmake_parse_arguments(PARSED_ARGS "" "" "EXTERNAL_PROJECTS" ${ARGN})
    foreach(item ${PARSED_ARGS_EXTERNAL_PROJECTS})
        ExternalProject_get_property(${item} binary_dir)
        ExternalProject_get_property(${item} source_dir)

        ExternalProject_add_Step(${item} final_install
            COMMAND
                ${CMAKE_COMMAND} -E chdir ${binary_dir}
                ${CMAKE_COMMAND} -DCMAKE_INSTALL_PREFIX=${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/surfex ${source_dir}
            COMMAND ${CMAKE_COMMAND} --build ${binary_dir} --target install
            EXCLUDE_FROM_MAIN YES
            ALWAYS YES
            )
        ExternalProject_add_StepTargets(${item} final_install)

        install(CODE "execute_process(COMMAND ${CMAKE_COMMAND} --build . --target ${item}-final_install)")
    endforeach(item)
endfunction(generate_install_targets)

if(${ENABLE_MPI})
    find_package(MPI REQUIRED COMPONENTS Fortran)
endif(${ENABLE_MPI})

if(${ENABLE_OMP})
    find_package(OpenMP REQUIRED)
endif(${ENABLE_OMP})

if(${BUILD_NETCDF})
    ExternalProject_add(HDF5
        URL ${CMAKE_CURRENT_SOURCE_DIR}/auxiliary/hdf5-1.8.19.tar.bz2
        INSTALL_DIR ${CMAKE_BINARY_DIR}/auxiliary
        CMAKE_ARGS
            -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
            -DHDF5_INSTALL_LIB_DIR:PATH=lib
            -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
            -DHDF5_ENABLE_Z_LIB_SUPPORT=ON
            -DHDF5_BUILD_FORTRAN=OFF
            -DBUILD_TESTING=OFF
            -DHDF5_BUILD_TOOLS=OFF
            -DHDF5_BUILD_EXAMPLES=OFF
            -DHDF5_BUILD_CPP_LIB=OFF
        CMAKE_CACHE_ARGS
            "-DCMAKE_Fortran_COMPILER:FILEPATH=${CMAKE_Fortran_COMPILER}"
        )

    ExternalProject_add(NetCDF_C
        DEPENDS HDF5
        URL ${CMAKE_CURRENT_SOURCE_DIR}/auxiliary/netcdf-4.4.1.1.tar.gz
        INSTALL_DIR ${CMAKE_BINARY_DIR}/auxiliary
        CMAKE_ARGS
            -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
            -DCMAKE_INSTALL_LIBDIR:PATH=lib
            -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
            -DENABLE_NETCDF_4=ON
            -DENABLE_DAP=OFF
            -DBUILD_UTILITIES=OFF
            -DENABLE_EXAMPLES=OFF
            -DENABLE_TESTS=OFF
        CMAKE_CACHE_ARGS
            "-DCMAKE_Fortran_COMPILER:FILEPATH=${CMAKE_Fortran_COMPILER}"
        )

    ExternalProject_add(NetCDF_Fortran
        DEPENDS NetCDF_C
        URL ${CMAKE_CURRENT_SOURCE_DIR}/auxiliary/netcdf-fortran-4.4.4.tar.gz
        INSTALL_DIR ${CMAKE_BINARY_DIR}/auxiliary
        CMAKE_ARGS
            -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
            -DCMAKE_INSTALL_LIBDIR:PATH=lib
            -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
            -DENABLE_TESTS=OFF
        CMAKE_CACHE_ARGS
            "-DCMAKE_Fortran_COMPILER:FILEPATH=${CMAKE_Fortran_COMPILER}"
        BUILD_BYPRODUCTS
            "auxiliary/${shared_lib_dir}/${CMAKE_SHARED_LIBRARY_PREFIX}netcdff${CMAKE_SHARED_LIBRARY_SUFFIX}"
        )

    ExternalProject_get_property(NetCDF_Fortran install_dir)

    set(LOCAL_NETCDF NetCDF_Fortran)

    # Hack to suppress error in INTERFACE_INCLUDE_DIRECTORIES
    file(MAKE_DIRECTORY "${install_dir}/include")

    add_library(NetCDF::NetCDF_Fortran SHARED IMPORTED)
    set_property(TARGET NetCDF::NetCDF_Fortran PROPERTY
        IMPORTED_LOCATION "${install_dir}/${shared_lib_dir}/${CMAKE_SHARED_LIBRARY_PREFIX}netcdff${CMAKE_SHARED_LIBRARY_SUFFIX}")
    if(WIN32 OR CYGWIN OR MINGW)
        set_property(TARGET NetCDF::NetCDF_Fortran PROPERTY
            IMPORTED_IMPLIB "${install_dir}/lib/${CMAKE_IMPORT_LIBRARY_PREFIX}netcdff${CMAKE_IMPORT_LIBRARY_SUFFIX}")
    endif()
    set_property(TARGET NetCDF::NetCDF_Fortran PROPERTY
        INTERFACE_INCLUDE_DIRECTORIES "${install_dir}/include")

    generate_install_targets(EXTERNAL_PROJECTS HDF5 NetCDF_C NetCDF_Fortran)
else(${BUILD_NETCDF})
    find_package(NetCDF REQUIRED COMPONENTS F90)
endif(${BUILD_NETCDF})

if(${BUILD_GRIB_API})
    ExternalProject_add(grib_api
        URL ${CMAKE_CURRENT_SOURCE_DIR}/auxiliary/grib_api-1.23.0-Source.tar.gz
        INSTALL_DIR ${CMAKE_BINARY_DIR}/auxiliary
        CMAKE_ARGS
            -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
            -DCMAKE_INSTALL_LIBDIR:PATH=lib
            -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
            -DENABLE_FORTRAN=ON
            -DENABLE_NETCDF=OFF
            -DENABLE_PYTHON=OFF
            -DENABLE_EXAMPLES=OFF
            -DENABLE_TESTS=OFF
        CMAKE_CACHE_ARGS
            "-DCMAKE_Fortran_COMPILER:FILEPATH=${CMAKE_Fortran_COMPILER}"
        BUILD_BYPRODUCTS
            "auxiliary/${shared_lib_dir}/${CMAKE_SHARED_LIBRARY_PREFIX}grib_api_f90${CMAKE_SHARED_LIBRARY_SUFFIX}"
        )

    ExternalProject_get_property(grib_api install_dir)

    set(LOCAL_GRIB_API grib_api)

    # Hack to suppress error in INTERFACE_INCLUDE_DIRECTORIES
    file(MAKE_DIRECTORY "${install_dir}/include")

    add_library(grib_api::grib_api_Fortran SHARED IMPORTED)
    set_property(TARGET grib_api::grib_api_Fortran PROPERTY
        IMPORTED_LOCATION "${install_dir}/${shared_lib_dir}/${CMAKE_SHARED_LIBRARY_PREFIX}grib_api_f90${CMAKE_SHARED_LIBRARY_SUFFIX}")
    if(WIN32 OR CYGWIN OR MINGW)
        set_property(TARGET grib_api::grib_api_Fortran PROPERTY
            IMPORTED_IMPLIB "${install_dir}/lib/${CMAKE_IMPORT_LIBRARY_PREFIX}grib_api_f90${CMAKE_IMPORT_LIBRARY_SUFFIX}")
    endif()
    set_property(TARGET grib_api::grib_api_Fortran PROPERTY
        INTERFACE_INCLUDE_DIRECTORIES "${install_dir}/include")

    generate_install_targets(EXTERNAL_PROJECTS grib_api)
else(${BUILD_GRIB_API})
    find_package(grib_api REQUIRED)
endif(${BUILD_GRIB_API})

if(${ENABLE_OASIS})
    if(NOT BUILD_NETCDF)
        set(oasis_netcdf_dir "-DNETCDF_DIR=${NETCDF_DIR}")
    endif()
    ExternalProject_add(oasis
        DEPENDS ${LOCAL_NETCDF}
        URL ${CMAKE_CURRENT_SOURCE_DIR}/auxiliary/oasis3-mct
        INSTALL_DIR ${CMAKE_BINARY_DIR}/auxiliary
        CMAKE_ARGS
            -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
            -DCMAKE_INSTALL_LIBDIR:PATH=lib
            -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
            ${oasis_netcdf_dir}
        CMAKE_CACHE_ARGS
            "-DCMAKE_Fortran_COMPILER:FILEPATH=${CMAKE_Fortran_COMPILER}"
            "-DCMAKE_Fortran_FLAGS:STRING=${CMAKE_Fortran_FLAGS}"
            "-DCMAKE_Fortran_FLAGS:STRING_DEBUG=${CMAKE_Fortran_FLAGS_DEBUG}"
            "-DCMAKE_Fortran_FLAGS:STRING_RELEASE=${CMAKE_Fortran_FLAGS_RELEASE}"
        BUILD_BYPRODUCTS
            "auxiliary/lib/libmct.a"
            "auxiliary/lib/libpsmile.a"
            "auxiliary/lib/libscrip.a"
        )

    ExternalProject_get_property(oasis install_dir)

    # Hack to suppress error in INTERFACE_INCLUDE_DIRECTORIES
    file(MAKE_DIRECTORY "${install_dir}/include")

    add_library(oasis::mct STATIC IMPORTED)
    set_property(TARGET oasis::mct PROPERTY IMPORTED_LOCATION "${install_dir}/lib/libmct.a")
    add_library(oasis::scrip STATIC IMPORTED)
    set_property(TARGET oasis::scrip PROPERTY IMPORTED_LOCATION "${install_dir}/lib/libscrip.a")

    add_library(oasis::psmile STATIC IMPORTED)
    set_property(TARGET oasis::psmile PROPERTY IMPORTED_LOCATION "${install_dir}/lib/libpsmile.a")
    set_property(TARGET oasis::psmile APPEND PROPERTY INTERFACE_LINK_LIBRARIES oasis::scrip)
    set_property(TARGET oasis::psmile APPEND PROPERTY INTERFACE_LINK_LIBRARIES oasis::mct)

    add_library(oasis::oasis INTERFACE IMPORTED)
    set_property(TARGET oasis::oasis PROPERTY INTERFACE_INCLUDE_DIRECTORIES "${install_dir}/include")

    set_property(TARGET oasis::oasis APPEND PROPERTY INTERFACE_LINK_LIBRARIES oasis::psmile)
    set_property(TARGET oasis::oasis APPEND PROPERTY INTERFACE_LINK_LIBRARIES MPI::MPI_Fortran)
    set_property(TARGET oasis::oasis APPEND PROPERTY INTERFACE_LINK_LIBRARIES NetCDF::NetCDF_Fortran)
endif()
