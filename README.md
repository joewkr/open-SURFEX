# Open-SURFEX
[![Build Status](https://travis-ci.org/joewkr/open-SURFEX.svg?branch=master)](https://travis-ci.org/joewkr/open-SURFEX)
[![Build status](https://ci.appveyor.com/api/projects/status/mu09s8ivcr1v2041/branch/master?svg=true)](https://ci.appveyor.com/project/joewkr/open-surfex/branch/master)

[SURFEX](http://www.cnrm-game-meteo.fr/surfex) (SURFace EXternalisée) is a modelling platform that parametrizes land and water surfaces. SURFEX is being developed by Météo-France and Open-SURFEX is
a freely available SURFEX version released under terms of CECILL-C Licence.

## Differences from the vanilla Open-SURFEX

This fork was created mainly to test the `cmake`-based build system as a replacement for the
original `makefile`-based build. It also contains a number of minor bug fixes.

## How to build Open-SURFEX with cmake-based build

### Required tools

* cmake version 3.11 or newer
* Fortran compiler (configuration files are available for GNU, Intel and Cray Fortran compilers)

### Build instructions

Clone Open-SURFEX repository to your machine and navigate into the cloned source directory.

To build Open-SURFEX with bundled NetCDF and GRIB API libraries and install under the specified directory use the following commands:
```shell
> mkdir build && cd build
> cmake .. -DCMAKE_INSTALL_PREFIX=<path to the installation directory>
> cmake --build .
> cmake --build . --target install
```

To speed-up the compilation process on a multi-core machine, the required flags could be passed in the following form:
```shell
> # In case of cmake 'Unix Makefiles' generator.
> cmake --build . -- -j<number of parallel make processes>
```

To build Open-SURFEX using system-provided NetCDF and GRIB API libraries:
```shell
> mkdir build && cd build
> cmake .. -DBUILD_NETCDF=OFF -DBUILD_GRIB_API=OFF
> cmake --build .
```

If NetCDF and GRIB API libraries are installed in non-standard locations, e.g. when Environment Modules are used, this information should be provided to `cmake` in the following form:
```shell
cmake .. -DBUILD_NETCDF=OFF -DBUILD_GRIB_API=OFF -DNETCDF_DIR=<path to NetCDF> -DGRIB_API_DIR=<path to GRIB API>
```

The following additional options could be provided as arguments for `cmake` during the
configuration stage:

* `-DENABLE_OMP=ON` compile Open-SURFEX with OpenMP multiprocessing;
* `-DENABLE_MPI=ON` compile Open-SURFEX with MPI support, requires installed MPI libraries;
* `-DINSPECT_MASTER_SOURCES=ON` check code for subroutines and functions that are placed outside Fortran modules;
* `-DUSER_SOURCES_DIR=<path to user sources>` add sources from the provided directory to the list of SURFEX' source files. This option reproduces the `VER_USER` functionality of the original build system and could be used to modify some source files without changing them in the original source tree;
* `-DMANGLE_SURFEX_BINARIES=ON` decorate compiled binaries to produce unique file name based on the current build environment. This option could be used to compile a number of different configurations of Open-SURFEX and install all of them under the same directory.

## How to build Open-SURFEX with original build system

Consult the official documentation at http://www.cnrm-game-meteo.fr/surfex