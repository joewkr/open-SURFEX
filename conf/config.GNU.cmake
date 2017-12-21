set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fdefault-real-8 -fdefault-double-8 -g -fno-second-underscore -fpic  -ffpe-trap=overflow,zero,invalid -fbacktrace -fconvert=swap")

set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -fcheck=bounds,do,mem,pointer,recursion -finit-real=nan")