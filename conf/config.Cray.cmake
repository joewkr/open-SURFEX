set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -h byteswapio -s real64 -h pic -e m")

set(CMAKE_Fortran_FLAGS_RELEASE "-G2 -O2")
set(CMAKE_Fortran_FLAGS_DEBUG "-G0 -O0 -K trap=fp -R bps -e i")
