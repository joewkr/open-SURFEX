set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -convert big_endian -r8 -g -assume nosource_include -assume byterecl -fpic -traceback -fp-model source")

set(CMAKE_Fortran_FLAGS_RELEASE "-O2 -fpe0 -ftz")
set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -fp-stack-check -ftrapuv -fpe3 -fp-speculation=strict -check all")