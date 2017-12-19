set (fort_flags "-convert big_endian -r8 -g -assume nosource_include -assume byterecl -fpic -traceback -fp-model source")
set (CMAKE_Fortran_FLAGS "-O2 -fpe0 -ftz ${fort_flags}")