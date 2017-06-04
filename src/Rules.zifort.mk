###
#  arch file for SLES 10-SP3 with ifort v12.1.6 mpich 3.1.3 & openmp
###
##########################################################
#                                                        #
# Compiler Options                                       #
#                                                        #
##########################################################
#
#RJ: few extra features
#DO_ASM = YES
DO_ASM = NO
USE_SPLR = YES
#USE_SPLR = NO
#USE_SPLR_WRAP = YES
USE_SPLR_WRAP = NO
#AVOID_CPP = NO
AVOID_CPP = YES
#
#OBJDIR_PATH=/home/escj/azertyuiopqsdfghjklm/wxcvbn/azertyuiopqsdfghjklmwxcvbn
#
#RJ: if using any interrupt signal watchers, dr_hook, mpi, valgrind or ifort itself do not forget to add:
#RJ -fpe0 -fp-model precise -assume ieee_fpe_flags ; specially ieee_fpe_flags one on mixed MPI/OMP
#
#OPT_BASE  = -openmp -openmp-threadprivate=compat -r8 -g -u -assume nosource_include -assume byterecl -fpic -traceback -fp-model precise -assume ieee_fpe_flags -convert big_endian
OPT_BASE  = -r8 -g -u -assume nosource_include -assume byterecl -traceback -fp-model precise -assume ieee_fpe_flags -convert big_endian
OPT_BASE += -fsource-asm
#
OPT_PERF0 = -O0 -fpe0 -ftz
OPT_PERF2 = -O2 -fpe0 -ftz
OPT_CHECK = -fp-stack-check -ftrapuv -fpe3 -fp-speculation=strict -check all
OPT_I8    = -i8
#
# Integer 4/8 option
#
MNH_INT   ?=I4
#RJ LFI_RECL  ?=512
#
ifeq "$(MNH_INT)" "I8"
OPT_BASE         += $(OPT_I8)
#RJ LFI_INT           ?=8
MNH_MPI_RANK_KIND ?=8
else
MNH_MPI_RANK_KIND ?=4
#RJ LFI_INT           ?=4
endif
#
OPT       = $(OPT_BASE) $(OPT_PERF2)
OPT0      = $(OPT_BASE) $(OPT_PERF0)
OPT_NOCB  = $(OPT_BASE) $(OPT_PERF2)
#
ifeq "$(OPTLEVEL)" "DEBUG"
OPT       = $(OPT_BASE) $(OPT_PERF0) $(OPT_CHECK)
OPT0      = $(OPT_BASE) $(OPT_PERF0) $(OPT_CHECK)
OPT_NOCB  = $(OPT_BASE) $(OPT_PERF0)
endif
#
ifeq "$(VER_MPI)" "NOMPI"
F90= ifort
F77= ifort
else
F90= mpif90
F77= mpif77
endif
#
REALFC=ifort
#
#RJ: on ifort 12.1.6 with -openmp-threadprivate=compat compiler crashes on spll_modd_teb_irrig_n.f90
#    catastrophic error: **Internal compiler error: internal abort**, nice
FCFLAGS_OMP= -openmp -openmp-threadprivate=compat
#RJ: temporary solution for OMP segfaults on TSNOW subcomponents in unpack_isba_patchn.F90
FCFLAGS_OMP += -check pointers
CFLAGS_OMP=
ifeq "$(VER_OMP)" "NOOMP"
FCFLAGS_OMP=
CFLAGS_OMP=
endif
#
F90FLAGS  = $(OPT) $(FCFLAGS_OMP) -free
F77FLAGS  = $(OPT) $(FCFLAGS_OMP) -nofree
FX90      = $(F77)
FX90FLAGS = $(OPT) $(FCFLAGS_OMP) -nofree
#
CC        = gcc
CFLAGS    = $(CFLAGS_OMP)
#
LDFLAGS   = $(FCFLAGS_OMP) -Wl,-warn-once -traceback
#
# NetCDF external
#
ifeq "$(VER_CDF)" "CDFEXT"
NETCDF = $(HOME)/hm_libs/ifort/netcdf_432
NETCDF_INC = $(NETCDF)/include
NETCDF_LIB = $(NETCDF)/lib
INC_NETCDF = -I$(NETCDF_INC)
LIB_NETCDF = -L$(NETCDF_LIB) -lnetcdff -lnetcdf
endif
#
# GRIBAPI external
#
ifeq "$(VER_GRIBAPI)" "GRIBAPI_EXT"
GRIB_API = $(HOME)/hm_libs/ifort/grib_api_112
GRIB_API_INC = $(GRIB_API)/include
GRIB_API_LIB = $(GRIB_API)/lib
INC_GRIBAPI = -I$(GRIB_API_INC)
LIB_GRIBAPI = $(GRIB_API_LIB)/libgrib_api_f90.a $(GRIB_API_LIB)/libgrib_api.a
endif
#
# preprocessing flags
#
CPP = cpp -P -traditional -Wcomment
#
#RJ: bug in ifort when creating modis in XRD40
VER_XRD=XRD39
#
FPPFLAGS_SURFEX    =
#RJ FPPFLAGS_SURCOUCHE = -DMNH_MPI_DOUBLE_PRECISION -DMNH_LINUX -DMNH_MPI_BSEND -DDEV_NULL -DMNH_MPI_RANK_KIND=$(MNH_MPI_RANK_KIND)
#RJ FPPFLAGS_RAD       =
#RJ FPPFLAGS_NEWLFI    = -DSWAPIO -DLINUX -DLFI_INT=${LFI_INT} -DLFI_RECL=${LFI_RECL}
#RJ FPPFLAGS_MNH       = -DMNH
#
# Gribex flags
#
TARGET_GRIBEX=linux
CNAME_GRIBEX=_ifort
##########################################################
#                                                        #
# Source of MESONH PACKAGE  Distribution                 #
#                                                        #
##########################################################
#
include Makefile.SURFEX.mk
#
##########################################################
#                                                        #
# extra VPATH, Compilation flag modification             #
#         systeme module , etc ...                       #
#         external precompiled module librairie          #
#         etc ...                                        #
#                                                        #
##########################################################

#RJ: force lfi output binary reproducibility (avoid use of unix timestamps inside)
FUNDEFS  += -ULFI_BFB
FPPFLAGS += -DLFI_BFB

#RJ: changes to allow successful serial runtime
FUNDEFS  += -URJ_OFIX
FPPFLAGS += -DRJ_OFIX
#RJ: changes to allow successful MPI/OMP runtime
FUNDEFS  += -URJ_PFIX
FPPFLAGS += -DRJ_PFIX

#RJ ifneq "$(findstring 8,$(LFI_INT))" ""
#RJ OBJS_I8=spll_NEWLFI_ALL.o
#RJ $(OBJS_I8) : OPT = $(OPT_BASE) $(OPT_PERF2) $(OPT_I8)
#RJ endif
