##########################################################
#                                                        #
# Compiler Options                                       #
#                                                        #
##########################################################
#OBJDIR_PATH=/home/escj/azertyuiopqsdfghjklm/wxcvbn/azertyuiopqsdfghjklmwxcvbn
#
OPT_BASE  =  -g -traceback -noauto -convert big_endian -assume byterecl -fpic -O2 -fp-model strict -ftz -r8 -openmp -openmp-threadprivate compat

#
OPT_PERF0 =
OPT_PERF2 =
OPT_CHECK =
OPT_I8    = -i8
#
#
# Integer 4/8 option
#
#MNH_INT   ?=I4
LFI_RECL  ?=512
#
ifeq "$(MNH_INT)" "I8"
OPT_BASE         += $(OPT_I8)
LFI_INT           ?=8
MNH_MPI_RANK_KIND ?=8
else
MNH_MPI_RANK_KIND ?=4
LFI_INT           ?=4
endif
#
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
#
FC = ifort
#ifeq "$(VER_MPI)" "MPIAUTO"
#F90 = mpif90
#else
F90 = ifort
#endif
#
F90FLAGS      =  $(OPT)
F77 = $(F90)
F77FLAGS      =  $(OPT)
FX90 = $(F90)
FX90FLAGS     =  $(OPT)
#
LDFLAGS   =   -pc 64 -fp-stack-check -openmp -openmp-threadprivate compat -convert big_endian -assume byterecl
#
# preprocessing flags
#
CPP = cpp -P -traditional -Wcomment
#
CPPFLAGS_SURFEX    = DLINUX -DLITTLE -DLITTLE_ENDIAN -DHIGHRES -DADDRESS64 -DPOINTER_64 -D_ABI64 -DBLAS \
                    -DSTATIC_LINKING -DINTEL -D_RTTOV_DO_DISTRIBCOEF -DINTEGER_IS_INT \
                    -DREAL_8 -DREAL_BIGGER_THAN_INTEGER -DUSE_SAMIO -D_RTTOV_DO_DISTRIBCOEF -DNO_CURSES
CPPFLAGS_SURCOUCHE = -DMNH_MPI_DOUBLE_PRECISION -DMNH_LINUX -DMNH_MPI_BSEND -DDEV_NULL  -DMNH_MPI_RANK_KIND=$(MNH_MPI_RANK_KIND)
CPPFLAGS_RAD       =
CPPFLAGS_NEWLFI    = -DSWAPIO -DLINUX -DLFI_INT=${LFI_INT} -DLFI_RECL=${LFI_RECL}
CPPFLAGS_MNH       = -DMNH -DAINT=INT -DAMOD=MOD
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

ifneq "$(findstring 8,$(LFI_INT))" ""
OBJS_I8=spll_NEWLFI_ALL.o
$(OBJS_I8) : OPT = $(OPT_BASE) $(OPT_PERF2) $(OPT_I8)
endif
