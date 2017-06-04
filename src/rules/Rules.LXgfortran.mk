##########################################################
#                                                        #
# Compiler Options                                       #
#                                                        #
##########################################################
#OBJDIR_PATH=/home/escj/azertyuiopqsdfghjklm/wxcvbn/azertyuiopqsdfghjklmwxcvbn
#
OPT_BASE  = -fopenmp -fdefault-real-8 -fdefault-double-8 -g -fno-second-underscore -fpic  -ffpe-trap=overflow,zero,invalid  -fbacktrace -fconvert=swap
#
OPT_PERF0 = -O0
OPT_PERF2 = -O2
OPT_CHECK = -fbounds-check -finit-real=nan
OPT_I8    = -fdefault-integer-8 
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
ifeq "$(VER_MPI)" "MPIAUTO"
F90 = mpif90
else         
F90 = gfortran
endif
#
F90FLAGS      =  $(OPT) 
F77 = $(F90)
F77FLAGS      =  $(OPT) 
FX90 = $(F90)
FX90FLAGS     =  $(OPT) 
#
LDFLAGS   =   -Wl,-warn-once -fopenmp
#
# preprocessing flags 
#
CPP = cpp -P -traditional -Wcomment
#
CPPFLAGS_SURFEX    =
CPPFLAGS_SURCOUCHE = -DMNH_MPI_DOUBLE_PRECISION -DMNH_LINUX -DMNH_MPI_BSEND -DDEV_NULL  -DMNH_MPI_RANK_KIND=$(MNH_MPI_RANK_KIND)
CPPFLAGS_RAD       =
CPPFLAGS_NEWLFI    = -DSWAPIO -DLINUX -DLFI_INT=${LFI_INT} -DLFI_RECL=${LFI_RECL}
CPPFLAGS_MNH       = -DMNH -DAINT=INT -DAMOD=MOD
#
# Gribex flags
#
TARGET_GRIBEX=linux
CNAME_GRIBEX=_gfortran
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
