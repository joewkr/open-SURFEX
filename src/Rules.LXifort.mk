#SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
#SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
#SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
#SFX_LIC for details. version 1.
##########################################################
#                                                        #
# Compiler Options                                       #
#                                                        #
##########################################################
#OBJDIR_PATH=/home/escj/azertyuiopqsdfghjklm/wxcvbn/azertyuiopqsdfghjklmwxcvbn
#
# use splr.pl script for dependency generation
#
DO_ASM = NO
USE_SPLR = YES
USE_SPLR_WRAP = NO
#AVOID_CPP = YES
AVOID_CPP = NO
#
OPT_BASE   =  -convert big_endian -r8 -g -assume nosource_include -assume byterecl -fpic -traceback -fp-model precise
#-switch fe_inline_all_arg_copy_inout
OPT_PERF0  =  -O0
OPT_PERF2  =  -O2 -fpe0 -ftz
OPT_CHECK  =  -fp-stack-check -ftrapuv -fpe3 -fp-speculation=strict -check all
# -diag-error  -debug full -assume fpe_summary -openmp-report2
OPT_I8     =  -i8
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
CFLAGS   += -g
endif
ifeq "$(OPTLEVEL)" "O2PAR"
PAR= -parallel -diag-file -par-report2
OPT       = $(OPT_BASE) $(OPT_PERF2) $(PAR)
OPT0      = $(OPT_BASE) $(OPT_PERF0) $(PAR)
OPT_NOCB  = $(OPT_BASE) $(OPT_PERF2) $(PAR)
endif
ifeq "$(OPTLEVEL)" "O2NOVEC"
OPT       = $(OPT_BASE) $(OPT_PERF2) -no-vec
OPT0      = $(OPT_BASE) $(OPT_PERF0) -no-vec
OPT_NOCB  = $(OPT_BASE) $(OPT_PERF2) -no-vec
endif
#
#
ifeq "$(VER_MPI)" "NOMPI"
F90= ifort
CC = icc
else
F90= mpiifort
CC = mpiicc
endif
#
REALFC=ifort
#
FCFLAGS_OMP= -openmp -openmp-threadprivate=compat
CFLAGS_OMP=
ifeq "$(VER_OMP)" "NOOMP"
FCFLAGS_OMP=
CFLAGS_OMP=
endif
#
F90FLAGS  = $(FCFLAGS_OMP) $(OPT)
F77  = $(F90)
F77FLAGS  = $(FCFLAGS_OMP) $(OPT)
# -132
FX90 = $(F90)
FX90FLAGS = $(FCFLAGS_OMP) $(OPT)
# -132
#
#LDFLAGS    =  -Wl,-noinhibit-exec  -Wl,-warn-once $(PAR)
LDFLAGS    =  $(FCFLAGS_OMP) -Wl,-warn-once $(PAR) -ldl -lrt
#
CFLAGS = $(CFLAGS_OMP) -DLINUX -DLITTLE_ENDIAN -DLITTLE -O3 -xAVX -vec-report3  -DPOINTER_64
#
# preprocessing flags
#
CPP = cpp -P -traditional -Wcomment
#
FPPFLAGS_SURFEX    =
#RJ FPPFLAGS_SURCOUCHE = -DMNH_MPI_DOUBLE_PRECISION -DMNH_LINUX -DMNH_MPI_BSEND -DDEV_NULL -DMNH_MPI_RANK_KIND=$(MNH_MPI_RANK_KIND)
#RJ #FPPFLAGS_SURCOUCHE = -DMNH_MPI_DOUBLE_PRECISION -DMNH_LINUX -DMNH_MPI_ISEND -DDEV_NULLL -DMNH_MPI_RANK_KIND=$(MNH_MPI_RANK_KIND)
#RJ FPPFLAGS_RAD       =
#RJ FPPFLAGS_NEWLFI    = -DSWAPIO -DLINUX -DLFI_INT=${LFI_INT} -DLFI_RECL=${LFI_RECL}
#RJ FPPFLAGS_MNH       = -DMNH
#
# Gribex flags
#
TARGET_GRIBEX=linux
CNAME_GRIBEX=ifort
##########################################################
#                                                        #
# Source of MESONH PACKAGE  Distribution                 #
#                                                        #
##########################################################
#DIR_SURCOUCHE   += ARCH_SRC/bug_surcouche
#DIR_MNH         += ARCH_SRC/bug_mnh
#DIR_RAD         += ARCH_SRC/bug_rad
#DIR_SURFEX      += ARCH_SRC/surfex
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
# Juan & Maud 20/03/2008 --> Ifort 10.1.008 Bug O2 optimization
OPT_PERF1  =  -O1
OBJS_O1= spll_schu.o spll_ps2str.o spll_p_abs.o spll_ini_one_way_n.o spll_urban_solar_abs.o
$(OBJS_O1) : OPT = $(OPT_BASE) $(OPT_PERF1)

#RJ ifneq "$(findstring 8,$(LFI_INT))" ""
#RJ OBJS_I8=spll_NEWLFI_ALL.o
#RJ $(OBJS_I8) : OPT = $(OPT_BASE) $(OPT_PERF2) $(OPT_I8)
#RJ endif

