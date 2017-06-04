#SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
#SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
#SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
#SFX_LIC for details. version 1.
##########################################################
#                                                        #
# Compiler Options                                       #
#                                                        #
##########################################################
#
# use splr.pl script for dependency generation
USE_SPLR = YES
#
#OBJDIR_PATH=/home/escj/azertyuiopqsdfghjklm/wxcvbn/azertyuiopqsdfghjklmwxcvbn
#
OPT_BASE  = -fdefault-real-8 -fdefault-double-8 -g -fno-second-underscore -fpic  -ffpe-trap=overflow,zero,invalid -fbacktrace -fconvert=swap
#
OPT_PERF0 = -O0
OPT_PERF2 = -O2
OPT_CHECK = -fcheck=bounds,do,mem,pointer,recursion -finit-real=nan
OPT_I8    = -fdefault-integer-8
#
#
# Integer 4/8 option
#
#MNH_INT   ?=I4
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
ifneq "$(OPTLEVEL)" "DEBUG"
OBJSD += spll_teb_garden.o
$(OBJSD) : OPT = $(OPT_BASE) $(OPT_PERF0)
endif
#
ifeq "$(VER_MPI)" "NOMPI"
F90= gfortran
CC = gcc
else
F90 = mpif90
CC  = mpicc
endif
#
REALFC=gfortran
#
FCFLAGS_OMP= -fopenmp
CFLAGS_OMP= -fopenmp
ifeq "$(VER_OMP)" "NOOMP"
FCFLAGS_OMP=
CFLAGS_OMP=
endif
#
F90FLAGS      = $(FCFLAGS_OMP) $(OPT)
F77 = $(F90)
F77FLAGS      = $(FCFLAGS_OMP) $(OPT)
FX90 = $(F90)
FX90FLAGS     = $(FCFLAGS_OMP) $(OPT)
#
LDFLAGS   =  $(FCFLAGS_OMP) -Wl,-warn-once -ldl -lrt
#
# preprocessing flags
#
CPP = cpp -P -traditional -Wcomment
#
FPPFLAGS_SURFEX    =
#RJ FPPFLAGS_SURCOUCHE = -DMNH_MPI_DOUBLE_PRECISION -DMNH_LINUX -DMNH_MPI_BSEND -DDEV_NULL  -DMNH_MPI_RANK_KIND=$(MNH_MPI_RANK_KIND)
#RJ FPPFLAGS_RAD       =
#RJ FPPFLAGS_NEWLFI    = -DSWAPIO -DLINUX -DLFI_INT=${LFI_INT} -DLFI_RECL=${LFI_RECL}
#RJ FPPFLAGS_MNH       = -DMNH -DAINT=INT -DAMOD=MOD
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

#RJ ifneq "$(findstring 8,$(LFI_INT))" ""
#RJ OBJS_I8=spll_NEWLFI_ALL.o
#RJ $(OBJS_I8) : OPT = $(OPT_BASE) $(OPT_PERF2) $(OPT_I8)
#RJ endif
