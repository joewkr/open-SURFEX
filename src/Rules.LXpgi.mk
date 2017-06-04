##########################################################
#                                                        #
# Compiler Options                                       #
#                                                        #
##########################################################
#OBJDIR_PATH=${WORKDIR}
#
OPT_BASE   =  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical -byteswapio
OPT_PERF0  =  -O0 -Kieee
OPT_PERF2  =  -O2 -Kieee
#OPT_CUDA  =  -O2 -Mcuda=keepgpu -ta=nvidia,cc20,cuda3.1,host,time -Minfo=accel,intensity,all,ccff  
OPT_CUDA  =  -O3 -fast -ta=nvidia,cc20,cuda3.1,keepgpu,host -Minfo=all,intensity,ccff 
OPT_CHECK  =  -C 
OPT_PROF  =  -Mprof=time,ccff
OPT_I8    =  -i8
#
# Integer option
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
#
OPT       = $(OPT_BASE) $(OPT_PERF2)
OPT0      = $(OPT_BASE) $(OPT_PERF0)
OPT_NOCB  = $(OPT_BASE) $(OPT_PERF2)
#
ifeq "$(OPTLEVEL)" "O2PROF"
OPT       = $(OPT_BASE) $(OPT_PERF2) $(OPT_PROF)
OPT0      = $(OPT_BASE) $(OPT_PERF0) $(OPT_PROF)
OPT_NOCB  = $(OPT_BASE) $(OPT_PERF2) $(OPT_PROF)
endif
ifeq "$(OPTLEVEL)" "DEBUG"
OPT       = $(OPT_BASE) $(OPT_PERF0) $(OPT_CHECK)
OPT0      = $(OPT_BASE) $(OPT_PERF0) $(OPT_CHECK)
OPT_NOCB  = $(OPT_BASE) $(OPT_PERF0)
endif

ifeq "$(OPTLEVEL)" "CUDA"
OPT       = $(OPT_BASE) $(OPT_CUDA) 
OPT0      = $(OPT_BASE) $(OPT_CUDA) $(OPT_PERF0)
OPT_NOCB  = $(OPT_BASE) $(OPT_CUDA) 
endif
#-Mcuda -ta=nvidia,host,time -Minfo=accel,intensity
#
FC = pgf90
ifeq "$(VER_MPI)" "MPIAUTO"
F90 = mpif90
CC  = mpicc
else
F90 = pgf90
CC  =
endif
#
F77FLAGS  =  $(OPT)
F77 = $(F90)
F90FLAGS  =  $(OPT)
FX90 = $(F90)
FX90FLAGS =  $(OPT)
#
LDFLAGS    =   -Wl,-warn-once $(OPT)
#
# preprocessing flags 
#
CPP = cpp -P -traditional -Wcomment
#

FPPFLAGS_SURFEX    =
#RJ FPPFLAGS_SURCOUCHE = -DMNH_MPI_DOUBLE_PRECISION -DMNH_LINUX -DMNH_MPI_BSEND -DMNH_MPI_RANK_KIND=$(MNH_MPI_RANK_KIND)
#RJ FPPFLAGS_RAD       =
#RJ FPPFLAGS_NEWLFI    = -DSWAPIO -DLINUX -DLFI_INT=${LFI_INT} -DLFI_RECL=${LFI_RECL}
#RJ FPPFLAGS_MNH       = -DMNH 

#
# Gribex flags
#
TARGET_GRIBEX=linux
CNAME_GRIBEX=_pgf77

##########################################################
#                                                        #
# Source of MESONH PACKAGE  Distribution                 #
#                                                        #
##########################################################
#DIR_SURFEX   += ARCH_SRC/surfex.MNH-462

OBJS_NOCB +=  spll_isba.o
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
#
#MODULE_SYSTEM = /opt/F95_42/lib/
#VPATH += $(MODULE_SYSTEM)
#



