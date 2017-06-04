##########################################################
#                                                        #
# Compiler Options                                       #
#                                                        #
##########################################################
#OBJDIR_PATH=/home/escj/azertyuiopqsdfghjklm/wxcvbn/azertyuiopqsdfghjklmwxcvbn
#
#OPT_BASE  = -q64 -qsigtrap -qfloat=nans \
            -qflttrap=enable:overflow:zerodivide:invalid \
            -qautodbl=dbl4 -qzerosize -g -qstrict -qfullpath -qspillsize=32648 \
            -qinitauto=0 -qdpc=e -qmaxmem=-1 -qnoescape
OPT_BASE = -g -w -qsmp=omp -qrealsize=8 -qnoescape -q64 -qextname \
	   -NS32648 -qmaxmem=-1 -bbigtoc
OPT_PERF0 = -O0 -qnooptimize
OPT_PERF2 = -O2 
OPT_PERF2 = -O3 -qarch=pwr6 -qstrict
OPT_CHECK = -C
OPT_I8    = -qintsize=8
#
#
# Integer 4/8 option
#
MNH_INT   ?=I4
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
ifeq "$(OPTLEVEL)" "O3"
OPT       = $(OPT_BASE) $(OPT_PERF3)
OPT0      = $(OPT_BASE) $(OPT_PERF0)
OPT_NOCB  = $(OPT_BASE) $(OPT_PERF3)
endif
#
#            
F90 = mpxlf95_r
F90FLAGS =       $(OPT) -qfree=f90 -qsuffix=f=f90 
F77 = $(F90)
F77FLAGS      =  $(OPT) -qfixed
FX90 = $(F90)
FX90FLAGS     =  $(OPT) -qfixed
FC = $(F90)
#

#
# vargas / c1a underscore management 
#
ifneq "$(findstring c1a,$(shell uname -n))" ""
LDFLAGS   =  $(OPT) -brename:.fminbits_in_word_,.fminbits_in_word__ -bloadmap:exec.log.out
else
LDFLAGS   =  $(OPT) -brename:.flush,.flush_ 
endif
#
# preprocessing flags 
#
CPP = /usr/lib/cpp -C -P -qlanglvl=classic
#
CPPFLAGS_SURFEX    =
CPPFLAGS_SURCOUCHE = -DMNH_MPI_DOUBLE_PRECISION -DMNH_LINUX -DMNH_SP4 -DMNH_MPI_ISEND  -DMNH_MPI_RANK_KIND=$(MNH_MPI_RANK_KIND)
CPPFLAGS_RAD       =
CPPFLAGS_NEWLFI    = -DLINUX -DLFI_INT=${LFI_INT} -DLFI_RECL=${LFI_RECL}
CPPFLAGS_MNH       = -DAMAX1=MAX -DMNH
#
# Gribex flags
#
#TARGET_GRIBEX=rs6000
TARGET_GRIBEX=ibm_power4
CNAME_GRIBEX=""
#A64=A64
##########################################################
#                                                        #
# Source of MESONH PACKAGE  Distribution                 #
#                                                        #
##########################################################
#DIR_SURFEX      += ARCH_SRC/surfex 
#DIR_SURCOUCHE   += ARCH_SRC/bug_surcouche
#
include Makefile.SURFEX.mk
#
INC += -I/usr/lpp/xlf/include
VPATH += /usr/lpp/xlf/include
CPPFLAGS += -DAIX64
#
##########################################################
#                                                        #
# extra VPATH, Compilation flag modification             #
#         systeme module , etc ...                       #
#         external precompiled module librairie          #
#         etc ...                                        #
#                                                        #
##########################################################
OPT_PERF1  =  -O0 # option -O1 non encore supporté sur IBM !!!
OBJS_O1 += spll_aeroopt_get.o
$(OBJS_O1) : OPT = $(OPT_BASE) $(OPT_PERF1)

#OBJS_O0 += spll_compute_exner_from_ground3d.o  spll_compute_exner_from_ground1d.o spll_modi_set_rsou.o spll_set_rsou.o
OBJS_O0 += spll_compute_exner_from_ground1d.o 
$(OBJS_O0) : OPT = $(OPT_BASE) $(OPT_PERF0)
#
