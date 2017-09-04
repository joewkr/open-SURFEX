##########################################################
#                                                        #
# Compiler Options                                       #
#                                                        #
##########################################################
OBJDIR_PATH=${workdir}
# -qsigtrap -qfloat=nans
# -qflttrap=enable:overflow:zerodivide:invalid
# -qextname
#OPT_BASE  = -qflttrap=enable:overflow:zerodivide:invalid \
#            -qfloat=nans -qarch=450 -qmoddir=$(OBJDIR) \
#            -qautodbl=dbl4 -qzerosize -g -qfullpath -qspillsize=32648 \
#            -qinitauto=0 -qdpc=e -qmaxmem=-1

OPT_BASE  = -qmoddir=$(OBJDIR) -qautodbl=dbl4 -qzerosize
# -qnopic

#OPT_PERF0   = -O0 -qnooptimize -qkeepparm
OPT_PERF2    = -O2 -qmaxmem=-1
#OPT_CHECK = -C
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
ifeq "$(OPTLEVEL)" "O3"
OPT_PERF3    = -O3 -qstrict -qmaxmem=-1
OPT       = $(OPT_BASE) $(OPT_PERF3)
OPT0      = $(OPT_BASE) $(OPT_PERF0)
OPT_NOCB  = $(OPT_BASE) $(OPT_PERF3)
endif
#
ifeq "$(OPTLEVEL)" "O3SMP"
OPT_PREF3SMP = -O3 -qsmp -qstrict -qmaxmem=-1
OPT       = $(OPT_BASE) $(OPT_PERF3SMP)
OPT0      = $(OPT_BASE) $(OPT_PERF0)
OPT_NOCB  = $(OPT_BASE) $(OPT_PERF3SMP)
endif
#
#
ifeq "$(OPTLEVEL)" "O4"
OPT_PERF4    = -O4
OPT       = $(OPT_BASE) $(OPT_PERF4)
OPT0      = $(OPT_BASE) $(OPT_PERF0)
OPT_NOCB  = $(OPT_BASE) $(OPT_PERF4)
endif
#
#
F90 = mpixlf95_r
F90FLAGS =       $(OPT) -qfree=f90 -qsuffix=f=f90
F77 = $(F90)
F77FLAGS      =  $(OPT) -qfixed
FX90 = $(F90)
FX90FLAGS     =  $(OPT) -qfixed
#
LDFLAGS   =  $(OPT) -Wl,--relax
AR = /bgsys/drivers/ppcfloor/gnu-linux/powerpc-bgp-linux/bin/ar
#
# preprocessing flags
#
CPP = cpp -P -traditional -Wcomment
CC  = mpixlc_r
#
CPPFLAGS_SURFEX    =
CPPFLAGS_SURCOUCHE = -DMNH_MPI_DOUBLE_PRECISION -DMNH_LINUX -DMNH_SP4 -DMNH_MPI_ISEND
CPPFLAGS_RAD       =
CPPFLAGS_NEWLFI    = -DLINUX
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
DIR_MNH      += ARCH_SRC/bug_mnh_BG
#
include Makefile.MESONH.mk
#
##########################################################
#                                                        #
# extra VPATH, Compilation flag modification             #
#         systeme module , etc ...                       #
#         external precompiled module librairie          #
#         etc ...                                        #
#                                                        #
##########################################################
OBJS_NOCB += spll_prep_ideal_case.o spll_mesonh.o
$(OBJS_NOCB) : OPT = $(OPT_NOCB)
#
IGNORE_OBJS += spll_abort.o spll_ch_make_lookup.o \
spll_compute_ver_grid.o spll_convlfi.o spll_diag.o spll_example_fwd.o spll_latlon_to_xy.o \
spll_prep_nest_pgd.o spll_prep_pgd.o spll_prep_real_case.o \
spll_prep_surfex.o spll_rad1driv.o spll_rttov_ascii2bin_coef.o spll_rttovcld_testad.o spll_rttovcld_test.o \
spll_rttovscatt_test.o spll_spawning.o spll_test_2_coef.o spll_test_coef.o spll_test_errorhandling.o \
spll_test_q2v.o spll_xy_to_latlon.o spll_zoom_pgd.o

