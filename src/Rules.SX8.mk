##########################################################
#                                                        #
# Compiler Options                                       #
#                                                        #
##########################################################
ifneq "$(findstring brodie,$(shell uname -n))" ""
OBJDIR_PATH=${workdir}
endif
#
OPT_BASE = -pi -dw -Wf,-Nesc -Wf\"-A idbl4\" -Popenmp -Wf\"-pvctl loopcnt=10000000 shape=10000000\"
#-Wf"-pvctl fullmsg -O infomsg -L fmtlist" -Wf"-L transform" -Wf"-L summary"
OPT_PERF0 = -C debug
OPT_PERF1 = -C ssafe
OPT_PERF2 = -C vsafe
OPT_PERF3 = -C vopt
OPT_PERF4 = -C hopt
OPT_CHECK = -ePR -Wf"-init stack=nan" -Wf"-init heap=nan"
OPT_I8    = -ew
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
ifeq "$(OPTLEVEL)" "O2"
OPT       = $(OPT_BASE) $(OPT_PERF2) 
OPT0      = $(OPT_BASE) $(OPT_PERF0) 
OPT_NOCB  = $(OPT_BASE) $(OPT_PERF2)
endif
#
ifeq "$(OPTLEVEL)" "O3"
OPT       = $(OPT_BASE) $(OPT_PERF3) 
OPT0      = $(OPT_BASE) $(OPT_PERF0)
OPT_NOCB  = $(OPT_BASE) $(OPT_PERF3)
endif
#
ifeq "$(OPTLEVEL)" "O2PROF"
OPT       = $(OPT_BASE) $(OPT_PERF2) -ftrace
OPT0      = $(OPT_BASE) $(OPT_PERF0) -ftrace
OPT_NOCB  = $(OPT_BASE) $(OPT_PERF2) -ftrace
endif
#
ifeq "$(OPTLEVEL)" "DEBUG"
OPT       = $(OPT_BASE) $(OPT_PERF0) $(OPT_CHECK)
OPT0      = $(OPT_BASE) $(OPT_PERF0) $(OPT_CHECK)
OPT_NOCB  = $(OPT_BASE) $(OPT_PERF0)
#LDFLAGS   +=  -Wl"-f nan" 
endif
#
ifeq "$(OPTLEVEL)" "O4"
OPT       = $(OPT_BASE) $(OPT_PERF4) 
OPT0      = $(OPT_BASE) $(OPT_PERF0)
OPT_NOCB  = $(OPT_BASE) $(OPT_PERF4)
# routines with probleme with inlining
OBJS_NI += spll_ecmwf_radiation_vers2.o spll_slow_terms.o
$(OBJS_NI) :  OPT =  $(OPT_BASE)  $(OPT_PERF4) -Wf,-Ni
# problem for NPROC >= 4 & reprod_sum
OBJS2 += spll_mode_construct_ll.o \
spll_mode_repro_sum.o spll_dotprod.o \
spll_ini_radiations_ecmwf.o spll_mass_leak.o spll_mode_sum_ll.o spll_modi_sum_ll.o \
spll_p_abs.o spll_reduce_sum_0dd_ll.o spll_reduce_sum_1dd_ll.o spll_relaxdef.o \
spll_set_ref.o spll_surf_solar_sum.o spll_test_double_double.o spll_trid.o
$(OBJS2) :  OPT =  $(OPT_BASE)  $(OPT_PERF2) 
endif

ifneq "$(OPTLEVEL)" "DEBUG"
OBJSD += spll_default_alb_eco1_01_1.o spll_default_alb_eco1_02_1.o \
spll_default_alb_eco1_03_1.o spll_default_alb_eco1_04_1.o \
spll_default_alb_eco1_05_1.o spll_default_alb_eco1_06_1.o \
spll_default_alb_eco1_07_1.o spll_default_alb_eco1_08_1.o \
spll_default_alb_eco1_09_1.o spll_default_alb_eco1_10_1.o \
spll_default_alb_eco1_11_1.o spll_default_alb_eco1_12_1.o \
spll_default_alb_eco1_01_2.o spll_default_alb_eco1_02_2.o \
spll_default_alb_eco1_03_2.o spll_default_alb_eco1_04_2.o \
spll_default_alb_eco1_05_2.o spll_default_alb_eco1_06_2.o \
spll_default_alb_eco1_07_2.o spll_default_alb_eco1_08_2.o \
spll_default_alb_eco1_09_2.o spll_default_alb_eco1_10_2.o \
spll_default_alb_eco1_11_2.o spll_default_alb_eco1_12_2.o \
spll_default_alb_eco2_01_1.o spll_default_alb_eco2_02_1.o \
spll_default_alb_eco2_03_1.o spll_default_alb_eco2_04_1.o \
spll_default_alb_eco2_05_1.o spll_default_alb_eco2_06_1.o \
spll_default_alb_eco2_07_1.o spll_default_alb_eco2_08_1.o \
spll_default_alb_eco2_09_1.o spll_default_alb_eco2_10_1.o \
spll_default_alb_eco2_11_1.o spll_default_alb_eco2_12_1.o \
spll_default_alb_eco2_01_2.o spll_default_alb_eco2_02_2.o \
spll_default_alb_eco2_03_2.o spll_default_alb_eco2_04_2.o \
spll_default_alb_eco2_05_2.o spll_default_alb_eco2_06_2.o \
spll_default_alb_eco2_07_2.o spll_default_alb_eco2_08_2.o \
spll_default_alb_eco2_09_2.o spll_default_alb_eco2_10_2.o \
spll_default_alb_eco2_11_2.o spll_default_alb_eco2_12_2.o \
spll_default_alb_eco2_01_3.o spll_default_alb_eco2_02_3.o \
spll_default_alb_eco2_03_3.o spll_default_alb_eco2_04_3.o \
spll_default_alb_eco2_05_3.o spll_default_alb_eco2_06_3.o \
spll_default_alb_eco2_07_3.o spll_default_alb_eco2_08_3.o \
spll_default_alb_eco2_09_3.o spll_default_alb_eco2_10_3.o \
spll_default_alb_eco2_11_3.o spll_default_alb_eco2_12_3.o \
spll_default_lai_eco1_01.o spll_default_lai_eco1_02.o \
spll_default_lai_eco1_03.o spll_default_lai_eco1_04.o \
spll_default_lai_eco1_05.o spll_default_lai_eco1_06.o \
spll_default_lai_eco1_07.o spll_default_lai_eco1_08.o \
spll_default_lai_eco1_09.o spll_default_lai_eco1_10.o \
spll_default_lai_eco1_11.o spll_default_lai_eco1_12.o \
spll_default_lai_eco2_y2002_01.o spll_default_lai_eco2_y2002_02.o \
spll_default_lai_eco2_y2002_03.o spll_default_lai_eco2_y2002_04.o \
spll_default_lai_eco2_y2002_05.o spll_default_lai_eco2_y2002_06.o \
spll_default_lai_eco2_y2002_07.o spll_default_lai_eco2_y2002_08.o \
spll_default_lai_eco2_y2002_09.o spll_default_lai_eco2_y2002_10.o \
spll_default_lai_eco2_y2002_11.o spll_default_lai_eco2_y2002_12.o \
spll_default_lai_eco2_y2003_01.o spll_default_lai_eco2_y2003_02.o \
spll_default_lai_eco2_y2003_03.o spll_default_lai_eco2_y2003_04.o \
spll_default_lai_eco2_y2003_05.o spll_default_lai_eco2_y2003_06.o \
spll_default_lai_eco2_y2003_07.o spll_default_lai_eco2_y2003_08.o \
spll_default_lai_eco2_y2003_09.o spll_default_lai_eco2_y2003_10.o \
spll_default_lai_eco2_y2003_11.o spll_default_lai_eco2_y2003_12.o \
spll_default_lai_eco2_y2004_01.o spll_default_lai_eco2_y2004_02.o \
spll_default_lai_eco2_y2004_03.o spll_default_lai_eco2_y2004_04.o \
spll_default_lai_eco2_y2004_05.o spll_default_lai_eco2_y2004_06.o \
spll_default_lai_eco2_y2004_07.o spll_default_lai_eco2_y2004_08.o \
spll_default_lai_eco2_y2004_09.o spll_default_lai_eco2_y2004_10.o \
spll_default_lai_eco2_y2004_11.o spll_default_lai_eco2_y2004_12.o \
spll_default_lai_eco2_y2005_01.o spll_default_lai_eco2_y2005_02.o \
spll_default_lai_eco2_y2005_03.o spll_default_lai_eco2_y2005_04.o \
spll_default_lai_eco2_y2005_05.o spll_default_lai_eco2_y2005_06.o \
spll_default_lai_eco2_y2005_07.o spll_default_lai_eco2_y2005_08.o \
spll_default_lai_eco2_y2005_09.o spll_default_lai_eco2_y2005_10.o \
spll_default_lai_eco2_y2005_11.o spll_default_lai_eco2_y2005_12.o \
spll_default_lai_eco2_y2006_01.o spll_default_lai_eco2_y2006_02.o \
spll_default_lai_eco2_y2006_03.o spll_default_lai_eco2_y2006_04.o \
spll_default_lai_eco2_y2006_05.o spll_default_lai_eco2_y2006_06.o \
spll_default_lai_eco2_y2006_07.o spll_default_lai_eco2_y2006_08.o \
spll_default_lai_eco2_y2006_09.o spll_default_lai_eco2_y2006_10.o \
spll_default_lai_eco2_y2006_11.o spll_default_lai_eco2_y2006_12.o
$(OBJSD) : OPT = $(OPT_BASE) $(OPT_PERF0)
endif

ifeq "$(VER_MPI)" "NOMPI"
F90 = sxf90
else         
F90 = sxmpif90
endif

F90FLAGS  = $(OPT)
F77 = $(F90)     
F77FLAGS  =    -f0 $(OPT)
FX90 = $(F90)        
FX90FLAGS =    -f0 $(OPT)
# 
LDFLAGS += $(OPT) -Wl,-Z,80G 
#
#
# preprocessing flags 
#
CPP = cpp -P -traditional -Wcomment
AR=sxar
#
FPPFLAGS_SURFEX    =
#RJ FPPFLAGS_SURCOUCHE = -DMNH_MPI_DOUBLE_PRECISION -DMNH_SX5 -DMNH_MPI_BSEND -DMNH_MPI_RANK_KIND=$(MNH_MPI_RANK_KIND)
#RJ FPPFLAGS_RAD       =
#RJ FPPFLAGS_NEWLFI    = -DMNH_SX5 -DLFI_INT=${LFI_INT} -DLFI_RECL=${LFI_RECL}
#RJ FPPFLAGS_MNH       = -DMNH
#
# Gribex flags
#
#ARCH_GRIBEX=NEC
TARGET_GRIBEX=NEC
CNAME_GRIBEX=sxmpif90
##########################################################
#                                                        #
# Source of MESONH PACKAGE  Distribution                 #
#                                                        #
##########################################################
#DIR_SURFEX      += ARCH_SRC/surfex
#
include Makefile.SURFEX.mk
#
INC += -I/SX/opt/sxf90/${SXF90VERSION}/include
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
OBJS0 += spll_ch_jac.o spll_ch_terms.o  \
         spll_fm_writ_ll.o spll_NEWLFI_ALL.o spll_fm_read_ll.o
$(OBJS0) : OPT_CHECK = 
