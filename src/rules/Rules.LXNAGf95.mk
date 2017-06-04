##########################################################
#                                                        #
# Compiler Options                                       #
#                                                        #
##########################################################
#OBJDIR_PATH=/home/escj/azertyuiopqsdfghjklm/wxcvbn/azertyuiopqsdfghjklmwxcvbn
#
OPT_BASE  = -r8 -mdir $(OBJDIR) -g -gline -kind=byte -w -maxcontin=200 -PIC
OPT_PERF0 = -O0
OPT_PERF2 = -O2
OPT_CHECK = -C -nan
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
F90 = f95
F90FLAGS      =  $(OPT) 
F77 = $(F90)
#F77FLAGS      =  $(OPT) -132 -fixed
F77FLAGS      =  $(OPT) -fixed
FX90 = $(F90)
#FX90FLAGS     =  $(OPT) -132 -fixed
FX90FLAGS     =  $(OPT) -fixed
#
LDFLAGS   =  -Wl,-Xlinker,-noinhibit-exec  -Wl,-Xlinker,-warn-once
#
# preprocessing flags 
#
CPP = cpp -P -traditional -Wcomment
#
CPPFLAGS_SURFEX    =
CPPFLAGS_SURCOUCHE = -DMNH_MPI_DOUBLE_PRECISION -DMNH_LINUX -DMNH_MPI_BSEND -DNAGf95
CPPFLAGS_RAD       =
CPPFLAGS_NEWLFI    = -DSWAPIO -DLINUX
CPPFLAGS_MNH       = -DMNH 

#
# Gribex flags
#
TARGET_GRIBEX=linux
CNAME_GRIBEX=f95
##########################################################
#                                                        #
# Source of MESONH PACKAGE  Distribution                 #
#                                                        #
##########################################################
#DIR_SURFEX      += ARCH_SRC/surfex
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
#
#  Where to find precompiled system module for "NAGf95"
#  like "f90_unix.mod"  , etc ...
#MODULE_SYSTEM = /opt/F95_42/lib/bytes
MODULE_SYSTEM = /usr/local/lib/NAGWare/bytes
all : $(MODULE_SYSTEM)
$(MODULE_SYSTEM) :
	@test -d $@ || \
        ( echo ATTENTION :: Rules.LXNAGF95 ; \
          echo MODULE_SYSTEM=$@ n existe pas ; exit 1 )
VPATH += $(MODULE_SYSTEM)
INC   += -I$(MODULE_SYSTEM)

#
# non conformance des argument/routine dans les appels MPI
#
OBJS1 = spll_mode_exchange2_ll.o spll_mode_exchange_ll.o spll_mode_fm.o \
spll_mode_fmread.o spll_mode_fmwrit.o spll_mode_gather_ll.o \
spll_mode_init_ll.o spll_mode_io_ll.o spll_mode_scatter_ll.o \
spll_mode_sum2_ll.o spll_mode_sum_ll.o spll_mode_tools_ll.o \
spll_zdiffusetup.o spll_lapack.o spll_fm_writ_ll.o \
spll_NEWLFI_ALL.o spll_fm_read_ll.o \
mode_tools_ll.mod mode_gather_ll.mod \
mode_fmwrit.mod mode_scatter_ll.mod \
mode_fmread.mod mode_sum_ll.mod \
mode_exchange_ll.mod
#
$(OBJS1): OPT = $(OPT_BASE) $(OPT_PERF0) -dusty 
