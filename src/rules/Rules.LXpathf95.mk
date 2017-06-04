##########################################################
#                                                        #
# Compiler Options                                       #
#                                                        #
##########################################################
#OBJDIR_PATH=/home/escj/azertyuiopqsdfghjklm/wxcvbn/azertyuiopqsdfghjklmwxcvbn
#
OPT_BASE  = -r8 -module $(OBJDIR) -g -fno-second-underscore -fPIC
OPT_PERF0 = -O0
OPT_PERF2 = -O2
OPT_CHECK = -C -ffortran-bounds-check 
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
F90 = pathf95
TOCASE=toupper
F90FLAGS      =  $(OPT) 
F77 = $(F90)
#F77FLAGS      =  $(OPT) -ffixed-line-length-132
F77FLAGS      =  $(OPT) 
FX90 = $(F90)
#FX90FLAGS     =  $(OPT) -ffixed-line-length-132
FX90FLAGS     =  $(OPT) 
#
LDFLAGS   =  -Wl,-noinhibit-exec  -Wl,-warn-once
#
# preprocessing flags 
#
CPP = cpp -P -traditional -Wcomment
#
CPPFLAGS_SURFEX    =
CPPFLAGS_SURCOUCHE = -DMNH_MPI_DOUBLE_PRECISION -DMNH_LINUX -DMNH_MPI_BSEND -DDEV_NULL
CPPFLAGS_RAD       =
CPPFLAGS_NEWLFI    = -DSWAPIO -DLINUX
CPPFLAGS_MNH       = -DAINT=INT -DAMOD=MOD -DMNH
#
# Gribex flags
#
TARGET_GRIBEX=linux
CNAME_GRIBEX=pathf95
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
