# Load "standard" Intel fortran settings
include Rules.LXifort.mk

# Overide compilers choice with Ryad's wrappers for enforcing Intel and IntelMPI versions
F90=/home/gmgec/mrga/alias/public/bin/impi-5.1.2.150_ifc-16.1.150
CC=/home/gmgec/mrga/alias/public/bin/impi-5.1.2.150_icc-16.1.150

# Note : Choice of NetCDF library is driven by VER_CDF, which is set upstream 
# (before configure), and used in Makefile.SURFEX.mk
