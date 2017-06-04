#!/bin/bash

# Some sanity checks
if [ "$SRC_SURFEX" == "" -o "$XYZ" == "" ]; then
  echo "You need to source the config file before calling this script!"
  exit 1
else
  suffix=$1
  [ ! -d $SRC_SURFEX/MY_RUN/SODA ] && echo "The directory $SRC_SURFEX/MY_RUN/SODA was not found" && exit 1
  [ ! -f $SRC_SURFEX/exe/SODA$XYZ ] && echo "The needed binary $SRC_SURFEX/exe/SODA$XYZ was not found" && exit 1
fi

###################################
YY=2008
MM=10
DD=01
HH=00
FCINT=06
npert=4
###################################

work=$SRC_SURFEX/MY_RUN/SODA/RUN/RUN_EKF
[ -d $work ] && rm -r $work
mkdir $work
cd $work || exit 1

# Copy namelists
cp $SRC_SURFEX/MY_RUN/SODA/NAMELISTS/OPTIONS.nam_soda_EKF_$suffix OPTIONS.nam

# Ecoclimap
ln -sf $SRC_SURFEX/MY_RUN/ECOCLIMAP/*.bin .
ln -sf $SRC_SURFEX/MY_RUN/ECOCLIMAP/*.dat .

# Copy PGD file
cp  $SRC_SURFEX/MY_RUN/SODA/OUTPUT/PGD_SODA.$suffix .

# Copy input grib files
cp $SRC_SURFEX/MY_RUN/SODA/INPUT/FG_OI_MAIN .
cp $SRC_SURFEX/MY_RUN/SODA/INPUT/CANARI .
cp $SRC_SURFEX/MY_RUN/SODA/INPUT/SST_SIC .
cp $SRC_SURFEX/MY_RUN/SODA/INPUT/clim_isba .

# Run control + perturbed runs
pert=0
while [ $pert -le $npert ]; do
  mbr="_PERT_"$(printf "%.2d" "$pert")

  # Perturb initial state
  # Copy the initial PREP file that should be perturbed
  cp $SRC_SURFEX/MY_RUN/SODA/OUTPUT/PREP_SODA.$suffix $SRC_SURFEX/MY_RUN/SODA/OUTPUT/PREP_SODA$mbr.$suffix

  # Run (pertubed ) offline
  offline="$SRC_SURFEX/MY_RUN/SODA/TESTS/OFFLINE.sh"
  if [ -f $offline ]; then
    $offline $pert $suffix || exit 1
  else
    echo "No script $offline found"
    exit 1
  fi

  # Copy stored simulation results
  cp $SRC_SURFEX/MY_RUN/SODA/OUTPUT/PREP_OFFLINE$mbr.$suffix PREP_EKF_PERT$pert.$suffix
 
  pert=$(( $pert + 1 ))
done

# Copy first guess. In this case OFFLINE PREP file (control run)
cp $SRC_SURFEX/MY_RUN/SODA/OUTPUT/PREP_OFFLINE_PERT_00.$suffix PREP_SODA_EKF.$suffix

###################
## Do EKF steps
###################

## Evolve B matrix - could be reset to BO periodically (test to be defined)
#
##
## Get BGROUND from the previous cycle
##
#
echo "Assuming we are evolving B matrix from initial state for first cycle with assimilation"

#
## Soil analyis
#

sed -e "s/LBEV=LBEV/LBEV=.TRUE./" \
    -e "s/LPRT=LPRT/LPRT=.FALSE./" \
    -e "s/LSIM=LSIM/LSIM=.FALSE./" \
    -e "s/IVAR=IVAR/IVAR=1/" \
    $SRC_SURFEX/MY_RUN/SODA/NAMELISTS/OPTIONS.nam_soda_EKF_$suffix > OPTIONS.nam

# Run SODA for EKF
[ ! -f $SRC_SURFEX/exe/SODA$XYZ ] && echo "$SRC_SURFEX/exe/SODA$XYZ not found" && exit 1
$SRC_SURFEX/exe/SODA$XYZ

mv BGROUNDout_LBEV.* $SRC_SURFEX/MY_RUN/SODA/OUTPUT/
mv BGROUNDout_ASSIM.* $SRC_SURFEX/MY_RUN/SODA/OUTPUT/

for f in `ls -1 LTM_del*_del*`; do
  [ -f $f ] && mv $f $SRC_SURFEX/MY_RUN/SODA/OUTPUT/${f}_$YY$MM$DD$HH.DAT
done

# Save output
mv PREP_SODA_EKF.$suffix $SRC_SURFEX/MY_RUN/SODA/OUTPUT/PREP_SODA_EKF.$suffix

