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

# Run offline
offline="$SRC_SURFEX/MY_RUN/SODA/TESTS/OFFLINE.sh"
if [ -f $offline ]; then
  $offline -1 $suffix || exit 1
else
  echo "No script $offline found"
  exit 1
fi

work=$SRC_SURFEX/MY_RUN/SODA/RUN/RUN_OI
[ -d $work ] || mkdir $work
cd $work || exit 1

# Copy namelists
cp $SRC_SURFEX/MY_RUN/SODA/NAMELISTS/OPTIONS.nam_soda_OI_$suffix OPTIONS.nam

# Ecoclimap
ln -sf $SRC_SURFEX/MY_RUN/ECOCLIMAP/*.bin .
ln -sf $SRC_SURFEX/MY_RUN/ECOCLIMAP/*.dat .

# Copy PGD file
cp  $SRC_SURFEX/MY_RUN/SODA/OUTPUT/PGD_SODA.$suffix .

# Copy OFFLINE PREP file
cp  $SRC_SURFEX/MY_RUN/SODA/OUTPUT/PREP_OFFLINE.$suffix .

# Copy input grib file
cp $SRC_SURFEX/MY_RUN/SODA/INPUT/FG_OI_MAIN .
cp $SRC_SURFEX/MY_RUN/SODA/INPUT/CANARI .
cp $SRC_SURFEX/MY_RUN/SODA/INPUT/SST_SIC .
cp $SRC_SURFEX/MY_RUN/SODA/INPUT/fort.61 .
cp $SRC_SURFEX/MY_RUN/SODA/INPUT/clim_isba .

# Run SODA
[ ! -f $SRC_SURFEX/exe/SODA$XYZ ] && echo "$SRC_SURFEX/exe/SODA$XYZ not found" && exit 1
$SRC_SURFEX/exe/SODA$XYZ

# Save output
mv PREP_OFFLINE.$suffix $SRC_SURFEX/MY_RUN/SODA/OUTPUT/PREP_SODA_OI.$suffix
