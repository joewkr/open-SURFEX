#!/bin/bash

#
# TEST of SODA
#

################################
### USER DEPENDENT VARIABLE ####
################################
CLIMDATA=/disk1/climdata/PGD/
CSURF_FILETYPE=ASCII
################################
################################
################################

if [ $# -ne 1 -a $# -ne 2 ]; then
  echo "Usage: $0 EXP [mode]"
  echo "mode=0 No PGD or PREP"
  echo "mode=1 PGD and PREP"
  echo "mode=2 Only PGD"
  echo "mode=3 Only PREP"
  exit 1
else
  exp=$1
  # Some sanity checks
  if [ "$SRC_SURFEX" == "" -o "$XYZ" == "" ]; then
    echo "You need to source the config file before calling this script!"
    exit 1
  fi
  script="$SRC_SURFEX/MY_RUN/SODA/TESTS/$exp.sh"
  if [ ! -f $script ]; then
    echo "Invalid test experiment $exp"
    echo "$script not found"
    exit 1
  fi
  mode=1
  if [ $# -eq 2 ]; then
    mode=$2
    case $mode in 
      0|1|2|3)
      ;;
      *)
       echo "Invalid mode $mode"
       echo "mode=0 No PGD or PREP"
       echo "mode=1 PGD and PREP"
       echo "mode=2 Only PGD"
       echo "mode=3 Only PREP"
       exit 1
      ;;
    esac
  fi
  # Some sanity checks
  if [ "$SRC_SURFEX" == "" -o "$XYZ" == "" ]; then
    echo "You need to source the config file before calling this script!"
    exit 1
  fi
  suffix=""
  case $CSURF_FILETYPE in
    "ASCII")
       suffix="txt"
    ;;
    "LFI")
       suffix="lfi"
    ;;
    *)
      echo "Filetype is not defined: $CSURF_FILETYPE"
      exit 1
    ;; 
  esac
fi


#
# PGD
#
case $mode in 
  1|2)
    work=$SRC_SURFEX/MY_RUN/SODA/RUN/RUN_PGD
    [ -d $work ] && rm -r $work
    mkdir $work
    cd $work || exit 1

    # Copy namelists
    cp $SRC_SURFEX/MY_RUN/SODA/NAMELISTS/OPTIONS.nam_pgd_$suffix OPTIONS.nam

    #  Databases (SYSTEM DEPENDENT)
    if [ ! -d $CLIMDATA ]; then
      echo "You have to set your path for the climate databases correct!"
      exit 1
    else
      ln -sf $CLIMDATA/* .
    fi
    # Ecoclimap
    ln -sf $SRC_SURFEX/MY_RUN/ECOCLIMAP/*.bin .
    ln -sf $SRC_SURFEX/MY_RUN/ECOCLIMAP/*.dat .


    # Make PGD
    [ ! -f $SRC_SURFEX/exe/PGD$XYZ ] && echo "$SRC_SURFEX/exe/PGD$XYZ not found" && exit 1
    $SRC_SURFEX/exe/PGD$XYZ
 
    if [ -f PGD_SODA.$suffix ]; then
      [ -d $SRC_SURFEX/MY_RUN/SODA/OUTPUT ] || mkdir $SRC_SURFEX/MY_RUN/SODA/OUTPUT
      mv PGD_SODA.$suffix $SRC_SURFEX/MY_RUN/SODA/OUTPUT/PGD_SODA.$suffix
    else
      echo "File PGD_SODA.$suffix does not exist!"
      exit 1
    fi
esac

#
# PREP
#
case $mode in
  1|3)
    work=$SRC_SURFEX/MY_RUN/SODA/RUN/RUN_PREP
    [ -d $work ] && rm -r $work
    mkdir $work
    cd $work || exit 1

    # Copy namelists
    cp $SRC_SURFEX/MY_RUN/SODA/NAMELISTS/OPTIONS.nam_prep_$suffix OPTIONS.nam

    # Ecoclimap
    ln -sf $SRC_SURFEX/MY_RUN/ECOCLIMAP/*.bin .
    ln -sf $SRC_SURFEX/MY_RUN/ECOCLIMAP/*.dat .

    # Copy PGD file
    [ -d $SRC_SURFEX/MY_RUN/SODA/OUTPUT ] || mkdir $SRC_SURFEX/MY_RUN/SODA/OUTPUT
    cp  $SRC_SURFEX/MY_RUN/SODA/OUTPUT/PGD_SODA.$suffix .

    # Copy input grib file
    cp $SRC_SURFEX/MY_RUN/SODA/INPUT/ECMWF_GRIB_FILE .

    # Make PREP
    [ ! -f $SRC_SURFEX/exe/PREP$XYZ ] && echo "$SRC_SURFEX/exe/PREP$XYZ not found" && exit 1
    $SRC_SURFEX/exe/PREP$XYZ

    if [ -f PREP_SODA.$suffix ]; then
      mv PREP_SODA.$suffix $SRC_SURFEX/MY_RUN/SODA/OUTPUT/PREP_SODA.$suffix
    else
      echo "File PREP_SODA.$suffix does not exist!"
      exit 1
    fi
esac

# Run SODA test (With single or multiple offline runs)
if [ -f $script ]; then
  $script $suffix || exit 1
else
  echo "Script $script for test $exp not found"
  exit 1
fi

