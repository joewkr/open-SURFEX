#!/bin/bash
#set -x
#------------------------------------------------------------
#
#   preparation of forcing files
#
#   P. Le Moigne 07/2008

#------------------------------------------------------------
# list of available datasets
#
XYZ=${ARCH}${MNH_INT}-${VERSION_XYZ}${VER_MPI:+-${VER_MPI}}${VER_OMP:+-${VER_OMP}}-${OPTLEVEL}-X${VER_XIOS}${VER_USER:+-${VER_USER}}
#
if [ "${1}" == "" ]
then
	echo " > "
	echo " > available experiments are : "
	echo " > "
	echo " > hapex         : dataset on vegetation on SW of France in 1986"
	echo " > me93          : dataset on Mexico city center in december 1993"
	echo " > vl92          : dataset on Vancouver city center in August 1992"
	echo " > ma01          : dataset on Marseille city center in july 2001"
	echo " > cdp9697       : dataset on Col de Porte 1996-1997         "
	echo " > Alp_for_0203  : dataset on forest alpine site 0203   "
	echo " > Alqueva0206   : dataset on lake Alqueva between 2002 and 2006   "
	echo " > safran        : dataset for safran"
	echo " > "
	echo " > use one of the following instructions: "
	echo " > "
	echo " > 2_prepare_files.bash hapex "
	echo " > 2_prepare_files.bash me93 "
	echo " > 2_prepare_files.bash vl92 "
	echo " > 2_prepare_files.bash ma01 "
	echo " > 2_prepare_files.bash cdp9697 "
	echo " > 2_prepare_files.bash Alp_for_0203 "
	echo " > 2_prepare_files.bash Alqueva0206 "
	echo " > 2_prepare_files.bash safran "
	exit
fi
#------------------------------------------------------------
if [ ! -f ../../exe/PRE_INPUT_EXPERIMENT-${XYZ} ]
then
	echo "PRE_INPUT_EXPERIMENT-"${XYZ}
	echo " "
	echo " you need to compile with VER_USER=FORC and to run profile_surfex....FORC... before "
	echo " "
	exit
fi
#------------------------------------------------------------
mkdir ${SRC_SURFEX}/MY_RUN/KTEST/$1    > $HOME/.bidon 2>&1
cp ${SRC_SURFEX}/MY_RUN/NAMELIST/$1/OPTIONS.nam .
cp ${SRC_SURFEX}/MY_RUN/NAMELIST/$1/MY_PARAM.nam .
/bin/vi MY_PARAM.nam
export OMP_NUM_THREADS=1
../../exe/PRE_INPUT_EXPERIMENT-${XYZ}
rm -f fort.1*

outfmt=$(grep YFORCING_FILETYPE MY_PARAM.nam)
outfmt_ascii=$(grep ASCII MY_PARAM.nam| cut -f2 -d"=" | cut -f2 -d"'")
outfmt_netcdf=$(grep NETCDF MY_PARAM.nam| cut -f2 -d"=" | cut -f2 -d"'")
outfmt_binary=$(grep BINARY MY_PARAM.nam| cut -f2 -d"=" | cut -f2 -d"'")

#echo " outfmt        = $outfmt"
#echo " outfmt_ascii  = $outfmt_ascii"
#echo " outfmt_netcdf = $outfmt_netcdf"
#echo " outfmt_binary = $outfmt_binary"

#rm MY_PARAM.nam*

cd ${SRC_SURFEX}/MY_RUN/KTEST/$1

if [ ! "$outfmt" ]
then
	mv ${SRC_SURFEX}/MY_RUN/FORCING/*FORCING.nc .
fi

if [ "$outfmt_ascii" ]
then
	ls -l ${SRC_SURFEX}/MY_RUN/FORCING/*.txt
	mv ${SRC_SURFEX}/MY_RUN/FORCING/*.txt .
fi

if [ "$outfmt_binary" ]
then
	ls -l ${SRC_SURFEX}/MY_RUN/FORCING/*.txt ${SRC_SURFEX}/MY_RUN/FORCING/*.bin
	mv ${SRC_SURFEX}/MY_RUN/FORCING/*.bin .
	mv ${SRC_SURFEX}/MY_RUN/FORCING/*.txt .
fi

if [ "$outfmt_netcdf" ]
then
	ls -l ${SRC_SURFEX}/MY_RUN/FORCING/*.nc
	mv ${SRC_SURFEX}/MY_RUN/FORCING/*FORCING.nc .
	mv ${SRC_SURFEX}/MY_RUN/FORCING/*PARAMS.nc .
	mv ${SRC_SURFEX}/MY_RUN/FORCING/*Forc_*.nc .
fi

if [ "$outfmt" ] && [ ! "$outfmt_netcdf" ] && [ ! "$outfmt_ascii" ] && [ ! "$outfmt_binary" ]
then
	echo "$outfmt NOT ALLOWED, MODIFY MY_PARAM.nam"
	exit
fi

	ln -s ${SRC_SURFEX}/exe/OFFLINE-${XYZ} offline.exe > $HOME/.bidon 2>&1
	ln -s ${SRC_SURFEX}/exe/PGD-${XYZ}     pgd.exe     > $HOME/.bidon 2>&1
	ln -s ${SRC_SURFEX}/exe/PREP-${XYZ}    prep.exe    > $HOME/.bidon 2>&1

	ln -s ${SRC_SURFEX}/MY_RUN/ECOCLIMAP/ecoclimapI_covers_param.bin > $HOME/.bidon 2>&1
	ln -s ${SRC_SURFEX}/MY_RUN/ECOCLIMAP/ecoclimapII_eu_covers_param.bin > $HOME/.bidon 2>&1

	
        if [ -f OPTIONS.nam ]
        then
	      cp ${SRC_SURFEX}/MY_RUN/KTEST/$1/OPTIONS.nam  ${SRC_SURFEX}/MY_RUN/KTEST/$1/OPTIONS.nam_save.$$
        fi
	cp ${SRC_SURFEX}/MY_RUN/NAMELIST/$1/OPTIONS.nam  .
	
	echo "====================================================================================="
	echo " > input files moved to ${SRC_SURFEX}/MY_RUN/KTEST/$1        "
	echo "====================================================================================="


	option1=$(grep CFORCING_FILETYPE OPTIONS.nam | cut -f2 -d "'")
	option2=$(grep YFORCING_FILETYPE ${SRC_SURFEX}/MY_RUN/FORCING/MY_PARAM.nam | cut -f2 -d "'")
	rm ${SRC_SURFEX}/MY_RUN/FORCING/MY_PARAM.nam*
	rm ${SRC_SURFEX}/MY_RUN/FORCING/OPTIONS.nam*

	if [ "$option1" != "$option2" ] 
	then
		echo " "
		echo " "
		echo " "
		echo " "
		echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
		echo " "
		echo " You must set CFORCING_FILETYPE to $option2 in namelist  "
		echo " ${SRC_SURFEX}/MY_RUN/$1/OPTIONS.nam "
		echo " "
		echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
		echo " "
		echo " "
		echo " "
		echo " "
	fi

#------------------------------------------------------------

