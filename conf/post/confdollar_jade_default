#!/bin/sh
#MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
#MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
#MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
#MNH_LIC for details. version 1.
##
## Run Parameter
##

export NBP=${NBP-"1"}
export TIME=${TIME-"3600"}
export MNH_EXP=${MNH_EXP-"MNH_EXP"}

export MPIRUN=${MPIRUN-"mpiexec_mpt -np ${NBP} "}
export MONORUN=${MONORUN-"mpiexec_mpt -np 1 "}
export CAT=${CAT-"cat"}

##
## exection directory
##

export SUBDIR=${SUBDIR-"${PWD}/${CONFEXP}"}
export EXECDIR=${EXECDIR-"/scratch/${USER}/${CONFEXP}"}

##
## transfert protocole
##

export LINKFILES=${LINKFILES-"ln -sf "}

export INDIR=${INDIR-"INDIR"}
export OUTDIR=${OUTDIR-"${INDIR}"}

case "${INHOST}" in 

    "" ) # default local transfert
    export GETFILES=${GETFILES-"ln -s "}
    export RMINDIR=${RMINDIR-"${INDIR}"}
    ;;
    
    *'@'*) # ssh transfert
    export GETFILES=${GETFILES-"scp"}
    export RMINDIR=${RMINDIR-"${INHOST}:${INDIR}"}
    ;;

    workdir) # get file form  $workdir
    export GETFILES=${GETFILES-"ln -s "}
    export INDIR="${WORKDIR}/${INDIR}"
    export RMINDIR="${INDIR}"
    ;;


esac

export OUTHOST=${OUTHOST-"${INHOST}"}

case "${OUTHOST}" in 
    
    "" ) # local transfert
    export PUTFILES=${PUTFILES-"mv "}  
    export RMMKDIR=${RMMKDIR-"mkdir -p "}
    export RMOUTDIR=${RMOUTDIR-"${OUTDIR}"}
    ;;
    
    *'@'*) # ssh transfert
    export PUTFILES=${PUTFILES-"scp"} 
    export RMMKDIR=${RMMKDIR-"ssh ${OUTHOST} mkdir -p "}
    export RMOUTDIR=${RMOUTDIR-"${OUTHOST}:${OUTDIR}"}
    ;;

    workdir) # put files in $workdir
    export PUTFILES=${PUTFILES-"cp "}
    export RMMKDIR=${RMMKDIR-"mkdir -p "}
    export OUTDIR="${WORKDIR}/${OUTDIR}"
    export RMOUTDIR="${OUTDIR}"
    ;;

esac

#export RMSHELL=${RMSHELL-"exec sh -c "}
export RMSHELL=${RMSHELL-"ssh -n service1 "}
export QSUB=${QSUB-"/opt/pbs/default/bin/qsub"}

##
## Job Header
##

export CORE=${CORE-8}
export NCPUS=${CORE} MPIPROCS=${CORE}
export NBNODES=$( echo " scale=0 ; 1 + ( ${NBP} - 1 ) / ${NCPUS} " | bc -l )

export JOBOUT=${JOBOUT-"JOBOUT.$$"}
export JOBNAME=${JOBNAME-"job_${CONFEXP}"}

export JOBMULTI="\
#PBS -S /bin/bash
#PBS -N  R`basename  $PWD | cut -c -14 `
##PBS -o ${JOBOUT} 
# concatene la sortie standard avec l erreur standard
#PBS -j oe 
# réservation de ${NBP} processeurs 
#PBS -l select=${NBNODES}:ncpus=${NCPUS}:mpiprocs=${MPIPROCS} 
#PBS -l place=scatter:excl
#PBS -l walltime=${TIME}
#PBS -W sandbox=PRIVATE
"

export JOBMONO="\
#PBS -S /bin/bash
#PBS -N  R`basename  $PWD | cut -c -14 `
##PBS -o ${JOBOUT}
# concatene la sortie standard avec l erreur standard
#PBS -j oe
# réservation de ${NBP} processeurs
#PBS -l select=${NBNODES}:ncpus=${NCPUS}:mpiprocs=${MPIPROCS}
#PBS -l place=scatter:excl
#PBS -l walltime=${TIME}
#PBS -W sandbox=PRIVATE
"

export JOBSTAT=${JOBSTAT-"/usr/pbs/bin/qstat -f \${PBS_JOBID} | grep resources_used"}

##
## Default Name of input/output files parameters ...
##

## PrepPgd

export PREP_PGD_FILES=${PREP_PGD_FILES-"${HOME}/PREP_PGD_FILES_WWW"}

export OUT_CPGDFILE=${OUT_CPGDFILE-"OUT_CPGDFILE"}
export INP_CPGDFILE_FATHER=${INP_CPGDFILE_FATHER-"INP_CPGDFILE_FATHER"}

## PrepNest

export INP_YPGD1=${INP_YPGD1-"INP_YPGD1"}
export INP_YPGD2=${INP_YPGD2-"INP_YPGD2"}
export INP_YPGD3=${INP_YPGD3-"INP_YPGD3"}
export INP_YPGD4=${INP_YPGD4-"INP_YPGD4"}
export LISTGET=${LISTGET-"LISTGET"}
export CRT_YNEST=${CRT_YNEST-"CRT_YNEST"}
export OUT_YPGD1_NEST=${OUT_YPGD1_NEST-"OUT_YPGD1_NEST"}
export OUT_YPGD2_NEST=${OUT_YPGD2_NEST-"OUT_YPGD2_NEST"}
export LISTE_PUT=${LISTE_PUT-"LISTE_PUT"}

## PrepReal

export INDIR_HATMFILE=${INDIR_HATMFILE-"${RMINDIR}"}
export INP_HATMFILE=${INP_HATMFILE-"INP_HATMFILE"}
export SUF=${SUF-"SUF"}
export INP_HPGDFILE=${INP_HPGDFILE-"INP_HPGDFILE"}
export INP_CFILE=${INP_CFILE-"INP_CFILE"}
export OUT_CINIFILE=${OUT_CINIFILE-"OUT_CINIFILE"}

## Spawning

export INP_YDOMAIN=${INP_YDOMAIN-"INP_YDOMAIN"}
export INP_CINIFILE=${INP_CINIFILE-"INP_CINIFILE"}
export OUT_CINIFILE_SPA=${OUT_CINIFILE_SPA-"OUT_CINIFILE_SPA"}

## Mesonh

export INP_CINIFILE1=${INP_CINIFILE1-"INP_CINIFILE1"}
export INP_CINIFILE2=${INP_CINIFILE2-"INP_CINIFILE2"}
export CRT_CEXP=${CRT_CEXP-"CRT_CEXP"}
export CRT_CSEG=${CRT_CSEG-"CRT_CSEG"}
export OUT_XFMOUT=${OUT_XFMOUT-"OUT_XFMOUT"}

## Diag

export INP_YINIFILE=${INP_YINIFILE-"INP_YINIFILE"}
export CRT_YSUFFIX=${CRT_YSUFFIX-"CRT_YSUFFIX"}
export OUT_DIAG=${OUT_DIAG-"OUT_DIAG"}

## Conv2dia

export CRT_CVYSUFFIX=${CRT_CVYSUFFIX-"CRT_CVYSUFFIX"}
export OUT_CVFILE=${OUT_CVFILE-"OUT_CVFILE"}

## Diaprog

export INP_FILE1=${INP_FILE1-"INP_FILE1"}
export NOVISU==${NOVISU=-"!"}
export OUT_GMFILE=${OUT_GMFILE-"OUT_GMFILE"}

