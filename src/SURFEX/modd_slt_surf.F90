!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
MODULE MODD_SLT_SURF
!
IMPLICIT NONE
!
REAL, PARAMETER  :: XDENSITY_SLT     = 2.1e3    ! [kg/m3] density of sea salt
REAL, PARAMETER  :: XMOLARWEIGHT_SLT = 58.e-3   ! [kg/mol] molar weight sea salt
!
INTEGER, PARAMETER  :: NEMISMODES_MAX=3
INTEGER, DIMENSION(NEMISMODES_MAX), PARAMETER :: JORDER_SLT=(/3,2,1/) !Dust modes in order of importance
!Set emission related parameters
REAL,DIMENSION(NEMISMODES_MAX)   :: XEMISRADIUS_INI_SLT          ! number madian radius initialization for sea salt mode (um)
REAL,DIMENSION(NEMISMODES_MAX)   :: XEMISSIG_INI_SLT             ! dispersion initialization for sea salt mode
!
 CHARACTER(LEN=5)   :: CEMISPARAM_SLT    ! Reference to paper where emission parameterization is proposed
INTEGER            :: JPMODE_SLT        ! number of sea salt modes (max 3; default = 1)
LOGICAL            :: LVARSIG_SLT       ! switch to active pronostic dispersion for all modes
LOGICAL            :: LRGFIX_SLT        ! switch to active pronostic mean radius for all modes
!
INTEGER            :: NSLT_MDEBEG       ! Index of mass flux in first sea salt mode in scalar list
INTEGER            :: NSLTMDE           ! Number of sea salt modes emitted
!
END MODULE MODD_SLT_SURF
