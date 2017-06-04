!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
MODULE MODD_DST_SURF
!
IMPLICIT NONE
!
REAL, PARAMETER  :: XDENSITY_DST     = 2.5e3    ! [kg/m3] density of dust
REAL, PARAMETER  :: XMOLARWEIGHT_DST = 100.e-3   ! [kg/mol] molar weight dust
!
INTEGER             :: NVEGNO_DST        !Number of vegetation classes considered dust emitters
INTEGER, PARAMETER  :: NEMISMODES_MAX=3
INTEGER, DIMENSION(NEMISMODES_MAX), PARAMETER :: JORDER_DST=(/3,2,1/) !Dust modes in order of importance
!Set emission related parameters
REAL,DIMENSION(NEMISMODES_MAX)   :: XEMISRADIUS_INI_DST          ! number madian radius initialization for sea salt mode (um)
REAL,DIMENSION(NEMISMODES_MAX)   :: XEMISSIG_INI_DST             ! dispersion initialization for sea salt mode
REAL,DIMENSION(NEMISMODES_MAX)   :: XMSS_FRC_SRC_INI             ! Mass fraction from each mode
!
 CHARACTER(LEN=5)   :: CEMISPARAM_DST    ! Reference to paper where emission parameterization is proposed
INTEGER            :: JPMODE_DST        ! number of sea salt modes (max 3; default = 1)
LOGICAL            :: LVARSIG_DST       ! switch to active pronostic dispersion for all modes
LOGICAL            :: LRGFIX_DST        ! switch to active pronostic mean radius for all modes
!
INTEGER            :: NDST_MDEBEG       ! Index of mass flux in first sea salt mode in scalar list
INTEGER            :: NDSTMDE           ! Number of sea salt modes emitted
 CHARACTER(LEN=6)   :: CVERMOD
!
REAL :: XFLX_MSS_FDG_FCT 
!
END MODULE MODD_DST_SURF
