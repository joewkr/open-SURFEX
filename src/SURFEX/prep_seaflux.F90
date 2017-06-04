!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_SEAFLUX (DTCO, UG, U, GCP, SG, SB, S, DTS, O, OR, &
                        HPROGRAM,HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,YDCTL)
!     #################################################################################
!
!!****  *PREP_SEAFLUX* - prepares variables for SEAFLUX scheme
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     S. Malardel 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      S. Riette   06/2009 PREP_SEAFLUX_SBL has no more argument
!!      Modified    07/2012, P. Le Moigne : CMO1D phasing
!!      Modified    01/2014, S. Senesi : introduce sea-ice model 
!!      Modified    01/2015, R. Séférian : introduce ocean surface albedo 
!!      P. Marguinaud10/2014, Support for a 2-part PREP
!!------------------------------------------------------------------
!
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODD_CANOPY_n, ONLY : CANOPY_t
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
USE MODD_DATA_SEAFLUX_n, ONLY : DATA_SEAFLUX_t
USE MODD_OCEAN_n, ONLY : OCEAN_t
USE MODD_OCEAN_REL_n, ONLY : OCEAN_REL_t
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
!
USE MODI_PREP_HOR_SEAFLUX_FIELD
USE MODI_PREP_VER_SEAFLUX
USE MODI_PREP_OUTPUT_GRID
USE MODI_PREP_SBL
USE MODI_PREP_SEAICE
USE MODI_GET_LUOUT
!
USE MODN_PREP_SEAFLUX
USE MODD_READ_NAMELIST,  ONLY : LNAM_READ
USE MODD_PREP,           ONLY : XZS_LS
USE MODD_SURF_ATM,       ONLY : LVERTSHIFT
!
USE MODE_PREP_CTL, ONLY : PREP_CTL
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_CLEAN_PREP_OUTPUT_GRID
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
TYPE(GRID_t), INTENT(INOUT) :: SG
TYPE(CANOPY_t), INTENT(INOUT) :: SB
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
TYPE(DATA_SEAFLUX_t), INTENT(INOUT) :: DTS
TYPE(OCEAN_t), INTENT(INOUT) :: O
TYPE(OCEAN_REL_t), INTENT(INOUT) :: OR
TYPE (PREP_CTL),    INTENT(INOUT) :: YDCTL
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=28),  INTENT(IN)  :: HATMFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6),   INTENT(IN)  :: HATMFILETYPE! type of the Atmospheric file
 CHARACTER(LEN=28),  INTENT(IN)  :: HPGDFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6),   INTENT(IN)  :: HPGDFILETYPE! type of the Atmospheric file
!
!*      0.2    declarations of local variables
!
INTEGER :: JMTH,INMTH
INTEGER :: ILUOUT
LOGICAL :: GFOUND         ! Return code when searching namelist
INTEGER :: ILUNAM         ! logical unit of namelist file
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!-------------------------------------------------------------------------------------
!
!*      0.     Default of configuration
!
!
IF (LHOOK) CALL DR_HOOK('PREP_SEAFLUX',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
 CALL PREP_OUTPUT_GRID(UG%G, SG, U%NSIZE_FULL, ILUOUT)
!
!-------------------------------------------------------------------------------------
!
!*      1.     Read namelist
!
S%LSBL = LSEA_SBL
!
O%LMERCATOR = LOCEAN_MERCATOR
O%LCURRENT  = LOCEAN_CURRENT
!
! Relaxation-forcing parameters
OR%XTAU_REL   = XTIME_REL
OR%XQCORR     = XCORFLX
!
OR%LREL_CUR   = LCUR_REL
OR%LREL_TS    = LTS_REL
OR%LFLUX_NULL = LZERO_FLUX
OR%LFLX_CORR  = LCORR_FLUX
OR%LDIAPYCNAL = LDIAPYC
!
!-------------------------------------------------------------------------------------
!
!*      2.     Reading and horizontal interpolations
!
!
!*      2.0    Large scale orography
!
CALL PREP_HOR_SEAFLUX_FIELD(DTCO, UG, U, GCP, DTS, O, OR, SIZE(SG%XLAT), S, &
                            HPROGRAM,'ZS     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,YDCTL)
!
!*      2.1.1    Temperature
!
CALL PREP_HOR_SEAFLUX_FIELD(DTCO, UG, U, GCP, DTS, O, OR, SIZE(SG%XLAT), S, &
                            HPROGRAM,'SST    ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,YDCTL)
!
!*      2.1.2    Salinity
!

CALL PREP_HOR_SEAFLUX_FIELD(DTCO, UG, U, GCP, DTS, O, OR, SIZE(SG%XLAT), S, &
                            HPROGRAM,'SSS    ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,YDCTL)
!
!*      2.1.3   Sea-ice
!
IF (CSEAICE_SCHEME /= 'NONE  ') THEN 
   CALL PREP_SEAICE(UG, DTCO, DTS, O, OR, SIZE(SG%XLAT), S, U, GCP, &
                    HPROGRAM,HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,YDCTL)
ENDIF
!
 CALL CLEAN_PREP_OUTPUT_GRID

IF (YDCTL%LPART6) THEN
!
!*      2.2    Roughness
!
  ALLOCATE(S%XZ0(SIZE(S%XSST)))
  S%XZ0 = 0.001
!
  ALLOCATE(S%XZ0H(SIZE(S%XSST)))
  S%XZ0H = S%XZ0
!
!*      2.3   Ocean Surface Albedo
!
  IF(S%CSEA_ALB=='RS14')THEN
    ALLOCATE(S%XDIR_ALB(SIZE(S%XSST)))
    ALLOCATE(S%XSCA_ALB(SIZE(S%XSST)))
    S%XDIR_ALB = 0.065
    S%XSCA_ALB = 0.065
  ENDIF
!
!-------------------------------------------------------------------------------------
!
!*      3.     Vertical interpolations of all variables
!
  IF(LVERTSHIFT)THEN
    CALL PREP_VER_SEAFLUX(S)
  ENDIF
!
  DEALLOCATE(XZS_LS)
!
!-------------------------------------------------------------------------------------
!
!*      4.     Preparation of optional interpolation of monthly sst
!
  S%LINTERPOL_SST=.FALSE.
  IF(TRIM(S%CINTERPOL_SST)/='NONE')THEN
!
    S%LINTERPOL_SST=.TRUE.
!
! Precedent, Current, Next, and Second-next Monthly SST
    INMTH=4
!
    ALLOCATE(S%XSST_MTH(SIZE(S%XSST),INMTH))
    DO JMTH=1,INMTH
      S%XSST_MTH(:,JMTH)=S%XSST(:)
    ENDDO
!
  ENDIF
!
!-------------------------------------------------------------------------------------
!
!
!*      5.     Optional preparation of interpolation of monthly Sea Surface salinity
!
  S%LINTERPOL_SSS=.FALSE.
  IF(TRIM(S%CINTERPOL_SSS)/='NONE')THEN
!
     S%LINTERPOL_SSS=.TRUE.
   !
   ! Precedent, Current, Next, and Second-next Monthly SSS
     INMTH=4
   !
     ALLOCATE(S%XSSS_MTH(SIZE(S%XSSS),INMTH))
     DO JMTH=1,INMTH
       S%XSSS_MTH(:,JMTH)=S%XSSS(:)
     ENDDO
   !
  ENDIF
!
!-------------------------------------------------------------------------------------
!
!*      6.     Preparation of SBL air variables
!
!
  IF (S%LSBL) CALL PREP_SBL(SG%NDIM, SB)
!
ENDIF
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_SEAFLUX',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_SEAFLUX
