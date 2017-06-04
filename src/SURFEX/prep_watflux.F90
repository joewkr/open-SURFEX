!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_WATFLUX (DTCO, UG, U, GCP, WG, W, SB,  &
                         HPROGRAM,HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,YDCTL)
!     #################################################################################
!
!!****  *PREP_WATFLUX* - prepares WATFLUX fields
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
!!      S. Riette   06/2009 PREP_WATFLUX_SBL has no more argument
!!      P. Marguinaud10/2014, Support for a 2-part PREP
!!------------------------------------------------------------------
!
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODD_WATFLUX_n, ONLY : WATFLUX_t
USE MODD_CANOPY_n, ONLY : CANOPY_t
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
!
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
!
USE MODI_PREP_HOR_WATFLUX_FIELD
USE MODI_PREP_VER_WATFLUX
USE MODI_PREP_OUTPUT_GRID
USE MODI_GET_LUOUT
USE MODI_PREP_SBL
!
USE MODD_READ_NAMELIST,  ONLY : LNAM_READ
USE MODN_PREP_WATFLUX
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
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
!
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
 TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
TYPE(GRID_t), INTENT(INOUT) :: WG
TYPE(WATFLUX_t), INTENT(INOUT) :: W
TYPE(CANOPY_t), INTENT(INOUT) :: SB
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
INTEGER :: JMTH, INMTH
!
INTEGER :: ILUOUT
LOGICAL :: GFOUND         ! Return code when searching namelist
INTEGER :: ILUNAM         ! logical unit of namelist file
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
!*      1.     Default of configuration
!
!
IF (LHOOK) CALL DR_HOOK('PREP_WATFLUX',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
 CALL PREP_OUTPUT_GRID(UG%G, WG, U%NSIZE_FULL, ILUOUT)
!
!-------------------------------------------------------------------------------------
!
!*      2.     Reading and horizontal interpolations
!
!
!*      2.0    Large scale orography
!
 CALL PREP_HOR_WATFLUX_FIELD(DTCO, U, GCP, SIZE(WG%XLAT), W, &
                             HPROGRAM,'ZS     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,YDCTL)
!
!*      2.1    Temperature
!
 CALL PREP_HOR_WATFLUX_FIELD(DTCO, U, GCP, SIZE(WG%XLAT), W, &
                             HPROGRAM,'TSWATER',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,YDCTL)
!
 CALL CLEAN_PREP_OUTPUT_GRID
!
IF (YDCTL%LPART6) THEN

!*      2.2    Roughness
!
  ALLOCATE(W%XZ0(SIZE(W%XTS)))
  W%XZ0 = 0.001
!
!-------------------------------------------------------------------------------------
!
!*      3.     Vertical interpolations of all variables
!
  IF(LVERTSHIFT)THEN
    CALL PREP_VER_WATFLUX(W)
  ENDIF
!
  DEALLOCATE(XZS_LS)
!-------------------------------------------------------------------------------------
!
!*      4.     Preparation of optional interpolation of monthly ts water
!
  W%LINTERPOL_TS=.FALSE.
  IF(W%CINTERPOL_TS/='NONE  ')THEN
    W%LINTERPOL_TS=.TRUE.
  ENDIF
!
  IF(W%LINTERPOL_TS)THEN
!
! Precedent, Current, Next, and Second-next Monthly TS water
    INMTH=4
!
    ALLOCATE(W%XTS_MTH(SIZE(W%XTS),INMTH))
    DO JMTH=1,INMTH
      W%XTS_MTH(:,JMTH)=W%XTS(:)
    ENDDO
!
  ENDIF
!
!-------------------------------------------------------------------------------------
!
!*      5.     Preparation of SBL air variables
!
  W%LSBL = LWAT_SBL
  IF (W%LSBL) CALL PREP_SBL(WG%NDIM, SB)
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('PREP_WATFLUX',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_WATFLUX
