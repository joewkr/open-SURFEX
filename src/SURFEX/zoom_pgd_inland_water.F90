!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###########################################################
      SUBROUTINE ZOOM_PGD_INLAND_WATER (DTCO, FG, F, UG, U, USS, WG, W, &
                                        HPROGRAM,HINIFILE,HINIFILETYPE, &
                                       HFILE,HFILETYPE,OECOCLIMAP)
!     ###########################################################

!!
!!    PURPOSE
!!    -------
!!   This program prepares the physiographic data fields.
!!
!!    METHOD
!!    ------
!!   
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson                   Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original     13/10/03
!!    B. Decharme  02/2014  Add LRM_RIVER
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
!
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODD_FLAKE_n, ONLY : FLAKE_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
USE MODD_WATFLUX_n, ONLY : WATFLUX_t
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_PGD_FLAKE
USE MODI_PGD_WATFLUX
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(GRID_t), INTENT(INOUT) :: FG
TYPE(FLAKE_t), INTENT(INOUT) :: F
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
TYPE(GRID_t), INTENT(INOUT) :: WG
TYPE(WATFLUX_t), INTENT(INOUT) :: W
!
CHARACTER(LEN=6),     INTENT(IN)  :: HPROGRAM    ! program calling
CHARACTER(LEN=28),    INTENT(IN)  :: HINIFILE    ! input atmospheric file name
CHARACTER(LEN=6),     INTENT(IN)  :: HINIFILETYPE! input atmospheric file type
CHARACTER(LEN=28),    INTENT(IN)  :: HFILE       ! output file name
CHARACTER(LEN=6),     INTENT(IN)  :: HFILETYPE   ! output file type
LOGICAL,              INTENT(IN)  :: OECOCLIMAP
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
LOGICAL :: LRM_RIVER ! dummy keys
!
!------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('ZOOM_PGD_INLAND_WATER',0,ZHOOK_HANDLE)
IF (U%CWATER=='NONE  ') THEN
  IF (LHOOK) CALL DR_HOOK('ZOOM_PGD_INLAND_WATER',1,ZHOOK_HANDLE)
  RETURN
ELSE IF (U%CWATER=='FLUX  ') THEN
  IF (LHOOK) CALL DR_HOOK('ZOOM_PGD_INLAND_WATER',1,ZHOOK_HANDLE)
  RETURN
ELSE IF (U%CWATER=='WATFLX') THEN
  CALL PGD_WATFLUX(DTCO, U, WG, W, HPROGRAM)
ELSE IF (U%CWATER=='FLAKE ') THEN
  LRM_RIVER=.TRUE.
  CALL PGD_FLAKE(DTCO, FG, F, UG, U, USS, HPROGRAM,LRM_RIVER)
END IF
IF (LHOOK) CALL DR_HOOK('ZOOM_PGD_INLAND_WATER',1,ZHOOK_HANDLE)
!
!_______________________________________________________________________________
!
END SUBROUTINE ZOOM_PGD_INLAND_WATER
