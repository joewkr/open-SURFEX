!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_INLAND_WATER (DTCO, FG, F, UG, U, USS, WG, W, HPROGRAM,ORM_RIVER)
!     #############################################################
!
!!****  *PGD_INLAND_WATER* - routine to choose initialization of lake scheme
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    03/2004
!!     B. Decharme  02/2014  Add LRM_RIVER
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
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
USE MODI_PGD_WATFLUX
USE MODI_PGD_FLAKE
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
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
CHARACTER(LEN=6),    INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
LOGICAL,             INTENT(IN)  :: ORM_RIVER ! delete river coverage (default = false)
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*       2.     Selection of surface scheme
!               ---------------------------
!
IF (LHOOK) CALL DR_HOOK('PGD_INLAND_WATER',0,ZHOOK_HANDLE)
!
IF (U%CWATER=='NONE  ') THEN
  IF (LHOOK) CALL DR_HOOK('PGD_INLAND_WATER',1,ZHOOK_HANDLE)
  RETURN
ELSE IF (U%CWATER=='FLUX  ') THEN
  IF (LHOOK) CALL DR_HOOK('PGD_INLAND_WATER',1,ZHOOK_HANDLE)
  RETURN
ELSE IF (U%CWATER=='WATFLX') THEN
  CALL PGD_WATFLUX(DTCO, U, WG, W, HPROGRAM)
ELSE IF (U%CWATER=='FLAKE ') THEN
  CALL PGD_FLAKE(DTCO, FG, F, UG, U, USS, HPROGRAM,ORM_RIVER)
END IF
!
IF (LHOOK) CALL DR_HOOK('PGD_INLAND_WATER',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_INLAND_WATER
