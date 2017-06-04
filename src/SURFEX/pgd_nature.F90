!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_NATURE (DTCO, DTZ, IM, UG, U, USS, HPROGRAM)
!     #############################################################
!
!!****  *PGD_NATURE* - routine to choose initialization of vegetation scheme
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
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DATA_TSZ0_n, ONLY : DATA_TSZ0_t
USE MODD_SURFEX_n, ONLY : ISBA_MODEL_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODI_PGD_ISBA
USE MODI_PGD_TSZ0_PAR
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DATA_TSZ0_t), INTENT(INOUT) :: DTZ
TYPE(ISBA_MODEL_t), INTENT(INOUT) :: IM
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
!
 REAL, DIMENSION(:,:), POINTER :: PVEGTYPE
!
 CHARACTER(LEN=6), INTENT(IN)  :: HPROGRAM   ! program calling surf. schemes
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
!*       2.     Selection of surface scheme
!               ---------------------------
!
IF (LHOOK) CALL DR_HOOK('PGD_NATURE',0,ZHOOK_HANDLE)
IF (U%CNATURE=='NONE  ') THEN
  IF (LHOOK) CALL DR_HOOK('PGD_NATURE',1,ZHOOK_HANDLE)
  RETURN
ELSE IF (U%CNATURE=='FLUX  ') THEN
  IF (LHOOK) CALL DR_HOOK('PGD_NATURE',1,ZHOOK_HANDLE)
  RETURN
ELSE IF (U%CNATURE=='ISBA  ' .OR. U%CNATURE=='TSZ0') THEN
  CALL PGD_ISBA(DTCO, IM%DTV, IM%G, IM%O, IM%S, IM%K, IM%ISS, UG, U, USS, HPROGRAM)
  IF (U%CNATURE=='TSZ0') CALL PGD_TSZ0_PAR(DTZ, HPROGRAM)
END IF
IF (LHOOK) CALL DR_HOOK('PGD_NATURE',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_NATURE
