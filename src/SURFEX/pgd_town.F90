!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_TOWN (DTCO, UG, U, USS, DTV, TM, GDM, GRM, HPROGRAM)
!     #############################################################
!
!!****  *PGD_TOWN* - routine to choose initialization of urban scheme
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
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
!
USE MODD_SURFEX_n, ONLY : TEB_MODEL_t
USE MODD_SURFEX_n, ONLY : TEB_GARDEN_MODEL_t
USE MODD_SURFEX_n, ONLY : TEB_GREENROOF_MODEL_t
!
USE MODI_PGD_TEB
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
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTV
TYPE(TEB_MODEL_t), INTENT(INOUT) :: TM
TYPE(TEB_GARDEN_MODEL_t), INTENT(INOUT) :: GDM
TYPE(TEB_GREENROOF_MODEL_t), INTENT(INOUT) :: GRM
!
 CHARACTER(LEN=6), INTENT(IN)  :: HPROGRAM   ! program calling surf. schemes
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
!*       2.     Selection of surface scheme
!               ---------------------------
!
IF (LHOOK) CALL DR_HOOK('PGD_TOWN',0,ZHOOK_HANDLE)
IF (U%CTOWN=='NONE  ') THEN
  IF (LHOOK) CALL DR_HOOK('PGD_TOWN',1,ZHOOK_HANDLE)
  RETURN
ELSE IF (U%CTOWN=='FLUX  ') THEN
  IF (LHOOK) CALL DR_HOOK('PGD_TOWN',1,ZHOOK_HANDLE)
  RETURN
ELSE IF (U%CTOWN=='TEB   ') THEN
  CALL PGD_TEB(DTCO, UG, U, USS, TM%TOP, TM%BOP, TM%G, TM%BDD, TM%DTT, TM%DTB, &
                          GDM%O, GDM%K, GDM%DTV, TM%TIR, &
                          GRM%O, GRM%S, GRM%K, GRM%DTV, HPROGRAM)
END IF
IF (LHOOK) CALL DR_HOOK('PGD_TOWN',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_TOWN
