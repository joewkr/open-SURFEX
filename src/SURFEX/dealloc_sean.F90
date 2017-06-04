!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE DEALLOC_SEA_n (SM, U)
!     ###############################################################################
!
!!****  *DEALLOC_SEA_n * - Deallocate all arrays
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
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!------------------------------------------------------------------
!
USE MODD_SURFEX_n, ONLY : SEAFLUX_MODEL_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_DEALLOC_IDEAL_FLUX
!
USE MODI_DEALLOC_SEAFLUX_n
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!*      0.2    declarations of local variables
!
!-------------------------------------------------------------------------------------
!
TYPE(SEAFLUX_MODEL_t), INTENT(INOUT) :: SM
!
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('DEALLOC_SEA_N',0,ZHOOK_HANDLE)
IF (U%CSEA=='SEAFLX') THEN
  CALL DEALLOC_SEAFLUX_n(SM)
ELSE IF (U%CSEA=='FLUX  ') THEN
  CALL DEALLOC_IDEAL_FLUX
END IF
IF (LHOOK) CALL DR_HOOK('DEALLOC_SEA_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DEALLOC_SEA_n
