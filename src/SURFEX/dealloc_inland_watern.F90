!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE DEALLOC_INLAND_WATER_n ( WM, FM, U)
!     ###############################################################################
!
!!****  *DEALLOC_INLAND_WATER_n * - Deallocate all arrays
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
!
USE MODD_SURFEX_n, ONLY : FLAKE_MODEL_t, WATFLUX_MODEL_t
!
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_DEALLOC_FLAKE_n
!
USE MODI_DEALLOC_IDEAL_FLUX
!
USE MODI_DEALLOC_WATFLUX_n
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
TYPE(WATFLUX_MODEL_t), INTENT(INOUT) :: WM
TYPE(FLAKE_MODEL_t), INTENT(INOUT) :: FM
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('DEALLOC_INLAND_WATER_N',0,ZHOOK_HANDLE)
IF (U%CWATER=='WATFLX') THEN
  CALL DEALLOC_WATFLUX_n(WM)
ELSE IF (U%CWATER=='FLAKE ') THEN
  CALL DEALLOC_FLAKE_n(FM)   
ELSE IF (U%CWATER=='FLUX  ') THEN
  CALL DEALLOC_IDEAL_FLUX
END IF
IF (LHOOK) CALL DR_HOOK('DEALLOC_INLAND_WATER_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DEALLOC_INLAND_WATER_n
