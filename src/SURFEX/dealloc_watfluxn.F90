!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #################################################################################
SUBROUTINE DEALLOC_WATFLUX_n (WM)
!     #################################################################################
!
!!****  *DEALLOC_WATFLUX_n * - Deallocate all arrays
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
USE MODD_SURFEX_n, ONLY : WATFLUX_MODEL_t
!
USE MODD_DIAG_n, ONLY : DIAG_INIT, DIAG_OPTIONS_INIT
!
USE MODD_CH_WATFLUX_n, ONLY : CH_WATFLUX_INIT
USE MODD_WATFLUX_n, ONLY : WATFLUX_INIT
!
USE MODD_SFX_GRID_n, ONLY : GRID_INIT
USE MODD_CANOPY_n, ONLY : CANOPY_INIT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
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
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('DEALLOC_WATFLUX_N',0,ZHOOK_HANDLE)
!
CALL DIAG_OPTIONS_INIT(WM%DWO)
CALL DIAG_INIT(WM%DW)
CALL DIAG_INIT(WM%DWC)
!  
CALL GRID_INIT(WM%G)
CALL CANOPY_INIT(WM%SB)  
CALL CH_WATFLUX_INIT(WM%CHW)
CALL WATFLUX_INIT(WM%W)
!
IF (LHOOK) CALL DR_HOOK('DEALLOC_WATFLUX_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DEALLOC_WATFLUX_n


