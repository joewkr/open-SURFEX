!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #################################################################################
SUBROUTINE DEALLOC_FLAKE_n (FM)
!     #################################################################################
!
!!****  *DEALLOC_FLAKE_n * - Deallocate all arrays
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
!!      Modified    04/2013, P. Le Moigne: FLake chemistry and XZ0
!!------------------------------------------------------------------
!
USE MODD_SURFEX_n, ONLY : FLAKE_MODEL_t
!
USE MODD_CH_FLAKE_n, ONLY : CH_FLAKE_INIT
USE MODD_FLAKE_n, ONLY : FLAKE_INIT
!
USE MODD_SFX_GRID_n, ONLY : GRID_INIT
USE MODD_CANOPY_n, ONLY : CANOPY_INIT
!
USE MODD_DIAG_n, ONLY : DIAG_INIT, DIAG_OPTIONS_INIT
USE MODD_DIAG_MISC_FLAKE_n, ONLY : DIAG_MISC_FLAKE_INIT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(FLAKE_MODEL_t), INTENT(INOUT) :: FM
!
!*      0.2    declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DEALLOC_FLAKE_N',0,ZHOOK_HANDLE)
!
CALL DIAG_OPTIONS_INIT(FM%DFO)
CALL DIAG_INIT(FM%DF)
CALL DIAG_INIT(FM%DFC)
CALL DIAG_MISC_FLAKE_INIT(FM%DMF)
! 
CALL GRID_INIT(FM%G)  
CALL CANOPY_INIT(FM%SB)  
CALL CH_FLAKE_INIT(FM%CHF)
CALL FLAKE_INIT(FM%F)
!
IF (LHOOK) CALL DR_HOOK('DEALLOC_FLAKE_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DEALLOC_FLAKE_n


