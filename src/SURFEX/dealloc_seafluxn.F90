!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #################################################################################
SUBROUTINE DEALLOC_SEAFLUX_n (SM)
!     #################################################################################
!
!!****  *DEALLOC_SEAFLUX_n * - Deallocate all arrays
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
!!      S. Belamari 03/2014   other _SEA_ variables
!!      S. Senesi   09/2013   introduce sea-ice-cover ans sea-surface salinity
!!------------------------------------------------------------------
!
USE MODD_DIAG_n, ONLY : DIAG_INIT, DIAG_OPTIONS_INIT
USE MODD_DIAG_OCEAN_n, ONLY : DIAG_OCEAN_INIT
USE MODD_DIAG_MISC_SEAICE_n, ONLY : DIAG_MISC_SEAICE_INIT
!
USE MODD_CH_SEAFLUX_n, ONLY : CH_SEAFLUX_INIT
USE MODD_DATA_SEAFLUX_n, ONLY : DATA_SEAFLUX_INIT
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_INIT
USE MODD_OCEAN_n, ONLY : OCEAN_INIT
USE MODD_OCEAN_REL_n, ONLY : OCEAN_REL_INIT
!
USE MODD_SFX_GRID_n, ONLY : GRID_INIT
USE MODD_CANOPY_n, ONLY : CANOPY_INIT
!
USE MODD_SURFEX_n, ONLY : SEAFLUX_MODEL_t
!
USE MODI_GLTOOLS_DEALLOC
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
!
TYPE(SEAFLUX_MODEL_t), INTENT(INOUT) :: SM
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DEALLOC_SEAFLUX_N',0,ZHOOK_HANDLE)
!
CALL DIAG_OPTIONS_INIT(SM%SD%O) 
CALL DIAG_INIT(SM%SD%D)
CALL DIAG_INIT(SM%SD%DC)
CALL DIAG_INIT(SM%SD%DI)
CALL DIAG_INIT(SM%SD%DIC)
CALL DIAG_OCEAN_INIT(SM%SD%GO)  
CALL DIAG_MISC_SEAICE_INIT(SM%SD%DMI)
  !  
CALL DATA_SEAFLUX_INIT(SM%DTS)
CALL GRID_INIT(SM%G)
CALL CANOPY_INIT(SM%SB)  
CALL CH_SEAFLUX_INIT(SM%CHS)
CALL SEAFLUX_INIT(SM%S)
CALL SEAFLUX_INIT(SM%S)  
CALL OCEAN_INIT(SM%O)
CALL OCEAN_REL_INIT(SM%OR)
!
!-------------------------------------------------------------------------------------
!
IF (ASSOCIATED(SM%S%TGLT%bat) .AND. SM%S%CSEAICE_SCHEME=='GELATO' ) CALL GLTOOLS_DEALLOC(SM%S%TGLT)
!
IF (LHOOK) CALL DR_HOOK('DEALLOC_SEAFLUX_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DEALLOC_SEAFLUX_n


