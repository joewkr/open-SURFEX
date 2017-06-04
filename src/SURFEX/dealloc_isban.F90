!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #################################################################################
SUBROUTINE DEALLOC_ISBA_n (IM)
!     #################################################################################
!
!!****  *DEALLOC_ISBA_n * - Deallocate all arrays
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
!!      P Samuelsson 10/2014  MEB
!!------------------------------------------------------------------
!
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_INIT
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_INIT
USE MODD_ISBA_n, ONLY : ISBA_S_INIT, ISBA_K_INIT, ISBA_NK_INIT, ISBA_NP_INIT, ISBA_NPE_INIT
USE MODD_SSO_n, ONLY : SSO_INIT, SSO_NP_INIT
USE MODD_SFX_GRID_n, ONLY : GRID_INIT, GRID_NP_INIT
USE MODD_CH_ISBA_n, ONLY : CH_ISBA_INIT, CH_ISBA_NP_INIT
!
USE MODD_DIAG_n, ONLY : DIAG_INIT, DIAG_NP_INIT, DIAG_OPTIONS_INIT
!
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_INIT, DIAG_EVAP_ISBA_NP_INIT
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_INIT, DIAG_MISC_ISBA_NP_INIT
!
USE MODD_AGRI_n, ONLY : AGRI_NP_INIT
USE MODD_SFX_GRID_n, ONLY : GRID_INIT, GRID_NP_INIT
USE MODD_CANOPY_n, ONLY : CANOPY_INIT
USE MODD_GR_BIOG_n, ONLY : GR_BIOG_INIT, GR_BIOG_NP_INIT
!
USE MODD_SURFEX_n, ONLY : ISBA_MODEL_t
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
TYPE(ISBA_MODEL_t), INTENT(INOUT) :: IM
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('DEALLOC_ISBA_N',0,ZHOOK_HANDLE)
!
CALL DIAG_OPTIONS_INIT(IM%ID%O)  
CALL DIAG_INIT(IM%ID%D)
CALL DIAG_INIT(IM%ID%DC)
CALL DIAG_EVAP_ISBA_INIT(IM%ID%DE)
CALL DIAG_EVAP_ISBA_INIT(IM%ID%DEC)
CALL DIAG_MISC_ISBA_INIT(IM%ID%DM)
!
CALL DIAG_NP_INIT(IM%ID%ND,0)
CALL DIAG_NP_INIT(IM%ID%NDC,0)
CALL DIAG_EVAP_ISBA_NP_INIT(IM%ID%NDE,0)
CALL DIAG_EVAP_ISBA_NP_INIT(IM%ID%NDEC,0) 
CALL DIAG_MISC_ISBA_NP_INIT(IM%ID%NDM,0)
  !
CALL DATA_ISBA_INIT(IM%DTV)
CALL CANOPY_INIT(IM%SB)
CALL ISBA_OPTIONS_INIT(IM%O)
CALL ISBA_S_INIT(IM%S)  
CALL CH_ISBA_INIT(IM%CHI)
CALL GR_BIOG_INIT(IM%GB)
CALL SSO_INIT(IM%ISS)
CALL GRID_INIT(IM%G)
CALL ISBA_K_INIT(IM%K)
!
CALL CH_ISBA_NP_INIT(IM%NCHI,0)
CALL GR_BIOG_NP_INIT(IM%NGB,0)
CALL SSO_NP_INIT(IM%NISS,0)
CALL GRID_NP_INIT(IM%NG,0)
CALL ISBA_NK_INIT(IM%NK,0)
CALL ISBA_NP_INIT(IM%NP,0)
CALL ISBA_NPE_INIT(IM%NPE,0)  
CALL AGRI_NP_INIT(IM%NAG,0)
!
!-------------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('DEALLOC_ISBA_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DEALLOC_ISBA_n
