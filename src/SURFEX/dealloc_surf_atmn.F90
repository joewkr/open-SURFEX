!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #################################################################################
SUBROUTINE DEALLOC_SURF_ATM_n (YSC)
!     #################################################################################
!
!!****  *DEALLOC_SURF_ATM_n * - Deallocate all arrays
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
USE MODD_CH_EMIS_FIELD_n, ONLY : CH_EMIS_FIELD_INIT
USE MODD_CH_SNAP_n, ONLY : CH_EMIS_SNAP_INIT
USE MODD_CH_SURF_n, ONLY : CH_SURF_INIT
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_INIT
USE MODD_DUMMY_SURF_FIELDS_n, ONLY : DUMMY_SURF_FIELDS_INIT
USE MODD_EMIS_GR_FIELD_n, ONLY : EMIS_GR_FIELD_INIT
USE MODD_CANOPY_n, ONLY : CANOPY_INIT
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_INIT
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_INIT
USE MODD_SSO_n, ONLY : SSO_INIT
USE MODD_SV_n, ONLY : SV_INIT
!
USE MODD_DATA_TSZ0_n, ONLY : DATA_TSZ0_INIT
!
USE MODD_IDEAL_n, ONLY : IDEAL_INIT
!
USE MODD_DST_n, ONLY : DST_NP_INIT
USE MODD_SLT_n, ONLY : SLT_INIT
!
USE MODD_DIAG_n, ONLY : DIAG_INIT, DIAG_NP_INIT, DIAG_OPTIONS_INIT
!
USE MODD_SURFEX_n, ONLY : SURFEX_t
!
USE MODI_DEALLOC_SEA_n
USE MODI_DEALLOC_INLAND_WATER_n
USE MODI_DEALLOC_NATURE_n
USE MODI_DEALLOC_TOWN_n
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
TYPE(SURFEX_t), INTENT(INOUT) :: YSC
!
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('DEALLOC_SURF_ATM_N',0,ZHOOK_HANDLE)
!
CALL DATA_COVER_INIT(YSC%DTCO)
CALL DATA_TSZ0_INIT(YSC%DTZ)
CALL DUMMY_SURF_FIELDS_INIT(YSC%DUU)
  !
CALL SURF_ATM_GRID_INIT(YSC%UG)
CALL DIAG_OPTIONS_INIT(YSC%DUO) 
CALL DIAG_INIT(YSC%DU) 
CALL DIAG_INIT(YSC%DUC)
CALL DIAG_NP_INIT(YSC%DUP,0) 
CALL DIAG_NP_INIT(YSC%DUPC,0)
CALL SSO_INIT(YSC%USS)
CALL CANOPY_INIT(YSC%SB)
  !
CALL DIAG_INIT(YSC%DL)
CALL DIAG_INIT(YSC%DLC)
CALL IDEAL_INIT(YSC%L)
  !
CALL SV_INIT(YSC%SV)
CALL CH_SURF_INIT(YSC%CHU)  
CALL CH_EMIS_FIELD_INIT(YSC%CHE)
CALL CH_EMIS_SNAP_INIT(YSC%CHN)
CALL EMIS_GR_FIELD_INIT(YSC%EGF)  
CALL SLT_INIT(YSC%SLT)
!
CALL DST_NP_INIT(YSC%NDST,0)
!
!-------------------------------------------------------------------------------------
!
IF (YSC%U%NDIM_SEA    >0) CALL DEALLOC_SEA_n(YSC%SM, YSC%U)
!
IF (YSC%U%NDIM_WATER  >0) CALL DEALLOC_INLAND_WATER_n(YSC%WM, YSC%FM, YSC%U)
!
IF (YSC%U%NDIM_NATURE >0) CALL DEALLOC_NATURE_n(YSC%IM, YSC%U)
!
IF (YSC%U%NDIM_TOWN   >0) CALL DEALLOC_TOWN_n(YSC%TM, YSC%GDM, YSC%GRM, YSC%U)
!
!-------------------------------------------------------------------------------------
!
CALL SURF_ATM_INIT(YSC%U)
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DEALLOC_SURF_ATM_N',1,ZHOOK_HANDLE)
!
!--------------------------------------------------------------------------------------
!
END SUBROUTINE DEALLOC_SURF_ATM_n
