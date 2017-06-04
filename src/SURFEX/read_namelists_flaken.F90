!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE READ_NAMELISTS_FLAKE_n (FM, HPROGRAM, HINIT)
!     #######################################################
!
!---------------------------    
!
!
USE MODD_SURFEX_n, ONLY : FLAKE_MODEL_t
!
USE MODN_FLAKE_n
!
USE MODI_DEFAULT_FLAKE
USE MODI_DEFAULT_CH_DEP
USE MODI_DEFAULT_DIAG_FLAKE
USE MODI_READ_DEFAULT_FLAKE_n
USE MODI_READ_FLAKE_CONF_n
!
USE MODI_READ_NAM_PREP_FLAKE_n
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
TYPE(FLAKE_MODEL_t), INTENT(INOUT) :: FM
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=3),   INTENT(IN)  :: HINIT     ! choice of fields to initialize
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!--------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_NAMELISTS_FLAKE_N',0,ZHOOK_HANDLE)
 CALL DEFAULT_FLAKE(XTSTEP,XOUT_TSTEP,LSEDIMENTS,CSNOW_FLK,CFLK_FLUX,CFLK_ALB,&
                   LSKINTEMP)
!
 CALL DEFAULT_CH_DEP(CCH_DRY_DEP)
!
 CALL DEFAULT_DIAG_FLAKE(N2M,LSURF_BUDGET,L2M_MIN_ZS,LRAD_BUDGET,LCOEF,LSURF_VARS, &
                         LWATER_PROFILE,LSURF_BUDGETC,LRESET_BUDGETC,XDIAG_TSTEP,  &
                         XZWAT_PROFILE             )  
!
 CALL READ_DEFAULT_FLAKE_n(FM%CHF, FM%DFO, FM%DMF, FM%F, HPROGRAM)
!
 CALL READ_FLAKE_CONF_n(FM%CHF, FM%DFO, FM%DMF, FM%F, HPROGRAM)
!
!----------------------------------------------------------------------------
!
IF (HINIT=='PRE') CALL READ_NAM_PREP_FLAKE_n(HPROGRAM)
IF (LHOOK) CALL DR_HOOK('READ_NAMELISTS_FLAKE_N',1,ZHOOK_HANDLE)
!
!----------------------

END SUBROUTINE READ_NAMELISTS_FLAKE_n
