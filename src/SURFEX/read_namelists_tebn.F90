!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE READ_NAMELISTS_TEB_n (TM, GRO, GDO, HPROGRAM, HINIT)
!     #######################################################
!
!---------------------------------------------------------------------------
!
!
USE MODD_SURFEX_n, ONLY : TEB_MODEL_t
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
!
USE MODN_TEB_n                          
USE MODN_TEB_VEG_n,            ONLY: CRUNOFF,CALBEDO,CSCOND,                       &
                                     CC1DRY, CSOILFRZ, CDIFSFCOND, CSNOWRES,       &
                                     CCPSURF, XCGMAX, CKSAT,                       &
                                     CRAIN, CHORT, LGLACIER,                       &
                                     LCANOPY_DRAG, LVEGUPD, LNITRO_DILU
USE MODN_TEB_GREENROOF_n,      ONLY: CRUNOFF_GR,CSCOND_GR,CKSAT_GR,CHORT_GR
!
USE MODI_DEFAULT_TEB
USE MODI_DEFAULT_TEB_VEG
USE MODI_DEFAULT_GREENROOF
USE MODI_DEFAULT_CH_DEP
USE MODI_DEFAULT_DIAG_TEB
USE MODI_READ_DEFAULT_TEB_n
USE MODI_READ_TEB_CONF_n
USE MODI_READ_TEB_VEG_CONF_n
!
USE MODI_READ_NAM_PREP_TEB_n
USE MODI_READ_NAM_PREP_GARDEN_n
USE MODI_READ_NAM_PREP_GREENROOF_n
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_READ_TEB_CONF_n
IMPLICIT NONE
!
!
TYPE(TEB_MODEL_t), INTENT(INOUT) :: TM
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: GRO
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: GDO
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=3),   INTENT(IN)  :: HINIT     ! choice of fields to initialize
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_NAMELISTS_TEB_N',0,ZHOOK_HANDLE)
 CALL DEFAULT_TEB(CZ0H,XTSTEP,XOUT_TSTEP, CCH_BEM, XDT_RES, XDT_OFF)
!
 CALL DEFAULT_TEB_VEG(CRUNOFF, CALBEDO, CSCOND,                 &
                      CC1DRY, CSOILFRZ, CDIFSFCOND, CSNOWRES,   &
                      CCPSURF, XCGMAX, CKSAT,                   &
                      CRAIN, CHORT, LGLACIER,                   &
                      LCANOPY_DRAG, LVEGUPD, LNITRO_DILU        )
!
 CALL DEFAULT_GREENROOF(CRUNOFF_GR,CSCOND_GR, CKSAT_GR,CHORT_GR)
!
 CALL DEFAULT_CH_DEP(CCH_DRY_DEP)
!
 CALL DEFAULT_DIAG_TEB(N2M,LSURF_BUDGET,L2M_MIN_ZS,LRAD_BUDGET, &
                      LCOEF,LSURF_VARS,LSURF_MISC_BUDGET,&
                      LSURF_DIAG_ALBEDO,LUTCI,LPGD,XDIAG_TSTEP)   
!               
 CALL READ_DEFAULT_TEB_n(TM%CHT, TM%TD%MTO, TM%TD%O, TM%TD%DUT, GRO, TM%NT%AL(1), TM%TOP, &
                         HPROGRAM)
!
 CALL READ_TEB_CONF_n(TM%CHT, TM%TD%MTO, TM%TD%O, TM%TD%DUT, TM%NT%AL(1), TM%TOP, &
                      HPROGRAM) 
!  
 CALL READ_TEB_VEG_CONF_n(TM%CHT, GDO, HPROGRAM) 
!
IF (HINIT=='PRE') THEN
        CALL READ_NAM_PREP_TEB_n(HPROGRAM)
        CALL READ_NAM_PREP_GARDEN_n(HPROGRAM)
        CALL READ_NAM_PREP_GREENROOF_n(HPROGRAM)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('READ_NAMELISTS_TEB_N',1,ZHOOK_HANDLE)
!
!------------------------------------
!
END SUBROUTINE READ_NAMELISTS_TEB_n
