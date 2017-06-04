!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE READ_NAMELISTS_SURF_n (CHU, DGO, USS, HPROGRAM,HINIT)
!     #######################################################
!
!---------------------------    
!
!
!
USE MODD_CH_SURF_n, ONLY : CH_SURF_t
USE MODD_DIAG_n, ONLY : DIAG_OPTIONS_t
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODN_SURF_ATM_n
!
USE MODN_SSO_n
!
USE MODI_DEFAULT_SSO
!
USE MODI_DEFAULT_CH_SURF_ATM
!
USE MODI_DEFAULT_DIAG_SURF_ATM
!
USE MODI_READ_DEFAULT_SURF_ATM_n
!
USE MODI_READ_SURF_ATM_CONF_n
!
USE MODI_READ_NAM_PREP_SURF_n
!
!------------------------------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
TYPE(CH_SURF_t), INTENT(INOUT) :: CHU
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DGO
TYPE(SSO_t), INTENT(INOUT) :: USS
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM    ! program calling surf. schemes
 CHARACTER(LEN=3),   INTENT(IN)  :: HINIT     ! choice of fields to initialize
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!---------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_NAMELISTS_SURF_N',0,ZHOOK_HANDLE)
 CALL DEFAULT_SSO(CROUGH,XFRACZ0,XCOEFBE)
!
 CALL DEFAULT_CH_SURF_ATM(CCHEM_SURF_FILE,LCH_SURF_EMIS)
!
CALL DEFAULT_DIAG_SURF_ATM(N2M, LT2MMW, LSURF_BUDGET, L2M_MIN_ZS, LRAD_BUDGET, &
                           LCOEF, LSURF_VARS, LSURF_BUDGETC, LRESET_BUDGETC, &
                           LSELECT, LPROVAR_TO_DIAG, LDIAG_GRID, LFRAC, &
                           XDIAG_TSTEP, LSNOWDIMNC, LRESETCUMUL, CSELECT )   
!      
 CALL READ_DEFAULT_SURF_ATM_n(CHU, DGO, USS, HPROGRAM) 
!
 CALL READ_SURF_ATM_CONF_n(CHU, DGO, USS, HPROGRAM)    
!       
!---------------------------------------------------------------------------
!PREP
IF (HINIT=='PRE') CALL READ_NAM_PREP_SURF_n(HPROGRAM)
IF (LHOOK) CALL DR_HOOK('READ_NAMELISTS_SURF_N',1,ZHOOK_HANDLE)
!
!------------------------------------------------------------------------------------
END SUBROUTINE READ_NAMELISTS_SURF_n
