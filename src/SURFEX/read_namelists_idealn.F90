!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE READ_NAMELISTS_IDEAL_n (DGO, HPROGRAM)
!     #######################################################
!
!--------------------------------------------------------------------------
!
!
!
USE MODD_DIAG_n, ONLY : DIAG_OPTIONS_t
!
USE MODN_IDEAL_n
!
USE MODI_DEFAULT_DIAG_IDEAL
USE MODI_READ_DEFAULT_IDEAL_n
USE MODI_READ_IDEAL_CONF_n
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DGO
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!----------------------------------------------------------
!ideal: reprend essentiellement la namelist NAM_DIAG_SURF
IF (LHOOK) CALL DR_HOOK('READ_NAMELISTS_IDEAL_N',0,ZHOOK_HANDLE)
 CALL DEFAULT_DIAG_IDEAL(N2M,LSURF_BUDGET,L2M_MIN_ZS,LRAD_BUDGET,LCOEF,LSURF_VARS,&
                         LSURF_BUDGETC,LRESET_BUDGETC,XDIAG_TSTEP           )
!                        
 CALL READ_DEFAULT_IDEAL_n(DGO, HPROGRAM)
!
 CALL READ_IDEAL_CONF_n(DGO, HPROGRAM)   
 !
IF (LHOOK) CALL DR_HOOK('READ_NAMELISTS_IDEAL_N',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_NAMELISTS_IDEAL_n
