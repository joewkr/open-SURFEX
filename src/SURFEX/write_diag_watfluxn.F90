!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE WRITE_DIAG_WATFLUX_n (DTCO, DUO, U, WM, HPROGRAM,HWRITE)
!     ###############################################################################
!
!!****  *WRITE_DIAG_WATFLUX_n * - diagnostics for lakes
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
USE MODD_SURFEX_n, ONLY : WATFLUX_MODEL_t
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DIAG_n, ONLY : DIAG_OPTIONS_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODI_WRITE_DIAG_SEB_WATFLUX_n
! 
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DUO
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(WATFLUX_MODEL_t), INTENT(INOUT) :: WM
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM ! program calling surf. schemes
 CHARACTER(LEN=3),   INTENT(IN)  :: HWRITE   ! 'PGD' : only physiographic fields are written
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!                                           ! 'ALL' : all fields are written
!
!*      0.2    declarations of local variables
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_WATFLUX_N',0,ZHOOK_HANDLE)
IF (HWRITE/='PGD') THEN
!        
   IF (WM%DWO%XDIAG_TSTEP==XUNDEF .OR. &
        ABS(NINT(WM%W%TTIME%TIME/WM%DWO%XDIAG_TSTEP)*WM%DWO%XDIAG_TSTEP-WM%W%TTIME%TIME)<1.E-3 ) THEN
      CALL WRITE_DIAG_SEB_WATFLUX_n(DTCO, DUO, U, WM%CHW, WM%DWO, WM%DW, WM%DWC, HPROGRAM)
   END IF
!        
ENDIF
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_WATFLUX_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE WRITE_DIAG_WATFLUX_n
