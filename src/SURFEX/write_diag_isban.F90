!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE WRITE_DIAG_ISBA_n (DTCO, DUO, U, IM, NDST,  HPROGRAM, HWRITE)
!     ###############################################################################
!
!!****  *WRITE_DIAG_ISBA_n * - Stores ISBA diagnostics
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
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DIAG_n, ONLY : DIAG_OPTIONS_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SURFEX_n, ONLY : ISBA_MODEL_t
USE MODD_DST_n, ONLY : DST_NP_t
!
USE MODD_SURF_PAR,    ONLY : XUNDEF
! 
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_WRITE_DIAG_MISC_ISBA_n
USE MODI_WRITE_DIAG_PGD_ISBA_n
USE MODI_WRITE_DIAG_SEB_ISBA_n
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DUO
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(ISBA_MODEL_t), INTENT(INOUT) :: IM
TYPE(DST_NP_t), INTENT(INOUT) :: NDST
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM ! program calling surf. schemes
 CHARACTER(LEN=3),   INTENT(IN)  :: HWRITE    ! 'PGD' : only physiographic fields are written
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!                                            ! 'ALL' : all fields are written
!
!*      0.2    declarations of local variables
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_ISBA_N',0,ZHOOK_HANDLE)
IF (HWRITE/='PGD') THEN
  IF (IM%ID%O%XDIAG_TSTEP==XUNDEF .OR. &
          ABS(NINT(IM%S%TTIME%TIME/IM%ID%O%XDIAG_TSTEP)*IM%ID%O%XDIAG_TSTEP-IM%S%TTIME%TIME)<1.E-3 ) THEN
    CALL WRITE_DIAG_SEB_ISBA_n(DTCO, DUO, U, IM%NCHI, IM%CHI, IM%ID, NDST, IM%GB, &
                               IM%O, IM%S, IM%NP, IM%NPE, HPROGRAM)
    CALL WRITE_DIAG_MISC_ISBA_n(DTCO, DUO%CSELECT, DUO%LSNOWDIMNC, U, IM%ID%O%LPATCH_BUDGET, &
                                IM%ID%D, IM%ID%ND, IM%ID%DM, IM%ID%NDM, IM%O, IM%S, IM%K,    &
                                IM%NP, IM%NPE%AL(1)%TSNOW, HPROGRAM)
  END IF
END IF
!
IF (IM%ID%O%LPGD) THEN
  IF (IM%ID%O%XDIAG_TSTEP==XUNDEF .OR. &
      ABS(NINT(IM%S%TTIME%TIME/IM%ID%O%XDIAG_TSTEP)*IM%ID%O%XDIAG_TSTEP-IM%S%TTIME%TIME)<1.E-3 ) THEN
    CALL WRITE_DIAG_PGD_ISBA_n(DTCO, DUO%CSELECT, U, IM%CHI, IM%NCHI, IM%ID%DM%LSURF_DIAG_ALBEDO, &
                               IM%O, IM%S, IM%K, IM%NP, IM%NPE, IM%ISS, HPROGRAM)
  END IF
END IF
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_ISBA_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE WRITE_DIAG_ISBA_n
