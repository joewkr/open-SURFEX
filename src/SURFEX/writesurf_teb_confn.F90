!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_TEB_CONF_n (CHT, DMTO, DGO, DUT, T, TOP, HPROGRAM)
!     ######################################################
!
!!****  *WRITESURF_TEB_CONF* - routine to read the configuration for TEB
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2003 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CH_TEB_n, ONLY : CH_TEB_t
USE MODD_DIAG_MISC_TEB_OPTIONS_n, ONLY : DIAG_MISC_TEB_OPTIONS_t
USE MODD_DIAG_n, ONLY : DIAG_OPTIONS_t
USE MODD_DIAG_UTCI_TEB_n, ONLY : DIAG_UTCI_TEB_t
USE MODD_TEB_n, ONLY : TEB_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
!
USE MODN_TEB_n
!
USE MODD_WRITE_SURF_ATM, ONLY : LNAM_TEB_WRITTEN
!
USE MODI_GET_DEFAULT_NAM_n
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(CH_TEB_t), INTENT(INOUT) :: CHT
TYPE(DIAG_MISC_TEB_OPTIONS_t), INTENT(INOUT) :: DMTO
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DGO
TYPE(DIAG_UTCI_TEB_t), INTENT(INOUT) :: DUT
TYPE(TEB_t), INTENT(INOUT) :: T
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
!
 CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM ! program calling TEB

!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: ILUDES
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_TEB_CONF_N',0,ZHOOK_HANDLE)
 CALL GET_DEFAULT_NAM_n(HPROGRAM,'WRITE',ILUDES,LNAM_TEB_WRITTEN)
!
IF (ILUDES==0 .AND. LHOOK) CALL DR_HOOK('WRITESURF_TEB_CONF_N',1,ZHOOK_HANDLE)
IF (ILUDES==0) RETURN
!
!-------------------------------------------------------------------------------
!
 CALL INIT_NAM_TEBn(T, TOP)
 CALL INIT_NAM_DIAG_TEBn(DMTO, DGO, DUT)
 CALL INIT_NAM_CH_TEBn(CHT)
!
WRITE(UNIT=ILUDES,NML=NAM_TEBn)
WRITE(UNIT=ILUDES,NML=NAM_DIAG_TEBn)
WRITE(UNIT=ILUDES,NML=NAM_CH_TEBn)
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_TEB_CONF_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_TEB_CONF_n
