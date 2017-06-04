!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_ATM_CONF_n (CHU, DGO, USS, HPROGRAM)
!     #########################################
!
!!****  *MNHWRITE_SURF_ATM_CONF* - routine to write the configuration for the surface
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
USE MODD_CH_SURF_n, ONLY : CH_SURF_t
USE MODD_DIAG_n, ONLY : DIAG_OPTIONS_t
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODN_SSO_n
USE MODN_SURF_ATM_n
!
USE MODN_CHS_ORILAM
USE MODN_SURF_ATM
USE MODN_WRITE_SURF_ATM
!
USE MODD_WRITE_SURF_ATM, ONLY : LNAM_ATM_WRITTEN
!
USE MODI_GET_DEFAULT_NAM_n
!
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
TYPE(CH_SURF_t), INTENT(INOUT) :: CHU
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DGO
TYPE(SSO_t), INTENT(INOUT) :: USS
!
 CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM ! program calling ISBA
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: ILUDES
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_ATM_CONF_N',0,ZHOOK_HANDLE)
 CALL GET_DEFAULT_NAM_n(HPROGRAM,'WRITE',ILUDES,LNAM_ATM_WRITTEN)
!
IF (ILUDES==0 .AND. LHOOK) CALL DR_HOOK('WRITESURF_ATM_CONF_N',1,ZHOOK_HANDLE)
IF (ILUDES==0) RETURN
!
!-------------------------------------------------------------------------------
!
 CALL INIT_NAM_SSOn(USS)
 CALL INIT_NAM_CH_CONTROLn(CHU)
 CALL INIT_NAM_CH_SURFn(CHU)
 CALL INIT_NAM_DIAG_SURF_ATMn(DGO)
 CALL INIT_NAM_DIAG_SURFn(DGO)
 CALL INIT_NAM_WRITE_DIAG_SURFn(DGO)
!
WRITE(UNIT=ILUDES,NML=NAM_SSOn)
WRITE(UNIT=ILUDES,NML=NAM_CH_CONTROLn)
WRITE(UNIT=ILUDES,NML=NAM_CH_SURFn)
WRITE(UNIT=ILUDES,NML=NAM_DIAG_SURF_ATMn)
WRITE(UNIT=ILUDES,NML=NAM_DIAG_SURFn)
WRITE(UNIT=ILUDES,NML=NAM_WRITE_DIAG_SURFn)
WRITE(UNIT=ILUDES,NML=NAM_CHS_ORILAM)
WRITE(UNIT=ILUDES,NML=NAM_SURF_ATM)
WRITE(UNIT=ILUDES,NML=NAM_WRITE_SURF_ATM)
IF (LHOOK) CALL DR_HOOK('WRITESURF_ATM_CONF_N',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_ATM_CONF_n
