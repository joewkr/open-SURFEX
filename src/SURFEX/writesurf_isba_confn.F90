!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_ISBA_CONF_n (CHI, DE, DGO, DMI, IO, HPROGRAM)
!     ######################################################
!
!!****  *WRITESURF_ISBA_CONF* - routine to read the configuration for ISBA
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
USE MODD_CH_ISBA_n, ONLY : CH_ISBA_t
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_t
USE MODD_DIAG_n, ONLY : DIAG_OPTIONS_t
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
!
USE MODN_ISBA_n
USE MODI_GET_DEFAULT_NAM_n
USE MODN_AGRI
USE MODN_DEEPSOIL
USE MODN_TREEDRAG
USE MODN_DST
!
USE MODD_WRITE_SURF_ATM, ONLY : LNAM_ISBA_WRITTEN
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
TYPE(CH_ISBA_t), INTENT(INOUT) :: CHI
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: DE
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DGO
TYPE(DIAG_MISC_ISBA_t), INTENT(INOUT) :: DMI
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
!
 CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM ! program calling ISBA

!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: ILUDES
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_ISBA_CONF_N',0,ZHOOK_HANDLE)
 CALL GET_DEFAULT_NAM_n(HPROGRAM,'WRITE',ILUDES,LNAM_ISBA_WRITTEN)
!
IF (ILUDES==0 .AND. LHOOK) CALL DR_HOOK('WRITESURF_ISBA_CONF_N',1,ZHOOK_HANDLE)
IF (ILUDES==0) RETURN
!
!-------------------------------------------------------------------------------
!
 CALL INIT_NAM_ISBAn(IO)
 CALL INIT_NAM_ISBA_AGSn(IO)
 CALL INIT_NAM_SGH_ISBAn(IO)
 CALL INIT_NAM_DIAG_ISBAn(DE, DGO, DMI)
 CALL INIT_NAM_CH_ISBAn(CHI)
 CALL INIT_NAM_SPINUP_CARB_ISBAn(IO)
 CALL INIT_NAM_ISBA_SNOWn(IO)
!
WRITE(UNIT=ILUDES,NML=NAM_ISBAn)
WRITE(UNIT=ILUDES,NML=NAM_ISBA_AGSn)
WRITE(UNIT=ILUDES,NML=NAM_SGH_ISBAn)
WRITE(UNIT=ILUDES,NML=NAM_DIAG_ISBAn)
WRITE(UNIT=ILUDES,NML=NAM_CH_ISBAn)
WRITE(UNIT=ILUDES,NML=NAM_SPINUP_CARBn)
WRITE(UNIT=ILUDES,NML=NAM_ISBA_SNOWn)
WRITE(UNIT=ILUDES,NML=NAM_AGRI)
WRITE(UNIT=ILUDES,NML=NAM_DEEPSOIL)
WRITE(UNIT=ILUDES,NML=NAM_TREEDRAG)
WRITE(UNIT=ILUDES,NML=NAM_SURF_DST)
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_ISBA_CONF_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_ISBA_CONF_n
