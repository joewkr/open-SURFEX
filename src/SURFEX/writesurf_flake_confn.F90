!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_FLAKE_CONF_n (CHF, DMF, F, HPROGRAM)
!     ######################################################
!
!!****  *WRITESURF_FLAKE_CONF* - routine to read the configuration for FLAKE
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
!!      Modified    04/2013, P. Le Moigne: FLake chemistry
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CH_FLAKE_n, ONLY : CH_FLAKE_t
USE MODD_DIAG_MISC_FLAKE_n, ONLY : DIAG_MISC_FLAKE_t
USE MODD_FLAKE_n, ONLY : FLAKE_t
!
USE MODN_FLAKE_n
!
USE MODD_WRITE_SURF_ATM, ONLY : LNAM_FLAKE_WRITTEN
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
TYPE(CH_FLAKE_t), INTENT(INOUT) :: CHF
TYPE(DIAG_MISC_FLAKE_t), INTENT(INOUT) :: DMF
TYPE(FLAKE_t), INTENT(INOUT) :: F
!
 CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM ! program calling FLAKE

!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: ILUDES
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!NAMELIST/NAM_DIAG_FLAKEn/LWATER_PROFILE,XZW_PROFILE
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_FLAKE_CONF_N',0,ZHOOK_HANDLE)
 CALL GET_DEFAULT_NAM_n(HPROGRAM,'WRITE',ILUDES,LNAM_FLAKE_WRITTEN)
!
IF (ILUDES==0 .AND. LHOOK) CALL DR_HOOK('WRITESURF_FLAKE_CONF_N',1,ZHOOK_HANDLE)
IF (ILUDES==0) RETURN
!
!-------------------------------------------------------------------------------
!
 CALL INIT_NAM_FLAKEn(F)
 CALL INIT_NAM_DIAG_FLAKEn(DMF)
 CALL INIT_NAM_CH_FLAKEn(CHF)
!
WRITE(UNIT=ILUDES,NML=NAM_FLAKEn)
WRITE(UNIT=ILUDES,NML=NAM_DIAG_FLAKEn)
WRITE(UNIT=ILUDES,NML=NAM_CH_FLAKEn)
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_FLAKE_CONF_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_FLAKE_CONF_n
