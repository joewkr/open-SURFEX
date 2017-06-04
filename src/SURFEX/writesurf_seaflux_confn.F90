!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_SEAFLUX_CONF_n (CHS, DGO, DGMSI, O, S, HPROGRAM)
!     ######################################################
!
!!****  *WRITESURF_SEAFLUX_CONF* - routine to read the configuration for SEAFLUX
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
!!      Modified    09/2013 : S. Senesi : handle seaice scheme
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CH_SEAFLUX_n, ONLY : CH_SEAFLUX_t
USE MODD_DIAG_OCEAN_n, ONLY : DIAG_OCEAN_t
USE MODD_DIAG_MISC_SEAICE_n, ONLY : DIAG_MISC_SEAICE_t
USE MODD_OCEAN_n, ONLY : OCEAN_t
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
!
USE MODN_SEAFLUX_n
USE MODN_SLT
!
USE MODD_WRITE_SURF_ATM, ONLY : LNAM_SEAFLUX_WRITTEN
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
TYPE(CH_SEAFLUX_t), INTENT(INOUT) :: CHS
TYPE(DIAG_OCEAN_t), INTENT(INOUT) :: DGO
TYPE(DIAG_MISC_SEAICE_t), INTENT(INOUT) :: DGMSI
TYPE(OCEAN_t), INTENT(INOUT) :: O
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
!
 CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM ! program calling SEAFLUX

!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: ILUDES
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_SEAFLUX_CONF_N',0,ZHOOK_HANDLE)
 CALL GET_DEFAULT_NAM_n(HPROGRAM,'WRITE',ILUDES,LNAM_SEAFLUX_WRITTEN)
!
IF (ILUDES==0 .AND. LHOOK) CALL DR_HOOK('WRITESURF_SEAFLUX_CONF_N',1,ZHOOK_HANDLE)
IF (ILUDES==0) RETURN
!
!-------------------------------------------------------------------------------
!
 CALL INIT_NAM_SEAFLUXn(O, S)
 CALL INIT_NAM_CH_SEAFLUXn(CHS)
 CALL INIT_NAM_DIAG_OCEANn(DGO)
 CALL INIT_NAM_SEAICEn(DGMSI, S)
!
WRITE(UNIT=ILUDES,NML=NAM_SEAFLUXn)
WRITE(UNIT=ILUDES,NML=NAM_CH_SEAFLUXn)
WRITE(UNIT=ILUDES,NML=NAM_DIAG_OCEANn)
WRITE(UNIT=ILUDES,NML=NAM_SEAICEn)
WRITE(UNIT=ILUDES,NML=NAM_SURF_SLT)
IF (LHOOK) CALL DR_HOOK('WRITESURF_SEAFLUX_CONF_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_SEAFLUX_CONF_n
