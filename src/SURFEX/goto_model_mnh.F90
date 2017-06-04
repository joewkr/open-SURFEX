!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#######################
MODULE MODI_GOTO_MODEL_MNH
  !#######################
  !
  INTERFACE
    !     ###############################
          SUBROUTINE GOTO_MODEL_MNH(U,HPROGRAM, KMI, KINFO_ll)
    !     ###############################
    !!
    !!    PURPOSE
    !!    -------
    !!
    !!    Calls GOTO_MODEL_SURFEX_MNH to
    !!    initialize local sizes in SURFEX module MODD_SURF_ATM_n for model KMI
    !!    and call GOTO_MODEL(KMI)
    !!             GOTO_MODEL_ll(KMI, KINFO_ll)
    !!
    !!    METHOD
    !!    ------
    !!
    !!    EXTERNAL
    !!    --------
    !!
    !!
    !!    IMPLICIT ARGUMENTS
    !!    ------------------
    !!
    !!
    !!    REFERENCE
    !!    ---------
    !!
    !!    AUTHOR
    !!    ------
    !!
    !!    M. Moge                   LA - CNRS
    !!
    !!    MODIFICATION
    !!    ------------
    !!
    !!    Original      08/2015
    !----------------------------------------------------------------------------
    !
    !*    0.     DECLARATION
    !            -----------
    !
    USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
    IMPLICIT NONE
    !
    !*    0.1    Declaration of dummy arguments
    !            ------------------------------
    !
!
    TYPE(SURF_ATM_t), INTENT(INOUT) :: U
    CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM ! calling program
    INTEGER,                         INTENT(IN)    :: KMI    !model id
    INTEGER,                         INTENT(OUT)    :: KINFO_ll
          END SUBROUTINE GOTO_MODEL_MNH
  !
  END INTERFACE
  !
END MODULE MODI_GOTO_MODEL_MNH
!     ###############################
      SUBROUTINE GOTO_MODEL_MNH(U,HPROGRAM, KMI, KINFO_ll)
!     ###############################
!!
!!    PURPOSE
!!    -------
!!
!!    Calls GOTO_MODEL_SURFEX_MNH to
!!    initialize local sizes in SURFEX module MODD_SURF_ATM_n for model KMI
!!    and call GOTO_MODEL(KMI)
!!             GOTO_MODEL_ll(KMI, KINFO_ll)
!!
!!    METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    M. Moge                   LA - CNRS
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original      08/2015
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
#ifdef SFX_MNH
USE MODI_GOTO_MODEL_SURFEX_MNH
#endif
!
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM ! calling program
INTEGER,                         INTENT(IN)    :: KMI    !model id
INTEGER,                         INTENT(OUT)    :: KINFO_ll
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER :: IMI ! return code of // routines
CHARACTER*1 :: HSPLIT
!
!------------------------------------------------------------------------------
!
IF (HPROGRAM=='MESONH') THEN
#ifdef SFX_MNH
  CALL GOTO_MODEL_SURFEX_MNH(U,KMI, KINFO_ll)
#else
  KINFO_ll = 0
#endif
ENDIF
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GOTO_MODEL_MNH
