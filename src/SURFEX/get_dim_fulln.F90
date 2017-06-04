!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ########################################
      SUBROUTINE GET_DIM_FULL_n (KDIM_FULL_IN, KDIM_FULL_OUT)
!     ########################################
!
!!****  *GET_DIM_FULL_n* - routine to get some ISBA fields
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
!!      Original    04/2008
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER, INTENT(IN) :: KDIM_FULL_IN
INTEGER, INTENT(OUT) :: KDIM_FULL_OUT ! total number of points
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('GET_DIM_FULL_N',0,ZHOOK_HANDLE)
KDIM_FULL_OUT = KDIM_FULL_IN
IF (LHOOK) CALL DR_HOOK('GET_DIM_FULL_N',1,ZHOOK_HANDLE)
!
!==============================================================================
!
END SUBROUTINE GET_DIM_FULL_n
