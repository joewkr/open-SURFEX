!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ########################################
      SUBROUTINE GET_OFFLINE_CONF(PTSTEP_OUTPUT)
!     ########################################
!
!
!!****  *GET_OFFLINE_CONF* - routine to get some ISBA fields
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
USE MODN_IO_OFFLINE,     ONLY : XTSTEP_OUTPUT
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
REAL, INTENT(OUT) :: PTSTEP_OUTPUT ! time step of output time series
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('GET_OFFLINE_CONF',0,ZHOOK_HANDLE)
PTSTEP_OUTPUT = XTSTEP_OUTPUT
IF (LHOOK) CALL DR_HOOK('GET_OFFLINE_CONF',1,ZHOOK_HANDLE)
!
!==============================================================================
!
END SUBROUTINE GET_OFFLINE_CONF
