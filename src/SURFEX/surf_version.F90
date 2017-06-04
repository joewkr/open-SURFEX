!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################
      SUBROUTINE SURF_VERSION
!     ##################
!
!!****  *SURF_VERSION * - subroutine to initialize the surface version
!!
!!    PURPOSE
!!    -------
!       The purpose of this routine is to initialize NVERSION and NBUGFIX
!     corresponding to the version chosen by the user.
!       The user can also set the name of his own binary library
!      These values will be writen in the output files
!
!!
!!    AUTHOR
!!    ------
!!      V. Masson          * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    02/2004
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
USE MODD_SURF_PAR, ONLY : NVERSION,NBUGFIX
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!

REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('SURF_VERSION',0,ZHOOK_HANDLE)
NVERSION = 8
NBUGFIX  = 1
IF (LHOOK) CALL DR_HOOK('SURF_VERSION',1,ZHOOK_HANDLE)
!
END SUBROUTINE SURF_VERSION
