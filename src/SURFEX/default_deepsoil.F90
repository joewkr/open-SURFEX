!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE DEFAULT_DEEPSOIL(ODEEPSOIL,OPHYSDOMC)
!     ########################################################################
!
!!****  *DEFAULT_ISBA* - routine to set default values for 
!                        main logical switch for deep soil characteristics
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
!!      P. Le Moigne  *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    05/2008
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
LOGICAL, INTENT(OUT) :: ODEEPSOIL
LOGICAL, INTENT(OUT) :: OPHYSDOMC
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.1   Declarations of arguments
!-------------------------------------------------------------------------------
!
! General switch
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_DEEPSOIL',0,ZHOOK_HANDLE)
ODEEPSOIL = .FALSE.
OPHYSDOMC = .FALSE.
IF (LHOOK) CALL DR_HOOK('DEFAULT_DEEPSOIL',1,ZHOOK_HANDLE)
!
END SUBROUTINE DEFAULT_DEEPSOIL
