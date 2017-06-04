!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE DEFAULT_AGRI(OAGRIP)
!     ########################################################################
!
!!****  *DEFAULT_ISBA* - routine to set default values for 
!                        main logical switch for irrigation
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
!!      L. Jarlan  *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2005
!!      Modified by P. Le Moigne (06/2006): seeding and irrigation
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
LOGICAL, INTENT(OUT) :: OAGRIP
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.1   Declarations of arguments
!-------------------------------------------------------------------------------
!
! General switch
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_AGRI',0,ZHOOK_HANDLE)
OAGRIP = .FALSE.
IF (LHOOK) CALL DR_HOOK('DEFAULT_AGRI',1,ZHOOK_HANDLE)
!
END SUBROUTINE DEFAULT_AGRI
