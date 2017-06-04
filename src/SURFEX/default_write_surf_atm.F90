!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE DEFAULT_WRITE_SURF_ATM(ONOWRITE_CANOPY, ONOWRITE_TEXFILE, OSPLIT_PATCH)
!     ########################################################################
!
!!****  *DEFAULT_WRITE_SURF_ATM* - routine to desactivate output writings
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
!!      P. Le Moigne   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    02/2008 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
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
LOGICAL,           INTENT(OUT) :: ONOWRITE_CANOPY  ! flag to desactivate writing of canopy fields
LOGICAL,           INTENT(OUT) :: ONOWRITE_TEXFILE ! flag to desactivate writing of tex file
LOGICAL,           INTENT(OUT) :: OSPLIT_PATCH
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('DEFAULT_WRITE_SURF_ATM',0,ZHOOK_HANDLE)
ONOWRITE_CANOPY  = .FALSE.
ONOWRITE_TEXFILE = .TRUE.
OSPLIT_PATCH = .TRUE.
IF (LHOOK) CALL DR_HOOK('DEFAULT_WRITE_SURF_ATM',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE DEFAULT_WRITE_SURF_ATM
