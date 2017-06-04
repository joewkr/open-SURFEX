!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE CLOSE_NAMELIST_LFI(HPROGRAM,KLUNAM)
!     #######################################################
!
!!****  *CLOSE_NAMELIST_LFI* - closes namelists read by surface in MESOHN
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
#ifdef SFX_LFI
USE MODD_FMDECLAR,    ONLY : CNAMFI
#endif
USE MODD_IO_SURF_LFI, ONLY : CLUOUT_LFI

!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! main program
INTEGER,           INTENT(IN)  :: KLUNAM   ! logical unit of namelist
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: IRESP
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!* closes the namelist
!  -------------------
!
IF (LHOOK) CALL DR_HOOK('CLOSE_NAMELIST_LFI',0,ZHOOK_HANDLE)
#ifdef SFX_LFI
 CALL FMFREE(CNAMFI(KLUNAM),CLUOUT_LFI,IRESP)
#endif
CLOSE(KLUNAM)
IF (LHOOK) CALL DR_HOOK('CLOSE_NAMELIST_LFI',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE CLOSE_NAMELIST_LFI
