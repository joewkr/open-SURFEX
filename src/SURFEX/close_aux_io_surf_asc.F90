!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #######################################################
      SUBROUTINE CLOSE_AUX_IO_SURF_ASC(HFILE,HFILETYPE)
!     #######################################################
!
!!****  *CLOSE_AUX_IO_SURF* - chooses the routine to OPENialize IO
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
!!      S.Malardel   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    09/2003 
!!      Modified    04/2004 by P. LeMoigne: add HACTION if ASCII mode selected
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_IO_SURF_ASC,ONLY:NUNIT,CMASK,NMASK,NFULL
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=28), INTENT(IN)  :: HFILE     ! file name
 CHARACTER(LEN=6),  INTENT(IN)  :: HFILETYPE ! main program
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('CLOSE_AUX_IO_SURF_ASC',0,ZHOOK_HANDLE)
CLOSE(NUNIT)
!
NFULL=0
NUNIT=0
CMASK='      '
DEALLOCATE(NMASK)
IF (LHOOK) CALL DR_HOOK('CLOSE_AUX_IO_SURF_ASC',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE CLOSE_AUX_IO_SURF_ASC
