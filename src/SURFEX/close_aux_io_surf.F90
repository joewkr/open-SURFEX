!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
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
!!    B. Decharme (03/2014) read fa file in prep
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
#ifdef SFX_ASC
USE MODI_CLOSE_AUX_IO_SURF_ASC
#endif
!
#ifdef SFX_FA
USE MODI_CLOSE_AUX_IO_SURF_FA
#endif
!
#ifdef SFX_LFI
USE MODI_CLOSE_AUX_IO_SURF_LFI
#endif
!
#ifdef SFX_OL
USE MODI_CLOSE_AUX_IO_SURF_OL
#endif
!
#ifdef SFX_NC
USE MODI_CLOSE_AUX_IO_SURF_NC
#endif
!
#ifdef SFX_MNH
USE MODI_MNHCLOSE_AUX_IO_SURF
#endif
!
IMPLICIT NONE
!
#ifdef SFX_ARO
#include "aroclose_aux_io_surf.h"
#endif
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
IF (LHOOK) CALL DR_HOOK('CLOSE_AUX_IO_SURF',0,ZHOOK_HANDLE)
IF (HFILETYPE=='MESONH') THEN
#ifdef SFX_MNH
  CALL MNHCLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
#endif
END IF
!
IF (HFILETYPE=='OFFLIN' ) THEN
#ifdef SFX_OL
  CALL CLOSE_AUX_IO_SURF_OL
#endif
ENDIF
!
IF (HFILETYPE=='NC    ' ) THEN
#ifdef SFX_NC
  CALL CLOSE_AUX_IO_SURF_NC(HFILE,HFILETYPE)
#endif
ENDIF
!
IF (HFILETYPE=='ASCII ' ) THEN
#ifdef SFX_ASC
  CALL CLOSE_AUX_IO_SURF_ASC(HFILE,HFILETYPE)
#endif
ENDIF
!
IF (HFILETYPE=='AROME ' ) THEN
#ifdef SFX_ARO
  CALL AROCLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
#endif
ENDIF
!
IF (HFILETYPE=='FA    ' ) THEN
#ifdef SFX_FA
  CALL CLOSE_AUX_IO_SURF_FA(HFILE,HFILETYPE)
#endif
ENDIF
!
IF (HFILETYPE=='LFI   ' ) THEN
#ifdef SFX_LFI
  CALL CLOSE_AUX_IO_SURF_LFI(HFILE,HFILETYPE)
#endif
ENDIF
IF (LHOOK) CALL DR_HOOK('CLOSE_AUX_IO_SURF',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE CLOSE_AUX_IO_SURF
