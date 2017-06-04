!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE OPEN_AUX_IO_SURF (HFILE,HFILETYPE,HMASK,HDIR)
!     #######################################################
!
!!****  *OPEN_AUX_IO_SURF* - chooses the routine to OPENialize IO
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
USE YOMHOOK ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
#ifdef SFX_ASC
USE MODI_OPEN_AUX_IO_SURF_ASC
#endif
!
#ifdef SFX_FA
USE MODI_OPEN_AUX_IO_SURF_FA
#endif
!
#ifdef SFX_LFI
USE MODI_OPEN_AUX_IO_SURF_LFI
#endif
!
#ifdef SFX_OL
USE MODI_OPEN_AUX_IO_SURF_OL
#endif
!
#ifdef SFX_NC
USE MODI_OPEN_AUX_IO_SURF_NC
#endif
!
#ifdef SFX_MNH
USE MODI_MNHOPEN_AUX_IO_SURF
#endif
!
IMPLICIT NONE
!
#ifdef SFX_ARO
#include "aroopen_aux_io_surf.h"
#endif
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
!
 CHARACTER(LEN=28), INTENT(IN)  :: HFILE     ! file name
 CHARACTER(LEN=6),  INTENT(IN)  :: HFILETYPE ! main program
 CHARACTER(LEN=6),  INTENT(IN)  :: HMASK
 CHARACTER(LEN=1), OPTIONAL, INTENT(IN) :: HDIR
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
 CHARACTER(LEN=1) :: YDIR
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('OPEN_AUX_IO_SURF',0,ZHOOK_HANDLE)
!
YDIR = '-'
IF (PRESENT(HDIR)) YDIR=HDIR
!
IF (HFILETYPE=='MESONH') THEN
#ifdef SFX_MNH
  CALL MNHOPEN_AUX_IO_SURF(HFILE,HFILETYPE,HMASK)
#endif
END IF
!
IF (HFILETYPE=='OFFLIN' ) THEN
#ifdef SFX_OL
  CALL OPEN_AUX_IO_SURF_OL
#endif
ENDIF
!
IF (HFILETYPE=='ASCII ' ) THEN
#ifdef SFX_ASC
  CALL OPEN_AUX_IO_SURF_ASC(HFILE,HFILETYPE,HMASK,YDIR)
#endif
ENDIF
!
IF (HFILETYPE=='AROME ' ) THEN
#ifdef SFX_ARO
  CALL AROOPEN_AUX_IO_SURF(HFILE,HFILETYPE,HMASK,YDIR)
#endif
ENDIF
!
IF (HFILETYPE=='FA    ' ) THEN
#ifdef SFX_FA
  CALL OPEN_AUX_IO_SURF_FA(HFILE,HFILETYPE,HMASK,YDIR)
#endif
ENDIF
!
IF (HFILETYPE=='LFI   ' ) THEN
#ifdef SFX_LFI
  CALL OPEN_AUX_IO_SURF_LFI(HFILE,HFILETYPE,HMASK,YDIR)
#endif
ENDIF
!
IF (HFILETYPE=='NC    ' ) THEN
#ifdef SFX_NC
  CALL OPEN_AUX_IO_SURF_NC(HFILE,HFILETYPE,HMASK,YDIR)
#endif
ENDIF
!
IF (LHOOK) CALL DR_HOOK('OPEN_AUX_IO_SURF',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE OPEN_AUX_IO_SURF
