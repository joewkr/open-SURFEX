!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
MODULE MODI_END_IO_SURF_n
CONTAINS
      SUBROUTINE END_IO_SURF_n(HPROGRAM)
!     #######################################################
!
!!****  *END_IO_SURF_n* - routine to close all relevant files
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
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
#ifdef SFX_ASC
USE MODI_END_IO_SURF_ASC_n
#endif
#ifdef SFX_FA
USE MODI_END_IO_SURF_FA_n
#endif
#ifdef SFX_LFI
USE MODI_END_IO_SURF_LFI_n
#endif
#ifdef SFX_NC
USE MODI_END_IO_SURF_NC_n
#endif
#ifdef SFX_OL
USE MODI_END_IO_SURF_OL_n
#endif
#ifdef SFX_MNH
USE MODI_MNHEND_IO_SURF_n
#endif
#ifdef SFX_ARO
USE MODI_AROEND_IO_SURF_n
#endif
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! main program
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('END_IO_SURF_N',0,ZHOOK_HANDLE)
IF (HPROGRAM=='MESONH') THEN
#ifdef SFX_MNH
  CALL MNHEND_IO_SURF_n(HPROGRAM)
#endif
ELSEIF (HPROGRAM=='ASCII ') THEN
#ifdef SFX_ASC
  CALL END_IO_SURF_ASC_n(HPROGRAM)
#endif
ELSEIF (HPROGRAM=='OFFLIN') THEN
#ifdef SFX_OL
  CALL END_IO_SURF_OL_n(HPROGRAM)
#endif
ELSEIF (HPROGRAM=='AROME ') THEN
#ifdef SFX_ARO
  CALL AROEND_IO_SURF_n(HPROGRAM)
#endif
ELSEIF (HPROGRAM=='FA    ') THEN
#ifdef SFX_FA
  CALL END_IO_SURF_FA_n(HPROGRAM)
#endif
ELSEIF (HPROGRAM=='LFI   ') THEN
#ifdef SFX_LFI
  CALL END_IO_SURF_LFI_n(HPROGRAM)
#endif
ELSEIF (HPROGRAM=='NC    ') THEN
#ifdef SFX_NC
  CALL END_IO_SURF_NC_n(HPROGRAM)
#endif
END IF
IF (LHOOK) CALL DR_HOOK('END_IO_SURF_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE END_IO_SURF_n
END MODULE MODI_END_IO_SURF_n
