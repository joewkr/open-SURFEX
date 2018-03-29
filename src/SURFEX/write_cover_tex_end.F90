!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
MODULE MODI_WRITE_COVER_TEX_END
CONTAINS
      SUBROUTINE WRITE_COVER_TEX_END(HPROGRAM)
!     ##########################
!
!!**** *WRITE_COVER_TEX* closes the tex file
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    08/01/98
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
!
USE MODD_WRITE_COVER_TEX,ONLY : NTEX
!
#ifdef SFX_LFI
USE MODI_CLOSE_WRITE_COVER_TEX_LFI
#endif
#ifdef SFX_MNH
USE MODI_MNHCLOSE_WRITE_COVER_TEX
#endif
#ifdef SFX_ARO
USE MODI_AROCLOSE_WRITE_COVER_TEX
#endif

!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=6),                INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_COVER_TEX_END',0,ZHOOK_HANDLE)
IF (NTEX==0 .AND. LHOOK) CALL DR_HOOK('WRITE_COVER_TEX_END',1,ZHOOK_HANDLE)
IF (NTEX==0) RETURN
!
!* writing in the file
!
WRITE(NTEX,*) '}}'
WRITE(NTEX,*) '\end{document}'
!
!* close the file
!
IF (HPROGRAM=='MESONH') THEN
#ifdef SFX_MNH
  CALL MNHCLOSE_WRITE_COVER_TEX
#endif
ELSEIF (HPROGRAM=='LFI   ') THEN
#ifdef SFX_LFI
  CALL CLOSE_WRITE_COVER_TEX_LFI(NTEX)
#endif
ELSEIF (HPROGRAM=='AROME') THEN
#ifdef SFX_ARO
  CALL AROCLOSE_WRITE_COVER_TEX(NTEX)
#endif
ELSE
  CLOSE(NTEX)
END IF
!
NTEX=0
IF (LHOOK) CALL DR_HOOK('WRITE_COVER_TEX_END',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITE_COVER_TEX_END
END MODULE MODI_WRITE_COVER_TEX_END
