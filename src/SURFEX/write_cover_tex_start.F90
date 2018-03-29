!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
MODULE MODI_WRITE_COVER_TEX_START
CONTAINS
      SUBROUTINE WRITE_COVER_TEX_START(HPROGRAM)
!     ##########################
!
!!**** *WRITE_COVER_TEX* opens the output tex file containing cover data
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
USE MODD_WRITE_COVER_TEX,ONLY : NTEX
USE MODD_WRITE_SURF_ATM, ONLY : LNOWRITE_TEXFILE
!
#ifdef SFX_LFI
USE MODI_OPEN_WRITE_COVER_TEX_LFI
#endif
#ifdef SFX_MNH
USE MODI_MNHOPEN_WRITE_COVER_TEX
#endif
#ifdef SFX_ARO
USE MODI_AROOPEN_WRITE_COVER_TEX
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
IF (LHOOK) CALL DR_HOOK('WRITE_COVER_TEX_START',0,ZHOOK_HANDLE)
IF (LNOWRITE_TEXFILE) THEN
   NTEX=0
   IF (LHOOK) CALL DR_HOOK('WRITE_COVER_TEX_START',1,ZHOOK_HANDLE)
   RETURN
ENDIF
!
!* opening of the file
!
IF (HPROGRAM=='MESONH') THEN
#ifdef SFX_MNH
  CALL MNHOPEN_WRITE_COVER_TEX(NTEX)
#endif
END IF
!
IF (HPROGRAM=='LFI   ') THEN
#ifdef SFX_LFI
  CALL OPEN_WRITE_COVER_TEX_LFI(NTEX)
#endif
END IF
!
IF (HPROGRAM=='ASCII ' .OR. HPROGRAM=='FA    ') THEN
  NTEX=13
  OPEN(NTEX,file='class_cover_data.tex',form='formatted')
ENDIF
!
IF (HPROGRAM=='AROME ') THEN
#ifdef SFX_ARO
   CALL AROOPEN_WRITE_COVER_TEX(NTEX)
#endif
END IF
!
IF (NTEX==0 .AND. LHOOK) CALL DR_HOOK('WRITE_COVER_TEX_START',1,ZHOOK_HANDLE)
IF (NTEX==0) RETURN
!
!* writing in the file
!
WRITE(NTEX,*) '\documentstyle[11pt]{report}'
WRITE(NTEX,*) '\setlength{\textwidth}{18.0cm}'
WRITE(NTEX,*) '\setlength{\textheight}{24.cm}'
WRITE(NTEX,*) '\hoffset=-3.5cm'
WRITE(NTEX,*) '\voffset=-3.cm'
WRITE(NTEX,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
WRITE(NTEX,*) '\begin{document}'
WRITE(NTEX,*) '{\footnotesize{'
IF (LHOOK) CALL DR_HOOK('WRITE_COVER_TEX_START',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITE_COVER_TEX_START
END MODULE MODI_WRITE_COVER_TEX_START
