!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE OPEN_FILE(HPROGRAM,KUNIT,HFILE,HFORM,HACTION,HACCESS,KRECL)
!     #######################################################
!
!!****  *OPEN_FILE* - routine to open a namelist file
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
#if defined(SFX_ASC) || defined(SFX_ARO) || defined(SFX_MNH) || defined(SFX_NC)
USE MODI_OPEN_FILE_ASC
#endif
#ifdef SFX_FA
USE MODI_OPEN_FILE_FA
#endif
#ifdef SFX_LFI
USE MODI_OPEN_FILE_LFI
#endif
#ifdef SFX_NC
USE MODI_OPEN_FILE_NC
#endif
#ifdef SFX_OL
USE MODI_OPEN_FILE_OL
#endif
#ifdef SFX_MNH
USE MODI_OPEN_FILE_MNH
#endif
!
USE YOMHOOK ,ONLY : LHOOK, DR_HOOK
USE PARKIND1 ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)           :: HPROGRAM ! main program
INTEGER,           INTENT(OUT)          :: KUNIT    ! logical unit
 CHARACTER(LEN=*),  INTENT(IN)           :: HFILE    ! file to open
 CHARACTER(LEN=*),  INTENT(IN)           :: HFORM    ! type of file
 CHARACTER(LEN=*),  INTENT(IN), OPTIONAL :: HACTION  ! action
 CHARACTER(LEN=*),  INTENT(IN), OPTIONAL :: HACCESS  ! access type
INTEGER,           INTENT(IN), OPTIONAL :: KRECL    ! record length

!
!*       0.2   Declarations of local variables
!              -------------------------------
!
 CHARACTER(LEN=28) :: YFILE
 CHARACTER(LEN=11) :: YFORM
 CHARACTER(LEN=9)  :: YACTION
 CHARACTER(LEN=6)  :: YACCESS
INTEGER           :: IRECL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('OPEN_FILE',0,ZHOOK_HANDLE)
!
YFILE = HFILE
YFORM = HFORM
IF (PRESENT(HACTION)) THEN
  YACTION = HACTION
ELSE
  YACTION = 'READWRITE'
END IF
IF (PRESENT(HACCESS)) THEN
  YACCESS = HACCESS
ELSE
  YACCESS = '      '
END IF
IF (PRESENT(KRECL)) THEN
  IRECL = KRECL
ELSE
  IRECL = 0
END IF
!
IF (HPROGRAM=='MESONH') THEN
#ifdef SFX_MNH
  CALL OPEN_FILE_MNH(KUNIT,YFILE,YFORM,YACTION,YACCESS,IRECL)
#endif
ELSE IF (HPROGRAM=='OFFLIN') THEN
#ifdef SFX_OL
  CALL OPEN_FILE_OL(KUNIT,YFILE,YFORM,YACTION,YACCESS,IRECL)
#endif
ELSE IF (HPROGRAM=='ASCII ' .OR. HPROGRAM=='AROME ') THEN
#if defined(SFX_ASC) || defined(SFX_ARO) || defined(SFX_MNH) || defined(SFX_NC)
  CALL OPEN_FILE_ASC(KUNIT,YFILE,YFORM,YACTION,YACCESS,IRECL)
#endif
ELSE IF (HPROGRAM=='FA    ') THEN
#ifdef SFX_FA
  CALL OPEN_FILE_FA(KUNIT,YFILE,YFORM,YACTION,YACCESS,IRECL)
#endif
ELSE IF (HPROGRAM=='NC    ') THEN
#ifdef SFX_NC
  CALL OPEN_FILE_NC(KUNIT,YFILE,YFORM,YACTION,YACCESS,IRECL)
#endif
ELSE IF (HPROGRAM=='LFI   ') THEN
#ifdef SFX_LFI
  CALL OPEN_FILE_LFI(KUNIT,YFILE,YFORM,YACTION,YACCESS,'ASIS  ',IRECL)
#endif
END IF
!
IF (LHOOK) CALL DR_HOOK('OPEN_FILE',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE OPEN_FILE
