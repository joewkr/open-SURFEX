!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE OPEN_FILE_OL(KUNIT,HFILE,HFORM,HACTION,HACCESS,KRECL)
!     #######################################################
!
!!****  *OPEN_FILE_OL* - routine to open a file
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
!       10/2014 : add 'status='old' E. Martin
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
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER,           INTENT(OUT):: KUNIT    ! logical unit
 CHARACTER(LEN=28), INTENT(IN) :: HFILE    ! file to open
 CHARACTER(LEN=11), INTENT(IN) :: HFORM    ! type of file
 CHARACTER(LEN=9),  INTENT(IN) :: HACTION  ! action
 CHARACTER(LEN=6),  INTENT(IN) :: HACCESS  ! access type
INTEGER,           INTENT(IN) :: KRECL    ! record length
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('OPEN_FILE_OL',0,ZHOOK_HANDLE)
KUNIT = 20
!
IF (HFORM=='FORMATTED') THEN
  OPEN(UNIT=KUNIT,FILE=HFILE,ACTION=HACTION,   &
         FORM=HFORM, STATUS='OLD', ERR=100            )  
ELSE 
  IF (HACCESS=='DIRECT') THEN
    OPEN(UNIT=KUNIT,FILE=HFILE,ACTION=HACTION,                       &
                   FORM=HFORM,ACCESS=HACCESS,RECL=KRECL, STATUS='OLD',ERR=100 )  
  ELSE
    OPEN(UNIT=KUNIT,FILE=HFILE,ACTION=HACTION, &
           FORM=HFORM, ACCESS=HACCESS, STATUS='OLD', ERR=100           )  
  END IF
END IF
!
IF (LHOOK) CALL DR_HOOK('OPEN_FILE_OL',1,ZHOOK_HANDLE)
RETURN
100 CONTINUE
 CALL ABOR1_SFX('OPEN_FILE_OL: ERROR WHEN OPENING FILE '//HFILE)
IF (LHOOK) CALL DR_HOOK('OPEN_FILE_OL',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE OPEN_FILE_OL
