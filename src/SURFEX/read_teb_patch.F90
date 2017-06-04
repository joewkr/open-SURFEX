!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #######################
      SUBROUTINE READ_TEB_PATCH (HFILEPGD,HFILEPGDTYPE,KVERSION,KBUGFIX,KTEB_PATCH,HDIR)
!     #######################
!
!
!
!
USE MODI_READ_SURF
!
USE MODI_OPEN_AUX_IO_SURF
USE MODI_CLOSE_AUX_IO_SURF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!* dummy arguments
!  ---------------
!
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILEPGD     ! name of file
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILEPGDTYPE ! type of file
INTEGER, INTENT(IN) :: KVERSION
INTEGER, INTENT(IN) :: KBUGFIX
INTEGER,            INTENT(OUT) :: KTEB_PATCH! number of TEB patches
 CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: HDIR
!
!
!* local variables
!  ---------------
!
 CHARACTER(LEN=1) :: YDIR
 CHARACTER(LEN=12) :: YRECFM     ! Name of the article to be read
INTEGER           :: IRESP      ! reading return code
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_TEB_PATCH',0,ZHOOK_HANDLE)
!
YDIR = 'H'
IF (PRESENT(HDIR)) YDIR = HDIR
!
IF (KVERSION<7 .OR. (KVERSION==7 .AND. KBUGFIX<=2)) THEN
  KTEB_PATCH = 1
ELSE
  YRECFM='TEB_PATCH'
  CALL READ_SURF(HFILEPGDTYPE,YRECFM,KTEB_PATCH,IRESP)
END IF
!
IF (LHOOK) CALL DR_HOOK('READ_TEB_PATCH',1,ZHOOK_HANDLE)
!
!------------------------------------------------------------------------------
!
END SUBROUTINE READ_TEB_PATCH
