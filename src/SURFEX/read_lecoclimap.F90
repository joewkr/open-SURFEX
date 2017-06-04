!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #######################
      SUBROUTINE READ_LECOCLIMAP (HPROGRAM,OECOCLIMAP,OECOSG,HDIR)
!     #######################
!
USE MODI_READ_SURF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!* dummy arguments
!  ---------------
!
 CHARACTER(LEN=6),     INTENT(IN)    :: HPROGRAM  ! program calling surf. schemes
LOGICAL,              INTENT(OUT)   :: OECOCLIMAP! flag for ecoclimap
LOGICAL,              INTENT(OUT)   :: OECOSG    ! flag for ecoclimap
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
INTEGER           :: IVERSION   ! surface version
INTEGER           :: IBUGFIX    ! surface bugfix
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_LECOCLIMAP',0,ZHOOK_HANDLE)
!
YDIR = 'H'
IF (PRESENT(HDIR)) then
YDIR = HDIR
endif
!
YRECFM='VERSION'
 CALL READ_SURF(HPROGRAM,YRECFM,IVERSION,IRESP,HDIR=YDIR)
YRECFM='BUG'
 CALL READ_SURF(HPROGRAM,YRECFM,IBUGFIX,IRESP,HDIR=YDIR)
!
IF (IVERSION<1 .OR. (IVERSION==1 .AND. IBUGFIX==0)) THEN
  OECOCLIMAP = .TRUE.
ELSE
  YRECFM='ECOCLIMAP'
  CALL READ_SURF(HPROGRAM,YRECFM,OECOCLIMAP,IRESP,HDIR=YDIR)
END IF
!
IF (IVERSION<8 .OR. (IVERSION==8 .AND. IBUGFIX==0)) THEN
  OECOSG = .FALSE.
ELSE
  YRECFM='ECOSG'
  CALL READ_SURF(HPROGRAM,YRECFM,OECOSG,IRESP,HDIR=YDIR)
END IF
!
IF (LHOOK) CALL DR_HOOK('READ_LECOCLIMAP',1,ZHOOK_HANDLE)
!
!------------------------------------------------------------------------------
!
END SUBROUTINE READ_LECOCLIMAP
