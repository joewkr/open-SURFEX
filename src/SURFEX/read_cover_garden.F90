!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ######################################################################
      SUBROUTINE READ_COVER_GARDEN (&
                                    HPROGRAM,OGARDEN,HDIR)
!     ######################################################################
!
!
!
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
!
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
LOGICAL,           INTENT(OUT) :: OGARDEN   ! T: Definition of urban green areas
 CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: HDIR
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
IF (LHOOK) CALL DR_HOOK('READ_COVER_GARDEN',0,ZHOOK_HANDLE)
!
YDIR = 'H'
IF (PRESENT(HDIR)) YDIR = HDIR
!
YRECFM='VERSION'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,IVERSION,IRESP,HDIR=YDIR)
!
IF (IVERSION<=5) THEN
  OGARDEN = .FALSE.
ELSE
  YRECFM='GARDEN'
  CALL READ_SURF(&
                HPROGRAM,YRECFM,OGARDEN,IRESP,HDIR=YDIR)
END IF
IF (LHOOK) CALL DR_HOOK('READ_COVER_GARDEN',1,ZHOOK_HANDLE)
!
!------------------------------------------------------------------------------
!
END SUBROUTINE READ_COVER_GARDEN
