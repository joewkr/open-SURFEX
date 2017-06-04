!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #######################
      SUBROUTINE READ_LCLIM_LAI (&
                                 HPROGRAM,OCLIM_LAI)
!     #######################
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
 CHARACTER(LEN=6),     INTENT(IN)    :: HPROGRAM  ! program calling surf. schemes
LOGICAL,              INTENT(OUT)   :: OCLIM_LAI ! flag for use of climatologic LAI
!
!
!* local variables
!  ---------------
!
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
IF (LHOOK) CALL DR_HOOK('READ_LCLIM_LAI',0,ZHOOK_HANDLE)
YRECFM='VERSION'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,IVERSION,IRESP)
YRECFM='BUG'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,IBUGFIX,IRESP)
!
IF (IVERSION<4 .OR. IVERSION==4 .AND. IBUGFIX==2) THEN
  OCLIM_LAI = .FALSE.
ELSE
  YRECFM='LCLIM_LAI'
  CALL READ_SURF(&
                HPROGRAM,YRECFM,OCLIM_LAI,IRESP)
END IF
IF (LHOOK) CALL DR_HOOK('READ_LCLIM_LAI',1,ZHOOK_HANDLE)
!
!------------------------------------------------------------------------------
!
END SUBROUTINE READ_LCLIM_LAI
