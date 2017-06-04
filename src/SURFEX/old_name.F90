!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE OLD_NAME (HPROGRAM,HRECIN,HRECOUT,HDIR)
!     #######################################################
!
!!****  *OLD_NAME* - get the old name of a field for reading in an old SURFEX file
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
!!      V. Masson    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    08/2011 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
!
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_READ_SURF
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! main program
 CHARACTER(LEN=12), INTENT(IN)  :: HRECIN   ! name of field to be read
 CHARACTER(LEN=12), INTENT(OUT) :: HRECOUT  ! name of field to be read is old file
 CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: HDIR
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
 CHARACTER(LEN=1) :: YDIR
INTEGER :: IVERSION  ! version of the old file
INTEGER :: IBUGFIX   ! bugfix  of the old file
!
INTEGER :: IRESP
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('OLD_NAME',0,ZHOOK_HANDLE)
!
YDIR = 'H'
IF (PRESENT(HDIR)) YDIR = HDIR
!
HRECOUT = HRECIN
IF (HRECIN=='COVER_LIST') THEN
  CALL READ_SURF(HPROGRAM,'VERSION',IVERSION,IRESP,HDIR=YDIR)
  CALL READ_SURF(HPROGRAM,'BUG', IBUGFIX ,IRESP,HDIR=YDIR)
  IF (IVERSION<7 .OR. (IVERSION==7 .AND. IBUGFIX==0)) HRECOUT='COVER'
END IF
!
IF (LHOOK) CALL DR_HOOK('OLD_NAME',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
END SUBROUTINE OLD_NAME
