!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE OPEN_NAMELIST_FA(HPROGRAM,KLUNAM,HFILE)
!     #######################################################
!
!!****  *OPEN_NAMELIST_FA* - opens namelists files for surface (OFFLINE universe)
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
!!      Original    01/2004 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! main program
INTEGER,           INTENT(OUT) :: KLUNAM   ! logical unit of namelist
 CHARACTER(LEN=28), INTENT(IN)  :: HFILE ! FAII file to open
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
 CHARACTER(LEN=28) :: YNAM
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!* reading of namelist
!  -------------------
!
IF (LHOOK) CALL DR_HOOK('OPEN_NAMELIST_FA',0,ZHOOK_HANDLE)
IF (LEN_TRIM(HFILE)>0) THEN
  YNAM = HFILE
ELSE
  YNAM='OPTIONS.nam'
END IF
!
KLUNAM=11
OPEN(KLUNAM,FILE=YNAM,ACTION='READ',FORM="FORMATTED",POSITION="REWIND")
IF (LHOOK) CALL DR_HOOK('OPEN_NAMELIST_FA',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE OPEN_NAMELIST_FA
