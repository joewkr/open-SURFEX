!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################################
      SUBROUTINE OPEN_WRITE_COVER_TEX_LFI(KTEX)
!     ##################################
!
!!****  *OPEN_WRITE_COVER_TEX_LFI* - opens cover listing file (in MESONH universe)
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
USE MODI_OPEN_FILE_LFI
!
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER, INTENT(OUT) :: KTEX ! logical unit of Tex file
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP
 CHARACTER(LEN=28) :: YTEX           ! name of tex file
 CHARACTER(LEN=11) :: YFORM
 CHARACTER(LEN=9)  :: YACTION
 CHARACTER(LEN=6)  :: YACCESS
 CHARACTER(LEN=6)  :: YPOSITION
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*       5.     Prints of cover parameters in a tex file
!               ----------------------------------------
!
IF (LHOOK) CALL DR_HOOK('OPEN_WRITE_COVER_TEX_LFI',0,ZHOOK_HANDLE)
YTEX  = 'class_cover_data.tex'
YFORM = 'FORMATTED'
YACTION = 'READWRITE'
YACCESS  = '   '
YPOSITION  = 'ASIS'
 CALL OPEN_FILE_LFI(KTEX,YTEX,YFORM,YACTION,YACCESS,YPOSITION,IRESP)
IF (LHOOK) CALL DR_HOOK('OPEN_WRITE_COVER_TEX_LFI',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE OPEN_WRITE_COVER_TEX_LFI
