!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_PGD_COVER_GARDEN(HPROGRAM,OGARDEN)
!     ##########################################################################
!!
!!    PURPOSE
!!    -------
!!    initialyse change water (not lake) to nature and/or town to rock keys
!!
!!    METHOD
!!    ------
!!   
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    B. Decharme                   Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original     05/03/09
!!
!!    A. Lemonsu        05/2009         Key for vegetation in TEB (TEB-Veg)
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_GET_LUOUT
!
USE MODN_PGD_SCHEMES
!
USE MODE_POS_SURF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM         ! program calling READ_PGD
LOGICAL,           INTENT(OUT) :: OGARDEN          ! T: Urban green areas
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER           :: ILUOUT     ! logical unit
INTEGER           :: ILUNAM     ! logical unit
!
LOGICAL           :: GFOUND
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*    0.2    local namelist variables
!            ------------------------
!
!------------------------------------------------------------------------------
!
!*       1.    defaults
! 
IF (LHOOK) CALL DR_HOOK('READ_PGD_COVER_GARDEN',0,ZHOOK_HANDLE)
!
LGARDEN = .FALSE.
!
!------------------------------------------------------------------------------
!
!*       2.    opening of namelist
! 
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
!-------------------------------------------------------------------------------
!
!*       3.    reading of namelist
! 
 CALL POSNAM(ILUNAM,'NAM_PGD_SCHEMES',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_PGD_SCHEMES)
!
!-------------------------------------------------------------------------------
!
!*       4.    initialize keys
! 
OGARDEN = LGARDEN
!
!------------------------------------------------------------------------------
!
!*       5.    close namelist file
! 
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
IF (LHOOK) CALL DR_HOOK('READ_PGD_COVER_GARDEN',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_PGD_COVER_GARDEN
