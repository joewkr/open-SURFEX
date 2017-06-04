!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE INIT_READ_DATA_COVER(HPROGRAM)
!     ##########################################################################
!!
!!    PURPOSE
!!    -------
!!    initialyse flag to read or not data for covers in external binary files
!!    ecoclimapI_covers_param.bin   &   ecoclimapII_eu_covers_param.bin
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
!!    V. Masson                     Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original     09/11
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_GET_LUOUT
!
USE MODD_DATA_COVER, ONLY : LREAD_DATA_COVER
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
NAMELIST/NAM_READ_DATA_COVER/LREAD_DATA_COVER
!------------------------------------------------------------------------------
!
!*       1.    defaults
! 
IF (LHOOK) CALL DR_HOOK('INIT_READ_DATA_COVER',0,ZHOOK_HANDLE)
!
!
LREAD_DATA_COVER = .TRUE.
!
#ifdef SFX_MNH
IF (HPROGRAM=='MESONH') LREAD_DATA_COVER = .FALSE.
#endif
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
 CALL POSNAM(ILUNAM,'NAM_READ_DATA_COVER',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_READ_DATA_COVER)
!
!-------------------------------------------------------------------------------
!
!*       5.    close namelist file
! 
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
IF (LHOOK) CALL DR_HOOK('INIT_READ_DATA_COVER',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INIT_READ_DATA_COVER
