!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_PGD_SCHEMES(HPROGRAM,HNATURE,HSEA,HTOWN,HWATER)
!     ######################################
!!
!!    PURPOSE
!!    -------
!!   initializes the surface SCHEMES.
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
!!    V. Masson                   Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original     13/10/03
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODI_DEFAULT_SCHEMES
USE MODI_TEST_NAM_VAR_SURF
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
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling READ_PGD
 CHARACTER(LEN=6),  INTENT(OUT) :: HNATURE  ! scheme for natural surfaces
 CHARACTER(LEN=6),  INTENT(OUT) :: HSEA     ! scheme for sea
 CHARACTER(LEN=6),  INTENT(OUT) :: HTOWN    ! scheme for towns
 CHARACTER(LEN=6),  INTENT(OUT) :: HWATER   ! scheme for inland water
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
!------------------------------------------------------------------------------
!
!*       1.    defaults
! 
IF (LHOOK) CALL DR_HOOK('READ_PGD_SCHEMES',0,ZHOOK_HANDLE)
 CALL DEFAULT_SCHEMES(HPROGRAM,CNATURE,CSEA,CTOWN,CWATER)
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
!*       4.    check of file type
! 
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CNATURE',CNATURE,'NONE  ','ISBA  ','TSZ0  ','FLUX  ')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CSEA   ',CSEA   ,'NONE  ','SEAFLX','FLUX  ')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CTOWN  ',CTOWN  ,'NONE  ','TEB   ','FLUX  ')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CWATER ',CWATER ,'NONE  ','WATFLX','FLUX  ','FLAKE ')
!
HNATURE = CNATURE
HSEA    = CSEA
HTOWN   = CTOWN
HWATER  = CWATER
!------------------------------------------------------------------------------
!
!*       5.    close namelist file
! 
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
IF (LHOOK) CALL DR_HOOK('READ_PGD_SCHEMES',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_PGD_SCHEMES
