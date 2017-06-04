!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE DEFAULT_SCHEMES(HPROGRAM,HNATURE,HSEA,HTOWN,HWATER)
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
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
#ifdef SFX_MNH
USE MODI_DEFAULT_SCHEMES_MNH
#endif
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
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_SCHEMES',0,ZHOOK_HANDLE)
IF (HPROGRAM=='MESONH') THEN
#ifdef SFX_MNH
  CALL DEFAULT_SCHEMES_MNH(HNATURE,HSEA,HTOWN,HWATER)
#endif
ELSE
  HNATURE = 'ISBA  '
  HSEA    = 'SEAFLX'
  HTOWN   = 'TEB   '
  HWATER  = 'WATFLX'
END IF
IF (LHOOK) CALL DR_HOOK('DEFAULT_SCHEMES',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DEFAULT_SCHEMES
