!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_SNAP_TEMP_PROFILE(HPROGRAM,HFILENAME,PSNAP_COEF, &
                                       KSNAP,KTPS,HSNAP_TIME_REF      )
!     ##############################################################
!
!!**** *PGD_SNAP_TEMP_PROFILE* reads a temporal emission profile
!!
!!    PURPOSE
!!    -------
!!
!!    AUTHOR
!!    ------
!!
!!    S. QUEGUINER          Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    09/2011 
!!    A. Alias    07/2013 CONTINUE procedure for compilation on beaufix
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!

USE MODI_OPEN_FILE
USE MODI_CLOSE_FILE
USE MODI_GET_LUOUT
USE MODI_TEST_NAM_VAR_SURF
!
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=6),     INTENT(IN) :: HPROGRAM      ! Type of program
 CHARACTER(LEN=28),    INTENT(IN) :: HFILENAME     ! Name of the field file.
REAL, DIMENSION(:,:), INTENT(OUT):: PSNAP_COEF    ! Snap coefficient
INTEGER,              INTENT(IN) :: KTPS          ! Number of time step
INTEGER,              INTENT(IN) :: KSNAP         ! Number of snaps
 CHARACTER(LEN=5), OPTIONAL, INTENT(OUT):: HSNAP_TIME_REF ! Reference time
!                                                        ! 'UTC  ' : UTC   time
!                                                        ! 'SOLAR' : SOLAR time
!                                                        ! 'LEGAL' : LEGAL time
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                           :: IUNIT      ! logical unit
INTEGER                           :: JSNAP      ! loop counter on snaps
 CHARACTER(LEN=200) :: YCOMMENT
REAL,    DIMENSION(KTPS)          :: ZSNAP_COEF ! snap temporal coefficient
INTEGER                           :: ISNAP      ! snap number
INTEGER                           :: ILUOUT     ! output listing
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
!*    1.      Open the file
!             -------------
!
IF (LHOOK) CALL DR_HOOK('PGD_SNAP_TEMP_PROFILE',0,ZHOOK_HANDLE)
!
 CALL OPEN_FILE(HPROGRAM,IUNIT,HFILENAME,'FORMATTED',HACTION='READ')
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!----------------------------------------------------------------------------
READ(IUNIT,'(A)') YCOMMENT
!----------------------------------------------------------------------------
!
!*    2.     Reading of time reference for hourly profiles
!            ---------------------------------------------
!
IF (PRESENT(HSNAP_TIME_REF)) THEN
  READ(IUNIT,'(A)') HSNAP_TIME_REF
  CALL TEST_NAM_VAR_SURF(ILUOUT,'CSNAP_TIME_REF',HSNAP_TIME_REF,'UTC  ','SOLAR','LEGAL')
END IF
!
!----------------------------------------------------------------------------
READ(IUNIT,'(A)') YCOMMENT
!----------------------------------------------------------------------------
!
!*    3.     Reading of snaps temporal coefficients
!            --------------------------------------
!
PSNAP_COEF(:,:)=0.
!
DO JSNAP=1,KSNAP
  READ(IUNIT,*,END=2000) ISNAP,ZSNAP_COEF(1:KTPS) 
  PSNAP_COEF(:,ISNAP)=ZSNAP_COEF(:)
ENDDO
!
!----------------------------------------------------------------------------
!
!*    8.    Closing of the data file
!           ------------------------
!
2000 CONTINUE
CALL CLOSE_FILE (HPROGRAM,IUNIT)
!
IF (LHOOK) CALL DR_HOOK('PGD_SNAP_TEMP_PROFILE',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_SNAP_TEMP_PROFILE
