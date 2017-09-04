!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_BINLLVFAST (UG, U, USS, &
                                  HPROGRAM,HSUBROUTINE,HFILENAME)
!     ##############################################################
!
!!**** *READ_BINLLVFAST* reads a binary latlonvalue file and call treatment
!!                   subroutine : optimized version of READ_BINLLV routine.
!!
!!    PURPOSE
!!    -------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    D. Gazen          L.A.
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    29/11/2002
!!                03/2004  externalization (V. Masson)
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_PGD_GRID,   ONLY : LLATLONMASK
!
USE MODI_OPEN_FILE
USE MODI_CLOSE_FILE
USE MODI_PT_BY_PT_TREATMENT
USE MODI_GET_LUOUT
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
!
 CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM      ! Type of program
 CHARACTER(LEN=6),  INTENT(IN) :: HSUBROUTINE   ! Name of the subroutine to call
 CHARACTER(LEN=28), INTENT(IN) :: HFILENAME     ! Name of the field file.
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                                :: IGLB       ! logical units
INTEGER                                :: JLAT, JLON ! indexes of LLATLONMASK array
INTEGER                                :: INELT      ! number of data points in file
INTEGER                                :: ICPT       ! number of data points to be computed
REAL,DIMENSION(:,:),ALLOCATABLE,TARGET :: ZLLV       ! ZLLV(1,:) :: latitude of data points
                                                     ! ZLLV(2,:) :: longitude of data points
                                                     ! ZLLV(3,:) :: value of data points
REAL,DIMENSION(:,:),POINTER            :: ZLLVWORK   ! point on ZLLV array
INTEGER                                :: JI         ! loop counter
!
INTEGER                                :: ILUOUT     ! output listing
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
!
!*    1.      Open the global file
!             --------------------
!
IF (LHOOK) CALL DR_HOOK('READ_BINLLVFAST',0,ZHOOK_HANDLE)
 CALL OPEN_FILE(HPROGRAM,IGLB,HFILENAME,'UNFORMATTED',HACTION='READ')
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!----------------------------------------------------------------------------
!
!*    3.     Reading of a data point
!            -----------------------
!
READ(IGLB) INELT ! number of data points
ALLOCATE(ZLLV(3,INELT))
READ(IGLB) ZLLV
!
!----------------------------------------------------------------------------
!
!*    4.     Test if point is in the domain
!            ------------------------------
!
ZLLV(2,:) = ZLLV(2,:)+NINT((180.-ZLLV(2,:))/360.)*360.
!
ICPT = 0
DO JI=1,INELT
  JLAT = 1 + INT( ( ZLLV(1,JI)+ 90. ) * 2. )
  JLAT = MIN(JLAT,360)
  JLON = 1 + INT( ( ZLLV(2,JI)      ) * 2. )
  JLON = MIN(JLON,720)
  IF (LLATLONMASK(JLON,JLAT)) THEN
    ICPT = ICPT+1
    ZLLV(:,ICPT) = ZLLV(:,JI)
  END IF
END DO
!
!-------------------------------------------------------------------------------
!
!*    5.     Call to the adequate subroutine (point by point treatment)
!            ----------------------------------------------------------
!
IF (ICPT > 0) THEN
  ZLLVWORK=>ZLLV(:,1:ICPT)
  CALL PT_BY_PT_TREATMENT(UG, U, USS, &
                          ILUOUT,ZLLVWORK(1,:),ZLLVWORK(2,:),ZLLVWORK(3,:),HSUBROUTINE)
END IF
!
!----------------------------------------------------------------------------
!
!*    6.    Closing of the data file
!           ------------------------
!
 CALL CLOSE_FILE (HPROGRAM,IGLB)
!
!-------------------------------------------------------------------------------
!
DEALLOCATE(ZLLV)
IF (LHOOK) CALL DR_HOOK('READ_BINLLVFAST',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_BINLLVFAST
