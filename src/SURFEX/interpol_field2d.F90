!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ################################################
      SUBROUTINE INTERPOL_FIELD2D (UG, U, &
                                   HPROGRAM,KLUOUT,KCODE,PFIELD,HFIELD,PDEF,KNPTS)
!     ################################################
!
!!**** *INTERPOL_FIELD* initializes coordinate system for spline interpolation
!!
!!    PURPOSE
!!    -------
!!
!!    The points are all on only one grid (defined with the coordinates
!!    of all the points). The code to apply for each point is:
!!
!!    KCODE>0 : data point (with field valid for interpolation)
!!    KCODE=-1: point to ignore
!!    KCODE=0 : point to interpolate
!!
!!
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson          Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    01/2004
!!    Modification
!!      A. Alias        07/2013 add MODI_ABOR1_SFX
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO
!
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_SURF_PAR,  ONLY : XUNDEF
!
USE MODI_GET_GRID_COORD
USE MODI_INTERPOL_NPTS
USE MODI_SUM_ON_ALL_PROCS
USE MODI_ABOR1_SFX
USE MODI_GET_INTERP_HALO
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
!
 CHARACTER(LEN=6),        INTENT(IN)   :: HPROGRAM ! host program
INTEGER,                 INTENT(IN)   :: KLUOUT   ! output listing
INTEGER,DIMENSION(:),  INTENT(INOUT)  :: KCODE    ! code for each point
                                                  ! >0 point used for interpolation
                                                  !  0 point to interpolate
                                                  ! -1 point not used
                                                  ! -2 point not used
!                                                 ! -3 if spline is no computed
!                                                 ! for this point
REAL,   DIMENSION(:,:),INTENT(INOUT)  :: PFIELD   ! pgd field on grid mesh.
 CHARACTER(LEN=*),        INTENT(IN)   :: HFIELD   ! name of the field for prints
REAL,DIMENSION(:),OPTIONAL, INTENT(IN):: PDEF     ! default value if not enough data
INTEGER, OPTIONAL,       INTENT(IN)   :: KNPTS    ! number of points to interpolate with

!
!*    0.2    Declaration of local variables
!            ------------------------------
!
REAL, DIMENSION(U%NDIM_FULL)   :: ZX             ! coordinate used for
REAL, DIMENSION(U%NDIM_FULL)   :: ZY             ! splines interpolation
REAL, DIMENSION(SIZE(PFIELD,2)):: ZDEF           ! default value for field
INTEGER                        :: INPTS         ! number of points to interpolate with
INTEGER :: IHALO, INEAR_NBR
!
INTEGER                        :: JLOOP          ! loop counter
!
INTEGER :: IERR0
INTEGER                        :: IERR1          ! number of points interpolated
INTEGER                        :: IERR2          ! number of points not interpolated in the end
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INTERPOL_FIELD:INTERPOL_FIELD2D',0,ZHOOK_HANDLE)
!
INPTS = 3
IF (PRESENT(KNPTS)) INPTS = KNPTS
!
ZDEF = XUNDEF
IF (PRESENT(PDEF)) ZDEF = PDEF
!
!*    2.     Miscellaneous Initializations
!            -----------------------------
!
 CALL GET_GRID_COORD(UG%G%CGRID, UG%G%NGRID_PAR, UG%G%XGRID_PAR, U%NSIZE_FULL, &
                     KLUOUT,KL=U%NDIM_FULL,HGRID=UG%G%CGRID,PGRID_PAR=UG%XGRID_FULL_PAR,&
                        PX=ZX,PY=ZY)
!
!-------------------------------------------------------------------------------
!
!*    5.     Interpolation with 3 nearest points
!            -----------------------------------
!
IERR0 = SUM_ON_ALL_PROCS(HPROGRAM,UG%G%CGRID,KCODE(:)==0)
!
 CALL GET_INTERP_HALO(HPROGRAM,UG%G%CGRID,IHALO)
!
IF (IHALO/=0) THEN
  INEAR_NBR = (2*IHALO+1)**2
ELSE
  INEAR_NBR = U%NDIM_FULL
ENDIF
!
 CALL INTERPOL_NPTS(UG, U, &
                    HPROGRAM,KLUOUT,INPTS,KCODE,ZX,ZY,PFIELD,INEAR_NBR)
!
!-------------------------------------------------------------------------------
!
!*    6.     Final check
!            -----------
!
IERR1 = SUM_ON_ALL_PROCS(HPROGRAM,UG%G%CGRID,KCODE(:)==0)
IERR2 = SUM_ON_ALL_PROCS(HPROGRAM,UG%G%CGRID,KCODE(:)==-4)
!
IF (NRANK==NPIO) THEN
  !
  IF (IERR1>0 .OR. IERR2>0) THEN
    !
    WRITE(KLUOUT,*) ' '
    WRITE(KLUOUT,*) ' Interpolation of field : ',HFIELD
    WRITE(KLUOUT,*) ' ----------------------'
    WRITE(KLUOUT,*) ' '
    WRITE(KLUOUT,*) ' Number of points interpolated with ',INPTS,' nearest points: ', &
                      IERR1
    !
    !
    IF (IERR2>0) THEN
      WRITE(KLUOUT,*) ' Number of points that could not be interpolated : ', &
                        IERR2
                !if all points were scanned or if no point could be interpolated 
      IF (PRESENT(PDEF) .AND. (INEAR_NBR>=U%NDIM_FULL .OR. IERR2==IERR0)) THEN          
        DO JLOOP=1,SIZE(PFIELD,2)
          WRITE(KLUOUT,*) ' For these points, the default value (',ZDEF(JLOOP),') is set.'
        ENDDO
      ELSE
        WRITE(KLUOUT,*) ' Please provide data with better resolution'
        WRITE(KLUOUT,*) ' Or define a higher halo value             '
      END IF
    END IF
    !
  END IF
  !
END IF
!
IF (IERR2>0) THEN
  !
  IF (.NOT.PRESENT(PDEF) .OR. (INEAR_NBR<U%NDIM_FULL .AND. IERR2/=IERR0)) &
          CALL ABOR1_SFX('Some points lack data and are too far away from other points. &
                Please define a higher halo value in NAM_IO_OFFLINE.')
  !
ENDIF
!
IF (COUNT(KCODE(:)==-4)>0) THEN
  !
  DO JLOOP=1,SIZE(PFIELD,2)
    IF (ZDEF(JLOOP)/=XUNDEF) THEN
      WHERE(KCODE(:)==-4)
        PFIELD(:,JLOOP)=ZDEF(JLOOP)
      END WHERE
    ENDIF
  END DO
  !
END IF
!
IF (LHOOK) CALL DR_HOOK('INTERPOL_FIELD:INTERPOL_FIELD2D',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE INTERPOL_FIELD2D
