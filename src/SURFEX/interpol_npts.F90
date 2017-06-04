!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE INTERPOL_NPTS (UG, U, &
                                HPROGRAM,KLUOUT,KNPTS,KCODE,PX,PY,PFIELD,KNEAR_NBR)
!     #########################################################
!
!!**** *INTERPOL_NPTS* interpolates with ###ine f77 programs a 2D field
!!                           from all grid points valid values
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
!!    METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson          Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    03/2004
!!    Modification
!!    B. Decharme  2014  scan all point case if gaussien grid or NHALO = 0
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_SURFEX_OMP, ONLY : NBLOCK
USE MODD_PGDWORK, ONLY : NSIZE_ALL
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, NCOMM, NPROC, IDX_I, NINDEX,NNUM
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODI_GATHER_AND_WRITE_MPI
USE MODI_READ_AND_SEND_MPI
USE MODI_GET_NEAR_MESHES
USE MODI_SUM_ON_ALL_PROCS
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6),      INTENT(IN)     :: HPROGRAM ! host program
INTEGER,               INTENT(IN)     :: KLUOUT   ! output listing
INTEGER,               INTENT(IN)     :: KNPTS    ! number of points to interpolate with
INTEGER,DIMENSION(:),  INTENT(INOUT)  :: KCODE    ! code for each point
                                                  ! >0 point used for interpolation
                                                  !  0 point to interpolate
                                                  ! -1 point not used
                                                  ! -2 point not used
!                                                 ! -3 if spline is no computed
!                                                 ! for this point
REAL,   DIMENSION(:),  INTENT(IN)     :: PX       ! x of each grid mesh.
REAL,   DIMENSION(:),  INTENT(IN)     :: PY       ! y of each grid mesh.
REAL,   DIMENSION(:,:),INTENT(INOUT)  :: PFIELD   ! pgd field on grid mesh.
INTEGER, INTENT(IN) :: KNEAR_NBR
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER, DIMENSION(0:NPROC-1) :: ISIZE
INTEGER :: IP, ICPT
INTEGER                                     :: IL1 ! number of points
INTEGER                                     :: JL ! loop counter on points to initialize
INTEGER                                     :: JP, JK, JKK ! loops counter on KNPTS
REAL, DIMENSION(:), ALLOCATABLE :: ZDIST ! square distance between two interpolating and interpolated points
!
REAL, DIMENSION(:,:), ALLOCATABLE       :: ZNDIST  ! 3 nearest square distances
REAL, DIMENSION(:,:), ALLOCATABLE :: ZNVAL  ! 3 corresponding field values
REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: ZFIELD, ZFIELD2
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZFIELD3
REAL, DIMENSION(:), ALLOCATABLE :: ZX, ZY
!
INTEGER, DIMENSION(:,:), ALLOCATABLE :: INEAR
INTEGER, DIMENSION(:,:), ALLOCATABLE :: INEAR_ALL
INTEGER, DIMENSION(:,:), ALLOCATABLE :: ININD
INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: ININD0  ! 3 corresponding field values
INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: ININD_ALL
!
INTEGER, DIMENSION(:), ALLOCATABLE :: ITNUM
!
REAL         :: ZSUM, ZMAXVALDIS
INTEGER                            :: JLIST          ! loop counter on points to interpolate
INTEGER                            :: ICOUNT, IL2   ! counter
INTEGER                            :: INPTS
INTEGER, DIMENSION(:), ALLOCATABLE :: IINDEX       ! list of index to scan
INTEGER                            :: INUM        ! halo available
#ifdef SFX_MPI
INTEGER, DIMENSION(MPI_STATUS_SIZE) :: ISTATUS
#endif
INTEGER :: INFOMPI
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INTERPOL_NPTS_1',0,ZHOOK_HANDLE)
!
!points to interpolate and that can be used for interpolation on the complete grid
ALLOCATE(NSIZE_ALL(U%NDIM_FULL,1))
CALL GATHER_AND_WRITE_MPI(KCODE,NSIZE_ALL(:,1))
!
!...known by all tasks
IF (NPROC>1) THEN
#ifdef SFX_MPI
  CALL MPI_BCAST(NSIZE_ALL,U%NDIM_FULL*KIND(NSIZE_ALL)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
#endif
ENDIF
!
IF (ALL(NSIZE_ALL/=0)) THEN
  DEALLOCATE(NSIZE_ALL)
  CALL DR_HOOK('INTERPOL_NPTS_1',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
!
IP = COUNT(KCODE(:)==0)
!
IL1 = SIZE(PFIELD,1)
IL2 = SIZE(PFIELD,2)
!
!PX, PY: coordinates of all the points
!ZX, ZY: coordinates of the points in this task
ALLOCATE(ZX(IL1),ZY(IL1))
 CALL READ_AND_SEND_MPI(PX,ZX)
 CALL READ_AND_SEND_MPI(PY,ZY)
!
IF (KNEAR_NBR/=U%NDIM_FULL) THEN
  IF (.NOT.ASSOCIATED(UG%NNEAR)) THEN
    ALLOCATE(UG%NNEAR(IL1,KNEAR_NBR))
    !seach near meshes in the complete grid (xgrid_full_par) for this task
    CALL GET_NEAR_MESHES(UG%G%CGRID,UG%NGRID_FULL_PAR,U%NDIM_FULL,UG%XGRID_FULL_PAR,KNEAR_NBR,UG%NNEAR)
  ENDIF
ENDIF 
!
!
!
IF (LHOOK) CALL DR_HOOK('INTERPOL_NPTS_1',1,ZHOOK_HANDLE)
IF (LHOOK) CALL DR_HOOK('INTERPOL_NPTS_2',0,ZHOOK_HANDLE)
!
ALLOCATE(IINDEX(KNEAR_NBR))
IINDEX    (:) = 0
!
IF (KNEAR_NBR==U%NDIM_FULL) THEN

  ICOUNT = 0
  DO JL=1,U%NDIM_FULL
    !is the neareast point available to interpolation
    IF (NSIZE_ALL(JL,1)>0) THEN  
      ICOUNT = ICOUNT+1
      IINDEX(ICOUNT) = JL
    END IF
  END DO

  !did we found enough points for interpolate
  IF (ICOUNT>=1) THEN
    INPTS = MIN(KNPTS,ICOUNT)
  ELSE
    WHERE(KCODE(:)==0) KCODE(:) = -4
  END IF
  ALLOCATE(ZDIST (ICOUNT))
ELSE
  ALLOCATE(ZDIST (KNEAR_NBR))
ENDIF
!
ZDIST     (:) = 0.
!
IF (LHOOK) CALL DR_HOOK('INTERPOL_NPTS_2',1,ZHOOK_HANDLE)
IF (LHOOK) CALL DR_HOOK('INTERPOL_NPTS_3',0,ZHOOK_HANDLE)
!
!indexes of points used for interpolation, for each point to interpolate
ALLOCATE(ININD (IP,KNPTS))
ININD(:,:) = 0
!distances of the points used for interpolation
ALLOCATE(ZNDIST(IP,0:KNPTS))
ZNDIST (:,1:KNPTS) = 1.E20
ZNDIST (:,0) = 0.
!
ICPT=0
!loop on points for this task
DO JL=1,IL1
  !
  !does this point need to be interpolated? 
  IF (KCODE(JL)/=0) CYCLE
  !
  IF (KNEAR_NBR/=U%NDIM_FULL) THEN

    ICOUNT = 0
    DO JK=1,KNEAR_NBR
      IF (UG%NNEAR(JL,JK)>0) THEN
        !is the neareast point available to interpolation
        IF (NSIZE_ALL(UG%NNEAR(JL,JK),1)>0) THEN  
          ICOUNT = ICOUNT+1
          IINDEX(ICOUNT) = UG%NNEAR(JL,JK)
        END IF
      END IF
    END DO
    !
    !did we found enough points for interpolate
    IF (ICOUNT>=KNPTS) THEN
      INPTS = KNPTS
    ELSE
      KCODE(JL) = -4
      CYCLE
    END IF
    !
  ENDIF
  !
  !one point more to interpolate
  ICPT = ICPT + 1
  !
  ZDIST(1:ICOUNT) = (PX(IINDEX(1:ICOUNT))-ZX(JL))**2 + (PY(IINDEX(1:ICOUNT))-ZY(JL))**2
  !
  DO JP = 1,ICOUNT
    !
    IF (ZDIST(JP)>ZNDIST(ICPT,INPTS)) CYCLE
    !
    DO JK = INPTS,1,-1
      !
      IF ( ZDIST(JP)>ZNDIST(ICPT,JK-1) ) THEN
        !
        IF ( JK<INPTS ) THEN
          DO JKK = INPTS,JK+1,-1
            ZNDIST(ICPT,JKK) = ZNDIST(ICPT,JKK-1)
            ININD (ICPT,JKK) = ININD (ICPT,JKK-1)
          ENDDO
        ENDIF
        !
        !distances and indexes of points used to interpolate are saved
        ZNDIST(ICPT,JK) = ZDIST (JP)
        ININD (ICPT,JK) = IINDEX(JP)
        !
        EXIT
        !
      ENDIF
      !
    ENDDO
    !
  ENDDO
  !
ENDDO
!
IF (LHOOK) CALL DR_HOOK('INTERPOL_NPTS_3',1,ZHOOK_HANDLE)
IF (LHOOK) CALL DR_HOOK('INTERPOL_NPTS_4',0,ZHOOK_HANDLE)
!
DEALLOCATE(IINDEX,ZX,ZY,NSIZE_ALL,ZDIST)
!
ZNDIST(:,:) = SQRT(ZNDIST(:,:))
!
!numbers of points to interpolated are gathered
IF (NPROC>1) THEN
#ifdef SFX_MPI
  CALL MPI_ALLGATHER(ICPT,KIND(ICPT)/4,MPI_INTEGER,&
                     ISIZE,KIND(ISIZE)/4,MPI_INTEGER,NCOMM,INFOMPI)
#endif
ELSE
  ISIZE(:) = ICPT
ENDIF
!
!this array contains, for each point to interpolate, 
!the correspondance between the task where is the point to use for interpolation
!(NINDEX) and its index in this task (NNUM)
ALLOCATE(ININD0(MAXVAL(ISIZE),KNPTS,0:NPROC-1))
ININD0(:,:,:) = 0
!
!number of points needed to interpolated
DO JL=1,KNPTS
  !number of points to interpolated
  DO JP=1,ICPT
    !index of the point needed in the whole grid
    JK = ININD(JP,JL)
    !inind0 contains the task and the index in this task for this point
    IF (JK/=0) ININD0(JP,JL,NINDEX(JK)) = NNUM(JK)
  ENDDO
ENDDO
!
!for each task, points needed and located in this task are gathered
ALLOCATE(ININD_ALL(MAXVAL(ISIZE),KNPTS,0:NPROC-1))
!
IF (NPROC>1) THEN
  !for each task
  DO JP=0,NPROC-1
#ifdef SFX_MPI   
    !inind_all receives from all tasks the points they need that are
    !located in it
    CALL MPI_GATHER(ININD0(:,:,JP),MAXVAL(ISIZE)*KNPTS*KIND(ININD0)/4,MPI_INTEGER,&
                    ININD_ALL,MAXVAL(ISIZE)*KNPTS*KIND(ININD_ALL)/4,MPI_INTEGER,&
                    JP,NCOMM,INFOMPI)
#endif
  ENDDO
  !
ELSE
  ININD_ALL(:,:,:) = ININD0(:,:,:)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('INTERPOL_NPTS_4',1,ZHOOK_HANDLE)
IF (LHOOK) CALL DR_HOOK('INTERPOL_NPTS_5',0,ZHOOK_HANDLE)
!
!zfield contains the values of the points needed located in this task
!(ie values for indexes of ININD_ALL)
ALLOCATE(ZFIELD(MAXVAL(ISIZE),KNPTS,SIZE(PFIELD,2),0:NPROC-1))
ZFIELD(:,:,:,:) = XUNDEF
DO JP=0,NPROC-1
  DO JK=1,MAXVAL(ISIZE)
    DO JL=1,KNPTS
      IF (ININD_ALL(JK,JL,JP)/=0) THEN
        !pfield in only on this task
        ZFIELD(JK,JL,:,JP) = PFIELD(ININD_ALL(JK,JL,JP),:)
      ENDIF
    ENDDO
  ENDDO
ENDDO
!
!ZFIELD2 gathers values needed for this task from all other tasks
!(inverse operation than before)
ALLOCATE(ZFIELD2(ICPT,KNPTS,SIZE(PFIELD,2),0:NPROC-1))
IF (NPROC>1) THEN
  DO JP=0,NPROC-1
#ifdef SFX_MPI
    CALL MPI_GATHER(ZFIELD(1:ISIZE(JP),:,:,JP),SIZE(ZFIELD(1:ISIZE(JP),:,:,JP))*KIND(ZFIELD)/4,MPI_REAL,&
                    ZFIELD2,ISIZE(JP)*KNPTS*SIZE(PFIELD,2)*KIND(ZFIELD2)/4,MPI_REAL,JP,NCOMM,INFOMPI)
#endif
  ENDDO
ELSE
  ZFIELD2(:,:,:,:) = ZFIELD(:,:,:,:)
ENDIF
!
DEALLOCATE(ZFIELD)
!
!zfield3 contains the values of the points needed for interpolation, gathered from all tasks
ALLOCATE(ZFIELD3(ICPT,KNPTS,SIZE(PFIELD,2)))
DO JP=0,NPROC-1
  WHERE (ZFIELD2(:,:,:,JP)/=XUNDEF) ZFIELD3(:,:,:) = ZFIELD2(:,:,:,JP)
ENDDO
DEALLOCATE(ZFIELD2)
!
IF (LHOOK) CALL DR_HOOK('INTERPOL_NPTS_5',1,ZHOOK_HANDLE)
IF (LHOOK) CALL DR_HOOK('INTERPOL_NPTS_6',0,ZHOOK_HANDLE)
!
!values of the points used for interpolation
ALLOCATE(ZNVAL(IP,IL2))
ZNVAL(:,:) = XUNDEF
!
!znval contains the averaged values for the knpts points
ZNVAL(:,:) = 0.
DO JL=1,ICPT
  ZSUM = 0.
  DO JP=1,KNPTS
    IF (ININD(JL,JP)/=0) THEN
      ZNVAL(JL,:) = ZNVAL(JL,:) + ZFIELD3(JL,JP,:)/ZNDIST(JL,JP)
      ZSUM = ZSUM + 1./ZNDIST(JL,JP)
    ENDIF
  ENDDO
  IF (ZSUM/=0.) ZNVAL(JL,:) = ZNVAL(JL,:) / ZSUM
ENDDO
!
DEALLOCATE(ININD, ZNDIST)
!
!
!finally, pfield contains the interpolated values! 
ICPT=0
DO JL=1,IL1

  IF (KCODE(JL)/=0) CYCLE

  ICPT = ICPT + 1
  PFIELD(JL,:) = ZNVAL(ICPT,:)
  
ENDDO
!
DEALLOCATE(ZNVAL)
!
IF (LHOOK) CALL DR_HOOK('INTERPOL_NPTS_6',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE INTERPOL_NPTS
