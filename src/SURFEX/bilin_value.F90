!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
     SUBROUTINE BILIN_VALUE (KLUOUT,KX,KY,PFIELD1,PCX,PCY,KCI,KCJ,PFIELD2)
!     #########################################################################
!
!!****  *BILIN_VALUEEAR * - subroutine to interpolate surface FIELD
!!
!!    PURPOSE
!!    -------
!!
!!
!!**  METHOD
!!    ------
!!
!!     Interpolation is bilinear, and uses 9 grid points, located in the
!!   center of model 1 grid mesh, and at the boundaries of this grid
!!   mesh (2 X limits, 2 Y limits and 4 corners).
!!     This implies that the grid mesh values located around the model 1
!!   grid mesh are not used directly. The values at the boundaries of the
!!   grid mesh are defined by the average between the middle point
!!   (this grid mesh value), and the one in the considered direction.
!!   So the eight grid meshes around the considered grid mesh are used
!!   equally.
!!     This is important to note that these average values are erased
!!   and replaced by zero if they are at the limit of any grid
!!   mesh with the zero value. This allows to insure zero value in model 2
!!   grid meshes where there was not the considered class in corresponding
!!   model 1 grid mesh, and to insure continuity of the FIELD type
!!   at such boundaries.
!!
!!
!!    The arrays and array index are defined on the following (model1) grid:
!!
!!
!!        XFIELD                    XFIELD                    XFIELD
!!          *                         *                         *
!!       i-1,j+1                    i,j+1                    i+1,j+1
!!
!!
!!
!!                   ZFIELD_XY     ZFIELD_Y    ZFIELD_XY
!!                       *            *            *
!!                     i,j+1        i,j+1       i+1,j+1
!!
!!
!!
!!        XFIELD      ZFIELD_X      XFIELD      ZFIELD_X      XFIELD
!!          *            *            *            *            *
!!        i-1,j         i,j          i,j         i+1,j        i+1,j
!!
!!
!!
!!                    ZFIELD_XY     ZFIELD_Y    ZFIELD_XY
!!                       *            *            *
!!                      i,j          i,j         i+1,j
!!
!!
!!
!!        XFIELD                    XFIELD                    XFIELD
!!          *                         *                         *
!!       i-1,j-1                    i,j-1                    i+1,j-1
!!
!!
!!
!!
!!
!!    AUTHOR
!!    ------
!!
!!       V. Masson     * METEO-FRANCE *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original     01/2004
! TD&DD: added OpenMP directives

!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, NCOMM, NPROC, IDX_I
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE MODI_BILIN_GRIDIN
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
!*       0.1   Declarations of dummy arguments :
!
INTEGER,              INTENT(IN) :: KLUOUT  ! output listing logical unit
INTEGER, INTENT(IN) :: KX, KY
REAL, DIMENSION(:,:), INTENT(IN) :: PFIELD1 ! FIELD on regular input grid
REAL, DIMENSION(:,:), INTENT(IN) :: PCX, PCY
INTEGER, DIMENSION(:), INTENT(IN):: KCI, KCJ
REAL, DIMENSION(:,:),   INTENT(OUT) :: PFIELD2 ! FIELD on model 2
!
!*       0.2    Declarations of local variables for print on FM file
!
#ifdef SFX_MPI
INTEGER, DIMENSION(MPI_STATUS_SIZE) :: ISTATUS
#endif
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZFIELD1,ZFIELDS
REAL, DIMENSION(:,:), ALLOCATABLE :: ZFIELD_X, ZFIELD_Y, ZFIELD_XY
INTEGER, DIMENSION(2) :: IB
INTEGER, DIMENSION(2,2) :: IJEXT
INTEGER, DIMENSION(2,2,0:NPROC-1) :: IBOR
INTEGER :: INFOMPI, ISIZE, IS1, IS2, J, IT1, IT2
INTEGER  :: JL, JK, JI, JJ, INL        ! grid 2 index
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE, ZHOOK_HANDLE_OMP
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('BILIN_VALUE_1',0,ZHOOK_HANDLE)
!
INL = SIZE(PFIELD2,2)
!
IF (SIZE(KCI)>0) THEN
  IJEXT(1,1) = MAX(1,MINVAL(KCI)-1)
  IJEXT(1,2) = MIN(MAXVAL(KCI)+1,KX)
  IJEXT(2,1) = MAX(1,MINVAL(KCJ)-1)
  IJEXT(2,2) = MIN(MAXVAL(KCJ)+1,KY)
ELSE
  IJEXT(:,:) = 0
ENDIF
!
IF (LHOOK) CALL DR_HOOK('BILIN_VALUE_1',1,ZHOOK_HANDLE)

IF (LHOOK) CALL DR_HOOK('BILIN_VALUE_2',0,ZHOOK_HANDLE)
!
IF (NPROC>1) THEN
#ifdef SFX_MPI
  CALL MPI_GATHER(IJEXT,4*KIND(IJEXT)/4,MPI_INTEGER,&
                IBOR,4*KIND(IBOR)/4,MPI_INTEGER,& 
                NPIO,NCOMM,INFOMPI)
#endif
ELSE
  IBOR(:,:,0) = IJEXT(:,:)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('BILIN_VALUE_2',1,ZHOOK_HANDLE)
!
IF (NRANK/=NPIO) THEN
  !
  IF (LHOOK) CALL DR_HOOK('BILIN_VALUE_3',0,ZHOOK_HANDLE)
  !
  IDX_I = IDX_I + 1
  IS1 = IJEXT(1,2)-IJEXT(1,1)+1
  IS2 = IJEXT(2,2)-IJEXT(2,1)+1
  ISIZE = IS1*IS2
  ALLOCATE(ZFIELD1(IS1,IS2,INL))
#ifdef SFX_MPI  
  IF (SUM(IJEXT)/=0) &
    CALL MPI_RECV(ZFIELD1,ISIZE*INL*KIND(ZFIELD1)/4,MPI_REAL,NPIO,IDX_I,NCOMM,ISTATUS,INFOMPI)
#endif
  !
  IF (LHOOK) CALL DR_HOOK('BILIN_VALUE_3',1,ZHOOK_HANDLE)
  !
ELSE
  !
!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP)
!$OMP DO SCHEDULE(DYNAMIC,1) PRIVATE(J,IT1,IT2,ISIZE,ZFIELDS)
  DO J=0,NPROC-1
    !
    IF (J/=NPIO) THEN
      !
      IF (LHOOK) CALL DR_HOOK('BILIN_VALUE_31',0,ZHOOK_HANDLE_OMP)
      !
      IT1 = IBOR(1,2,J)-IBOR(1,1,J)+1
      IT2 = IBOR(2,2,J)-IBOR(2,1,J)+1
      ISIZE = IT1*IT2
      IF (SUM(IBOR(:,:,J))/=0) THEN
        ALLOCATE(ZFIELDS(IT1,IT2,INL))
        DO JL=IBOR(2,1,J),IBOR(2,2,J)
          ZFIELDS(:,JL-IBOR(2,1,J)+1,:) = PFIELD1(KX*(JL-1)+IBOR(1,1,J):KX*(JL-1)+IBOR(1,2,J),:)
        ENDDO
#ifdef SFX_MPI        
        CALL MPI_SEND(ZFIELDS,SIZE(ZFIELDS)*KIND(ZFIELDS)/4,MPI_REAL,J,IDX_I+1,NCOMM,INFOMPI)
#endif
        DEALLOCATE(ZFIELDS)
      ENDIF
      IF (LHOOK) CALL DR_HOOK('BILIN_VALUE_31',1,ZHOOK_HANDLE_OMP)
      !
    ELSE
      !    
      IF (LHOOK) CALL DR_HOOK('BILIN_VALUE_32',0,ZHOOK_HANDLE_OMP)
      !
      IS1 = IBOR(1,2,0)-IBOR(1,1,0)+1
      IS2 = IBOR(2,2,0)-IBOR(2,1,0)+1
      ISIZE = IS1*IS2
      ALLOCATE(ZFIELD1(IS1,IS2,INL))
      IF (SUM(IBOR(:,:,0))/=0) THEN    
        DO JL=IBOR(2,1,0),IBOR(2,2,0)
          ZFIELD1(:,JL-IBOR(2,1,0)+1,:) = PFIELD1(KX*(JL-1)+IBOR(1,1,0):KX*(JL-1)+IBOR(1,2,0),:)
        ENDDO
      ENDIF
      !
      IF (LHOOK) CALL DR_HOOK('BILIN_VALUE_32',1,ZHOOK_HANDLE_OMP)
      !
    ENDIF
    !
  ENDDO
!$OMP END DO
!$OMP END PARALLEL
  !
  IDX_I = IDX_I + 1
  !
ENDIF
!
ALLOCATE(ZFIELD_X(IS1+1,IS2),ZFIELD_Y(IS1,IS2+1),ZFIELD_XY(IS1+1,IS2+1))
!
DO JK=1,INL
  !
  IF (LHOOK) CALL DR_HOOK('BILIN_VALUE_4',0,ZHOOK_HANDLE)
  CALL BILIN_GRIDIN(ZFIELD1(:,:,JK),ZFIELD_X,ZFIELD_Y,ZFIELD_XY)
  !
  IF (LHOOK) CALL DR_HOOK('BILIN_VALUE_4',1,ZHOOK_HANDLE)
  IF (LHOOK) CALL DR_HOOK('BILIN_VALUE_5',0,ZHOOK_HANDLE)
  PFIELD2(:,JK) = XUNDEF
  !
  DO JL=1,SIZE(PFIELD2,1)
    !
    JI = KCI(JL) - IJEXT(1,1) + 1
    JJ = KCJ(JL) - IJEXT(2,1) + 1
    JI = MAX(MIN(JI,SIZE(ZFIELD1,1)),0)
    JJ = MAX(MIN(JJ,SIZE(ZFIELD1,2)),0)
    !
    !* interpolation weights in X direction
    !
    !
    !* interpolation
    !
    IF(ZFIELD1(JI,JJ,JK) /= XUNDEF) THEN
      
      PFIELD2(JL,JK) = PCY(JL,1) * &
         ( PCX(JL,1) * ZFIELD_XY(JI,JJ)   + PCX(JL,2) * ZFIELD_Y(JI,JJ)   + PCX(JL,3) * ZFIELD_XY(JI+1,JJ) ) &
                  + PCY(JL,2) * &
         ( PCX(JL,1) * ZFIELD_X (JI,JJ)   + PCX(JL,2) * ZFIELD1 (JI,JJ,JK) + PCX(JL,3) * ZFIELD_X (JI+1,JJ) ) &
                  + PCY(JL,3) * &
         ( PCX(JL,1) * ZFIELD_XY(JI,JJ+1) + PCX(JL,2) * ZFIELD_Y(JI,JJ+1) + PCX(JL,3) * ZFIELD_XY(JI+1,JJ+1) )  

    ENDIF

  END DO
  !
  IF (LHOOK) CALL DR_HOOK('BILIN_VALUE_5',1,ZHOOK_HANDLE)
  !
ENDDO
!
IF (LHOOK) CALL DR_HOOK('BILIN_VALUE_6',0,ZHOOK_HANDLE)
!
DEALLOCATE(ZFIELD1,ZFIELD_X,ZFIELD_Y,ZFIELD_XY)
!
!-------------------------------------------------------------------------------
!
WHERE(ABS(PFIELD2-XUNDEF)<1.E-6) PFIELD2=XUNDEF
!
IF (LHOOK) CALL DR_HOOK('BILIN_VALUE_6',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE BILIN_VALUE
