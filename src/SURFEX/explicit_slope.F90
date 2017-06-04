!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########################################################################
SUBROUTINE EXPLICIT_SLOPE (UG,KDIM_FULL, &
                           PZS,PSSO_SLOPE)
!     #########################################################################
!!    AUTHOR
!!    ------
!!      M. Lafaysse      * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    19/07/13
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
!
USE MODI_READ_AND_SEND_MPI
USE MODI_GATHER_AND_WRITE_MPI
!
USE MODI_GET_GRID_DIM
USE MODI_GET_MESH_DIM

IMPLICIT NONE
!
!*       0.1   DECLARATIONS OF DUMMY ARGUMENTS :
!
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
!
INTEGER, INTENT(IN) :: KDIM_FULL
REAL,DIMENSION(:),INTENT(IN)::PZS ! resolved model orography
REAL,DIMENSION(:),INTENT(OUT)::PSSO_SLOPE ! resolved slope tangent



!
!
!*       0.2   DECLARATIONS OF LOCAL VARIABLES
!

INTEGER :: IX       ! number of points in X direction
INTEGER :: IY       ! number of points in Y direction

INTEGER :: INNX !  number of points in X direction for large domain
INTEGER :: INNY !  number of points in Y direction for large domain

LOGICAL::GRECT=.TRUE.

INTEGER :: JX       ! loop counter
INTEGER :: JY       ! loop counter

REAL, DIMENSION(:,:), ALLOCATABLE :: ZMAP         ! map factor
REAL, DIMENSION(:,:), ALLOCATABLE :: ZZS ! orography in a 2D array
REAL, DIMENSION(:,:), ALLOCATABLE :: ZSSO_SLOPE ! explicit slope in a 2D array
REAL, DIMENSION(:,:), ALLOCATABLE :: ZZS_XY ! orography at southwest corner of the mesh
REAL, DIMENSION(:,:),ALLOCATABLE :: ZZSL   ! orography in a 2D array
REAL, DIMENSION(:),ALLOCATABLE :: ZXHAT  ! X coordinate
REAL, DIMENSION(:),ALLOCATABLE :: ZYHAT  ! Y coordinate


REAL,DIMENSION(:), ALLOCATABLE :: ZDX        ! grid mesh size in x direction
REAL,DIMENSION(:), ALLOCATABLE  :: ZDY       ! grid mesh size in y direction

REAL, DIMENSION(:), ALLOCATABLE :: ZZS0
REAL, DIMENSION(:), ALLOCATABLE :: ZSSO_SLOPE0
!
! parameters
REAL,    PARAMETER :: XPI=4.*ATAN(1.)  ! Pi
INTEGER, PARAMETER :: JPHEXT = 1 ! number of points around the physical domain
!
INTEGER :: IIB, IIE, IJB, IJE
INTEGER :: JI, JJ, JB
INTEGER :: JT
!
REAL                   :: ZDZSDX   ! slope in X and Y direction
REAL                   :: ZDZSDY   ! of a triangle surface
REAL                   :: ZSURF    ! surface of 4 triangles
!



!-------------------------------------------------------------------------------
!
!*    1.1     Gets the geometry of the grid
!            -----------------------------
!
CALL GET_GRID_DIM(UG%G%CGRID,SIZE(UG%XGRID_FULL_PAR),UG%XGRID_FULL_PAR,GRECT,IX,IY)
!
INNX=IX+2
INNY=IY+2

!

!*    1.2    Grid dimension (meters)
!            -----------------------
!
ALLOCATE(ZDX (IX*IY))
ALLOCATE(ZDY (IX*IY))

CALL GET_MESH_DIM(UG%G%CGRID,SIZE(UG%XGRID_FULL_PAR),IX*IY,UG%XGRID_FULL_PAR,ZDX,ZDY,UG%G%XMESH_SIZE)

!
!*    2.     If grid is not rectangular, nothing is done
!            -------------------------------------------
!
 ALLOCATE(ZZS0(KDIM_FULL))
 CALL GATHER_AND_WRITE_MPI(PZS,ZZS0)
 !
!IF (.NOT. GRECT) RETURN
!
IF (SIZE(ZZS0) /= IX * IY) RETURN
!
!-------------------------------------------------------------------------------
!
!*    3.1     Grid rectangular: orography is put in a 2D array
!            ------------------------------------------------
!
ALLOCATE(ZZS (IX,IY))
ALLOCATE(ZZSL (INNX,INNY))

DO JY=1,IY
  DO JX=1,IX
    ZZS (JX,JY) = ZZS0 ( JX + (JY-1)*IX ) 
  END DO
END DO

DEALLOCATE(ZZS0)
!
ZZSL(2:INNX-1,2:INNY-1) = ZZS(:,:)
ZZSL(1,:) = ZZSL(2,:)
ZZSL(INNX,:) = ZZSL(INNX-1,:)
ZZSL(:,1) = ZZSL(:,2)
ZZSL(:,INNY) = ZZSL(:,INNY-1)

!------------------------------------------------------------------------------------------
!
!*    3.2.    Orography of SW corner of grid meshes
!     -------------------------------------
!

ALLOCATE(ZZS_XY (INNX,INNY))

ZZS_XY(2:INNX,2:INNY) = 0.25*(  ZZSL(2:INNX,2:INNY)   + ZZSL(1:INNX-1,2:INNY)   &
                          + ZZSL(2:INNX,1:INNY-1) + ZZSL(1:INNX-1,1:INNY-1) )
!
ZZS_XY(1,:) = ZZS_XY(2,:)
ZZS_XY(:,1) = ZZS_XY(:,2)

!
!*    3.3     Initialize Grid meshes
!      -----------
!
ALLOCATE(ZXHAT (INNX))
ALLOCATE(ZYHAT (INNY))

DO JX=1,INNX
  ZXHAT(JX) = ZDX(1)*JX
END DO
DO JY=1,INNY
  ZYHAT(JY) = ZDY(1)*JY
END DO

DEALLOCATE(ZDX,ZDY)

!-------------------------------------------------------------------------------
!
IIB= 1+JPHEXT
IIE=INNX-JPHEXT
IJB=1+JPHEXT
IJE=INNY-JPHEXT
!
ALLOCATE(ZMAP(INNX,INNY))
ZMAP(:,:)=1.0
ALLOCATE(ZSSO_SLOPE(IX,IY))

!-------------------------------------------------------------------------------
!
!*       1.    LOOP ON GRID MESHES
!              -------------------
!
!* discretization of the grid mesh in four triangles
!
!
DO JJ=IJB,IJE
  DO JI=IIB,IIE
    ZSURF=0.
    DO JT=1,4
!
!* slopes in x and y
!
      SELECT CASE (JT)
        CASE (1)
          ZDZSDX=(    2.* ZZSL   (JI,JJ)                   &
                   - (ZZS_XY(JI,JJ)+ZZS_XY(JI,JJ+1)) )    &
                 / (ZXHAT(JI+1)-ZXHAT(JI))  * ZMAP(JI,JJ)
          ZDZSDY=(  ZZS_XY(JI,JJ+1) - ZZS_XY(JI,JJ) )     &
                 / (ZYHAT(JJ+1)-ZYHAT(JJ))  * ZMAP(JI,JJ)
        CASE (2)
           ZDZSDX=(  ZZS_XY(JI+1,JJ+1) -ZZS_XY(JI,JJ+1))  &
                 / (ZXHAT(JI+1)-ZXHAT(JI))  * ZMAP(JI,JJ)
           ZDZSDY=(  (ZZS_XY(JI+1,JJ+1)+ZZS_XY(JI,JJ+1))  &
                     - 2.* ZZSL (JI,JJ) )                  &
                 / (ZYHAT(JJ+1)-ZYHAT(JJ))  * ZMAP(JI,JJ)
        CASE (3)
          ZDZSDX=(  (ZZS_XY(JI+1,JJ)+ZZS_XY(JI+1,JJ+1))   &
                   - 2.* ZZSL(JI,JJ)                    )  &
                 / (ZXHAT(JI+1)-ZXHAT(JI))  * ZMAP(JI,JJ)
          ZDZSDY=(  ZZS_XY(JI+1,JJ+1) - ZZS_XY(JI+1,JJ) ) &
                 / (ZYHAT(JJ+1)-ZYHAT(JJ))  * ZMAP(JI,JJ)
        CASE (4)
           ZDZSDX=(  ZZS_XY(JI+1,JJ) - ZZS_XY(JI,JJ) )    &
                 / (ZXHAT(JI+1)-ZXHAT(JI))  * ZMAP(JI,JJ)
           ZDZSDY=(  2.* ZZSL(JI,JJ)                       &
                   - (ZZS_XY(JI+1,JJ)+ZZS_XY(JI,JJ)) )    &
                 / (ZYHAT(JJ+1)-ZYHAT(JJ))  * ZMAP(JI,JJ)
      END SELECT
!
!
      ! If slope is higher than 60 degrees : numerical problems
      ZDZSDX=MIN(2.0,MAX(-2.0,ZDZSDX))
      ZDZSDY=MIN(2.0,MAX(-2.0,ZDZSDY))
    
     ! total surface of 4 triangles
      ZSURF=ZSURF+0.25*SQRT(1. + ZDZSDX**2 + ZDZSDY**2)

    END DO
    
    !equivalent tangent slope of a homogeneous surface with the same area
    ZSSO_SLOPE(JI-JPHEXT,JJ-JPHEXT)=SQRT(ZSURF**2-1)
    
  END DO
END DO
DEALLOCATE(ZZSL)
DEALLOCATE(ZZS)
DEALLOCATE(ZZS_XY)
DEALLOCATE(ZMAP)
!
ALLOCATE(ZSSO_SLOPE0(KDIM_FULL))
!
DO JY=1,IY
  DO JX=1,IX
    ZSSO_SLOPE0( JX + (JY-1)*IX )=ZSSO_SLOPE(JX,JY)
  END DO
END DO
!
CALL READ_AND_SEND_MPI(ZSSO_SLOPE0,PSSO_SLOPE)
!
DEALLOCATE(ZSSO_SLOPE0)
!
END SUBROUTINE EXPLICIT_SLOPE
