!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE INIT_SLOPE_PARAM (UG, PGRID_FULL_PAR, PZS, KI, PLAT)
!
!!**** *INIT_SLOPE_PARAM
!!                         Compute physiographic field necessary to account for
!!                         slope and aspect effects on short-wave radiations
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!   
!!
!!    AUTHOR
!!    ------
!!
!!    V. Vionnet        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    04/11
!!
!!
!!    Modifications Matthieu Lafaysse 04/14 :
!!       * Computations of XSLOPANG, XSLOPAZI and XSURF_TRIANGLE in this routine (optimization)
!!       * Call to GET_GRID_DIM moved in get_size_parallel to initialize NIX and NIY
!!       * Modifications linked to parallelization : gather full domain on all MPI threads
!!       * Solar azimuth is now counted from North clockwise : N --> 0 ; E --> pi/2 ; S --> pi ; W --> -pi/2
!!         for consistency with solar azimuth.
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODD_SFX_GRID_n, ONLY : GRID_t
!
USE MODI_GET_MESH_DIM
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, NPROC, NCOMM, NSIZE, NINDEX, NSIZE_TASK

USE MODD_SLOPE_EFFECT
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif

!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(GRID_t), INTENT(INOUT) :: UG
REAL, DIMENSION(:), INTENT(IN) :: PGRID_FULL_PAR
!
INTEGER,              INTENT(IN)  :: KI        ! number of points
REAL, DIMENSION(KI),   INTENT(IN) :: PZS      ! orography of this MPI thread (or total domain if Open MP)
REAL,DIMENSION(:),INTENT(IN):: PLAT ! latitudes
!
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
LOGICAL :: GRECT    ! true when grid is rectangular

INTEGER :: JX       ! loop counter
INTEGER :: JY       ! loop counter

REAL, DIMENSION(:),ALLOCATABLE :: ZZS1D_FULL !orography of total domain
REAL, DIMENSION(:,:), ALLOCATABLE :: ZZS ! orography in a 2D array

REAL,DIMENSION(:), ALLOCATABLE :: ZDX        ! grid mesh size in x direction
REAL,DIMENSION(:), ALLOCATABLE  :: ZDY       ! grid mesh size in y direction

REAL,    PARAMETER :: XPI=4.*ATAN(1.)  ! Pi
INTEGER, PARAMETER :: JPHEXT = 1 ! number of points around the physical domain

REAL                   :: ZDZSDX   ! slope in X and Y direction
REAL                   :: ZDZSDY   ! of a triangle surface

INTEGER :: IIB, IIE, IJB, IJE
INTEGER :: JI, JJ
INTEGER :: JT       ! loop counter (triangles)
INTEGER :: INFOMPI
INTEGER,DIMENSION(:),ALLOCATABLE::IDISPLS
INTEGER::JPOINT,JPROC
INTEGER::IRANK,INEXTRANK,IPOS
INTEGER::IINDY

!-------------------------------------------------------------------------------
!
!*    1.1     Gets the geometry of the grid
!            -----------------------------
!


! Toutes les threads MPI doivent connaître le champ d'altitude complet
ALLOCATE(ZZS1D_FULL(NIX*NIY))

#ifdef SFX_MPI

IF (NPROC>1) THEN

  ALLOCATE(IDISPLS(0:NPROC-1))
  IDISPLS(0)=0

  !IRANK est le rang du thread associé au 1er point
  IPOS=1
  IRANK=NINDEX(IPOS)
  IDISPLS(IRANK)=0

  ! Attention cette méthode ne marche que si NINDEX est continu (découpage linéaire)
  DO JPROC=1,NPROC-1
     !INEXTRANK est le rang du thread suivant : on décale IPOS du nb de points affectés au thread précedent
     !                                        et on ajoute à IDISPLS(INEXTRANK) l'espace correspondant
     IPOS=IPOS+NSIZE_TASK(IRANK)
     INEXTRANK=NINDEX(IPOS)
     IDISPLS(INEXTRANK)=IDISPLS(IRANK)+NSIZE_TASK(IRANK)*KIND(PZS)/4
     IRANK=INEXTRANK
  END DO

  CALL MPI_ALLGATHERV(PZS,SIZE(PZS)*KIND(PZS)/4,MPI_REAL,ZZS1D_FULL,          &
                      NSIZE_TASK(:)*KIND(PZS)/4,IDISPLS,MPI_REAL,NCOMM,INFOMPI)

ELSE
  ZZS1D_FULL=PZS
ENDIF

#else
  ZZS1D_FULL=PZS
#endif

NNX=NIX+2
NNY=NIY+2

!

!*    1.2    Grid dimension (meters)
!            -----------------------
!
ALLOCATE(ZDX (NIX*NIY))
ALLOCATE(ZDY (NIX*NIY))

IF (NRANK==NPIO) THEN

  CALL GET_MESH_DIM(UG%CGRID,SIZE(PGRID_FULL_PAR),NIX*NIY,PGRID_FULL_PAR,ZDX,ZDY,UG%XMESH_SIZE)
  
ENDIF

#ifdef SFX_MPI
IF (NPROC>1) THEN
  CALL MPI_BCAST(ZDX,SIZE(ZDX)*KIND(ZDX)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
  CALL MPI_BCAST(ZDY,SIZE(ZDY)*KIND(ZDY)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)  
END IF
#endif
!
!-------------------------------------------------------------------------------
!
!*    2.     If grid is not rectangular, nothing is done
!            -------------------------------------------
!
IF (SIZE(ZZS1D_FULL) /= NIX * NIY) STOP "BIG PROBLEM WITH SIZE"
!
!-------------------------------------------------------------------------------
!
!*    3.1     Grid rectangular: orography is put in a 2D array
!            ------------------------------------------------
!
ALLOCATE(ZZS (NIX,NIY))
ALLOCATE(XZSL (NNX,NNY))
!
!RJ: next one does not work with NPROC>4
!RJ 'Fortran runtime error: Index '13' of dimension 1 of array 'plat' above upper bound of 12'
! 2d grid should be increasing in latitude
LREVERTGRID=(PLAT(1)>PLAT(1+NIX))

IF (LREVERTGRID) THEN
  DO JY=1,NIY
    IINDY=NIY-JY+1
    DO JX=1,NIX
      ZZS (JX,IINDY) = ZZS1D_FULL ( JX + (JY-1)*NIX ) 
    END DO
  END DO
ELSE
  DO JY=1,NIY
    DO JX=1,NIX
      ZZS (JX,JY) = ZZS1D_FULL ( JX + (JY-1)*NIX ) 
    END DO
  END DO
ENDIF

XZSL(2:NNX-1,2:NNY-1) = ZZS(:,:)
XZSL(1,:) = XZSL(2,:)
XZSL(NNX,:) = XZSL(NNX-1,:)
XZSL(:,1) = XZSL(:,2)
XZSL(:,NNY) = XZSL(:,NNY-1)

!------------------------------------------------------------------------------------------
!
!*    3.2.    Orography of SW corner of grid meshes
!     -------------------------------------
!
ALLOCATE(XZS_XY (NNX,NNY))
XZS_XY(2:NNX,2:NNY) = 0.25*(XZSL(2:NNX,2:NNY)  +XZSL(1:NNX-1,2:NNY) +&
                            XZSL(2:NNX,1:NNY-1)+XZSL(1:NNX-1,1:NNY-1))
!
XZS_XY(1,:) = XZS_XY(2,:)
XZS_XY(:,1) = XZS_XY(:,2)
!
!*    3.3     Initialize Grid meshes
!      -----------
!
ALLOCATE(XXHAT (NNX))
ALLOCATE(XYHAT (NNY))

XXHAT(1)=ZDX(1)
XYHAT(1)=ZDY(1)
DO JX=2,NNX
  XXHAT(JX) = XXHAT(JX-1)+ZDX(JX)
END DO
DO JY=2,NNY
  XYHAT(JY) = XYHAT(JY-1)+ZDY(JY)
END DO

DEALLOCATE(ZZS)

!-------------------------------------------------------------------------------
!
!*       4.    Compute slope angles
!              -------------------

IIB= 1+JPHEXT
IIE=SIZE(XXHAT)-JPHEXT
IJB=1+JPHEXT
IJE=SIZE(XYHAT)-JPHEXT

ALLOCATE(XSLOPANG(NNX,NNY,4))
ALLOCATE(XSLOPAZI(NNX,NNY,4))
ALLOCATE(XSURF_TRIANGLE(NNX,NNY,4))

!          4.1 Loop on 4 triangles
!
!------------------------------------------------------------------------------------------
DO JT=1,4
!
  DO JJ=IJB,IJE
    DO JI=IIB,IIE
!
!-------------------------------------------------------------------------------
!
!*       4.2  Compute local slope
!              --------------------------------------------
!
!* slopes in x and y
!
      SELECT CASE (JT)
        CASE (1)
          ZDZSDX=(    2.* XZSL   (JI,JJ)                   &
                   - (XZS_XY(JI,JJ)+XZS_XY(JI,JJ+1)) )    &
                 / (XXHAT(JI+1)-XXHAT(JI))  
          ZDZSDY=(  XZS_XY(JI,JJ+1) - XZS_XY(JI,JJ) )     &
                 / (XYHAT(JJ+1)-XYHAT(JJ))  
        CASE (2)
           ZDZSDX=(  XZS_XY(JI+1,JJ+1) -XZS_XY(JI,JJ+1))  &
                 / (XXHAT(JI+1)-XXHAT(JI))  
           ZDZSDY=(  (XZS_XY(JI+1,JJ+1)+XZS_XY(JI,JJ+1))  &
                     - 2.* XZSL (JI,JJ) )                  &
                 / (XYHAT(JJ+1)-XYHAT(JJ))  
        CASE (3)
          ZDZSDX=(  (XZS_XY(JI+1,JJ)+XZS_XY(JI+1,JJ+1))   &
                   - 2.* XZSL(JI,JJ)                    )  &
                 / (XXHAT(JI+1)-XXHAT(JI))  
          ZDZSDY=(  XZS_XY(JI+1,JJ+1) - XZS_XY(JI+1,JJ) ) &
                 / (XYHAT(JJ+1)-XYHAT(JJ))  
        CASE (4)
           ZDZSDX=(  XZS_XY(JI+1,JJ) - XZS_XY(JI,JJ) )    &
                 / (XXHAT(JI+1)-XXHAT(JI))  
           ZDZSDY=(  2.* XZSL(JI,JJ)                       &
                   - (XZS_XY(JI+1,JJ)+XZS_XY(JI,JJ)) )    &
                 / (XYHAT(JJ+1)-XYHAT(JJ)) 
      END SELECT
!
      ! If slope is higher than 60 degrees : numerical problems
      ZDZSDX=MIN(2.0,MAX(-2.0,ZDZSDX))
      ZDZSDY=MIN(2.0,MAX(-2.0,ZDZSDY))
!* slope angles
!
      XSLOPANG(JI,JJ,JT) = ATAN(SQRT(ZDZSDX**2+ZDZSDY**2))
      XSLOPAZI(JI,JJ,JT) = 1.5*XPI - ATAN2( ZDZSDY, ZDZSDX + SIGN(1.E-30,ZDZSDX) )
!

! surface of each triangle
      XSURF_TRIANGLE(JI,JJ,JT)=SQRT(1. + ZDZSDX**2 + ZDZSDY**2)

!
!* normalizes received radiation by the surface of the triangle to obtain
!  radiation representative of an horizontal surface.
!
!       XSSO_SURF(JI,JJ) = XSSO_SURF(JI,JJ) + 0.25*XSURF_TRIANGLE(JI,JJ,JT)
!
    END DO
  END DO
END DO

! 2d arrays for processor domain

!------------------------------------------------------------------------------------------


END SUBROUTINE INIT_SLOPE_PARAM
