!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_MESH_DIM_GAUSS(KGRID_PAR,KL,PGRID_PAR,PMESHSIZE,PDX,PDY)
!     ##############################################################
!
!!**** *GET_MESH_DIM_GAUSS* get the grid mesh dimensions
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson         Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    03/2004
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_CSTS, ONLY : XPI, XRADIUS
!
USE MODE_GRIDTYPE_GAUSS
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
INTEGER,                         INTENT(IN)    :: KGRID_PAR ! size of PGRID_PAR
INTEGER,                         INTENT(IN)    :: KL        ! number of points
REAL,    DIMENSION(KGRID_PAR),   INTENT(IN)    :: PGRID_PAR ! grid parameters
REAL,    DIMENSION(KL),          INTENT(IN)    :: PMESHSIZE ! mesh size (m2)
REAL,    DIMENSION(KL),          INTENT(OUT)   :: PDX       ! mean dimension in x dir. (meters)
REAL,    DIMENSION(KL),          INTENT(OUT)   :: PDY       ! mean dimension in y dir. (meters)
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
INTEGER :: INLATI    ! number of pseudo-latitudes
REAL    :: ZLAPO     ! latitude of the rotated pole  (deg)
REAL    :: ZLOPO     ! longitude of the rotated pole (deg)
REAL    :: ZCODIL    ! stretching factor (must be greater than or equal to 1)
INTEGER, DIMENSION(:), ALLOCATABLE :: INLOPA ! number of pseudo-longitudes on each
                                             ! pseudo-latitude circle
REAL,  DIMENSION(KL) :: ZLAT  ! latitudes
REAL,  DIMENSION(KL) :: ZLON  ! longitudes
REAL,  DIMENSION(KL) :: ZXINF ! minimum pseudo longitude of the grid point (deg)
REAL,  DIMENSION(KL) :: ZXSUP ! maximum pseudo longitude of the grid point (deg)
REAL,  DIMENSION(KL) :: ZYINF ! minimum pseudo latitude  of the grid point (deg)
REAL,  DIMENSION(KL) :: ZYSUP ! maximum pseudo latitude  of the grid point (deg)
REAL,  DIMENSION(KL) :: ZMAP  ! map factor
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!----------------------------------------------------------------------------
!
!*       1.    Gets grid definition
!              --------------------
!
IF (LHOOK) CALL DR_HOOK('GET_MESH_DIM_GAUSS',0,ZHOOK_HANDLE)
 CALL GET_GRIDTYPE_GAUSS(PGRID_PAR,INLATI)
!
ALLOCATE(INLOPA(INLATI))
 CALL GET_GRIDTYPE_GAUSS(PGRID_PAR,INLATI,ZLAPO,ZLOPO,ZCODIL,INLOPA(:),PLAT=ZLAT,PLON=ZLON)
!
!---------------------------------------------------------------------------
!
!*       2.    Computation of pseudo latitude and pseudo longitude limits for each mesh
!              ------------------------------------------------------------------------
!
 CALL GAUSS_GRID_LIMITS(INLATI,INLOPA,ZXINF,ZXSUP,ZYINF,ZYSUP)
!
!-----------------------------------------------------------------------------
!
!*       3.    Map factor
!              ----------
!
 CALL MAP_FACTOR_GAUSS(ZLAPO,ZLOPO,ZCODIL,ZLAT,ZLON,ZMAP)
!
!-----------------------------------------------------------------------------
!
!*       4.    Compute mean grid dimension in each direction
!              ---------------------------------------------
!
PDY(:) = XRADIUS * XPI/180. * (ZYSUP(:)-ZYINF(:)) * ZMAP(:)
PDX(:) = PMESHSIZE(:) / PDY(:)
!
!-------------------------------------------------------------------------------
!
DEALLOCATE(INLOPA)
IF (LHOOK) CALL DR_HOOK('GET_MESH_DIM_GAUSS',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_MESH_DIM_GAUSS
