!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########################################################################
      SUBROUTINE LATLON_GRIDTYPE_LONLATVAL(G,KL,PDIR)
!     #########################################################################
!
!!****  *LATLON_GRIDTYPE_IGN* - routine to compute the horizontal geographic fields
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
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
!!      E. Martin   *Meteo France*
!!      M Lafaysse 08/2013 missing deallocate
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2007 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SFX_GRID_n, ONLY : GRID_t
!
USE MODD_CSTS,     ONLY : XPI, XRADIUS
!
USE MODE_GRIDTYPE_LONLATVAL
!
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(GRID_t), INTENT(INOUT) :: G
!
INTEGER,                    INTENT(IN)  :: KL         ! number of points
REAL, DIMENSION(KL),        INTENT(OUT) :: PDIR ! direction of main grid Y axis (deg. from N, clockwise)
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(:),   ALLOCATABLE :: ZX       ! X Lambert coordinate
REAL, DIMENSION(:),   ALLOCATABLE :: ZY       ! Y  Lambertcoordinate
REAL, DIMENSION(:),   ALLOCATABLE :: ZDX      ! size in X Lambert coordinate
REAL, DIMENSION(:),   ALLOCATABLE :: ZDY      ! size in Y Lambert coordinate
REAL, DIMENSION(:),   ALLOCATABLE :: ZDLAT   ! grid size in latitude  unit
REAL, DIMENSION(:),   ALLOCATABLE :: ZDLON   ! grid size in longitude unit
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!---------------------------------------------------------------------------
!
!*       1.    Projection and 2D grid parameters
!              ---------------------------------
!
IF (LHOOK) CALL DR_HOOK('LATLON_GRIDTYPE_LONLATVAL',0,ZHOOK_HANDLE)
ALLOCATE(ZX (SIZE(G%XLAT)))
ALLOCATE(ZY (SIZE(G%XLAT)))
ALLOCATE(ZDX(SIZE(G%XLAT)))
ALLOCATE(ZDY(SIZE(G%XLAT)))
ALLOCATE(ZDLON(SIZE(G%XLAT)))
ALLOCATE(ZDLAT(SIZE(G%XLAT)))

!
 CALL GET_GRIDTYPE_LONLATVAL(G%XGRID_PAR,PX=ZX,PY=ZY,PDX=ZDX,PDY=ZDY      )
!
!---------------------------------------------------------------------------
!
!*       2.    Computation of latitude and longitude
!              -------------------------------------
!
 CALL LATLON_LONLATVAL(ZX,ZY,G%XLAT,G%XLON)
!
!-----------------------------------------------------------------------------
!
!*       3.    Compute grid size (2D array)
!              -----------------
!
!
ZDLAT = ZDY
ZDLON = ZDX
!
G%XMESH_SIZE(:) = XRADIUS**2 * XPI/180.*(ZDLON(:))              &
       * (SIN((G%XLAT(:)+ZDLAT(:)/2.)*XPI/180.)-SIN((G%XLAT(:)-ZDLAT(:)/2.)*XPI/180.))  
!
!-----------------------------------------------------------------------------
!
!*       4.    Direction of Y axis (from North) for each grid point
!              ----------------------------------------------------
!
PDIR(:) = 0.



DEALLOCATE(ZX)
DEALLOCATE(ZY)
DEALLOCATE(ZDX)
DEALLOCATE(ZDY)
DEALLOCATE(ZDLON)
DEALLOCATE(ZDLAT)



IF (LHOOK) CALL DR_HOOK('LATLON_GRIDTYPE_LONLATVAL',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------------
!
END SUBROUTINE LATLON_GRIDTYPE_LONLATVAL
