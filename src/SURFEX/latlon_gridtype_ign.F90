!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########################################################################
      SUBROUTINE LATLON_GRIDTYPE_IGN(G,KL,PDIR)
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
!!
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
USE MODD_CSTS,     ONLY : XPI
USE MODD_IGN, ONLY : XA, XDELTY
!
USE MODE_GRIDTYPE_IGN
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
REAL, DIMENSION(:),   ALLOCATABLE :: ZMAP     ! map factor
REAL, DIMENSION(:),   ALLOCATABLE :: ZDX      ! size in X Lambert coordinate
REAL, DIMENSION(:),   ALLOCATABLE :: ZDY      ! size in Y Lambert coordinate
REAL, DIMENSION(:),   ALLOCATABLE :: ZYDELTY  ! Y + DELTY Lambert coordinate            
REAL, DIMENSION(:),   ALLOCATABLE :: ZLATDY   ! latitude 
REAL, DIMENSION(:),   ALLOCATABLE :: ZLONDY   ! longitude
!
INTEGER :: ILAMBERT ! Lambert type
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!---------------------------------------------------------------------------
!
!*       1.    Projection and 2D grid parameters
!              ---------------------------------
!
IF (LHOOK) CALL DR_HOOK('LATLON_GRIDTYPE_IGN',0,ZHOOK_HANDLE)
ALLOCATE(ZX (SIZE(G%XLAT)))
ALLOCATE(ZY (SIZE(G%XLAT)))
ALLOCATE(ZDX(SIZE(G%XLAT)))
ALLOCATE(ZDY(SIZE(G%XLAT)))
!
 CALL GET_GRIDTYPE_IGN(G%XGRID_PAR,KLAMBERT=ILAMBERT,PX=ZX,PY=ZY,PDX=ZDX,PDY=ZDY      )
!
!---------------------------------------------------------------------------
!
!*       2.    Computation of latitude and longitude
!              -------------------------------------
!
 CALL LATLON_IGN(ILAMBERT,ZX,ZY,G%XLAT,G%XLON)
!
!-----------------------------------------------------------------------------
!
!*       3.    Compute grid size (2D array)
!              -----------------
!
!        3.1   Map factor
!              ----------
!
ALLOCATE(ZMAP(SIZE(G%XLAT)))
!
 CALL MAP_FACTOR_IGN(ILAMBERT,ZX,ZY,ZMAP)
!
!        3.2   Grid size
!              ---------
!
G%XMESH_SIZE(:) = ZDX(:) * ZDY(:) / ZMAP(:)**2
!
!-----------------------------------------------------------------------------
!
!*       4.    Direction of Y axis (from North) for each grid point
!              ----------------------------------------------------
!
!* the following formulae is given for clockwise angles.
ALLOCATE(ZYDELTY(SIZE(G%XLAT)))
ALLOCATE(ZLATDY (SIZE(G%XLAT)))
ALLOCATE(ZLONDY (SIZE(G%XLAT)))
ZYDELTY=ZY+XDELTY
 CALL LATLON_IGN(ILAMBERT,ZX,ZYDELTY,ZLATDY,ZLONDY)
!
PDIR(:)= ATAN( (XA(ILAMBERT)*(ZLONDY(:)-G%XLON(:))*XPI/180.) / XDELTY) * XPI/180.
!
!---------------------------------------------------------------------------
DEALLOCATE(ZX)
DEALLOCATE(ZY)
DEALLOCATE(ZMAP)
DEALLOCATE(ZDX)
DEALLOCATE(ZDY)
DEALLOCATE(ZYDELTY)
DEALLOCATE(ZLATDY)
DEALLOCATE(ZLONDY)
IF (LHOOK) CALL DR_HOOK('LATLON_GRIDTYPE_IGN',1,ZHOOK_HANDLE)
!---------------------------------------------------------------------------
!
END SUBROUTINE LATLON_GRIDTYPE_IGN

