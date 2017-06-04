!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########################################################################
      SUBROUTINE LATLON_GRIDTYPE_CONF_PROJ(G,KL,PDIR)
!     #########################################################################
!
!!****  *LATLON_GRIDTYPE_CONF_PROJ* - routine to compute the horizontal geographic fields
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
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SFX_GRID_n, ONLY : GRID_t
!
USE MODD_CSTS,     ONLY : XPI
!
USE MODE_GRIDTYPE_CONF_PROJ
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
REAL                              :: ZLAT0    ! reference latitude
REAL                              :: ZLON0    ! reference longitude
REAL                              :: ZRPK     ! projection parameter 
!                                             !   K=1 : stereographic north pole
!                                             ! 0<K<1 : Lambert, north hemisphere
!                                             !   K=0 : Mercator
!                                             !-1<K<0 : Lambert, south hemisphere
!                                             !   K=-1: stereographic south pole
REAL                              :: ZBETA    ! angle between grid and reference longitude
REAL                              :: ZLATOR   ! latitude  of point of coordinates X=0, Y=0
REAL                              :: ZLONOR   ! longitude of point of coordinates X=0, Y=0
REAL, DIMENSION(:),   ALLOCATABLE :: ZX       ! X conformal coordinate
REAL, DIMENSION(:),   ALLOCATABLE :: ZY       ! Y conformal coordinate
REAL, DIMENSION(:),   ALLOCATABLE :: ZMAP     ! map factor
REAL, DIMENSION(:),   ALLOCATABLE :: ZDX      ! size in X conformal coordinate
REAL, DIMENSION(:),   ALLOCATABLE :: ZDY      ! size in Y conformal coordinate
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!---------------------------------------------------------------------------
!
!*       1.    Projection and 2D grid parameters
!              ---------------------------------
!
IF (LHOOK) CALL DR_HOOK('LATLON_GRIDTYPE_CONF_PROJ',0,ZHOOK_HANDLE)
ALLOCATE(ZX (SIZE(G%XLAT)))
ALLOCATE(ZY (SIZE(G%XLAT)))
ALLOCATE(ZDX(SIZE(G%XLAT)))
ALLOCATE(ZDY(SIZE(G%XLAT)))
!
 CALL GET_GRIDTYPE_CONF_PROJ(G%XGRID_PAR,ZLAT0,ZLON0,ZRPK,ZBETA,&
                              ZLATOR,ZLONOR,                   &
                              PX=ZX,PY=ZY,PDX=ZDX,PDY=ZDY      )  
!
!---------------------------------------------------------------------------
!
!*       2.    Computation of latitude and longitude
!              -------------------------------------
!
 CALL LATLON_CONF_PROJ(ZLAT0,ZLON0,ZRPK,ZBETA,ZLATOR,ZLONOR,ZX,ZY,G%XLAT,G%XLON)
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
 CALL MAP_FACTOR_CONF_PROJ(ZLAT0,ZRPK,G%XLAT,ZMAP)
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
PDIR(:) = ZRPK * (G%XLON(:) - ZLON0) - ZBETA
!
WHERE (PDIR(:) <0.)    PDIR = PDIR + 360.
WHERE (PDIR(:) >=360.) PDIR = PDIR - 360.
!
!---------------------------------------------------------------------------
DEALLOCATE(ZX)
DEALLOCATE(ZY)
DEALLOCATE(ZMAP)
DEALLOCATE(ZDX)
DEALLOCATE(ZDY)
IF (LHOOK) CALL DR_HOOK('LATLON_GRIDTYPE_CONF_PROJ',1,ZHOOK_HANDLE)
!---------------------------------------------------------------------------
!
END SUBROUTINE LATLON_GRIDTYPE_CONF_PROJ

