!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########################################################################
      SUBROUTINE LATLON_GRIDTYPE_LONLAT_REG(G,KL,PDIR)
!     #########################################################################
!
!!****  *LATLON_GRIDTYPE_LONLAT_REG* - routine to compute the horizontal geographic fields
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
USE MODD_CSTS,     ONLY : XPI, XRADIUS
!
USE MODE_GRIDTYPE_LONLAT_REG
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
REAL    :: ZLONMIN ! minimum longitude (degrees)
REAL    :: ZLONMAX ! maximum longitude (degrees)
REAL    :: ZLATMIN ! minimum latitude  (degrees)
REAL    :: ZLATMAX ! maximum latitude  (degrees)
INTEGER :: ILON    ! number of points in longitude
INTEGER :: ILAT    ! number of points in latitude
!
REAL    :: ZDLAT   ! grid size in latitude  unit
REAL    :: ZDLON   ! grid size in longitude unit
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!---------------------------------------------------------------------------
!
!*       1.    Grid parameters
!              ---------------
!
IF (LHOOK) CALL DR_HOOK('LATLON_GRIDTYPE_LONLAT_REG',0,ZHOOK_HANDLE)
 CALL GET_GRIDTYPE_LONLAT_REG(G%XGRID_PAR,ZLONMIN,ZLONMAX,                    &
                               ZLATMIN,ZLATMAX,ILON,ILAT,PLON=G%XLON,PLAT=G%XLAT )  
!
!-----------------------------------------------------------------------------
!
!*       2.    Compute grid size
!              -----------------
!
ZDLAT = (ZLATMAX-ZLATMIN)/FLOAT(ILAT)
ZDLON = (ZLONMAX-ZLONMIN)/FLOAT(ILON)
!
G%XMESH_SIZE(:) = XRADIUS**2 * XPI/180.*(ZDLON)              &
       * (SIN((G%XLAT(:)+ZDLAT/2.)*XPI/180.)-SIN((G%XLAT(:)-ZDLAT/2.)*XPI/180.))  
!
!-----------------------------------------------------------------------------
!
!*       4.    Direction of of grid from North for each grid point
!              ---------------------------------------------------
!
PDIR(:) = 0.
IF (LHOOK) CALL DR_HOOK('LATLON_GRIDTYPE_LONLAT_REG',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------------
!
END SUBROUTINE LATLON_GRIDTYPE_LONLAT_REG

