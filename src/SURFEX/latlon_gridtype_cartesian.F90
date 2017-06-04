!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########################################################################
      SUBROUTINE LATLON_GRIDTYPE_CARTESIAN(G,KL,PDIR)
!     #########################################################################
!
!!****  *LATLON_GRIDTYPE_CARTESIAN* - routine to compute the horizontal geographic fields
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
USE MODE_GRIDTYPE_CARTESIAN
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
REAL, DIMENSION(:),   ALLOCATABLE :: ZDX      ! size in X conformal coordinate
REAL, DIMENSION(:),   ALLOCATABLE :: ZDY      ! size in Y conformal coordinate
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!---------------------------------------------------------------------------
!
!*       1.    Projection and 2D grid parameters
!              ---------------------------------
!
IF (LHOOK) CALL DR_HOOK('LATLON_GRIDTYPE_CARTESIAN',0,ZHOOK_HANDLE)
ALLOCATE(ZDX(SIZE(G%XLAT)))
ALLOCATE(ZDY(SIZE(G%XLAT)))
!
 CALL GET_GRIDTYPE_CARTESIAN(G%XGRID_PAR,ZLAT0,ZLON0, &
                              PDX=ZDX,PDY=ZDY        )  
!
!---------------------------------------------------------------------------
!
!*       2.    Computation of latitude and longitude
!              -------------------------------------
!
 CALL LATLON_CARTESIAN(ZLAT0,ZLON0,G%XLAT,G%XLON)
!
!-----------------------------------------------------------------------------
!
!*       3.    Compute grid size (2D array)
!              -----------------
!
G%XMESH_SIZE(:) = ZDX(:) * ZDY(:)
!
!-----------------------------------------------------------------------------
!
!*       4.    Direction of Y axis (from North) for each grid point
!              ----------------------------------------------------
!
PDIR(:) = 0.
!
!---------------------------------------------------------------------------
DEALLOCATE(ZDX)
DEALLOCATE(ZDY)
IF (LHOOK) CALL DR_HOOK('LATLON_GRIDTYPE_CARTESIAN',1,ZHOOK_HANDLE)
!---------------------------------------------------------------------------
!
END SUBROUTINE LATLON_GRIDTYPE_CARTESIAN

