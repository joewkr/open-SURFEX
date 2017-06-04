!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE LATLON_GRID(G,KL,PDIR)
!     #########################################################################
!
!!****  *LATLON_GRID* - routine to compute the horizontal geographic fields
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
!!                  10/2007 (E. Martin) IGN grids
!!                  12/2012 (P. Samuelsson SMHI) Rotated lonlat
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SFX_GRID_n, ONLY : GRID_t
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
USE MODI_LATLON_GRIDTYPE_CARTESIAN
!
USE MODI_LATLON_GRIDTYPE_CONF_PROJ
!
USE MODI_LATLON_GRIDTYPE_GAUSS
!
USE MODI_LATLON_GRIDTYPE_IGN
!
USE MODI_LATLON_GRIDTYPE_LONLAT_REG
!
USE MODI_LATLON_GRIDTYPE_LONLATVAL
!
USE MODI_LATLON_GRIDTYPE_LONLAT_ROT
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(GRID_t), INTENT(INOUT) :: G
INTEGER,            INTENT(IN)  :: KL         ! number of points
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PDIR ! direction of main grid Y axis (deg. from N, clockwise)
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(KL) :: ZDIR
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!---------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('LATLON_GRID',0,ZHOOK_HANDLE)
SELECT CASE (G%CGRID)
!
!*    1.      Conformal projection grid
!             -------------------------
!
  CASE ('CONF PROJ ')
    CALL LATLON_GRIDTYPE_CONF_PROJ(G,KL,ZDIR)
    IF (PRESENT(PDIR)) PDIR = ZDIR
    ! note that all points of the grid will be kept, whatever the surface
    ! type under consideration (e.g. sea points will be kept even for
    ! initialization of continents)
    !

!*    2.      latitude/longitude grid
!             -----------------------
!
  CASE ('LONLAT REG')
    CALL LATLON_GRIDTYPE_LONLAT_REG(G,KL,ZDIR)
    IF (PRESENT(PDIR)) PDIR = ZDIR

!
!*    3.      Cartesian grid
!             --------------
!
  CASE ('CARTESIAN ')
    CALL LATLON_GRIDTYPE_CARTESIAN(G,KL,ZDIR)
    IF (PRESENT(PDIR)) PDIR = ZDIR
    ! note that all points of the grid will be kept, whatever the surface
    ! type under consideration (e.g. sea points will be kept even for
    ! initialization of continents)
    !
!*    4.      gaussian grid
!             -------------
!
  CASE ('GAUSS     ')
    CALL LATLON_GRIDTYPE_GAUSS(G,KL,ZDIR)
    IF (PRESENT(PDIR)) PDIR = ZDIR
!
!*    5.      IGN grid
!             --------
!
  CASE ('IGN       ')
    CALL LATLON_GRIDTYPE_IGN(G,KL,ZDIR)
    IF (PRESENT(PDIR)) PDIR = ZDIR
!
!*    6.      lonlatval grid
!             --------
!
  CASE ('LONLATVAL ')
    CALL LATLON_GRIDTYPE_LONLATVAL(G,KL,ZDIR)
    IF (PRESENT(PDIR)) PDIR = ZDIR
!
!*    7.      Rotated lonlat grid
!             -------------------
!
  CASE ('LONLAT ROT')
    CALL LATLON_GRIDTYPE_LONLAT_ROT(G,KL,ZDIR)
    IF (PRESENT(PDIR)) PDIR = ZDIR
!
!
  CASE DEFAULT
    CALL ABOR1_SFX('LATLON_GRID: GRID TYPE NOT SUPPORTED '//G%CGRID)

END SELECT
IF (LHOOK) CALL DR_HOOK('LATLON_GRID',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------------
!
END SUBROUTINE LATLON_GRID
