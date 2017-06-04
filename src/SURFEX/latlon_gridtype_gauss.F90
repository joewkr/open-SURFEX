!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#########################################################################
SUBROUTINE LATLON_GRIDTYPE_GAUSS(G,KL,PDIR)
!#########################################################################
!
!!****  *LATLON_GRIDTYPE_GAUSS* - routine to get the horizontal geographic fields
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
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004 
!!      F. Taillefer   11/2007  correct estimation of the grid meshes
!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SFX_GRID_n, ONLY : GRID_t
!
USE MODE_GRIDTYPE_GAUSS
!
USE MODD_CSTS, ONLY : XRADIUS, XPI
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
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!---------------------------------------------------------------------------
!
!*       1.    get grid component
!              ------------------
!
IF (LHOOK) CALL DR_HOOK('LATLON_GRIDTYPE_GAUSS',0,ZHOOK_HANDLE)
 CALL GET_GRIDTYPE_GAUSS(G%XGRID_PAR,PLAT=G%XLAT,PLON=G%XLON,PMESH_SIZE=G%XMESH_SIZE)
!
!-----------------------------------------------------------------------------
!
!*       2.    Direction of Y axis (from North) for each grid point (degrees)
!              ----------------------------------------------------
!
!* the following formulae is given for clockwise angles.
!
!A COMPLETER
!
PDIR(:) = 0.
IF (LHOOK) CALL DR_HOOK('LATLON_GRIDTYPE_GAUSS',1,ZHOOK_HANDLE)
!
!WHERE (PDIR(:) <0.)    PDIR = PDIR + 360.
!WHERE (PDIR(:) >=360.) PDIR = PDIR - 360.
!
!---------------------------------------------------------------------------
!
END SUBROUTINE LATLON_GRIDTYPE_GAUSS
