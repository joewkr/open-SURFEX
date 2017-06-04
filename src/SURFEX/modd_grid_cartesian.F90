!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ################
      MODULE MODD_GRID_CARTESIAN
!     ################
!
!!****  *MODD_GRID_CARTESIAN - declaration of Arome gris characteristics
!!
!!    PURPOSE
!!    -------
!     Used if CINGRID_TYPE = 'CONF PROJ '
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      V. Masson    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       20/09/02
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
REAL, DIMENSION(:), ALLOCATABLE    :: XX  ! X coordinate (meters)
REAL, DIMENSION(:), ALLOCATABLE    :: XY  ! Y coordinate (meters)
REAL, DIMENSION(:,:), ALLOCATABLE    :: XCX
REAL, DIMENSION(:,:), ALLOCATABLE    :: XCY
INTEGER, DIMENSION(:,:), ALLOCATABLE :: NCIJ
INTEGER :: NX  ! number of points in X direction
INTEGER :: NY  ! number of points in Y direction
!
REAL    :: XLAT0  ! reference latitude
REAL    :: XLON0  ! reference longitude
!
END MODULE MODD_GRID_CARTESIAN
