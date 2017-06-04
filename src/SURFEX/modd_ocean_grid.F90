!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################
      MODULE MODD_OCEAN_GRID
!     ##################
!
!!****  *MODD_OCEAN_GRID - declaration of grid for oceanic model
!!
!!    PURPOSE
!!    -------
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
!!      C. Lebeaupin Brossier   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       01/2008
!       11/2014 : NOCKMAX not parameter
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
INTEGER, SAVE :: NOCKMIN  !first ocean level indice
INTEGER, SAVE :: NOCKMAX  ! last ocean level indice
!
REAL, POINTER, DIMENSION(:) :: XK1
REAL, POINTER, DIMENSION(:) :: XK2
REAL, POINTER, DIMENSION(:) :: XK3
REAL, POINTER, DIMENSION(:) :: XK4
REAL, POINTER, DIMENSION(:) :: XZHOC
REAL, POINTER, DIMENSION(:) :: XZ2
REAL, POINTER, DIMENSION(:) :: XDZ1
REAL, POINTER, DIMENSION(:) :: XDZ2
REAL, POINTER, DIMENSION(:) :: XRAY
!
END MODULE MODD_OCEAN_GRID
