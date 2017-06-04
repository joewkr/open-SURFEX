!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ################
      MODULE MODD_GRID_GAUSS
!     ################
!
!!****  *MODD_GRID_GAUSS - declaration of Gauss grid characteristics
!!
!!    PURPOSE
!!    -------
!     Used if CINGRID_TYPE = 'GAUSS '
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
!!      Original       01/2004
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
REAL                               :: XILA1   ! Lat. (y) of first input point
REAL                               :: XILO1   ! Lon. (x) of first input point
REAL                               :: XILA2   ! Lat. (y) of last input point
REAL                               :: XILO2   ! Lon. (x) of last input point
INTEGER                            :: NINLA   ! Number of parallels
INTEGER, DIMENSION(:), ALLOCATABLE :: NINLO   ! Nb. of points on each parallel
INTEGER                            :: NILEN   ! size of input arrays
!
LOGICAL                             :: LROTPOLE! .TRUE. if pole is rotated
REAL                                :: XLAP    ! Latitude of stretching pole
REAL                                :: XLOP    ! Longitude of stretching pole
REAL                                :: XCOEF   ! Stretching coefficient
!
REAL, DIMENSION(:), ALLOCATABLE :: XLAT, XLON
!
END MODULE MODD_GRID_GAUSS

