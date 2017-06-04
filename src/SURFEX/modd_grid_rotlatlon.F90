!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ################
      MODULE MODD_GRID_ROTLATLON
!     ################
!
!!****  *MODD_GRID_ROTLATLON - declaration of rotated lat lon characteristics
!!
!!    PURPOSE
!!    -------
!     Used if CINGRID_TYPE = 'ROTLATLON'
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
!!      U, Andrae    *SMHI'
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       10/2007
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
INTEGER                            :: NRY      ! Number of points in Y-direction
INTEGER                            :: NRX      ! Number of points in X-direction
REAL                               :: XRILA1   ! Lat. (y) of first input point
REAL                               :: XRILO1   ! Lon. (x) of first input point
REAL                               :: XRILA2   ! Lat. (y) of last input point
REAL                               :: XRILO2   ! Lon. (x) of last input point
REAL                               :: XRLAP    ! Latitude of rotated pole
REAL                               :: XRLOP    ! Longitude of rotated pole
REAL                               :: XRDY     ! Grid size in Y-direction in degrees
REAL                               :: XRDX     ! Grid size in X-direction in degrees
!
LOGICAL                            :: LRROTPOLE! .TRUE. if pole is rotated

END MODULE MODD_GRID_ROTLATLON

