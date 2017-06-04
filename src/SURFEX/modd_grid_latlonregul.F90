!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ################
      MODULE MODD_GRID_LATLONREGUL
!     ################
!
!!****  *MODD_GRID_LATLONREGUL - declaration of Gauss grid characteristics
!!
!!    PURPOSE
!!    -------
!     Used if CINGRID_TYPE = 'LATLON'
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
!!      C. Lebeaupin Brossier    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       01/2008
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
REAL                               :: XILAT1   ! Lat. (y) of first input point
REAL                               :: XILON1   ! Lon. (x) of first input point
REAL                               :: XILAT2   ! Lat. (y) of last input point
REAL                               :: XILON2   ! Lon. (x) of last input point
INTEGER                            :: NINLAT   ! Number of parallels
INTEGER , DIMENSION(:), ALLOCATABLE:: NINLON   ! Nb. of points on each parallel
INTEGER                            :: NILENGTH ! size of input arrays
INTEGER                            :: NINDEPTH  ! nb of vertical levels
REAL, DIMENSION(:), ALLOCATABLE    :: XILATARRAY! latitudes values array
REAL, DIMENSION(:), ALLOCATABLE    :: XILONARRAY! longitudes values array
REAL, DIMENSION(:), ALLOCATABLE    :: XIDEPARRAY! depths values array
!
END MODULE MODD_GRID_LATLONREGUL
