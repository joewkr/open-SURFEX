!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##############################
MODULE MODD_GET_MESH_INDEX_LONLAT_ROT
!     ##############################
!
!!**** - declaration of rotated lon/lat grid characteristics for
!        routine get_mesh_index_lonlat_rot
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
!!      P. Samuelsson  SMHI
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       12/2012
!
IMPLICIT NONE
!
!*       0.   DECLARATIONS
!             ------------
!
REAL, DIMENSION(:), ALLOCATABLE   :: XLONLIM  ! longitude left   limit of rotated grid mesh (dim ILON+1)
REAL, DIMENSION(:), ALLOCATABLE   :: XLATLIM  ! latitude  bottom limit of rotated grid mesh (dim ILAT+1)
!
INTEGER :: NLON    ! number of points in longitude
INTEGER :: NLAT    ! number of points in latitude
REAL    :: XLON0   ! centre longitude of the rotated grid
REAL    :: XPOLON  ! Longitude of rotated pole (degrees)
REAL    :: XPOLAT  ! Latitude of rotated pole  (degrees)
!
END MODULE MODD_GET_MESH_INDEX_LONLAT_ROT
