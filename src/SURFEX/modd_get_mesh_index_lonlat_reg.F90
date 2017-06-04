!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##############################
MODULE MODD_GET_MESH_INDEX_LONLAT_REG
!     ##############################
!
!!****  *MODD_GRID_GAUSS - declaration of reular lon/lat grid characteristics for
!                          routine get_mesh_index_lonlat_reg
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
!!      V. Masson    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       10/2006
!
IMPLICIT NONE
!
!*       0.   DECLARATIONS
!             ------------
!
REAL, DIMENSION(:), ALLOCATABLE   :: XLONLIM  ! longitude left   limit of grid mesh (dim ILON+1)
REAL, DIMENSION(:), ALLOCATABLE   :: XLATLIM  ! latitude  bottom limit of grid mesh (dim ILAT+1)
!
INTEGER, DIMENSION(:), ALLOCATABLE :: NFRACDLON
INTEGER, DIMENSION(:), ALLOCATABLE :: NFRACDLAT
!
INTEGER :: NLON    ! number of points in longitude
INTEGER :: NLAT    ! number of points in latitude
REAL    :: XLON0   ! centre longitude of the grid
!
END MODULE MODD_GET_MESH_INDEX_LONLAT_REG
