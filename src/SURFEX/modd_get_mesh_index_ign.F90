!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##############################
MODULE MODD_GET_MESH_INDEX_IGN
!     ##############################
!
!!****  *MODD_GRID_GAUSS - declaration of conformal grid characteristics for
!                          routine get_mesh_index_IGN
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
REAL, DIMENSION(:), ALLOCATABLE   :: XXLIM    ! X left   limit of grid mesh
REAL, DIMENSION(:), ALLOCATABLE   :: XYLIM    ! Y bottom limit of grid mesh
REAL, DIMENSION(:), ALLOCATABLE   :: XXLIMS   ! X left   limit of grid mesh
INTEGER, DIMENSION(:), ALLOCATABLE :: NXIDS
INTEGER, DIMENSION(:), ALLOCATABLE :: NFRACD
REAL, DIMENSION(:), ALLOCATABLE   :: XDX      ! X size of grid mesh
REAL, DIMENSION(:), ALLOCATABLE   :: XDY      ! Y size of grid mesh
REAL                              :: XX_MIN   ! minimum X of the whole grid
REAL                              :: XX_MAX   ! maximum X of the whole grid
REAL                              :: XY_MIN   ! minimum Y of the whole grid
REAL                              :: XY_MAX   ! maximum Y of the whole grid
REAL                              :: XDX_MAX   ! maximum X of the whole grid

END MODULE MODD_GET_MESH_INDEX_IGN
