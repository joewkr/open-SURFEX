!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##############################
MODULE MODD_GET_MESH_INDEX_CONF_PROJ
!     ##############################
!
!!****  *MODD_GRID_GAUSS - declaration of conformal grid characteristics for
!                          routine get_mesh_index_conf_proj
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
REAL                              :: XLAT0    ! reference latitude
REAL                              :: XLON0    ! reference longitude
REAL                              :: XRPK     ! projection parameter 
!                                             !   K=1 : stereographic north pole
!                                             ! 0<K<1 : Lambert, north hemisphere
!                                             !   K=0 : Mercator
!                                             !-1<K<0 : Lambert, south hemisphere
!                                             !   K=-1: stereographic south pole
REAL                              :: XBETA    ! angle between grid and reference longitude
REAL                              :: XLATOR   ! latitude  of point of coordinates X=0, Y=0
REAL                              :: XLONOR   ! longitude of point of coordinates X=0, Y=0
INTEGER                           :: NIMAX    ! number of points in I direction
INTEGER                           :: NJMAX    ! number of points in J direction
!
REAL, DIMENSION(:), ALLOCATABLE   :: XXLIM    ! X left   limit of grid mesh (dim IIMAX+1)
REAL, DIMENSION(:), ALLOCATABLE   :: XYLIM    ! Y bottom limit of grid mesh (dim IJMAX+1)

END MODULE MODD_GET_MESH_INDEX_CONF_PROJ
