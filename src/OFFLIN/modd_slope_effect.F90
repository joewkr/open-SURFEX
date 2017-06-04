!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ######################
MODULE MODD_SLOPE_EFFECT
!     ######################
!
!!****  *MODD_SLOPE_EFFECT - declaration of parameters relative to
!! computations of slopes and aspect effects on direct shortwave
!radiations 
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
!!      V. Vionnet   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       15/04/11
!!!                03/14  : M Lafaysse, modifs for optimization and parallelization
!*       0.   DECLARATIONS
!             ------------
!
!
IMPLICIT NONE


INTEGER                            :: NNX !  number of points in X direction for large domain (all processors) + 1 line left, right, up, down
INTEGER                            :: NNY !  number of points in Y direction for large domain (all processors) + 1 line left, right, up, down

INTEGER                            :: NIX=0 !  number of points in X direction for complete domain (all processors)
INTEGER                            :: NIY=0 !  number of points in Y direction for complete domain (all processors)

INTEGER,SAVE                       :: NNXLOC !  number of points in X direction for large domain (this processor) + 1 line left, right, up, down
INTEGER,SAVE                       :: NNYLOC !  number of points in Y direction for large domain (this processor) + 1 line left, right, up, down
INTEGER,SAVE                       :: NIXLOC !  number of points in X direction for complete domain (this processor)
INTEGER,SAVE                       :: NIYLOC !  number of points in Y direction for complete domain (this processor)


INTEGER,SAVE::NINDX1_X,NINDX2_X,NINDX1_Y,NINDX2_Y

LOGICAL :: LREVERTGRID

REAL, DIMENSION(:,:),ALLOCATABLE :: XZSL   ! orography in a 2D array
REAL, DIMENSION(:,:),ALLOCATABLE :: XZS_XY ! orography at southwest corner of the mesh 
REAL, DIMENSION(:),ALLOCATABLE :: XXHAT  ! X coordinate
REAL, DIMENSION(:),ALLOCATABLE :: XYHAT  ! Y coordinate

REAL, DIMENSION(:),ALLOCATABLE :: XXHAT_THREAD  ! X coordinate
REAL, DIMENSION(:),ALLOCATABLE :: XYHAT_THREAD  ! Y coordinate

REAL, DIMENSION(:,:,:),ALLOCATABLE                   :: XSLOPAZI ! azimuthal slope angle
REAL, DIMENSION(:,:,:),ALLOCATABLE                   :: XSLOPANG ! vertical slope angle
REAL, DIMENSION(:,:,:),ALLOCATABLE                   :: XSURF_TRIANGLE ! surface of triangles

!idem for local processor
REAL,DIMENSION(:,:),ALLOCATABLE::XZS_THREAD,XZS_XY_THREAD
REAL,DIMENSION(:,:,:),ALLOCATABLE::XSLOPANG_THREAD,XSLOPAZI_THREAD,XSURF_TRIANGLE_THREAD

END MODULE MODD_SLOPE_EFFECT
