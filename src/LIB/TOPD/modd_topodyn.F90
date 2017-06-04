!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!-------------------------------------------------------------------------------
!     ##################
      MODULE MODD_TOPODYN
!     ##################
!
!!****  *MODD_TOPODYN - declaration of variables used by Topodyn
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
!!     F. Habets and K. Chancibault
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       29/09/03
!!      BV: modifications  2006: division in two part (some variables are
!                            now in modd_coupling_topo_n    
!!      BV: modifications  04/2007: addition of XTOPD_STEP and NNB_TOPD_STEP
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_TOPD_PAR, ONLY : JPCAT
!
IMPLICIT NONE
!
!-------------------------------------------------------------------------------
! Variables specific to Topodyn
!
 CHARACTER(LEN=15), DIMENSION(JPCAT) :: CCAT     ! base name for topographic files
INTEGER                             :: NNCAT    ! catchments number
!
INTEGER                             :: NNB_TOPD_STEP   ! number of TOPODYN time steps
REAL                                :: XTOPD_STEP      ! TOPODYN time step
!
INTEGER                             :: NMESHT   ! maximal number of catchments meshes

REAL, ALLOCATABLE, DIMENSION(:,:)   :: XDMAXT   ! maximal deficit on TOPODYN grid (m)
REAL, ALLOCATABLE, DIMENSION(:)     :: XDXT     ! catchment grid mesh size (m)
REAL, ALLOCATABLE, DIMENSION(:)     :: XMPARA   ! M parameter on TOPODYN grid (m)

INTEGER, ALLOCATABLE, DIMENSION(:)  :: NNMC     ! catchments pixels number
REAL, ALLOCATABLE, DIMENSION(:,:,:) :: XCONN    ! pixels reference number and 
                                                ! connections between
INTEGER, ALLOCATABLE, DIMENSION(:,:):: NLINE    ! second index of the pixel in the array 
                                                ! XCONN
REAL, ALLOCATABLE, DIMENSION(:,:)   :: XTANB    ! pixels topographic slope (Tan(Beta))
REAL, ALLOCATABLE, DIMENSION(:,:)   :: XSLOP    ! pixels topographic slope/length flow

!Variables à priori inutiles
REAL, ALLOCATABLE, DIMENSION(:,:)   :: XDAREA   ! drainage area (aire drainee)

! Variables defining the catchments

INTEGER, ALLOCATABLE, DIMENSION(:)  :: NNXC     ! number of topographic grid points on 
                                                ! abscissa axis
INTEGER, ALLOCATABLE, DIMENSION(:)  :: NNYC     ! number of topographic grid points on ordinate 
                                                ! axis
INTEGER, ALLOCATABLE, DIMENSION(:)  :: NNPT     ! number of pixels in the topographic 
                                                ! domain
INTEGER                             :: NPMAX    ! maximal number of pixels in the 
                                                ! topographic grid

REAL, ALLOCATABLE, DIMENSION(:)     :: XX0,XY0  ! coordinates bottom-left pixel of each 
                                                ! topographic domain

REAL, ALLOCATABLE, DIMENSION(:)     :: XNUL     ! undefined value in topographic files

REAL, ALLOCATABLE, DIMENSION(:,:)   :: XTOPD    ! topographic values in topographic files
REAL, DIMENSION(JPCAT)              :: XRTOP_D2 ! depth used by topodyn for lateral transfers
                                                ! (expressed in ratio of isba d2)
                                                !
! Variables used in routing module 
INTEGER, ALLOCATABLE, DIMENSION(:)  :: NNISO    ! number of time step for the isochrones
REAL, ALLOCATABLE, DIMENSION(:,:)   :: XCISO    ! isochrones routing constants 

REAL, DIMENSION(JPCAT)              :: XQINIT   ! Initial discharge at the outlet of the catchments
REAL, ALLOCATABLE, DIMENSION(:,:)   :: XQTOT    ! Total discharge at the outlet of the catchments

REAL, DIMENSION(JPCAT)              :: XSPEEDR,XSPEEDH ! River and hillslope speed
REAL, DIMENSION(JPCAT)              :: XSPEEDG         ! Ground speed
REAL, ALLOCATABLE, DIMENSION(:,:)   :: XDRIV, XDHIL    ! River and hillslope distances
REAL, ALLOCATABLE, DIMENSION(:,:)   :: XDGRD           ! Ground distance
REAL, ALLOCATABLE, DIMENSION(:,:)   :: XTIME_TOPD      ! Time to go to the outlet
                                                       ! at the soil surface
REAL, ALLOCATABLE, DIMENSION(:,:)   :: XTIME_TOPD_DRAIN! Time to go to the outlet in the ground

INTEGER, ALLOCATABLE, DIMENSION(:)  :: NX_STEP_ROUT   ! number of maximal time step to join the outlet of 
                                                ! any catchment

! Variables used in exfiltration module 
REAL, ALLOCATABLE, DIMENSION(:,:)   :: XLAMBDA  ! pure topographic index
REAL, ALLOCATABLE, DIMENSION(:,:)   :: XCSTOPT  ! hydraulic conductivity at saturation on 
                                                ! TOP-LAT grid
                                                !ludo
REAL, ALLOCATABLE, DIMENSION(:,:)   :: XQB_DR
REAL, ALLOCATABLE, DIMENSION(:,:)   :: XQB_RUN
! for topodyn alone
REAL, ALLOCATABLE, DIMENSION(:)   :: XRI,XRI_PREV! recharge on ISBA grid
REAL, ALLOCATABLE, DIMENSION(:)   :: XSRFULL! reservoir of interception for
!TOPODYN only
REAL, ALLOCATABLE, DIMENSION(:,:) :: XDEFT! pixel deficit
!
!-------------------------------------------------------------------------------------
!
END MODULE MODD_TOPODYN

