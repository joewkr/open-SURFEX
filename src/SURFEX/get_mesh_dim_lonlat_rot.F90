!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_MESH_DIM_LONLAT_ROT(KGRID_PAR,KL,PGRID_PAR,PDX,PDY)
!     ##############################################################
!
!!**** *GET_MESH_DIM_LONLAT_ROT* get the grid mesh dimensions
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    P. Samuelsson  SMHI
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    12/2012
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_CSTS, ONLY : XPI, XRADIUS
!
USE MODE_GRIDTYPE_LONLAT_ROT
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
INTEGER,                         INTENT(IN)    :: KGRID_PAR ! size of PGRID_PAR
INTEGER,                         INTENT(IN)    :: KL        ! number of points
REAL,    DIMENSION(KGRID_PAR),   INTENT(IN)    :: PGRID_PAR ! grid parameters
REAL,    DIMENSION(KL),          INTENT(OUT)   :: PDX       ! dimension in x dir. (meters)
REAL,    DIMENSION(KL),          INTENT(OUT)   :: PDY       ! dimension in y dir. (meters)
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
REAL, DIMENSION(KL) :: ZLAT     ! rotated latitude
REAL    :: ZWEST   ! West longitude in rotated grid (degrees)
REAL    :: ZSOUTH  ! South latitude in rotated grid  (degrees)
REAL    :: ZDLON   ! Longitudal grid spacing  (degrees)
REAL    :: ZDLAT   ! Latitudal grid spacing  (degrees)
REAL    :: ZPOLON  ! Longitude of rotated pole (degrees)
REAL    :: ZPOLAT  ! Latitude of rotated pole  (degrees)
INTEGER :: ILON    ! number of points in longitude
INTEGER :: ILAT    ! number of points in latitude
INTEGER :: JLON, JLAT, JL
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!----------------------------------------------------------------------------
!
!*       1.    Get grid information
!              --------------------
!
IF (LHOOK) CALL DR_HOOK('GET_MESH_DIM_LONLAT_ROT',0,ZHOOK_HANDLE)
!
 CALL GET_GRIDTYPE_LONLAT_ROT(PGRID_PAR,                                 &
                               ZWEST,ZSOUTH,ZDLON,ZDLAT,ZPOLON,ZPOLAT,  &
                               ILON,ILAT                                )  
!
!-----------------------------------------------------------------------------
!
!*       2.    Compute grid size
!              -----------------
!
DO JLAT=1,ILAT
  DO JLON=1,ILON
    JL = JLON + ILON * (JLAT-1)
    ZLAT(JL) = ZSOUTH + ZDLAT * (JLAT-1)
  END DO
END DO
!
PDX(:) = ZDLON * (XPI / 180.) * XRADIUS * COS(ZLAT(:)*XPI/180.)
PDY(:) = ZDLAT * (XPI / 180.) * XRADIUS
IF (LHOOK) CALL DR_HOOK('GET_MESH_DIM_LONLAT_ROT',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_MESH_DIM_LONLAT_ROT
