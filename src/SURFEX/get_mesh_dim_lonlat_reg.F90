!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_MESH_DIM_LONLAT_REG(KGRID_PAR,KL,PGRID_PAR,PDX,PDY)
!     ##############################################################
!
!!**** *GET_MESH_DIM_LONLAT_REG* get the grid mesh dimensions
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
!!    V. Masson         Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    03/2004
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_CSTS, ONLY : XPI, XRADIUS
!
USE MODE_GRIDTYPE_LONLAT_REG
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
REAL, DIMENSION(KL) :: ZLAT     ! latitude
REAL, DIMENSION(KL) :: ZLON     ! longitude
REAL    :: ZLONMIN ! minimum longitude (degrees)
REAL    :: ZLONMAX ! maximum longitude (degrees)
REAL    :: ZLATMIN ! minimum latitude  (degrees)
REAL    :: ZLATMAX ! maximum latitude  (degrees)
INTEGER :: ILON    ! number of points in longitude
INTEGER :: ILAT    ! number of points in latitude
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!----------------------------------------------------------------------------
!
!*       1.    Get grid information
!              --------------------
!
IF (LHOOK) CALL DR_HOOK('GET_MESH_DIM_LONLAT_REG',0,ZHOOK_HANDLE)
 CALL GET_GRIDTYPE_LONLAT_REG(PGRID_PAR,ZLONMIN,ZLONMAX,                    &
                               ZLATMIN,ZLATMAX,ILON,ILAT,PLON=ZLON,PLAT=ZLAT )  
!
!-----------------------------------------------------------------------------
!
!*       2.    Compute grid size
!              -----------------
!
PDX(:) = (ZLONMAX-ZLONMIN)/FLOAT(ILON) * (XPI / 180.) * XRADIUS * COS(ZLAT(:)*XPI/180.)
PDY(:) = (ZLATMAX-ZLATMIN)/FLOAT(ILAT) * (XPI / 180.) * XRADIUS
IF (LHOOK) CALL DR_HOOK('GET_MESH_DIM_LONLAT_REG',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_MESH_DIM_LONLAT_REG
