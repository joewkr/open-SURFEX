!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###############################################################
      SUBROUTINE GET_MESH_CORNER_LONLAT_REG(KGRID_PAR,KL,KC,PGRID_PAR,PCORNER_LAT,PCORNER_LON)
!     ###############################################################
!
!!**** *GET_MESH_CORNER_LONLAT_REG* get the grid mesh where point (lat,lon) is located
!!
!!    PURPOSE
!!    -------
!!
!!    AUTHOR
!!    ------
!!
!!    B. Decharme         Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    10/2013
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODE_GRIDTYPE_LONLAT_REG
!
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
INTEGER,                       INTENT(IN)    :: KGRID_PAR ! size of PGRID_PAR
INTEGER,                       INTENT(IN)    :: KL        ! number of points
INTEGER,                       INTENT(IN)    :: KC        ! number of grid point corner
REAL,    DIMENSION(KGRID_PAR), INTENT(IN)    :: PGRID_PAR ! grid parameters
REAL,    DIMENSION(KL,KC),     INTENT(OUT)   :: PCORNER_LAT ! Grid corner Latitude
REAL,    DIMENSION(KL,KC),     INTENT(OUT)   :: PCORNER_LON ! Grid corner Longitude
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
INTEGER             :: ILAT    ! Number of lattitude
INTEGER             :: ILON    ! Number of longitude
INTEGER             :: INI     ! Number of point
REAL                :: ZLONMIN ! minimum longitude (degrees)
REAL                :: ZLONMAX ! maximum longitude (degrees)
REAL                :: ZLATMIN ! minimum latitude  (degrees)
REAL                :: ZLATMAX ! maximum latitude  (degrees)
REAL                :: ZDLON   ! longitude grid size
REAL                :: ZDLAT   ! latitude  grid size
REAL, DIMENSION(KL) :: ZLON    ! longitude grid
REAL, DIMENSION(KL) :: ZLAT    ! latitude  grid
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_MESH_CORNER_LONLAT_REG',0,ZHOOK_HANDLE)
!
!*    1.     Uncode parameters of the grid
!            -----------------------------
!
CALL GET_GRIDTYPE_LONLAT_REG(PGRID_PAR,ZLONMIN,ZLONMAX,ZLATMIN,ZLATMAX,ILON,ILAT,INI)  
!  
IF(KL/=INI)THEN
  CALL ABOR1_SFX('GET_MESH_CORNER_LONLAT_REG: WRONG NUMBER OF POINT')
ENDIF
!
CALL GET_GRIDTYPE_LONLAT_REG(PGRID_PAR,PLON=ZLON,PLAT=ZLAT)
!
ZDLON = (ZLONMAX-ZLONMIN) / FLOAT(ILON)
ZDLAT = (ZLATMAX-ZLATMIN) / FLOAT(ILAT)
!
!
!*    2.     grid cell corner (counterclockwise sense)
!            -----------------------------------------
!
!       4_______3
!       |       |
!       |   .   |
!       |       |
!       |_______|
!       1       2
!
!
PCORNER_LON(:,1) = ZLON(:)-ZDLON/2.
PCORNER_LAT(:,1) = ZLAT(:)-ZDLAT/2.
!
PCORNER_LON(:,3) = ZLON(:)+ZDLON/2.
PCORNER_LAT(:,3) = ZLAT(:)+ZDLAT/2.
!
PCORNER_LON(:,2) = PCORNER_LON(:,3)
PCORNER_LAT(:,2) = PCORNER_LAT(:,1)
!
PCORNER_LON(:,4) = PCORNER_LON(:,1)
PCORNER_LAT(:,4) = PCORNER_LAT(:,3)
!
IF (LHOOK) CALL DR_HOOK('GET_MESH_CORNER_LONLAT_REG',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_MESH_CORNER_LONLAT_REG
