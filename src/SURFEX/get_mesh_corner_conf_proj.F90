!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #######################################################################################
      SUBROUTINE GET_MESH_CORNER_CONF_PROJ(KGRID_PAR,KL,KC,PGRID_PAR,PCORNER_LAT,PCORNER_LON)
!     #######################################################################################
!
!!**** *GET_MESH_CORNER_CONF_PROJ* get the grid mesh where point (lat,lon) is located
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
USE MODE_GRIDTYPE_CONF_PROJ
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
INTEGER             :: JC       ! loop on corner
!
INTEGER             :: INI      ! Number of point
!
REAL                :: ZLAT0    ! reference latitude
REAL                :: ZLON0    ! reference longitude
REAL                :: ZRPK     ! projection parameter 
REAL                :: ZBETA    ! angle between grid and reference longitude
REAL                :: ZLATOR   ! latitude  of point of coordinates X=0, Y=0
REAL                :: ZLONOR   ! longitude of point of coordinates X=0, Y=0
REAL, DIMENSION(KL) :: ZX       ! X conformal coordinate
REAL, DIMENSION(KL) :: ZY       ! Y conformal coordinate
REAL, DIMENSION(KL) :: ZDX      ! size in X conformal coordinate
REAL, DIMENSION(KL) :: ZDY      ! size in Y conformal coordinate
!
REAL, DIMENSION(KL,KC) :: ZCX ! Grid corner in X
REAL, DIMENSION(KL,KC) :: ZCY ! Grid corner in Y
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_MESH_CORNER_CONF_PROJ',0,ZHOOK_HANDLE)
!
!*    1.     Gets parameters of the projection
!            ---------------------------------
!
CALL GET_GRIDTYPE_CONF_PROJ(PGRID_PAR,KL=INI)
!  
IF(KL/=INI)THEN
  CALL ABOR1_SFX('GET_MESH_CORNER_CONF_PROJ: WRONG NUMBER OF POINT')
ENDIF
!
CALL GET_GRIDTYPE_CONF_PROJ(PGRID_PAR,ZLAT0,ZLON0,ZRPK,ZBETA,ZLATOR, &
                            ZLONOR,PX=ZX,PY=ZY,PDX=ZDX,PDY=ZDY       )
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
ZCX(:,1) = ZX(:)-ZDX(:)/2.
ZCY(:,1) = ZY(:)-ZDY(:)/2.
!
ZCX(:,3) = ZX(:)+ZDX(:)/2.
ZCY(:,3) = ZY(:)+ZDY(:)/2.
!
ZCX(:,2) = ZCX(:,3)
ZCY(:,2) = ZCY(:,1)
!
ZCX(:,4) = ZCX(:,1)
ZCY(:,4) = ZCY(:,3)
!
DO JC=1,KC
   CALL LATLON_CONF_PROJ(ZLAT0,ZLON0,ZRPK,ZBETA,ZLATOR,ZLONOR,ZCX(:,JC), &
                         ZCY(:,JC),PCORNER_LAT(:,JC),PCORNER_LON(:,JC)   )   
ENDDO
!
IF (LHOOK) CALL DR_HOOK('GET_MESH_CORNER_CONF_PROJ',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE GET_MESH_CORNER_CONF_PROJ
