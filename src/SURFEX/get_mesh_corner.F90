!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_MESH_CORNER (UG, &
                                  KLUOUT,PCORNER_LAT,PCORNER_LON)
!     ##############################################################
!
!!**** *GET_MESH_CORNER* get the grid cell corner for each (lat,lon)
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
!
!
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
!
USE MODI_GET_MESH_CORNER_CARTESIAN
USE MODI_GET_MESH_CORNER_CONF_PROJ
USE MODI_GET_MESH_CORNER_GAUSS
USE MODI_GET_MESH_CORNER_IGN
USE MODI_GET_MESH_CORNER_LONLAT_REG
USE MODI_GET_MESH_CORNER_LONLATVAL
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
!
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
!
INTEGER,                         INTENT(IN)    :: KLUOUT      ! output listing
REAL,    DIMENSION(:,:),         INTENT(OUT)   :: PCORNER_LAT ! Grid corner Latitude
REAL,    DIMENSION(:,:),         INTENT(OUT)   :: PCORNER_LON ! Grid corner Longitude
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
INTEGER :: IL, IC
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!----------------------------------------------------------------------------
!
!*    1.     Get position
!            ------------
!
IF (LHOOK) CALL DR_HOOK('GET_MESH_CORNER',0,ZHOOK_HANDLE)
!
IL = SIZE(PCORNER_LAT,1)
IC = SIZE(PCORNER_LAT,2)
!
SELECT CASE (UG%G%CGRID)
!     
  CASE("LONLAT REG")
      CALL GET_MESH_CORNER_LONLAT_REG(UG%G%NGRID_PAR,IL,IC,UG%G%XGRID_PAR,PCORNER_LAT,PCORNER_LON) 
  CASE("CARTESIAN")
      CALL GET_MESH_CORNER_CARTESIAN(UG%G%NGRID_PAR,IL,IC,UG%G%XGRID_PAR,PCORNER_LAT,PCORNER_LON)        
  CASE("CONF PROJ")
      CALL GET_MESH_CORNER_CONF_PROJ(UG%G%NGRID_PAR,IL,IC,UG%G%XGRID_PAR,PCORNER_LAT,PCORNER_LON)      
  CASE("GAUSS     ")
      CALL GET_MESH_CORNER_GAUSS(UG%G%NGRID_PAR,IL,IC,UG%G%XGRID_PAR,PCORNER_LAT,PCORNER_LON)
  CASE("IGN       ")
      CALL GET_MESH_CORNER_IGN(UG%G%NGRID_PAR,IL,IC,UG%G%XGRID_PAR,PCORNER_LAT,PCORNER_LON)  
  CASE("LONLATVAL ")
      CALL GET_MESH_CORNER_LONLATVAL(UG%G%NGRID_PAR,IL,IC,UG%G%XGRID_PAR,PCORNER_LAT,PCORNER_LON)  
  CASE DEFAULT
    WRITE(KLUOUT,*) 'error in grid cell corner computations (routine GET_MESH_CORNER)'
    WRITE(KLUOUT,*) 'It is impossible to retrieve geographical coordinates (latitude, longitude)'
    WRITE(KLUOUT,*) 'for the following grid type: CGRID = ', UG%G%CGRID
    CALL ABOR1_SFX('GET_MESH_CORNER: IMPOSSIBLE TO CALCULATE GRID CELL CORNER')
!    
END SELECT
!
IF (LHOOK) CALL DR_HOOK('GET_MESH_CORNER',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_MESH_CORNER
