!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_MESH_DIM(HGRID,KGRID_PAR,KL,PGRID_PAR,PDX,PDY,PMESHSIZE)
!     ##############################################################
!
!!**** *GET_MESH_DIM* get the grid mesh dimensions
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
!!    P. Samuelsson   SMHI   10/2014   Rotated lonlat
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_MESH_DIM_CONF_PROJ
USE MODI_GET_MESH_DIM_GAUSS
USE MODI_GET_MESH_DIM_IGN
USE MODI_GET_MESH_DIM_LONLAT_REG
USE MODI_GET_MESH_DIM_LONLATVAL
USE MODI_GET_MESH_DIM_LONLAT_ROT
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=10),               INTENT(IN)    :: HGRID     ! grid type
INTEGER,                         INTENT(IN)    :: KGRID_PAR ! size of PGRID_PAR
INTEGER,                         INTENT(IN)    :: KL        ! number of points
REAL,    DIMENSION(KGRID_PAR),   INTENT(IN)    :: PGRID_PAR ! grid parameters
REAL,    DIMENSION(KL),          INTENT(OUT)   :: PDX       ! dimension in x dir. (meters)
REAL,    DIMENSION(KL),          INTENT(OUT)   :: PDY       ! dimension in y dir. (meters)
REAL,    DIMENSION(KL),          INTENT(IN)    :: PMESHSIZE ! mesh size (m2)
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_MESH_DIM',0,ZHOOK_HANDLE)
SELECT CASE (HGRID)
!     
  CASE("CONF PROJ ")
    CALL GET_MESH_DIM_CONF_PROJ(KGRID_PAR,KL,PGRID_PAR,PDX,PDY)

  CASE("LONLAT REG")
    CALL GET_MESH_DIM_LONLAT_REG(KGRID_PAR,KL,PGRID_PAR,PDX,PDY)

  CASE("GAUSS     ")
    CALL GET_MESH_DIM_GAUSS(KGRID_PAR,KL,PGRID_PAR,PMESHSIZE,PDX,PDY)

  CASE("IGN       ")
    CALL GET_MESH_DIM_IGN(KGRID_PAR,KL,PGRID_PAR,PDX,PDY)

  CASE("LONLATVAL ")
    CALL GET_MESH_DIM_LONLATVAL(KGRID_PAR,KL,PGRID_PAR,PDX,PDY)

  CASE("LONLAT ROT")
    CALL GET_MESH_DIM_LONLAT_ROT(KGRID_PAR,KL,PGRID_PAR,PDX,PDY)

  CASE("NONE      ")
    PDX(:) = SQRT(PMESHSIZE)
    PDY(:) = SQRT(PMESHSIZE)

END SELECT
IF (LHOOK) CALL DR_HOOK('GET_MESH_DIM',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_MESH_DIM
