!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_NEAR_MESHES(HGRID,KGRID_PAR,KL,PGRID_PAR,KNEAR_NBR,KNEAR)
!     ##############################################################
!
!!**** *GET_NEAR_MESHES* get the near grid mesh indices
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
!!                10/2007 IGN Grids
!!                P. Samuelsson  SMHI 10/2014 Rotated lonlat
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
USE MODI_GET_NEAR_MESHES_CARTESIAN
USE MODI_GET_NEAR_MESHES_CONF_PROJ
USE MODI_GET_NEAR_MESHES_GAUSS
USE MODI_GET_NEAR_MESHES_IGN
USE MODI_GET_NEAR_MESHES_LONLAT_REG
USE MODI_GET_NEAR_MESHES_LONLATVAL
USE MODI_GET_NEAR_MESHES_LONLAT_ROT
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=10),               INTENT(IN)    :: HGRID     ! grid type
INTEGER,                         INTENT(IN)    :: KGRID_PAR ! size of PGRID_PAR
INTEGER,                         INTENT(IN)    :: KL        ! number of points
INTEGER,                         INTENT(IN)    :: KNEAR_NBR ! number of nearest points wanted
REAL,    DIMENSION(KGRID_PAR),   INTENT(IN)    :: PGRID_PAR ! grid parameters
INTEGER, DIMENSION(:,:),POINTER :: KNEAR     ! near mesh indices
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_NEAR_MESHES',0,ZHOOK_HANDLE)
SELECT CASE (HGRID)
!     
  CASE("CONF PROJ ")
    CALL GET_NEAR_MESHES_CONF_PROJ(KGRID_PAR,KL,PGRID_PAR,KNEAR_NBR,KNEAR)

  CASE("CARTESIAN ")
    CALL GET_NEAR_MESHES_CARTESIAN(KGRID_PAR,KL,PGRID_PAR,KNEAR_NBR,KNEAR)

  CASE("LONLAT REG")
    CALL GET_NEAR_MESHES_LONLAT_REG(KGRID_PAR,KL,PGRID_PAR,KNEAR_NBR,KNEAR)

  CASE("GAUSS     ")
    CALL GET_NEAR_MESHES_GAUSS(KGRID_PAR,KL,PGRID_PAR,KNEAR_NBR,KNEAR)

  CASE("IGN       ")
    CALL GET_NEAR_MESHES_IGN(KGRID_PAR,KL,PGRID_PAR,KNEAR_NBR,KNEAR)

  CASE("LONLATVAL ")
    CALL GET_NEAR_MESHES_LONLATVAL(KGRID_PAR,KL,PGRID_PAR,KNEAR_NBR,KNEAR)

  CASE("LONLAT ROT")
    CALL GET_NEAR_MESHES_LONLAT_ROT(KGRID_PAR,KL,PGRID_PAR,KNEAR_NBR,KNEAR)

  CASE("NONE      ")
    KNEAR(:,:) = 0

END SELECT
IF (LHOOK) CALL DR_HOOK('GET_NEAR_MESHES',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_NEAR_MESHES
