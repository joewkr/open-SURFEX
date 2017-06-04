!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_GRID_DIM(HGRID,KGRID_PAR,PGRID_PAR,ORECT,KDIM1,KDIM2)
!     ##############################################################
!
!!**** *GET_GRID_DIM* get the grid mesh dimensions
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
!!      07/2011     add IGN grid (B. Decharme)
!!      P. Samuelsson SMHI  10/2014   Rotated lonlat
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODI_GET_GRID_DIM_CONF_PROJ
USE MODI_GET_GRID_DIM_CARTESIAN
USE MODI_GET_GRID_DIM_LONLAT_REG
USE MODI_GET_GRID_DIM_GAUSS
USE MODI_GET_GRID_DIM_LONLATVAL
USE MODI_GET_GRID_DIM_IGN
USE MODI_GET_GRID_DIM_LONLAT_ROT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=10),               INTENT(IN)    :: HGRID     ! grid type
INTEGER,                         INTENT(IN)    :: KGRID_PAR ! size of PGRID_PAR
REAL,    DIMENSION(KGRID_PAR),   INTENT(IN)    :: PGRID_PAR ! grid parameters
LOGICAL,                         INTENT(OUT)   :: ORECT     ! T if rectangular grid
INTEGER,                         INTENT(OUT)   :: KDIM1     ! 1st dimension
INTEGER,                         INTENT(OUT)   :: KDIM2     ! 2nd dimension
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_GRID_DIM',0,ZHOOK_HANDLE)
SELECT CASE (HGRID)
!     
  CASE("CONF PROJ ")
    CALL GET_GRID_DIM_CONF_PROJ(KGRID_PAR,PGRID_PAR,ORECT,KDIM1,KDIM2)

  CASE("CARTESIAN ")
    CALL GET_GRID_DIM_CARTESIAN(KGRID_PAR,PGRID_PAR,ORECT,KDIM1,KDIM2)

  CASE("LONLAT REG")
    CALL GET_GRID_DIM_LONLAT_REG(KGRID_PAR,PGRID_PAR,ORECT,KDIM1,KDIM2)

  CASE("GAUSS     ")
    CALL GET_GRID_DIM_GAUSS(KGRID_PAR,PGRID_PAR,ORECT,KDIM1,KDIM2)

  CASE("LONLATVAL ")
    CALL GET_GRID_DIM_LONLATVAL(KGRID_PAR,PGRID_PAR,ORECT,KDIM1,KDIM2)

  CASE("IGN       ")
    CALL GET_GRID_DIM_IGN(KGRID_PAR,PGRID_PAR,ORECT,KDIM1,KDIM2)

  CASE("LONLAT ROT")
    CALL GET_GRID_DIM_LONLAT_ROT(KGRID_PAR,PGRID_PAR,ORECT,KDIM1,KDIM2)

  CASE("NONE      ")
    ORECT = .FALSE.
    KDIM1 = 0
    KDIM2 = 0

END SELECT
IF (LHOOK) CALL DR_HOOK('GET_GRID_DIM',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_GRID_DIM
