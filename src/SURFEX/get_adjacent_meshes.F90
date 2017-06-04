!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_ADJACENT_MESHES(HGRID,KGRID_PAR,KL,PGRID_PAR,KLEFT,KRIGHT,KTOP,KBOTTOM)
!     ##############################################################
!
!!**** *GET_ADJACENT_MESHES* get the near grid mesh indices
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
!!                10/2007 E. Martin IGN Grid
!!                10/2014 P. Samuelsson SMHI  Rotated lonlat
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
USE MODI_GET_ADJ_MES_CART
USE MODI_GET_ADJ_MES_CONF_PROJ
USE MODI_GET_ADJ_MES_GAUSS
USE MODI_GET_ADJ_MES_IGN
USE MODI_GET_ADJ_MES_LONLAT_REG
USE MODI_GET_ADJ_MES_LONLATVAL
USE MODI_GET_ADJ_MES_LONLAT_ROT
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=10),               INTENT(IN)    :: HGRID     ! grid type
INTEGER,                         INTENT(IN)    :: KGRID_PAR ! size of PGRID_PAR
INTEGER,                         INTENT(IN)    :: KL        ! number of points
REAL,    DIMENSION(KGRID_PAR),   INTENT(IN)    :: PGRID_PAR ! grid parameters
INTEGER, DIMENSION(KL),          INTENT(OUT)   :: KLEFT     ! left   mesh index
INTEGER, DIMENSION(KL),          INTENT(OUT)   :: KRIGHT    ! right  mesh index
INTEGER, DIMENSION(KL),          INTENT(OUT)   :: KTOP      ! top    mesh index
INTEGER, DIMENSION(KL),          INTENT(OUT)   :: KBOTTOM   ! bottom mesh index
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_ADJACENT_MESHES',0,ZHOOK_HANDLE)
SELECT CASE (HGRID)
!     
  CASE("CONF PROJ ")

         CALL GET_ADJ_MES_CONF_PROJ(KGRID_PAR,KL,PGRID_PAR,KLEFT,KRIGHT,KTOP,KBOTTOM)
  CASE("CARTESIAN ")
         CALL GET_ADJ_MES_CART(KGRID_PAR,KL,PGRID_PAR,KLEFT,KRIGHT,KTOP,KBOTTOM)

  CASE("LONLAT REG")
         CALL GET_ADJ_MES_LONLAT_REG(KGRID_PAR,KL,PGRID_PAR,KLEFT,KRIGHT,KTOP,KBOTTOM)

  CASE("GAUSS     ")
         CALL GET_ADJ_MES_GAUSS(KGRID_PAR,KL,PGRID_PAR,KLEFT,KRIGHT,KTOP,KBOTTOM)

  CASE("IGN       ")
         CALL GET_ADJ_MES_IGN(KGRID_PAR,KL,PGRID_PAR,KLEFT,KRIGHT,KTOP,KBOTTOM)

  CASE("LONLATVAL ")
         CALL GET_ADJ_MES_LONLATVAL(KGRID_PAR,KL,PGRID_PAR,KLEFT,KRIGHT,KTOP,KBOTTOM)

  CASE("LONLAT ROT")
         CALL GET_ADJ_MES_LONLAT_ROT(KGRID_PAR,KL,PGRID_PAR,KLEFT,KRIGHT,KTOP,KBOTTOM)

  CASE("NONE      ")
    KLEFT  (:) = 0
    KRIGHT (:) = 0
    KTOP   (:) = 0
    KBOTTOM(:) = 0

END SELECT
IF (LHOOK) CALL DR_HOOK('GET_ADJACENT_MESHES',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_ADJACENT_MESHES
