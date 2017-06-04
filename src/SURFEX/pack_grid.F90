!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PACK_GRID(KMASK,HGRID1,HGRID2,PGRID_PAR1,PGRID_PAR2)
!     ##############################################################
!
!!**** *PACK_GRID* packs the grid definition vector
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
!!    P. Samuelsson   SMHI   12/2012  Rotated lonlat
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
USE MODI_ABOR1_SFX
!
USE MODI_PACK_GRID_CARTESIAN
!
USE MODI_PACK_GRID_CONF_PROJ
!
USE MODI_PACK_GRID_GAUSS
!
USE MODI_PACK_GRID_IGN
!
USE MODI_PACK_GRID_LONLAT_REG
!
USE MODI_PACK_GRID_LONLATVAL
!
USE MODI_PACK_GRID_LONLAT_ROT
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
INTEGER, DIMENSION(:), INTENT(IN) :: KMASK      ! mask used
 CHARACTER(LEN=10),     INTENT(IN) :: HGRID1     ! input grid type
 CHARACTER(LEN=10),     INTENT(OUT):: HGRID2     ! output grid type
REAL,    DIMENSION(:), POINTER    :: PGRID_PAR1 ! parameters of input grid
REAL,    DIMENSION(:), POINTER    :: PGRID_PAR2 ! parameters of output packed grid
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
INTEGER :: KGRID_PAR2 ! size of packed grid vector
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!----------------------------------------------------------------------------
!
!*    1.     Chooses grid type
!            -----------------
!
IF (LHOOK) CALL DR_HOOK('PACK_GRID',0,ZHOOK_HANDLE)
HGRID2 = HGRID1
!
!
!*    2.     Computes grid parameters
!            ------------------------
!
SELECT CASE (HGRID1)
!     
  CASE("CONF PROJ ","LONLAT REG","CARTESIAN","GAUSS     ","IGN       ","LONLATVAL ","LONLAT ROT")
    !
    !
    KGRID_PAR2 = 0
    ALLOCATE(PGRID_PAR2(0))
    IF (HGRID1=="CONF PROJ ") &
      CALL PACK_GRID_CONF_PROJ(SIZE(KMASK),KMASK,SIZE(PGRID_PAR1),PGRID_PAR1,KGRID_PAR2,.FALSE.,PGRID_PAR2)  
    IF (HGRID1=="CARTESIAN ") &
      CALL PACK_GRID_CARTESIAN(SIZE(KMASK),KMASK,SIZE(PGRID_PAR1),PGRID_PAR1,KGRID_PAR2,.FALSE.,PGRID_PAR2)  
    IF (HGRID1=="LONLAT REG") &
      CALL PACK_GRID_LONLAT_REG(SIZE(KMASK),KMASK,SIZE(PGRID_PAR1),PGRID_PAR1,KGRID_PAR2,.FALSE.,PGRID_PAR2)  
    IF (HGRID1=="GAUSS     ") &
      CALL PACK_GRID_GAUSS(SIZE(KMASK),KMASK,SIZE(PGRID_PAR1),PGRID_PAR1,KGRID_PAR2,.FALSE.,PGRID_PAR2)  
    IF (HGRID1=="IGN       ") &
      CALL PACK_GRID_IGN(SIZE(KMASK),KMASK,SIZE(PGRID_PAR1),PGRID_PAR1,KGRID_PAR2,.FALSE.,PGRID_PAR2)  
    IF (HGRID1=="LONLATVAL ") &
      CALL PACK_GRID_LONLATVAL(SIZE(KMASK),KMASK,SIZE(PGRID_PAR1),PGRID_PAR1,KGRID_PAR2,.FALSE.,PGRID_PAR2)  
    IF (HGRID1=="LONLAT ROT") &
      CALL PACK_GRID_LONLAT_ROT(SIZE(KMASK),KMASK,SIZE(PGRID_PAR1),PGRID_PAR1,KGRID_PAR2,.FALSE.,PGRID_PAR2)  
    
    DEALLOCATE(PGRID_PAR2)
    !
    ALLOCATE(PGRID_PAR2(KGRID_PAR2))
    IF (HGRID1=="CONF PROJ ") &
      CALL PACK_GRID_CONF_PROJ(SIZE(KMASK),KMASK,SIZE(PGRID_PAR1),PGRID_PAR1,KGRID_PAR2,.TRUE.,PGRID_PAR2)  
    IF (HGRID1=="CARTESIAN ") &
      CALL PACK_GRID_CARTESIAN(SIZE(KMASK),KMASK,SIZE(PGRID_PAR1),PGRID_PAR1,KGRID_PAR2,.TRUE.,PGRID_PAR2)  
    IF (HGRID1=="LONLAT REG") &
      CALL PACK_GRID_LONLAT_REG(SIZE(KMASK),KMASK,SIZE(PGRID_PAR1),PGRID_PAR1,KGRID_PAR2,.TRUE.,PGRID_PAR2)  
    IF (HGRID1=="GAUSS     ") &
      CALL PACK_GRID_GAUSS(SIZE(KMASK),KMASK,SIZE(PGRID_PAR1),PGRID_PAR1,KGRID_PAR2,.TRUE.,PGRID_PAR2)  
    IF (HGRID1=="IGN       ") &
      CALL PACK_GRID_IGN(SIZE(KMASK),KMASK,SIZE(PGRID_PAR1),PGRID_PAR1,KGRID_PAR2,.TRUE.,PGRID_PAR2)  
    IF (HGRID1=="LONLATVAL ") &
      CALL PACK_GRID_LONLATVAL(SIZE(KMASK),KMASK,SIZE(PGRID_PAR1),PGRID_PAR1,KGRID_PAR2,.TRUE.,PGRID_PAR2)  
    IF (HGRID1=="LONLAT ROT") &
      CALL PACK_GRID_LONLAT_ROT(SIZE(KMASK),KMASK,SIZE(PGRID_PAR1),PGRID_PAR1,KGRID_PAR2,.TRUE.,PGRID_PAR2)  
    !
  CASE DEFAULT
    CALL ABOR1_SFX('PACK_GRID: GRID TYPE NOT SUPPORTED '//HGRID1)

END SELECT
IF (LHOOK) CALL DR_HOOK('PACK_GRID',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PACK_GRID
