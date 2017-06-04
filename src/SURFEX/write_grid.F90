!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_GRID (HSELECT,HPROGRAM,HGRID,PGRID_PAR,PLAT,PLON,PMESH_SIZE,KRESP,HDIR)
!     #########################################
!
!!****  *WRITE_GRID* - routine to write the horizontal grid of a scheme
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004 
!!      P. Samuelsson SMHI  12/2012  Rotated lonlat
!!      S. Senesi    08/15  Adapt to XIOS (non-sensical in that case => return immediately)
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODI_WRITE_SURF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_WRITE_GRIDTYPE_CARTESIAN
!
USE MODI_WRITE_GRIDTYPE_CONF_PROJ
!
USE MODI_WRITE_GRIDTYPE_GAUSS
!
USE MODI_WRITE_GRIDTYPE_IGN
!
USE MODI_WRITE_GRIDTYPE_LONLAT_REG
!
USE MODI_WRITE_GRIDTYPE_LONLATVAL
!
USE MODI_WRITE_GRIDTYPE_LONLAT_ROT
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: HSELECT
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM   ! calling program
 CHARACTER(LEN=10),  INTENT(IN)  :: HGRID      ! type of horizontal grid
REAL, DIMENSION(:), POINTER     :: PGRID_PAR  ! parameters defining this grid
REAL, DIMENSION(:), INTENT(IN)  :: PLAT       ! latitude  (degrees)
REAL, DIMENSION(:), INTENT(IN)  :: PLON       ! longitude (degrees)
REAL, DIMENSION(:), INTENT(IN)  :: PMESH_SIZE ! horizontal mesh size (m2)
INTEGER,            INTENT(OUT) :: KRESP      ! error return code
 CHARACTER(LEN=1),    INTENT(IN), OPTIONAL :: HDIR ! type of field :
                                                  ! 'H' : field with
                                                  !       horizontal spatial dim.
                                                  ! 'A' : (complete) field with
                                                  !       horizontal spatial dim.
                                                  ! '-' : no horizontal dim.
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
 CHARACTER(LEN=100) :: YCOMMENT
 CHARACTER(LEN=1) :: YDIR
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!---------------------------------------------------------------------------
!
!*       1.    Write type of grid
!              ------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_GRID',0,ZHOOK_HANDLE)
!
IF (TRIM(HPROGRAM) == 'XIOS') THEN 
  IF (LHOOK) CALL DR_HOOK('WRITE_GRID',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
YCOMMENT='GRID TYPE'
 CALL WRITE_SURF(HSELECT, HPROGRAM,'GRID_TYPE',HGRID,KRESP,YCOMMENT)
!
!---------------------------------------------------------------------------
!
!*       2.    Write parameters of the grid
!              ----------------------------
!
YDIR='H'
IF (PRESENT(HDIR)) YDIR = HDIR
!
SELECT CASE (HGRID)
  CASE("CONF PROJ ")
    CALL WRITE_GRIDTYPE_CONF_PROJ(HSELECT, HPROGRAM,SIZE(PLAT),SIZE(PGRID_PAR),PGRID_PAR(:),KRESP,YDIR)
  CASE("CARTESIAN ")
    CALL WRITE_GRIDTYPE_CARTESIAN(HSELECT, HPROGRAM,SIZE(PLAT),SIZE(PGRID_PAR),PGRID_PAR(:),KRESP,YDIR)
  CASE("LONLAT REG")
    CALL WRITE_GRIDTYPE_LONLAT_REG(HSELECT, HPROGRAM,SIZE(PLAT),SIZE(PGRID_PAR),PGRID_PAR(:),KRESP)
  CASE("GAUSS     ")
    CALL WRITE_GRIDTYPE_GAUSS(HSELECT, HPROGRAM,SIZE(PLAT),SIZE(PGRID_PAR),PGRID_PAR(:),KRESP)
  CASE("IGN       ")
    CALL WRITE_GRIDTYPE_IGN(HSELECT, HPROGRAM,SIZE(PLAT),SIZE(PGRID_PAR),PGRID_PAR(:),KRESP)
  CASE("LONLATVAL ")
    CALL WRITE_GRIDTYPE_LONLATVAL(HSELECT, HPROGRAM,SIZE(PLAT),SIZE(PGRID_PAR),PGRID_PAR(:),KRESP)
  CASE("LONLAT ROT")
    CALL WRITE_GRIDTYPE_LONLAT_ROT(HSELECT, HPROGRAM,SIZE(PLAT),SIZE(PGRID_PAR),PGRID_PAR(:),KRESP)
  CASE("NONE      ")
    YCOMMENT='LON (DEGREES)'
    CALL WRITE_SURF(HSELECT, HPROGRAM,'LON',      PLON,KRESP,YCOMMENT)
    IF (KRESP/=0 .AND. LHOOK) CALL DR_HOOK('WRITE_GRID',1,ZHOOK_HANDLE)
    IF (KRESP/=0) RETURN
    YCOMMENT='LAT (DEGREES)'
    CALL WRITE_SURF(HSELECT,  HPROGRAM,'LAT',      PLAT,KRESP,YCOMMENT)
    IF (KRESP/=0 .AND. LHOOK) CALL DR_HOOK('WRITE_GRID',1,ZHOOK_HANDLE)
    IF (KRESP/=0) RETURN
    YCOMMENT='MESH SIZE (M2)'
    CALL WRITE_SURF(HSELECT, HPROGRAM,'MESH_SIZE',PMESH_SIZE,KRESP,YCOMMENT)
    IF (KRESP/=0 .AND. LHOOK) CALL DR_HOOK('WRITE_GRID',1,ZHOOK_HANDLE)
    IF (KRESP/=0) RETURN
END SELECT
IF (LHOOK) CALL DR_HOOK('WRITE_GRID',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------------
!
END SUBROUTINE WRITE_GRID
