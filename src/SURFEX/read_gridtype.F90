!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_GRIDTYPE (&
                                HPROGRAM,HGRID,KGRID_PAR,KLU,OREAD,PGRID_PAR,KRESP,HDIR)
!     #########################################
!
!!****  *READ_GRID* - routine to initialise the horizontal grid of a scheme
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
!!      Original    01/2003 
!!                  10/2007  E. Martin  IGN Grids
!!                  12/2012  P. Samuelsson  Rotated lonlat
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_READ_GRIDTYPE_CARTESIAN
!
USE MODI_READ_GRIDTYPE_CONF_PROJ
!
USE MODI_READ_GRIDTYPE_GAUSS
!
USE MODI_READ_GRIDTYPE_IGN
!
USE MODI_READ_GRIDTYPE_LONLAT_REG
!
USE MODI_READ_GRIDTYPE_LONLATVAL
!
USE MODI_READ_GRIDTYPE_LONLAT_ROT
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
!
 CHARACTER(LEN=6),             INTENT(IN)    :: HPROGRAM   ! calling program
 CHARACTER(LEN=10),            INTENT(IN)    :: HGRID      ! type of horizontal grid
INTEGER,                      INTENT(INOUT) :: KGRID_PAR  ! size of PGRID_PAR
INTEGER,                      INTENT(IN)    :: KLU        ! number of points
LOGICAL,                      INTENT(IN)    :: OREAD      ! flag to read the grid
REAL, DIMENSION(:), OPTIONAL, INTENT(OUT)   :: PGRID_PAR  ! parameters defining this grid
INTEGER,            OPTIONAL, INTENT(OUT)   :: KRESP      ! error return code
 CHARACTER(LEN=1),   OPTIONAL, INTENT(IN)    :: HDIR       ! reading directive ('A': all field;'H': this proc)
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(:), ALLOCATABLE :: ZGRID_PAR
INTEGER                         :: IGRID_PAR
INTEGER                         :: IRESP
 CHARACTER(LEN=1)                :: YDIR
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!---------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_GRIDTYPE',0,ZHOOK_HANDLE)
IF (OREAD) THEN
  IGRID_PAR = KGRID_PAR
ELSE
  IGRID_PAR = 0
END IF
!
IF (PRESENT(HDIR)) THEN
  YDIR = HDIR
ELSE
  YDIR = 'H'
END IF
!
ALLOCATE(ZGRID_PAR(IGRID_PAR))
!
SELECT CASE (HGRID)
  CASE("NONE      ")
    DEALLOCATE(ZGRID_PAR)
    IF (LHOOK) CALL DR_HOOK('READ_GRIDTYPE',1,ZHOOK_HANDLE)
    RETURN

  CASE ("CONF PROJ ")
    CALL READ_GRIDTYPE_CONF_PROJ(&
                                 HPROGRAM,KGRID_PAR,KLU,OREAD,IGRID_PAR,ZGRID_PAR,IRESP,YDIR)

  CASE ("CARTESIAN ")
    CALL READ_GRIDTYPE_CARTESIAN(&
                                 HPROGRAM,KGRID_PAR,KLU,OREAD,IGRID_PAR,ZGRID_PAR,IRESP,YDIR)

  CASE ("LONLAT REG")
    CALL READ_GRIDTYPE_LONLAT_REG(&
                                  HPROGRAM,KGRID_PAR,KLU,OREAD,IGRID_PAR,ZGRID_PAR,IRESP,YDIR)

  CASE ("GAUSS     ")
    CALL READ_GRIDTYPE_GAUSS(&
                             HPROGRAM,KGRID_PAR,KLU,OREAD,IGRID_PAR,ZGRID_PAR,IRESP,YDIR)

  CASE ("IGN       ")
    CALL READ_GRIDTYPE_IGN(&
                           HPROGRAM,KGRID_PAR,KLU,OREAD,IGRID_PAR,ZGRID_PAR,IRESP,YDIR)

  CASE ("LONLATVAL ")
    CALL READ_GRIDTYPE_LONLATVAL(&
                                 HPROGRAM,KGRID_PAR,KLU,OREAD,IGRID_PAR,ZGRID_PAR,IRESP,YDIR)

  CASE ("LONLAT ROT")
    CALL READ_GRIDTYPE_LONLAT_ROT(&
                                  HPROGRAM,KGRID_PAR,KLU,OREAD,IGRID_PAR,ZGRID_PAR,IRESP,YDIR)

END SELECT
!
IF (OREAD) PGRID_PAR = ZGRID_PAR
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
DEALLOCATE(ZGRID_PAR)
IF (LHOOK) CALL DR_HOOK('READ_GRIDTYPE',1,ZHOOK_HANDLE)
!---------------------------------------------------------------------------
!
END SUBROUTINE READ_GRIDTYPE
