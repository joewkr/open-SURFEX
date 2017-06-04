!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #################################################################
      SUBROUTINE WRITE_GRIDTYPE_LONLAT_REG (HSELECT,HPROGRAM,KLU,KGRID_PAR,PGRID_PAR,KRESP)
!     #################################################################
!
!!****  *WRITE_GRIDTYPE_LONLAT_REG* - routine to write the horizontal grid
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
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODI_WRITE_SURF
!
USE MODE_GRIDTYPE_LONLAT_REG
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: HSELECT
!
 CHARACTER(LEN=6),           INTENT(IN)  :: HPROGRAM   ! calling program
INTEGER,                    INTENT(IN)  :: KLU        ! number of points
INTEGER,                    INTENT(IN)  :: KGRID_PAR  ! size of PGRID_PAR
REAL, DIMENSION(KGRID_PAR), INTENT(IN)  :: PGRID_PAR  ! parameters defining this grid
INTEGER,                    INTENT(OUT) :: KRESP      ! error return code
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL    :: ZLONMIN ! minimum longitude (degrees)
REAL    :: ZLONMAX ! maximum longitude (degrees)
REAL    :: ZLATMIN ! minimum latitude  (degrees)
REAL    :: ZLATMAX ! maximum latitude  (degrees)
INTEGER :: ILON    ! number of points in longitude
INTEGER :: ILAT    ! number of points in latitude
INTEGER :: IL      ! number of points
REAL, DIMENSION(:), ALLOCATABLE :: ZLON ! longitude of points
REAL, DIMENSION(:), ALLOCATABLE :: ZLAT ! latitude  of points
!
 CHARACTER(LEN=100)                :: YCOMMENT ! comment written in the file
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!---------------------------------------------------------------------------
!
!*       1.    Grid parameters
!              ---------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_GRIDTYPE_LONLAT_REG',0,ZHOOK_HANDLE)
 CALL GET_GRIDTYPE_LONLAT_REG(PGRID_PAR,ZLONMIN,ZLONMAX,    &
                               ZLATMIN,ZLATMAX,ILON,ILAT,IL  )  
!
ALLOCATE(ZLON(IL))
ALLOCATE(ZLAT(IL))
 CALL GET_GRIDTYPE_LONLAT_REG(PGRID_PAR,PLON=ZLON,PLAT=ZLAT)
!
!---------------------------------------------------------------------------
!
!*       2.    Writing of the grid definition parameters
!              -----------------------------------------
!
YCOMMENT=' '
 CALL WRITE_SURF(HSELECT,HPROGRAM,'LONMIN',ZLONMIN,KRESP,YCOMMENT)
 CALL WRITE_SURF(HSELECT,HPROGRAM,'LONMAX',ZLONMAX,KRESP,YCOMMENT)
 CALL WRITE_SURF(HSELECT,HPROGRAM,'LATMIN',ZLATMIN,KRESP,YCOMMENT)
 CALL WRITE_SURF(HSELECT,HPROGRAM,'LATMAX',ZLATMAX,KRESP,YCOMMENT)
 CALL WRITE_SURF(HSELECT,HPROGRAM,'NLON',ILON,KRESP,YCOMMENT)
 CALL WRITE_SURF(HSELECT,HPROGRAM,'NLAT',ILAT,KRESP,YCOMMENT)
 CALL WRITE_SURF(HSELECT,HPROGRAM,'REG_LON',ZLON,KRESP,YCOMMENT)
 CALL WRITE_SURF(HSELECT,HPROGRAM,'REG_LAT',ZLAT,KRESP,YCOMMENT)
!---------------------------------------------------------------------------
DEALLOCATE(ZLON)
DEALLOCATE(ZLAT)
IF (LHOOK) CALL DR_HOOK('WRITE_GRIDTYPE_LONLAT_REG',1,ZHOOK_HANDLE)
!---------------------------------------------------------------------------
!
END SUBROUTINE WRITE_GRIDTYPE_LONLAT_REG
