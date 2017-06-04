!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#################################################################
SUBROUTINE WRITE_GRIDTYPE_GAUSS (HSELECT,HPROGRAM,KLU,KGRID_PAR,PGRID_PAR,KRESP)
!#################################################################
!
!!****  *WRITE_GRIDTYPE_GAUSS* - routine to write the horizontal grid
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
USE MODE_GRIDTYPE_GAUSS
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
INTEGER :: INLATI  ! number of pseudo-latitudes
REAL    :: ZLAPO   ! latitude  of the rotated pole (deg)
REAL    :: ZLOPO   ! longitude of the rotated pole (deg)
REAL    :: ZCODIL  ! stretching factor (must be greater than or equal to 1)
INTEGER, DIMENSION(:), ALLOCATABLE :: INLOPA ! number of pseudo-longitudes on each
                                             ! pseudo-latitude circle
REAL,    DIMENSION(KLU) :: ZLAT ! latitudes
REAL,    DIMENSION(KLU) :: ZLON ! longitudes
REAL,    DIMENSION(KLU) :: ZLAT_XY
REAL,    DIMENSION(KLU) :: ZLON_XY
REAL,    DIMENSION(KLU) :: ZMESH_SIZE
!                                                                 _____ Sup
REAL,    DIMENSION(KLU) :: ZLATSUP     ! Grid corner Latitude    |     |
REAL,    DIMENSION(KLU) :: ZLONSUP     ! Grid corner Longitude   |     |
REAL,    DIMENSION(KLU) :: ZLATINF     ! Grid corner Latitude    |_____|
REAL,    DIMENSION(KLU) :: ZLONINF     ! Grid corner Longitude  Inf
!
INTEGER                            :: IL    ! total number of points
!
 CHARACTER(LEN=100)                :: YCOMMENT ! comment written in the file
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!---------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_GRIDTYPE_GAUSS',0,ZHOOK_HANDLE)
!
!*       1.    Projection and 2D grid parameters
!              ---------------------------------
!
 CALL GET_GRIDTYPE_GAUSS(PGRID_PAR,INLATI)
!
ALLOCATE(INLOPA(INLATI))
!
 CALL GET_GRIDTYPE_GAUSS(PGRID_PAR,INLATI,ZLAPO,ZLOPO,ZCODIL,INLOPA(:),IL,&
                         ZLAT,ZLON,ZLAT_XY,ZLON_XY,ZMESH_SIZE,            &
                         ZLONINF,ZLATINF,ZLONSUP,ZLATSUP                  )  
!
!---------------------------------------------------------------------------
!
!*       2.    Writing of the grid definition parameters
!              -----------------------------------------
!
YCOMMENT=' '
 CALL WRITE_SURF(HSELECT,HPROGRAM,'NLATI',INLATI,KRESP,YCOMMENT)
 CALL WRITE_SURF(HSELECT,HPROGRAM,'LAPO',ZLAPO, KRESP,YCOMMENT)
 CALL WRITE_SURF(HSELECT,HPROGRAM,'LOPO',ZLOPO,KRESP,YCOMMENT)
 CALL WRITE_SURF(HSELECT,HPROGRAM,'CODIL',ZCODIL,KRESP,YCOMMENT)
 CALL WRITE_SURF(HSELECT,HPROGRAM,'NLOPA',INLOPA(:),KRESP,YCOMMENT,HDIR='-',HNAM_DIM='Nlati           ')
 CALL WRITE_SURF(HSELECT,HPROGRAM,'LATGAUSS',ZLAT(:),KRESP,YCOMMENT)
 CALL WRITE_SURF(HSELECT,HPROGRAM,'LONGAUSS',ZLON(:),KRESP,YCOMMENT)
 CALL WRITE_SURF(HSELECT,HPROGRAM,'LAT_G_XY',ZLAT_XY(:),KRESP,YCOMMENT)
 CALL WRITE_SURF(HSELECT,HPROGRAM,'LON_G_XY',ZLON_XY(:),KRESP,YCOMMENT)
 CALL WRITE_SURF(HSELECT,HPROGRAM,'MESHGAUSS',ZMESH_SIZE(:),KRESP,YCOMMENT)
 CALL WRITE_SURF(HSELECT,HPROGRAM,'LONINF',ZLONINF(:),KRESP,YCOMMENT)
 CALL WRITE_SURF(HSELECT,HPROGRAM,'LATINF',ZLATINF(:),KRESP,YCOMMENT)
 CALL WRITE_SURF(HSELECT,HPROGRAM,'LONSUP',ZLONSUP(:),KRESP,YCOMMENT)
 CALL WRITE_SURF(HSELECT,HPROGRAM,'LATSUP',ZLATSUP(:),KRESP,YCOMMENT)
!
DEALLOCATE(INLOPA)
!
!---------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('WRITE_GRIDTYPE_GAUSS',1,ZHOOK_HANDLE)
!---------------------------------------------------------------------------
!
END SUBROUTINE WRITE_GRIDTYPE_GAUSS
