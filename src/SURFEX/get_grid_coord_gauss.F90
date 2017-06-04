!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###############################################
      SUBROUTINE GET_GRID_COORD_GAUSS(KGRID_PAR,KL,PGRID_PAR,PLON_XY,PLAT_XY)
!     ###############################################
!
!!****  *GET_GRID_COORD_GAUSS* - gets pseudo longitude LON_XY and 
!                                pseudo-latitude LAT_XY of the grid points
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
!!
!!    MODIFICATIONS
!!    -------------
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
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
INTEGER,                    INTENT(IN)  :: KGRID_PAR  ! size of PGRID_PAR
INTEGER,                    INTENT(IN)  :: KL         ! number of points
REAL, DIMENSION(KGRID_PAR), INTENT(IN)  :: PGRID_PAR  ! parameters defining this grid
REAL, DIMENSION(KL),        INTENT(OUT) :: PLON_XY    ! pseudo-latitude  (deg)
REAL, DIMENSION(KL),        INTENT(OUT) :: PLAT_XY    ! pseudo-longitude (deg)
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL    :: ZLAPO   ! latitude  of the rotated pole (deg)
REAL    :: ZLOPO   ! longitude of the rotated pole (deg)
REAL    :: ZCODIL  ! stretching factor (must be greater than or equal to 1)
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!---------------------------------------------------------------------------
!
!*       1.    Gets latitudes and longitudes
!              -----------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_GRID_COORD_GAUSS',0,ZHOOK_HANDLE)
 CALL GET_GRIDTYPE_GAUSS(PGRID_PAR,PLAPO=ZLAPO,PLOPO=ZLOPO,PCODIL=ZCODIL, &
                          PLAT_XY=PLAT_XY,PLON_XY=PLON_XY)  
IF (LHOOK) CALL DR_HOOK('GET_GRID_COORD_GAUSS',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------------
!
END SUBROUTINE GET_GRID_COORD_GAUSS
