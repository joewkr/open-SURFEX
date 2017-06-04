!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###############################################
      SUBROUTINE GET_GRID_COORD_LONLAT_ROT(KGRID_PAR,KL,PGRID_PAR,PX,PY)
!     ###############################################
!
!!****  *GET_GRID_COORD_LONLAT_ROT* - computes X and Y of ALL points
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
!!      P. Samuelsson  SMHI
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    12/2012 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODE_GRIDTYPE_LONLAT_ROT
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
REAL, DIMENSION(KL),        INTENT(OUT) :: PX         ! (deg)
REAL, DIMENSION(KL),        INTENT(OUT) :: PY         ! (deg)
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!---------------------------------------------------------------------------
!
!*       1.    Grid parameters
!              ---------------
!
IF (LHOOK) CALL DR_HOOK('GET_GRID_COORD_LONLAT_ROT',0,ZHOOK_HANDLE)
 CALL GET_GRIDTYPE_LONLAT_ROT(PGRID_PAR,PLON=PX,PLAT=PY)
IF (LHOOK) CALL DR_HOOK('GET_GRID_COORD_LONLAT_ROT',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------------
!
END SUBROUTINE GET_GRID_COORD_LONLAT_ROT
