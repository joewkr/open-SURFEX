!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_MESH_DIM_IGN(KGRID_PAR,KL,PGRID_PAR,PDX,PDY)
!     ##############################################################
!
!!**** *GET_MESH_DIM_IGN* get the grid mesh dimensions
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
!!    E. Martin         Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    10/2007
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODE_GRIDTYPE_IGN
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
INTEGER,                         INTENT(IN)    :: KGRID_PAR ! size of PGRID_PAR
INTEGER,                         INTENT(IN)    :: KL        ! number of points
REAL,    DIMENSION(KGRID_PAR),   INTENT(IN)    :: PGRID_PAR ! grid parameters
REAL,    DIMENSION(KL),          INTENT(OUT)   :: PDX       ! dimension in x dir. (meters)
REAL,    DIMENSION(KL),          INTENT(OUT)   :: PDY       ! dimension in y dir. (meters)
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
INTEGER             :: ILAMBERT ! Lambert type
REAL, DIMENSION(KL) :: ZX       ! X Lambert   coordinate
REAL, DIMENSION(KL) :: ZY       ! Y Lambert   coordinate
REAL, DIMENSION(KL) :: ZLAT     ! latitude
REAL, DIMENSION(KL) :: ZLON     ! longitude
REAL, DIMENSION(KL) :: ZMAP     ! map factor
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_MESH_DIM_IGN',0,ZHOOK_HANDLE)
 CALL GET_GRIDTYPE_IGN(PGRID_PAR,KLAMBERT=ILAMBERT, &
                              PX=ZX,PY=ZY,PDX=PDX,PDY=PDY      )  
!
!---------------------------------------------------------------------------
!
!*       2.    Computation of latitude and longitude
!              -------------------------------------
!
 CALL LATLON_IGN(ILAMBERT,ZX,ZY,ZLAT,ZLON)
!
!-----------------------------------------------------------------------------
!
!*       3.    Compute grif size
!              -----------------
!
 CALL MAP_FACTOR_IGN(ILAMBERT,ZX,ZY,ZMAP)
!
PDX(:) = PDX(:) / ZMAP(:)
PDY(:) = PDY(:) / ZMAP(:)
IF (LHOOK) CALL DR_HOOK('GET_MESH_DIM_IGN',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_MESH_DIM_IGN
