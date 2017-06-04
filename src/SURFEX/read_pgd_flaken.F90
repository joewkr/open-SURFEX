!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_PGD_FLAKE_n (DTCO, U, UG, FG, F, HPROGRAM)
!     #########################################
!
!!****  *READ_PGD_FLAKE_n* - read FLAKE physiographic fields
!!                        
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
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODD_FLAKE_n, ONLY : FLAKE_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_TYPE_DATE_SURF
!
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER


!
USE MODE_READ_SURF_COV, ONLY : READ_SURF_COV
!
USE MODI_READ_SURF
USE MODI_PACK_INIT
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_TYPE_DIM_n
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(GRID_t), INTENT(INOUT) :: FG
TYPE(FLAKE_t), INTENT(INOUT) :: F
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! Error code after redding
! 
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!* 1D physical dimension
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_FLAKE_N',0,ZHOOK_HANDLE)
YRECFM='SIZE_WATER'
 CALL GET_TYPE_DIM_n(DTCO, U, 'WATER ',FG%NDIM)
!
!
!*       2.     Physiographic data fields:
!               -------------------------
!
!* cover classes
!
ALLOCATE(F%LCOVER(JPCOVER))
ALLOCATE(F%XZS(FG%NDIM))
ALLOCATE(FG%XLAT       (FG%NDIM))
ALLOCATE(FG%XLON       (FG%NDIM))
!
ALLOCATE(FG%XMESH_SIZE (FG%NDIM))
CALL PACK_INIT(DTCO, U, UG,HPROGRAM,'WATER ',FG, F%LCOVER,F%XCOVER,F%XZS)
!
!* FLake parameters
!
ALLOCATE(F%XWATER_DEPTH   (FG%NDIM))
YRECFM='WATER_DEPTH'
 CALL READ_SURF(HPROGRAM,YRECFM,F%XWATER_DEPTH(:),IRESP)
!
ALLOCATE(F%XWATER_FETCH   (FG%NDIM))
YRECFM='WATER_FETCH'
 CALL READ_SURF(HPROGRAM,YRECFM,F%XWATER_FETCH(:),IRESP)
!
ALLOCATE(F%XT_BS          (FG%NDIM))
YRECFM='T_BS'
 CALL READ_SURF(HPROGRAM,YRECFM,F%XT_BS(:),IRESP)
!
ALLOCATE(F%XDEPTH_BS      (FG%NDIM))
YRECFM='DEPTH_BS'
 CALL READ_SURF(HPROGRAM,YRECFM,F%XDEPTH_BS(:),IRESP)
!
ALLOCATE(F%XEXTCOEF_WATER (FG%NDIM))
YRECFM='EXTCOEF_WAT'
 CALL READ_SURF(HPROGRAM,YRECFM,F%XEXTCOEF_WATER(:),IRESP)
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_FLAKE_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_PGD_FLAKE_n
