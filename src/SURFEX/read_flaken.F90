!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_FLAKE_n (DTCO, U, F, HPROGRAM)
!     #########################################
!
!!****  *READ_FLAKE_n* - reads FLAKE variables
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
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_FLAKE_n, ONLY : FLAKE_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODI_READ_SURF
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
TYPE(FLAKE_t), INTENT(INOUT) :: F
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
INTEGER           :: ILU          ! 1D physical dimension
!
INTEGER           :: IRESP          ! Error code after redding
!
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!* 1D physical dimension
!
IF (LHOOK) CALL DR_HOOK('READ_FLAKE_N',0,ZHOOK_HANDLE)
YRECFM='SIZE_WATER'
 CALL GET_TYPE_DIM_n(DTCO, U, 'WATER ',ILU)
!
!*       3.     Prognostic fields:
!               -----------------
!
!* water temperature
!
ALLOCATE(F%XTS(ILU))
!
ALLOCATE(F%XT_SNOW (ILU))
ALLOCATE(F%XT_ICE  (ILU))
ALLOCATE(F%XT_MNW  (ILU))
ALLOCATE(F%XT_WML  (ILU))
ALLOCATE(F%XT_BOT  (ILU))
ALLOCATE(F%XT_B1   (ILU))
ALLOCATE(F%XCT     (ILU))
ALLOCATE(F%XH_SNOW (ILU))
ALLOCATE(F%XH_ICE  (ILU))
ALLOCATE(F%XH_ML   (ILU))
ALLOCATE(F%XH_B1   (ILU))

YRECFM='TS_WATER'
  CALL READ_SURF(HPROGRAM,YRECFM,F%XTS(:),IRESP)
YRECFM='T_SNOW'
  CALL READ_SURF(HPROGRAM,YRECFM,F%XT_SNOW(:),IRESP)
YRECFM='T_ICE'
  CALL READ_SURF(HPROGRAM,YRECFM,F%XT_ICE(:),IRESP)
YRECFM='T_MNW'
  CALL READ_SURF(HPROGRAM,YRECFM,F%XT_MNW(:),IRESP)
YRECFM='T_WML'
  CALL READ_SURF(HPROGRAM,YRECFM,F%XT_WML(:),IRESP)
YRECFM='T_BOT'
  CALL READ_SURF(HPROGRAM,YRECFM,F%XT_BOT(:),IRESP)
YRECFM='T_B1'
  CALL READ_SURF(HPROGRAM,YRECFM,F%XT_B1(:),IRESP)
YRECFM='CT'
  CALL READ_SURF(HPROGRAM,YRECFM,F%XCT(:),IRESP)
YRECFM='H_SNOW'
  CALL READ_SURF(HPROGRAM,YRECFM,F%XH_SNOW(:),IRESP)
YRECFM='H_ICE'
  CALL READ_SURF(HPROGRAM,YRECFM,F%XH_ICE(:),IRESP)
YRECFM='H_ML'
  CALL READ_SURF(HPROGRAM,YRECFM,F%XH_ML(:),IRESP)
YRECFM='H_B1'
  CALL READ_SURF(HPROGRAM,YRECFM,F%XH_B1(:),IRESP)
!
!-------------------------------------------------------------------------------
!
!*       4.     Semi-prognostic fields:
!               ----------------------
!
!* roughness length
!
 ALLOCATE(F%XZ0(ILU))
 YRECFM='Z0WATER'
 F%XZ0(:) = 0.001
 CALL READ_SURF(HPROGRAM,YRECFM,F%XZ0(:),IRESP)
!
!
!* friction velocity
!
 ALLOCATE(F%XUSTAR(ILU))
 YRECFM='USTAR_WATER'
 F%XUSTAR(:) = 0.
 CALL READ_SURF(HPROGRAM,YRECFM,F%XUSTAR(:),IRESP)
IF (LHOOK) CALL DR_HOOK('READ_FLAKE_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------

!
END SUBROUTINE READ_FLAKE_n
