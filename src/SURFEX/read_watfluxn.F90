!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_WATFLUX_n (DTCO, U, W, HPROGRAM)
!     #########################################
!
!!****  *READ_WATFLUX_n* - reads WATFLUX variables
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
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_WATFLUX_n, ONLY : WATFLUX_t
!
USE MODI_READ_SURF
USE MODI_INTERPOL_TS_WATER_MTH
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
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(WATFLUX_t), INTENT(INOUT) :: W
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: JMTH, INMTH
 CHARACTER(LEN=2 ) :: YMTH
!
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
IF (LHOOK) CALL DR_HOOK('READ_WATFLUX_N',0,ZHOOK_HANDLE)
YRECFM='SIZE_WATER'
 CALL GET_TYPE_DIM_n(DTCO, U, 'WATER ',ILU)
!
!*       3.     Prognostic fields:
!               -----------------
!
!* water temperature
!
ALLOCATE(W%XTS(ILU))
!
IF(W%LINTERPOL_TS)THEN
!
!  Initialize current Month
   W%TZTIME%TDATE%YEAR  = W%TTIME%TDATE%YEAR
   W%TZTIME%TDATE%MONTH = W%TTIME%TDATE%MONTH
   W%TZTIME%TDATE%DAY   = W%TTIME%TDATE%DAY
   W%TZTIME%TIME        = W%TTIME%TIME

! Precedent, Current, Next, and Second-next Monthly SST
  INMTH=4
!
  ALLOCATE(W%XTS_MTH(SIZE(W%XTS),INMTH))
  DO JMTH=1,INMTH
     WRITE(YMTH,'(I2)') (JMTH-1)
     YRECFM='TS_WATER'//ADJUSTL(YMTH(:LEN_TRIM(YMTH)))
     CALL READ_SURF(HPROGRAM,YRECFM,W%XTS_MTH(:,JMTH),IRESP)
  ENDDO
!
  CALL INTERPOL_TS_WATER_MTH(W)
!
ELSE
! 
  ALLOCATE(W%XTS_MTH(0,0))
!
  YRECFM='TS_WATER'
  CALL READ_SURF(HPROGRAM,YRECFM,W%XTS(:),IRESP)
!
ENDIF
!
!
!-------------------------------------------------------------------------------
!
!*       4.     Semi-prognostic fields:
!               ----------------------
!
!* roughness length
!
ALLOCATE(W%XZ0(ILU))
YRECFM='Z0WATER'
W%XZ0(:) = 0.001
  CALL READ_SURF(HPROGRAM,YRECFM,W%XZ0(:),IRESP)
IF (LHOOK) CALL DR_HOOK('READ_WATFLUX_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------

!
END SUBROUTINE READ_WATFLUX_n
