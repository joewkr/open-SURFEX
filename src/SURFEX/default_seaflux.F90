!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE DEFAULT_SEAFLUX(PTSTEP,POUT_TSTEP,HSEA_ALB,HSEA_FLUX,   &
                                   OPWG, OPRECIP, OPWEBB, KZ0, KGRVWAVES,&
                                   OPROGSST, KTIME_COUPLING,POCEAN_TSTEP,&
                                   PICHCE, HINTERPOL_SST, HINTERPOL_SSS  )  
!     ########################################################################
!
!!****  *DEFAULT_SEAFLUX* - routine to set default values for the configuration for SEAFLUX scheme
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
!!      Modified    01/2006 : sea flux parameterization.
!!      S. Belamari 03/2014 : add KZ0 (to choose PZ0SEA formulation)
!!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
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
!
REAL,              INTENT(OUT) :: PTSTEP        ! time step for run
REAL,              INTENT(OUT) :: POUT_TSTEP    ! time step for writing
CHARACTER(LEN=6),  INTENT(OUT) :: HSEA_FLUX     ! type of sea scheme
CHARACTER(LEN=4),  INTENT(OUT) :: HSEA_ALB      ! type of sea albedo
LOGICAL,           INTENT(OUT) :: OPWG          ! gustiness impact
LOGICAL,           INTENT(OUT) :: OPRECIP       ! precipitation correction
LOGICAL,           INTENT(OUT) :: OPWEBB        ! Webb correction
INTEGER,           INTENT(OUT) :: KZ0           ! PZ0SEA formulation
INTEGER,           INTENT(OUT) :: KGRVWAVES     ! Wave gravity in roughness length
LOGICAL,           INTENT(OUT) :: OPROGSST      !two-way coupling
INTEGER,           INTENT(OUT) :: KTIME_COUPLING!coupling frequency
REAL,              INTENT(OUT) :: PICHCE        !CE coef calculation for ECUME
REAL,              INTENT(OUT) :: POCEAN_TSTEP  !ocean 1D model time-step
CHARACTER(LEN=6),  INTENT(OUT) :: HINTERPOL_SST ! Quadratic interpolation of monthly SST
CHARACTER(LEN=6),  INTENT(OUT) :: HINTERPOL_SSS ! Quadratic interpolation of monthly SSS
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_SEAFLUX',0,ZHOOK_HANDLE)
!
PTSTEP     = XUNDEF
POUT_TSTEP = XUNDEF
!
HSEA_FLUX = "ECUME "
HSEA_ALB  = "TA96"
!
OPWG    = .FALSE.
OPRECIP = .FALSE. 
OPWEBB  = .FALSE.
!
KZ0 = 0
KGRVWAVES = 0
!
OPROGSST = .FALSE.
KTIME_COUPLING = 300
POCEAN_TSTEP = 300.
!
PICHCE = 0.0
!
HINTERPOL_SST = "NONE"
HINTERPOL_SSS = "NONE"
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_SEAFLUX',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DEFAULT_SEAFLUX
