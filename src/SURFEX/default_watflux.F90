!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE DEFAULT_WATFLUX(PTSTEP,POUT_TSTEP,HWAT_ALB,HINTERPOL_TS)
!     ########################################################################
!
!!****  *DEFAULT_WATFLUX* - routine to set default values for the configuration for WATFLUX scheme
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
REAL,              INTENT(OUT) :: PTSTEP     ! time step for run
REAL,              INTENT(OUT) :: POUT_TSTEP ! time step for writing
 CHARACTER(LEN=4),  INTENT(OUT) :: HWAT_ALB   ! type of sea albedo
 CHARACTER(LEN=6),  INTENT(OUT) :: HINTERPOL_TS ! Quadratic interpolation of monthly TS
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_WATFLUX',0,ZHOOK_HANDLE)
PTSTEP       = XUNDEF
POUT_TSTEP   = XUNDEF
HWAT_ALB     = "UNIF"
HINTERPOL_TS = "NONE"
IF (LHOOK) CALL DR_HOOK('DEFAULT_WATFLUX',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DEFAULT_WATFLUX
