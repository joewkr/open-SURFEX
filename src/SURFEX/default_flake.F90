!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE DEFAULT_FLAKE(PTSTEP,POUT_TSTEP,OSEDIMENTS,HSNOW_FLK, &
        HFLK_FLUX,HFLK_ALB,OSKINTEMP)  
!     ########################################################################
!
!!****  *DEFAULT_FLAKE* - routine to set default values for the configuration for FLAKE scheme
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
REAL,             INTENT(OUT) :: PTSTEP     ! time step for run
REAL,             INTENT(OUT) :: POUT_TSTEP ! time step for writing
!
LOGICAL,          INTENT(OUT) :: OSEDIMENTS 
LOGICAL,          INTENT(OUT) :: OSKINTEMP 
CHARACTER(LEN=3), INTENT(OUT) :: HSNOW_FLK
CHARACTER(LEN=5), INTENT(OUT) :: HFLK_FLUX
CHARACTER(LEN=4), INTENT(OUT) :: HFLK_ALB
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_FLAKE',0,ZHOOK_HANDLE)
PTSTEP     = XUNDEF
POUT_TSTEP = XUNDEF
!
OSEDIMENTS  = .FALSE.
OSKINTEMP   = .FALSE.
HSNOW_FLK   = 'DEF'
HFLK_FLUX   = 'DEF  '
HFLK_ALB    = 'UNIF'
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_FLAKE',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE DEFAULT_FLAKE
