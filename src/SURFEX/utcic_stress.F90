!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!   ##########################################################################
SUBROUTINE UTCIC_STRESS(PTSTEP, PUTCI, PUTCIC )
!   ##########################################################################
!
!!****  *UTCIC_STRESS*  
!!
!!    PURPOSE
!!    -------
!
! Integrates the UTCI index according to several heat stress ranges
!         
!     
!!**  METHOD
!     ------
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!   www.utci.org
!!      
!!    AUTHOR
!!    ------
!!
!!      V. Masson           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original  10/2013
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_UTCI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
REAL,                 INTENT(IN)     :: PTSTEP ! time-step (s)
REAL, DIMENSION(:),   INTENT(IN)     :: PUTCI  ! UTCI  (C)
REAL, DIMENSION(:,:), INTENT(INOUT)  :: PUTCIC ! Time cumulated in each sheat-stress range (s)
!
!*      0.2    declarations of local variables
!
INTEGER :: JSTRESS ! Loop counter on stress ranges
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('UTCIC_STRESS',0,ZHOOK_HANDLE)
!
DO JSTRESS=1,NUTCI_STRESS
!* loop on each stress range
  !* integrates time when in the heat/cold stress range
  WHERE (PUTCI(:)>XUTCI_STRESS_LIMITS(JSTRESS-1) .AND. PUTCI(:)<=XUTCI_STRESS_LIMITS(JSTRESS))
    PUTCIC(:,JSTRESS) = PUTCIC(:,JSTRESS) + PTSTEP
  END WHERE
END DO
!
IF (LHOOK) CALL DR_HOOK('UTCIC_STRESS',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE UTCIC_STRESS
