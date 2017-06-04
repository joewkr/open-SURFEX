!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE SPINUP_MAX(PSPINMAX,KNBYEARSPIN,KNBYEARSOLD,KSPIN)
  
!     #######################################################################
!
!
!!****  *SPINUP_MAX*  
!!
!!    PURPOSE
!!    -------
!!    Number of times the accelerated subroutine is called  
!!     
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!      
!!    REFERENCE
!!    ---------
!!      
!!    AUTHOR
!!    ------
!!      R. Alkama           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original      03/26/2012
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CO2V_PAR,  ONLY : XSPIN_CO2
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
REAL,    INTENT(IN)              :: PSPINMAX  ! max number of times the accelerated subroutine
                                              ! is called for each time step in simulation
                                              ! during the acceleration procedure

INTEGER, INTENT(IN)              :: KNBYEARSPIN ! spinup duration in years
                                                ! nbr of years needed to reach the equilibrium
INTEGER, INTENT(IN)              :: KNBYEARSOLD 
INTEGER, INTENT(OUT)             :: KSPIN        
!                                         
!
!*      0.2    declarations of local variables
!
!We assume that 10% of the spinup period is for ramping up CO2 concentration
!from XCO2_START to XCO2_END
!
REAL, PARAMETER  :: ZSPIN_MAX      = 0.6  ! spin up soil at its maximum PSPINMAX
REAL             :: ZSPIN_DECREASE        ! fraction of KNBYEARSPIN period used to
!
REAL             :: ZSLOPE
REAL             :: ZMAX
REAL             :: ZDECREASE
!
INTEGER          :: IMAX
INTEGER          :: IDECREASE
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!       1.     Initializations
!              ---------------
!
IF (LHOOK) CALL DR_HOOK('SPINUP_MAX',0,ZHOOK_HANDLE)
!
ZSPIN_DECREASE=1.0-ZSPIN_MAX-XSPIN_CO2
!
ZMAX = ZSPIN_MAX*REAL(KNBYEARSPIN)
IMAX = NINT(ZMAX)
!
ZDECREASE = ZMAX+ZSPIN_DECREASE*REAL(KNBYEARSPIN)
IDECREASE = NINT(ZDECREASE)
!
IF ( KNBYEARSOLD <= IMAX)THEN
   !
   KSPIN = NINT(PSPINMAX)
   !
ELSE IF (KNBYEARSOLD > IMAX .AND. KNBYEARSOLD <= IDECREASE)THEN
   !
   ZSLOPE  = (PSPINMAX-1.0) / (ZDECREASE - ZMAX)
   !
   KSPIN = NINT(PSPINMAX - ZSLOPE * (REAL(KNBYEARSOLD) - ZMAX))
   !
   KSPIN = MAX(KSPIN,1)
   !
ELSE
   !
   KSPIN = 1
   !   
ENDIF
!
IF (LHOOK) CALL DR_HOOK('SPINUP_MAX',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE 
