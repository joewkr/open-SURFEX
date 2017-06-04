!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE CARBON_INIT 
!     #####################
!
!!****  *CARBON_INIT* - routine to initialize soil carbon parameters
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
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!      Gibelin et al. 2008, AFM
!!        Modelling energy and CO2 fluxes with an interactive vegetation land surface model -
!!        Evaluation at high and middle latitudes.
!!
!!    AUTHOR
!!    ------
!!      A.-L. Gibelin           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    15/06/09
!!      B. Decharme   2012    variable must be allocated once by run
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CO2V_PAR,       ONLY : XCN, XLC, XFRAC_LITTER, XTAU_LITTER,  &
                                  XFRAC_SOILCARB, XTAU_SOILCARB  
USE MODD_SURF_PAR,       ONLY : XUNDEF
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
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('CARBON_INIT',0,ZHOOK_HANDLE)
!
!*       2.     BIOMASS CONSTANTS
!               -----------------
!
! Biomass Carbon/Nitrogen ratio
XCN(1) = 40.0
XCN(2) = 40.0
XCN(3) = 40.0
XCN(4) = 40.0
XCN(5) = 40.0
XCN(6) = 40.0
!    
! Biomass Lignin/Carbon ratio
XLC(1) = 0.22
XLC(2) = 0.35
XLC(3) = 0.35
XLC(4) = 0.35
XLC(5) = 0.35
XLC(6) = 0.35
!    
! Fraction of biomass pools going into litter pools
XFRAC_LITTER(:,1) = 0.85 - 0.018 * XLC(:) * XCN(:)
XFRAC_LITTER(:,2) = 1. - XFRAC_LITTER(:,1)
!  
!
!*       3.     LITTER CONSTANTS
!               ----------------
!        
! Residence times in litter pools (s)
XTAU_LITTER(1) = 0.066*365.0*86400.0
XTAU_LITTER(2) = 0.245*365.0*86400.0
!  
! Fraction of litter decomposition flux that goes into soil.
! The rest goes into the atmosphere
XFRAC_SOILCARB(:,:,:) = XUNDEF
! 
! Structural litter: lignin fraction goes into slow pool + respiration,
!                    rest into active pool + respiration.
XFRAC_SOILCARB(2,1,1) = 0.55
XFRAC_SOILCARB(2,1,2) = 0.45
XFRAC_SOILCARB(2,2,1) = 0.70
XFRAC_SOILCARB(2,2,2) = 0.70
!  
! Metabolic litter: all goes into active pool + respiration,
!                   nothing into slow or passive pool.
XFRAC_SOILCARB(1,1,1) = 0.45
XFRAC_SOILCARB(1,1,2) = 0.45
!   
!
!*       4.     SOIL CONSTANTS
!               --------------
!  
! Residence times in carbon pools (s)
XTAU_SOILCARB(1) = 0.149*365.0*86400.0
XTAU_SOILCARB(2) = 5.480*365.0*86400.0
XTAU_SOILCARB(3) = 241.0*365.0*86400.0
!
IF (LHOOK) CALL DR_HOOK('CARBON_INIT',1,ZHOOK_HANDLE)
!
END SUBROUTINE CARBON_INIT
