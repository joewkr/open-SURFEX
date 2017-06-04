!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE CARBON_SOIL (PTSTEP, PSAND,                                      &
                          PSOILCARBON_INPUT, PCONTROL_TEMP, PCONTROL_MOIST, &
                          PSOILCARB, PRESP_HETERO_SOIL)  

!   ###############################################################
!!**  CARBON_SOIL 
!!
!!    PURPOSE
!!    -------
!!    Calculates soil carbon pools evolution.
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    none
!!
!!    REFERENCE
!!    ---------
!!
!!      Parton et al., Biogeochemestry, 1988
!!      Krinner et al., Global Biochemical Cycles, 2005
!!      Gibelin et al. 2008, AFM
!!      
!!    AUTHOR
!!    ------
!!
!!      A.-L. Gibelin           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    23/06/09
!!      B. Decharme 05/2012 : Optimization
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CO2V_PAR,       ONLY : XTAU_SOILCARB
USE MODD_CSTS,           ONLY : XDAY
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE


!*       0.1 input

! time step in s
REAL, INTENT(IN)                                                  :: PTSTEP
! sand fraction (between 0 and 1)
REAL, DIMENSION(:), INTENT(IN)                                    :: PSAND
! quantity of carbon going into carbon pools from litter decomposition
!   (gC/m**2/day)
REAL, DIMENSION(:,:), INTENT(IN)                                  :: PSOILCARBON_INPUT
! temperature control of heterotrophic respiration
REAL, DIMENSION(:,:), INTENT(IN)                                  :: PCONTROL_TEMP
! moisture control of heterotrophic respiration
REAL, DIMENSION(:,:), INTENT(IN)                                  :: PCONTROL_MOIST

!*       0.2 modified fields

! carbon pool: active, slow, or passive (gC/m**2)
REAL, DIMENSION(:,:), INTENT(INOUT)                               :: PSOILCARB

!*       0.3 output

! soil heterotrophic respiration (in gC/day/m**2)
REAL, DIMENSION(:), INTENT(OUT)                                   :: PRESP_HETERO_SOIL

!*       0.4 local

! time step in days
REAL                                                              :: ZDT
! flux fractions within carbon pools
REAL, DIMENSION(SIZE(PSOILCARB,1),SIZE(PSOILCARB,2),SIZE(PSOILCARB,2)) :: ZFRAC_CARB
! fraction of carbon flux which goes into heterotrophic respiration
REAL, DIMENSION(SIZE(PSOILCARB,1),SIZE(PSOILCARB,2))                   :: ZFRAC_RESP
! total flux out of carbon pools (gC/m**2)
REAL, DIMENSION(SIZE(PSOILCARB,1),SIZE(PSOILCARB,2))                   :: ZFLUXTOT
! fluxes between carbon pools (gC/m**2)
REAL, DIMENSION(SIZE(PSOILCARB,1),SIZE(PSOILCARB,2),SIZE(PSOILCARB,2)) :: ZFLUX
!
REAL, DIMENSION(SIZE(PSOILCARB,1))                                     :: ZWORK ! Work array
!
! dimensions
INTEGER                                                           :: INI, INSOILCARB
! indices
INTEGER                                                           :: JI, JL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! correspondence between array indices and litter levels
! LT_ABOVE = 1
! LT_BELOW = 2
! correspondence between array indices and soil carbon pools
! SL_ACTIVE = 1
! SL_SLOW = 2
! SL_PASSIVE = 3
!-------------------------------------------------------------------------------

!
!*       1 Initialisations
!
!
!*       1.1 dimensions
!
IF (LHOOK) CALL DR_HOOK('CARBON_SOIL',0,ZHOOK_HANDLE)
!
INI        = SIZE(PSOILCARB,1)
INSOILCARB = SIZE(PSOILCARB,2)
!
!*       1.2 get soil "constants"
!
!*       1.2.1 flux fractions between carbon pools: depend on soil texture, recalculated each time
!
!*       1.2.1.1 from active pool: depends on soil texture
!
ZFRAC_CARB(:,1,1) = 0.0
ZFRAC_CARB(:,1,3) = 0.004
ZFRAC_CARB(:,1,2) = 1. - ( .85 - .68 * (1.-PSAND(:)) ) - ZFRAC_CARB(:,1,3)
!
!*       1.2.1.2 from slow pool
!
ZFRAC_CARB(:,2,2) = .0
ZFRAC_CARB(:,2,1) = .42
ZFRAC_CARB(:,2,3) = .03
!
!*       1.2.1.3 from passive pool
!
ZFRAC_CARB(:,3,3) = .0
ZFRAC_CARB(:,3,1) = .45
ZFRAC_CARB(:,3,2) = .0
!
!*       1.3 set output to zero
!
PRESP_HETERO_SOIL(:) = 0.0
!
!
!*       2 input into carbon pools
!
ZDT = PTSTEP/XDAY
!
PSOILCARB(:,:) = PSOILCARB(:,:) + PSOILCARBON_INPUT(:,:) * ZDT
!
!
!*       3 fluxes within carbon reservoirs + respiration
!
!*       3.1 determine fraction of flux that is respiration
!     diagonal elements of frac_carb are zero
!
ZFRAC_RESP(:,:) = 1. - ZFRAC_CARB(:,:,1) - ZFRAC_CARB(:,:,2) - ZFRAC_CARB(:,:,3)   
!
!*       3.2 calculate fluxes
!
!*       3.2.1 flux out of pools
!
!soil property dependance (1.0-0.75*(1.0-PSAND(:)))
ZWORK(:)=0.25+0.75*PSAND(:)
!
! determine total flux out of pool
ZFLUXTOT(:,1) = PTSTEP/XTAU_SOILCARB(1)*PSOILCARB(:,1)*PCONTROL_MOIST(:,2)*PCONTROL_TEMP(:,2)*ZWORK(:) 
ZFLUXTOT(:,2) = PTSTEP/XTAU_SOILCARB(2)*PSOILCARB(:,2)*PCONTROL_MOIST(:,2)*PCONTROL_TEMP(:,2) 
ZFLUXTOT(:,3) = PTSTEP/XTAU_SOILCARB(3)*PSOILCARB(:,3)*PCONTROL_MOIST(:,2)*PCONTROL_TEMP(:,2) 
!
!decrease this carbon pool
PSOILCARB(:,:) = PSOILCARB(:,:) - ZFLUXTOT(:,:)
!
!fluxes towards the other pools (k -> kk)
DO JL=1,INSOILCARB
   DO JI=1,INI
      ZFLUX(JI,1,JL) = ZFRAC_CARB(JI,1,JL) * ZFLUXTOT(JI,1)
      ZFLUX(JI,2,JL) = ZFRAC_CARB(JI,2,JL) * ZFLUXTOT(JI,2)
      ZFLUX(JI,3,JL) = ZFRAC_CARB(JI,3,JL) * ZFLUXTOT(JI,3)
   ENDDO
ENDDO
!
!*       3.2.2 respiration
!
PRESP_HETERO_SOIL(:) = ( ZFRAC_RESP(:,1) * ZFLUXTOT(:,1) +         &
                         ZFRAC_RESP(:,2) * ZFLUXTOT(:,2) +         &
                         ZFRAC_RESP(:,3) * ZFLUXTOT(:,3)  ) / ZDT  
!
!*       3.2.3 add fluxes to active, slow, and passive pools
!
PSOILCARB(:,1) = PSOILCARB(:,1) + ZFLUX(:,1,1) + ZFLUX(:,2,1) + ZFLUX(:,3,1)  
PSOILCARB(:,2) = PSOILCARB(:,2) + ZFLUX(:,1,2) + ZFLUX(:,2,2) + ZFLUX(:,3,2)  
PSOILCARB(:,3) = PSOILCARB(:,3) + ZFLUX(:,1,3) + ZFLUX(:,2,3) + ZFLUX(:,3,3)  
!
IF (LHOOK) CALL DR_HOOK('CARBON_SOIL',1,ZHOOK_HANDLE)
!
END SUBROUTINE CARBON_SOIL
