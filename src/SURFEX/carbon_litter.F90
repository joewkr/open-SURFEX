!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE CARBON_LITTER (PTSTEP, PTURNOVER, PLITTER, PLIGNIN_STRUC,          &
                          PCONTROL_TEMP, PCONTROL_MOIST,                      &
                          PRESP_HETERO_LITTER, PSOILCARBON_INPUT)  

!   ###############################################################
!!**  CARBON_LITTER 
!!
!!    PURPOSE
!!    -------
!!    Calculates litter evolution.
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
USE MODD_CO2V_PAR,       ONLY : XLC, XTAU_LITTER, XFRAC_LITTER, XFRAC_SOILCARB
USE MODD_CSTS,           ONLY : XDAY, XTT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1 input
!
! time step in s
REAL, INTENT(IN)                                                 :: PTSTEP
!time step in s
! Turnover rates (gC/m**2/s)
REAL, DIMENSION(:,:), INTENT(IN)                                 :: PTURNOVER
! temperature control of heterotrophic respiration, above and below
REAL, DIMENSION(:,:), INTENT(IN)                                 :: PCONTROL_TEMP
! moisture control of heterotrophic respiration
REAL, DIMENSION(:,:), INTENT(IN)                                 :: PCONTROL_MOIST
!
!*       0.2 modified fields
!
! metabolic and structural litter, above and below ground (gC/m**2)
REAL, DIMENSION(:,:,:), INTENT(INOUT)                            :: PLITTER
! ratio Lignin/Carbon in structural litter, above and below ground (gC/m**2)
REAL, DIMENSION(:,:), INTENT(INOUT)                              :: PLIGNIN_STRUC
!
!*       0.3 output
!
! litter heterotrophic respiration (in gC/m**2/day)
REAL, DIMENSION(:), INTENT(OUT)                                  :: PRESP_HETERO_LITTER
! quantity of carbon going into carbon pools from litter decomposition
!   (gC/m**2/day)
REAL, DIMENSION(:,:), INTENT(OUT)                                :: PSOILCARBON_INPUT
!
!*       0.4 local
!
! time step in days
REAL                                                             :: ZDT
! fraction of structural or metabolic litter decomposed
REAL                                                             :: ZFD
! quantity of structural or metabolic litter decomposed (gC/m**2)
REAL                                                             :: ZQD
! old structural litter, above and below (gC/m**2)
REAL, DIMENSION(SIZE(PLITTER,1),SIZE(PLITTER,3))                 :: ZOLD_STRUC
! increase of metabolic and structural litter, above and below ground (gC/m**2)
REAL, DIMENSION(SIZE(PLITTER,1),SIZE(PLITTER,2),SIZE(PLITTER,3)) :: ZLITTER_INC
! lignin increase in structural litter, above and below ground (gC/m**2)
REAL, DIMENSION(SIZE(PLITTER,1),SIZE(PLITTER,3))                 :: ZLIGNIN_STRUC_INC
! dimensions
INTEGER                                                          :: INLITTER,INLITTLEVS
! indices
INTEGER                                                          :: INI,JI,JL
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! correspondence between array indices and biomass compartments
! LEAF = 1
! STRUCT_ACT = 2
! STRUCT_PAS = 3
! STRUCT_BELOW = 4
! WOOD_ABOVE = 5
! WOOD_BELOW = 6
! correspondence between array indices and litter type
! LT_METABOLIC = 1
! LT_STRUCTURAL = 2
! correspondence between array indices and litter levels
! LT_ABOVE = 1
! LT_BELOW = 2
! correspondence between array indices and soil carbon pools
! SL_ACTIVE = 1
! SL_SLOW = 2
! SL_PASSIVE = 3
!-------------------------------------------------------------------------------
!
!*    1 Initialisations
!
!
!*    1.1 dimensions
!
IF (LHOOK) CALL DR_HOOK('CARBON_LITTER',0,ZHOOK_HANDLE)
!
INI        = SIZE(PLITTER,1)
INLITTER   = SIZE(PLITTER,2)
INLITTLEVS = SIZE(PLITTER,3)
!
!*    1.2 set output to zero
!
PRESP_HETERO_LITTER(:) = 0.0
PSOILCARBON_INPUT(:,:) = 0.0
!
!*    2 Add biomass to different litterpools
!
ZDT = PTSTEP/XDAY
!
!*    2.1 first, save old structural litter (needed for lignin fractions).
!            (above/below)
!
ZOLD_STRUC(:,:) = PLITTER(:,2,:)
!
! *   2.2 update litter, and lignin content in structural litter
!
ZLITTER_INC    (:,:,:) = 0.0
ZLIGNIN_STRUC_INC(:,:) = 0.0
!
!*    2.2.1 calculate litter increase (per m**2 of ground).
!           Litter increase for structural and metabolic, above/below
!
ZLITTER_INC(:,1,1) = ( XFRAC_LITTER(1,1) * PTURNOVER(:,1) +          &
                       XFRAC_LITTER(2,1) * PTURNOVER(:,2) +          &
                       XFRAC_LITTER(3,1) * PTURNOVER(:,3) +          &
                       XFRAC_LITTER(5,1) * PTURNOVER(:,5) ) * PTSTEP 

ZLITTER_INC(:,1,2) = ( XFRAC_LITTER(4,1) * PTURNOVER(:,4) +          &
                       XFRAC_LITTER(6,1) * PTURNOVER(:,6) ) * PTSTEP  
!
ZLITTER_INC(:,2,1) = ( XFRAC_LITTER(1,2) * PTURNOVER(:,1) +          &
                       XFRAC_LITTER(2,2) * PTURNOVER(:,2) +          &
                       XFRAC_LITTER(3,2) * PTURNOVER(:,3) +          &
                       XFRAC_LITTER(5,2) * PTURNOVER(:,5) ) * PTSTEP 

ZLITTER_INC(:,2,2) = ( XFRAC_LITTER(4,2) * PTURNOVER(:,4) +          &
                       XFRAC_LITTER(6,2) * PTURNOVER(:,6) ) * PTSTEP  
!
!*    2.2.2 lignin increase in structural litter
!
ZLIGNIN_STRUC_INC(:,1) = ZLIGNIN_STRUC_INC(:,1) + ( XLC(1)*PTURNOVER(:,1) + XLC(2)*PTURNOVER(:,2) +          &
                                                    XLC(3)*PTURNOVER(:,3) + XLC(5)*PTURNOVER(:,5) ) * PTSTEP  
ZLIGNIN_STRUC_INC(:,2) = ZLIGNIN_STRUC_INC(:,2) + ( XLC(4)*PTURNOVER(:,4) + XLC(6)*PTURNOVER(:,6) ) * PTSTEP  
!
!*    2.2.3 add new litter (struct/met, above/below)
!
PLITTER(:,:,:) = PLITTER(:,:,:) + ZLITTER_INC(:,:,:)
!
!*    2.2.4 for security: can't add more lignin than structural litter
!           (above/below)
!
ZLIGNIN_STRUC_INC(:,:) = MIN( ZLIGNIN_STRUC_INC(:,:), ZLITTER_INC(:,2,:) )
!
!*    2.2.5 new lignin content: add old lignin and lignin increase, divide by 
!           total structural litter (above/below)
!
WHERE(PLITTER(:,2,:)>0.0)
      PLIGNIN_STRUC(:,:) = (PLIGNIN_STRUC(:,:)*ZOLD_STRUC(:,:)+ZLIGNIN_STRUC_INC(:,:))/PLITTER(:,2,:)
ENDWHERE
!
!*    3 fluxes from litter to carbon pools and respiration
!
DO JL=1,INLITTLEVS
   DO JI=1,INI
!
!*    3.1 structural litter: goes into active and slow carbon pools + respiration
!
!*    3.1.1 total quantity of structural litter which is decomposed
!
      ZFD=PTSTEP/XTAU_LITTER(2)*PCONTROL_TEMP(JI,JL)*PCONTROL_MOIST(JI,JL)*EXP(-3.0*PLIGNIN_STRUC(JI,JL))  
!
      ZQD=PLITTER(JI,2,JL)*ZFD
!      
      PLITTER(JI,2,JL)=PLITTER(JI,2,JL)-ZQD
!
!*    3.1.2 non-lignin fraction of structural litter goes into active carbon pool + respiration
!
      PSOILCARBON_INPUT(JI,1)=PSOILCARBON_INPUT(JI,1)+XFRAC_SOILCARB(2,1,JL)*ZQD*(1.0-PLIGNIN_STRUC(JI,JL))/ZDT  
!
      PRESP_HETERO_LITTER(JI)=PRESP_HETERO_LITTER(JI)+(1.0-XFRAC_SOILCARB(2,1,JL))*ZQD*(1.0-PLIGNIN_STRUC(JI,JL))/ZDT  
!
!*    3.1.3 lignin fraction of structural litter goes into slow carbon pool + respiration
!
      PSOILCARBON_INPUT(JI,2)=PSOILCARBON_INPUT(JI,2)+XFRAC_SOILCARB(2,2,JL)*ZQD*PLIGNIN_STRUC(JI,JL)/ZDT  
!
      PRESP_HETERO_LITTER(JI)=PRESP_HETERO_LITTER(JI)+(1.0-XFRAC_SOILCARB(2,2,JL))*ZQD*PLIGNIN_STRUC(JI,JL)/ZDT  
!
!*    3.2 metabolic litter goes into active carbon pool + respiration
!
!*    3.2.1 total quantity of metabolic litter that is decomposed
!
      ZFD = PTSTEP/XTAU_LITTER(1)*PCONTROL_TEMP(JI,JL)*PCONTROL_MOIST(JI,JL)
!
      ZQD = PLITTER(JI,1,JL)*ZFD
!
      PLITTER(JI,1,JL)=PLITTER(JI,1,JL)-ZQD
!
!*    3.2.2 put decomposed litter into carbon pool + respiration
!
      PSOILCARBON_INPUT(JI,1)=PSOILCARBON_INPUT(JI,1)+XFRAC_SOILCARB(1,1,JL)*ZQD/ZDT  
!
      PRESP_HETERO_LITTER(JI) = PRESP_HETERO_LITTER(JI)+(1.0-XFRAC_SOILCARB(1,1,JL))*ZQD/ZDT
!
   ENDDO
ENDDO
!
IF (LHOOK) CALL DR_HOOK('CARBON_LITTER',1,ZHOOK_HANDLE)

!
END SUBROUTINE CARBON_LITTER
