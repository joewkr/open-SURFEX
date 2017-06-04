!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE CARBON_EVOL(IO, KK, PK, PEK, DEK, PTSTEP, PRHOA, PRESP_BIOMASS_INST )
!   ###############################################################
!!****  *CARBON EVOL*
!!
!!    PURPOSE
!!    -------
!!
!!    Diagnoses respiration carbon fluxes and performs the time evolution of 
!!    carbon pools in the case of 'CNT' option (ISBA-CC) 
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
!!      Gibelin et al. 2008, AFM
!!        Modelling energy and CO2 fluxes with an interactive vegetation land surface model -
!!        Evaluation at high and middle latitudes.
!!      
!!    AUTHOR
!!    ------
!!
!!      A.L. Gibelin       * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    22/06/09
!!      S.QUEGUINER 09/2011 Cas 'DEF'- condition si LAI=UNDEF->ZRESP_SOIL_TOT=0
!!      C.   Delire 04/2012 : spinup soil carbon
!!      B. Decharme 05/2012 : Optimization and ISBA-DIF coupling
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_K_t, ISBA_P_t, ISBA_PE_t
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_t
!
USE MODD_CO2V_PAR,       ONLY : XMC, XMCO2, XPCCO2
USE MODD_CSTS,           ONLY : XDAY, XTT
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODI_CONTROL_MOIST_FUNC
USE MODI_CONTROL_TEMP_FUNC
USE MODI_CARBON_LITTER
USE MODI_CARBON_SOIL

!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_K_t), INTENT(INOUT) :: KK
TYPE(ISBA_P_t), INTENT(INOUT) :: PK
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: DEK
!
REAL, INTENT(IN)             :: PTSTEP                 ! time step
REAL, DIMENSION(:), INTENT(IN)        :: PRHOA         ! air density (kg/m3)
REAL, DIMENSION(:,:), INTENT(IN)      :: PRESP_BIOMASS_INST ! instantaneous respiration of biomass (kgCO2/kgair m/s)
!
!*      0.2    declarations of local variables
!  
REAL, PARAMETER                  :: ZCOEF1 = 4.4E-8
REAL, PARAMETER                  :: ZCOEF2 = 13.5
REAL, PARAMETER                  :: ZCOEF3 = 5.4
REAL, PARAMETER                  :: ZCOEF4 = 0.1
REAL, PARAMETER                  :: ZCOEF5 = 25.0
!
REAL, PARAMETER                  :: ZDTOP  = 0.1   !Top depth m
REAL, PARAMETER                  :: ZDSUB  = 1.0   !Sub depth m
!
REAL, DIMENSION(SIZE(PEK%XTG,1))     :: ZRESP_SOIL_TOT     ! total soil respiration (kgCO2/kgair m/s)
REAL, DIMENSION(SIZE(PEK%XTG,1))     :: ZRESP_AUTO_ABOVE   ! total above ground biomass respiration (kgCO2/kgair m/s)
REAL, DIMENSION(SIZE(PEK%XTG,1))     :: ZRESP_HETERO       ! total heterotrophic respiration (kgCO2/kgair m/s)
!
!
REAL, DIMENSION(SIZE(PEK%XSOILCARB,1),SIZE(PEK%XSOILCARB,2)) :: ZSOILCARBON_INPUT
!                  quantity of carbon going into carbon pools 
!                  from litter decomposition (gC/m2/day)
!
REAL, DIMENSION(SIZE(PEK%XSOILCARB,1)) :: ZRESP_HETERO_DAY_LITTER 
!                  litter heterotrophic respiration (gC/m2/day)
REAL, DIMENSION(SIZE(PEK%XSOILCARB,1)) :: ZRESP_HETERO_DAY_SOIL   
!                  soil heterotrophic respiration (gC/m2/day)
!
REAL, DIMENSION(SIZE(PEK%XLIGNIN_STRUC,1),SIZE(PEK%XLIGNIN_STRUC,2)) :: ZCONTROL_MOIST, &
                                                                ZCONTROL_TEMP
!                  ZCONTROL_MOIST = moisture control of heterotrophic respiration
!                  ZCONTROL_TEMP = temperature control of heterotrophic respiration
!
REAL, DIMENSION(SIZE(PEK%XTG,1))              :: ZTG_TOP     ! Top soil temperature   (C)
REAL, DIMENSION(SIZE(PEK%XTG,1))              :: ZTG_SUB     ! Sub soil temperature   (C)
REAL, DIMENSION(SIZE(PEK%XTG,1))              :: ZSAND_SUB   ! Sub soil sand fraction (-)
!
REAL, DIMENSION(SIZE(PEK%XTG,1))              :: ZMOIST_TOP  ! Top soil moisture index (-)
REAL, DIMENSION(SIZE(PEK%XTG,1))              :: ZMOIST_SUB  ! Sub soil moisture index (-)
REAL, DIMENSION(SIZE(PEK%XTG,1))              :: ZSAT_TOP    ! Top soil saturated index(-)
REAL, DIMENSION(SIZE(PEK%XTG,1))              :: ZSAT_SUB    ! Sub soil saturated index(-)
!
REAL, DIMENSION(SIZE(PEK%XTG,1))              :: ZDG_TOP     ! Top soil depth for DIF (m)
REAL, DIMENSION(SIZE(PEK%XTG,1))              :: ZDG_SUB     ! Sub soil depth for DIF (m)
!
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2))  :: ZWGHT_TOP   ! Weight top for DIF (m)    
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2))  :: ZWGHT_SUB   ! Weight sub for DIF (m)  
!
REAL, DIMENSION(SIZE(PEK%XTG,1))              :: ZWORK  ! work array
!
REAL     :: ZMOISTL, ZSATL, ZLOG2
!
INTEGER  :: JNBIOMASS
INTEGER  :: ITCSPIN
!
INTEGER  :: INI, INL, JI, JL, IDEKTH, INBIOMASS
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*      1.     Preliminaries
!              -------------
!
IF (LHOOK) CALL DR_HOOK('CARBON_EVOL',0,ZHOOK_HANDLE)
!
DEK%XRESP_AUTO    = 0.0
DEK%XRESP_ECO     = 0.0
!
INI=SIZE(PEK%XTG,1)
INL=SIZE(PEK%XTG,2)
INBIOMASS=SIZE(PRESP_BIOMASS_INST,2)
!
ZRESP_SOIL_TOT(:)          = XUNDEF
ZRESP_AUTO_ABOVE(:)        = XUNDEF
ZRESP_HETERO(:)            = XUNDEF
ZSOILCARBON_INPUT(:,:)     = XUNDEF
ZRESP_HETERO_DAY_LITTER(:) = XUNDEF
ZRESP_HETERO_DAY_SOIL(:)   = XUNDEF
ZCONTROL_MOIST(:,:)        = XUNDEF
ZCONTROL_TEMP(:,:)         = XUNDEF
ZWGHT_TOP(:,:)             = XUNDEF
ZWGHT_SUB(:,:)             = XUNDEF
!
ZTG_TOP   (:) = 0.0
ZTG_SUB   (:) = 0.0
!
ZLOG2  = LOG(2.0)
!
! convert soil temperature from K to C (over 1m depth for DIF)
!
IF(IO%CISBA/='DIF')THEN        
  ZTG_TOP   (:) = PEK%XTG(:,1)-XTT  
  ZTG_SUB   (:) = PEK%XTG(:,2)-XTT  
ELSE       
  DO JI=1,INI
     IDEKTH=PK%NWG_LAYER(JI)
     ZDG_TOP(JI)=MIN(ZDTOP,PK%XDG(JI,IDEKTH))
     ZDG_SUB(JI)=MIN(ZDSUB,PK%XDG(JI,IDEKTH))
  ENDDO  
  DO JL=1,INL
     DO JI=1,INI     
        ZWGHT_TOP(JI,JL)=MIN(PK%XDZG(JI,JL),MAX(0.0,ZDG_TOP(JI)-PK%XDG(JI,JL)+PK%XDZG(JI,JL)))
        ZWGHT_SUB(JI,JL)=MIN(PK%XDZG(JI,JL),MAX(0.0,ZDG_SUB(JI)-PK%XDG(JI,JL)+PK%XDZG(JI,JL)))        
        ZTG_TOP(JI)=ZTG_TOP(JI)+(PEK%XTG(JI,JL)-XTT)*ZWGHT_TOP(JI,JL)/ZDG_TOP(JI)
        ZTG_SUB(JI)=ZTG_SUB(JI)+(PEK%XTG(JI,JL)-XTT)*ZWGHT_SUB(JI,JL)/ZDG_SUB(JI)
     ENDDO
  ENDDO 
ENDIF
!
!
!*      2.     Autotrophic respiration
!              -----------------------
!
ZRESP_AUTO_ABOVE(:)=0.
!
IF (IO%CPHOTO=='NIT') THEN
!        
  DO JNBIOMASS=1,2
    ZRESP_AUTO_ABOVE(:) = ZRESP_AUTO_ABOVE(:) + PRESP_BIOMASS_INST(:,JNBIOMASS)
  ENDDO
!  
ELSE IF (IO%CPHOTO=='NCB') THEN
!        
  DO JNBIOMASS=1,3
    ZRESP_AUTO_ABOVE(:) = ZRESP_AUTO_ABOVE(:) + PRESP_BIOMASS_INST(:,JNBIOMASS)
  ENDDO
!  
ELSE IF (IO%CPHOTO=='AST') THEN
!        
  ZRESP_AUTO_ABOVE(:) = PRESP_BIOMASS_INST(:,1)
!  
ENDIF
!
DO JNBIOMASS=1,INBIOMASS
   DEK%XRESP_AUTO(:) = DEK%XRESP_AUTO (:) + PRESP_BIOMASS_INST(:,JNBIOMASS)
ENDDO
!
!
!*      3.     Soil and ecosystem respiration
!              ------------------------------
!
IF (IO%CRESPSL == 'DEF') THEN
!
   ZWORK(:)=0.0
!   
   IF(IO%CISBA/='DIF')THEN            
      ZWORK (:) = PEK%XWG(:,1)  
   ELSE
      DO JL=1,INL
         DO JI=1,INI        
            ZWORK(JI)=ZWORK(JI)+PEK%XWG(JI,JL)*ZWGHT_TOP(JI,JL)/ZDG_TOP(JI)
         ENDDO
      ENDDO  
   ENDIF
!
! Soil respiration from Norman et al 1992 (kgCO2/kgair m/s)
!
  WHERE (PEK%XLAI(:) == XUNDEF)
    ZRESP_SOIL_TOT(:) = 0.0
  ELSEWHERE
! Before optimization = (ZCOEF1/PRHOA)*(ZCOEF2+ZCOEF3*PEK%XLAI(:))*PEK%XWG(:,1)*2.**(ZCOEF4*(ZT2(:)-ZCOEF5))        
    ZRESP_SOIL_TOT(:) = (ZCOEF1/PRHOA)*(ZCOEF2+ZCOEF3*PEK%XLAI(:)) * &
                       ZWORK(:)*EXP(ZLOG2*(ZCOEF4*(ZTG_SUB(:)-ZCOEF5)))
  ENDWHERE
!
! RESP_ECO is diagnosed from RESP_SOIL_TOT and RESP_AUTO_ABOVE
!
  DEK%XRESP_ECO(:) = ZRESP_SOIL_TOT(:) + ZRESP_AUTO_ABOVE(:)
!  
ELSE IF (IO%CRESPSL == 'PRM') THEN
!
   ZWORK(:)=0.0
!   
   IF(IO%CISBA/='DIF')THEN            
      ZWORK (:) = PEK%XWG(:,1)  
   ELSE
      DO JL=1,INL
         DO JI=1,INI        
            ZWORK(JI)=ZWORK(JI)+MIN(1.0,PEK%XWG(JI,JL)/KK%XWFC(JI,JL))*ZWGHT_TOP(JI,JL)/ZDG_TOP(JI)
         ENDDO
      ENDDO  
   ENDIF
!
! Ecosystem respiration : Q10 model following Albergel et al. 2009 for SMOSREX
! (kgCO2/kgair m/s)
! RESP_ECO is directly calculated by the parameterization
!
! Before optimization 
! DEK%XRESP_ECO(:) = PK%XRE25(:)/PRHOA(:) * MIN(PEK%XWG(:,1)/KK%XWFC(:),1.)*2.**(ZCOEF4*(ZT2(:)-ZCOEF5))        
  DEK%XRESP_ECO(:) = PK%XRE25(:)/PRHOA(:) * ZWORK(:)*EXP(ZLOG2*(ZCOEF4*(ZTG_SUB(:)-ZCOEF5)))
!
ELSE IF (IO%CRESPSL == 'CNT') THEN
!
! Heterotrophic respiration following CENTURY model from Gibelin et al. 2008
!
  ZMOIST_TOP(:)=0.0
  ZSAT_TOP  (:)=0.0
  ZMOIST_SUB(:)=0.0
  ZSAT_SUB  (:)=0.0
  ZSAND_SUB (:)=0.0
!
  IF(IO%CISBA/='DIF')THEN
!          
    ZMOIST_TOP(:) = MIN(1.0,MAX(0.0,(PEK%XWG(:,1)-KK%XWWILT(:,1))/(KK%XWFC (:,1)-KK%XWWILT(:,1))))
    ZSAT_TOP  (:) = MIN(1.0,MAX(0.0,(PEK%XWG(:,1)-KK%XWFC  (:,1))/(KK%XWSAT(:,1)-KK%XWFC  (:,1))))
    ZMOIST_SUB(:) = MIN(1.0,MAX(0.0,(PEK%XWG(:,2)-KK%XWWILT(:,2))/(KK%XWFC (:,2)-KK%XWWILT(:,2))))
    ZSAT_SUB  (:) = MIN(1.0,MAX(0.0,(PEK%XWG(:,2)-KK%XWFC  (:,2))/(KK%XWSAT(:,2)-KK%XWFC  (:,2))))
!    
    ZSAND_SUB (:) = KK%XSAND (:,2)
!    
  ELSE
!          
    DO JL=1,INL
       DO JI=1,INI
!       
          ZMOISTL=MIN(1.0,MAX(0.0,(PEK%XWG(JI,JL)-KK%XWWILT(JI,JL))/(KK%XWFC (JI,JL)-KK%XWWILT(JI,JL))))
          ZSATL  =MIN(1.0,MAX(0.0,(PEK%XWG(JI,JL)-KK%XWFC  (JI,JL))/(KK%XWSAT(JI,JL)-KK%XWFC  (JI,JL)))) 
! 
          ZMOIST_TOP(JI)=ZMOIST_TOP(JI)+ZMOISTL*ZWGHT_TOP(JI,JL)/ZDG_TOP(JI)
          ZSAT_TOP  (JI)=ZSAT_TOP  (JI)+ZSATL  *ZWGHT_TOP(JI,JL)/ZDG_TOP(JI)
          ZMOIST_SUB(JI)=ZMOIST_SUB(JI)+ZMOISTL*ZWGHT_SUB(JI,JL)/ZDG_SUB(JI)
          ZSAT_SUB  (JI)=ZSAT_SUB  (JI)+ZSATL  *ZWGHT_SUB(JI,JL)/ZDG_SUB(JI)          
!
          ZSAND_SUB(JI)=ZSAND_SUB(JI)+KK%XSAND(JI,JL)*ZWGHT_SUB(JI,JL)/ZDG_SUB(JI)
!          
       ENDDO
    ENDDO 
!    
  ENDIF
!
  ZCONTROL_TEMP (:,1) = CONTROL_TEMP_FUNC (ZTG_TOP(:))
  ZCONTROL_TEMP (:,2) = CONTROL_TEMP_FUNC (ZTG_SUB(:))
  ZCONTROL_MOIST(:,1) = CONTROL_MOIST_FUNC(ZMOIST_TOP(:),ZSAT_TOP(:))
  ZCONTROL_MOIST(:,2) = CONTROL_MOIST_FUNC(ZMOIST_SUB(:),ZSAT_SUB(:))
!
  CALL CARBON_LITTER (PTSTEP, PK%XTURNOVER, PEK%XLITTER, PEK%XLIGNIN_STRUC, &
                     ZCONTROL_TEMP, ZCONTROL_MOIST, ZRESP_HETERO_DAY_LITTER,ZSOILCARBON_INPUT)  
!
  DO ITCSPIN = 1,IO%NSPINS
     CALL CARBON_SOIL (PTSTEP, ZSAND_SUB, ZSOILCARBON_INPUT, ZCONTROL_TEMP,&
                       ZCONTROL_MOIST, PEK%XSOILCARB, ZRESP_HETERO_DAY_SOIL   )  
  ENDDO
!
! Transform units : gC/m2/day -> kgCO2/kgair m/s
!
  ZRESP_HETERO(:) = (ZRESP_HETERO_DAY_LITTER(:) + ZRESP_HETERO_DAY_SOIL(:)) &
                    * (XMCO2/XMC) / (1000. * PRHOA(:)* XDAY)  
!  
! RESP_ECO is diagnosed from RESP_HETERO and RESP_AUTO
!
  DEK%XRESP_ECO(:) = ZRESP_HETERO(:) + DEK%XRESP_AUTO(:)
!  
ENDIF
!
IF (LHOOK) CALL DR_HOOK('CARBON_EVOL',1,ZHOOK_HANDLE)
!
!-----------------------------------------------------------------
!
END SUBROUTINE CARBON_EVOL
