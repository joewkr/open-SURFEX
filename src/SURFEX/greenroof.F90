!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE GREENROOF (DTCO, G, T, TOP, TIR, DTV, GB, DK, DEK, DMK, GRO, S, K, P, PEK,    &
                          HIMPLICIT_WIND, TPTIME, PTSUN, PPEW_A_COEF, PPEW_B_COEF,  &
                          PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF, PPEQ_B_COEF,       &
                          PTSTEP, PZREF, PUREF, PTA, PQA, PEXNS, PEXNA, PRHOA,      &
                          PCO2, PPS, PRR, PSR, PZENITH, PSW, PLW, PVMOD,            &
                          PALBNIR_TVEG, PALBVIS_TVEG, PALBNIR_TSOIL, PALBVIS_TSOIL, &                
                          PRN, PH, PLE, PGFLUX, PSFCO2, PEVAP, PUW, PRUNOFF, PDRAIN,&
                          PAC, PQSAT, PTSRAD, PAC_AGG, PHU_AGG, PDEEP_FLUX, PIRRIG )  
!   ##################################################################################
!
!!****  *GREENROOF*  
!!
!!    PURPOSE
!!    -------
!!
!!    call the vegetation scheme (ISBA) inside TEB for greenroofs
!!     
!!**  METHOD
!!     ------
!!    based on subroutine "garden" 
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!      
!!    REFERENCE
!!    ---------
!!    Based on subroutine "garden"
!!      
!!    AUTHOR
!!    ------
!!
!!      C. de Munck & A. Lemonsu          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!     Original    09/2011 
!     C. de Munck   02/2013  irrigation (drip irrigation)
!     B. decharme 04/2013 : Variables required in TEB to allow coupling with AROME/ALADIN/ARPEGE
!                           phasing call isba
!                           calculation of vegetation CO2 flux
!                           dummy for water table / surface coupling
!!    P. Samuelsson  10/2014  Introduced dummy variables in call to ISBA for MEB
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODD_SSO_n, ONLY : SSO_t, SSO_INIT
USE MODD_TEB_n, ONLY : TEB_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
USE MODD_TEB_IRRIG_n, ONLY : TEB_IRRIG_t
!
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_GR_BIOG_n, ONLY : GR_BIOG_t
!
USE MODD_DIAG_n, ONLY : DIAG_t
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_t
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_S_t, ISBA_K_t, ISBA_P_t, ISBA_PE_t
!
USE MODD_AGRI_n, ONLY : AGRI_t, AGRI_INIT
!
USE MODD_SURF_PAR,             ONLY: XUNDEF
USE MODD_TYPE_DATE_SURF,       ONLY: DATE_TIME
USE MODD_CSTS,                 ONLY: XCPD
!
USE MODI_ISBA
USE MODI_VEGETATION_UPDATE
USE MODI_VEGETATION_EVOL
USE MODI_CARBON_EVOL
USE MODE_THERMOS
USE MODI_ROOF_IMPL_COEF
USE MODI_TEB_IRRIG
USE MODI_FLAG_TEB_VEG_n
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    Declarations of arguments
!
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(GRID_t), INTENT(INOUT) :: G
TYPE(TEB_t), INTENT(INOUT) :: T
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
TYPE(TEB_IRRIG_t), INTENT(INOUT) :: TIR
!
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTV
TYPE(GR_BIOG_t), INTENT(INOUT) :: GB
!
TYPE(DIAG_t), INTENT(INOUT) :: DK
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: DEK
TYPE(DIAG_MISC_ISBA_t), INTENT(INOUT) :: DMK
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: GRO
TYPE(ISBA_S_t), INTENT(INOUT) :: S
TYPE(ISBA_K_t), INTENT(INOUT) :: K
TYPE(ISBA_P_t), INTENT(INOUT) :: P
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
!
 CHARACTER(LEN=*),     INTENT(IN)  :: HIMPLICIT_WIND   ! wind implicitation option
!                                                     ! 'OLD' = direct
!                                                     ! 'NEW' = Taylor serie, order 1
TYPE(DATE_TIME)     , INTENT(IN)    :: TPTIME             ! current date and time from teb
REAL, DIMENSION(:)  , INTENT(IN)    :: PTSUN              ! solar time      (s from midnight)
REAL, DIMENSION(:)  , INTENT(IN)    :: PPEW_A_COEF        ! implicit coefficients
REAL, DIMENSION(:)  , INTENT(IN)    :: PPEW_B_COEF        ! for wind coupling
REAL, DIMENSION(:)  , INTENT(IN)    :: PPEQ_A_COEF        ! implicit coefficients
REAL, DIMENSION(:)  , INTENT(IN)    :: PPEQ_B_COEF        ! for humidity
REAL, DIMENSION(:)  , INTENT(IN)    :: PPET_A_COEF        ! implicit coefficients
REAL, DIMENSION(:)  , INTENT(IN)    :: PPET_B_COEF        ! for temperature
REAL                , INTENT(IN)    :: PTSTEP             ! time step
REAL, DIMENSION(:)  , INTENT(IN)    :: PZREF              ! height of the first atmospheric level
REAL, DIMENSION(:)  , INTENT(IN)    :: PUREF              ! reference height for the wind
REAL, DIMENSION(:)  , INTENT(IN)    :: PTA                ! temperature at first atm. level 
REAL, DIMENSION(:)  , INTENT(IN)    :: PQA                ! specific humidity at first atm. level
REAL, DIMENSION(:)  , INTENT(IN)    :: PPS                ! pressure at the surface
REAL, DIMENSION(:)  , INTENT(IN)    :: PEXNA              ! Exner function at first atm. level
REAL, DIMENSION(:)  , INTENT(IN)    :: PEXNS              ! surface Exner function
REAL, DIMENSION(:)  , INTENT(IN)    :: PRHOA              ! air density at the lowest level
REAL, DIMENSION(:)  , INTENT(IN)    :: PCO2               ! CO2 concentration in the air    (kg/m3)
REAL, DIMENSION(:)  , INTENT(IN)    :: PRR                ! rain rate
REAL, DIMENSION(:)  , INTENT(IN)    :: PSR                ! snow rate
REAL, DIMENSION(:)  , INTENT(IN)    :: PZENITH            ! solar zenithal angle
REAL, DIMENSION(:)  , INTENT(IN)    :: PSW                ! incoming total solar rad on an horizontal surface
REAL, DIMENSION(:)  , INTENT(IN)    :: PLW                ! atmospheric infrared radiation
REAL, DIMENSION(:)  , INTENT(IN)    :: PVMOD              ! module of horizontal wind near first atm. level
REAL, DIMENSION(:)  , INTENT(IN)    :: PALBNIR_TVEG       ! nearIR  veg tot albedo
REAL, DIMENSION(:)  , INTENT(IN)    :: PALBVIS_TVEG       ! visible veg tot albedo
REAL, DIMENSION(:)  , INTENT(IN)    :: PALBNIR_TSOIL      ! nearIR  soil tot albedo
REAL, DIMENSION(:)  , INTENT(IN)    :: PALBVIS_TSOIL      ! visible soil tot albedo
!
REAL, DIMENSION(:)  , INTENT(OUT)   :: PRN         ! net radiation over greenroofs
REAL, DIMENSION(:)  , INTENT(OUT)   :: PH          ! sensible heat flux over greenroofs
REAL, DIMENSION(:)  , INTENT(OUT)   :: PLE         ! latent heat flux over greenroofs
REAL, DIMENSION(:)  , INTENT(OUT)   :: PGFLUX      ! flux through the greenroofs
REAL, DIMENSION(:)  , INTENT(OUT)   :: PSFCO2      ! flux of greenroof CO2       (m/s*kg_CO2/kg_air)
REAL, DIMENSION(:)  , INTENT(OUT)   :: PEVAP       ! total evaporation over greenroofs (kg/m2/s)
REAL, DIMENSION(:)  , INTENT(OUT)   :: PUW         ! friction flux (m2/s2)
REAL, DIMENSION(:)  , INTENT(OUT)   :: PRUNOFF     ! greenroof surface runoff
REAL, DIMENSION(:)  , INTENT(OUT)   :: PDRAIN      ! greenroof surface drainage
REAL, DIMENSION(:)  , INTENT(OUT)   :: PAC         ! greenroof aerodynamical conductance
REAL, DIMENSION(:)  , INTENT(OUT)   :: PQSAT       ! saturation humidity
REAL, DIMENSION(:)  , INTENT(OUT)   :: PTSRAD      ! greenroof radiative surface temp. (snow free)
REAL, DIMENSION(:)  , INTENT(OUT)   :: PAC_AGG     ! aggreg. aeodynamic resistance for greenroofs for latent heat flux
REAL, DIMENSION(:)  , INTENT(OUT)   :: PHU_AGG     ! aggreg. relative humidity for greenroofs for latent heat flux
REAL, DIMENSION(:)  , INTENT(OUT)   :: PDEEP_FLUX  ! Heat Flux at the bottom layer of the greenroof
REAL, DIMENSION(:)  , INTENT(OUT)   :: PIRRIG      ! greenroof summer irrigation rate
!
!
!*      0.2    Declarations of local variables
!
TYPE(SSO_t) :: YSS
TYPE(AGRI_t) :: YAG
!
REAL, DIMENSION(SIZE(PPS)) :: ZDIRCOSZW           ! orography slope cosine (=1 in TEB)
REAL, DIMENSION(SIZE(PPS),GRO%NNBIOMASS) :: ZRESP_BIOMASS_INST       ! instantaneous biomass respiration (kgCO2/kgair m/s)
REAL, DIMENSION(SIZE(PPS)) :: ZUSTAR
!
!  temperatures
!
REAL, DIMENSION(SIZE(PPS)) :: ZTA ! estimate of air temperature at future time
!                                 ! step as if modified by ISBA flux alone.
!
!  surfaces relative fractions
!  for flood
REAL, DIMENSION(SIZE(PPS)) :: ZEMISF
!
!  variables for deep soil temperature
REAL, DIMENSION(SIZE(PPS)) :: ZTDEEP_A
!
! Dummy variables for MEB:
REAL, DIMENSION(SIZE(PPS)) :: ZP_MEB_SCA_SW, ZPALPHAN, ZZ0G_WITHOUT_SNOW, &
                              ZZ0_MEBV, ZZ0H_MEBV, ZZ0EFF_MEBV, ZZ0_MEBN, &
                              ZZ0H_MEBN, ZZ0EFF_MEBN
!
!
INTEGER                    :: ILU
LOGICAL :: GUPDATED, GALB
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*      1.     various initialisations
!              -----------------------
!
IF (LHOOK) CALL DR_HOOK('GREENROOF',0,ZHOOK_HANDLE)
ILU = SIZE(PPS)
!
ZDIRCOSZW = 1.
!
 CALL SSO_INIT(YSS)
!
 CALL AGRI_INIT(YAG)
!
!* automatic summer irrigation 
!
PIRRIG(:) = 0.
!
!* deep soil implicitation with roof
!
 CALL ROOF_IMPL_COEF(T, PTSTEP, ZTDEEP_A, K%XTDEEP)
!
!-------------------------------------------------------------------------------
!
!*      9.     Treatment of green areas
!              ------------------------
!
!radiative temperature diagnostic
!-------------------------------
!
!*      9.1    Summer irrigation 
!              ------------------
!
!* irrigation automatique de type goutte à goutte (arrosage du sol seulement)
!
CALL TEB_IRRIG(TIR%LPAR_GR_IRRIG, PTSTEP, TPTIME%TDATE%MONTH, PTSUN,         &
               TIR%XGR_START_MONTH, TIR%XGR_END_MONTH, TIR%XGR_START_HOUR,   &
               TIR%XGR_END_HOUR, TIR%XGR_24H_IRRIG, PIRRIG     )
!
! --------------------------------------------------------------------------------------
! Vegetation update (in case of non-interactive vegetation):
! --------------------------------------------------------------------------------------
!
S%TTIME = TPTIME
!
GUPDATED=.FALSE.
GALB = .FALSE. 
IF (GRO%CPHOTO=='NIT'.OR.GRO%CPHOTO=='NCB') GALB = .TRUE.
!
  CALL VEGETATION_UPDATE(DTCO, DTV, G%NDIM, GRO, K, P, PEK, 1,              &
                         PTSTEP, S%TTIME, TOP%XCOVER, TOP%LCOVER,  .FALSE., &
                         'GNR', GALB, YSS, GUPDATED, OABSENT=(T%XGREENROOF==0.)  )
!
!*      9.2    Call ISBA for greenroofs
!              ------------------------
!
DK%XZ0(:) = PEK%XZ0(:)
DK%XZ0H(:) = PEK%XZ0(:) / P%XZ0_O_Z0H(:)
!
DK%XZ0EFF(:) =  PEK%XZ0(:)
!
ALLOCATE(GB%XIACAN(SIZE(PPS),SIZE(S%XABC)))
!
 CALL ISBA(GRO, K, P, PEK, G, YAG, DK, DEK, DMK,                                  &
           TPTIME, S%XPOI, S%XABC, GB%XIACAN, .FALSE., PTSTEP,                    &
           HIMPLICIT_WIND, PZREF, PUREF, ZDIRCOSZW, PTA, PQA, PEXNA, PRHOA, PPS,  &
           PEXNS, PRR, PSR, PZENITH, ZP_MEB_SCA_SW, PSW, PLW, PVMOD, PPEW_A_COEF, &
           PPEW_B_COEF, PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF, PPEQ_B_COEF,       &
           PALBNIR_TVEG, PALBVIS_TVEG, PALBNIR_TSOIL, PALBVIS_TSOIL, ZPALPHAN,    &
           ZZ0G_WITHOUT_SNOW, ZZ0_MEBV, ZZ0H_MEBV, ZZ0EFF_MEBV, ZZ0_MEBN,         &
           ZZ0H_MEBN, ZZ0EFF_MEBN, ZTDEEP_A, PCO2, K%XFFG(:), K%XFFV(:),          &
           ZEMISF, ZUSTAR, PAC_AGG, PHU_AGG, ZRESP_BIOMASS_INST, PDEEP_FLUX, PIRRIG )
!
IF (PEK%TSNOW%SCHEME=='3-L' .OR. PEK%TSNOW%SCHEME=='CRO') PEK%TSNOW%TS(:) = DMK%XSNOWTEMP(:,1)
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Diagnostic of respiration carbon fluxes and soil carbon evolution
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Vegetation evolution for interactive LAI
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!
IF (GRO%CPHOTO=='NIT') THEN
  CALL VEGETATION_EVOL(GRO, DTV, P, PEK, .FALSE., PTSTEP, TPTIME%TDATE%MONTH, TPTIME%TDATE%DAY,     &
                       TPTIME%TIME, G%XLAT, PRHOA, PCO2, YSS, ZRESP_BIOMASS_INST )          
END IF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
PSFCO2          (:) = 0.
DEK%XRESP_ECO (:) = 0.
DEK%XRESP_AUTO(:) = 0.
!
IF (GRO%CPHOTO/='NON' .AND. GRO%CRESPSL/='NON' .AND. ANY(PEK%XLAI(:)/=XUNDEF)) THEN
  ! faire intervenir le type de vegetation du greenroof ? (CTYP_GR)
  CALL CARBON_EVOL(GRO, K, P, PEK, DEK, PTSTEP, PRHOA, ZRESP_BIOMASS_INST  )
  ! calculation of vegetation CO2 flux
  ! Positive toward the atmosphere
  PSFCO2(:) = DEK%XRESP_ECO(:) - DEK%XGPP(:)
END IF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!*      4.     Set undefined values for points where there is no garden
!              --------------------------------------------------------
!
! This way, these points are clearly flaged, and one will not try to interpret
! the values for those points
!
 CALL FLAG_TEB_VEG_n(PEK, GRO, T%XGREENROOF, 2)
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!*      9.     Fields required for TEB
!              -----------------------
!
WHERE (T%XGREENROOF/=0.)
  !
  ! energy balance
  !
  DK%XLE(:) = PEK%XLE(:)
  !
  ! Estimate of green area aerodynamic conductance recomputed from heat flux,
  ! surface (radiative) temp. and forcing air temperature (estimated at future time step)
  ZTA = PPET_B_COEF + PPET_A_COEF * DK%XH
  PAC = 0.
  WHERE (DK%XTSRAD /= ZTA)
    PAC(:)   = MAX(DK%XH(:) / XCPD / PRHOA(:) / (DK%XTSRAD - ZTA) , 0.)
  ENDWHERE
  !
  ! Humidity of saturation for green areas
  PQSAT(:) = QSAT(PEK%XTG(:,1),PPS(:))
  !
  !* friction flux
  PUW(:)    = -ZUSTAR(:)**2
  !
ELSEWHERE
  !
  DK%XRN    (:) = XUNDEF
  DK%XH     (:) = XUNDEF
  DK%XLE    (:) = XUNDEF
  DK%XGFLUX (:) = XUNDEF
  DK%XEVAP  (:) = XUNDEF
  !
  PAC    (:) = XUNDEF
  PQSAT  (:) = XUNDEF
  PUW    (:) = XUNDEF  
  !
END WHERE
!
!
PTSRAD(:) = DK%XTSRAD(:)
!
PRN   (:) = DK%XRN     (:) 
PH    (:) = DK%XH      (:) 
PLE   (:) = DK%XLE     (:) 
PGFLUX(:) = DK%XGFLUX  (:) 
PEVAP (:) = DK%XEVAP   (:) 
PRUNOFF(:) =DEK%XRUNOFF(:)
PDRAIN (:) =DEK%XDRAIN (:)
!
IF (LHOOK) CALL DR_HOOK('GREENROOF',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!
END SUBROUTINE GREENROOF
