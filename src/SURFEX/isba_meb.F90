!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
MODULE MODI_ISBA_MEB
CONTAINS
      SUBROUTINE ISBA_MEB(IO, KK, PK, PEK, DK, DEK, DMK, G, AG,                     &
                          TPTIME, OMEB, OSHADE, HIMPLICIT_WIND, PTSTEP,             &
                          PSOILHCAPZ, PSOILCONDZ, PFROZEN1, PPS, PZENITH,           &
                          PSCA_SW, PSW_RAD, PVMOD, PRR, PSR, PRHOA, PTA, PQA,       &
                          PDIRCOSZW, PEXNS, PEXNA, PPET_A_COEF, PPET_B_COEF,        &
                          PPEQ_A_COEF, PPEQ_B_COEF, PPEW_A_COEF, PPEW_B_COEF,       &
                          PZREF, PUREF, PZ0G_WITHOUT_SNOW, PZ0_MEBV, PZ0H_MEBV,     &
                          PZ0EFF_MEBV, PZ0_MEBN, PZ0H_MEBN, PZ0EFF_MEBN,            &
                          PALBNIR_TVEG, PALBVIS_TVEG, PALBNIR_TSOIL, PALBVIS_TSOIL, &
                          PABC, PIACAN, PPOI, PCSP, PRESP_BIOMASS_INST, PPALPHAN,   &
                          PF2, PLW_RAD, PGRNDFLUX, PFLSN_COR, PUSTAR, PEMIST,       &
                          PHU_AGG, PAC_AGG, PDELHEATV_SFC, PDELHEATG_SFC, PDELHEATG,&
                          PDELHEATN, PDELHEATN_SFC, PRESTOREN, PTDEEP_A, PDEEP_FLUX,&
                          PRISNOW, PSNOW_THRUFAL, PSNOW_THRUFAL_SOIL, PEVAPCOR,     &
                          PSUBVCOR, PLITCOR, PSNOWSFCH, PQSNOW)
!     ##########################################################################
!
!
!!****  *isba_meb*
!!
!!    PURPOSE
!!    -------
!       Monitor for the calculation of the surface fluxes and of the
!     prognostic variables of the surface over natural areas
!     with an explicit vegetation layer
!
!     NOTE...currently MEB can be coupled with
!     IO%CISBA='DIF' or '3-L' soil options
!     HSNOW='3-L' snow scheme
!     Soon, HSNOW=CRO and IO%CPHOTO/=NON (i.e. Ags will be added)
!
!!**  METHOD
!!    ------
!
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
!!    Noilhan and Planton (1989)
!!
!!    AUTHOR
!!    ------
!!	A. Boone           * Meteo-France *
!!      P. Samuelsson      * SMHI *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       10/2014
!!      (A. Napoly)    09/2015  Add Litter layer option code
!!      (A. Boone)     02/2017  Owing to fix to FAPAIR.F90 routine (called by
!!                              RAIDATIVE_TRANSFERT.F90 herein), edited slightly
!!                              SWnet computations to be compatible.
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_K_t, ISBA_P_t, ISBA_PE_t
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODD_AGRI_n, ONLY : AGRI_t
USE MODD_DIAG_n, ONLY : DIAG_t
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_t
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_CSTS,           ONLY : XCPD, XDAY, XRHOLW, XLVTT, XLSTT
USE MODD_MEB_PAR,        ONLY : XSW_WGHT_VIS, XSW_WGHT_NIR
USE MODD_ISBA_PAR,       ONLY : XRS_MAX, XLIMH
USE MODD_DATA_COVER_PAR, ONLY : NVT_SNOW
!
USE MODD_TYPE_DATE_SURF, ONLY : DATE_TIME
!
USE MODE_THERMOS
USE MODE_MEB,            ONLY : SNOW_INTERCEPT_EFF
!
USE MODI_WET_LEAVES_FRAC
USE MODI_VEG
USE MODI_SNOW_LEAVES_FRAC_MEB
USE MODI_PREPS_FOR_MEB_EBUD_RAD
USE MODI_ISBA_LWNET_MEB
USE MODI_DRAG_MEB
USE MODI_E_BUDGET_MEB
USE MODI_ISBA_FLUXES_MEB
USE MODI_SNOW_LOAD_MEB
USE MODI_HYDRO_VEG
USE MODI_SNOW3L_ISBA
USE MODI_RADIATIVE_TRANSFERT
USE MODI_COTWORES
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!              -------------------------
!
!
!* general variables
!  -----------------
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_K_t), INTENT(INOUT) :: KK
TYPE(ISBA_P_t), INTENT(INOUT) :: PK
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
TYPE(GRID_t), INTENT(INOUT) :: G
TYPE(AGRI_t), INTENT(INOUT) :: AG
TYPE(DIAG_t), INTENT(INOUT) :: DK
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: DEK
TYPE(DIAG_MISC_ISBA_t), INTENT(INOUT) :: DMK
!
TYPE(DATE_TIME),      INTENT(IN)    :: TPTIME        ! current date and time
!
LOGICAL,              INTENT(IN)    :: OMEB          ! True = patch with multi-energy balance
!                                                    ! False = patch with classical ISBA
LOGICAL, DIMENSION(:),INTENT(INOUT) :: OSHADE        ! where vegetation evolution occurs
CHARACTER(LEN=*),     INTENT(IN)    :: HIMPLICIT_WIND! wind implicitation option
!                                                    ! 'OLD' = direct
!                                                    ! 'NEW' = Taylor serie, order 1
REAL,                 INTENT(IN)    :: PTSTEP        ! Model time step (s)
!
REAL, DIMENSION(:),   INTENT(IN)    :: PPS           ! Pressure [Pa]
REAL, DIMENSION(:),   INTENT(IN)    :: PZENITH       ! solar zenith angle
REAL, DIMENSION(:),   INTENT(IN)    :: PSW_RAD       ! solar (shortwave) incoming radiation [W/m2]
REAL, DIMENSION(:),   INTENT(IN)    :: PLW_RAD       ! thermal (longwave) incoming radiation [W/m2]
REAL, DIMENSION(:),   INTENT(IN)    :: PSCA_SW       ! solar diffuse incoming radiation [W/m2]
REAL, DIMENSION(:),   INTENT(IN)    :: PEXNA         ! Exner function: forcing level (-)
REAL, DIMENSION(:),   INTENT(IN)    :: PEXNS         ! Exner function: surface (-)
REAL, DIMENSION(:),   INTENT(IN)    :: PRR           ! Rain rate (kg/m2/s)
REAL, DIMENSION(:),   INTENT(IN)    :: PSR           ! Snow rate (kg/m2/s)
REAL, DIMENSION(:),   INTENT(IN)    :: PRHOA         ! air density (kg/m3)
REAL, DIMENSION(:),   INTENT(IN)    :: PVMOD         ! modulus of the wind
!                                                    ! parallel to the orography (m/s)
REAL, DIMENSION(:),   INTENT(IN)    :: PTA           ! Temperature of atmosphere (K)
REAL, DIMENSION(:),   INTENT(IN)    :: PQA           ! specific humidity of atmosphere (kg/kg)
REAL, DIMENSION(:),   INTENT(IN)    :: PZREF         ! normal distance of the first
!                                                    ! atmospheric level to the
!                                                    ! orography (m)
REAL, DIMENSION(:),   INTENT(IN)    :: PUREF         ! reference height of the wind (m)
!                                                    ! NOTE this is different from ZZREF
!                                                    ! ONLY in stand-alone/forced mode,
!                                                    ! NOT when coupled to a model (MesoNH)
REAL, DIMENSION(:),   INTENT(IN)    :: PDIRCOSZW     ! Director Cosinus along the z
!                                                    ! direction at the surface w-point
REAL, DIMENSION(:,:), INTENT(IN)    :: PSOILHCAPZ    ! ISBA-DF Soil heat capacity
!                                                    ! profile [J/(m3 K)]
REAL, DIMENSION(:,:), INTENT(IN)    :: PSOILCONDZ    ! ISBA-DF Soil conductivity
!                                                    ! profile  [W/(m K)]
REAL, DIMENSION(:),   INTENT(IN)    :: PFROZEN1      ! surface frozen fraction (-)
!
REAL, DIMENSION(:),   INTENT(IN)    :: PPALPHAN      ! snow/canopy transition coefficient
REAL, DIMENSION(:),   INTENT(IN)    :: PALBNIR_TVEG  ! albedo of vegetation in NIR
!                                                    ! (needed for LM_TR or MEB)
REAL, DIMENSION(:),   INTENT(IN)    :: PALBVIS_TVEG  ! albedo of vegetation in VIS
!                                                    ! (needed for LM_TR or MEB)
REAL, DIMENSION(:),   INTENT(IN)    :: PALBNIR_TSOIL ! albedo of bare soil in NIR
!                                                    ! (needed for LM_TR or MEB)
REAL, DIMENSION(:),   INTENT(IN)    :: PALBVIS_TSOIL ! albedo of bare soil in VIS
REAL, DIMENSION(:),   INTENT(IN)    :: PF2           ! Soil water stress factor for transpiration (-)
REAL, DIMENSION(:),   INTENT(IN)    :: PZ0G_WITHOUT_SNOW ! roughness length for momentum at snow-free canopy floor (m)
REAL, DIMENSION(:),   INTENT(IN)    :: PZ0_MEBV      ! roughness length for momentum over MEB vegetation part of patch (m)
REAL, DIMENSION(:),   INTENT(IN)    :: PZ0H_MEBV     ! roughness length for heat over MEB vegetation part of path (m)
REAL, DIMENSION(:),   INTENT(IN)    :: PZ0EFF_MEBV   ! roughness length for momentum over MEB vegetation part of patch (m)
REAL, DIMENSION(:),   INTENT(IN)    :: PZ0_MEBN      ! roughness length for momentum over MEB snow part of patch (m)
REAL, DIMENSION(:),   INTENT(IN)    :: PZ0H_MEBN     ! roughness length for heat over MEB snow part of path (m)
REAL, DIMENSION(:),   INTENT(IN)    :: PZ0EFF_MEBN   ! roughness length for momentum over MEB snow part of patch (m)
!
! implicit atmospheric coupling coefficients:
!
REAL, DIMENSION(:),   INTENT(IN)    :: PPET_A_COEF, PPET_B_COEF, &
                                       PPEQ_A_COEF, PPEQ_B_COEF, &
                                       PPEW_A_COEF, PPEW_B_COEF
!                                                    ! PPEW_A_COEF  A-wind coefficient
!                                                    ! PPEW_B_COEF  B-wind coefficient
!                                                    ! PPET_A_COEF  A-air temperature coefficient
!                                                    ! PPET_B_COEF  B-air temperature coefficient
!                                                    ! PPEQ_A_COEF  A-air specific humidity coefficient
!                                                    ! PPEQ_B_COEF  B-air specific humidity coefficient
REAL, DIMENSION(:),   INTENT(IN)    :: PTDEEP_A          ! Deep soil temperature boundary condition
!                                                         ! (prescribed)
!                                      PTDEEP_A = Deep soil temperature
!                                                 coefficient depending on flux
!
! ISBA-Ags parameters
! (see also parameters with 'Ags:' in comments)
!
REAL, DIMENSION(:),   INTENT(IN) :: PCSP       ! atmospheric CO2 concentration
!                                                 [ppmm]=[kg CO2 / kg air]
REAL, DIMENSION(:),   INTENT(IN) :: PPOI       ! Gaussian weights (as above)
!                                              ! temperature
! - - - - - - - - - - - - - - - - - - - -
!
REAL, DIMENSION(:),   INTENT(INOUT) :: PABC          ! Ags: abscissa needed for integration
!                                                    ! of net assimilation and stomatal
!                                                    ! conductance over canopy depth
REAL, DIMENSION(:,:), INTENT(OUT)   :: PIACAN        ! PAR in the canopy at different gauss levels
!                                                    ! when using the DIF soil option (W/m2)
REAL, DIMENSION(:),   INTENT(OUT)   :: PUSTAR        ! friction velocity
!
REAL, DIMENSION(:),   INTENT(OUT)   :: PGRNDFLUX     ! snow/soil-biomass interface flux (W/m2)
REAL, DIMENSION(:),   INTENT(OUT)   :: PFLSN_COR     ! soil/snow interface correction flux to conserve energy (W/m2)
REAL, DIMENSION(:),   INTENT(OUT)   :: PEMIST        ! total effective surface emissivity...LWUP = EMIST*TS_RAD**4 (-)
REAL, DIMENSION(:),   INTENT(OUT)   :: PAC_AGG       ! aggregated aerodynamic conductance
                                                     ! for evaporative flux calculations
REAL, DIMENSION(:),   INTENT(OUT)   :: PHU_AGG       ! aggregated relative humidity
                                                     ! for evaporative flux calculations
REAL, DIMENSION(:),   INTENT(OUT)   :: PDELHEATV_SFC ! change in heat storage of the vegetation canopy layer over the current time step (W/m2)
REAL, DIMENSION(:),   INTENT(OUT)   :: PDELHEATG_SFC ! change in heat storage of the ground sfc layer over the current time step (W/m2)
REAL, DIMENSION(:),   INTENT(OUT)   :: PDELHEATG     ! change in heat storage of the entire soil column over the current time step (W/m2)
REAL, DIMENSION(:),   INTENT(OUT)   :: PRESTOREN     ! conductive heat flux between the surface and sub-surface soil layers
!                                                    ! for the multi-layer snow schemes..for composite snow, it is
!                                                    ! equal to DEK%XRESTORE (W/m2)
REAL, DIMENSION(:),   INTENT(OUT)   :: PDELHEATN     ! change in heat storage of the entire snow column over the current time step (W/m2)
REAL, DIMENSION(:),   INTENT(OUT)   :: PDELHEATN_SFC ! change in heat storage of the surface snow layer over the current time step (W/m2)
REAL, DIMENSION(:),   INTENT(OUT)   :: PDEEP_FLUX    ! Heat flux at bottom of ISBA (W/m2)
REAL, DIMENSION(:),   INTENT(OUT)   :: PRISNOW       ! Richarson number over ground-based snowpack (-)
REAL, DIMENSION(:),   INTENT(OUT)   :: PSNOW_THRUFAL ! rate that liquid water leaves (explicit) snow pack:
!                                                    ! ISBA-ES or CROCUS [kg/(m2 s)]
REAL, DIMENSION(:),   INTENT(OUT)   :: PSNOW_THRUFAL_SOIL !liquid water leaving the snowpack directly to the
!                                                         !soil, ISBA-ES: [kg/(m2 s)] (equal to ZSNOW_THRUFAL
!                                                         !if OMEB_LITTER=False and zero if OMEB_LITTER=True)
!                                                    ! ISBA-ES or CROCUS [kg/(m2 s)]
REAL, DIMENSION(:),   INTENT(OUT)   :: PEVAPCOR      !  evaporation correction as last traces of snow
!                                                    ! cover ablate..if sublimation exceeds trace amounts
                                                     ! of snow during time step, required residual mass taken
                                                     ! from sfc soil layer [kg/(m2 s)]
REAL, DIMENSION(:),   INTENT(OUT)   :: PSUBVCOR      ! A possible snow mass correction (to be potentially
!                                                    !  removed from soil)  (kg/m2/s)
REAL, DIMENSION(:),   INTENT(OUT)   :: PLITCOR       ! A possible ice mass correction in litter layer (to be potentially
!                                                    !  removed from soil)  (kg/m2/s)
REAL, DIMENSION(:),   INTENT(OUT)   :: PSNOWSFCH     ! snow surface layer pseudo-heating term owing to
!                                                    !  changes in grid thickness            (W/m2)
REAL, DIMENSION(:),   INTENT(OUT)   :: PQSNOW        ! snow surface specific humidity (kg/kg)
!
! diagnostic variables for Carbon assimilation:
!
REAL, DIMENSION(:,:), INTENT(OUT)   :: PRESP_BIOMASS_INST ! instantaneous biomass respiration (kgCO2/kgair m/s)
!
!*      0.2    declarations of local variables
!
!
REAL, PARAMETER                                    :: ZTSTEP_EB     = 300. ! s Minimum time tstep required
!                                                                          !   to time-split MEB energy budget
REAL, PARAMETER                                    :: ZSWRAD_MIN    = 1.E-6! W/m2 Threshold SWdown for which Sun is up...approx
                                                                           !      (No need to do nonsense SW computations during the night)
!
INTEGER                                            :: JTSPLIT_EB           ! number of time splits
INTEGER                                            :: JDT                  ! time split loop index
!
REAL                                               :: ZTSTEP               ! Local time split timestep (s)
REAL, DIMENSION(SIZE(PPS))                         :: ZWORK,ZWORK2,ZWORK3,ZWORK4  ! Working variables [*]
REAL, DIMENSION(SIZE(PEK%TSNOW%WSNOW,1),SIZE(PEK%TSNOW%WSNOW,2)) :: ZSNOWCOND            ! snow thermal conductivity  [W/(m K)]
REAL, DIMENSION(SIZE(PEK%TSNOW%WSNOW,1),SIZE(PEK%TSNOW%WSNOW,2)) :: ZSNOWHCAP            ! snow heat capacity [J/(m3 K)]
REAL, DIMENSION(SIZE(PEK%TSNOW%WSNOW,1),SIZE(PEK%TSNOW%WSNOW,2)) :: ZSNOWRHO             ! snow layer density (kg/m3)
REAL, DIMENSION(SIZE(PEK%TSNOW%WSNOW,1),SIZE(PEK%TSNOW%WSNOW,2)) :: ZSNOWAGE             ! snow layer grain age
REAL, DIMENSION(SIZE(PEK%TSNOW%WSNOW,1),SIZE(PEK%TSNOW%WSNOW,2)) :: ZSNOWSWE             ! snow layer liquid water equivalent (kg/m2)
REAL, DIMENSION(SIZE(PEK%TSNOW%WSNOW,1),SIZE(PEK%TSNOW%WSNOW,2)) :: ZSNOWLIQ             ! snow layer liquid water (m)
REAL, DIMENSION(SIZE(PEK%TSNOW%WSNOW,1),SIZE(PEK%TSNOW%WSNOW,2)) :: ZTAU_N               ! snow rad transmission coef at layer base (-)
REAL, DIMENSION(SIZE(PPS))                         :: ZCHIP                !
REAL, DIMENSION(SIZE(PPS))                         :: ZALBS                ! Effective surface (ground) albedo
REAL, DIMENSION(SIZE(PPS))                         :: ZSIGMA_F             ! LW transmission factor
REAL, DIMENSION(SIZE(PPS))                         :: ZSIGMA_FN            ! LW transmission factor - including buried (snow)
!                                                                          ! vegetation effect
REAL, DIMENSION(SIZE(PPS))                         :: ZDLWNET_V_DTV        ! LW Jacobian: flux derrivative d LWnet_v/dTv [W/(m K2)]
REAL, DIMENSION(SIZE(PPS))                         :: ZDLWNET_V_DTG        ! LW Jacobian: flux derrivative d LWnet_v/dTg [W/(m K2)]
REAL, DIMENSION(SIZE(PPS))                         :: ZDLWNET_V_DTN        ! LW Jacobian: flux derrivative d LWnet_v/dTn [W/(m K2)]
REAL, DIMENSION(SIZE(PPS))                         :: ZDLWNET_G_DTV        ! LW Jacobian: flux derrivative d LWnet_g/dTv [W/(m K2)]
REAL, DIMENSION(SIZE(PPS))                         :: ZDLWNET_G_DTG        ! LW Jacobian: flux derrivative d LWnet_g/dTg [W/(m K2)]
REAL, DIMENSION(SIZE(PPS))                         :: ZDLWNET_G_DTN        ! LW Jacobian: flux derrivative d LWnet_g/dTn [W/(m K2)]
REAL, DIMENSION(SIZE(PPS))                         :: ZDLWNET_N_DTV        ! LW Jacobian: flux derrivative d LWnet_n/dTv [W/(m K2)]
REAL, DIMENSION(SIZE(PPS))                         :: ZDLWNET_N_DTG        ! LW Jacobian: flux derrivative d LWnet_n/dTg [W/(m K2)]
REAL, DIMENSION(SIZE(PPS))                         :: ZDLWNET_N_DTN        ! LW Jacobian: flux derrivative d LWnet_n/dTn [W/(m K2)]
REAL, DIMENSION(SIZE(PPS))                         :: ZWRMAX               ! maximum canopy water equivalent interception capacity  [kg/m2]
REAL, DIMENSION(SIZE(PPS))                         :: ZWRLMAX              ! maximum litter water equivalent interception capacity  [kg/m2]
REAL, DIMENSION(SIZE(PPS))                         :: ZRS                  ! stomatal resistance (s/m)
REAL, DIMENSION(SIZE(PPS))                         :: ZRSN                 ! stomatal resistance of non-snow-buried canopy (s/m)
!                                                                          ! Etv=>0 as F2=>0 (-)
REAL, DIMENSION(SIZE(PPS))                         :: ZWRVNMAX             ! maximum snow water equivalent interception capacity (kg/m2)
REAL, DIMENSION(SIZE(PPS))                         :: ZPSNCV               ! intercepted canopy snow fraction (-) NOTE! Not the same as the
!                                                                          ! ground-based snowpack
REAL, DIMENSION(SIZE(PPS))                         :: ZMELTVN              ! intercepted canopy snow net freeze/melt rate (kg/m2/s)
!                                                                          ! (if it is < 0, this signifies freezing)
REAL, DIMENSION(SIZE(PPS))                         :: ZTHRMA_TA            ! linear transform energy budget coefficient for Ta
REAL, DIMENSION(SIZE(PPS))                         :: ZTHRMB_TA            ! linear transform energy budget coefficient for Ta
REAL, DIMENSION(SIZE(PPS))                         :: ZTHRMA_TC            ! linear transform energy budget coefficient for Tc
REAL, DIMENSION(SIZE(PPS))                         :: ZTHRMB_TC            ! linear transform energy budget coefficient for Tc
REAL, DIMENSION(SIZE(PPS))                         :: ZTHRMA_TN            ! linear transform energy budget coefficient for Tn
REAL, DIMENSION(SIZE(PPS))                         :: ZTHRMB_TN            ! linear transform energy budget coefficient for Tn
REAL, DIMENSION(SIZE(PPS))                         :: ZTHRMA_TG            ! linear transform energy budget coefficient for Tg
REAL, DIMENSION(SIZE(PPS))                         :: ZTHRMB_TG            ! linear transform energy budget coefficient for Tg
REAL, DIMENSION(SIZE(PPS))                         :: ZTHRMA_TV            ! linear transform energy budget coefficient for Tv
REAL, DIMENSION(SIZE(PPS))                         :: ZTHRMB_TV            ! linear transform energy budget coefficient for Tv
REAL, DIMENSION(SIZE(PPS))                         :: ZPET_A_COEF          ! atmospheric coupling coefficient: Ta
REAL, DIMENSION(SIZE(PPS))                         :: ZPET_B_COEF          ! atmospheric coupling coefficient: Ta
REAL, DIMENSION(SIZE(PPS))                         :: ZKVN                 ! snow interception efficiency
REAL, DIMENSION(SIZE(PPS))                         :: ZVELC                ! wind speed at the top of the canopy (m/s)
REAL, DIMENSION(SIZE(PPS))                         :: ZDELTA               ! fraction of the foliage
!                                                                          ! covered with intercepted water (-)
REAL, DIMENSION(SIZE(PPS))                         :: ZHUGI                ! humidity over frozen bare ground (-)
REAL, DIMENSION(SIZE(PPS))                         :: ZHVN                 ! Halstead coefficient vegetation canopy above snow (-)
REAL, DIMENSION(SIZE(PPS))                         :: ZHVG                 ! Halstead coefficient vegetation canopy above snow-free ground (-)
REAL, DIMENSION(SIZE(PPS))                         :: ZLEG_DELTA           ! soil evaporation delta fn (-)
REAL, DIMENSION(SIZE(PPS))                         :: ZLEGI_DELTA          ! soil sublimation delta fn (-)
REAL, DIMENSION(SIZE(PPS))                         :: ZHSGL                ! surface halstead cofficient for bare soil (-)
REAL, DIMENSION(SIZE(PPS))                         :: ZHSGF                ! surface halstead cofficient for bare soil ice  (-)
REAL, DIMENSION(SIZE(PPS))                         :: ZFLXC_CA             ! turb transfer coef between vegetation canopy air and atmosphere (kg/m2/s)
REAL, DIMENSION(SIZE(PPS))                         :: ZFLXC_N_A            ! ...between the snow on the ground and atmosphere    (kg/m2/s)
REAL, DIMENSION(SIZE(PPS))                         :: ZFLXC_GV             ! ...between snow-free ground and canopy air     (kg/m2/s)
REAL, DIMENSION(SIZE(PPS))                         :: ZFLXC_GN             ! ...between snow on the ground and canopy air   (kg/m2/s)
REAL, DIMENSION(SIZE(PPS))                         :: ZFLXC_VG_C           ! ...between vegetation canopy over snow-free ground and canopy air   (kg/m2/s)
REAL, DIMENSION(SIZE(PPS))                         :: ZFLXC_VN_C           ! ...between vegetation canopy over the snow on the ground and canopy air  (kg/m2/s)
REAL, DIMENSION(SIZE(PPS))                         :: ZFLXC_CV             ! ...between vegetation canopy and canopy air  (kg/m2/s)
REAL, DIMENSION(SIZE(PPS))                         :: ZFLXC_MOM            ! Effective drag coefficient for momentum [kg/(m2 s)]
REAL, DIMENSION(SIZE(PPS))                         :: ZQSATG               ! saturation specific humidity for PEK%XTG (ground surface: kg kg-1)
REAL, DIMENSION(SIZE(PPS))                         :: ZQSATV               ! saturation specific humidity for PEK%XTV (vegetation canopy: kg kg-1)
REAL, DIMENSION(SIZE(PPS))                         :: ZQSATC               ! saturation specific humidity for PEK%XTC (canopy air: kg kg-1)
REAL, DIMENSION(SIZE(PPS))                         :: ZQSATN               ! saturation specific humidity for DMK%XSNOWTEMP (snow surface: kg kg-1)
REAL, DIMENSION(SIZE(PPS))                         :: ZDELTAVK             ! canopy interception capacity fraction including K-factor (-)
REAL, DIMENSION(SIZE(PPS))                         :: ZCHEATV              ! Vegetation canopy *effective surface* heat capacity    (J m-2 K-1)
REAL, DIMENSION(SIZE(PPS))                         :: ZCHEATG              ! Understory-ground *effective surface* heat capacity    (J m-2 K-1)
REAL, DIMENSION(SIZE(PPS))                         :: ZCHEATN              ! Ground-based snow *effective surface* heat capacity    (J m-2 K-1)
REAL, DIMENSION(SIZE(PPS))                         :: ZHVGS                ! Dimensionless pseudo humidity factor for computing
!                                                                          !  vapor fluxes from the non-buried part of the canopy
!                                                                          !  to the canopy air                                     (-)
REAL, DIMENSION(SIZE(PPS))                         :: ZHVNS                ! Dimensionless pseudo humidity factor for computing
!                                                                          !  vapor fluxes from the partly-buried part of the canopy
!                                                                          !  to the canopy air                                     (-)
REAL, DIMENSION(SIZE(PPS))                         :: ZDQSAT_G             ! saturation specific humidity derivative for understory (kg kg-1 K-1)
REAL, DIMENSION(SIZE(PPS))                         :: ZDQSAT_V             ! saturation specific humidity derivative for the
!                                                                          !  vegetation canopy                                     (kg kg-1 K-1)
REAL, DIMENSION(SIZE(PPS))                         :: ZDQSATI_N            ! saturation specific humidity derivative over ice for
!                                                                          !  the ground-based snowpack                             (kg kg-1 K-1)
REAL, DIMENSION(SIZE(PPS))                         :: ZDELTAT_G            ! Time change in soil surface temperature                (K)
REAL, DIMENSION(SIZE(PPS))                         :: ZDELTAT_V            ! Time change in vegetation canopy temperature           (K)
REAL, DIMENSION(SIZE(PPS))                         :: ZDELTAT_N            ! Time change in snowpack surface temperature            (K)
REAL, DIMENSION(SIZE(PPS))                         :: ZRNET_V              ! Net vegetation canopy radiation                        (W/m2)
REAL, DIMENSION(SIZE(PPS))                         :: ZRNET_G              ! Net understory-ground radiation                        (W/m2)
REAL, DIMENSION(SIZE(PPS))                         :: ZFLXC_C_A_F          ! Exchange coefficient between the snow on the ground and
!                                                                          !  atmosphere modified by a partially to fully buried
!                                                                          !  vegetation canopy                                     [kg/(m2 s)]
REAL, DIMENSION(SIZE(PPS))                         :: ZFLXC_N_A_F          ! Exchange coefficient between vegetation canopy air and
!                                                                          !  atmosphere modified by a partially to fully buried
!                                                                          !  vegetation canopy                                     [kg/(m2 s)]
REAL, DIMENSION(SIZE(PPS))                         :: ZEVAP_C_A            ! Total canopy evapotranspiration and sublimation
!                                                                          !  of intercepted snow                                    (kg/m2/s)
REAL, DIMENSION(SIZE(PPS))                         :: ZEVAP_N_A            ! Vapor flux from the ground-based snowpack (part burying
!                                                                          !  the canopy vegetation) to the atmosphere              (kg/m2/s)
REAL, DIMENSION(SIZE(PPS))                         :: ZH_N_A               ! Sensible heat flux from the ground-based snowpack (part
!                                                                          !  burying the canopy vegetation) to the atmosphere      (W/m2)
REAL, DIMENSION(SIZE(PPS))                         :: ZVEGFACT             ! Fraction of canopy vegetation possibly receiving
!                                                                          !  rainfall                                              (-)
REAL, DIMENSION(SIZE(PPS))                         :: ZRRSFC               ! The sum of all non-intercepted rain and canopy drip    (kg/m2/s)
REAL, DIMENSION(SIZE(PPS))                         :: ZRRSFCL              ! The sum of all non-intercepted rain and drip from litter (kg/m2/s)
REAL, DIMENSION(SIZE(PPS))                         :: ZLES3L               ! latent heat flux - sublimation of ice from the ground
!                                                                          !  based snowpack (W/m2)
REAL, DIMENSION(SIZE(PPS))                         :: ZLEL3L               ! latent heat flux - evaporation of liquid water from the
!                                                                          !  ground based snowpack (W/m2))
REAL, DIMENSION(SIZE(PPS))                         :: ZEVAP3L              ! total mass loss via evap & sublm from the ground based snowpack (kg/m2/s)
REAL, DIMENSION(SIZE(PPS))                         :: ZUSTAR2_IC           ! friction velocity (possibly implicitly coupled) (m/s)
REAL, DIMENSION(SIZE(PPS))                         :: ZTA_IC               ! atmospheric temperature (possibly implicitly coupled) (m/s)
REAL, DIMENSION(SIZE(PPS))                         :: ZQA_IC               ! atmospheric specific humidity (possibly implicitly coupled) (m/s)
REAL, DIMENSION(SIZE(PPS))                         :: ZSWUP                ! net upwelling shortwave radiation [W/m2]
REAL, DIMENSION(SIZE(PPS))                         :: ZLWUP                ! net upwelling longwave radiation [W/m2]
REAL, DIMENSION(SIZE(PPS))                         :: ZUSTAR2SNOW          ! snow fraciton velocity squared (m2/s2)
REAL, DIMENSION(SIZE(PPS))                         :: ZVMOD                ! lowest level atmospheric wind speed update estimate (K)
REAL, DIMENSION(SIZE(PPS))                         :: ZRR                  ! combined rain rate (above canopy) and irrigation need (kg/m2/s)
REAL, DIMENSION(SIZE(PPS))                         :: ZFLSN_COR            ! snow/soil-biomass correction flux (W/m2) (not MEB)
REAL, DIMENSION(SIZE(PPS))                         :: ZWSFC                ! surface liquid water content for resistances  (m3/m3)
REAL, DIMENSION(SIZE(PPS))                         :: ZWISFC               ! surface frozen water content for resistances  (m3/m3)
REAL, DIMENSION(SIZE(PPS))                         :: ZLESFC               ! evaporation from the surface (soil or litter) (W/m2)
REAL, DIMENSION(SIZE(PPS))                         :: ZLESFCI              ! sublimation from the surface (soil or litter) (W/m2)
REAL, DIMENSION(SIZE(PPS))                         :: ZPERMSNOWFRAC        ! fraction of permanent snow/ice
!
! - TR_ML radiation option: NOTE...always used by MEB
!
REAL, DIMENSION(SIZE(PPS),SIZE(PABC))              :: ZIACAN_SUNLIT        ! Absorbed PAR of each level within the
REAL, DIMENSION(SIZE(PPS),SIZE(PABC))              :: ZIACAN_SHADE         !    canopy - Split into SHADEd and SUNLIT
REAL, DIMENSION(SIZE(PPS),SIZE(PABC))              :: ZFRAC_SUN            !    fraction of sunlit leaves
!
REAL, DIMENSION(SIZE(PPS))                         :: ZLAI                 ! Potentially covered/buried canopy LAI (m2/m2)
REAL, DIMENSION(SIZE(PPS))                         :: ZALBVIS_TSOIL        ! average snow-free ground VIS albedo (soil plus flooded fraction)
REAL, DIMENSION(SIZE(PPS))                         :: ZALBNIR_TSOIL        ! average snow-free ground NIR albedo (soil plus flooded fraction)
REAL, DIMENSION(SIZE(PPS))                         :: ZSWNET_S             ! Net SW radiation at the surface (below canopy snow/ground/flooded zone)
REAL, DIMENSION(SIZE(PPS))                         :: ZLTT                 ! Average latent heat (normalization factor) (J/kg)
REAL, DIMENSION(SIZE(PPS))                         :: ZLSTTC               ! Working coefficient to compute ZLTT: frozen part (-)
REAL, DIMENSION(SIZE(PPS))                         :: ZLVTTC               ! Working coefficient to compute ZLTT: non-frozen part (-)
REAL, DIMENSION(SIZE(PPS))                         :: ZZREF
REAL, DIMENSION(SIZE(PPS))                         :: ZUREF
!
!
! - CPHOTO/=NON (Ags Option(s)):
!
REAL, DIMENSION(SIZE(PPS))                         :: ZQSAT                ! CPHOTO/=NON (Ags Option(s))diagnosed (past time step) Qsat relative to canopy (for Ags)
REAL, DIMENSION(SIZE(PPS))                         :: ZFFV                 ! submerged vegetation (by flooding) fraction (-)
REAL, DIMENSION(SIZE(PPS),SIZE(PABC))              :: ZIACAN               ! PAR in the canopy at different gauss levels: local working needed if
!                                                                          ! Ags if off (i.e. CPHOTO==NON)
!
REAL, DIMENSION(:,:), ALLOCATABLE                  :: ZTGL                 ! Temporary temperature of litter + soil
REAL, DIMENSION(:,:), ALLOCATABLE                  :: ZSOILHCAPZ           ! Temporary heat capacity of litter + soil
REAL, DIMENSION(:,:), ALLOCATABLE                  :: ZSOILCONDZ           ! Temporary heat conductivity of litter + soil
REAL, DIMENSION(:,:), ALLOCATABLE                  :: ZD_G                 ! Temporary depth of bottom litter + soil layers
REAL, DIMENSION(:,:), ALLOCATABLE                  :: ZDZG                 ! Temporary thickness of litter + soil layers
REAL, DIMENSION(:,:), ALLOCATABLE                  :: ZWFC                 ! Temporary Wfc of bottom litter + soil layers
REAL, DIMENSION(:,:), ALLOCATABLE                  :: ZWSAT                ! Temporary Wsat of bottom litter + soil layers
!
! Working sums for flux averaging over MEB time split
!
REAL, DIMENSION(SIZE(PPS))   :: ZH_SUM, ZH_CA_SUM, ZH_N_A_SUM, ZH_CV_SUM, ZH_GV_SUM, &
                                ZH_GN_SUM, ZHSNOW_SUM, ZHPSNOW_SUM
REAL, DIMENSION(SIZE(PPS))   :: ZHU_AGG_SUM, ZAC_AGG_SUM

REAL, DIMENSION(SIZE(PPS))   :: ZLE_SUM, ZLE_CA_SUM, ZLE_CV_SUM, ZLE_GV_SUM,              &
                                ZLE_GN_SUM, ZLETR_CV_SUM, ZLEG_SUM,ZLEGI_SUM,ZLESFC_SUM,  &
                                ZLESFCI_SUM, ZLER_CV_SUM, ZLE_FLOOD_SUM, ZLEI_FLOOD_SUM,  &
                                ZLES_CV_SUM, ZLETR_SUM, ZLER_SUM, ZLEV_SUM,               &
                                ZLEI_SUM, ZLES3L_SUM, ZLEL3L_SUM, ZEVAP3L_SUM,            &
                                ZUSTAR2_SUM, ZUSTAR2SNOW_SUM, ZCDSNOW_SUM,                &
                                ZCHSNOW_SUM, ZRISNOW_SUM, ZEVAP_SUM

REAL, DIMENSION(SIZE(PPS))   :: ZGRNDFLUX_SUM, ZRESTORE_SUM

REAL, DIMENSION(SIZE(PPS))   :: ZSWNET_V_SUM, ZSWNET_G_SUM, ZSWNET_N_SUM, ZLWNET_V_SUM, &
                                ZLWNET_G_SUM, ZLWNET_N_SUM, ZEMIST_SUM, ZSWUP_SUM,      &
                                ZLWUP_SUM
REAL, DIMENSION(SIZE(PPS))   :: ZDELHEATG_SFC_SUM, ZDELHEATV_SFC_SUM, ZDELHEATG_SUM
!
REAL, DIMENSION(SIZE(PPS))   :: ZALBV, ZALBG, ZTR
!
REAL, DIMENSION(SIZE(PEK%XWR,1))         :: ZPHASEL  ! Phase changement in litter (W/m2)
REAL, DIMENSION(SIZE(PEK%XWR,1))         :: ZCTSFC
REAL, DIMENSION(SIZE(PEK%XWR,1))         :: ZFROZEN1SFC
!
INTEGER :: INJ, INL, JJ, JL
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*      1.0    Preliminaries
!              -------------
!
IF (LHOOK) CALL DR_HOOK('ISBA_MEB',0,ZHOOK_HANDLE)
!
PIACAN(:,:) = 0.0
!
DMK%XFAPAR(:)    = 0.0
DMK%XFAPIR(:)    = 0.0
DMK%XFAPAR_BS(:) = 0.0
DMK%XFAPIR_BS(:) = 0.0
!
DEK%XRRLIT(:)   = 0.0
DEK%XDRIPLIT(:) = 0.0
!
DEK%XLEGI(:)    = 0.0
DEK%XLEG(:)     = 0.0
!
ZLESFCI(:) = 0.0
ZLESFC(:)  = 0.0
!
ZIACAN_SUNLIT(:,:) = XUNDEF
ZIACAN_SHADE(:,:)  = XUNDEF
ZFRAC_SUN(:,:)     = XUNDEF
ZLAI(:)            = XUNDEF
ZALBVIS_TSOIL(:)   = XUNDEF
ZALBNIR_TSOIL(:)   = XUNDEF
ZSWNET_S(:)        = XUNDEF
ZQSAT(:)           = XUNDEF
!
ZWORK(:)           = XUNDEF
ZWORK2(:)          = XUNDEF
ZWORK3(:)          = XUNDEF
ZWORK4(:)          = XUNDEF
!
ZTR(:)             = 0.0
!
!*      1.1    Preliminaries for litter parameters
!              -----------------------------------
!
INJ=SIZE(PEK%XWG,1)
INL=SIZE(PEK%XWG,2)
!
 CALL ALLOCATE_LOCAL_VARS_PREP_GRID_SOIL
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!*      1.2    Preliminaries for litter temperature
!              ------------------------------------
!
! Concatenate PEK%XTL and PEK%XTG and the parameters linked to heat transfer into the soil
!

CALL PREP_MEB_SOIL(IO%LMEB_LITTER, PSOILHCAPZ, PSOILCONDZ, KK, PK, PEK, &
                   ZD_G, ZDZG,ZTGL, ZSOILHCAPZ, ZSOILCONDZ, ZWSAT, ZWFC,&
                   ZWSFC, ZWISFC, ZCTSFC, DMK%XCT, PFROZEN1, ZFROZEN1SFC )
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!*      2.0    Preliminaries for energy and radiation budget
!              ---------------------------------------------
!
ZPERMSNOWFRAC(:) = PK%XVEGTYPE_PATCH(:,NVT_SNOW)
!
! Local working:
! - possibly adjust these prognostic variables locally, but do not save
!
ZSNOWRHO(:,:)    = PEK%TSNOW%RHO (:,:)
ZSNOWAGE(:,:)    = PEK%TSNOW%AGE (:,:)
ZSNOWSWE(:,:)    = PEK%TSNOW%WSNOW(:,:)
!
CALL PREPS_FOR_MEB_EBUD_RAD(PPS, PEK%XLAI, ZSNOWRHO, ZSNOWSWE, PEK%TSNOW%HEAT, ZSNOWLIQ, &
                            DMK%XSNOWTEMP, DMK%XSNOWDZ, ZSNOWCOND, ZSNOWHCAP,  &
                            PEK%TSNOW%EMIS, ZSIGMA_F, ZCHIP, PTSTEP, PSR, PTA, &
                            PVMOD, ZSNOWAGE, ZPERMSNOWFRAC  )
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!*      3.0    Shortwave radiative transfer
!              ----------------------------
!
! Calculate snow albedo: split into spectral bands:
!
CALL SNOWALB_SPECTRAL_BANDS_MEB(PK%XVEGTYPE_PATCH, PEK, ZSNOWRHO, ZSNOWAGE, PPS,&
                                DMK%XSNOWDZ,PZENITH, ZTAU_N)
!
!
! NOTE, currently MEB only uses 2 of 3 potential snow albedo spectral bands
!
!
WHERE(PEK%TSNOW%ALB(:) /= XUNDEF)
   ZLAI(:)          = PEK%XLAI(:)*(1.0-PPALPHAN(:))
   ZALBVIS_TSOIL(:) = PALBVIS_TSOIL(:)*(1.-PEK%XPSN(:)) + PEK%XPSN(:)*PEK%TSNOW%ALBVIS(:)
   ZALBNIR_TSOIL(:) = PALBNIR_TSOIL(:)*(1.-PEK%XPSN(:)) + PEK%XPSN(:)*PEK%TSNOW%ALBNIR(:)
ELSEWHERE
   ZLAI(:)          = PEK%XLAI(:)
   ZALBVIS_TSOIL(:) = PALBVIS_TSOIL(:)
   ZALBNIR_TSOIL(:) = PALBNIR_TSOIL(:)
END WHERE
!
 CALL RADIATIVE_TRANSFERT(IO%LAGRI_TO_GRASS, PK%XVEGTYPE_PATCH,                      &
                          PALBVIS_TVEG, ZALBVIS_TSOIL, PALBNIR_TVEG, ZALBNIR_TSOIL,  &
                          PSW_RAD, ZLAI, PZENITH, PABC, PEK%XFAPARC, PEK%XFAPIRC,    &
                          PEK%XMUS, PEK%XLAI_EFFC, OSHADE, ZIACAN,  ZIACAN_SUNLIT,   &
                          ZIACAN_SHADE, ZFRAC_SUN, DMK%XFAPAR, DMK%XFAPIR,           &
                          DMK%XFAPAR_BS, DMK%XFAPIR_BS )

! Compute all-wavelength effective ground (soil+snow) surface,
! soil and veg albedos, respectively:

ZALBS(:)      = XSW_WGHT_NIR*ZALBNIR_TSOIL(:) + XSW_WGHT_VIS*ZALBVIS_TSOIL(:)

ZALBG(:)      = XSW_WGHT_NIR*PALBNIR_TSOIL(:) + XSW_WGHT_VIS*PALBVIS_TSOIL(:)

ZALBV(:)      = XSW_WGHT_NIR*PALBNIR_TVEG(:)  + XSW_WGHT_VIS*PALBVIS_TVEG(:)
!
WHERE(PSW_RAD(:) > ZSWRAD_MIN) ! Sun is up...approx
!
! Total effective surface (canopy, ground/flooded zone, snow) all-wavelength
! albedo: diagnosed from shortwave energy budget closure

  DK%XALBT(:)      = 1. - (XSW_WGHT_VIS*(DMK%XFAPAR(:)+DMK%XFAPAR_BS(:)) +   &
                           XSW_WGHT_NIR*(DMK%XFAPIR(:)+DMK%XFAPIR_BS(:)))
  ZSWUP   (:)      = PSW_RAD(:)*DK%XALBT(:)
  DK%XALBT(:)      = ZSWUP(:)/MAX(1.E-5, PSW_RAD(:))

! Diagnose all-wavelength SW radiative budget
! for the canopy and below canopy (surface) components;

  DEK%XSWNET_V(:) = PSW_RAD(:)*(XSW_WGHT_VIS*DMK%XFAPAR   (:) + XSW_WGHT_NIR*DMK%XFAPIR   (:))
  ZSWNET_S(:)     = PSW_RAD(:)*(XSW_WGHT_VIS*DMK%XFAPAR_BS(:) + XSW_WGHT_NIR*DMK%XFAPIR_BS(:))
!
! Compute total all wavelength SW transmission:
! A solution of a quadradic equation based on ground energy budget Eq in FAPAIR.F90
! Tr = [ -b - sqrt(b^2 - 4ac) ]/(2a)   (this is good root for this computation)
! Here we derrive Eq so that a=1

   ZWORK4(:)     = ZALBS(:)*ZALBV(:)
   ZWORK2(:)     = -(1.-ZALBS(:)*(1.-ZALBV(:)))/ZWORK4(:)          ! b
   ZWORK3(:)     = ZSWNET_S(:)/(PSW_RAD(:)*ZWORK4(:))              ! c
   ZWORK(:)      = SQRT(MAX(0.0, ZWORK2(:)**2 - 4*ZWORK3(:)))      ! sqrt(b**2 - 4c)
   ZTR(:)        = 0.5*(-ZWORK2(:) - ZWORK(:))                     ! -b - sqrt(b^2 - 4c) ]/2
   ZTR(:)        = MIN(1.,MAX(0., ZTR(:) ))
!
! Downwelling SW radiation arriving at ground/snow surface

   DEK%XSWDOWN_GN(:) = PSW_RAD(:)*ZTR(:)

! Get snow and ground components:

   DEK%XSWNET_G(:)   = (1.-PEK%XPSN(:))*DEK%XSWDOWN_GN(:)*(1.-ZALBG(:)+ZALBS(:)*(1.-ZTR(:))*ZALBV(:))
   DEK%XSWNET_N(:)   = ZSWNET_S(:)-DEK%XSWNET_G(:) ! conservative computation

! Quantity of net shortwave radiation absorbed in surface snow layer

  DEK%XSWNET_NS(:)  = DEK%XSWNET_N(:)*(1.0 - ZTAU_N(:,1))
!
! Any SW radiation reaching the base of the lowest snow layer can pass
! into the soil:

   ZTAU_N(:,SIZE(PEK%TSNOW%WSNOW,2)) = ZTAU_N(:,SIZE(PEK%TSNOW%WSNOW,2))*(1.-ZALBG(:))
!
ELSEWHERE

! Sun is down: (below threshold)
! radiation amounts quite small, so make a simple approximation here:

   DK%XALBT(:)                = ZALBV(:)
   ZSWUP(:)                   = DK%XALBT(:)*PSW_RAD(:)

   DEK%XSWDOWN_GN(:)          = 0.
   DEK%XSWNET_G(:)            = 0.
   DEK%XSWNET_V(:)            = (1.-DK%XALBT(:))*PSW_RAD(:)
   DEK%XSWNET_N(:)            = 0.
   DEK%XSWNET_NS(:)           = 0.
   ZTAU_N(:,SIZE(PEK%TSNOW%WSNOW,2)) = 0.

END WHERE
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!*      4.0    Longwave radiative transfer
!              ---------------------------
!
CALL ISBA_LWNET_MEB(PEK%XLAI, PEK%XPSN, PPALPHAN,PEK%TSNOW%EMIS, KK%XEMISF, KK%XFF,          &
                    PEK%XTV, ZTGL(:,1), DMK%XSNOWTEMP(:,1), PLW_RAD, DEK%XLWNET_N,           &
                    DEK%XLWNET_V, DEK%XLWNET_G, ZDLWNET_V_DTV, ZDLWNET_V_DTG, ZDLWNET_V_DTN, &
                    ZDLWNET_G_DTV, ZDLWNET_G_DTG, ZDLWNET_G_DTN, ZDLWNET_N_DTV,              &
                    ZDLWNET_N_DTG, ZDLWNET_N_DTN, ZSIGMA_F, ZSIGMA_FN, DEK%XLWDOWN_GN )
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!*      5.0    Fraction of leaves occupied by intercepted water
!              ------------------------------------------------
!
! Vegetation canopy:
!
! First, compute an effective veg fraction: it can only be < unity if vegetation is buried by snowpack...
!
ZWORK(:) = (1.0 - PEK%XPSN(:) + PEK%XPSN(:)*(1.0 - PPALPHAN(:)))
!
CALL WET_LEAVES_FRAC(PEK%XWR(:), ZWORK, PEK%XWRMAX_CF(:), PZ0_MEBV, PEK%XLAI(:), ZWRMAX, ZDELTA)
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!*      6.0    Plant stress, stomatal resistance and, possibly, CO2 assimilatio
!              --------------------------------------------------------------------
!
!              MEB-NOTE here assumed IO%CPHOTO=='DEF' or 'AST' for now
!              More Ags options to be added later
!
IF (IO%CPHOTO=='NON') THEN
!
! Canopy vegetation (no snow, or snow below the main part of the canopy):
!
   CALL VEG(PSW_RAD, PEK%XTC(:), PEK%XQC(:), PPS, PEK%XRGL(:), PEK%XLAI(:), &
            PEK%XRSMIN(:), PEK%XGAMMA(:), PF2, ZRS)
!
!
ELSE IF (MAXVAL(PEK%XGMES) /= XUNDEF .OR. MINVAL(PEK%XGMES) /= XUNDEF) THEN
!
! NOTE: For now we assume that forest canopy can be flooded.
! However, we need to likely compute a fraction like PALPHAN (for snow vertical extent)
! for floods for grasses/crops/shrubs...i.e. low vegetation

   ZFFV(:)  = 0.0

   ZQSAT(:) = QSAT(PEK%XTV(:),PPS)

   ZWORK(:) = PEK%XLE(:) ! passed in as LE: Since Qc corresponds to the effective
   PEK%XLE(:) = 0.       ! surface specific humidity in ISBA-MEB,
                         ! so no need for LE correction (only required for composite ISBA)
   CALL COTWORES(PTSTEP, IO, OSHADE,  PK, PEK, PK%XDMAX, PPOI, PCSP, PEK%XTV, &
                 PF2, PSW_RAD, PEK%XQC, ZQSAT, PPALPHAN, ZDELTA, PRHOA,       &
                 PZENITH, ZFFV, ZIACAN_SUNLIT, ZIACAN_SHADE, ZFRAC_SUN,       &
                 ZIACAN, PABC, ZRS, DEK%XGPP, PRESP_BIOMASS_INST(:,1))
   PEK%XLE(:) = ZWORK(:)
!
   PIACAN(:,:) = ZIACAN(:,:)
!
ELSE
   PRESP_BIOMASS_INST(:,1) = 0.0
   DEK%XGPP(:)             = 0.0
ENDIF
!
! Additional resistance for possibly snow-buried canopy vegetation:
!
ZRSN(:) = ZRS(:)/( 1.0 - MIN(PPALPHAN(:), 1.0 - (ZRS(:)/XRS_MAX)) )
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!*      6.0    Canopy snow (intercepted) needed diagnostics:
!              ---------------------------------------------
!
CALL SNOW_LEAVES_FRAC_MEB(PEK%XPSN, PPALPHAN, PEK%XWRVN, PEK%XTV, ZCHIP, &
                          PEK%XLAI, ZWRVNMAX, ZPSNCV, ZMELTVN)
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!*      7.0    Aerodynamic drag and heat/mass transfer/fluxes
!              and energy budget solution
!              ----------------------------------------------
!
! NOTE, this assumes thermodynamic variable herein is potential T

ZPET_A_COEF(:) = -PPET_A_COEF(:)*XCPD
ZPET_B_COEF(:) =  PPET_B_COEF(:)*XCPD
ZTHRMA_TA(:)   =  XCPD/PEXNA(:)
ZTHRMB_TA(:)   =  0.0
ZWORK(:)       =  XCPD/PEXNS(:)
ZTHRMA_TC(:)   =  ZWORK(:)
ZTHRMB_TC(:)   =  0.0
ZTHRMA_TN(:)   =  ZWORK(:)
ZTHRMB_TN(:)   =  0.0
ZTHRMA_TG(:)   =  ZWORK(:)
ZTHRMB_TG(:)   =  0.0
ZTHRMA_TV(:)   =  ZWORK(:)
ZTHRMB_TV(:)   =  0.0
!
!
! For turbulence computations:
! Adjust (shift upward) local reference heights "seen by the turbulence scheme"
! if they are below the canopy:
! NOTE, this approach is an alternative to LFORC_MEASURE=F, which shifts
! vegetation downward by the displacement height. Both approaches essentially
! conceptually assume that the vegetation is part of the terrain.
! Also, here, conserve any UREF and ZREF differences.
!
ZZREF(:)       = PZREF(:)
ZUREF(:)       = PUREF(:)
IF(IO%LFORC_MEASURE)THEN
   WHERE(PZREF(:) - PEK%XH_VEG(:) < XLIMH)
      ZZREF(:) = PEK%XH_VEG(:) + XLIMH
      ZUREF(:) = PEK%XH_VEG(:) + XLIMH + MAX(0.,PUREF(:)-PZREF(:))
   END WHERE
ENDIF
!
! Compute the average latent heat (normalization factor) (J kg-1):
! NOTE that we could use a function which depends on the different resistances,
! but this can make the average latent heat relatively noisy(leading to a slightly less
! tightly closed budgets owing to numerical noise): here we opt for a more
! temporally smooth approximation based on fractional coverage:
!
ZLVTTC(:) = ( ZSIGMA_F(:)*(1.-ZPSNCV(:)) + (1.0-PEK%XPSN(:)-KK%XFF(:))*(1.0-ZFROZEN1SFC(:))           )* &
            (1.0 - PEK%XPSN(:)*PPALPHAN(:))
ZLSTTC(:) = ( ZSIGMA_F(:)*    ZPSNCV(:)  + (1.0-PEK%XPSN(:)-KK%XFF(:))*     ZFROZEN1SFC(:)  + PEK%XPSN(:) )* &
            (1.0 - PEK%XPSN(:)*PPALPHAN(:)) + PEK%XPSN(:)*PPALPHAN(:)
ZLTT(:)   = (ZLVTTC(:)*XLVTT + ZLSTTC(:)*XLSTT)/MAX(1.E-12, ZLVTTC(:) + ZLSTTC(:))
ZLTT(:)   = MIN(XLSTT, MAX(ZLTT(:), XLVTT)) ! numerical check
!
! Possibly split time step if large:
! Although the energy budget is fully implicit, a very small canopy heat capacity
! (and neglect of canopy air space heat capacity) can possibly lead to
! numerical shocks, especially during transition periods between stable and unstable
! regimes. Thus, for relatively large steps, a simple time split scheme is activated.
! Note that soil moisture is held constant, while turbulent exchange coefficients are updated during the split.
! Also, experience shows that splitting at least once for moderately sized time steps
! is quite effective in removing any lingering small but possible oscillations.
! Finally, for *very* small time steps (such as those for high res runs), no split is performed.
! Fluxes are averaged over the time split for conservation.
!
JTSPLIT_EB = 1 + INT(PTSTEP/ZTSTEP_EB)  ! number of split-time steps
ZTSTEP     = PTSTEP/JTSPLIT_EB          ! split time step...for relatively small time steps, no split
!
! initialize time split sums for fluxes:
!
CALL INIT_SUM_FLUXES_MEB_TSPLIT
!
!
! Note, when implicitly coupled to the atmosphere, these
! 3 variables will evolve during the split...we used updated
! values for turbulent exchange computations (drag_meb).
! NOTE...when explicit coupling used, these 3 variables do NOT vary
! during the split.
!
ZVMOD(:)  = PVMOD(:)
ZTA_IC(:) = PTA(:)
ZQA_IC(:) = PQA(:)
!
!
LOOP_TIME_SPLIT_EB: DO JDT=1,JTSPLIT_EB
!*      7.1    Aerodynamic drag and heat transfer coefficients
!              -----------------------------------------------
!
   CALL DRAG_MEB(IO, PEK, DMK, DK,  &
                 ZTGL(:,1), ZTA_IC,  ZQA_IC, ZVMOD, ZWSFC, ZWISFC,   &
                 ZWSAT(:,1), ZWFC(:,1), PEXNS, PEXNA, PPS, PRR, PSR, &
                 PRHOA, PZ0G_WITHOUT_SNOW, PZ0_MEBV, PZ0H_MEBV,      &
                 PZ0EFF_MEBV, PZ0_MEBN, PZ0H_MEBN, PZ0EFF_MEBN,      &
                 ZSNOWSWE(:,1), ZCHIP, ZTSTEP, ZRS, ZRSN, PPALPHAN,  &
                 ZZREF, ZUREF, PDIRCOSZW, ZPSNCV, ZDELTA, ZVELC,     &
                 PRISNOW, ZUSTAR2SNOW, ZHUGI, ZHVG,                  &
                 ZHVN, ZLEG_DELTA, ZLEGI_DELTA, ZHSGL, ZHSGF,        &
                 ZFLXC_CA, ZFLXC_N_A, ZFLXC_GV, ZFLXC_GN,            &
                 ZFLXC_VG_C, ZFLXC_VN_C, ZFLXC_MOM, ZQSATG, ZQSATV,  &
                 ZQSATC, ZQSATN, ZDELTAVK )
!
   ZKVN(:) = SNOW_INTERCEPT_EFF(ZCHIP,ZVELC,ZWRVNMAX)

!*      7.2    Resolution of the surface energy budgets
!              ----------------------------------------
!
   CALL E_BUDGET_MEB(IO, KK, PK, PEK, DK, DEK, DMK,  &
                     ZTSTEP, ZLTT, PPS, ZCTSFC, PTDEEP_A, ZD_G, ZSOILCONDZ, ZSOILHCAPZ,&
                     ZSNOWRHO, ZSNOWCOND, ZSNOWHCAP, ZTAU_N, ZDLWNET_V_DTV, ZDLWNET_V_DTG, &
                     ZDLWNET_V_DTN, ZDLWNET_G_DTV, ZDLWNET_G_DTG, ZDLWNET_G_DTN, &
                     ZDLWNET_N_DTV, ZDLWNET_N_DTG, ZDLWNET_N_DTN, PPEW_A_COEF,   &
                     PPEW_B_COEF, ZPET_A_COEF, PPEQ_A_COEF, ZPET_B_COEF,         &
                     PPEQ_B_COEF, ZTHRMA_TA, ZTHRMB_TA, ZTHRMA_TC, ZTHRMB_TC,    &
                     ZTHRMA_TG, ZTHRMB_TG, ZTHRMA_TV, ZTHRMB_TV, ZTHRMA_TN,      &
                     ZTHRMB_TN, ZQSATG, ZQSATV, ZQSATN, PPALPHAN, ZPSNCV,        &
                     ZCHEATV, ZCHEATG, ZCHEATN, ZLEG_DELTA, ZLEGI_DELTA, ZHUGI,  &
                     ZHVG, ZHVN, ZFROZEN1SFC, ZFLXC_CA, ZFLXC_GV, ZFLXC_VG_C,    &
                     ZFLXC_VN_C, ZFLXC_GN, ZFLXC_N_A, ZFLXC_MOM, ZTGL, ZSNOWLIQ, &
                     ZFLXC_CV, ZHVGS, ZHVNS, ZDQSAT_G,ZDQSAT_V,ZDQSATI_N,        &
                     ZTA_IC, ZQA_IC, ZUSTAR2_IC, ZVMOD, ZDELTAT_G, ZDELTAT_V,    &
                     ZDELTAT_N, PGRNDFLUX, PDEEP_FLUX, PDELHEATV_SFC,            &
                     PDELHEATG_SFC, PDELHEATG                              )
!
!*      7.3    Energy and momentum fluxes and radiative temperature and emissivity
!              -------------------------------------------------------------------
!
   CALL ISBA_FLUXES_MEB(KK, PK, PEK, DK, DEK, DMK, PRHOA, ZLTT, ZSIGMA_F, ZSIGMA_FN, &
                        ZRNET_V, ZRNET_G, ZDLWNET_V_DTV, ZDLWNET_V_DTG,          &
                        ZDLWNET_V_DTN, ZDLWNET_G_DTV, ZDLWNET_G_DTG,             &
                        ZDLWNET_G_DTN, ZDLWNET_N_DTV, ZDLWNET_N_DTG,             &
                        ZDLWNET_N_DTN, ZTHRMA_TA, ZTHRMB_TA, ZTHRMA_TC,          &
                        ZTHRMB_TC, ZTHRMA_TG, ZTHRMB_TG, ZTHRMA_TV, ZTHRMB_TV,   &
                        ZTHRMA_TN, ZTHRMB_TN,  ZQSATG, ZQSATV, ZQSATN, PPALPHAN, &
                        ZPSNCV, ZFROZEN1SFC, ZLEG_DELTA, ZLEGI_DELTA, ZHUGI,     &
                        ZHVG, ZHVN, ZFLXC_CA, ZFLXC_GV, ZFLXC_VG_C, ZFLXC_VN_C,  &
                        ZFLXC_GN, ZFLXC_N_A, ZFLXC_MOM, ZFLXC_CV, ZHVGS,         &
                        ZHVNS, ZTGL, ZDQSAT_G, ZDQSAT_V, ZDQSATI_N, ZTA_IC,      &
                        ZQA_IC, ZDELTAVK, ZDELTAT_G, ZDELTAT_V, ZDELTAT_N,       &
                        ZSWUP, PSW_RAD, PLW_RAD, ZLWUP, ZH_N_A, ZEVAP_C_A,       &
                        ZEVAP_N_A, ZLESFC, ZLESFCI, ZLES3L, ZLEL3L, ZEVAP3L,     &
                        PEMIST                                   )
!
! Compute aggregated coefficients for evaporation
! Sum(LEC+LES+LEL) = ACagg * Lv * RHOA * (HUagg.Qsat - Qa)
!
   ZFLXC_C_A_F(:) = ZFLXC_CA (:)*(1.0-PEK%XPSN(:)*PPALPHAN(:))
   ZFLXC_N_A_F(:) = ZFLXC_N_A(:)*     PEK%XPSN(:)*PPALPHAN(:)

   PHU_AGG(:)     = (ZFLXC_C_A_F(:)*PEK%XQC(:)+ ZFLXC_N_A_F(:)*ZQSATN(:))/   &
                    (ZFLXC_C_A_F(:)*ZQSATC(:) + ZFLXC_N_A_F(:)*ZQSATN(:))

   PAC_AGG(:)     = ZFLXC_C_A_F(:) + ZFLXC_N_A_F(:) ! kg/m2/s
!
! Sum fluxes over time split:

   CALL SUM_FLUXES_MEB_TSPLIT

ENDDO LOOP_TIME_SPLIT_EB
!
CALL AVG_FLUXES_MEB_TSPLIT     ! average fluxes over time split
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!*     8.0    Snow explicit canopy loading/interception
!             ------------------------------------------
!
CALL SNOW_LOAD_MEB(PK, PEK, DEK, PTSTEP, PSR, ZWRVNMAX, ZKVN, ZCHEATV, ZMELTVN, &
                   ZVELC, PSUBVCOR)
!

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!*     9.0    Snow explicit canopy loading/interception
!             ------------------------------------------
!
ZRR(:)         = PRR(:)
DEK%XIRRIG_FLUX(:) = 0.0
!
!* add irrigation over vegetation to liquid precipitation (rr)
!  Water "need" treated as if sprayed from above (over vegetation and soil):
!
IF (SIZE(AG%LIRRIGATE,1)>0) THEN
  WHERE (AG%LIRRIGATE(:) .AND. PEK%XIRRIG(:)>0. .AND. PEK%XIRRIG(:) /= XUNDEF .AND. (PF2(:)<AG%XTHRESHOLDSPT(:)) )
    DEK%XIRRIG_FLUX(:) = PEK%XWATSUP(:) / XDAY
    ZRR            (:) = PRR(:) + PEK%XWATSUP(:)/XDAY
    AG%LIRRIDAY    (:) = .TRUE.
  END WHERE
ENDIF
!
! Call canopy interception...here because meltwater should be allowed to fall
! on understory snowpack
!
! Fraction of canopy vegetation possibly receiving rainfall/irrigation
!
ZVEGFACT(:) = ZSIGMA_F(:)*(1.0-PPALPHAN(:)*PEK%XPSN(:))
!
! The sum of all non-intercepted rain and drip is "ZRRSFC" (kg/m2/s):
! this is then partitioned by snow scheme into part falling on
! snowpack and part falling onto snow-free understory.
!
!
 CALL HYDRO_VEG(IO%CRAIN, PTSTEP, KK%XMUF, ZRR, DEK%XLEV_CV, DEK%XLETR_CV,        &
                ZVEGFACT, ZPSNCV, PEK%XWR, ZWRMAX, ZRRSFC, DEK%XDRIP, DEK%XRRVEG, &
                PK%XLVTT  )
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!*      10.0    Explicit snow scheme (MEB: impose surface fluxes as upper BC)
!              ----------------------------------------------------------------
!
 CALL SNOW3L_ISBA(IO, G, PK, PEK, DK, DEK, DMK, OMEB, HIMPLICIT_WIND,                   &
                  TPTIME, PTSTEP, PK%XVEGTYPE_PATCH,  ZTGL, ZCTSFC,                     &
                  ZSOILHCAPZ, ZSOILCONDZ(:,1), PPS, PEK%XTC, DEK%XSWDOWN_GN, PEK%XQC,   &
                  PVMOD, PLW_RAD, ZRRSFC, DEK%XSR_GN, PRHOA, ZUREF, PEXNS,              &
                  PEXNA, PDIRCOSZW, ZZREF, ZALBG, ZD_G, ZDZG, PPEW_A_COEF,              &
                  PPEW_B_COEF, PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF,                   &
                  PPEQ_B_COEF, PSNOW_THRUFAL, PGRNDFLUX, PFLSN_COR,                     &
                  PRESTOREN, PEVAPCOR, DEK%XLES, DEK%XLESL, ZEVAP3L, PSNOWSFCH,         &
                  PDELHEATN, PDELHEATN_SFC, PRISNOW, PZENITH, PDELHEATG,                &
                  PDELHEATG_SFC, PQSNOW     )
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!*      11.0    Litter layer hydrology:
!               -----------------------
!
IF(IO%LMEB_LITTER)THEN
!
  ZWORK(:)   = 0.
  ZWORK2(:)  = PEK%XWRL(:)
  ZWORK3(:)  = 1.
  ZWORK4(:)  = PSNOW_THRUFAL(:) + ZRRSFC(:)*(1-PEK%XPSN)
  ZWRLMAX(:) = PEK%XGNDLITTER(:)*ZWFC(:,1)*XRHOLW

  CALL HYDRO_VEG(IO%CRAIN, PTSTEP, KK%XMUF, ZWORK4(:), ZLESFC, ZWORK, ZWORK3, ZWORK, &
                 PEK%XWRL, ZWRLMAX, ZRRSFCL, DEK%XDRIPLIT, DEK%XRRLIT, PK%XLVTT  )

  DMK%XRRSFC(:) = ZRRSFCL(:)
  PSNOW_THRUFAL_SOIL(:) = 0.0

ELSE

  PSNOW_THRUFAL_SOIL(:) = PSNOW_THRUFAL(:)

ENDIF
!
!*      11.0    Separate litter and soil temperature
!              ------------------------------------
!
 CALL RESHIFT_MEB_SOIL(IO%LMEB_LITTER, ZTGL, ZLESFC, ZLESFCI, PEK, DEK)
!
CALL DEALLOCATE_LOCAL_VARS_PREP_GRID_SOIL
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!*      13.0    Ice litter effect
!              ------------------
!
IF(IO%LMEB_LITTER)THEN
!
 CALL ICE_LITTER(PTSTEP, DEK%XLELITTERI, PSOILHCAPZ, PEK, PK%NWG_LAYER, &
                 PK%XDZG, ZPHASEL,ZCTSFC,PK%XLSTT,PLITCOR   )
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('ISBA_MEB',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
CONTAINS
!
!===============================================================================
!
SUBROUTINE INIT_SUM_FLUXES_MEB_TSPLIT
!
IMPLICIT NONE
!
!
!*      0.2    declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ISBA_MEB:INIT_SUM_FLUXES_MEB_TSPLIT ',0,ZHOOK_HANDLE)
!
! sensible heat fluxes:
!
ZH_SUM(:)       = 0.0
ZH_N_A_SUM(:)   = 0.0
ZH_CA_SUM(:)    = 0.0
ZH_CV_SUM(:)    = 0.0
ZH_GV_SUM(:)    = 0.0
ZH_GN_SUM(:)    = 0.0
ZHSNOW_SUM(:)   = 0.0
!
! latent heat/water vapor fluxes:
!
ZLE_SUM(:)       = 0.0
!
ZLE_CA_SUM(:)    = 0.0
ZLE_CV_SUM(:)    = 0.0
ZLE_GV_SUM(:)    = 0.0
ZLE_GN_SUM(:)    = 0.0
!
ZLETR_CV_SUM(:)  = 0.0
ZLER_CV_SUM(:)   = 0.0
ZLES_CV_SUM(:)   = 0.0
!
ZLEG_SUM(:)      = 0.0
ZLEGI_SUM(:)     = 0.0
ZLESFC_SUM(:)    = 0.0
ZLESFCI_SUM(:)   = 0.0
ZLE_FLOOD_SUM(:) = 0.0
ZLEI_FLOOD_SUM(:)= 0.0
ZLETR_SUM(:)     = 0.0
ZLER_SUM(:)      = 0.0
ZLEV_SUM(:)      = 0.0
ZLEI_SUM(:)      = 0.0
ZLES3L_SUM(:)    = 0.0
ZLEL3L_SUM(:)    = 0.0
ZEVAP3L_SUM(:)   = 0.0
ZEVAP_SUM(:)     = 0.0
!
ZHU_AGG_SUM(:)   = 0.0
ZAC_AGG_SUM(:)   = 0.0
!
! momentum/turb:
!
ZUSTAR2_SUM(:)     = 0.0
ZUSTAR2SNOW_SUM(:) = 0.
ZCDSNOW_SUM(:)     = 0.
ZCHSNOW_SUM(:)     = 0.
ZRISNOW_SUM(:)     = 0.
!
! surface interfacial/sub-surface heat fluxes:
!
ZGRNDFLUX_SUM(:) = 0.0
ZRESTORE_SUM(:)  = 0.0
ZHPSNOW_SUM(:)   = 0.0
!
! radiative fluxes:
!
ZSWNET_V_SUM(:)  = 0.0
ZSWNET_G_SUM(:)  = 0.0
ZSWNET_N_SUM(:)  = 0.0
ZLWNET_V_SUM(:)  = 0.0
ZLWNET_G_SUM(:)  = 0.0
ZLWNET_N_SUM(:)  = 0.0
ZEMIST_SUM(:)    = 0.0
ZSWUP_SUM(:)     = 0.0
ZLWUP_SUM(:)     = 0.0
!
ZDELHEATV_SFC_SUM(:) = 0.0
ZDELHEATG_SFC_SUM(:) = 0.0
ZDELHEATG_SUM(:)     = 0.0
!
IF (LHOOK) CALL DR_HOOK('ISBA_MEB:INIT_SUM_FLUXES_MEB_TSPLIT ',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_SUM_FLUXES_MEB_TSPLIT
!
!===============================================================================
!
SUBROUTINE SUM_FLUXES_MEB_TSPLIT
!
IMPLICIT NONE
!
!*      0.2    declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ISBA_MEB:SUM_FLUXES_MEB_TSPLIT ',0,ZHOOK_HANDLE)
!
! Sum fluxes over MEB TIME SPLIT:
!
! sensible heat fluxes:
!
ZH_N_A_SUM(:) = ZH_N_A_SUM(:) + ZH_N_A(:)
!
ZH_SUM(:)     = ZH_SUM(:)     + DK%XH(:)
!
ZH_CA_SUM(:)  = ZH_CA_SUM(:)  + DEK%XH_CA(:)
ZH_CV_SUM(:)  = ZH_CV_SUM(:)  + DEK%XH_CV(:)
ZH_GV_SUM(:)  = ZH_GV_SUM(:)  + DEK%XH_GV(:)
ZH_GN_SUM(:)  = ZH_GN_SUM(:)  + DEK%XH_GN(:)
!
ZHSNOW_SUM(:) = ZHSNOW_SUM(:) + DMK%XHSNOW(:)
!
! latent heat/water vapor fluxes:
!
ZLE_SUM(:)   = ZLE_SUM(:)   + PEK%XLE(:)
!
ZLEI_SUM(:)  = ZLEI_SUM(:)  + DK%XLEI(:)
ZEVAP_SUM(:) = ZEVAP_SUM(:) + DK%XEVAP(:)
!
ZLE_CA_SUM(:)    = ZLE_CA_SUM(:)    + DEK%XLE_CA(:)
ZLE_CV_SUM(:)    = ZLE_CV_SUM(:)    + DEK%XLE_CV(:)
ZLE_GV_SUM(:)    = ZLE_GV_SUM(:)    + DEK%XLE_GV(:)
ZLE_GN_SUM(:)    = ZLE_GN_SUM(:)    + DEK%XLE_GN(:)
!
ZLETR_CV_SUM(:)  = ZLETR_CV_SUM(:)  + DEK%XLETR_CV(:)
ZLER_CV_SUM(:)   = ZLER_CV_SUM(:)   + DEK%XLER_CV(:)
ZLES_CV_SUM(:)   = ZLES_CV_SUM(:)   + DEK%XLES_CV(:)
!
ZLETR_SUM(:)     = ZLETR_SUM(:)     + DEK%XLETR(:)
ZLER_SUM(:)      = ZLER_SUM(:)      + DEK%XLER(:)
ZLEV_SUM(:)      = ZLEV_SUM(:)      + DEK%XLEV(:)
!
ZLEG_SUM(:)       = ZLEG_SUM(:)       + DEK%XLEG(:)
ZLEGI_SUM(:)      = ZLEGI_SUM(:)      + DEK%XLEGI(:)

ZLE_FLOOD_SUM(:)  = ZLE_FLOOD_SUM(:)  + DEK%XLE_FLOOD(:)
ZLEI_FLOOD_SUM(:) = ZLEI_FLOOD_SUM(:) + DEK%XLEI_FLOOD(:)
!
ZLESFC_SUM(:)    = ZLESFC_SUM(:)    + ZLESFC(:)
ZLESFCI_SUM(:)   = ZLESFCI_SUM(:)   + ZLESFCI(:)
!
ZLES3L_SUM(:)    = ZLES3L_SUM(:)    + ZLES3L(:)
ZLEL3L_SUM(:)    = ZLEL3L_SUM(:)    + ZLEL3L(:)
ZEVAP3L_SUM(:)   = ZEVAP3L_SUM(:)   + ZEVAP3L(:)
!
ZHU_AGG_SUM(:)   = ZHU_AGG_SUM(:)   + PHU_AGG(:)
ZAC_AGG_SUM(:)   = ZAC_AGG_SUM(:)   + PAC_AGG(:)
!
! momentum/turb:
!
ZCDSNOW_SUM(:)     = ZCDSNOW_SUM(:)     + DMK%XCDSNOW(:)
ZCHSNOW_SUM(:)     = ZCHSNOW_SUM(:)     + DMK%XCHSNOW(:)
!
ZUSTAR2_SUM(:)     = ZUSTAR2_SUM(:)     + ZUSTAR2_IC(:)
ZUSTAR2SNOW_SUM(:) = ZUSTAR2SNOW_SUM(:) + ZUSTAR2SNOW(:)
ZRISNOW_SUM(:)     = ZRISNOW_SUM(:)     + PRISNOW(:)
!
! surface interfacial/sub-surface heat fluxes:
!
ZGRNDFLUX_SUM(:) = ZGRNDFLUX_SUM(:) + PGRNDFLUX(:)
!
ZRESTORE_SUM(:)  = ZRESTORE_SUM(:)  + DEK%XRESTORE(:)
!
ZHPSNOW_SUM(:)   = ZHPSNOW_SUM(:)   + DMK%XHPSNOW(:)
!
! radiative fluxes:
!
ZSWNET_V_SUM(:)  = ZSWNET_V_SUM(:)  +   DEK%XSWNET_V(:)
ZSWNET_G_SUM(:)  = ZSWNET_G_SUM(:)  +   DEK%XSWNET_G(:)
ZSWNET_N_SUM(:)  = ZSWNET_N_SUM(:)  +   DEK%XSWNET_N(:)
ZLWNET_V_SUM(:)  = ZLWNET_V_SUM(:)  +   DEK%XLWNET_V(:)
ZLWNET_G_SUM(:)  = ZLWNET_G_SUM(:)  +   DEK%XLWNET_G(:)
ZLWNET_N_SUM(:)  = ZLWNET_N_SUM(:)  +   DEK%XLWNET_N(:)
!
ZEMIST_SUM(:)    = ZEMIST_SUM(:)    +   PEMIST(:)
ZSWUP_SUM(:)     = ZSWUP_SUM(:)     +   ZSWUP(:)
ZLWUP_SUM(:)     = ZLWUP_SUM(:)     +   ZLWUP(:)
!
ZDELHEATV_SFC_SUM(:) = ZDELHEATV_SFC_SUM(:) +   PDELHEATV_SFC(:)
ZDELHEATG_SFC_SUM(:) = ZDELHEATG_SFC_SUM(:) +   PDELHEATG_SFC(:)
ZDELHEATG_SUM(:)     = ZDELHEATG_SUM(:)     +   PDELHEATG(:)
!
IF (LHOOK) CALL DR_HOOK('ISBA_MEB:SUM_FLUXES_MEB_TSPLIT ',1,ZHOOK_HANDLE)
!
END SUBROUTINE SUM_FLUXES_MEB_TSPLIT
!
!===============================================================================
!
SUBROUTINE AVG_FLUXES_MEB_TSPLIT
!
USE MODD_CSTS, ONLY : XSTEFAN
!
IMPLICIT NONE
!
!*      0.2    declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ISBA_MEB:AVG_FLUXES_MEB_TSPLIT ',0,ZHOOK_HANDLE)
!
! Average fluxes over MEB TIME SPLIT:
!
! sensible heat fluxes:
!
ZH_N_A(:)    = ZH_N_A_SUM(:) /JTSPLIT_EB
!
DK%XH(:)     = ZH_SUM(:)     /JTSPLIT_EB
!
DEK%XH_CA(:) = ZH_CA_SUM(:)  /JTSPLIT_EB
DEK%XH_CV(:) = ZH_CV_SUM(:)  /JTSPLIT_EB
DEK%XH_GV(:) = ZH_GV_SUM(:)  /JTSPLIT_EB
DEK%XH_GN(:) = ZH_GN_SUM(:)  /JTSPLIT_EB
!
DMK%XHSNOW(:) = ZHSNOW_SUM(:) /JTSPLIT_EB
!
! latent heat/water vapor fluxes:
!
ZLESFC(:)         = ZLESFC_SUM(:)    /JTSPLIT_EB
ZLESFCI(:)        = ZLESFCI_SUM(:)   /JTSPLIT_EB
!
DK%XLEI(:)        = ZLEI_SUM(:)      /JTSPLIT_EB
DK%XEVAP(:)       = ZEVAP_SUM(:)     /JTSPLIT_EB
!
PEK%XLE(:)        = ZLE_SUM(:)       /JTSPLIT_EB
!
DEK%XLE_CA(:)    = ZLE_CA_SUM(:)     /JTSPLIT_EB
DEK%XLE_CV(:)    = ZLE_CV_SUM(:)     /JTSPLIT_EB
DEK%XLE_GV(:)    = ZLE_GV_SUM(:)     /JTSPLIT_EB
DEK%XLE_GN(:)    = ZLE_GN_SUM(:)     /JTSPLIT_EB
!
DEK%XLETR_CV(:)  = ZLETR_CV_SUM(:)   /JTSPLIT_EB
DEK%XLER_CV(:)   = ZLER_CV_SUM(:)    /JTSPLIT_EB
DEK%XLES_CV(:)   = ZLES_CV_SUM(:)   /JTSPLIT_EB
!
DEK%XLETR(:)      = ZLETR_SUM(:)     /JTSPLIT_EB
DEK%XLER(:)       = ZLER_SUM(:)      /JTSPLIT_EB
DEK%XLEV(:)       = ZLEV_SUM(:)      /JTSPLIT_EB
!
DEK%XLEG(:)       = ZLEG_SUM(:)      /JTSPLIT_EB
DEK%XLEGI(:)      = ZLEGI_SUM(:)     /JTSPLIT_EB
DEK%XLE_FLOOD(:)  = ZLE_FLOOD_SUM(:) /JTSPLIT_EB
DEK%XLEI_FLOOD(:) = ZLEI_FLOOD_SUM(:)/JTSPLIT_EB
DEK%XLES(:)       = ZLES3L_SUM(:)    /JTSPLIT_EB
DEK%XLESL(:)      = ZLEL3L_SUM(:)    /JTSPLIT_EB
!
ZEVAP3L(:)        = ZEVAP3L_SUM(:)   /JTSPLIT_EB
!
PHU_AGG(:)   = ZHU_AGG_SUM(:)   /JTSPLIT_EB
PAC_AGG(:)   = ZAC_AGG_SUM(:)   /JTSPLIT_EB
!
! momentum/turb:
!
PUSTAR(:)          = SQRT( ZUSTAR2_SUM(:)    /JTSPLIT_EB )
PRISNOW(:)         = ZRISNOW_SUM(:)          /JTSPLIT_EB
!
DMK%XUSTARSNOW(:) = SQRT( ZUSTAR2SNOW_SUM(:)/JTSPLIT_EB )
DMK%XCDSNOW(:)    = ZCDSNOW_SUM(:)          /JTSPLIT_EB
DMK%XCHSNOW(:)    = ZCHSNOW_SUM(:)          /JTSPLIT_EB
!
! surface interfacial/sub-surface heat fluxes:
!
PGRNDFLUX(:)    = ZGRNDFLUX_SUM(:) /JTSPLIT_EB
!
DEK%XRESTORE(:) = ZRESTORE_SUM(:)   /JTSPLIT_EB
DMK%XHPSNOW(:)  = ZHPSNOW_SUM(:)   /JTSPLIT_EB
!
! radiative fluxes:
!
DEK%XSWNET_V(:)  = ZSWNET_V_SUM(:)  /JTSPLIT_EB
DEK%XSWNET_G(:)  = ZSWNET_G_SUM(:)  /JTSPLIT_EB
DEK%XSWNET_N(:)  = ZSWNET_N_SUM(:)  /JTSPLIT_EB
DEK%XLWNET_V(:)  = ZLWNET_V_SUM(:)  /JTSPLIT_EB
DEK%XLWNET_G(:)  = ZLWNET_G_SUM(:)  /JTSPLIT_EB
DEK%XLWNET_N(:)  = ZLWNET_N_SUM(:)  /JTSPLIT_EB
!
PEMIST(:)    = ZEMIST_SUM(:)    /JTSPLIT_EB
ZSWUP(:)     = ZSWUP_SUM(:)     /JTSPLIT_EB
ZLWUP(:)     = ZLWUP_SUM(:)     /JTSPLIT_EB
!
PDELHEATV_SFC(:) = ZDELHEATV_SFC_SUM(:) /JTSPLIT_EB
PDELHEATG_SFC(:) = ZDELHEATG_SFC_SUM(:) /JTSPLIT_EB
PDELHEATG(:)     = ZDELHEATG_SUM(:)     /JTSPLIT_EB
!
! Additional diagnostics depending on AVG quantities:
!
DK%XTSRAD(:)  = ((ZLWUP(:) - PLW_RAD(:)*(1.0-PEMIST(:)))/(XSTEFAN*PEMIST(:)))**0.25
!
ZRNET_V(:)      = DEK%XSWNET_V(:) + DEK%XLWNET_V(:)
!
ZRNET_G(:)      = DEK%XSWNET_G(:) + DEK%XLWNET_G(:)
!
DMK%XRNSNOW(:) = DEK%XSWNET_N(:) + DEK%XLWNET_N(:)
!
DK%XRN(:)     = ZRNET_V(:) + ZRNET_G(:) + DMK%XRNSNOW(:)
!
DEK%XLEV_CV(:)  = DEK%XLE_CV(:) - DEK%XLES_CV(:)
!
IF (LHOOK) CALL DR_HOOK('ISBA_MEB:AVG_FLUXES_MEB_TSPLIT ',1,ZHOOK_HANDLE)
!
END SUBROUTINE AVG_FLUXES_MEB_TSPLIT
!
!===============================================================================
SUBROUTINE SNOWALB_SPECTRAL_BANDS_MEB(PVEGTYPE,PEK,PSNOWRHO,PSNOWAGE,PPS, &
                                      PSNOWDZ,PZENITH,PTAU_N)
!
! Split Total snow albedo into N-spectral bands
! NOTE currently MEB only uses 2 bands of the 3 possible.
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_DATA_COVER_PAR, ONLY : NVT_SNOW
USE MODD_MEB_PAR,        ONLY : XSW_WGHT_VIS, XSW_WGHT_NIR
USE MODD_SNOW_PAR,       ONLY : NSPEC_BAND_SNOW
USE MODD_SNOW_METAMO,    ONLY : XSNOWDZMIN
!
USE MODE_SNOW3L,         ONLY : SNOW3LALB, SNOW3LDOPT
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
REAL, DIMENSION(:,:), INTENT(IN)    :: PVEGTYPE      ! fraction of each vegetation (-)
REAL, DIMENSION(:,:), INTENT(IN)    :: PSNOWRHO      ! Snow layer average density (kg/m3)
REAL, DIMENSION(:,:), INTENT(IN)    :: PSNOWDZ       ! Snow layer thickness (m)
REAL, DIMENSION(:),   INTENT(IN)    :: PZENITH       ! Zenith angle (rad)
REAL, DIMENSION(:,:), INTENT(IN)    :: PSNOWAGE      ! Snow grain age
REAL, DIMENSION(:),   INTENT(IN)    :: PPS           ! Pressure [Pa]
REAL, DIMENSION(:,:), INTENT(OUT)   :: PTAU_N        ! SW absorption (coef) in uppermost snow layer (-)
!
!*      0.2    declarations of local variables
!
INTEGER                             :: JJ, JI, INJ, INLVLS
REAL, DIMENSION(SIZE(PPS))          :: ZWORK, ZWORKA, ZAGE
REAL, DIMENSION(SIZE(PPS))          :: ZPROJLAT, ZDSGRAIN, ZBETA1, ZBETA2, ZBETA3, &
                                       ZOPTICALPATH1, ZOPTICALPATH2, ZOPTICALPATH3
REAL, DIMENSION(SIZE(PPS))          :: ZPERMSNOWFRAC
REAL, DIMENSION(SIZE(PSNOWDZ,1),SIZE(PSNOWDZ,2)) :: ZSNOWDZ
REAL, DIMENSION(SIZE(PPS),NSPEC_BAND_SNOW)       :: ZSPECTRALALBEDO
!                                      ZSPECTRALALBEDO = spectral albedo (3 bands in algo:
!                                                        MEB currently uses 2)
!                                                        1=VIS, 2=NIR, 3=UV
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ISBA_MEB:SNOWALB_SPECTRAL_BANDS_MEB',0,ZHOOK_HANDLE)
!
INJ    = SIZE(PSNOWDZ,1)
INLVLS = SIZE(PSNOWDZ,2)
!
! 1) Spectral albedo
! ------------------
!
ZWORK(:)         = 0.0
ZWORKA(:)        = PEK%TSNOW%ALB(:)
ZPERMSNOWFRAC(:) = PVEGTYPE(:,NVT_SNOW)
!
CALL SNOW3LALB(ZWORKA,ZSPECTRALALBEDO,PSNOWRHO(:,1),PSNOWAGE(:,1),ZPERMSNOWFRAC,PPS)
!
! Since we only consider VIS and NIR bands for soil and veg in MEB currently:
! (also note, here PSNOWALB doesn't evolve...we just diagnose spectral components).
!
WHERE(PEK%TSNOW%ALB(:)/=XUNDEF)
!
   PEK%TSNOW%ALBVIS(:) = ZSPECTRALALBEDO(:,1)
!
! We diagnose NIR albedo such that total albedo is conserved
! (using just 2 spectral bands in MEB)
!
   PEK%TSNOW%ALBNIR(:) = (PEK%TSNOW%ALB(:) - XSW_WGHT_VIS*PEK%TSNOW%ALBVIS(:))/XSW_WGHT_NIR
!
! currently NOT used by MEB
!
   PEK%TSNOW%ALBFIR(:) = XUNDEF
!
! For the surface layer absorbtion computation:
!
   ZSPECTRALALBEDO(:,1) = PEK%TSNOW%ALBVIS(:)
   ZSPECTRALALBEDO(:,2) = PEK%TSNOW%ALBNIR(:)
   ZSPECTRALALBEDO(:,3) = PEK%TSNOW%ALBFIR(:)
!
ELSEWHERE
!
   PEK%TSNOW%ALBVIS(:) = XUNDEF
   PEK%TSNOW%ALBNIR(:) = XUNDEF
   PEK%TSNOW%ALBFIR(:) = XUNDEF
!
END WHERE
!
! Snow optical grain diameter (no age dependency over polar regions):
!
ZAGE(:) = (1.0-ZPERMSNOWFRAC(:))*PSNOWAGE(:,1)
!
ZDSGRAIN(:) = SNOW3LDOPT(PSNOWRHO(:,1),ZAGE)
!
! 2) SW absorption in uppermost snow layer
! ----------------------------------------
! For now, consider just 2 bands with MEB, so renormalize:

ZSPECTRALALBEDO(:,2) = (PEK%TSNOW%ALB(:) - XSW_WGHT_VIS*ZSPECTRALALBEDO(:,1))/XSW_WGHT_NIR
ZSPECTRALALBEDO(:,3) = XUNDEF
!
! Adjust thickness to be as in snow computations:
!
DO JJ=1,INLVLS
   DO JI=1,INJ
      ZSNOWDZ(JI,JJ) = PSNOWDZ(JI,JJ)/MAX(1.E-4,PEK%XPSN(JI))
   ENDDO
ENDDO
!
CALL SNOW3LRADTRANS(XSNOWDZMIN, ZSPECTRALALBEDO, ZSNOWDZ, PSNOWRHO, &
                           ZPERMSNOWFRAC, PZENITH,  PSNOWAGE, PTAU_N)
!
! Note that because we force a snow thickness to compute tramission,
! a bogus value ( < 0) can be computed despite the non-estance of snow.
! To check/prevent any problems, simply make a simple check:
!
PTAU_N(:,:) = MAX(0., PTAU_N(:,:))
!
IF (LHOOK) CALL DR_HOOK('ISBA_MEB:SNOWALB_SPECTRAL_BANDS_MEB',1,ZHOOK_HANDLE)
!
END SUBROUTINE SNOWALB_SPECTRAL_BANDS_MEB
!===============================================================================
      SUBROUTINE SNOW3LRADTRANS(PSNOWDZMIN, PSPECTRALALBEDO, PSNOWDZ, PSNOWRHO, &
                                PPERMSNOWFRAC, PZENITH,  PSNOWAGE, PRADTRANS)
!
!!    PURPOSE
!!    -------
!     Calculate the transmission of shortwave (solar) radiation
!     through the snowpack (using a form of Beer's Law: exponential
!     decay of radiation with increasing snow depth).
!
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_MEB_PAR,  ONLY : XSW_WGHT_VIS, XSW_WGHT_NIR
!
USE MODE_SNOW3L,   ONLY : SNOW3LDOPT, SNOW3LRADABS_SFC
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,                 INTENT(IN)    :: PSNOWDZMIN
!
REAL, DIMENSION(:),   INTENT(IN)    :: PPERMSNOWFRAC
REAL, DIMENSION(:),   INTENT(IN)    :: PZENITH
REAL, DIMENSION(:,:), INTENT(IN)    :: PSNOWRHO, PSNOWDZ, PSNOWAGE
REAL, DIMENSION(:,:), INTENT(IN)    :: PSPECTRALALBEDO
!
REAL, DIMENSION(:,:), INTENT(OUT)   :: PRADTRANS
!
!
!*      0.2    declarations of local variables
!
INTEGER                              :: JJ, JI
!
INTEGER                              :: INJ
INTEGER                              :: INLVLS
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZDSGRAIN, ZCOEF, ZSNOWDZ, ZAGE
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
! 0. Initialization:
! ------------------
!
IF (LHOOK) CALL DR_HOOK('SNOW3LRADTRANS',0,ZHOOK_HANDLE)
!
INJ    = SIZE(PSNOWDZ(:,:),1)
INLVLS = SIZE(PSNOWDZ(:,:),2)
!
!
! 1. Vanishingly thin snowpack check:
! -----------------------------------
!    For vanishingly thin snowpacks, much of the radiation
!    can pass through snowpack into underlying soil, making
!    a large (albeit temporary) thermal gradient: by imposing
!    a minimum thickness, this increases the radiation absorbtion
!    for vanishingly thin snowpacks.
!
ZSNOWDZ(:,:) = MAX(PSNOWDZMIN, PSNOWDZ(:,:))
!
!
! 2. Extinction of net shortwave radiation
! ----------------------------------------
! Fn of snow depth and density (Loth and Graf 1993:
! SNOWCVEXT => from Bohren and Barkstrom 1974
! SNOWAGRAIN and SNOWBGRAIN=> from Jordan 1976)
!
! Snow optical grain diameter (no age dependency over polar regions):
!
ZAGE(:,:) = 0.
DO JJ=1,INLVLS
   DO JI=1,INJ
      IF(PSNOWAGE(JI,JJ)/=XUNDEF)THEN
         ZAGE(JI,JJ) = (1.0-PPERMSNOWFRAC(JI))*PSNOWAGE(JI,JJ)
      ENDIF
   ENDDO
ENDDO
!
ZDSGRAIN(:,:) = SNOW3LDOPT(PSNOWRHO,ZAGE)
!
ZCOEF(:,:)    = SNOW3LRADABS_SFC(PSNOWRHO,ZSNOWDZ,PSPECTRALALBEDO,   &
                                 PZENITH,PPERMSNOWFRAC,ZDSGRAIN)
!
! 3. Radiation trans at base of each layer
! ----------------------------------
! NOTE, at level=0, rad = Swnet*(1-alb)  so radcoef(0)=1 implicitly
!
PRADTRANS(:,:)  = ZCOEF(:,:)
!
IF (LHOOK) CALL DR_HOOK('SNOW3LRADTRANS',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SNOW3LRADTRANS
!===============================================================================

SUBROUTINE ALLOCATE_LOCAL_VARS_PREP_GRID_SOIL
!
IMPLICIT NONE
!
!*      0.2    declarations of local variables
!
INTEGER         :: INLL
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('ISBA_MEB:ALLOCATE_LOCAL_VARS_PREP_GRID_SOIL ',0,ZHOOK_HANDLE)

INLL = INL
IF(IO%LMEB_LITTER)INLL = INL + 1

ALLOCATE ( ZTGL       (INJ, INLL ))
ALLOCATE ( ZSOILHCAPZ (INJ, INLL ))
ALLOCATE ( ZSOILCONDZ (INJ, INLL ))
ALLOCATE ( ZD_G       (INJ, INLL ))
ALLOCATE ( ZDZG       (INJ, INLL ))
ALLOCATE ( ZWFC       (INJ, INLL ))
ALLOCATE ( ZWSAT      (INJ, INLL ))

IF (LHOOK) CALL DR_HOOK('ISBA_MEB:ALLOCATE_LOCAL_VARS_PREP_GRID_SOIL ',1,ZHOOK_HANDLE)

END SUBROUTINE ALLOCATE_LOCAL_VARS_PREP_GRID_SOIL
!===============================================================================
SUBROUTINE DEALLOCATE_LOCAL_VARS_PREP_GRID_SOIL
!
IMPLICIT NONE
!
!*      0.2    declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('ISBA_MEB:DEALLOCATE_LOCAL_VARS_PREP_GRID_SOIL ',0,ZHOOK_HANDLE)

DEALLOCATE ( ZTGL       )
DEALLOCATE ( ZSOILHCAPZ )
DEALLOCATE ( ZSOILCONDZ )
DEALLOCATE ( ZD_G       )
DEALLOCATE ( ZDZG       )
DEALLOCATE ( ZWSAT      )
DEALLOCATE ( ZWFC       )

IF (LHOOK) CALL DR_HOOK('ISBA_MEB:DEALLOCATE_LOCAL_VARS_PREP_GRID_SOIL ',1,ZHOOK_HANDLE)

END SUBROUTINE DEALLOCATE_LOCAL_VARS_PREP_GRID_SOIL
!===============================================================================
SUBROUTINE RESHIFT_MEB_SOIL(OMEB_LITTER,PTGL,PLESFC,PLESFCI,PEK,DEK)
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
LOGICAL,                INTENT(IN)    :: OMEB_LITTER
REAL,   DIMENSION(:,:), INTENT(IN)    :: PTGL
REAL,   DIMENSION(:),   INTENT(IN)    :: PLESFC
REAL,   DIMENSION(:),   INTENT(IN)    :: PLESFCI
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: DEK
!
!*      0.2    declarations of local variables
!
INTEGER                               :: JJ, JL
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------

INJ  = SIZE(PEK%XTG(:,:),1)
INL  = SIZE(PEK%XTG(:,:),2)

IF (LHOOK) CALL DR_HOOK('ISBA_MEB:FINISH_MEB_SOIL ',0,ZHOOK_HANDLE)

IF (OMEB_LITTER)THEN

   PEK%XTL(:)           = PTGL(:,1)

   DO JL=1,INL
      DO JJ=1,INJ
         PEK%XTG(JJ,JL) = PTGL(JJ,JL+1)
      ENDDO
   ENDDO

   DEK%XLEG(:)          = 0.0
   DEK%XLEGI(:)         = 0.0
   DEK%XLELITTER(:)     = PLESFC(:)
   DEK%XLELITTERI(:)    = PLESFCI(:)
ELSE

   PEK%XTG(:,:)          = PTGL(:,:)

   DEK%XLEG(:)          = PLESFC(:)
   DEK%XLEGI(:)         = PLESFCI(:)
   DEK%XLELITTER(:)     = 0.
   DEK%XLELITTERI(:)    = 0.

ENDIF


IF (LHOOK) CALL DR_HOOK('ISBA_MEB:FINISH_MEB_SOIL ',1,ZHOOK_HANDLE)

END SUBROUTINE RESHIFT_MEB_SOIL
!===============================================================================
SUBROUTINE PREP_MEB_SOIL(OMEB_LITTER,PSOILHCAPZ,PSOILCONDZ,KK,PK,PEK,PD_GL,&
                         PDZGL,PTGL,PSOILHCAPL,PSOILCONDL,PWSATL,PWFCL,PWSFC,&
                         PWISFC,PCTSFC,PCT,PFROZEN1,PFROZEN1SFC)
!
USE MODD_CSTS,       ONLY : XRHOLW, XCL
USE MODD_ISBA_PAR,   ONLY : XWGMIN
USE MODD_MEB_PAR,    ONLY : XLITTER_HYD_Z4, XLITTER_HYD_Z5
!
USE MODE_MEB,        ONLY : MEBLITTER_THRM
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(ISBA_K_t), INTENT(INOUT) :: KK
TYPE(ISBA_P_t), INTENT(INOUT) :: PK
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
LOGICAL,                INTENT(IN)    :: OMEB_LITTER
REAL,   DIMENSION(:,:), INTENT(IN)    :: PSOILHCAPZ
REAL,   DIMENSION(:,:), INTENT(IN)    :: PSOILCONDZ
!
REAL,   DIMENSION(:),   INTENT(IN)    :: PCT
REAL,   DIMENSION(:),   INTENT(IN)    :: PFROZEN1
REAL,   DIMENSION(:,:), INTENT(OUT)   :: PD_GL
REAL,   DIMENSION(:,:), INTENT(OUT)   :: PDZGL
REAL,   DIMENSION(:,:), INTENT(OUT)   :: PTGL
REAL,   DIMENSION(:,:), INTENT(OUT)   :: PSOILHCAPL
REAL,   DIMENSION(:,:), INTENT(OUT)   :: PSOILCONDL
REAL,   DIMENSION(:,:), INTENT(OUT)   :: PWSATL
REAL,   DIMENSION(:,:), INTENT(OUT)   :: PWFCL
REAL,   DIMENSION(:),   INTENT(OUT)   :: PWSFC
REAL,   DIMENSION(:),   INTENT(OUT)   :: PWISFC
REAL,   DIMENSION(:),   INTENT(OUT)   :: PCTSFC
REAL,   DIMENSION(:),   INTENT(OUT)   :: PFROZEN1SFC
!
!*      0.2    declarations of local variables
!
INTEGER                               :: INJ, INL, JJ, JL
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ISBA_MEB:PREP_MEB_SOIL',0,ZHOOK_HANDLE)
!
INJ  = SIZE(PK%XDG,1)
INL  = SIZE(PK%XDG,2)
!
IF(OMEB_LITTER)THEN
   PTGL(:,1)                  = PEK%XTL(:)
   CALL MEBLITTER_THRM(PEK%XWRL,PEK%XWRLI,PEK%XGNDLITTER,PSOILHCAPL(:,1),PSOILCONDL(:,1))
   PWSATL(:,1)                = XLITTER_HYD_Z4
   PWFCL(:,1)                 = XLITTER_HYD_Z5
   PD_GL(:,1)                 = PEK%XGNDLITTER(:)
   PDZGL(:,1)                 = PEK%XGNDLITTER(:)
   PCTSFC(:)                  = 1. / (PSOILHCAPL(:,1) * PEK%XGNDLITTER(:))
   PFROZEN1SFC(:)             = PEK%XWRLI(:) / ( PEK%XWRLI(:) + MAX(PEK%XWRL(:), (XWGMIN*XRHOLW)*PEK%XGNDLITTER(:) ))

   DO JL=1,INL
      DO JJ=1,INJ
         PTGL(JJ,JL+1)        = PEK%XTG(JJ,MIN(JL,SIZE(PEK%XTG,2)))
         PSOILHCAPL(JJ,JL+1)  = PSOILHCAPZ(JJ,JL)
         PSOILCONDL(JJ,JL+1)  = PSOILCONDZ(JJ,JL)
         PWSATL(JJ,JL+1)      = KK%XWSAT(JJ,JL)
         PWFCL(JJ,JL+1)       = KK%XWFC(JJ,JL)
         PD_GL(JJ,JL+1)       = PEK%XGNDLITTER(JJ) + PK%XDG(JJ,JL)
         PDZGL(JJ,JL+1)       = PK%XDZG(JJ,JL)
      ENDDO
   ENDDO
   PWSFC(:)                   = PEK%XWRL(:) /(XRHOLW*PEK%XGNDLITTER(:)) ! (m3/m3)
   PWISFC(:)                  = PEK%XWRLI(:)/(XRHOLW*PEK%XGNDLITTER(:)) ! (m3/m3)

ELSE
   PTGL(:,:)                  = PEK%XTG(:,:)
   PSOILHCAPL(:,:)            = PSOILHCAPZ(:,:)
   PSOILCONDL(:,:)            = PSOILCONDZ(:,:)
   PWSATL(:,:)                = KK%XWSAT(:,:)
   PWFCL(:,:)                 = KK%XWFC(:,:)
   PD_GL(:,:)                 = PK%XDG(:,:)
   PDZGL(:,:)                 = PK%XDZG(:,:)
   PCTSFC(:)                  = PCT(:)
   PWSFC(:)                   = PEK%XWG(:,1)
   PWISFC(:)                  = PEK%XWGI(:,1)
   PFROZEN1SFC(:)             = PFROZEN1(:)
ENDIF
IF (LHOOK) CALL DR_HOOK('ISBA_MEB:PREP_MEB_SOIL',1,ZHOOK_HANDLE)

END SUBROUTINE PREP_MEB_SOIL
!===============================================================================
SUBROUTINE ICE_LITTER(PTSTEP, PLELITTERI, PSOILHCAPZ, PEK, &
                      KWG_LAYER, PDZG, PPHASEL, PCTSFC, PLSTT, PLITCOR   )
!
USE MODD_CSTS,     ONLY : XLMTT, XTT, XCI, XRHOLI, XRHOLW
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
REAL, INTENT(IN)                    :: PTSTEP
!                                      PTSTEP     = Model time step (s)
!
REAL, DIMENSION(:), INTENT(IN)      :: PLELITTERI
!                                      PLELITTERI = ice sublimation (m s-1)
REAL, DIMENSION(:), INTENT(IN)      :: PCTSFC
REAL, DIMENSION(:), INTENT(IN)      :: PLSTT
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PSOILHCAPZ
!                                      PSOILHCAPZ = soil heat capacity [J/(m3 K)]
REAL, DIMENSION(:,:), INTENT(IN)    :: PDZG
!                                      PDZG       = Layer thickness (DIF option)
!
INTEGER, DIMENSION(:), INTENT(IN)   :: KWG_LAYER
!                                      KWG_LAYER = Number of soil moisture layers (DIF option)
!
REAL, DIMENSION(:), INTENT(OUT)     :: PPHASEL
!                                      PPHASEL = Phase changement in litter (W/m2)
REAL, DIMENSION(:), INTENT(OUT)     :: PLITCOR
!                                      PLITCOR = A possible ice mass correction (to be potentially
!                                                removed from soil)  (kg/m2/s)
!
!*      0.2    declarations of local variables
!
INTEGER                             :: JL     ! loop control
!
INTEGER                             :: INL    ! Number of explicit soil layers
!
REAL, DIMENSION(SIZE(PEK%XTG,1))    :: ZEXCESS, ZK, ZHCAPL,ZELITTERI,               &
                                            ZDELTAT,ZPHASE,ZPHASEM,ZPHASEF,ZPHASEX, &
                                            ZWRL,ZWRLI,Z0,ZPHASEC
!
REAL                                :: ZPSI
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      0.3    declaration of local parameters
!
REAL, PARAMETER                     :: ZERTOL     = 1.E-6 ! (-)     error tolerance
REAL, PARAMETER                     :: ZTAUICE    = 3300. ! (s)     litter phase change characteristic time scale
REAL, PARAMETER                     :: ZWRLSAT    = 0.85  ! (m3/m3) litter porosity
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ISBA_MEB:ICE_LITTER',0,ZHOOK_HANDLE)
!
! Initialization:
! ---------------
!
IF (SIZE(KWG_LAYER)>0) THEN
  INL = MAXVAL(KWG_LAYER(:))
ELSE
  INL = SIZE(PEK%XWG,2)
ENDIF
!
ZEXCESS(:)  = 0.0
ZPHASEC(:)  = 0.0
PLITCOR(:)  = 0.0
!
ZHCAPL(:)   = 1/(PCTSFC(:)*PEK%XGNDLITTER(:))
!
!-------------------------------------------------------------------------------
!
! 1. Surface layer vegetation insulation coefficient (-)
!    ---------------------------------------------------
!
! 1.1 Convert to m3/m3
!    -----------------
!
ZWRL(:)= PEK%XWRL(:) /(XRHOLW*PEK%XGNDLITTER(:))
ZWRLI(:)=PEK%XWRLI(:)/(XRHOLW*PEK%XGNDLITTER(:))
!
! 2. Litter ice evolution computation:
!    --------------------------------
!
ZDELTAT(:) = PEK%XTL(:) - XTT
!
!
!     *Melt* ice if energy and ice available:
ZPHASEM(:)  = (PTSTEP/ZTAUICE)*MIN((XCI*XRHOLI)*MAX(0.0,ZDELTAT),ZWRLI(:)*(XLMTT*XRHOLW))
!
!     *Freeze* liquid water if energy and water available and do not exceed porosity:
ZPHASEF(:)  = (PTSTEP/ZTAUICE)*MIN((XCI*XRHOLI)*MAX(0.0,-ZDELTAT),ZWRL(:)*(XLMTT*XRHOLW))
ZPHASEF(:)  = min(ZPHASEF(:) , (ZWRLSAT - ZWRLI(:)) * (XLMTT*XRHOLW) ) !!!!! LOOK!!!!!!!!
!
ZPHASE(:) = ZPHASEF(:) - ZPHASEM(:)

!     Update heat content if melting or freezing
PEK%XTL(:) = PEK%XTL(:) + ZPHASE(:)/ZHCAPL(:)
!
!     Get estimate of actual total phase change (J/m3) for equivalent litter water changes:

ZPHASEX(:) = ZPHASE(:)
!
!     Adjust ice and liquid water conents (m3/m3) accordingly :
!
ZWRL (:) = ZWRL (:) - ZPHASEX/(XLMTT*XRHOLW)
ZWRLI(:) = ZWRLI(:) + ZPHASEX/(XLMTT*XRHOLW)
!
! 2.1 Convert to Kg/m2
!    -----------------
!
PEK%XWRL(:) = ZWRL(:)  * PEK%XGNDLITTER(:) * XRHOLW
PEK%XWRLI(:)= ZWRLI(:) * PEK%XGNDLITTER(:) * XRHOLW
!
! 3. Adjust litter ice content for sublimation
!    -----------------------------------------
!
ZELITTERI    = PLELITTERI(:) * (PTSTEP/PLSTT(:))
ZEXCESS(:)   = MAX( 0.0 , ZELITTERI - PEK%XWRLI(:) )
PLITCOR      = ZEXCESS / PTSTEP
PEK%XWRLI(:) = PEK%XWRLI(:) - ( ZELITTERI - ZEXCESS )
!
! 4. Prevent some possible problems
!    ------------------------------
!
PEK%XWGI (:,1) = PEK%XWGI(:,1)- ZEXCESS / (XRHOLW * PDZG(:,1))
!
ZEXCESS(:)     = MAX( 0.0, - PEK%XWGI(:,1) )
PEK%XWGI(:,1)  = PEK%XWGI(:,1) + ZEXCESS(:)
PEK%XWG (:,1)  = PEK%XWG (:,1) - ZEXCESS(:)
PEK%XTG (:,1)  = PEK%XTG (:,1) + ZEXCESS(:) * (XLMTT*XRHOLW)/PSOILHCAPZ(:,1)
!
DO JL=1,INL-1
   ZEXCESS = MAX(0.0,-PEK%XWG(:,JL))
   PEK%XWG(:,JL+1) = PEK%XWG(:,JL+1) - ZEXCESS*PDZG(:,JL)/PDZG(:,JL+1)
   PEK%XWG(:,JL)   = PEK%XWG(:,JL)   + ZEXCESS
ENDDO
!
! 5. Prevent from keeping track of ice in litter
!    -------------------------------------------
!
WHERE (PEK%XWRLI(:) < ZERTOL )
   PEK%XWRL (:) = PEK%XWRL(:) + PEK%XWRLI(:)
   PEK%XTL  (:) = PEK%XTL(:)  + PEK%XWRLI(:) * XLMTT / PEK%XGNDLITTER(:) / ZHCAPL(:)
   ZPHASEC  (:) = PEK%XWRLI(:) * XLMTT / PEK%XGNDLITTER(:)
   PEK%XWRLI(:) = 0.0
ELSEWHERE
   ZPHASEC(:) = 0.0
END WHERE
!
PPHASEL(:)=(ZPHASE(:) + ZPHASEC(:))/PTSTEP*PEK%XGNDLITTER
!
IF (LHOOK) CALL DR_HOOK('ISBA_MEB:ICE_LITTER',1,ZHOOK_HANDLE)
!
END SUBROUTINE ICE_LITTER

!===============================================================================

END SUBROUTINE ISBA_MEB
END MODULE MODI_ISBA_MEB
