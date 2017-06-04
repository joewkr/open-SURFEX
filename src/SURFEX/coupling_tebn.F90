!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###############################################################################
SUBROUTINE COUPLING_TEB_n (DTCO, DST, SLT, TOP, SB, G, CHT, NT, TPN, TIR, BOP, NB, TD, GDM, GRM, &
                           HPROGRAM, HCOUPLING, PTSTEP, KYEAR, KMONTH, KDAY, PTIME, KI, KSV,&
                           KSW, PTSUN, PZENITH, PAZIM, PZREF, PUREF, PZS, PU, PV, PQA, PTA, &
                           PRHOA, PSV, PCO2, HSV, PRAIN, PSN, PLW, PDIR_SW, PSCA_SW,        &
                           PSW_BANDS, PPS, PPA, PSFTQ, PSFTH, PSFTS, PSFCO2, PSFU, PSFV,    &
                           PTRAD, PDIR_ALB, PSCA_ALB, PEMIS, PTSURF, PZ0, PZ0H, PQSURF,     &
                           PPEW_A_COEF, PPEW_B_COEF, PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF, &
                           PPEQ_B_COEF, HTEST     )
!     ###############################################################################
!
!!****  *COUPLING_TEB_n * - Driver for TEB 
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!                  10/2005 (G.Pigeon) transfer of domestic heating
!!      S. Riette   06/2009 Initialisation of XT, XQ, XU and XTKE on canopy levels
!!      S. Riette   01/2010 Use of interpol_sbl to compute 10m wind diagnostic
!!      G. Pigeon   09/2012 CCH_BEM, ROUGH_WALL, ROUGH_ROOF for building conv. coef
!!      G. Pigeon   10/2012 XF_WIN_WIN as arg. of TEB_GARDEN
!!      B. Decharme 09/2012 New wind implicitation
!!      J. Escobar  09/2012 KI not allowed without-interface , replace by KI
!!      V. Masson   08/2013 adds solar panels & occupation calendar
!!      B. Decharme  04/2013 new coupling variables
!!---------------------------------------------------------------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DST_n, ONLY : DST_t
USE MODD_SLT_n, ONLY : SLT_t
!
USE MODD_CH_TEB_n, ONLY : CH_TEB_t
USE MODD_CANOPY_n, ONLY: CANOPY_t
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
USE MODD_TEB_PANEL_n, ONLY : TEB_PANEL_t
USE MODD_TEB_IRRIG_n, ONLY : TEB_IRRIG_t
USE MODD_TEB_n, ONLY : TEB_NP_t
USE MODD_SURFEX_n, ONLY : TEB_DIAG_t
USE MODD_BEM_OPTION_n, ONLY : BEM_OPTIONS_t
USE MODD_BEM_n, ONLY : BEM_NP_t
!
USE MODD_SURFEX_n, ONLY : TEB_GARDEN_MODEL_t
USE MODD_SURFEX_n, ONLY : TEB_GREENROOF_MODEL_t
!
USE MODD_REPROD_OPER, ONLY : CIMPLICIT_WIND
!
USE MODD_CSTS,         ONLY : XRD, XCPD, XP00, XLVTT, XPI, XKARMAN, XG
USE MODD_SURF_PAR,     ONLY : XUNDEF
!                            
USE MODD_DST_SURF
USE MODD_SLT_SURF
!
USE MODE_DSLT_SURF
USE MODE_THERMOS
USE MODE_SBLS
!
USE MODI_AVERAGE_RAD
USE MODI_SM10
USE MODI_ADD_FORECAST_TO_DATE_SURF
USE MODI_DIAG_INLINE_TEB_n
USE MODI_CUMUL_DIAG_TEB_n
USE MODI_CH_AER_DEP
USE MODI_CH_DEP_TOWN
USE MODI_DSLT_DEP
USE MODI_TEB_GARDEN
USE MODI_TEB_CANOPY
! 
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
USE MODI_CANOPY_EVOL
USE MODI_CANOPY_GRID_UPDATE
USE MODI_UTCI_TEB
USE MODI_UTCIC_STRESS
USE MODI_CIRCUMSOLAR_RAD
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DST_t), INTENT(INOUT) :: DST
TYPE(SLT_t), INTENT(INOUT) :: SLT
!
TYPE(CH_TEB_t), INTENT(INOUT) :: CHT 
TYPE(CANOPY_t), INTENT(INOUT) :: SB
TYPE(GRID_t), INTENT(INOUT) :: G
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
TYPE(TEB_PANEL_t), INTENT(INOUT) :: TPN
TYPE(TEB_IRRIG_t), INTENT(INOUT) :: TIR
TYPE(TEB_NP_t), INTENT(INOUT) :: NT
!
TYPE(TEB_DIAG_t), INTENT(INOUT) :: TD
!
TYPE(BEM_OPTIONS_t), INTENT(INOUT) :: BOP 
TYPE(BEM_NP_t), INTENT(INOUT) :: NB
!
TYPE(TEB_GARDEN_MODEL_t), INTENT(INOUT) :: GDM
TYPE(TEB_GREENROOF_MODEL_t), INTENT(INOUT) :: GRM
!
 CHARACTER(LEN=6),    INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=1),    INTENT(IN)  :: HCOUPLING ! type of coupling
                                              ! 'E' : explicit
                                              ! 'I' : implicit
INTEGER,             INTENT(IN)  :: KYEAR     ! current year (UTC)
INTEGER,             INTENT(IN)  :: KMONTH    ! current month (UTC)
INTEGER,             INTENT(IN)  :: KDAY      ! current day (UTC)
REAL,                INTENT(IN)  :: PTIME     ! current time since midnight (UTC, s)
INTEGER,             INTENT(IN)  :: KI        ! number of points
INTEGER,             INTENT(IN)  :: KSV       ! number of scalars
INTEGER,             INTENT(IN)  :: KSW       ! number of short-wave spectral bands
REAL, DIMENSION(KI), INTENT(IN)  :: PTSUN     ! solar time                    (s from midnight)
REAL,                INTENT(IN)  :: PTSTEP    ! atmospheric time-step                 (s)
REAL, DIMENSION(KI), INTENT(IN)  :: PZREF     ! height of T,q forcing                 (m)
REAL, DIMENSION(KI), INTENT(IN)  :: PUREF     ! height of wind forcing                (m)
!
REAL, DIMENSION(KI), INTENT(IN)  :: PTA       ! air temperature forcing               (K)
REAL, DIMENSION(KI), INTENT(IN)  :: PQA       ! air humidity forcing                  (kg/m3)
REAL, DIMENSION(KI), INTENT(IN)  :: PRHOA     ! air density                           (kg/m3)
REAL, DIMENSION(KI,KSV),INTENT(IN) :: PSV     ! scalar variables
!                                             ! chemistry:   first char. in HSV: '#'  (molecule/m3)
!                                             !
 CHARACTER(LEN=6), DIMENSION(KSV),INTENT(IN):: HSV  ! name of all scalar variables
REAL, DIMENSION(KI), INTENT(IN)  :: PU        ! zonal wind                            (m/s)
REAL, DIMENSION(KI), INTENT(IN)  :: PV        ! meridian wind                         (m/s)
REAL, DIMENSION(KI,KSW),INTENT(IN) :: PDIR_SW ! direct  solar radiation (on horizontal surf.)
!                                             !                                       (W/m2)
REAL, DIMENSION(KI,KSW),INTENT(IN) :: PSCA_SW ! diffuse solar radiation (on horizontal surf.)
!                                             !                                       (W/m2)
REAL, DIMENSION(KSW),INTENT(IN)  :: PSW_BANDS ! mean wavelength of each shortwave band (m)
REAL, DIMENSION(KI), INTENT(IN)  :: PZENITH   ! zenithal angle       (radian from the vertical)
REAL, DIMENSION(KI), INTENT(IN)  :: PAZIM     ! azimuthal angle      (radian from North, clockwise)
REAL, DIMENSION(KI), INTENT(IN)  :: PLW       ! longwave radiation (on horizontal surf.)
!                                             !                                       (W/m2)
REAL, DIMENSION(KI), INTENT(IN)  :: PPS       ! pressure at atmospheric model surface (Pa)
REAL, DIMENSION(KI), INTENT(IN)  :: PPA       ! pressure at forcing level             (Pa)
REAL, DIMENSION(KI), INTENT(IN)  :: PZS       ! atmospheric model orography           (m)
REAL, DIMENSION(KI), INTENT(IN)  :: PCO2      ! CO2 concentration in the air          (kg/m3)
REAL, DIMENSION(KI), INTENT(IN)  :: PSN     ! snow precipitation                    (kg/m2/s)
REAL, DIMENSION(KI), INTENT(IN)  :: PRAIN     ! liquid precipitation                  (kg/m2/s)
!
!
REAL, DIMENSION(KI), INTENT(OUT) :: PSFTH     ! flux of heat                          (W/m2)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFTQ     ! flux of water vapor                   (kg/m2/s)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFU      ! zonal momentum flux                   (Pa)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFV      ! meridian momentum flux                (Pa)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFCO2    ! flux of CO2                           (kg/m2/s)
REAL, DIMENSION(KI,KSV),INTENT(OUT):: PSFTS   ! flux of scalar var.                   (kg/m2/s)
!
REAL, DIMENSION(KI), INTENT(OUT) :: PTRAD     ! radiative temperature                 (K)
REAL, DIMENSION(KI,KSW),INTENT(OUT):: PDIR_ALB! direct albedo for each spectral band  (-)
REAL, DIMENSION(KI,KSW),INTENT(OUT):: PSCA_ALB! diffuse albedo for each spectral band (-)
REAL, DIMENSION(KI), INTENT(OUT) :: PEMIS     ! emissivity                            (-)
!
REAL, DIMENSION(KI), INTENT(OUT) :: PTSURF    ! surface effective temperature         (K)
REAL, DIMENSION(KI), INTENT(OUT) :: PZ0       ! roughness length for momentum         (m)
REAL, DIMENSION(KI), INTENT(OUT) :: PZ0H      ! roughness length for heat             (m)
REAL, DIMENSION(KI), INTENT(OUT) :: PQSURF    ! specific humidity at surface          (kg/kg)
!
REAL, DIMENSION(KI), INTENT(IN) :: PPEW_A_COEF! implicit coefficients
REAL, DIMENSION(KI), INTENT(IN) :: PPEW_B_COEF! needed if HCOUPLING='I'
REAL, DIMENSION(KI), INTENT(IN) :: PPET_A_COEF
REAL, DIMENSION(KI), INTENT(IN) :: PPEQ_A_COEF
REAL, DIMENSION(KI), INTENT(IN) :: PPET_B_COEF
REAL, DIMENSION(KI), INTENT(IN) :: PPEQ_B_COEF
 CHARACTER(LEN=2),    INTENT(IN) :: HTEST ! must be equal to 'OK'
!
!
!*      0.2    declarations of local variables
!
INTEGER                     :: JSWB        ! loop counter on shortwave spectral bands
!         
REAL, DIMENSION(KI)  :: ZQA         ! specific humidity                 (kg/kg)
REAL, DIMENSION(KI)  :: ZEXNA       ! Exner function at forcing level
REAL, DIMENSION(KI)  :: ZEXNS       ! Exner function at surface level
REAL, DIMENSION(KI)  :: ZWIND       ! wind
!
! Ouput Diagnostics:
!
REAL, DIMENSION(KI)  :: ZU_CANYON   ! wind in canyon
REAL, DIMENSION(KI)  :: ZT_CANYON   ! temperature in canyon
REAL, DIMENSION(KI)  :: ZQ_CANYON   ! specific humidity in canyon
REAL, DIMENSION(KI)  :: ZAVG_T_CANYON ! temperature in canyon for town 
REAL, DIMENSION(KI)  :: ZAVG_Q_CANYON ! specific humidity in canyon for town
REAL, DIMENSION(KI)  :: ZT_CAN      ! temperature in canyon       (evolving in TEB)
REAL, DIMENSION(KI)  :: ZQ_CAN      ! specific humidity in canyon (evolving in TEB)
!
REAL, DIMENSION(KI)  :: ZPEW_A_COEF   ! implicit coefficients
REAL, DIMENSION(KI)  :: ZPEW_B_COEF   ! needed if HCOUPLING='I'
!
REAL, DIMENSION(KI) :: ZT_LOWCAN  ! temperature at lowest canyon level (K)
REAL, DIMENSION(KI) :: ZQ_LOWCAN  ! humidity    at lowest canyon level (kg/kg)
REAL, DIMENSION(KI) :: ZU_LOWCAN  ! wind        at lowest canyon level (m/s)
REAL, DIMENSION(KI) :: ZZ_LOWCAN  ! height      of lowest canyon level (m)
!
REAL, DIMENSION(KI) :: ZPEW_A_COEF_LOWCAN   ! implicit coefficients for wind coupling
REAL, DIMENSION(KI) :: ZPEW_B_COEF_LOWCAN   ! between first canopy level and road
!
REAL, DIMENSION(KI) :: ZTA        ! temperature at canyon level just above roof (K)
REAL, DIMENSION(KI) :: ZPA        ! pressure    at canyon level just above roof (K)
REAL, DIMENSION(KI) :: ZUA        ! wind        at canyon level just above roof (m/s)
REAL, DIMENSION(KI) :: ZUREF      ! height      of canyon level just above roof (m)
REAL, DIMENSION(KI) :: ZZREF      ! height      of canyon level just above roof (m)
!
REAL, DIMENSION(KI)  :: ZDIR_SW       ! total direct SW
REAL, DIMENSION(KI)  :: ZSCA_SW       ! total diffuse SW
REAL, DIMENSION(KI) :: ZAVG_SCA_SW
REAL, DIMENSION(KI) :: ZAVG_DIR_SW 
REAL, DIMENSION(KI,SIZE(PDIR_SW,2))  :: ZDIR_SWB ! total direct SW per band
REAL, DIMENSION(KI,SIZE(PSCA_SW,2))  :: ZSCA_SWB ! total diffuse SW per band
!
!
REAL, DIMENSION(KI)  :: ZLE_WL_A  ! latent heat flux on walls
REAL, DIMENSION(KI)  :: ZLE_WL_B  ! latent heat flux on walls
REAL, DIMENSION(KI)  :: ZAVG_H_WL
!
REAL, DIMENSION(KI)  :: ZPROD_BLD        ! averaged     energy production from solar panel (W/m2 bld)
REAL, DIMENSION(KI) :: ZHU_BLD
REAL, DIMENSION(KI) :: ZAVG_TI_BLD
REAL, DIMENSION(KI) :: ZAVG_QI_BLD
!
REAL, DIMENSION(KI)  :: ZRN_GRND    ! net radiation on ground built surf
REAL, DIMENSION(KI)  :: ZH_GRND     ! sensible heat flux on ground built surf
REAL, DIMENSION(KI)  :: ZLE_GRND    ! latent heat flux on ground built surf
REAL, DIMENSION(KI)  :: ZGFLX_GRND ! storage flux in ground built surf
REAL, DIMENSION(KI)  :: ZUW_GRND      ! momentum flux for ground built surf
REAL, DIMENSION(KI)  :: ZDUWDU_GRND   !
REAL, DIMENSION(KI)  :: ZAC_GRND      ! ground built surf aerodynamical conductance
REAL, DIMENSION(KI)  :: ZAC_GRND_WAT  ! ground built surf water aerodynamical conductance
REAL, DIMENSION(KI) :: ZEMIT_LW_GRND
REAL, DIMENSION(KI) :: ZREF_SW_GRND ! total solar rad reflected from ground
REAL, DIMENSION(KI)  :: ZAVG_UW_GRND
REAL, DIMENSION(KI)  :: ZAVG_DUWDU_GRND
REAL, DIMENSION(KI)  :: ZAVG_H_GRND
REAL, DIMENSION(KI)  :: ZAVG_AC_GRND
REAL, DIMENSION(KI)  :: ZAVG_AC_GRND_WAT
REAL, DIMENSION(KI)  :: ZAVG_E_GRND
REAL, DIMENSION(KI) :: ZAVG_REF_SW_GRND
REAL, DIMENSION(KI) :: ZAVG_EMIT_LW_GRND
!
REAL, DIMENSION(KI)  :: ZLEW_RF   ! latent heat flux on snowfree roof
REAL, DIMENSION(KI)  :: ZRNSN_RF  ! net radiation over snow
REAL, DIMENSION(KI)  :: ZHSN_RF   ! sensible heat flux over snow
REAL, DIMENSION(KI)  :: ZLESN_RF  ! latent heat flux over snow
REAL, DIMENSION(KI)  :: ZGSN_RF   ! flux under the snow
REAL, DIMENSION(KI)  :: ZMELT_RF    ! snow melt
REAL, DIMENSION(KI)  :: ZUW_RF      ! momentum flux for roofs
REAL, DIMENSION(KI)  :: ZDUWDU_RF   !
REAL, DIMENSION(KI)  :: ZAVG_UW_RF
REAL, DIMENSION(KI)  :: ZAVG_DUWDU_RF
REAL, DIMENSION(KI)  :: ZAVG_H_RF
REAL, DIMENSION(KI)  :: ZAVG_E_RF
!
REAL, DIMENSION(KI)  :: ZLEW_RD   ! latent heat flux on snowfree road
REAL, DIMENSION(KI)  :: ZRNSN_RD  ! net radiation over snow
REAL, DIMENSION(KI)  :: ZHSN_RD   ! sensible heat flux over snow
REAL, DIMENSION(KI)  :: ZLESN_RD  ! latent heat flux over snow
REAL, DIMENSION(KI)  :: ZGSN_RD   ! flux under the snow
REAL, DIMENSION(KI)  :: ZMELT_RD    ! snow melt
REAL, DIMENSION(KI)  :: ZAC_RD      ! road aerodynamical conductance
REAL, DIMENSION(KI)  :: ZAC_RD_WAT  ! road water aerodynamical conductance
!
REAL, DIMENSION(KI)  :: ZAC_GD    ! green area aerodynamical conductance
REAL, DIMENSION(KI)  :: ZAC_GD_WAT! green area water aerodynamical conductance
REAL, DIMENSION(KI,1):: ZESN_GD    ! green area snow emissivity
!
REAL, DIMENSION(KI)  :: ZAC_GRF ! green roof aerodynamical conductance
REAL, DIMENSION(KI)  :: ZAC_GRF_WAT! green roof water aerodynamical conductance
!
REAL, DIMENSION(KI)  :: ZTRAD         ! radiative temperature for current patch
REAL, DIMENSION(KI)  :: ZEMIS         ! emissivity for current patch
REAL, DIMENSION(KI,TOP%NTEB_PATCH) :: ZTRAD_PATCH ! radiative temperature for each patch
REAL, DIMENSION(KI,TOP%NTEB_PATCH) :: ZEMIS_PATCH ! emissivity for each patch
!
REAL, DIMENSION(KI)  :: ZDIR_ALB      ! direct albedo of town
REAL, DIMENSION(KI)  :: ZSCA_ALB      ! diffuse albedo of town
REAL, DIMENSION(KI,KSW,TOP%NTEB_PATCH) :: ZDIR_ALB_PATCH ! direct albedo per wavelength and patch
REAL, DIMENSION(KI,KSW,TOP%NTEB_PATCH) :: ZSCA_ALB_PATCH ! diffuse albedo per wavelength and patch
REAL, DIMENSION(KI)  :: ZAVG_DIR_ALB  ! direct albedo of town
REAL, DIMENSION(KI)  :: ZAVG_SCA_ALB  ! diffuse albedo of town
!
REAL, DIMENSION(KI)  :: ZSFCO2        ! CO2 flux over town
!
REAL, DIMENSION(KI)  :: ZRI           ! Richardson number
REAL, DIMENSION(KI)  :: ZCD           ! drag coefficient
REAL, DIMENSION(KI)  :: ZCDN          ! neutral drag coefficient
REAL, DIMENSION(KI)  :: ZCH           ! heat drag
REAL, DIMENSION(KI)  :: ZRN           ! net radiation over town
REAL, DIMENSION(KI)  :: ZH            ! sensible heat flux over town
REAL, DIMENSION(KI)  :: ZLE           ! latent heat flux over town
REAL, DIMENSION(KI)  :: ZGFLX        ! flux through the ground
REAL, DIMENSION(KI)  :: ZEVAP         ! evaporation (km/m2/s)
!
REAL, DIMENSION(KI)  :: ZAVG_CD       ! aggregated drag coefficient
REAL, DIMENSION(KI)  :: ZAVG_CDN      ! aggregated neutral drag coefficient
REAL, DIMENSION(KI)  :: ZAVG_RI       ! aggregated Richardson number
REAL, DIMENSION(KI)  :: ZAVG_CH       ! aggregated Heat transfer coefficient
!
REAL, DIMENSION(KI)  :: ZUSTAR        ! friction velocity
REAL, DIMENSION(KI)  :: ZSFU          ! momentum flux for patch (U direction)
REAL, DIMENSION(KI)  :: ZSFV          ! momentum flux for patch (V direction)
!
REAL, DIMENSION(KI)  :: ZH_TRAFFIC    ! anthropogenic sensible
!                                     ! heat fluxes due to traffic
REAL, DIMENSION(KI)  :: ZLE_TRAFFIC   ! anthropogenic latent
!                                     ! heat fluxes due to traffic
!
REAL                 :: ZBEGIN_TRAFFIC_TIME ! start traffic time (solar time, s)
REAL                 :: ZEND_TRAFFIC_TIME   ! end traffic time   (solar time, s)
!
REAL, DIMENSION(KI)  :: ZRESA    ! aerodynamical resistance
!
REAL, DIMENSION(KI) :: ZEMIT_LW_FAC
REAL, DIMENSION(KI) :: ZT_RAD_IND   ! Indoor mean radiant temperature [K]
REAL, DIMENSION(KI) :: ZREF_SW_FAC  ! total solar rad reflected from facade
!
REAL, DIMENSION(KI) :: ZAVG_Z0
REAL, DIMENSION(KI) :: ZAVG_RESA
REAL, DIMENSION(KI) :: ZAVG_USTAR        ! town avegared Ustar
REAL, DIMENSION(KI) :: ZAVG_BLD          ! town averaged building fraction
REAL, DIMENSION(KI) :: ZAVG_BLD_HEIGHT   ! town averaged building height
REAL, DIMENSION(KI) :: ZAVG_WL_O_HOR     ! town averaged Wall/hor ratio
REAL, DIMENSION(KI) :: ZAVG_CAN_HW_RATIO ! town averaged road aspect ratio
REAL, DIMENSION(KI) :: ZAVG_H
REAL, DIMENSION(KI) :: ZAVG_LE
REAL, DIMENSION(KI) :: ZAVG_RN
REAL, DIMENSION(KI) :: ZAVG_GFLX
REAL, DIMENSION(KI) :: ZAVG_REF_SW_FAC
REAL, DIMENSION(KI) :: ZAVG_EMIT_LW_FAC
REAL, DIMENSION(KI) :: ZAVG_T_RAD_IND
!
! absorbed solar and infra-red radiation by road, wall and roof
!
REAL, DIMENSION(KI) :: ZU_UTCI ! wind speed for the UTCI calculation (m/s) 

REAL, DIMENSION(KI) :: ZALFAU   ! V+(1) = alfa u'w'(1) + beta
REAL, DIMENSION(KI) :: ZBETAU   ! V+(1) = alfa u'w'(1) + beta
REAL, DIMENSION(KI) :: ZALFAT   ! Th+(1) = alfa w'th'(1) + beta
REAL, DIMENSION(KI) :: ZBETAT   ! Th+(1) = alfa w'th'(1) + beta
REAL, DIMENSION(KI) :: ZALFAQ   ! Q+(1) = alfa w'q'(1) + beta
REAL, DIMENSION(KI) :: ZBETAQ   ! Q+(1) = alfa w'q'(1) + beta
!***** CANOPY  *****
REAL, DIMENSION(KI) :: ZWAKE      ! reduction of average wind speed
!                                              ! in canyon due to direction average.

!new local variables for UTCI calculation
REAL, DIMENSION(KI) :: ZF1_o_B
!
!***** CANOPY  *****
REAL, DIMENSION(KI) :: ZSFLUX_U  ! Surface flux u'w' (m2/s2)
REAL, DIMENSION(KI) :: ZSFLUX_T  ! Surface flux w'T' (mK/s)
REAL, DIMENSION(KI) :: ZSFLUX_Q  ! Surface flux w'q' (kgm2/s)
REAL, DIMENSION(KI,SB%NLVL)   :: ZFORC_U   ! tendency due to drag force for wind
REAL, DIMENSION(KI,SB%NLVL)   :: ZDFORC_UDU! formal derivative of
!                                              ! tendency due to drag force for wind
REAL, DIMENSION(KI,SB%NLVL)   :: ZFORC_E   ! tendency due to drag force for TKE
REAL, DIMENSION(KI,SB%NLVL)   :: ZDFORC_EDE! formal derivative of
!                                              ! tendency due to drag force for TKE
REAL, DIMENSION(KI,SB%NLVL)   :: ZFORC_T   ! tendency due to drag force for Temp
REAL, DIMENSION(KI,SB%NLVL)   :: ZDFORC_TDT! formal derivative of
!                                              ! tendency due to drag force for Temp
REAL, DIMENSION(KI,SB%NLVL)   :: ZFORC_Q   ! tendency due to drag force for hum
REAL, DIMENSION(KI,SB%NLVL)   :: ZDFORC_QDQ! formal derivative of
!                                           ! tendency due to drag force for hum.
REAL, DIMENSION(KI) :: ZLAMBDA_F  ! frontal density (-)
REAL, DIMENSION(KI) :: ZLMO       ! Monin-Obukhov length at canopy height (m)
REAL, DIMENSION(KI,SB%NLVL)   :: ZL         ! Mixing length generic profile at mid levels
!
REAL, DIMENSION(KI) :: ZCOEF
!
REAL                       :: ZCONVERTFACM0_SLT, ZCONVERTFACM0_DST
REAL                       :: ZCONVERTFACM3_SLT, ZCONVERTFACM3_DST
REAL                       :: ZCONVERTFACM6_SLT, ZCONVERTFACM6_DST
!
INTEGER                           :: JI
INTEGER                           :: JLAYER
INTEGER                           :: JJ
!
! number of TEB patches
!
INTEGER                    :: JP, IBEG, IEND ! loop counter
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
! Preliminaries:
!-------------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('COUPLING_TEB_N',0,ZHOOK_HANDLE)
IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('COUPLING_TEBN: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF

!-------------------------------------------------------------------------------------
!
! scalar fluxes
!
PSFTS(:,:) = 0.
!
! broadband radiative fluxes
!
ZDIR_SW(:) = 0.
ZSCA_SW(:) = 0.
DO JSWB=1,KSW
  !add directionnal contrib from scattered radiation
  CALL CIRCUMSOLAR_RAD(PDIR_SW(:,JSWB), PSCA_SW(:,JSWB), PZENITH, ZF1_o_B)
  ZDIR_SWB(:,JSWB) = PDIR_SW(:,JSWB) + PSCA_SW(:,JSWB) * ZF1_o_B
  ZSCA_SWB(:,JSWB) = PSCA_SW(:,JSWB) * (1. - ZF1_o_B)
  !add directionnal contrib from scattered radiation
  DO JJ=1,SIZE(PDIR_SW,1)
    ZDIR_SW(JJ) = ZDIR_SW(JJ) + ZDIR_SWB(JJ,JSWB)
    ZSCA_SW(JJ) = ZSCA_SW(JJ) + ZSCA_SWB(JJ,JSWB)
  ENDDO
END DO
!
DO JJ=1,KI
! specific humidity (conversion from kg/m3 to kg/kg)
!
  ZQA(JJ) = PQA(JJ) / PRHOA(JJ)
!
! wind
!
  ZWIND(JJ) = SQRT(PU(JJ)**2+PV(JJ)**2)
!
ENDDO
! method of wind coupling
!
IF (HCOUPLING=='I') THEN
  ZPEW_A_COEF = PPEW_A_COEF
  ZPEW_B_COEF = PPEW_B_COEF
ELSE
  ZPEW_A_COEF =  0.
  ZPEW_B_COEF =  ZWIND
END IF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Time evolution
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
TOP%TTIME%TIME = TOP%TTIME%TIME + PTSTEP
 CALL ADD_FORECAST_TO_DATE_SURF(TOP%TTIME%TDATE%YEAR, TOP%TTIME%TDATE%MONTH,&
                                TOP%TTIME%TDATE%DAY, TOP%TTIME%TIME)
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  Anthropogenic fluxes (except building heating)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
ZBEGIN_TRAFFIC_TIME = 21600.
ZEND_TRAFFIC_TIME   = 64800.
!
WHERE( PTSUN>ZBEGIN_TRAFFIC_TIME  .AND.  PTSUN<ZEND_TRAFFIC_TIME  )
  ZH_TRAFFIC  (:) = NT%AL(1)%XH_TRAFFIC   (:)
  ZLE_TRAFFIC (:) = NT%AL(1)%XLE_TRAFFIC  (:)
ELSEWHERE
  ZH_TRAFFIC  (:) = 0.
  ZLE_TRAFFIC (:) = 0.   
END WHERE
!
!--------------------------------------------------------------------------------------
!  Canyon forcing for TEB
!--------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
! Town averaged quantities to force canopy atmospheric layers
!-------------------------------------------------------------------------------------

DO JP=1,TOP%NTEB_PATCH
  CALL ADD_PATCH_CONTRIB(JP,ZAVG_BLD,         NT%AL(JP)%XBLD         )
  CALL ADD_PATCH_CONTRIB(JP,ZAVG_BLD_HEIGHT,  NT%AL(JP)%XBLD_HEIGHT  )
  CALL ADD_PATCH_CONTRIB(JP,ZAVG_WL_O_HOR,    NT%AL(JP)%XWALL_O_HOR  )
  CALL ADD_PATCH_CONTRIB(JP,ZAVG_CAN_HW_RATIO,NT%AL(JP)%XCAN_HW_RATIO)
  CALL ADD_PATCH_CONTRIB(JP,ZAVG_Z0,          NT%AL(JP)%XZ0_TOWN     )
END DO
!
IF (TOP%LCANOPY) THEN
  !-------------------------------------------------------------------------------------
  ! Updates canopy vertical grid as a function of forcing height
  !-------------------------------------------------------------------------------------
  !
  !* determines where is the forcing level and modifies the upper levels of the canopy grid
  !
  CALL CANOPY_GRID_UPDATE(KI, ZAVG_BLD_HEIGHT, ZAVG_BLD_HEIGHT+PUREF, SB)
  !
  !* Initialisations of T, Q, TKE and wind at first time step
  !
  IF(ANY(SB%XT(:,:) == XUNDEF)) THEN
    DO JLAYER=1,SB%NLVL
      SB%XT(:,JLAYER) = PTA(:)
      SB%XQ(:,JLAYER) = PQA(:)
      SB%XU(:,JLAYER) = 2./XPI * ZWIND(:)                                  &
              * LOG( (          2.* NT%AL(1)%XBLD_HEIGHT(:)/3.) / NT%AL(1)%XZ0_TOWN(:))   &
              / LOG( (PUREF(:)+ 2.* NT%AL(1)%XBLD_HEIGHT(:)/3.) / NT%AL(1)%XZ0_TOWN(:))
    END  DO
    SB%XTKE(:,:) = 1.
  ENDIF
  !
  !* default forcing above roof: forcing level
  ZUREF(:) = PUREF(:)
  ZZREF(:) = PZREF(:)
  ZUA(:)   = SB%XU(:,SB%NLVL)
  ZTA(:)   = SB%XT(:,SB%NLVL)
  ZQA(:)   = SB%XQ(:,SB%NLVL)/PRHOA(:)
  ZPA(:)   = SB%XP(:,SB%NLVL)
  !* for the time being, only one value is kept for wall in-canyon forcing, in the middle of the canyon
  ZU_CANYON(:) = ZUA(:)
  ZT_CANYON(:) = ZTA(:)
  ZQ_CANYON(:) = ZQA(:)
  DO JLAYER=1,SB%NLVL-1
    DO JI=1,KI
      !* finds middle canyon layer
      IF (SB%XZ(JI,JLAYER)<ZAVG_BLD_HEIGHT(JI)/2. .AND. SB%XZ(JI,JLAYER+1)>=ZAVG_BLD_HEIGHT(JI)/2.) THEN
        ZCOEF(JI) = (ZAVG_BLD_HEIGHT(JI)/2.-SB%XZ(JI,JLAYER))/(SB%XZ(JI,JLAYER+1)-SB%XZ(JI,JLAYER))
        ZU_CANYON(JI) = SB%XU(JI,JLAYER) + ZCOEF(JI) * (SB%XU(JI,JLAYER+1)-SB%XU(JI,JLAYER))
        ZT_CANYON(JI) = SB%XT(JI,JLAYER) + ZCOEF(JI) * (SB%XT(JI,JLAYER+1)-SB%XT(JI,JLAYER))
        ZQ_CANYON(JI) =(SB%XQ(JI,JLAYER) + ZCOEF(JI) * (SB%XQ(JI,JLAYER+1)-SB%XQ(JI,JLAYER)))/PRHOA(JI)
      END IF
      !* finds layer just above roof (at least 1m above roof)
      IF (SB%XZ(JI,JLAYER)<ZAVG_BLD_HEIGHT(JI)+1. .AND. SB%XZ(JI,JLAYER+1)>=ZAVG_BLD_HEIGHT(JI)+1.) THEN
        ZUREF(JI) = SB%XZ(JI,JLAYER+1) - ZAVG_BLD_HEIGHT(JI)
        ZZREF(JI) = SB%XZ(JI,JLAYER+1) - ZAVG_BLD_HEIGHT(JI)
        ZTA  (JI) = SB%XT(JI,JLAYER+1)
        ZQA  (JI) = SB%XQ(JI,JLAYER+1)/PRHOA(JI)
        !ZUA  (JI) = XU(JI,JLAYER+1)
        ZUA  (JI) = MAX(SB%XU(JI,JLAYER+1) - 2.*SQRT(SB%XTKE(JI,JLAYER+1)) , SB%XU(JI,JLAYER+1)/3.)
        ZPA  (JI) = SB%XP(JI,JLAYER+1)
        ZLMO (JI) = SB%XLMO(JI,JLAYER+1)
      END IF
    END DO
  END DO
  ZU_CANYON= MAX(ZU_CANYON,0.2)
  ZU_LOWCAN=SB%XU(:,1)
  ZT_LOWCAN=SB%XT(:,1)
  ZQ_LOWCAN=SB%XQ(:,1) / PRHOA(:)
  ZZ_LOWCAN=SB%XZ(:,1)
  WHERE(ZPA==XUNDEF) ZPA = PPA   ! security for first time step
  !
  !-------------------------------------------------------------------------------------
  ! determine the vertical profile for mixing and dissipative lengths (at full levels)
  !-------------------------------------------------------------------------------------
  !
  ! frontal density
  ZLAMBDA_F(:) = ZAVG_CAN_HW_RATIO*ZAVG_BLD / (0.5*XPI)
  !
  CALL SM10(SB%XZ, ZAVG_BLD_HEIGHT, ZLAMBDA_F, ZL)
  !
  !-------------------------------------------------------------------------------------
  ! computes coefficients for implicitation
  !-------------------------------------------------------------------------------------
  !
  ZAVG_UW_GRND(:)    = 0.
  ZAVG_DUWDU_GRND(:) = 0.
  ZAVG_UW_RF(:)      = 0.
  ZAVG_DUWDU_RF(:)   = 0.
  ZAVG_H_GRND(:)     = 0.
  ZAVG_H_WL(:)       = 0.
  ZAVG_H_RF(:)       = 0.
  ZAVG_E_GRND(:)     = 0.
  ZAVG_E_RF(:)       = 0.
  ZAVG_AC_GRND(:)    = 0.
  ZAVG_AC_GRND_WAT(:)= 0.
  ZSFLUX_U(:)        = 0.
  ZSFLUX_T(:)        = 0.
  ZSFLUX_Q(:)        = 0.
  !
  DO JLAYER=1,SB%NLVL-1
    !* Monin-Obuhkov theory not used inside the urban canopy
    ! => neutral mixing  if layer is below : (roof level +1 meter)
    WHERE (SB%XZ(:,JLAYER)<=ZAVG_BLD_HEIGHT(:)+1.) SB%XLMO(:,JLAYER) = XUNDEF
  ENDDO
  !
  !* computes tendencies on wind and Tke due to canopy
  CALL TEB_CANOPY(KI, SB, ZAVG_BLD, ZAVG_BLD_HEIGHT, ZAVG_WL_O_HOR, PPA, PRHOA, &
                  ZAVG_DUWDU_GRND, ZAVG_UW_RF, ZAVG_DUWDU_RF, ZAVG_H_WL,         &
                  ZAVG_H_RF, ZAVG_E_RF, ZAVG_AC_GRND, ZAVG_AC_GRND_WAT, ZFORC_U, &
                  ZDFORC_UDU, ZFORC_E, ZDFORC_EDE, ZFORC_T, ZDFORC_TDT, ZFORC_Q, &
                  ZDFORC_QDQ )
  !
  !* computes coefficients for implicitation
  CALL CANOPY_EVOL(SB, KI, PTSTEP, 1, ZL, ZWIND, PTA, PQA, PPA, PRHOA, &
                   ZSFLUX_U, ZSFLUX_T, ZSFLUX_Q, ZFORC_U, ZDFORC_UDU,   &
                   ZFORC_E, ZDFORC_EDE, ZFORC_T, ZDFORC_TDT, ZFORC_Q,   &
                   ZDFORC_QDQ, SB%XLM, SB%XLEPS, ZAVG_USTAR, ZALFAU,  &
                   ZBETAU, ZALFAT, ZBETAT, ZALFAQ, ZBETAQ)
  !
  ZPEW_A_COEF_LOWCAN = - ZALFAU / PRHOA
  ZPEW_B_COEF_LOWCAN = ZBETAU  
  !
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
ELSE              ! no canopy case
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  DO JI=1,KI
    !* skimming flow for h/w>1 (maximum effect of direction on wind in the canyon);
    !* isolated flow for h/w<0.5 (wind is the same in large streets for all dir.)
    !* wake flow between.
    !
    ZWAKE(JI)= 1. + (2./XPI-1.) * 2. * (ZAVG_CAN_HW_RATIO(JI)-0.5)
    ZWAKE(JI)= MAX(MIN(ZWAKE(JI),1.),2./XPI)
    !
    !* Estimation of canyon wind speed from wind just above roof level
    !  (at 1.33h). Wind at 1.33h is estimated using the log law.
    !
    IF (ZAVG_BLD_HEIGHT(JI) .GT. 0.) THEN
      ZU_CANYON(JI) = ZWAKE(JI) * EXP(-ZAVG_CAN_HW_RATIO(JI)/4.) * ZWIND(JI)     &
                  * LOG( (           2.* ZAVG_BLD_HEIGHT(JI)/3.) / ZAVG_Z0(JI))   &
                  / LOG( (PUREF(JI)+ 2.* ZAVG_BLD_HEIGHT(JI)/3.) / ZAVG_Z0(JI))
      ZZ_LOWCAN(JI) = ZAVG_BLD_HEIGHT(JI) / 2.
    ELSE
      ZU_CANYON(JI) = ZWIND(JI)
      ZZ_LOWCAN(JI) = PZREF(JI)
    ENDIF
  END DO
  !
  !* Without SBL scheme, canyon air is assumed at mid height
  ZU_LOWCAN = ZU_CANYON

  ZT_LOWCAN = NT%AL(1)%XT_CANYON
  ZQ_LOWCAN = NT%AL(1)%XQ_CANYON
  ZT_CANYON = NT%AL(1)%XT_CANYON
  ZQ_CANYON = NT%AL(1)%XQ_CANYON

  ZUREF     = PUREF
  ZZREF     = PZREF
  ZTA       = PTA
  ZUA       = ZWIND
  ZPA       = PPA
  ZPEW_A_COEF_LOWCAN =  0.
  ZPEW_B_COEF_LOWCAN =  ZU_CANYON

END IF
!
! Exner functions
!
ZEXNS     (:) = (PPS(:)/XP00)**(XRD/XCPD)
ZEXNA     (:) = (ZPA(:)/XP00)**(XRD/XCPD)

!--------------------------------------------------------------------------------------
! Over Urban surfaces/towns:
!--------------------------------------------------------------------------------------
!
DO JP = 1,TOP%NTEB_PATCH
  !
  ZT_CAN = ZT_CANYON
  ZQ_CAN = ZQ_CANYON
  !
  IF (TOP%LCANOPY) THEN
    NT%AL(JP)%XT_CANYON(:) = ZT_CANYON(:)
    NT%AL(JP)%XQ_CANYON(:) = ZQ_CANYON(:)
  END IF
  !
  ZLESN_RF(:) = 0.
  ZLESN_RD(:) = 0.
  TD%NDMT%AL(JP)%XG_GREENROOF_ROOF(:) = 0.
  !
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! Call the physical routines of TEB (including gardens & greenroofs)
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
  CALL TEB_GARDEN(DTCO, G, TOP, NT%AL(JP), BOP, NB%AL(JP), TPN, TIR, TD%NDMT%AL(JP), GDM, GRM, JP, &
                  CIMPLICIT_WIND, PTSUN, ZT_CAN, ZQ_CAN, ZU_CANYON, ZT_LOWCAN, ZQ_LOWCAN, &
                  ZU_LOWCAN, ZZ_LOWCAN, ZPEW_A_COEF, ZPEW_B_COEF, ZPEW_A_COEF_LOWCAN,     &
                  ZPEW_B_COEF_LOWCAN, PPS, ZPA, ZEXNS, ZEXNA, ZTA, ZQA, PRHOA, PCO2, PLW, &
                  ZDIR_SWB, ZSCA_SWB, PSW_BANDS, KSW, PZENITH, PAZIM, PRAIN, PSN, ZZREF,  &
                  ZUREF, ZUA, ZH_TRAFFIC, ZLE_TRAFFIC, PTSTEP, ZLEW_RF, ZLEW_RD, ZLE_WL_A,&
                  ZLE_WL_B, ZRNSN_RF, ZHSN_RF, ZLESN_RF, ZGSN_RF, ZMELT_RF, ZRNSN_RD,     &
                  ZHSN_RD, ZLESN_RD, ZGSN_RD, ZMELT_RD, ZRN_GRND, ZH_GRND, ZLE_GRND,      &
                  ZGFLX_GRND, ZRN, ZH, ZLE, ZGFLX, ZEVAP, ZSFCO2, ZUW_GRND,               &
                  ZUW_RF, ZDUWDU_GRND, ZDUWDU_RF, ZUSTAR, ZCD, ZCDN, ZCH, ZRI, ZTRAD,     &
                  ZEMIS, ZDIR_ALB, ZSCA_ALB, ZRESA, ZAC_RD, ZAC_GD, ZAC_GRF, ZAC_RD_WAT,  &
                  ZAC_GD_WAT, ZAC_GRF_WAT, KDAY, ZEMIT_LW_FAC, ZEMIT_LW_GRND, ZT_RAD_IND, &
                  ZREF_SW_GRND, ZREF_SW_FAC, ZHU_BLD, PTIME, ZPROD_BLD       )


  !
  IF (.NOT. TOP%LCANOPY) THEN

    CALL ADD_PATCH_CONTRIB(JP,ZAVG_T_CANYON,ZT_CAN)
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_Q_CANYON,ZQ_CAN)
    !
    ! Momentum fluxes
    !
    ZSFU = 0.
    ZSFV = 0.
    DO JJ=1,SIZE(PU)
      IF (ZWIND(JJ)>0.) THEN
        ZCOEF(JJ) = - PRHOA(JJ) * ZUSTAR(JJ)**2 / ZWIND(JJ)
        ZSFU(JJ) = ZCOEF(JJ) * PU(JJ)
        ZSFV(JJ) = ZCOEF(JJ) * PV(JJ)
      ENDIF
    ENDDO
    CALL ADD_PATCH_CONTRIB(JP,PSFU,ZSFU)
    CALL ADD_PATCH_CONTRIB(JP,PSFV,ZSFV)
    !
  ENDIF
  !
  !-------------------------------------------------------------------------------------
  ! Outputs:
  !-------------------------------------------------------------------------------------
  !
  ! Grid box average fluxes/properties: Arguments and standard diagnostics
  !
  CALL ADD_PATCH_CONTRIB(JP,PSFTH,ZH)
  CALL ADD_PATCH_CONTRIB(JP,PSFTQ,ZEVAP)
  CALL ADD_PATCH_CONTRIB(JP,PSFCO2,ZSFCO2)
  !
  !
  ! Albedo for each wavelength and patch
  !
  DO JSWB=1,SIZE(PSW_BANDS)
    DO JJ=1,SIZE(ZDIR_ALB)
      ZDIR_ALB_PATCH(JJ,JSWB,JP) = ZDIR_ALB(JJ)
      ZSCA_ALB_PATCH(JJ,JSWB,JP) = ZSCA_ALB(JJ)
    ENDDO
  END DO
  !
  ! emissivity and radiative temperature
  !
  ZEMIS_PATCH(:,JP) = ZEMIS
  ZTRAD_PATCH(:,JP) = ZTRAD
  !
  ! computes some aggregated diagnostics
  !
  CALL ADD_PATCH_CONTRIB(JP,ZAVG_CD ,ZCD )
  CALL ADD_PATCH_CONTRIB(JP,ZAVG_CDN,ZCDN)
  CALL ADD_PATCH_CONTRIB(JP,ZAVG_RI ,ZRI )
  CALL ADD_PATCH_CONTRIB(JP,ZAVG_CH ,ZCH )
  CALL ADD_PATCH_CONTRIB(JP,ZAVG_RN ,ZRN )
  CALL ADD_PATCH_CONTRIB(JP,ZAVG_H  ,ZH  )
  CALL ADD_PATCH_CONTRIB(JP,ZAVG_LE ,ZLE )
  CALL ADD_PATCH_CONTRIB(JP,ZAVG_GFLX ,ZGFLX )
  !
  !* warning: aerodynamical resistance does not yet take into account gardens
  CALL ADD_PATCH_CONTRIB(JP,ZAVG_RESA,1./ZRESA)
  IF (JP==TOP%NTEB_PATCH) ZAVG_RESA = 1./ZAVG_RESA
  !
  !-------------------------------------------------------------------------------------
  ! Diagnostics on each patch
  !-------------------------------------------------------------------------------------
  !
  IF (TD%MTO%LSURF_MISC_BUDGET) THEN
    !
    ! cumulated diagnostics 
    ! ---------------------
    !
    CALL CUMUL_DIAG_TEB_n(TD%NDMTC%AL(JP), TD%NDMT%AL(JP), GDM%VD%NDEC%AL(JP), GDM%VD%NDE%AL(JP), &
                          GRM%VD%NDEC%AL(JP), GRM%VD%NDE%AL(JP), TOP, PTSTEP)
    !
  END IF
  !
  !
  !-------------------------------------------------------------------------------------
  ! Computes averaged parameters necessary for UTCI
  !-------------------------------------------------------------------------------------
  !
  IF (TD%O%N2M >0 .AND. TD%DUT%LUTCI) THEN
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_REF_SW_GRND ,ZREF_SW_GRND )
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_REF_SW_FAC  ,ZREF_SW_FAC  )
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_SCA_SW      ,ZSCA_SW      )
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_DIR_SW      ,ZDIR_SW      )
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_EMIT_LW_FAC ,ZEMIT_LW_FAC )
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_EMIT_LW_GRND,ZEMIT_LW_GRND)
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_T_RAD_IND   ,ZT_RAD_IND   )
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_TI_BLD      ,NB%AL(JP)%XTI_BLD)
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_QI_BLD      ,NB%AL(JP)%XQI_BLD)
  END IF
  !
  !-------------------------------------------------------------------------------------
  ! Use of the canopy version of TEB
  !-------------------------------------------------------------------------------------
  !
  IF (TOP%LCANOPY) THEN
    !-------------------------------------------------------------------------------------
    ! Town averaged quantities to force canopy atmospheric layers
    !-------------------------------------------------------------------------------------

    CALL ADD_PATCH_CONTRIB(JP,ZAVG_DUWDU_GRND, ZDUWDU_GRND )
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_UW_RF     , ZUW_RF)
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_DUWDU_RF  , ZDUWDU_RF)
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_H_WL      , 0.5*(TD%NDMT%AL(JP)%XH_WALL_A+TD%NDMT%AL(JP)%XH_WALL_B))
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_H_RF      , (TD%NDMT%AL(JP)%XH_ROOF + NT%AL(JP)%XH_INDUSTRY))
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_E_RF      , (TD%NDMT%AL(JP)%XLE_ROOF+ NT%AL(JP)%XLE_INDUSTRY)/XLVTT)
    !
    !-------------------------------------------------------------------------------------
    ! Computes the impact of canopy and surfaces on air
    !-------------------------------------------------------------------------------------
    !
    ZAC_GRND    (:) = (NT%AL(JP)%XROAD(:)*ZAC_RD    (:) + NT%AL(JP)%XGARDEN(:)*ZAC_GD    (:)) / &
                      (NT%AL(JP)%XROAD(:)+NT%AL(JP)%XGARDEN(:))
    ZAC_GRND_WAT(:) = (NT%AL(JP)%XROAD(:)*ZAC_RD_WAT(:) + NT%AL(JP)%XGARDEN(:)*ZAC_GD_WAT(:)) / &
                      (NT%AL(JP)%XROAD(:)+NT%AL(JP)%XGARDEN(:))
    !
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_AC_GRND    , ZAC_GRND    )
    CALL ADD_PATCH_CONTRIB(JP,ZAVG_AC_GRND_WAT, ZAC_GRND_WAT)
    CALL ADD_PATCH_CONTRIB(JP,ZSFLUX_U        , ZUW_GRND * (1.-NT%AL(JP)%XBLD))
    CALL ADD_PATCH_CONTRIB(JP,ZSFLUX_T        , ZH_GRND  * (1.-NT%AL(JP)%XBLD)/XCPD/PRHOA)
    CALL ADD_PATCH_CONTRIB(JP,ZSFLUX_Q        , ZLE_GRND * (1.-NT%AL(JP)%XBLD)/XLVTT)
    !
  END IF
  !
  !-------------------------------------------------------------------------------------
  ! end of loop on TEB patches
END DO
!-------------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------------
!* Evolution of canopy air if canopy option is active
!-------------------------------------------------------------------------------------
!
IF (TOP%LCANOPY) THEN
  !
  !-------------------------------------------------------------------------------------
  !* Impact of TEB fluxes on the air
  !-------------------------------------------------------------------------------------
  !
  CALL TEB_CANOPY(KI, SB, ZAVG_BLD, ZAVG_BLD_HEIGHT, ZAVG_WL_O_HOR, PPA, PRHOA, &
                  ZAVG_DUWDU_GRND, ZAVG_UW_RF, ZAVG_DUWDU_RF, ZAVG_H_WL,         &
                  ZAVG_H_RF, ZAVG_E_RF, ZAVG_AC_GRND, ZAVG_AC_GRND_WAT, ZFORC_U, &
                  ZDFORC_UDU, ZFORC_E, ZDFORC_EDE, ZFORC_T, ZDFORC_TDT, ZFORC_Q, &
                  ZDFORC_QDQ )
  !
  !-------------------------------------------------------------------------------------
  !* Evolution of canopy air due to these impacts
  !-------------------------------------------------------------------------------------
  !
  CALL CANOPY_EVOL(SB, KI, PTSTEP, 2, ZL, ZWIND, PTA, PQA, PPA, PRHOA,  &
                   ZSFLUX_U, ZSFLUX_T, ZSFLUX_Q, ZFORC_U, ZDFORC_UDU,    &
                   ZFORC_E, ZDFORC_EDE, ZFORC_T, ZDFORC_TDT, ZFORC_Q,    &
                   ZDFORC_QDQ, SB%XLM, SB%XLEPS, ZAVG_USTAR, ZALFAU,   &
                   ZBETAU, ZALFAT, ZBETAT, ZALFAQ, ZBETAQ      )
  !
  !-------------------------------------------------------------------------------------
  ! Momentum fluxes in the case canopy is active
  !-------------------------------------------------------------------------------------
  !
  PSFU=0.
  PSFV=0.
  ZAVG_Z0(:) = MIN(ZAVG_Z0(:),PUREF(:)*0.5)
  ZAVG_CDN=(XKARMAN/LOG(PUREF(:)/ZAVG_Z0(:)))**2
  ZAVG_CD = ZAVG_CDN
  ZAVG_RI = 0.
  DO JJ=1,SIZE(PU)
    IF (ZWIND(JJ)>0.) THEN
      ZCOEF(JJ) = - PRHOA(JJ) * ZAVG_USTAR(JJ)**2 / ZWIND(JJ)
      PSFU(JJ) = ZCOEF(JJ) * PU(JJ)
      PSFV(JJ) = ZCOEF(JJ) * PV(JJ)
      ZAVG_CD(JJ) = ZAVG_USTAR(JJ)**2 / ZWIND(JJ)**2
      ZAVG_RI(JJ) = -XG/PTA(JJ)*ZSFLUX_T(JJ)/ZAVG_USTAR(JJ)**4
    ENDIF
  ENDDO
  !
  !-------------------------------------------------------------------------------------
  ! End of specific case with canopy option
  !-------------------------------------------------------------------------------------
  !
END IF
!
!-------------------------------------------------------------------------------------
! Outputs:
!-------------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------------
!Radiative properties should be at time t+1 (see by the atmosphere) in order to close
!the energy budget between surfex and the atmosphere. It is not the case here
!for ALB and EMIS
!-------------------------------------------------------------------------------------
!
 CALL AVERAGE_RAD(TOP%XTEB_PATCH, ZDIR_ALB_PATCH, ZSCA_ALB_PATCH, ZEMIS_PATCH, &
                  ZTRAD_PATCH, PDIR_ALB, PSCA_ALB, PEMIS, PTRAD )
!
!-------------------------------------------------------------------------------
!Physical properties see by the atmosphere in order to close the energy budget 
!between surfex and the atmosphere. All variables should be at t+1 but very 
!difficult to do. Maybe it will be done later. However, Ts can be at time t+1
!-------------------------------------------------------------------------------
!
PTSURF (:) = PTRAD         (:) ! Should be the surface effective temperature; not radative
PZ0    (:) = ZAVG_Z0  (:) ! Should account for ISBA (greenroof and garden) Z0
PZ0H   (:) = PZ0 (:) / 200.    ! Should account for ISBA (greenroof and garden) Z0
PQSURF (:) = NT%AL(1)%XQ_CANYON(:) ! Should account for ISBA (greenroof and garden) Qs
!
!-------------------------------------------------------------------------------------
! Scalar fluxes:
!-------------------------------------------------------------------------------------
!
ZAVG_USTAR    (:) = SQRT(SQRT(PSFU**2+PSFV**2))
!
!
IF (CHT%SVT%NBEQ>0) THEN

  IBEG = CHT%SVT%NSV_CHSBEG
  IEND = CHT%SVT%NSV_CHSEND

  IF (CHT%CCH_DRY_DEP == "WES89") THEN
    CALL CH_DEP_TOWN(ZAVG_RESA,  ZAVG_USTAR, PTA, PTRAD, ZAVG_WL_O_HOR,&
                     PSV(:,IBEG:IEND), CHT%SVT%CSV(IBEG:IEND), CHT%XDEP(:,1:CHT%SVT%NBEQ)  )
   
    DO JI=IBEG,IEND
!cdir nodep
      DO JJ=1,SIZE(PSFTS,1)
        PSFTS(JJ,JI) = - PSV(JJ,JI) * CHT%XDEP(JJ,JI-IBEG+1)
      ENDDO
    ENDDO

    IF (CHT%SVT%NAEREQ > 0 ) THEN

      IBEG = CHT%SVT%NSV_AERBEG
      IEND = CHT%SVT%NSV_AEREND

      CALL CH_AER_DEP(PSV(:,IBEG:IEND), PSFTS(:,IBEG:IEND), &
                      ZAVG_USTAR, ZAVG_RESA, PTA, PRHOA)   
    END IF

  ELSE

    IBEG = CHT%SVT%NSV_CHSBEG
    IEND = CHT%SVT%NSV_CHSEND

    DO JI=IBEG,IEND
      PSFTS(:,JI) =0.
    ENDDO

    IBEG = CHT%SVT%NSV_AERBEG
    IEND = CHT%SVT%NSV_AEREND

    IF(IBEG.LT.IEND) THEN
      DO JI=IBEG,IEND
        PSFTS(:,JI) =0.
      ENDDO
    ENDIF
  ENDIF

ENDIF

IF (CHT%SVT%NDSTEQ>0) THEN
  ! Blindage à enlever lorsque que TEB aura été corrigé
  ZUSTAR(:) = MIN(ZUSTAR(:), 10.)
  ZRESA (:) = MAX(ZRESA(:), 10.)
  !
  IBEG = CHT%SVT%NSV_DSTBEG
  IEND = CHT%SVT%NSV_DSTEND
  !
  CALL DSLT_DEP(PSV(:,IBEG:IEND), PSFTS(:,IBEG:IEND), ZUSTAR, ZRESA, PTA, PRHOA, &
                DST%XEMISSIG_DST, DST%XEMISRADIUS_DST, JPMODE_DST, XDENSITY_DST, &
                XMOLARWEIGHT_DST, ZCONVERTFACM0_DST, ZCONVERTFACM6_DST,          &
                ZCONVERTFACM3_DST, LVARSIG_DST, LRGFIX_DST, CVERMOD  )  

  CALL MASSFLUX2MOMENTFLUX(         &
    PSFTS(:,IBEG:IEND),             & !I/O ![kg/m2/sec] In: flux of only mass, out: flux of moments
    PRHOA,                          & !I [kg/m3] air density
    DST%XEMISRADIUS_DST,                &!I [um] emitted radius for the modes (max 3)
    DST%XEMISSIG_DST,                   &!I [-] emitted sigma for the different modes (max 3)
    NDSTMDE,                        &
    ZCONVERTFACM0_DST,              &
    ZCONVERTFACM6_DST,              &
    ZCONVERTFACM3_DST,              &
    LVARSIG_DST, LRGFIX_DST         )  
ENDIF
IF (CHT%SVT%NSLTEQ>0) THEN
  !
  IBEG = CHT%SVT%NSV_SLTBEG
  IEND = CHT%SVT%NSV_SLTEND
  !
  CALL DSLT_DEP(PSV(:,IBEG:IEND), PSFTS(:,IBEG:IEND), ZUSTAR, ZRESA, PTA, PRHOA, &
                SLT%XEMISSIG_SLT, SLT%XEMISRADIUS_SLT, JPMODE_SLT, XDENSITY_SLT, &
                XMOLARWEIGHT_SLT, ZCONVERTFACM0_SLT, ZCONVERTFACM6_SLT,          &
                ZCONVERTFACM3_SLT, LVARSIG_SLT, LRGFIX_SLT, CVERMOD  )  

  CALL MASSFLUX2MOMENTFLUX(         &
    PSFTS(:,IBEG:IEND),             & !I/O ![kg/m2/sec] In: flux of only mass, out: flux of moments
    PRHOA,                          & !I [kg/m3] air density
    SLT%XEMISRADIUS_SLT,                &!I [um] emitted radius for the modes (max 3)
    SLT%XEMISSIG_SLT,                   &!I [-] emitted sigma for the different modes (max 3)
    NSLTMDE,                        &
    ZCONVERTFACM0_SLT,              &
    ZCONVERTFACM6_SLT,              &
    ZCONVERTFACM3_SLT,              &
    LVARSIG_SLT, LRGFIX_SLT         ) 
ENDIF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Inline diagnostics
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
 CALL DIAG_INLINE_TEB_n(TD%O, TD%D, SB, NT%AL(1), TOP%LCANOPY, &
                        PTA, PTRAD, ZQA, PPA, PPS, PRHOA, PU, PV, ZWIND, PZREF, PUREF, &
                        ZAVG_CD, ZAVG_CDN, ZAVG_RI, ZAVG_CH, ZAVG_Z0, PTRAD, PEMIS,    &
                        PDIR_ALB, PSCA_ALB, PLW, ZDIR_SWB, ZSCA_SWB,  PSFTH, PSFTQ,    &
                        PSFU, PSFV, PSFCO2, ZAVG_RN, ZAVG_H, ZAVG_LE, ZAVG_GFLX       )
!
!-------------------------------------------------------------------------------------
! Stores Canyon air and humidity if historical option of TEB is active
!-------------------------------------------------------------------------------------
!
IF (.NOT. TOP%LCANOPY) THEN
  DO JP=1,TOP%NTEB_PATCH
    NT%AL(JP)%XT_CANYON(:) = ZAVG_T_CANYON(:)
    NT%AL(JP)%XQ_CANYON(:) = ZAVG_Q_CANYON(:)
  END DO
END IF
!          
!-------------------------------------------------------------------------------------
! Thermal confort index
!-------------------------------------------------------------------------------------
!
IF (TD%DUT%LUTCI .AND. TD%O%N2M >0) THEN
  DO JJ=1,KI
    IF (TD%D%XZON10M(JJ)/=XUNDEF) THEN
      ZU_UTCI(JJ) = SQRT(TD%D%XZON10M(JJ)**2+TD%D%XMER10M(JJ)**2)
    ELSE
      ZU_UTCI(JJ) = ZWIND(JJ)
    ENDIF
  ENDDO
 CALL UTCI_TEB(NT%AL(1), TD%DUT, ZAVG_TI_BLD, ZAVG_QI_BLD, ZU_UTCI, PPS, ZAVG_REF_SW_GRND, &
               ZAVG_REF_SW_FAC, ZAVG_SCA_SW, ZAVG_DIR_SW, PZENITH, ZAVG_EMIT_LW_FAC,  &
               ZAVG_EMIT_LW_GRND, PLW, ZAVG_T_RAD_IND    )
 CALL UTCIC_STRESS(PTSTEP,TD%DUT%XUTCI_IN      ,TD%DUT%XUTCIC_IN      )
 CALL UTCIC_STRESS(PTSTEP,TD%DUT%XUTCI_OUTSUN  ,TD%DUT%XUTCIC_OUTSUN  )
 CALL UTCIC_STRESS(PTSTEP,TD%DUT%XUTCI_OUTSHADE,TD%DUT%XUTCIC_OUTSHADE)
ELSE IF (TD%DUT%LUTCI) THEN
  TD%DUT%XUTCI_IN         (:) = XUNDEF
  TD%DUT%XUTCI_OUTSUN     (:) = XUNDEF
  TD%DUT%XUTCI_OUTSHADE   (:) = XUNDEF
  TD%DUT%XTRAD_SUN        (:) = XUNDEF
  TD%DUT%XTRAD_SHADE      (:) = XUNDEF
  TD%DUT%XUTCIC_IN      (:,:) = XUNDEF
  TD%DUT%XUTCIC_OUTSUN  (:,:) = XUNDEF
  TD%DUT%XUTCIC_OUTSHADE(:,:) = XUNDEF
ENDIF

!
IF (LHOOK) CALL DR_HOOK('COUPLING_TEB_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
CONTAINS
SUBROUTINE ADD_PATCH_CONTRIB(JP,PAVG,PFIELD)
INTEGER, INTENT(IN) :: JP
REAL, DIMENSION(:), INTENT(INOUT) :: PAVG
REAL, DIMENSION(:), INTENT(IN)    :: PFIELD
!
IF (JP==1) PAVG = 0.
PAVG = PAVG + TOP%XTEB_PATCH(:,JP) * PFIELD(:)
!
END SUBROUTINE ADD_PATCH_CONTRIB
!-------------------------------------------------------------------------------------
!
END SUBROUTINE COUPLING_TEB_n


