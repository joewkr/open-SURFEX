!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###############################################################################
SUBROUTINE COUPLING_FLAKE_n (CHF, DGO, D, DC, DMF, F, DST, SLT, &
                             HPROGRAM, HCOUPLING, PTSTEP, KYEAR, KMONTH, KDAY, PTIME,  &
                             KI, KSV, KSW, PTSUN, PZENITH, PZENITH2, PAZIM, PZREF,     &
                             PUREF, PU, PV, PQA, PTA, PRHOA, PSV, PCO2, HSV, PRAIN,    &
                             PSNOW, PLW, PDIR_SW, PSCA_SW, PSW_BANDS, PPS, PPA, PSFTQ, &
                             PSFTH, PSFTS, PSFCO2, PSFU, PSFV, PTSRAD, PDIR_ALB,       &
                             PSCA_ALB, PEMIS, PTSURF, PZ0, PZ0H, PQSURF, PPEW_A_COEF,  &
                             PPEW_B_COEF, PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF,       &
                             PPEQ_B_COEF, HTEST                              )  
!     ###############################################################################

!
!!****  *COUPLING_FLAKE_n * - Driver for FLAKE scheme for lakes
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
!!      V. Masson   05/2009 Implicitation of momentum fluxes
!!      B. Decharme 01/2010 Add XTT in water_flux
!!      V. Masson   11/2011 Ch limited to 1.E-7 in all cases and Cd coming from
!!                          Flake_interface routine if computed by flake
!!      B. Decharme 09/2012 New wind implicitation
!!      P. Le Moigne 10/2012 ECUME option for FLake. Remove wind threshold
!!      P. Le Moigne 04/2013 Remove ECUME option for FLake
!!      P. Le Moigne 04/2013 Chemistry, UPDATE_RAD_FLAKE
!!      B. Decharme  04/2013 New diag, new coupling variables
!!      P. Le Moigne 10/2014 Threshold on Cd when fluxes computed by FLake
!!------------------------------------------------------------------------------
!
!
USE MODD_CH_FLAKE_n, ONLY : CH_FLAKE_t
USE MODD_DIAG_n, ONLY : DIAG_OPTIONS_t, DIAG_t
USE MODD_DIAG_MISC_FLAKE_n, ONLY : DIAG_MISC_FLAKE_t
USE MODD_FLAKE_n, ONLY : FLAKE_t
!
USE MODD_DST_n, ONLY : DST_t
USE MODD_SLT_n, ONLY : SLT_t
!
USE MODD_REPROD_OPER, ONLY : CIMPLICIT_WIND
!
USE MODD_CSTS,     ONLY : XRD, XCPD, XP00, XLVTT, XLSTT, XKARMAN, XTT
USE MODD_SURF_PAR, ONLY : XUNDEF
!
!                          
!
USE MODD_SLT_SURF
USE MODD_DST_SURF
!
USE MODE_DSLT_SURF
USE MODE_THERMOS
! 
USE MODI_WATER_FLUX
USE MODI_ADD_FORECAST_TO_DATE_SURF
USE MODI_DIAG_INLINE_FLAKE_n 
USE MODI_DIAG_MISC_FLAKE_n
USE MODI_CH_AER_DEP
USE MODI_CH_DEP_WATER
USE MODI_DSLT_DEP
USE MODI_FLAKE_ALBEDO
USE MODI_UPDATE_RAD_FLAKE
USE MODI_ABOR1_SFX
USE MODI_FLAKE_INTERFACE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(CH_FLAKE_t), INTENT(INOUT) :: CHF
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DGO
TYPE(DIAG_t), INTENT(INOUT) :: D
TYPE(DIAG_t), INTENT(INOUT) :: DC
TYPE(DIAG_MISC_FLAKE_t), INTENT(INOUT) :: DMF
TYPE(FLAKE_t), INTENT(INOUT) :: F
!
TYPE(DST_t), INTENT(INOUT) :: DST
TYPE(SLT_t), INTENT(INOUT) :: SLT
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
REAL, DIMENSION(KI), INTENT(IN)  :: PZENITH   ! zenithal angle at t         (radian from the vertical)
REAL, DIMENSION(KI), INTENT(IN)  :: PZENITH2  ! zenithal angle at t+1       (radian from the vertical)
REAL, DIMENSION(KI), INTENT(IN)  :: PAZIM     ! azimuthal angle      (radian from North, clockwise)
REAL, DIMENSION(KI), INTENT(IN)  :: PLW       ! longwave radiation (on horizontal surf.)
!                                             !                                       (W/m2)
REAL, DIMENSION(KI), INTENT(IN)  :: PPS       ! pressure at atmospheric model surface (Pa)
REAL, DIMENSION(KI), INTENT(IN)  :: PPA       ! pressure at forcing level             (Pa)
REAL, DIMENSION(KI), INTENT(IN)  :: PCO2      ! CO2 concentration in the air          (kg/m3)
REAL, DIMENSION(KI), INTENT(IN)  :: PSNOW     ! snow precipitation                    (kg/m2/s)
REAL, DIMENSION(KI), INTENT(IN)  :: PRAIN     ! liquid precipitation                  (kg/m2/s)
!
!
REAL, DIMENSION(KI), INTENT(OUT) :: PSFTH     ! flux of heat                          (W/m2)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFTQ     ! flux of water vapor                   (kg/m2/s)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFU      ! zonal momentum flux                   (Pa)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFV      ! meridian momentum flux                (Pa)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFCO2    ! flux of CO2                           (m/s*kg_CO2/kg_air)
REAL, DIMENSION(KI,KSV),INTENT(OUT):: PSFTS   ! flux of scalar var.                   (kg/m2/s)
!
REAL, DIMENSION(KI), INTENT(OUT) :: PTSRAD    ! radiative temperature                 (K)
REAL, DIMENSION(KI,KSW),INTENT(OUT):: PDIR_ALB! direct albedo for each spectral band  (-)
REAL, DIMENSION(KI,KSW),INTENT(OUT):: PSCA_ALB! diffuse albedo for each spectral band (-)
REAL, DIMENSION(KI), INTENT(OUT) :: PEMIS     ! emissivity                            (-)
!
REAL, DIMENSION(KI), INTENT(OUT) :: PTSURF    ! surface effective temperature         (K)
REAL, DIMENSION(KI), INTENT(OUT) :: PZ0       ! roughness length for momentum         (m)
REAL, DIMENSION(KI), INTENT(OUT) :: PZ0H      ! roughness length for heat             (m)
REAL, DIMENSION(KI), INTENT(OUT) :: PQSURF    ! specific humidity at surface          (kg/kg)
!
REAL, DIMENSION(KI), INTENT(IN) :: PPEW_A_COEF! implicit coefficients   (m2s/kg)
REAL, DIMENSION(KI), INTENT(IN) :: PPEW_B_COEF! needed if HCOUPLING='I' (m/s)
REAL, DIMENSION(KI), INTENT(IN) :: PPET_A_COEF
REAL, DIMENSION(KI), INTENT(IN) :: PPEQ_A_COEF
REAL, DIMENSION(KI), INTENT(IN) :: PPET_B_COEF
REAL, DIMENSION(KI), INTENT(IN) :: PPEQ_B_COEF
 CHARACTER(LEN=2),    INTENT(IN) :: HTEST ! must be equal to 'OK'
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(KI,KSW) :: ZDIR_ALB   ! Direct albedo at time t , 
REAL, DIMENSION(KI,KSW) :: ZSCA_ALB   ! Diffuse albedo at time t
!
REAL, DIMENSION(KI)  :: ZALB     ! surface albedo
REAL, DIMENSION(KI)  :: ZSWE     ! snow water equivalent (kg.m-2)
!
REAL, DIMENSION(KI)  :: ZEXNA  ! Exner function at forcing level
REAL, DIMENSION(KI)  :: ZEXNS  ! Exner function at surface level
!
REAL, DIMENSION(KI)  :: ZWIND  ! Wind
REAL, DIMENSION(KI)  :: ZGLOBAL_SW    ! Solar radiation flux at the surface (W/m2) 
REAL, DIMENSION(KI)  :: ZQA    ! Air specific humidity (kg/kg)
!
REAL, DIMENSION(KI)  :: ZUSTAR ! friction velocity (m/s)
REAL, DIMENSION(KI)  :: ZUSTAR2! square of friction velocity (m2/s2)
REAL, DIMENSION(KI)  :: ZSFM   ! flux of momentum (Pa)
!
REAL, DIMENSION(KI)  :: ZRESA_WATER ! aerodynamical resistance
!
!salgado only for inline diagnostics - not used for the moment
!                                      flake don't have it
REAL, DIMENSION(KI)  :: ZCD    ! Drag coefficient
REAL, DIMENSION(KI)  :: ZCDN   ! Neutral Drag coefficient
REAL, DIMENSION(KI)  :: ZCH    ! Heat transfer coefficient
REAL, DIMENSION(KI)  :: ZCE    ! Heat transfer coefficient
REAL, DIMENSION(KI)  :: ZRI    ! Richardson number
REAL, DIMENSION(KI)  :: ZHU    ! Near surface relative humidity
REAL, DIMENSION(KI)  :: ZZ0H   ! heat roughness length
REAL, DIMENSION(KI)  :: ZQSAT  ! humidity at saturation
REAL, DIMENSION(KI)  :: ZTSTEP ! time-step
REAL, DIMENSION(KI)  :: ZLE    ! total latent heat flux (W/m2)
REAL, DIMENSION(KI)  :: ZLEI   ! sublimation heat flux (W/m2)
REAL, DIMENSION(KI)  :: ZSUBL  ! sublimation (kg/m2/s)
REAL, DIMENSION(KI)  :: ZLWUP  ! upward longwave flux at t
REAL, DIMENSION(KI)  :: ZTRAD  ! Radiative temperature at time t
REAL, DIMENSION(KI)  :: ZWORK  ! Work array
!
REAL                 :: ZCONVERTFACM0_SLT, ZCONVERTFACM0_DST
REAL                 :: ZCONVERTFACM3_SLT, ZCONVERTFACM3_DST
REAL                 :: ZCONVERTFACM6_SLT, ZCONVERTFACM6_DST
!
INTEGER              :: ISWB   ! number of shortwave spectral bands
INTEGER              :: JSWB   ! loop counter on shortwave spectral bands
!  
INTEGER              :: ILUOUT ! output logical unit
!
LOGICAL              :: GPWG        = .FALSE.
LOGICAL              :: GHANDLE_SIC = .FALSE. ! no sea-ice model
!
REAL                 :: ZEPS = 1.E-7
!
INTEGER :: IBEG, IEND
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
! Preliminaries:
!-------------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('COUPLING_FLAKE_N',0,ZHOOK_HANDLE)
IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('COUPLING_FLAKEN: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF
!-------------------------------------------------------------------------------------
! Variables needed by flake:
!-------------------------------------------------------------------------------------
!
ZCD        (:) = XUNDEF    
ZCDN       (:) = XUNDEF
ZCH        (:) = XUNDEF
ZRI        (:) = XUNDEF
ZRESA_WATER(:) = XUNDEF
ZZ0H       (:) = XUNDEF
ZQSAT      (:) = XUNDEF
ZWORK      (:) = XUNDEF
ZALB       (:) = XUNDEF
ZGLOBAL_SW (:) = XUNDEF
ZSWE       (:) = XUNDEF
!
ZDIR_ALB   (:,:) = XUNDEF
ZSCA_ALB   (:,:) = XUNDEF
ZLE        (:)   = XUNDEF
ZLEI       (:)   = XUNDEF
ZSUBL      (:)   = XUNDEF
ZLWUP      (:)   = XUNDEF
ZTRAD      (:)   = XUNDEF
!
ZTSTEP(:) = PTSTEP
!
ZEXNS(:)     = (PPS(:)/XP00)**(XRD/XCPD)
ZEXNA(:)     = (PPA(:)/XP00)**(XRD/XCPD)
!
!
ZWIND(:) = SQRT(PU(:)**2+PV(:)**2)
!
ZQA(:) = PQA(:)/PRHOA(:)
!
PSFTS(:,:) = 0.
!
ZHU(:) = 1.
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Time evolution
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
F%TTIME%TIME = F%TTIME%TIME + PTSTEP
 CALL ADD_FORECAST_TO_DATE_SURF(F%TTIME%TDATE%YEAR,F%TTIME%TDATE%MONTH,F%TTIME%TDATE%DAY,F%TTIME%TIME)
!
!----------------------------------------
!
PSFU(:) = 0.
PSFV(:) = 0.
ZSFM(:) = 0.
!
IF (F%CFLK_FLUX=='DEF  ') THEN
!
    CALL WATER_FLUX(F%XZ0, PTA, ZEXNA, PRHOA, F%XTS, ZEXNS, PQA, &
                    PRAIN, PSNOW, XTT, ZWIND, PZREF, PUREF,      &
                    PPS, GHANDLE_SIC, ZQSAT, PSFTH, PSFTQ, ZUSTAR,&
                    ZCD, ZCDN, ZCH, ZRI, ZRESA_WATER, ZZ0H        )  
!                  
    WHERE (F%XTS(:)<XTT)
      ZLE  (:) = PSFTQ(:) * XLSTT
      ZLEI (:) = PSFTQ(:) * XLSTT
      ZSUBL(:) = PSFTQ(:)
    ELSEWHERE
      ZLE  (:) = PSFTQ(:) * XLVTT
      ZLEI (:) = 0.0
      ZSUBL(:) = 0.0
    END WHERE
!
    IF(CIMPLICIT_WIND=='OLD')THEN    
!     old implicitation (m2/s2)
      ZUSTAR2(:) = (ZCD(:)*ZWIND(:)*PPEW_B_COEF(:))/            &
                   (1.0-PRHOA(:)*ZCD(:)*ZWIND(:)*PPEW_A_COEF(:)) 
    ELSE
!     new implicitation (m2/s2)            
      ZUSTAR2(:) = (ZCD(:)*ZWIND(:)*(2.*PPEW_B_COEF(:)-ZWIND(:))) /&
                   (1.0-2.0*PRHOA(:)*ZCD(:)*ZWIND(:)*PPEW_A_COEF(:)) 
!                   
      ZWORK(:) = PRHOA(:)*PPEW_A_COEF(:)*ZUSTAR2(:) + PPEW_B_COEF(:)
      ZWORK(:) = MAX(ZWORK(:),0.)
!
      WHERE(PPEW_A_COEF(:)/= 0.)
            ZUSTAR2(:) = MAX( ( ZWORK(:) - PPEW_B_COEF(:) ) / (PRHOA(:)*PPEW_A_COEF(:)), 0.)
      ENDWHERE
!                   
    ENDIF
!    
    WHERE (ZWIND(:)>0.)            
      ZSFM(:) = - PRHOA(:) * ZUSTAR2(:)
      PSFU(:) = ZSFM(:) * PU(:) / ZWIND(:)
      PSFV(:) = ZSFM(:) * PV(:) / ZWIND(:)
    END WHERE
!
ELSE
   ZUSTAR(:) = F%XUSTAR(:)
   ZZ0H  (:) = F%XZ0   (:)
ENDIF
!
!----------------------------------------
!radiative properties at t
!----------------------------------------
!
ISWB = SIZE(PSW_BANDS)
!
DO JSWB=1,ISWB 
  ZDIR_ALB(:,JSWB) = F%XDIR_ALB(:)
  ZSCA_ALB(:,JSWB) = F%XSCA_ALB(:)
END DO
!
ZTRAD = F%XTS
!
 CALL FLAKE_ALBEDO(PDIR_SW,PSCA_SW,KSW,ZDIR_ALB,ZSCA_ALB,ZGLOBAL_SW,ZALB)
!
 CALL FLAKE_INTERFACE(F, KI, &
! Atmospheric forcing:
                      PSNOW, ZGLOBAL_SW, PLW, PUREF, PZREF, ZWIND, PTA, ZQA, PPS, &
! Constant parameters
                      ZTSTEP,      &
! surface albedo
                      ZALB,        &
! Surface heat, momentum fluxes, and other diags
                      PSFTH, ZLE, ZSFM, ZZ0H, ZQSAT, ZRI, ZUSTAR,     &
                      ZCD, PSFTQ, ZLEI, ZSUBL, ZLWUP, ZSWE,           &
! Flags              
                      PPEW_A_COEF, PPEW_B_COEF, PRHOA, CIMPLICIT_WIND )
!
!-------------------------------------------------------------------------------------
!
! Momentum fluxes
!
IF (F%CFLK_FLUX=='FLAKE') THEN
   PSFU = 0.
   PSFV = 0.
  WHERE (ZWIND(:)>0.)
    PSFU(:) = ZSFM(:) * PU(:) / ZWIND(:)
    PSFV(:) = ZSFM(:) * PV(:) / ZWIND(:)
  END WHERE
  ! 
  ! ZUSTAR and ZRESA_WATER are not in Flake but are needed to the ch_* routines
  !
  ZUSTAR(:)       = SQRT (ABS(ZSFM(:))/PRHOA(:))
  ZEXNS (:)       = (PPS(:)/XP00)**(XRD/XCPD)
  ZEXNA (:)       = (PPA(:)/XP00)**(XRD/XCPD)
  ZRESA_WATER=2.E4
  WHERE (PSFTH/=0.)
  ZRESA_WATER (:) = XCPD * PRHOA(:) * (F%XTS(:) - PTA(:) * ZEXNS(:)/ZEXNA(:)) &
                     / (PSFTH(:) * ZEXNS(:))  
  END WHERE
!
ENDIF
!                               
F%XUSTAR(:) = ZUSTAR(:)
!
! CO2 flux
!
PSFCO2(:)       =  0.0    ! Assumes no CO2 emission over water bodies
!
!-------------------------------------------------------------------------------------
! Scalar fluxes:
!-------------------------------------------------------------------------------------
!
!
!salgado The scalar fluxes are computed as in watflux
IF (CHF%SVF%NBEQ>0) THEN
  !
  IF (CHF%CCH_DRY_DEP == "WES89") THEN
    !
    IBEG = CHF%SVF%NSV_CHSBEG
    IEND = CHF%SVF%NSV_CHSEND
    !
    CALL CH_DEP_WATER  (ZRESA_WATER, ZUSTAR, PTA, ZTRAD, PSV(:,IBEG:IEND), &
                        CHF%SVF%CSV(IBEG:IEND), CHF%XDEP(:,1:CHF%SVF%NBEQ) )  

    PSFTS(:,IBEG:IEND) = - PSV(:,IBEG:IEND) * CHF%XDEP(:,1:CHF%SVF%NBEQ) 
    ! 
    IF (CHF%SVF%NAEREQ > 0 ) THEN
      IBEG = CHF%SVF%NSV_AERBEG
      IEND = CHF%SVF%NSV_AEREND
      !
      CALL CH_AER_DEP(PSV(:,IBEG:IEND),PSFTS(:,IBEG:IEND),ZUSTAR,ZRESA_WATER,PTA,PRHOA)     
      END IF

  ELSE
    IBEG = CHF%SVF%NSV_AERBEG
    IEND = CHF%SVF%NSV_AEREND
    !
    PSFTS(:,IBEG:IEND) =0.
    IF(IBEG.LT.IEND) PSFTS(:,IBEG:IEND) =0.
  ENDIF
  !
ENDIF

IF (CHF%SVF%NDSTEQ>0) THEN
  !
  IBEG = CHF%SVF%NSV_DSTBEG
  IEND = CHF%SVF%NSV_DSTEND
  !
  CALL DSLT_DEP(PSV(:,IBEG:IEND), PSFTS(:,IBEG:IEND), ZUSTAR, ZRESA_WATER, PTA, PRHOA, &
                DST%XEMISSIG_DST, DST%XEMISRADIUS_DST, JPMODE_DST, XDENSITY_DST, &
                XMOLARWEIGHT_DST, ZCONVERTFACM0_DST, ZCONVERTFACM6_DST, ZCONVERTFACM3_DST, &
                LVARSIG_DST, LRGFIX_DST, CVERMOD  )  

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


IF (CHF%SVF%NSLTEQ>0) THEN
  !
  IBEG = CHF%SVF%NSV_SLTBEG
  IEND = CHF%SVF%NSV_SLTEND
  !
  CALL DSLT_DEP(PSV(:,IBEG:IEND), PSFTS(:,IBEG:IEND), ZUSTAR, ZRESA_WATER, PTA, PRHOA, &
                SLT%XEMISSIG_SLT, SLT%XEMISRADIUS_SLT, JPMODE_SLT, XDENSITY_SLT, &
                XMOLARWEIGHT_SLT, ZCONVERTFACM0_SLT, ZCONVERTFACM6_SLT, ZCONVERTFACM3_SLT, &
                LVARSIG_SLT, LRGFIX_SLT, CVERMOD  )  

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
IF (F%CFLK_FLUX=='FLAKE') THEN  !compute some variables not present in FLake code
!
  ZCH = ZEPS
!
  WHERE (ABS((F%XTS(:) - PTA(:) * ZEXNS(:)/ZEXNA(:))) > 1.E-2 .AND. ZWIND(:)/=0.)
     ZCH = MAX(ZEPS,PSFTH / (XCPD * PRHOA(:) * ZWIND(:) * (F%XTS(:) - PTA(:) * ZEXNS(:)/ZEXNA(:))) * ZEXNS(:))
  END WHERE
!
  ZCDN(:) = (XKARMAN/LOG(PUREF(:)/F%XZ0(:)))**2
  ZCD (:) = MAX(ZEPS,ZCD(:))
!
ENDIF
!
 CALL DIAG_INLINE_FLAKE_n(DGO, D, DC, F, &
                          PTSTEP, PTA,  ZQA, PPA, PPS, PRHOA, PU,    &
                          PV, PZREF, PUREF, PRAIN, PSNOW,            &
                          ZCD, ZCDN, ZCH, ZRI, ZHU,                  &
                          ZZ0H, ZQSAT, PSFTH, PSFTQ, PSFU, PSFV,     &
                          PDIR_SW, PSCA_SW, PLW, ZDIR_ALB, ZSCA_ALB, &
                          ZLE, ZLEI, ZSUBL, ZLWUP, ZALB, ZSWE        )  
!
!-------------------------------------------------------------------------------------
!
 CALL DIAG_MISC_FLAKE_n(DMF, F)
!
!-------------------------------------------------------------------------------
!Physical properties see by the atmosphere in order to close the energy budget 
!between surfex and the atmosphere. All variables should be at t+1 but very 
!difficult to do. Maybe it will be done later. However, Ts can be at time t+1
!-------------------------------------------------------------------------------
!
PTSURF (:) = F%XTS  (:)
PZ0    (:) = F%XZ0  (:)
PZ0H   (:) = ZZ0H (:)
PQSURF (:) = ZQSAT(:)
!
!-------------------------------------------------------------------------------------
!Radiative properties at time t+1 (see by the atmosphere) in order to close
!the energy budget between surfex and the atmosphere
!-------------------------------------------------------------------------------------
!
 CALL UPDATE_RAD_FLAKE(F,PZENITH2,PDIR_ALB,PSCA_ALB,PEMIS,PTSRAD  )                       
!                         
IF (LHOOK) CALL DR_HOOK('COUPLING_FLAKE_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE COUPLING_FLAKE_n
