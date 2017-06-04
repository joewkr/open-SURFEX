!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###############################################################################
SUBROUTINE COUPLING_WATFLUX_n (CHW, DGO, D, DC, W, DST, SLT, &
                               HPROGRAM, HCOUPLING, PTIMEC, PTSTEP, KYEAR, KMONTH, KDAY, PTIME, &
                               KI, KSV, KSW, PTSUN, PZENITH, PZENITH2, PAZIM, PZREF, PUREF,     &
                               PU, PV, PQA, PTA, PRHOA, PSV, PCO2, HSV, PRAIN, PSNOW, PLW,      &
                               PDIR_SW, PSCA_SW, PSW_BANDS, PPS, PPA, PSFTQ, PSFTH, PSFTS,      &
                               PSFCO2, PSFU, PSFV, PTRAD, PDIR_ALB, PSCA_ALB, PEMIS, PTSURF,    &
                               PZ0, PZ0H, PQSURF, PPEW_A_COEF, PPEW_B_COEF, PPET_A_COEF,        &
                               PPEQ_A_COEF, PPET_B_COEF, PPEQ_B_COEF, HTEST      )  
!     ###############################################################################
!
!!****  *COUPLING_WATFLUX_n * - Driver for WATER_FLUX scheme for lakes
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
!!      B. Decharme 03/2009 TS_WATER could change during a run => ALB and EMIS
!!      V. Masson   05/2009 Implicitation of momentum fluxes
!!      B. Decharme 09/2009 Radiative properties at time t+1 in order to close
!                           the energy budget between surfex and the atmosphere 
!!      B. Decharme 01/2010 Add XTT
!!      B. Decharme 09/2012 New wind implicitation
!!      P. LeMoigne 04/2013 Wind implicitation displaced
!!      B. Decharme  04/2013 new coupling variables
!!----------------------------------------------------------------------------
!
!
USE MODD_CH_WATFLUX_n, ONLY : CH_WATFLUX_t
USE MODD_DIAG_n, ONLY : DIAG_OPTIONS_t, DIAG_t
USE MODD_WATFLUX_n, ONLY : WATFLUX_t
!
USE MODD_DST_n, ONLY : DST_t
USE MODD_SLT_n, ONLY : SLT_t
!
USE MODD_REPROD_OPER, ONLY : CIMPLICIT_WIND
!
USE MODD_CSTS,       ONLY : XRD, XCPD, XP00, XTT, XDAY, XTTS
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_SFX_OASIS,  ONLY : LCPL_SEAICE
USE MODD_WATER_PAR
!
!
USE MODI_WATER_FLUX
USE MODI_ADD_FORECAST_TO_DATE_SURF
USE MODI_DIAG_INLINE_WATFLUX_n
USE MODI_CH_AER_DEP
USE MODI_CH_DEP_WATER
USE MODI_DSLT_DEP
USE MODI_UPDATE_RAD_WATER
USE MODI_INTERPOL_TS_WATER_MTH
!
USE MODE_DSLT_SURF
USE MODD_DST_SURF
USE MODD_SLT_SURF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
USE MODI_COUPLING_ICEFLUX_n
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(CH_WATFLUX_t), INTENT(INOUT) :: CHW
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DGO
TYPE(DIAG_t), INTENT(INOUT) :: D 
TYPE(DIAG_t), INTENT(INOUT) :: DC
TYPE(WATFLUX_t), INTENT(INOUT) :: W 
!
TYPE(DST_t), INTENT(INOUT) :: DST
TYPE(SLT_t), INTENT(INOUT) :: SLT
!
 CHARACTER(LEN=6),    INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=1),    INTENT(IN)  :: HCOUPLING ! type of coupling
                                              ! 'E' : explicit
                                              ! 'I' : implicit
REAL,                INTENT(IN)  :: PTIMEC    ! cumulated time since beginning of simulation
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
REAL, DIMENSION(KI), INTENT(IN)  :: PZENITH   ! zenithal angle at t      (radian from the vertical)
REAL, DIMENSION(KI), INTENT(IN)  :: PZENITH2  ! zenithal angle at t+1    (radian from the vertical)
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
REAL, DIMENSION(KI)  :: ZEXNA      ! Exner function at forcing level
REAL, DIMENSION(KI)  :: ZEXNS      ! Exner function at surface level
REAL, DIMENSION(KI)  :: ZWIND      ! Wind
REAL, DIMENSION(KI)  :: ZCD        ! Drag coefficient
REAL, DIMENSION(KI)  :: ZCDN       ! Neutral Drag coefficient
REAL, DIMENSION(KI)  :: ZCH        ! Heat transfer coefficient
REAL, DIMENSION(KI)  :: ZRI        ! Richardson number
REAL, DIMENSION(KI)  :: ZHU        ! Near surface relative humidity
REAL, DIMENSION(KI)  :: ZRESA_WATER! aerodynamical resistance
REAL, DIMENSION(KI)  :: ZUSTAR     ! friction velocity (m/s)
REAL, DIMENSION(KI)  :: ZUSTAR2    ! square of friction velocity (m2/s2)
REAL, DIMENSION(KI)  :: ZZ0H       ! heat roughness length over water
REAL, DIMENSION(KI)  :: ZQSAT      ! humidity at saturation
REAL, DIMENSION(KI)  :: ZQA        ! specific humidity (kg/kg)
REAL, DIMENSION(KI)  :: ZEMIS      ! Emissivity at time t
REAL, DIMENSION(KI)  :: ZTRAD      ! Radiative temperature at time t
REAL, DIMENSION(KI)  :: ZSFTH_ICE  ! Sea ice flux of heat
REAL, DIMENSION(KI)  :: ZSFTQ_ICE  ! Sea ice flux of ice sublimation
REAL, DIMENSION(KI)  :: ZWORK      ! Work array
!
REAL, DIMENSION(KI,KSW) :: ZDIR_ALB   ! Direct albedo at time t
REAL, DIMENSION(KI,KSW) :: ZSCA_ALB   ! Diffuse albedo at time t
!
REAL                       :: ZCONVERTFACM0_SLT, ZCONVERTFACM0_DST
REAL                       :: ZCONVERTFACM3_SLT, ZCONVERTFACM3_DST
REAL                       :: ZCONVERTFACM6_SLT, ZCONVERTFACM6_DST
!
INTEGER                          :: ISWB       ! number of shortwave spectral bands
INTEGER                          :: JSWB       ! loop counter on shortwave spectral bands      
!
INTEGER :: IBEG, IEND
!
LOGICAL                    :: GHANDLE_SIC = .FALSE. ! no sea-ice model
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
! Preliminaries:
!-------------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('COUPLING_WATFLUX_N',0,ZHOOK_HANDLE)
IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('COUPLING_WATFLUXN: FATAL ERROR DURING ARGUMENT TRANSFER')        
END IF
!-------------------------------------------------------------------------------------
!
ZEXNA      (:) = XUNDEF
ZEXNS      (:) = XUNDEF
ZWIND      (:) = XUNDEF
ZCD        (:) = XUNDEF    
ZCDN       (:) = XUNDEF
ZCH        (:) = XUNDEF
ZRI        (:) = XUNDEF
ZHU        (:) = XUNDEF
ZRESA_WATER(:) = XUNDEF
ZUSTAR     (:) = XUNDEF
ZUSTAR2    (:) = XUNDEF
ZZ0H       (:) = XUNDEF
ZQSAT      (:) = XUNDEF
ZEMIS      (:) = XUNDEF
ZTRAD      (:) = XUNDEF
ZWORK      (:) = XUNDEF
ZDIR_ALB   (:,:) = XUNDEF
ZSCA_ALB   (:,:) = XUNDEF
!
IF(LCPL_SEAICE)THEN
  ZSFTQ_ICE(:) = XUNDEF
  ZSFTH_ICE(:) = XUNDEF
ENDIF
!
!-------------------------------------------------------------------------------------
!
ZEXNS(:)     = (PPS(:)/XP00)**(XRD/XCPD)
ZEXNA(:)     = (PPA(:)/XP00)**(XRD/XCPD)
!
ZWIND(:) = SQRT(PU(:)**2+PV(:)**2)
!
PSFTS(:,:) = 0.
!
ZHU = 1.
!
ZQA(:) = PQA(:) / PRHOA(:)
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Time evolution
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
W%TTIME%TIME = W%TTIME%TIME + PTSTEP
CALL ADD_FORECAST_TO_DATE_SURF(W%TTIME%TDATE%YEAR,W%TTIME%TDATE%MONTH,W%TTIME%TDATE%DAY,W%TTIME%TIME)
!
!--------------------------------------------------------------------------------------
! Fluxes over water according to Charnock formulae
!--------------------------------------------------------------------------------------
!
!
CALL WATER_FLUX(W%XZ0, PTA, ZEXNA, PRHOA, W%XTS, ZEXNS, ZQA, PRAIN,  &
                PSNOW, XTT, ZWIND, PZREF, PUREF, PPS, GHANDLE_SIC,   &
                ZQSAT, PSFTH, PSFTQ, ZUSTAR, ZCD, ZCDN, ZCH, ZRI,    &
                ZRESA_WATER, ZZ0H                                  )  
!
!-------------------------------------------------------------------------------------
!radiative properties at t
!-------------------------------------------------------------------------------------
!
ISWB = SIZE(PSW_BANDS)
!
DO JSWB=1,ISWB
     ZDIR_ALB(:,JSWB) = W%XDIR_ALB(:)
     ZSCA_ALB(:,JSWB) = W%XSCA_ALB(:)
END DO
!
ZEMIS  = W%XEMIS
ZTRAD  = W%XTS
!
!-------------------------------------------------------------------------------------
!Specific fields for GELATO when using earth system model 
!(intermediate step before explicit sea and ice fluxes comutation)
!-------------------------------------------------------------------------------------
!
IF(LCPL_SEAICE)THEN   
  CALL COUPLING_ICEFLUX_n(KI, PTA, ZEXNA, PRHOA, W%XTICE, ZEXNS,  &
                          ZQA, PRAIN, PSNOW, ZWIND, PZREF, PUREF, &
                          PPS, W%XTS, XTT, ZSFTH_ICE, ZSFTQ_ICE     )  
ENDIF
!
!-------------------------------------------------------------------------------------
! Scalar fluxes:
!-------------------------------------------------------------------------------------
!
!
IF (CHW%SVW%NBEQ>0) THEN
  !
  IF (CHW%CCH_DRY_DEP == "WES89") THEN
    !
    IBEG = CHW%SVW%NSV_CHSBEG
    IEND = CHW%SVW%NSV_CHSEND
    !
    CALL CH_DEP_WATER  (ZRESA_WATER, ZUSTAR, PTA, ZTRAD, PSV(:,IBEG:IEND), &
                        CHW%SVW%CSV(IBEG:IEND), CHW%XDEP(:,1:CHW%SVW%NBEQ) )  

    PSFTS(:,IBEG:IEND) = - PSV(:,IBEG:IEND) * CHW%XDEP(:,1:CHW%SVW%NBEQ)  
    IF (CHW%SVW%NAEREQ > 0 ) THEN
      IBEG = CHW%SVW%NSV_AERBEG
      IEND = CHW%SVW%NSV_AEREND        
      !
      CALL CH_AER_DEP(PSV(:,IBEG:IEND),PSFTS(:,IBEG:IEND),ZUSTAR,ZRESA_WATER,PTA,PRHOA)     
    END IF
    !
  ELSE
    !
    IBEG = CHW%SVW%NSV_AERBEG
    IEND = CHW%SVW%NSV_AEREND        
    !
    PSFTS(:,IBEG:IEND) =0.
    IF(IBEG.LT.IBEG) PSFTS(:,IBEG:IBEG) =0.
    !
  ENDIF
  !
ENDIF
!
IF (CHW%SVW%NDSTEQ>0) THEN
  !
  IBEG = CHW%SVW%NSV_DSTBEG
  IEND = CHW%SVW%NSV_DSTEND
  !
  CALL DSLT_DEP(PSV(:,IBEG:IEND), PSFTS(:,IBEG:IEND), ZUSTAR, ZRESA_WATER, PTA, PRHOA, &
                DST%XEMISSIG_DST, DST%XEMISRADIUS_DST, JPMODE_DST, XDENSITY_DST, &
                XMOLARWEIGHT_DST, ZCONVERTFACM0_DST,  ZCONVERTFACM6_DST, ZCONVERTFACM3_DST, &
                LVARSIG_DST, LRGFIX_DST, CVERMOD  )  
  !
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
  !
ENDIF
!
IF (CHW%SVW%NSLTEQ>0) THEN
  !
  IBEG = CHW%SVW%NSV_SLTBEG
  IEND = CHW%SVW%NSV_SLTEND
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
  !
ENDIF
!
!-------------------------------------------------------------------------------------
! Outputs fluxes at time t+1
!-------------------------------------------------------------------------------------
!
! Momentum fluxes
!
IF(CIMPLICIT_WIND=='OLD')THEN
! old implicitation
  ZUSTAR2(:) = (ZCD(:)*ZWIND(:)*PPEW_B_COEF(:)) /          &
               (1.0-PRHOA(:)*ZCD(:)*ZWIND(:)*PPEW_A_COEF(:))
ELSE
! new implicitation
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
PSFU = 0.
PSFV = 0.
WHERE (ZWIND(:)>0.)
  PSFU(:) = - PRHOA(:) * ZUSTAR2(:) * PU(:) / ZWIND(:)
  PSFV(:) = - PRHOA(:) * ZUSTAR2(:) * PV(:) / ZWIND(:)
END WHERE
!
! CO2 flux
!
PSFCO2(:)       =  0.0    ! Assumes no CO2 emission over water bodies
!
!-------------------------------------------------------------------------------------
! Inline diagnostics at time t for TS and TRAD
!-------------------------------------------------------------------------------------
!
 CALL DIAG_INLINE_WATFLUX_n(DGO, D, DC, W, &
                            PTSTEP,PTA, ZQA, PPA, PPS, PRHOA, PU, PV, PZREF,  &
                            PUREF, ZCD, ZCDN, ZCH, ZRI, ZHU, ZZ0H, ZQSAT,     &
                            PSFTH, PSFTQ, PSFU, PSFV, PDIR_SW, PSCA_SW, PLW,  &
                            ZDIR_ALB, ZSCA_ALB, ZEMIS, ZTRAD, PRAIN, PSNOW,   &
                            ZSFTH_ICE, ZSFTQ_ICE                            )
!
!-------------------------------------------------------------------------------
! IMPOSED MONTHLY TS AT TIME t+1
!-------------------------------------------------------------------------------
!  
IF (W%LINTERPOL_TS.AND.MOD(W%TTIME%TIME,XDAY) == 0.) THEN
   CALL INTERPOL_TS_WATER_MTH(W)
ENDIF
!
!-------------------------------------------------------------------------------
!Physical properties see by the atmosphere in order to close the energy budget 
!between surfex and the atmosphere. All variables should be at t+1 but very 
!difficult to do. Maybe it will be done later. However, Ts can be at time t+1
!-------------------------------------------------------------------------------
!
PTSURF (:) = W%XTS  (:)
PZ0    (:) = W%XZ0  (:)
PZ0H   (:) = ZZ0H (:)
PQSURF (:) = ZQSAT(:)
!
!-------------------------------------------------------------------------------------
!Radiative properties at time t+1 (see by the atmosphere) in order to close
!the energy budget between surfex and the atmosphere
!-------------------------------------------------------------------------------------
!
 CALL UPDATE_RAD_WATER(W,PZENITH2,XTT,PDIR_ALB,PSCA_ALB,PEMIS,PTRAD    )  
!
!=======================================================================================
!
IF (LHOOK) CALL DR_HOOK('COUPLING_WATFLUX_N',1,ZHOOK_HANDLE)
!
END SUBROUTINE COUPLING_WATFLUX_n
