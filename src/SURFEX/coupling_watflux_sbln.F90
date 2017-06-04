!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###############################################################################
SUBROUTINE COUPLING_WATFLUX_SBL_n (SB, CHW, DGO, D, DC, W, DST, SLT, &
                                   HPROGRAM, HCOUPLING,  PTIMEC, PTSTEP, KYEAR, KMONTH, KDAY, PTIME, &
                                   KI, KSV, KSW, PTSUN, PZENITH, PZENITH2, PAZIM, PZREF, PUREF, PU, PV, &
                                   PQA, PTA, PRHOA, PSV, PCO2, HSV,PRAIN, PSNOW, PLW, PDIR_SW, PSCA_SW, &
                                   PSW_BANDS, PPS, PPA, PSFTQ, PSFTH, PSFTS, PSFCO2, PSFU, PSFV, &
                                   PTRAD, PDIR_ALB, PSCA_ALB, PEMIS, PTSURF, PZ0, PZ0H, PQSURF, &
                                   PPEW_A_COEF, PPEW_B_COEF, PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF, &
                                   PPEQ_B_COEF, HTEST                                             )
!     ###############################################################################
!
!!****  *COUPLING_WATFLUX_SBL_n * - Adds a SBL into SEAFLUX
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
!!      Original    09/2007
!!      V. Masson   05/2009 Implicitation of momentum fluxes
!!      S. Riette   06/2009 Initialisation of XT, PQ, XU and XTKE on canopy levels
!!      S. Riette   10/2009 Iterative computation of XZ0
!!      S. Riette   01/2010 Use of interpol_sbl to compute 10m wind diagnostic
!!      B. Decharme  04/2013 new coupling variables
!----------------------------------------------------------------
!
!
USE MODD_CANOPY_n, ONLY : CANOPY_t
USE MODD_CH_WATFLUX_n, ONLY : CH_WATFLUX_t
USE MODD_DIAG_n, ONLY : DIAG_OPTIONS_t, DIAG_t
USE MODD_WATFLUX_n, ONLY : WATFLUX_t
!
USE MODD_DST_n, ONLY : DST_t
USE MODD_SLT_n, ONLY : SLT_t
!
USE MODD_SURF_PAR,         ONLY : XUNDEF
USE MODD_CSTS,             ONLY : XCPD
! 
USE MODE_COUPLING_CANOPY
!
USE MODI_INIT_WATER_SBL
!
USE MODI_CANOPY_EVOL
USE MODI_CANOPY_GRID_UPDATE
!
USE MODI_COUPLING_WATFLUX_n
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(CANOPY_t), INTENT(INOUT) :: SB
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
REAL, DIMENSION(KI), INTENT(IN)  :: PZENITH   ! zenithal angle at t  (radian from the vertical)
REAL, DIMENSION(KI), INTENT(IN)  :: PZENITH2  ! zenithal angle at t+1(radian from the vertical)
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
REAL, DIMENSION(KI), INTENT(IN) :: PPEW_A_COEF! implicit coefficients
REAL, DIMENSION(KI), INTENT(IN) :: PPEW_B_COEF! needed if HCOUPLING='I'
REAL, DIMENSION(KI), INTENT(IN) :: PPET_A_COEF
REAL, DIMENSION(KI), INTENT(IN) :: PPEQ_A_COEF
REAL, DIMENSION(KI), INTENT(IN) :: PPET_B_COEF
REAL, DIMENSION(KI), INTENT(IN) :: PPEQ_B_COEF
 CHARACTER(LEN=2),    INTENT(IN) :: HTEST ! must be equal to 'OK'
!
!*      0.2    declarations of local variables
!
!* forcing variables
!
REAL, DIMENSION(KI)     :: ZWIND    ! lowest atmospheric level wind speed           (m/s)
REAL, DIMENSION(KI)     :: ZEXNA    ! Exner function at lowest SBL scheme level     (-)
REAL, DIMENSION(KI)     :: ZTA      ! temperature                                   (K)
REAL, DIMENSION(KI)     :: ZPA      ! pressure                                      (Pa)
REAL, DIMENSION(KI)     :: ZZREF    ! temperature forcing level                     (m)
REAL, DIMENSION(KI)     :: ZUREF    ! wind        forcing level                     (m)
REAL, DIMENSION(KI)     :: ZU       ! zonal wind                                    (m/s)
REAL, DIMENSION(KI)     :: ZV       ! meridian wind                                 (m/s)
REAL, DIMENSION(KI)     :: ZQA      ! specific humidity                             (kg/m3)
REAL, DIMENSION(KI)     :: ZPEQ_A_COEF ! specific humidity implicit
REAL, DIMENSION(KI)     :: ZPEQ_B_COEF ! coefficients (hum. in kg/kg)
!
!
! SBL turbulence scheme
!
REAL, DIMENSION(KI)        :: ZSFLUX_U  ! Surface flux u'w' (m2/s2)
REAL, DIMENSION(KI)        :: ZSFLUX_T  ! Surface flux w'T' (mK/s)
REAL, DIMENSION(KI)        :: ZSFLUX_Q  ! Surface flux w'q' (kgm2/s)
REAL, DIMENSION(KI,SB%NLVL)   :: ZFORC_U   ! tendency due to drag force for wind
REAL, DIMENSION(KI,SB%NLVL)   :: ZDFORC_UDU! formal derivative of
!                                              ! tendency due to drag force for wind
REAL, DIMENSION(KI,SB%NLVL)   :: ZFORC_E   ! tendency due to drag force for TKE
REAL, DIMENSION(KI,SB%NLVL)   :: ZDFORC_EDE! formal derivative of
!                                              ! tendency due to drag force for TKE
REAL, DIMENSION(KI,SB%NLVL)   :: ZFORC_T   ! tendency due to drag force for Temp
REAL, DIMENSION(KI,SB%NLVL)   :: ZDFORC_TDT! formal derivative of
!                                              ! tendency due to drag force for Temp
REAL, DIMENSION(KI,SB%NLVL)   :: ZFORC_Q   ! tendency due to drag force for Temp
REAL, DIMENSION(KI,SB%NLVL)   :: ZDFORC_QDQ! formal derivative of
!                                              ! tendency due to drag force for hum.
REAL, DIMENSION(KI,SB%NLVL)   :: ZLM       ! mixing length
REAL, DIMENSION(KI,SB%NLVL)   :: ZLEPS     ! dissipative length
REAL, DIMENSION(KI)     :: ZH           ! canopy height (m)
REAL, DIMENSION(KI)     :: ZUSTAR       ! friction velocity (m/s)
!
REAL, DIMENSION(KI)     :: ZPET_A_COEF ! temperature implicit
REAL, DIMENSION(KI)     :: ZPET_B_COEF ! coefficients (K)
REAL, DIMENSION(KI)     :: ZPEW_A_COEF ! wind implicit
REAL, DIMENSION(KI)     :: ZPEW_B_COEF ! coefficients (m/s)

REAL, DIMENSION(KI)   :: ZALFAU   ! V+(1) = - alfa rho u'w'(1) + beta
REAL, DIMENSION(KI)   :: ZBETAU   ! V+(1) = - alfa rho u'w'(1) + beta
REAL, DIMENSION(KI)   :: ZALFATH  ! Th+(1) = - alfa rho w'th'(1) + beta
REAL, DIMENSION(KI)   :: ZBETATH  ! Th+(1) = - alfa rho w'th'(1) + beta
REAL, DIMENSION(KI)   :: ZALFAQ   ! Q+(1) = - alfa rho w'q'(1) + beta
REAL, DIMENSION(KI)   :: ZBETAQ   ! Q+(1) = - alfa rho w'q'(1) + beta
!
INTEGER :: JLAYER
 CHARACTER(LEN=1) :: GCOUPLING
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
!
!*      1.     Preliminary computations of the SBL scheme
!              ------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('COUPLING_WATFLUX_SBL_N',0,ZHOOK_HANDLE)
IF (W%LSBL) THEN
!
!*      1.1    Updates SBL vertical grid as a function of forcing height
!              ---------------------------------------------------------
!
!* determines where is the forcing level and modifies the upper levels of the canopy grid
!
  ZH = 0.
  CALL CANOPY_GRID_UPDATE(KI,ZH,PUREF,SB)
!
!
!
!*     1.2     Initialisation at first time step
!              ---------------------------------
!
  IF(ANY(SB%XT(:,:) == XUNDEF)) THEN
    CALL INIT_WATER_SBL(SB, PPA, PPS, PTA, PQA, PRHOA, PU, PV, PRAIN, PSNOW,  &
                        PSFTH, PSFTQ, PZREF, PUREF, W%XTS, W%XZ0)
  ENDIF
!
!
!*      1.3    Allocations
!              -----------
!
  CALL INIT_FORC( ZFORC_U, ZDFORC_UDU, ZFORC_E, ZDFORC_EDE, &
                  ZFORC_T, ZDFORC_TDT, ZFORC_Q, ZDFORC_QDQ )
!
  ZSFLUX_U = 0.
  ZSFLUX_T = 0.
  ZSFLUX_Q = 0.
!
!*      1.3   Computes coefficients for implicitation
!             ---------------------------------------
!
  ZWIND = SQRT(PU**2+PV**2)
  CALL CANOPY_EVOL(SB,KI,PTSTEP,1,SB%XZ,ZWIND,PTA,PQA,PPA,PRHOA,   &
                   ZSFLUX_U,ZSFLUX_T,ZSFLUX_Q,ZFORC_U,ZDFORC_UDU,&
                   ZFORC_E,ZDFORC_EDE,ZFORC_T,ZDFORC_TDT,ZFORC_Q,ZDFORC_QDQ, &
                   ZLM,ZLEPS,ZUSTAR,ZALFAU,ZBETAU,ZALFATH,ZBETATH,ZALFAQ,ZBETAQ )

!
!*     1.5     Goes from atmospheric forcing to canopy forcing height
!              ------------------------------------------------------
!
  GCOUPLING = 'I'
!
  CALL INIT_COUPLING_CANOPY( SB, PPA, PU, PV,  &
                            PRHOA, ZALFAU, ZBETAU, ZALFATH,   &
                            ZBETATH, ZALFAQ, ZBETAQ,          &
                            ZPA, ZTA, ZQA, ZU, ZV,            &
                            ZUREF, ZZREF, ZEXNA,              &
                            ZPEW_A_COEF, ZPEW_B_COEF,         &
                            ZPET_A_COEF, ZPET_B_COEF,         &
                            ZPEQ_A_COEF, ZPEQ_B_COEF          )
!
!-------------------------------------------------------------------------------------
ELSE
!-------------------------------------------------------------------------------------
!
!*      2.     If no SBL scheme is used, forcing is not modified
!              -------------------------------------------------
!
  GCOUPLING = HCOUPLING
!
  CALL INIT_COUPLING( HCOUPLING,                  &
                      PPS, PPA, PTA, PQA, PU, PV, &
                      PUREF, PZREF,               &
                      PPEW_A_COEF, PPEW_B_COEF,   &
                      PPET_A_COEF, PPET_B_COEF,   &
                      PPEQ_A_COEF, PPEQ_B_COEF,   &
                      ZPA, ZTA, ZQA, ZU, ZV,      &
                      ZUREF, ZZREF,               &
                      ZPEW_A_COEF, ZPEW_B_COEF,   &
                      ZPET_A_COEF, ZPET_B_COEF,   &
                      ZPEQ_A_COEF, ZPEQ_B_COEF    ) 
!
END IF
!
!-------------------------------------------------------------------------------------
!
!*      2.     Call of SEAFLUX
!              ------------
!
  CALL COUPLING_WATFLUX_n(CHW, DGO, D, DC, W, DST, SLT, HPROGRAM, GCOUPLING, PTIMEC,    &
                          PTSTEP, KYEAR, KMONTH, KDAY, PTIME, KI, KSV, KSW, PTSUN, PZENITH, &
                          PZENITH2, PAZIM, ZZREF, ZUREF, ZU, ZV, ZQA, ZTA, PRHOA, PSV, PCO2,&
                          HSV, PRAIN, PSNOW, PLW, PDIR_SW, PSCA_SW, PSW_BANDS, PPS, ZPA,    &
                          PSFTQ, PSFTH, PSFTS, PSFCO2, PSFU, PSFV, PTRAD, PDIR_ALB, PSCA_ALB,&
                          PEMIS, PTSURF, PZ0, PZ0H, PQSURF, ZPEW_A_COEF, ZPEW_B_COEF,       &
                          ZPET_A_COEF, ZPEQ_A_COEF, ZPET_B_COEF, ZPEQ_B_COEF,'OK'     )
!
!-------------------------------------------------------------------------------------
!
!*      3.     End if no SBL is used
!              ---------------------
!
IF (.NOT. W%LSBL .AND. LHOOK) CALL DR_HOOK('COUPLING_WATFLUX_SBL_N',1,ZHOOK_HANDLE)
IF (.NOT. W%LSBL) RETURN
!
!-------------------------------------------------------------------------------------
!
!*      4.     Computes the impact of canopy and surfaces on air
!              -------------------------------------------------
!
 CALL INIT_FORC( ZFORC_U, ZDFORC_UDU, ZFORC_E, ZDFORC_EDE, &
                 ZFORC_T, ZDFORC_TDT, ZFORC_Q, ZDFORC_QDQ )
!
ZSFLUX_U = - SQRT(PSFU(:)**2+PSFV(:)**2) / PRHOA(:)
ZSFLUX_T(:) = PSFTH(:) / XCPD * ZEXNA(:) / PRHOA(:)
ZSFLUX_Q(:) = PSFTQ(:)
!
!-------------------------------------------------------------------------------------
!
!*      6.    Evolution of canopy air due to these impacts
!             --------------------------------------------
!
ZWIND = SQRT(PU**2+PV**2)
 CALL CANOPY_EVOL(SB,KI,PTSTEP,2,SB%XZ,ZWIND,PTA,PQA,PPA,PRHOA,  &
                  ZSFLUX_U,ZSFLUX_T,ZSFLUX_Q,ZFORC_U,ZDFORC_UDU,   &
                  ZFORC_E,ZDFORC_EDE,ZFORC_T,ZDFORC_TDT, &
                  ZFORC_Q,ZDFORC_QDQ,ZLM,ZLEPS,ZUSTAR,             &
                  ZALFAU,ZBETAU,ZALFATH,ZBETATH,ZALFAQ,ZBETAQ     )
!
DO JLAYER=1,SB%NLVL-1
  SB%XLMO(:,JLAYER) = SB%XLMO(:,SB%NLVL)
ENDDO
!
!-------------------------------------------------------------------------------------
!
!*      7.    2m and 10m diagnostics if canopy is used
!             ----------------------------------------
!
!
IF (W%LSBL .AND. DGO%N2M>=1) CALL INIT_2M_10M( SB, D, PU, PV, ZWIND, PRHOA )
!
IF (LHOOK) CALL DR_HOOK('COUPLING_WATFLUX_SBL_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE COUPLING_WATFLUX_SBL_n
