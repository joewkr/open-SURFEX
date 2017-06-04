!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###############################################################################
SUBROUTINE COUPLING_ISBA_CANOPY_n (DTCO, UG, U, USS, SB, NAG, CHI, NCHI, DTV, ID, NGB, GB, &
                                   ISS, NISS, IG, NIG, IO, S, K, NK, NP, NPE, NDST, SLT,   &
                                   HPROGRAM, HCOUPLING, PTSTEP,                            &
                                   KYEAR, KMONTH, KDAY, PTIME, KI, KSV, KSW, PTSUN,        &
                                   PZENITH, PZENITH2, PAZIM, PZREF, PUREF, PZS, PU, PV,    &
                                   PQA, PTA, PRHOA, PSV, PCO2, HSV, PRAIN, PSNOW, PLW,     &
                                   PDIR_SW, PSCA_SW, PSW_BANDS, PPS, PPA, PSFTQ, PSFTH,    &
                                   PSFTS, PSFCO2, PSFU, PSFV, PTRAD, PDIR_ALB, PSCA_ALB,   &
                                   PEMIS, PTSURF, PZ0,PZ0H, PQSURF, PPEW_A_COEF,           &
                                   PPEW_B_COEF, PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF,     &
                                   PPEQ_B_COEF, HTEST  )
!     ###############################################################################
!
!!****  *COUPLING_ISBA_CANOPY_n * - Adds a SBL into ISBA
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
!!      S. Riette   06/2009 Initialisation of XT, XQ, XU and XTKE on canopy levels
!!      S. Riette   01/2010 Use of interpol_sbl to compute 10m wind diagnostic
!!      Modified    09/2012  : J. Escobar , SIZE(PTA) not allowed without-interface , replace by KI
!!      B. Decharme  04/2013 new coupling variables
!----------------------------------------------------------------
!
USE MODD_AGRI_n, ONLY : AGRI_NP_t
USE MODD_CH_ISBA_n, ONLY : CH_ISBA_t, CH_ISBA_NP_t
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_SURFEX_n, ONLY : ISBA_DIAG_t
USE MODD_GR_BIOG_n, ONLY : GR_BIOG_t, GR_BIOG_NP_t
USE MODD_SSO_n, ONLY : SSO_t, SSO_NP_t
USE MODD_SFX_GRID_n, ONLY : GRID_t, GRID_NP_t
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_S_t, ISBA_K_t, ISBA_P_t, ISBA_PE_t, ISBA_NK_t, ISBA_NP_t, ISBA_NPE_t
!
USE MODD_DST_n, ONLY : DST_NP_t

USE MODD_CANOPY_n, ONLY : CANOPY_t
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_SLT_n, ONLY : SLT_t
!
!
USE MODD_CSTS,          ONLY : XCPD
USE MODD_SURF_PAR,      ONLY : XUNDEF
USE MODD_CANOPY_TURB,   ONLY : XALPSBL
!
USE MODE_COUPLING_CANOPY
!
USE MODI_INIT_ISBA_SBL
!
USE MODI_CANOPY_EVOL
USE MODI_CANOPY_GRID_UPDATE
!
USE MODI_COUPLING_ISBA_n
!
USE MODI_ISBA_CANOPY
USE MODI_SSO_BELJAARS04
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(AGRI_NP_t), INTENT(INOUT) :: NAG
TYPE(CH_ISBA_t), INTENT(INOUT) :: CHI
TYPE(CH_ISBA_NP_t), INTENT(INOUT) :: NCHI
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTV
TYPE(ISBA_DIAG_t), INTENT(INOUT) :: ID
TYPE(GR_BIOG_NP_t), INTENT(INOUT) :: NGB
TYPE(GR_BIOG_t), INTENT(INOUT) :: GB
TYPE(SSO_t), INTENT(INOUT) :: ISS 
TYPE(SSO_NP_t), INTENT(INOUT) :: NISS
TYPE(GRID_t), INTENT(INOUT) :: IG
TYPE(GRID_NP_t), INTENT(INOUT) :: NIG
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t), INTENT(INOUT) :: S
TYPE(ISBA_K_t), INTENT(INOUT) :: K
TYPE(ISBA_NK_t), INTENT(INOUT) :: NK
TYPE(ISBA_NP_t), INTENT(INOUT) :: NP
TYPE(ISBA_NPE_t), INTENT(INOUT) ::NPE
!
TYPE(DST_NP_t), INTENT(INOUT) :: NDST
!
TYPE(CANOPY_t), INTENT(INOUT) :: SB
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
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
REAL, DIMENSION(KI), INTENT(IN)  :: PZENITH   ! zenithal angle at t       (radian from the vertical)
REAL, DIMENSION(KI), INTENT(IN)  :: PZENITH2  ! zenithal angle at t+1     (radian from the vertical)
REAL, DIMENSION(KI), INTENT(IN)  :: PAZIM     ! azimuthal angle      (radian from North, clockwise)
REAL, DIMENSION(KI), INTENT(IN)  :: PLW       ! longwave radiation (on horizontal surf.)
!                                             !                                       (W/m2)
REAL, DIMENSION(KI), INTENT(IN)  :: PPS       ! pressure at atmospheric model surface (Pa)
REAL, DIMENSION(KI), INTENT(IN)  :: PPA       ! pressure at forcing level             (Pa)
REAL, DIMENSION(KI), INTENT(IN)  :: PZS       ! atmospheric model CANOPY           (m)
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
TYPE(ISBA_P_t), POINTER :: PK
TYPE(ISBA_PE_t), POINTER :: PEK
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
! canopy turbulence scheme
!
REAL, DIMENSION(KI)        :: ZCANOPY   ! height of canopy   (m)
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
REAL, DIMENSION(KI)     :: ZUSTAR       ! friction velocity including drag effect (m/s)
REAL, DIMENSION(KI)     :: ZUSTAR_GROUND! friction velocity at ground only (ISBA) (m/s)
!
REAL, DIMENSION(KI)     :: ZPET_A_COEF ! temperature implicit
REAL, DIMENSION(KI)     :: ZPET_B_COEF ! coefficients (K)
REAL, DIMENSION(KI)     :: ZPEW_A_COEF ! wind implicit
REAL, DIMENSION(KI)     :: ZPEW_B_COEF ! coefficients (m/s)
!
REAL, DIMENSION(KI)   :: ZALFAU   ! V+(1) = - alfa rho u'w'(1) + beta
REAL, DIMENSION(KI)   :: ZBETAU   ! V+(1) = - alfa rho u'w'(1) + beta
REAL, DIMENSION(KI)   :: ZALFATH  ! Th+(1) = - alfa rho w'th'(1) + beta
REAL, DIMENSION(KI)   :: ZBETATH  ! Th+(1) = - alfa rho w'th'(1) + beta
REAL, DIMENSION(KI)   :: ZALFAQ   ! Q+(1) = - alfa rho w'q'(1) + beta
REAL, DIMENSION(KI)   :: ZBETAQ   ! Q+(1) = - alfa rho w'q'(1) + beta
!
 CHARACTER(LEN=1) :: GCOUPLING
!
REAL, DIMENSION(KI)   ::ZCANOPY_DENSITY
REAL, DIMENSION(KI)   ::ZUW_GROUND
REAL, DIMENSION(KI)   ::ZDUWDU_GROUND
!
INTEGER                      :: JJ, JLAYER, IMASK, JP, JI
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!-------------------------------------------------------------------------------------
!
!
!*      1.     Preliminary computations of the SBL scheme
!              ------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('COUPLING_ISBA_CANOPY_N',0,ZHOOK_HANDLE)
!
IF (IO%LCANOPY) THEN
!
!*      1.1    Updates canopy vertical grid as a function of forcing height
!              ------------------------------------------------------------
!
!* determines where is the forcing level and modifies the upper levels of the canopy grid
!
  ZCANOPY = 0.
  CALL CANOPY_GRID_UPDATE(KI,ZCANOPY,PUREF,SB)
!
!
!
!*      1.2    Allocations and initialisations
!              -------------------------------
!
!
!       1.2.1  First time step canopy initialisation
!
  IF(ANY(SB%XT(:,:) == XUNDEF)) THEN
    CALL INIT_ISBA_SBL(IO, K, NP, NPE, SB, &
                       PTSTEP, PPA, PPS, PTA, PQA, PRHOA, PU, PV, PDIR_SW,  &
                       PSCA_SW, PSW_BANDS, PRAIN, PSNOW, PZREF, PUREF, ISS%XSSO_SLOPE )
  ENDIF
!
!*      1.3    Allocations
!              -----------
!
  CALL INIT_FORC(ZFORC_U, ZDFORC_UDU, ZFORC_E, ZDFORC_EDE, ZFORC_T, ZDFORC_TDT, ZFORC_Q, ZDFORC_QDQ )
!
  ZSFLUX_U = 0.
  ZSFLUX_T = 0.
  ZSFLUX_Q = 0.
!
!* default :
!* no canopy in ISBA scheme
!
  ZH = 0.
  ZCANOPY_DENSITY(:) = 0.
!
!
!
!*      1.4   canopy for wind drag only
!             -------------------------
!
  IF (IO%LCANOPY_DRAG) THEN
!* mean canopy height
!
!* in ecoclimap, height is set retrieved from roughness length (z0/0.13)
    DO JP = 1,IO%NPATCH
      PK => NP%AL(JP)
      PEK => NPE%AL(JP)
      DO JJ = 1,PK%NSIZE_P
        IMASK = PK%NR_P(JJ)
        ZH(IMASK) = ZH(IMASK) + PK%XPATCH(JJ) * PEK%XZ0(JJ) / 0.13
        ZCANOPY_DENSITY(IMASK) = ZCANOPY_DENSITY(IMASK) + PK%XPATCH(JJ) *  PEK%XLAI(JJ)
      ENDDO
    ENDDO
    !
    DO JJ=1,KI
      ZH(JJ) = MIN(ZH(JJ), SB%XZF(JJ,SB%NLVL))
      IF (ZH(JJ)<=SB%XDZ(JJ,1)) ZH(JJ) = 0.
!
!* canopy for wind drag only
      ZUW_GROUND(JJ)      = 0.
      ZDUWDU_GROUND(JJ)   = 0.
      !
    ENDDO
!
!* computes tendencies on wind and Tke due to canopy
    CALL ISBA_CANOPY(IO%XCDRAG, KI, SB, ZH,ZCANOPY_DENSITY, ZUW_GROUND, &
                     ZDUWDU_GROUND, ZFORC_U, ZDFORC_UDU, ZFORC_E, ZDFORC_EDE )
!
  ENDIF
!
!*      1.5   Computes coefficients for implicitation
!             ---------------------------------------
!
  ZWIND = SQRT(PU**2+PV**2)
  CALL CANOPY_EVOL(SB, KI, PTSTEP, 1, SB%XZ, ZWIND, PTA, PQA, PPA, PRHOA, &
                   ZSFLUX_U, ZSFLUX_T, ZSFLUX_Q, ZFORC_U, ZDFORC_UDU,       &
                   ZFORC_E, ZDFORC_EDE, ZFORC_T, ZDFORC_TDT, ZFORC_Q,       &
                   ZDFORC_QDQ, ZLM, ZLEPS, ZUSTAR, ZALFAU, ZBETAU, ZALFATH, &
                   ZBETATH, ZALFAQ, ZBETAQ   )
!
!*     1.6     Goes from atmospheric forcing to canopy forcing height
!              ------------------------------------------------------
!
  GCOUPLING ='I'
!
  CALL INIT_COUPLING_CANOPY(SB, PPA, PU, PV, PRHOA, ZALFAU, &
                            ZBETAU, ZALFATH, ZBETATH, ZALFAQ,&
                            ZBETAQ, ZPA, ZTA, ZQA, ZU, ZV,   &
                            ZUREF, ZZREF, ZEXNA, ZPEW_A_COEF,&
                            ZPEW_B_COEF, ZPET_A_COEF,        &
                            ZPET_B_COEF, ZPEQ_A_COEF,        &
                            ZPEQ_B_COEF          )
!
!-------------------------------------------------------------------------------------
ELSE
!-------------------------------------------------------------------------------------
!
!*      2.     If no canopy scheme is used, forcing is not modified
!              ----------------------------------------------------
!
  GCOUPLING = HCOUPLING
!
  CALL INIT_COUPLING(HCOUPLING, PPS, PPA, PTA, PQA, &
                     PU, PV, PUREF, PZREF,          &
                     PPEW_A_COEF, PPEW_B_COEF,      &
                     PPET_A_COEF, PPET_B_COEF,      &
                     PPEQ_A_COEF, PPEQ_B_COEF,      &
                     ZPA, ZTA, ZQA, ZU, ZV, ZUREF,  &
                     ZZREF, ZPEW_A_COEF,            &
                     ZPEW_B_COEF, ZPET_A_COEF,      &
                     ZPET_B_COEF, ZPEQ_A_COEF,      &
                     ZPEQ_B_COEF    ) 
! 
END IF
!
!-------------------------------------------------------------------------------------
!
!*      2.     Call of ISBA
!              ------------
!
 CALL COUPLING_ISBA_n(DTCO, UG, U, USS, NAG, CHI, NCHI, DTV, ID, NGB, GB, ISS,NISS, IG, &
                      NIG, IO, S, K, NK, NP, NPE, NDST, SLT, HPROGRAM, GCOUPLING,       &
                      PTSTEP, KYEAR, KMONTH, KDAY, PTIME, KI, KSV, KSW, PTSUN, PZENITH, &
                      PZENITH2, ZZREF, ZUREF, PZS, ZU, ZV, ZQA, ZTA, PRHOA, PSV, PCO2,  &
                      HSV, PRAIN, PSNOW, PLW, PDIR_SW, PSCA_SW, PSW_BANDS, PPS, ZPA,    &
                      PSFTQ, PSFTH, PSFTS, PSFCO2, PSFU, PSFV, PTRAD, PDIR_ALB,         &
                      PSCA_ALB, PEMIS, PTSURF, PZ0, PZ0H, PQSURF, ZPEW_A_COEF,          &
                      ZPEW_B_COEF, ZPET_A_COEF, ZPEQ_A_COEF, ZPET_B_COEF, ZPEQ_B_COEF,  &
                      'OK' )
!
!-------------------------------------------------------------------------------------
!
!*      3.     End if no canopy is used
!              ------------------------
!
IF (.NOT. IO%LCANOPY .AND. LHOOK) CALL DR_HOOK('COUPLING_ISBA_CANOPY_N',1,ZHOOK_HANDLE)
IF (.NOT. IO%LCANOPY) RETURN
!
!-------------------------------------------------------------------------------------
!
!*      4.     Computes the impact of surface on air
!              -------------------------------------
!
 CALL INIT_FORC(ZFORC_U, ZDFORC_UDU, ZFORC_E, ZDFORC_EDE, &
                ZFORC_T, ZDFORC_TDT, ZFORC_Q, ZDFORC_QDQ )
!
ZSFLUX_U = - SQRT(PSFU(:)**2+PSFV(:)**2) / PRHOA(:)
ZSFLUX_T(:) = PSFTH(:) / XCPD * ZEXNA(:) / PRHOA(:)
ZSFLUX_Q(:) = PSFTQ(:)
!
!-------------------------------------------------------------------------------------
!
!*      5.     Computes the impact of canopy on air
!              ------------------------------------
!
IF (IO%LCANOPY_DRAG) THEN
!
  DO JJ=1,KI
    ZUW_GROUND   (JJ) = -SQRT(PSFU(JJ)**2+PSFV(JJ)**2)/ PRHOA(JJ)
    ZDUWDU_GROUND(JJ) = 0.
    IF (SB%XU(JJ,1)/=0.) ZDUWDU_GROUND(JJ) = 2. * ZUW_GROUND(JJ) / SB%XU(JJ,1)
  ENDDO

!* computes tendencies on wind and Tke due to canopy and surface
  CALL ISBA_CANOPY(IO%XCDRAG, KI, SB, ZH, ZCANOPY_DENSITY, ZUW_GROUND, &
                   ZDUWDU_GROUND, ZFORC_U, ZDFORC_UDU, ZFORC_E, ZDFORC_EDE  )

  ZSFLUX_U = 0.  ! surface friction is incorporated in ZFORC_U by ISBA_CANOPY routine
!
END IF
!
!-------------------------------------------------------------------------------------
!
!*      6.    Evolution of canopy air due to these impacts
!             --------------------------------------------
!
ZWIND = SQRT(PU**2+PV**2)
 CALL CANOPY_EVOL(SB, KI, PTSTEP, 2, SB%XZ, ZWIND, PTA, PQA, PPA, PRHOA,  &
                  ZSFLUX_U, ZSFLUX_T, ZSFLUX_Q, ZFORC_U, ZDFORC_UDU,ZFORC_E,&
                  ZDFORC_EDE, ZFORC_T, ZDFORC_TDT, ZFORC_Q, ZDFORC_QDQ, ZLM,&
                  ZLEPS, ZUSTAR, ZALFAU, ZBETAU, ZALFATH, ZBETATH, ZALFAQ,  &
                  ZBETAQ                  )
!
DO JLAYER=1,SB%NLVL-1
  SB%XLMO(:,JLAYER) = SB%XLMO(:,SB%NLVL)
ENDDO
!
! Momentum fluxes if canopy is used
!
!* Total friction due to surface averaged friction and averaged canopy drag
IF (IO%LCANOPY_DRAG) THEN
  DO JJ=1,KI
    ZUSTAR_GROUND(JJ) = SQRT(SQRT(PSFU(JJ)**2+PSFV(JJ)**2)/PRHOA(JJ))
    IF (ZUSTAR_GROUND(JJ)>0.) THEN
      PSFU(JJ) = PSFU(JJ) * ZUSTAR(JJ)**2/ZUSTAR_GROUND(JJ)**2
      PSFV(JJ) = PSFV(JJ) * ZUSTAR(JJ)**2/ZUSTAR_GROUND(JJ)**2
    ENDIF
  ENDDO
!* Total friction due to surface averaged friction and averaged canopy drag
  IF (ID%O%LSURF_BUDGET) THEN
    ID%D%XFMU = PSFU
    ID%D%XFMV = PSFV          
  ENDIF
END IF
!
!-------------------------------------------------------------------------------------
!
!*      7.    2m and 10m diagnostics if canopy is used
!             ----------------------------------------
!
IF (ID%O%N2M>=1) CALL INIT_2M_10M(SB, ID%D, PU, PV, ZWIND, PRHOA )
!
IF (LHOOK) CALL DR_HOOK('COUPLING_ISBA_CANOPY_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE COUPLING_ISBA_CANOPY_n
