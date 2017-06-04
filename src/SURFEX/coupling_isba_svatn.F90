!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###############################################################################
SUBROUTINE COUPLING_ISBA_SVAT_n (DTCO, UG, U, USS, IM, NDST, SLT, HPROGRAM, HCOUPLING, PTSTEP, &
                                 KYEAR, KMONTH, KDAY, PTIME, KI, KSV, KSW, PTSUN, PZENITH,    &
                                 PZENITH2, PAZIM, PZREF, PUREF, PZS, PU, PV, PQA, PTA, PRHOA, &
                                 PSV, PCO2, HSV, PRAIN, PSNOW, PLW, PDIR_SW, PSCA_SW,         &
                                 PSW_BANDS, PPS, PPA, PSFTQ, PSFTH, PSFTS, PSFCO2, PSFU, PSFV,&
                                 PTRAD, PDIR_ALB, PSCA_ALB, PEMIS, PTSURF, PZ0, PZ0H, PQSURF, &
                                 PPEW_A_COEF, PPEW_B_COEF, PPET_A_COEF, PPEQ_A_COEF,          &
                                 PPET_B_COEF, PPEQ_B_COEF, HTEST    )  
!     ###############################################################################
!
!!****  *COUPLING_ISBA_SVAT_n * - Chooses the time method (explicit, 
!!        implicit, time-spliting) for ISBA scheme   
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
!!     A. Bogatchev 09/2005 EBA snow option
!!     A. Boone     11/2009 Exner correction for Offline T-B coef
!!     B. Decharme  11/2009 Implicit coupling ok with all snow scheme
!!     B. Decharme  04/2013 new coupling variables and init local variables
!!-------------------------------------------------------------------
!
!
USE MODD_SURFEX_n, ONLY : ISBA_MODEL_t
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
USE MODD_DST_n, ONLY : DST_NP_t
USE MODD_SLT_n, ONLY : SLT_t
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_COUPLING_ISBA_OROGRAPHY_n
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(ISBA_MODEL_t), INTENT(INOUT) :: IM
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
TYPE(DST_NP_t), INTENT(INOUT) :: NDST
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
REAL, DIMENSION(KI), INTENT(IN)  :: PZENITH   ! zenithal angle at t      (radian from the vertical)
REAL, DIMENSION(KI), INTENT(IN)  :: PZENITH2  ! zenithal angle at t+1    (radian from the vertical)
REAL, DIMENSION(KI), INTENT(IN)  :: PAZIM     ! azimuthal angle      (radian from North, clockwise)
REAL, DIMENSION(KI), INTENT(IN)  :: PLW       ! longwave radiation (on horizontal surf.)
!                                             !                                       (W/m2)
REAL, DIMENSION(KI), INTENT(IN)  :: PPS       ! pressure at atmospheric model surface (Pa)
REAL, DIMENSION(KI), INTENT(IN)  :: PPA       ! pressure at forcing level             (Pa)
REAL, DIMENSION(KI), INTENT(IN)  :: PZS       ! atmospheric model orography           (m)
REAL, DIMENSION(KI), INTENT(IN)  :: PCO2      ! CO2 concentration in the air          (kg/m3)
REAL, DIMENSION(KI), INTENT(IN)  :: PSNOW     ! snow precipitation                    (kg/m2/s)
REAL, DIMENSION(KI), INTENT(IN)  :: PRAIN     ! liquid precipitation                  (kg/m2/s)
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
REAL, DIMENSION(KI)     :: ZSFTH   ! surface temperature flux 
REAL, DIMENSION(KI)     :: ZSFTQ   ! surface water vapor flux 
REAL, DIMENSION(KI)     :: ZSFCO2  ! surface CO2 flux 
REAL, DIMENSION(KI,KSV) :: ZSFTS   ! surface scalar flux   
REAL, DIMENSION(KI)     :: ZSFU    ! zonal momentum flux
REAL, DIMENSION(KI)     :: ZSFV    ! meridian momentum flux
REAL, DIMENSION(KI)     :: ZTRAD   ! surface radiative temperature
REAL, DIMENSION(KI)     :: ZEMIS   ! surface emissivity
REAL, DIMENSION(KI,KSW) :: ZDIR_ALB! direct surface albedo
REAL, DIMENSION(KI,KSW) :: ZSCA_ALB! diffuse surface albedo
REAL, DIMENSION(KI)     :: ZTSURF  ! surface effective temperature         (K)
REAL, DIMENSION(KI)     :: ZZ0     ! roughness length for momentum         (m)
REAL, DIMENSION(KI)     :: ZZ0H    ! roughness length for heat             (m)
REAL, DIMENSION(KI)     :: ZQSURF  ! specific humidity at surface          (kg/kg)
!
REAL, DIMENSION(KI)     :: ZWORK_LW  ! work array for mean upward longwave surface flux
REAL, DIMENSION(KI)     :: ZWORK_Z0  ! work array for mean roughness length for momentum
REAL, DIMENSION(KI)     :: ZWORK_Z0H ! work array for mean roughness length for heat
!
INTEGER :: JT      ! time loop counter
INTEGER :: IT      ! total number of surface timesteps in one atmospheric timestep
REAL    :: ZT      ! total number of surface timesteps in one atmospheric timestep
REAL    :: ZTSTEP  ! surface time step
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
!*      1.     number of time-steps
!              --------------------
!
!* only one timestep in Implicit coupling
IF (LHOOK) CALL DR_HOOK('COUPLING_ISBA_SVAT_N',0,ZHOOK_HANDLE)
IF (HCOUPLING=='I') THEN
  IT=1
  ZT=1.
  ZTSTEP=PTSTEP
!
!* same timestep as atmospheric timestep as default
ELSE IF (IM%O%XTSTEP==XUNDEF) THEN
  IT=1
  ZT=1.
  ZTSTEP=PTSTEP
!
!* case of specified SVAT time-step
ELSE
  IT=MAX(NINT(PTSTEP/IM%O%XTSTEP),1)
  ZT=FLOAT(IT)
  ZTSTEP=PTSTEP/ZT
ENDIF
!
!*      3.     initialization of outputs
!              -------------------------
!
PSFTQ   = 0.0
PSFTH   = 0.0
PSFTS   = 0.0
PSFCO2  = 0.0
PSFU    = 0.0
PSFV    = 0.0
PTRAD   = 0.0
PDIR_ALB= 0.0
PSCA_ALB= 0.0
PEMIS   = 0.0
PTSURF  = 0.0
PZ0     = 0.0
PZ0H    = 0.0
PQSURF  = 0.0
!
ZSFTH   = 0.0  ! surface temperature flux 
ZSFTQ   = 0.0  ! surface water vapor flux 
ZSFCO2  = 0.0  ! surface CO2 flux 
ZSFTS   = 0.0  ! surface scalar flux   
ZSFU    = 0.0  ! zonal momentum flux
ZSFV    = 0.0  ! meridian momentum flux
ZTRAD   = 0.0  ! surface radiative temperature
ZEMIS   = 0.0  ! surface emissivity
ZDIR_ALB= 0.0  ! direct surface albedo
ZSCA_ALB= 0.0  ! diffuse surface albedo
ZTSURF  = 0.0  ! surface effective temperature         (K)
ZZ0     = 0.0  ! roughness length for momentum         (m)
ZZ0H    = 0.0  ! roughness length for heat             (m)
ZQSURF  = 0.0  ! specific humidity at surface
!
ZWORK_LW = 0.0 ! work array for mean upward longwave surface flux
ZWORK_Z0 = 0.0 ! work array for mean roughness length for momentum
ZWORK_Z0H= 0.0 ! work array for mean roughness length for heat
!
!*      4.     loop on surface time-step
!              -------------------------
!
DO JT=1,IT
!
  CALL COUPLING_ISBA_OROGRAPHY_n(DTCO, UG, U, USS, IM%SB, IM%NAG, IM%CHI, IM%NCHI, IM%DTV,     &
                                 IM%ID, IM%NGB, IM%GB, IM%ISS, IM%NISS, IM%G, IM%NG, IM%O,     &
                                 IM%S, IM%K, IM%NK, IM%NP, IM%NPE, NDST, SLT,                  &
                                 HPROGRAM, HCOUPLING, ZTSTEP, KYEAR, KMONTH, KDAY, PTIME, KI,  &
                                 KSV, KSW, PTSUN, PZENITH, PZENITH2, PAZIM, PZREF, PUREF, PZS, &
                                 PU, PV, PQA, PTA, PRHOA, PSV, PCO2, HSV, PRAIN, PSNOW, PLW,   &
                                 PDIR_SW, PSCA_SW, PSW_BANDS, PPS, PPA, ZSFTQ, ZSFTH, ZSFTS,   &
                                 ZSFCO2, ZSFU, ZSFV, ZTRAD, ZDIR_ALB, ZSCA_ALB, ZEMIS, ZTSURF, &
                                 ZZ0, ZZ0H, ZQSURF, PPEW_A_COEF, PPEW_B_COEF, PPET_A_COEF,     &
                                 PPEQ_A_COEF, PPET_B_COEF, PPEQ_B_COEF, 'OK'    ) 
!
  PSFTQ    = PSFTQ    + ZSFTQ    / ZT
  PSFTH    = PSFTH    + ZSFTH    / ZT
  PSFTS    = PSFTS    + ZSFTS    / ZT
  PSFCO2   = PSFCO2   + ZSFCO2   / ZT
  PSFU     = PSFU     + ZSFU     / ZT
  PSFV     = PSFV     + ZSFV     / ZT
  PEMIS    = PEMIS    + ZEMIS    / ZT
  PDIR_ALB = PDIR_ALB + ZDIR_ALB / ZT
  PSCA_ALB = PSCA_ALB + ZSCA_ALB / ZT
  PTSURF   = PTSURF   + ZTSURF   / ZT
  PQSURF   = PQSURF   + ZQSURF   / ZT
!  
  ZWORK_LW  = ZWORK_LW  + ZEMIS*ZTRAD**4 / ZT
  ZWORK_Z0  = ZWORK_Z0  + (1.0/(LOG(PUREF(:)/ZZ0 ))**2) / ZT
  ZWORK_Z0H = ZWORK_Z0H + (1.0/(LOG(PZREF(:)/ZZ0H))**2) / ZT
!  
END DO
!
!* radiative temperature retrieved from upward longwave flux
!
PTRAD = (ZWORK_LW/PEMIS)**(0.25)
!
!* roughness length for momentum and heat
!
PZ0  = PUREF(:) * EXP( - SQRT(1./ZWORK_Z0 (:)) )
PZ0H = PZREF(:) * EXP( - SQRT(1./ZWORK_Z0H(:)) )
!
IF (LHOOK) CALL DR_HOOK('COUPLING_ISBA_SVAT_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE COUPLING_ISBA_SVAT_n
