!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#############################################################
SUBROUTINE INIT_ISBA_n (DTCO, OREAD_BUDGETC, UG, U, USS, GCP, IM, DTZ,&
                        NDST, SLT, SV, HPROGRAM, HINIT, OLAND_USE,    &
                        KI, KSV, KSW, HSV, PCO2, PRHOA, PZENITH,      &
                        PAZIM, PSW_BANDS, PDIR_ALB, PSCA_ALB, PEMIS,  &
                        PTSRAD, PTSURF, KYEAR, KMONTH, KDAY, PTIME,   &
                        TPDATE_END, HATMFILE, HATMFILETYPE, HTEST      )
!#############################################################
!
!!****  *INIT_ISBA_n* - routine to initialize ISBA
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      Modified by P. Le Moigne (11/2004): miscellaneous diagnostics
!!      Modified by P. Le Moigne (06/2006): seeding and irrigation    
!!      Modified by B. Decharme    (2008) : SGH and Flooding scheme
!!      Modified by B. Decharme  (01/2009): optional deep soil temperature as in Arpege
!!      Modified by R. Hamdi     (01/2009): Cp and L
!!      Modified by B. Decharme  (06/2009): read topographic index statistics
!!      Modified by P. Le Moigne (01/2009): Beljaars sso
!!      Modified by B. Decharme  (08/2009): Active Trip coupling variable if Earth System Model
!!      A.L. Gibelin   04/09 : change BSLAI_NITRO initialisation
!!      A.L. Gibelin   04/09 : modifications for CENTURY model 
!!      A.L. Gibelin   06/09 : soil carbon initialisation
!!      B. Decharme    07/11 : read pgd+prep
!!      R. Alkama      05/12 : new carbon spinup
!!      J.Escobar      11/13 : add USE MODI_DEFAULT_CROCUS
!!      B. Decharme  04/2013 new coupling variables
!!      P. Samuelsson  10/14 : MEB
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURFEX_n, ONLY : ISBA_MODEL_t
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
USE MODD_DATA_TSZ0_n, ONLY : DATA_TSZ0_t
USE MODD_DST_n, ONLY : DST_NP_t
USE MODD_SLT_n, ONLY : SLT_t
USE MODD_SV_n, ONLY : SV_t
!
USE MODD_TYPE_DATE_SURF, ONLY : DATE
!
USE MODD_DATA_COVER,     ONLY : XDATA_LAI, XDATA_H_TREE,                          &
                                XDATA_ALBNIR_VEG, XDATA_ALBVIS_VEG,               &
                                XDATA_ALBUV_VEG, XDATA_RSMIN,                     &
                                XDATA_ROOT_EXTINCTION,XDATA_ROOT_LIN,             &
                                XDATA_RGL, XDATA_CV, XDATA_GAMMA, XDATA_GMES,     &
                                XDATA_GC, XDATA_BSLAI, XDATA_SEFOLD, XDATA_LAIMIN,&
                                XDATA_DMAX, XDATA_STRESS, XDATA_F2I,              &
                                XDATA_VEG, XDATA_GREEN, XDATA_Z0, XDATA_Z0_O_Z0H, &
                                XDATA_EMIS_ECO, XDATA_WRMAX_CF,                   &
                                XDATA_CE_NITRO,XDATA_CF_NITRO,XDATA_CNA_NITRO,    &
                                XDATA_SOILRC_SO2, XDATA_SOILRC_O3, XDATA_RE25,    &
                                XDATA_GMES_ST, XDATA_BSLAI_ST, XDATA_SEFOLD_ST,   &
                                XDATA_GC_ST, XDATA_DMAX_ST
!
USE MODD_WRITE_SURF_ATM,  ONLY : LSPLIT_PATCH
USE MODD_SURF_ATM,       ONLY : LCPL_GCM
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
USE MODD_AGRI,           ONLY : LAGRIP
!
USE MODE_TARTES, ONLY : INIT_TARTES
USE MODE_SNOWCRO_FLANNER, ONLY : READ_FZ06
!
USE MODD_READ_NAMELIST,  ONLY : LNAM_READ
!
USE MODD_CO2V_PAR,  ONLY : XMCO2, XSPIN_CO2
USE MODD_CSTS,      ONLY : XMD
!
USE MODI_INIT_IO_SURF_n
!
USE MODI_GET_LUOUT
USE MODI_ABOR1_SFX
USE MODI_DEFAULT_ISBA
USE MODI_DEFAULT_CH_DEP
USE MODI_DEFAULT_CH_BIO_FLUX
USE MODI_DEFAULT_DIAG_ISBA
USE MODI_DEFAULT_CROCUS
USE MODI_READ_DEFAULT_ISBA_n
USE MODI_READ_ISBA_CONF_n
USE MODI_READ_PREP_ISBA_SNOW
USE MODI_READ_PREP_ISBA_CARBON
USE MODI_READ_SURF
USE MODI_PREP_CTRL_ISBA
USE MODI_READ_ISBA_DATE
USE MODI_READ_PGD_ISBA_n
USE MODI_COMPUTE_ISBA_PARAMETERS
USE MODI_READ_NAM_PREP_ISBA_n
USE MODI_INI_DATA_PARAM
!
USE MODI_SET_SURFEX_FILEIN
!
USE MODI_END_IO_SURF_n
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
LOGICAL, INTENT(IN) :: OREAD_BUDGETC
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
TYPE(ISBA_MODEL_t), INTENT(INOUT) :: IM
!
TYPE(DATA_TSZ0_t), INTENT(INOUT) :: DTZ
TYPE(DST_NP_t), INTENT(INOUT) :: NDST
TYPE(SLT_t), INTENT(INOUT) :: SLT
TYPE(SV_t), INTENT(INOUT) :: SV
!
 CHARACTER(LEN=6),                 INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=3),                 INTENT(IN)  :: HINIT     ! choice of fields to initialize
LOGICAL,                          INTENT(IN)  :: OLAND_USE !
INTEGER,                          INTENT(IN)  :: KI        ! number of points
INTEGER,                          INTENT(IN)  :: KSV       ! number of scalars
INTEGER,                          INTENT(IN)  :: KSW       ! number of short-wave spectral bands
 CHARACTER(LEN=6), DIMENSION(KSV), INTENT(IN)  :: HSV       ! name of all scalar variables
REAL,             DIMENSION(KI),  INTENT(IN)  :: PCO2      ! CO2 concentration (kg/m3)
REAL,             DIMENSION(KI),  INTENT(IN)  :: PRHOA     ! air density
REAL,             DIMENSION(KI),  INTENT(IN)  :: PZENITH   ! solar zenithal angle
REAL,             DIMENSION(KI),  INTENT(IN)  :: PAZIM     ! solar azimuthal angle (rad from N, clock)
REAL,             DIMENSION(KSW), INTENT(IN)  :: PSW_BANDS ! middle wavelength of each band
REAL,             DIMENSION(KI,KSW),INTENT(OUT) :: PDIR_ALB  ! direct albedo for each band
REAL,             DIMENSION(KI,KSW),INTENT(OUT) :: PSCA_ALB  ! diffuse albedo for each band
REAL,             DIMENSION(KI),  INTENT(OUT) :: PEMIS     ! emissivity
REAL,             DIMENSION(KI),  INTENT(OUT) :: PTSRAD    ! radiative temperature
REAL,             DIMENSION(KI),  INTENT(OUT) :: PTSURF    ! surface effective temperature         (K)
!
INTEGER,                          INTENT(IN)  :: KYEAR     ! current year (UTC)
INTEGER,                          INTENT(IN)  :: KMONTH    ! current month (UTC)
INTEGER,                          INTENT(IN)  :: KDAY      ! current day (UTC)
REAL,                             INTENT(IN)  :: PTIME     ! current time since
                                                          !  midnight (UTC, s)
TYPE(DATE), INTENT(INOUT) :: TPDATE_END
!
 CHARACTER(LEN=28),                INTENT(IN)  :: HATMFILE    ! atmospheric file name
 CHARACTER(LEN=6),                 INTENT(IN)  :: HATMFILETYPE! atmospheric file type
 CHARACTER(LEN=2),                 INTENT(IN)  :: HTEST       ! must be equal to 'OK'
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(KI) :: ZCO2     ! CO2 concentration  (kg/m3)
REAL                :: ZSPINCO2
INTEGER             :: ISPINEND
!
INTEGER             :: ILUOUT   ! unit of output listing file
INTEGER             :: IVERSION       ! surface version
INTEGER             :: IRESP   ! return code
INTEGER             :: ISIZE_LMEB_PATCH   ! Number of patches where multi-energy balance should be applied
!
 CHARACTER(LEN=3) :: YSNOW_SCHEME
INTEGER :: ISNOW_NLAYER, JP
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!               Initialisation for IO
!
!
IF (LHOOK) CALL DR_HOOK('INIT_ISBA_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('INIT_ISBAN: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF
!
!               Other little things
!
IF (LNAM_READ) THEN
 !
 !*       0.     Defaults
!               --------

 !        0.1. Hard defaults
 !      
 CALL DEFAULT_ISBA(IM%O%XTSTEP, IM%O%XOUT_TSTEP,                                   &
                   IM%O%CRUNOFF, IM%O%CSCOND,                                      &
                   IM%O%CC1DRY, IM%O%CSOILFRZ, IM%O%CDIFSFCOND, IM%O%CSNOWRES,     &
                   IM%O%CCPSURF, IM%O%XCGMAX, IM%O%XCDRAG, IM%O%CKSAT, IM%O%LSOC,  &
                   IM%O%CRAIN, IM%O%CHORT, IM%O%LGLACIER, IM%O%LCANOPY_DRAG,       &
                   IM%O%LVEGUPD, IM%O%LSPINUPCARBS, IM%O%LSPINUPCARBW,             &
                   IM%O%XSPINMAXS, IM%O%XSPINMAXW, IM%O%XCO2_START, IM%O%XCO2_END, &
                   IM%O%NNBYEARSPINS, IM%O%NNBYEARSPINW, IM%O%LNITRO_DILU     )
 !                  
 CALL DEFAULT_CH_DEP(IM%CHI%CCH_DRY_DEP)
 CALL DEFAULT_CH_BIO_FLUX(IM%CHI%LCH_BIO_FLUX)                  
 CALL DEFAULT_DIAG_ISBA(IM%ID%O%N2M, IM%ID%O%LSURF_BUDGET, IM%ID%O%L2M_MIN_ZS, IM%ID%O%LRAD_BUDGET, &
                        IM%ID%O%LCOEF, IM%ID%O%LSURF_VARS, IM%ID%DE%LSURF_EVAP_BUDGET,              &
                        IM%ID%DM%LSURF_MISC_BUDGET, IM%ID%DM%LSURF_DIAG_ALBEDO,                     &
                        IM%ID%O%LSURF_BUDGETC, IM%ID%DM%LSURF_MISC_DIF, IM%ID%O%LPATCH_BUDGET,      &
                        IM%ID%O%LPGD, IM%ID%O%LRESET_BUDGETC, IM%ID%DE%LWATER_BUDGET,               &
                        IM%ID%DM%LPROSNOW,IM%ID%DM%LVOLUMETRIC_SNOWLIQ,IM%ID%O%XDIAG_TSTEP          )  
 !
 CALL DEFAULT_CROCUS(IM%O%LSNOWDRIFT, IM%O%LSNOWDRIFT_SUBLIM, IM%O%LSNOW_ABS_ZENITH, &
                     IM%O%CSNOWMETAMO, IM%O%CSNOWRAD)
 ! 
ENDIF
!
!        0.2. Defaults from file header
!    
 CALL READ_DEFAULT_ISBA_n(IM%CHI, IM%ID%DE, IM%ID%O, IM%ID%DM, IM%O, HPROGRAM)
!
 CALL READ_ISBA_CONF_n(IM%CHI, IM%ID%DE, IM%ID%O, IM%ID%DM, IM%O, HPROGRAM)
!
CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'FULL  ','ISBA  ','READ ')
 CALL READ_SURF(HPROGRAM,'VERSION',IVERSION,IRESP)
 CALL END_IO_SURF_n(HPROGRAM)
!
!*       1.     Reading of configuration:
!               -------------------------
!
!* initialization of snow and carbon schemes
!
IM%O%NNBYEARSOLD = 1
IM%O%NSPINS      = 1
IM%O%NSPINW      = 1
!
IF (HINIT=='PRE') THEN 
  CALL READ_PREP_ISBA_SNOW(HPROGRAM,YSNOW_SCHEME,ISNOW_NLAYER)
!
!* initialization of soil carbon scheme
!
  CALL READ_PREP_ISBA_CARBON(HPROGRAM,IM%O%CRESPSL)
!
  IF (IM%O%CRESPSL=='CNT') THEN
    IM%O%NNLITTER = 2
    IM%O%NNLITTLEVS = 2
    IM%O%NNSOILCARB = 3
  ELSE
    IM%O%NNLITTER = 0
    IM%O%NNLITTLEVS = 0
    IM%O%NNSOILCARB = 0
  ENDIF

ELSEIF (HINIT=='ALL') THEN
  !
  CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'NATURE','ISBA  ','READ ')
  !
  IF (IVERSION<6) THEN
    IM%O%CRESPSL='DEF'
  ELSE  
    CALL READ_SURF(HPROGRAM,'RESPSL',IM%O%CRESPSL,IRESP)
    CALL READ_SURF(HPROGRAM,'NLITTER',IM%O%NNLITTER,IRESP)
    CALL READ_SURF(HPROGRAM,'NLITTLEVS',IM%O%NNLITTLEVS,IRESP)
    CALL READ_SURF(HPROGRAM,'NSOILCARB',IM%O%NNSOILCARB,IRESP)
    IF(IVERSION>=7.AND.(IM%O%LSPINUPCARBS.OR.IM%O%LSPINUPCARBW))THEN
      CALL READ_SURF(HPROGRAM,'NBYEARSOLD',IM%O%NNBYEARSOLD,IRESP)
    ELSE
      IM%O%NNBYEARSOLD=NUNDEF
    ENDIF
  ENDIF
  !
  CALL END_IO_SURF_n(HPROGRAM)
  !
ENDIF
!
IF (IM%O%CISBA/="DIF") THEN
  IM%O%NLAYER_DUN  = 2
  IM%O%NLAYER_HORT = 2        
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       2.     Physiographic fields
!               --------------------
!
!
!* date
!
SELECT CASE (HINIT)
  CASE ('PGD')
    IM%S%TTIME%TDATE%YEAR = NUNDEF
    IM%S%TTIME%TDATE%MONTH= NUNDEF
    IM%S%TTIME%TDATE%DAY  = NUNDEF
    IM%S%TTIME%TIME       = XUNDEF

  CASE ('PRE')
    CALL PREP_CTRL_ISBA(IM%ID%O, IM%ID%DE%LSURF_EVAP_BUDGET, IM%ID%DM%LSURF_MISC_BUDGET, &
                        IM%ID%DM%LSURF_MISC_DIF, ILUOUT)    
    IF (LNAM_READ) CALL READ_NAM_PREP_ISBA_n(HPROGRAM)                        
    CALL READ_ISBA_DATE(HPROGRAM, HINIT, ILUOUT, HATMFILE, HATMFILETYPE, KYEAR, KMONTH, KDAY, PTIME, IM%S%TTIME)
    TPDATE_END = IM%S%TTIME%TDATE

  CASE DEFAULT
    CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'FULL  ','SURF  ','READ ')
    CALL READ_SURF(HPROGRAM,'DTCUR',IM%S%TTIME,IRESP)
    CALL END_IO_SURF_n(HPROGRAM)
END SELECT
!
!-----------------------------------------------------------------------------------------------------
! READ PGD FILE
!-----------------------------------------------------------------------------------------------------
!
! initialization for I/O
!
 CALL SET_SURFEX_FILEIN(HPROGRAM,'PGD ') ! change input file name to pgd name
CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'NATURE','ISBA  ','READ ')
!
!
!*       2.1    Cover, soil and orographic fields:
!               ---------------------------------
!
 CALL READ_PGD_ISBA_n(IM%CHI, DTCO, IM%DTV, DTZ, IM%GB, IM%G, IM%ISS, IM%O, IM%S, IM%K, &
                      UG, U, USS, GCP, SV, HPROGRAM, OLAND_USE, TPDATE_END)
!
IF (HINIT=='PRE') THEN 
  DO JP = 1,IM%O%NPATCH
    IM%NPE%AL(JP)%TSNOW%SCHEME = YSNOW_SCHEME
    IM%NPE%AL(JP)%TSNOW%NLAYER = ISNOW_NLAYER
  ENDDO
ENDIF
!
ISIZE_LMEB_PATCH=COUNT(IM%O%LMEB_PATCH(:))
!
!
!*       2.2    Check:
!               ------
!
IF ( IM%O%CPHOTO/='NON' .AND. IM%O%NPATCH/=12 .AND. IM%O%NPATCH/=19 )THEN
  CALL ABOR1_SFX('INIT_ISBAN: INCONSISTENCY BETWEEN CPHOTO AND NPATCH')
ENDIF
!
IF (HINIT=='PRE' .AND. YSNOW_SCHEME.NE.'3-L' .AND. YSNOW_SCHEME.NE.'CRO' .AND. IM%O%CISBA=='DIF') THEN
    CALL ABOR1_SFX("INIT_ISBAN: WITH CISBA = DIF, CSNOW MUST BE 3-L OR CRO")
ENDIF
IF ( IM%O%CPHOTO/='NIT' .AND. IM%O%CPHOTO/='NCB' .AND. LAGRIP) THEN
  CALL ABOR1_SFX('INIT_ISBAN: INCONSISTENCY BETWEEN CPHOTO AND LAGRIP')
ENDIF
IF ( IM%O%CPHOTO/='NCB' .AND. IM%O%CRESPSL=='CNT') THEN
  CALL ABOR1_SFX('INIT_ISBAN: INCONSISTENCY BETWEEN CPHOTO AND CRESPSL')
ENDIF
IF (HINIT=='PRE' .AND. ISIZE_LMEB_PATCH>0 .AND. YSNOW_SCHEME.NE.'3-L' .AND. YSNOW_SCHEME.NE.'CRO') THEN
    CALL ABOR1_SFX("INIT_ISBAN: WITH LMEB_PATCH = TRUE, CSNOW MUST BE 3-L OR CRO")
ENDIF
IF(IM%O%CPHOTO/='NCB'.AND.IM%O%LSPINUPCARBW)THEN
  CALL ABOR1_SFX('INIT_ISBAN: INCONSISTENCY BETWEEN CPHOTO AND LSPINUPCARBW (if not NCB must be false)')
ENDIF
IF(IM%O%CRESPSL/='CNT'.AND.IM%O%LSPINUPCARBS)THEN
  CALL ABOR1_SFX('INIT_ISBAN: INCONSISTENCY BETWEEN CRESPSL AND LSPINUPCARBS (if not CNT must be false)')
ENDIF
IF(IM%O%LSPINUPCARBW.AND.REAL(IM%O%NNBYEARSPINW)>REAL(IM%O%NNBYEARSPINS)*0.5)THEN
  WRITE(ILUOUT,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
  WRITE(ILUOUT,*)'INIT_ISBAN: INCONSISTENCY BETWEEN NNBYEARSPINW AND NNBYEARSPINS'
  WRITE(ILUOUT,*)'NNBYEARSPINW MUST BE < TO 0.5 * NNBYEARSPINS'
  WRITE(ILUOUT,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
  CALL ABOR1_SFX('INIT_ISBAN: INCONSISTENCY BETWEEN NNBYEARSPINW AND NNBYEARSPINS')
ENDIF
IF(IM%O%LSPINUPCARBS.AND.(IM%O%XCO2_START==XUNDEF.OR.IM%O%XCO2_END==XUNDEF))THEN
  WRITE(ILUOUT,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
  WRITE(ILUOUT,*)'INIT_ISBAN: INCONSISTENCY BETWEEN LSPINUPCARBS AND XCO2_START OR XCO2_END'
  WRITE(ILUOUT,*)'FOR ISBA-CC SPINUP XCO2_START AND XCO2_END MUST BE DEFINED'
  WRITE(ILUOUT,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
  CALL ABOR1_SFX('INIT_ISBAN: INCONSISTENCY BETWEEN LSPINUPCARBS AND XCO2_START OR XCO2_END')
ENDIF
!
 CALL END_IO_SURF_n(HPROGRAM)
 CALL SET_SURFEX_FILEIN(HPROGRAM,'PREP') ! restore input file name
!
!-------------------------------------------------------------------------------
!
! During soil carbon spinup with ISBA-CC: 
!        (1) grass parameters are attributed to all agricultural PFT with atmospheric CO2 concentration 
!            fixed to Pre-industrial CO2 consentration XCO2_START
!        (2) Atmospheric CO2 concentration rampin up from XCO2_START to XCO2_END
!
ISPINEND=IM%O%NNBYEARSPINS-NINT(IM%O%NNBYEARSPINS*XSPIN_CO2)
!
IM%O%LAGRI_TO_GRASS = .FALSE.
!
IF ( IM%O%LSPINUPCARBS .AND. (IM%O%NNBYEARSOLD <= ISPINEND) ) THEN
!
   IM%O%LAGRI_TO_GRASS = .TRUE.
!
   CALL INI_DATA_PARAM(PH_TREE=XDATA_H_TREE,PLAI=XDATA_LAI,                                           &
                       PALBNIR_VEG=XDATA_ALBNIR_VEG, PALBVIS_VEG=XDATA_ALBVIS_VEG,                    &
                       PALBUV_VEG=XDATA_ALBUV_VEG, PRSMIN=XDATA_RSMIN,                                &
                       PRGL=XDATA_RGL, PCV=XDATA_CV, PGAMMA=XDATA_GAMMA,                              &
                       PGMES=XDATA_GMES, PGC=XDATA_GC, PBSLAI=XDATA_BSLAI,                            &
                       PSEFOLD=XDATA_SEFOLD, PLAIMIN_OUT=XDATA_LAIMIN, PDMAX=XDATA_DMAX,              &
                       PSTRESS=XDATA_STRESS, PF2I=XDATA_F2I, PVEG_OUT=XDATA_VEG,                      &
                       PGREEN=XDATA_GREEN, PZ0=XDATA_Z0, PZ0_O_Z0H=XDATA_Z0_O_Z0H,                    &
                       PEMIS_ECO=XDATA_EMIS_ECO, PWRMAX_CF=XDATA_WRMAX_CF,                            &
                       PROOT_LIN=XDATA_ROOT_LIN, PROOT_EXTINCTION=XDATA_ROOT_EXTINCTION,              &
                       PSOILRC_SO2=XDATA_SOILRC_SO2, PSOILRC_O3=XDATA_SOILRC_O3, PRE25=XDATA_RE25,    &
                       PCE_NITRO=XDATA_CE_NITRO,PCF_NITRO=XDATA_CF_NITRO,PCNA_NITRO=XDATA_CNA_NITRO,  &
                       PGMES_ST=XDATA_GMES_ST, PGC_ST=XDATA_GC_ST, PBSLAI_ST=XDATA_BSLAI_ST,          &
                       PSEFOLD_ST=XDATA_SEFOLD_ST, PDMAX_ST=XDATA_DMAX_ST, OAGRI_TO_GRASS=IM%O%LAGRI_TO_GRASS)
!
   ZCO2(:) = PRHOA(:) * IM%O%XCO2_START * 1.E-6 * XMCO2 / XMD
!
ELSEIF(IM%O%LSPINUPCARBS .AND. (IM%O%NNBYEARSOLD > ISPINEND) .AND. (IM%O%NNBYEARSOLD <= IM%O%NNBYEARSPINS) )THEN
!
   ZSPINCO2 = IM%O%XCO2_START + (IM%O%XCO2_END-IM%O%XCO2_START) * REAL(IM%O%NNBYEARSOLD - ISPINEND) / &
                REAL(IM%O%NNBYEARSPINS - ISPINEND)
!
   ZCO2(:) = PRHOA(:) * ZSPINCO2 * 1.E-6 * XMCO2 / XMD
!
ELSE
!
   ZCO2(:) = PCO2(:)
!
ENDIF
!
!-----------------------------------------------------------------------------------------------------
! END READ PGD FILE
!-----------------------------------------------------------------------------------------------------
!
!-----------------------------------------------------------------------------------------------------
! Make sure some diags are conputed when coupled with atmosphere
!-----------------------------------------------------------------------------------------------------
!
IF(LCPL_GCM.AND.IM%ID%O%LSURF_BUDGET) THEN
  IM%ID%DE%LSURF_EVAP_BUDGET=.TRUE.
ENDIF
!
!-----------------------------------------------------------------------------------------------------
!
IF (OLAND_USE .OR. HINIT=='PGD') THEN
  IF (LHOOK) CALL DR_HOOK('INIT_ISBA_N',1,ZHOOK_HANDLE)
  RETURN
END IF
!
CALL COMPUTE_ISBA_PARAMETERS(DTCO, OREAD_BUDGETC, UG, U,                    &
                             IM%O, IM%DTV, IM%SB, IM%S, IM%G, IM%K, IM%NK,  &
                             IM%NG, IM%NP, IM%NPE, IM%NAG, IM%NISS, IM%ISS, &
                             IM%NCHI, IM%CHI, IM%ID, IM%GB, IM%NGB,         &
                             NDST, SLT, SV, HPROGRAM,HINIT,OLAND_USE,       &
                             KI,KSV,KSW, HSV,ZCO2,PRHOA,                &
                             PZENITH,PSW_BANDS,PDIR_ALB,PSCA_ALB,       &
                             PEMIS,PTSRAD,PTSURF,HTEST                  )
!
IF ( IM%O%CSNOWMETAMO/="B92" ) THEN
  CALL READ_FZ06('drdt_bst_fit_60.nc')
ENDIF
!
IF ( IM%O%CSNOWRAD=="TAR" .OR. IM%O%CSNOWRAD=="TA1" .OR.  IM%O%CSNOWRAD=="TA2" ) THEN
  CALL INIT_TARTES()
END IF
!
IF (HINIT=='ALL') THEN 
  YSNOW_SCHEME = IM%NPE%AL(1)%TSNOW%SCHEME
  ISNOW_NLAYER = IM%NPE%AL(1)%TSNOW%NLAYER
ENDIF
!
IF (.NOT.LSPLIT_PATCH) THEN

  ALLOCATE(IM%S%XWORK_WR(KI,IM%O%NPATCH))
  IM%S%XWORK_WR(:,:) = XUNDEF

  ALLOCATE(IM%S%XWSN_WR(KI,ISNOW_NLAYER,IM%O%NPATCH))
  ALLOCATE(IM%S%XRHO_WR(KI,ISNOW_NLAYER,IM%O%NPATCH))
  ALLOCATE(IM%S%XALB_WR(KI,IM%O%NPATCH))
  IF (YSNOW_SCHEME=='3-L' .OR. YSNOW_SCHEME=='CRO') THEN
    ALLOCATE(IM%S%XHEA_WR(KI,ISNOW_NLAYER,IM%O%NPATCH))
    ALLOCATE(IM%S%XAGE_WR(KI,ISNOW_NLAYER,IM%O%NPATCH))
    IF (YSNOW_SCHEME=='CRO') THEN
      ALLOCATE(IM%S%XSG1_WR(KI,ISNOW_NLAYER,IM%O%NPATCH))
      ALLOCATE(IM%S%XSG2_WR(KI,ISNOW_NLAYER,IM%O%NPATCH))
      ALLOCATE(IM%S%XHIS_WR(KI,ISNOW_NLAYER,IM%O%NPATCH))
    ELSE
      ALLOCATE(IM%S%XSG1_WR(0,0,1))
      ALLOCATE(IM%S%XSG2_WR(0,0,1)) 
      ALLOCATE(IM%S%XHIS_WR(0,0,1))   
    ENDIF
  ELSE
    ALLOCATE(IM%S%XHEA_WR(0,0,1))
    ALLOCATE(IM%S%XAGE_WR(0,0,1))
  ENDIF

  ALLOCATE(IM%S%TDATE_WR(KI,IM%O%NPATCH))

ELSE

  ALLOCATE(IM%S%XWORK_WR(0,1))

  ALLOCATE(IM%S%XWSN_WR(0,0,1))
  ALLOCATE(IM%S%XRHO_WR(0,0,1))
  ALLOCATE(IM%S%XALB_WR(0,1))
  ALLOCATE(IM%S%XHEA_WR(0,0,1))
  ALLOCATE(IM%S%XAGE_WR(0,0,1))
  ALLOCATE(IM%S%XSG1_WR(0,0,1))
  ALLOCATE(IM%S%XSG2_WR(0,0,1)) 
  ALLOCATE(IM%S%XHIS_WR(0,0,1))

  ALLOCATE(IM%S%TDATE_WR(0,1))

ENDIF
!
IF (LHOOK) CALL DR_HOOK('INIT_ISBA_N',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_ISBA_n
