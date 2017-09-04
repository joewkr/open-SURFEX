!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #############################################################
      SUBROUTINE INIT_TEB_n (DTCO, UG, U, GCP, CHT, DTT, SB, TG, TOP, TPN,    &
                             TIR, NT, TD, BDD, BOP, DTB, NB, GDM, GRM,        &
                             HPROGRAM, HINIT, KI, KSV, KSW, HSV, PCO2,        &
                             PRHOA, PZENITH, PAZIM, PSW_BANDS, PDIR_ALB,      &
                             PSCA_ALB, PEMIS, PTSRAD, PTSURF, KYEAR, KMONTH,  &
                             KDAY, PTIME, HATMFILE, HATMFILETYPE, HTEST )
!     #############################################################
!
!!****  *INIT_TEB_n* - routine to initialize TEB
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
!!      Original    01/2003
!!      G. Pigeon   09/2012: add ROUGH_WALL/ROUGH_ROOF/CH_BEM for conv. coef.
!!      B. Decharme  04/2013 new coupling variables
!!                           delete CTOPREG option (never used)
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
!
USE MODD_CH_TEB_n, ONLY : CH_TEB_t
USE MODD_DATA_TEB_n, ONLY : DATA_TEB_t
USE MODD_CANOPY_n, ONLY: CANOPY_t
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
USE MODD_TEB_PANEL_n, ONLY : TEB_PANEL_t
USE MODD_TEB_IRRIG_n, ONLY : TEB_IRRIG_t
USE MODD_TEB_n, ONLY : TEB_NP_t
USE MODD_SURFEX_n, ONLY : TEB_DIAG_t, TEB_VEG_DIAG_t, TEB_GARDEN_MODEL_t, TEB_GREENROOF_MODEL_t
USE MODD_BLD_DESCRIPTION_n, ONLY : BLD_DESC_t
USE MODD_BEM_OPTION_n, ONLY : BEM_OPTIONS_t
USE MODD_DATA_BEM_n, ONLY : DATA_BEM_t
USE MODD_BEM_n, ONLY : BEM_NP_t
!
USE MODD_IO_SURF_ASC,ONLY: CMASK
USE MODD_SNOW_PAR, ONLY : XEMISSN
!
USE MODD_READ_NAMELIST, ONLY : LNAM_READ
!
USE MODD_CHS_AEROSOL,     ONLY: LVARSIGI, LVARSIGJ
USE MODD_DST_SURF,        ONLY: LVARSIG_DST, NDSTMDE, NDST_MDEBEG, LRGFIX_DST
USE MODD_SLT_SURF,        ONLY: LVARSIG_SLT, NSLTMDE, NSLT_MDEBEG, LRGFIX_SLT
USE MODD_SURF_PAR,        ONLY: XUNDEF, NUNDEF
!
USE MODN_PREP_TEB, ONLY : CROAD_DIR, CWALL_OPT
!
USE MODI_INIT_IO_SURF_n
USE MODI_DEFAULT_CH_DEP
USE MODI_DEFAULT_TEB
USE MODI_DEFAULT_DIAG_TEB
USE MODI_READ_DEFAULT_TEB_n
USE MODI_READ_TEB_CONF_n
USE MODI_PREP_CTRL_TEB
USE MODI_READ_TEB_n
USE MODI_READ_PGD_TEB_n
USE MODI_CONVERT_TEB
USE MODI_CONVERT_PATCH_TEB
USE MODI_INIT_SNOW_LW
USE MODI_AVERAGED_TSRAD_TEB
USE MODI_AVERAGED_ALBEDO_TEB
USE MODI_DIAG_TEB_INIT_n
USE MODI_DIAG_MISC_TEB_INIT_n
USE MODI_END_IO_SURF_n
USE MODI_GET_LUOUT
USE MODI_READ_SURF
USE MODI_READ_PREP_TEB_SNOW
USE MODI_READ_TEB_DATE
USE MODI_READ_NAM_PREP_TEB_n
USE MODI_INIT_CHEMICAL_n
USE MODI_TEB_VEG_PROPERTIES
USE MODI_HVAC_AUTOSIZE
!
USE MODI_INIT_TEB_GARDEN_n
USE MODI_INIT_TEB_GARDEN_PGD_n
USE MODI_INIT_TEB_VEG_OPTIONS_n
USE MODI_TEB_MORPHO
USE MODI_INIT_BEM_n
USE MODI_INIT_TEB_GREENROOF_n
USE MODI_INIT_TEB_GREENROOF_PGD_n
USE MODI_READ_PGD_TEB_IRRIG_n
!
USE MODI_READ_COVER_GARDEN
USE MODI_ABOR1_SFX
USE MODI_READ_SBL_n
USE MODI_SET_SURFEX_FILEIN
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
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
TYPE(CH_TEB_t), INTENT(INOUT) :: CHT
TYPE(DATA_TEB_t), INTENT(INOUT) :: DTT
TYPE(CANOPY_t), INTENT(INOUT) :: SB
TYPE(GRID_t), INTENT(INOUT) :: TG
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
TYPE(TEB_PANEL_t), INTENT(INOUT) :: TPN
TYPE(TEB_IRRIG_t), INTENT(INOUT) :: TIR
TYPE(TEB_NP_t), INTENT(INOUT) :: NT
!
TYPE(TEB_DIAG_t), INTENT(INOUT) :: TD
!
TYPE(BLD_DESC_t), INTENT(INOUT) :: BDD
TYPE(BEM_OPTIONS_t), INTENT(INOUT) :: BOP
TYPE(DATA_BEM_t), INTENT(INOUT) :: DTB
TYPE(BEM_NP_t), INTENT(INOUT) :: NB
!
TYPE(TEB_GARDEN_MODEL_t), INTENT(INOUT) :: GDM
TYPE(TEB_GREENROOF_MODEL_t), INTENT(INOUT) :: GRM
!
 CHARACTER(LEN=6),                   INTENT(IN)  :: HPROGRAM    ! program calling surf. schemes
 CHARACTER(LEN=3),                   INTENT(IN)  :: HINIT       ! choice of fields to initialize
INTEGER,                            INTENT(IN)  :: KI          ! number of points
INTEGER,                            INTENT(IN)  :: KSV         ! number of scalars
INTEGER,                            INTENT(IN)  :: KSW         ! number of short-wave spectral bands
 CHARACTER(LEN=6), DIMENSION(KSV),   INTENT(IN)  :: HSV         ! name of all scalar variables
REAL,             DIMENSION(KI),    INTENT(IN)  :: PCO2        ! CO2 concentration (kg/m3)
REAL,             DIMENSION(KI),    INTENT(IN)  :: PRHOA       ! air density
REAL,             DIMENSION(KI),    INTENT(IN)  :: PZENITH     ! solar zenithal angle
REAL,             DIMENSION(KI),    INTENT(IN)  :: PAZIM       ! solar azimuthal angle (rad from N, clock)
REAL,             DIMENSION(KSW),   INTENT(IN)  :: PSW_BANDS   ! middle wavelength of each band
REAL,             DIMENSION(KI,KSW),INTENT(OUT) :: PDIR_ALB    ! direct albedo for each band
REAL,             DIMENSION(KI,KSW),INTENT(OUT) :: PSCA_ALB    ! diffuse albedo for each band
REAL,             DIMENSION(KI),    INTENT(OUT) :: PEMIS       ! emissivity
REAL,             DIMENSION(KI),    INTENT(OUT) :: PTSRAD      ! radiative temperature
REAL,             DIMENSION(KI),    INTENT(OUT) :: PTSURF      ! surface effective temperature         (K)
INTEGER,                            INTENT(IN)  :: KYEAR       ! current year (UTC)
INTEGER,                            INTENT(IN)  :: KMONTH      ! current month (UTC)
INTEGER,                            INTENT(IN)  :: KDAY        ! current day (UTC)
REAL,                               INTENT(IN)  :: PTIME       ! current time since
                                                               !  midnight (UTC, s)
!
 CHARACTER(LEN=28),                  INTENT(IN)  :: HATMFILE    ! atmospheric file name
 CHARACTER(LEN=6),                   INTENT(IN)  :: HATMFILETYPE! atmospheric file type
 CHARACTER(LEN=2),                   INTENT(IN)  :: HTEST       ! must be equal to 'OK'
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER                         :: ILU              ! sizes of TEB arrays
INTEGER                         :: ILUOUT           ! unit of output listing file
INTEGER                         :: IRESP            ! return code
!
INTEGER                         :: ISWB             ! number of shortwave spectral bands
INTEGER                         :: JSWB             ! loop on shortwave spectral bands
!
REAL                            :: ZDEF_ROAD_DIR    ! default raod direction
REAL, DIMENSION(:), ALLOCATABLE :: ZDIR_ALB         ! direct town albedo
REAL, DIMENSION(:), ALLOCATABLE :: ZSCA_ALB         ! diffuse town albedo
!
!              local variables for urban green areas
REAL, DIMENSION(KI,KSW)         :: ZDIR_SW          ! direct  SW for each band
REAL, DIMENSION(KI,KSW)         :: ZSCA_SW          ! diffuse SW for each band
REAL, DIMENSION(KI)             :: ZEMIS_GARDEN     ! emissivity
REAL, DIMENSION(KI)             :: ZALB_GARDEN      ! albedo
REAL, DIMENSION(KI)             :: ZTS_GARDEN       ! radiative temperature
!
!              local variables for urban greenroofs
REAL, DIMENSION(KI)             :: ZEMIS_GREENROOF     ! emissivity
REAL, DIMENSION(KI)             :: ZALB_GREENROOF      ! albedo
REAL, DIMENSION(KI)             :: ZTS_GREENROOF       ! radiative temperature
!
INTEGER                         :: JP
INTEGER                         :: IVERSION, IBUGFIX

REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!         Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('INIT_TEB_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('INIT_TEBN: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF
!
!         Other little things
!
PDIR_ALB = XUNDEF
PSCA_ALB = XUNDEF
PEMIS    = XUNDEF
PTSRAD   = XUNDEF
PTSURF   = XUNDEF
!
TD%MTO%LSURF_EVAP_BUDGET = .FALSE.
!
IF (LNAM_READ) THEN
 !
 !*       0.     Defaults
 !               --------
 !
 !        0.1. Hard defaults
 !
  DO JP=1,TOP%NTEB_PATCH
    CALL DEFAULT_TEB(TOP%CZ0H, TOP%XTSTEP, TOP%XOUT_TSTEP, TOP%CCH_BEM, &
                     NT%AL(1)%XDT_RES, NT%AL(1)%XDT_OFF)
  ENDDO
 CALL DEFAULT_CH_DEP(CHT%CCH_DRY_DEP)
 CALL DEFAULT_DIAG_TEB(TD%O%N2M, TD%O%LSURF_BUDGET, TD%O%L2M_MIN_ZS, TD%O%LRAD_BUDGET,&
                       TD%O%LCOEF, TD%O%LSURF_VARS, TD%MTO%LSURF_MISC_BUDGET, &
                       TD%MTO%LSURF_DIAG_ALBEDO, TD%DUT%LUTCI, TD%O%LPGD,     &
                       TD%O%XDIAG_TSTEP)
!
ENDIF
!
!        0.2. Defaults from file header
!
 CALL READ_DEFAULT_TEB_n(CHT, TD%MTO, TD%O, TD%DUT, GRM%O, NT%AL(1), TOP, HPROGRAM)
!
!*       1.     Reading of configuration:
!               -------------------------
!
 CALL READ_TEB_CONF_n(CHT, TD%MTO, TD%O, TD%DUT, NT%AL(1), TOP, HPROGRAM)
!
!* initialization of snow scheme
!
IF (HINIT=='PRE') THEN
  DO JP=1,TOP%NTEB_PATCH
    CALL READ_PREP_TEB_SNOW(HPROGRAM, NT%AL(1)%TSNOW_ROOF%SCHEME, NT%AL(1)%TSNOW_ROOF%NLAYER, &
                                      NT%AL(1)%TSNOW_ROAD%SCHEME, NT%AL(1)%TSNOW_ROAD%NLAYER)
  END DO
ENDIF
!
!*       2.     Cover fields and grid:
!               ---------------------
!* date
!
SELECT CASE (HINIT)
  CASE ('PGD')
    TOP%TTIME%TDATE%YEAR = NUNDEF
    TOP%TTIME%TDATE%MONTH= NUNDEF
    TOP%TTIME%TDATE%DAY  = NUNDEF
    TOP%TTIME%TIME       = XUNDEF

  CASE ('PRE')
    CALL PREP_CTRL_TEB(TD%O, TD%MTO%LSURF_EVAP_BUDGET, TD%MTO%LSURF_MISC_BUDGET, TD%DUT%LUTCI,ILUOUT )
    IF (LNAM_READ) CALL READ_NAM_PREP_TEB_n(HPROGRAM)
    CALL READ_TEB_DATE(HPROGRAM,HINIT,ILUOUT,HATMFILE,HATMFILETYPE,KYEAR,KMONTH,KDAY,PTIME,TOP%TTIME)

  CASE DEFAULT
    CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'FULL  ','SURF  ','READ ')
    CALL READ_SURF(HPROGRAM,'DTCUR',TOP%TTIME,IRESP)
    CALL END_IO_SURF_n(HPROGRAM)
END SELECT
!
!-----------------------------------------------------------------------------------------------------
! READ PGD FILE
!-----------------------------------------------------------------------------------------------------
!
!         Initialisation for IO
!
 CALL SET_SURFEX_FILEIN(HPROGRAM,'PGD ') ! change input file name to pgd name
CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'TOWN  ','TEB   ','READ ')
!
 CALL READ_SURF(HPROGRAM,'VERSION',IVERSION,IRESP)
 CALL READ_SURF(HPROGRAM,'BUG',IBUGFIX,IRESP)
!
!         Reading of the fields
!
 CALL READ_COVER_GARDEN(HPROGRAM,TOP%LGARDEN)
!
 CALL READ_PGD_TEB_n(DTCO, U, UG, GCP, TOP, TG, BOP, BDD, DTB, DTT, HPROGRAM)
!
 CALL END_IO_SURF_n(HPROGRAM)
!
!*        Fraction of each patch in the grid mesh
!
ILU = SIZE(TOP%XCOVER,1)
!
ALLOCATE(TOP%XTEB_PATCH(ILU,TOP%NTEB_PATCH))
 CALL CONVERT_TEB(TOP%NTEB_PATCH, TOP%XCOVER,TOP%XTEB_PATCH)
!
!
!* reads what is the option defined for road orientations & walls
!
IF (HINIT=='ALL') THEN
  !
  CALL SET_SURFEX_FILEIN(HPROGRAM,'PREP') ! restore input file name
  CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'TOWN  ','TEB   ','READ ')
!
  CALL READ_SURF(HPROGRAM,'VERSION',IVERSION,IRESP)
  CALL READ_SURF(HPROGRAM,'BUG',IBUGFIX,IRESP)
!
  TOP%CROAD_DIR='UNIF'
  TOP%CWALL_OPT='UNIF'
  IF (IVERSION>7 .OR. (IVERSION==7 .AND. IBUGFIX>=3)) THEN
    CALL READ_SURF(HPROGRAM,'ROAD_DIR',TOP%CROAD_DIR,IRESP)
    CALL READ_SURF(HPROGRAM,'WALL_OPT',TOP%CWALL_OPT,IRESP)
  END IF
  CALL END_IO_SURF_n(HPROGRAM)
  !
ELSE
  !
  TOP%CROAD_DIR = CROAD_DIR
  TOP%CWALL_OPT = CWALL_OPT
  !
ENDIF
!
!-----------------------------------------------------------------------------------
!
!*              LOOP ON TEB PATCHES
!               -------------------
!
DO JP=1,TOP%NTEB_PATCH
  !
  !-----------------------------------------------------------------------------------
  !
  !*       3.     Physiographic data fields from land cover:
  !               -----------------------------------------
  !
  ALLOCATE(NT%AL(JP)%XZ0_TOWN     (ILU))
  ALLOCATE(NT%AL(JP)%XALB_ROOF    (ILU))
  ALLOCATE(NT%AL(JP)%XEMIS_ROOF   (ILU))
  ALLOCATE(NT%AL(JP)%XALB_ROAD    (ILU))
  ALLOCATE(NT%AL(JP)%XEMIS_ROAD   (ILU))
  ALLOCATE(NT%AL(JP)%XALB_WALL    (ILU))
  ALLOCATE(NT%AL(JP)%XEMIS_WALL   (ILU))
  ALLOCATE(NT%AL(JP)%XBLD         (ILU))
  ALLOCATE(NT%AL(JP)%XROAD_DIR    (ILU))
  ALLOCATE(NT%AL(JP)%XROAD        (ILU))
  ALLOCATE(NT%AL(JP)%XBLD_HEIGHT  (ILU))
  ALLOCATE(NT%AL(JP)%XWALL_O_HOR  (ILU))
  ALLOCATE(NT%AL(JP)%XCAN_HW_RATIO(ILU))
  ALLOCATE(NT%AL(JP)%XROAD_O_GRND (ILU))
  ALLOCATE(NT%AL(JP)%XGARDEN_O_GRND(ILU))
  ALLOCATE(NT%AL(JP)%XWALL_O_GRND (ILU))
  ALLOCATE(NT%AL(JP)%XWALL_O_BLD(  ILU))
  ALLOCATE(NT%AL(JP)%XH_TRAFFIC   (ILU))
  ALLOCATE(NT%AL(JP)%XLE_TRAFFIC  (ILU))
  ALLOCATE(NT%AL(JP)%XH_INDUSTRY  (ILU))
  ALLOCATE(NT%AL(JP)%XLE_INDUSTRY (ILU))
  ALLOCATE(NT%AL(JP)%XHC_ROOF     (ILU,TOP%NROOF_LAYER))
  ALLOCATE(NT%AL(JP)%XTC_ROOF     (ILU,TOP%NROOF_LAYER))
  ALLOCATE(NT%AL(JP)%XD_ROOF      (ILU,TOP%NROOF_LAYER))
  ALLOCATE(NT%AL(JP)%XHC_ROAD     (ILU,TOP%NROAD_LAYER))
  ALLOCATE(NT%AL(JP)%XTC_ROAD     (ILU,TOP%NROAD_LAYER))
  ALLOCATE(NT%AL(JP)%XD_ROAD      (ILU,TOP%NROAD_LAYER))
  ALLOCATE(NT%AL(JP)%XHC_WALL     (ILU,TOP%NWALL_LAYER))
  ALLOCATE(NT%AL(JP)%XTC_WALL     (ILU,TOP%NWALL_LAYER))
  ALLOCATE(NT%AL(JP)%XD_WALL      (ILU,TOP%NWALL_LAYER))
  ALLOCATE(NT%AL(JP)%XROUGH_ROOF  (ILU))
  ALLOCATE(NT%AL(JP)%XROUGH_WALL  (ILU))
  ALLOCATE(NT%AL(JP)%XRESIDENTIAL (ILU))
  ALLOCATE(NT%AL(JP)%XGREENROOF   (ILU))
  ALLOCATE(NT%AL(JP)%XGARDEN      (ILU))
  ALLOCATE(TPN%XEMIS_PANEL    (ILU))
  ALLOCATE(TPN%XALB_PANEL     (ILU))
  ALLOCATE(TPN%XEFF_PANEL     (ILU))
  ALLOCATE(TPN%XFRAC_PANEL    (ILU))
  !
  NT%AL(JP)%XROAD_DIR(:) = 0.
  NT%AL(JP)%XROAD    (:) = 0.
  !
  ZDEF_ROAD_DIR = 0.
  IF (TOP%CROAD_DIR/='UNIF') THEN
    !* road direction if not specified by the user depends on patch number
    !  First patch has a Notrh-South road. Other patches have roads spaced by
    !  regular angles
    ZDEF_ROAD_DIR = 180. * FLOAT(JP-1) / FLOAT(TOP%NTEB_PATCH)
  END IF
  !
  CALL CONVERT_PATCH_TEB(BDD, DTB, DTCO, DTT, TOP, ZDEF_ROAD_DIR, T=NT%AL(JP), TPN=TPN  )
  !
  IF (.NOT. TOP%LGREENROOF .AND. MAXVAL(NT%AL(JP)%XGREENROOF)>0. ) THEN !<== A paralleliser pour un stop propre
    WRITE(ILUOUT,*) 'You choose NOT to have greenroofs, BUT your greenroof fraction is not zero'
    WRITE(ILUOUT,*) 'Please activate the greenroof option (and rerun the SURFEX suite from the PGD step)'
    WRITE(ILUOUT,*) 'Or be sure NOT to have any greenroofs in your area'
    CALL ABOR1_SFX('INIT_TEBN: GREENROOF OPTION NOT ACTIVATED WHILE GREENROOFS ARE PRESENT')
  ENDIF
  !
  IF (.NOT. TOP%LSOLAR_PANEL .AND. MAXVAL(TPN%XFRAC_PANEL)>0. ) THEN !<== A paralleliser pour un stop propre
    WRITE(ILUOUT,*) 'You choose NOT to have solar panels, BUT your solar panel fraction is not zero'
    WRITE(ILUOUT,*) 'Please activate the solar panel option (and rerun the SURFEX suite from the PGD step)'
    WRITE(ILUOUT,*) 'Or be sure NOT to have any solar panel in your area'
    CALL ABOR1_SFX('INIT_TEBN: SOLAR_PANEL OPTION NOT ACTIVATED WHILE SOLAR PANELS ARE PRESENT')
  ENDIF
  !
  !-------------------------------------------------------------------------------
  !
  !*       5.     Sky-view-factors:
  !               ----------------
  !
  ALLOCATE(NT%AL(JP)%XSVF_ROAD  (ILU))
  ALLOCATE(NT%AL(JP)%XSVF_GARDEN(ILU))
  ALLOCATE(NT%AL(JP)%XSVF_WALL  (ILU))
  !
  ALLOCATE(NB%AL(JP)%XGR          (ILU))
  ALLOCATE(NB%AL(JP)%XALB_WIN     (ILU))
  ALLOCATE(NB%AL(JP)%XF_WASTE_CAN (ILU))
  !
  !
  CALL TEB_MORPHO(HPROGRAM, NT%AL(JP)  )
                !
  !-------------------------------------------------------------------------------
  !
  !*       6.     Building Energy Model
  !               ---------------------
  !
  CALL INIT_BEM_n(DTCO, TOP, BOP, DTT, DTB, BDD, TG, NT%AL(JP), NB%AL(JP), ILUOUT)
  !
  !-------------------------------------------------------------------------------
  !
  !*      7.      Case of urban green areas
  !               -------------------------
  !
  IF (TOP%LGARDEN) THEN
  !
    CALL SET_SURFEX_FILEIN(HPROGRAM,'PGD ') ! change input file name to pgd name
    CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'TOWN  ','TEB   ','READ ')
    IF (JP==1) CALL INIT_TEB_VEG_OPTIONS_n(CHT, TD%MTO%LSURF_DIAG_ALBEDO, TOP%LGREENROOF, GDM%O, GRM%O, HPROGRAM)
    CALL INIT_TEB_GARDEN_PGD_n(DTCO, U, CHT%LCH_BIO_FLUX, TG, NT%AL(JP)%XGARDEN, TOP, &
                               GDM%O, GDM%S, GDM%K, GDM%P, GDM%NPE%AL(JP), GDM%DTV, GDM%GB, &
                               HPROGRAM,HINIT,(JP==1),KI,IVERSION,IBUGFIX,PCO2,PRHOA)
    ! Case of urban green roofs
    IF (TOP%LGREENROOF) THEN
      CALL INIT_TEB_GREENROOF_PGD_n(DTCO, U, CHT%LCH_BIO_FLUX, TG, NT%AL(JP)%XGREENROOF, TOP, &
                                    GRM%O, GRM%S, GRM%K, GRM%P, GRM%NPE%AL(JP), GRM%DTV, GRM%GB, &
                                    HPROGRAM,HINIT,(JP==1),KI,IVERSION,PCO2,PRHOA)
    ENDIF
    CALL END_IO_SURF_n(HPROGRAM)
    !
  ENDIF
!-------------------------------------------------------------------------------
END DO ! end of loop on TEB patches
!-------------------------------------------------------------------------------
!
!* Read irrigation parameters for TEB
!
 CALL SET_SURFEX_FILEIN(HPROGRAM,'PGD ') ! change input file name to pgd name
 CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'TOWN  ','TEB   ','READ ')
 CALL READ_PGD_TEB_IRRIG_n(TG, TIR, HPROGRAM)
 CALL END_IO_SURF_n(HPROGRAM)
!
!-------------------------------------------------------------------------------
!
!* if only physiographic fields are to be initialized, stop here.
!
IF (HINIT/='ALL' .AND. HINIT/='SOD') THEN
  IF (LHOOK) CALL DR_HOOK('INIT_TEB_N',1,ZHOOK_HANDLE)
  RETURN
END IF
!
!-------------------------------------------------------------------------------
!
!         Initialisation for IO
!
 CALL SET_SURFEX_FILEIN(HPROGRAM,'PREP') ! restore input file name
CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'TOWN  ','TEB   ','READ ')
!
!*       9.     Prognostic fields:
!               -----------------
!
!*              LOOP ON TEB PATCHES
!               -------------------
!
DO JP=1,TOP%NTEB_PATCH
!
!* TEB fields
  CALL READ_TEB_n(NB%AL(JP), BOP, DTCO, U, NT%AL(JP), TOP, TPN, HPROGRAM,JP)
!
  ALLOCATE(NT%AL(JP)%XAC_ROOF    (ILU))
  ALLOCATE(NT%AL(JP)%XAC_ROAD    (ILU))
  ALLOCATE(NT%AL(JP)%XAC_WALL    (ILU))
  ALLOCATE(NT%AL(JP)%XAC_TOP     (ILU))
  ALLOCATE(NT%AL(JP)%XAC_ROOF_WAT(ILU))
  ALLOCATE(NT%AL(JP)%XAC_ROAD_WAT(ILU))
  ALLOCATE(NT%AL(JP)%XQSAT_ROOF  (ILU))
  ALLOCATE(NT%AL(JP)%XQSAT_ROAD  (ILU))
  ALLOCATE(NT%AL(JP)%XDELT_ROOF  (ILU))
  ALLOCATE(NT%AL(JP)%XDELT_ROAD  (ILU))
!
!* Case of urban green areas
  IF (TOP%LGARDEN) THEN
    !
    CALL INIT_TEB_GARDEN_n(DTCO, UG, U, TD%MTO, TOP, GDM%O, GDM%DTV, GDM%K, GDM%P, &
                           GDM%NPE%AL(JP), GDM%VD%ND%AL(JP), GDM%VD%NDE%AL(JP), GDM%VD%NDEC%AL(JP), GDM%VD%NDM%AL(JP), &
                           HPROGRAM, HINIT, KI, KSW, PSW_BANDS, JP)
    ! Case of urban green roofs
    IF (TOP%LGREENROOF) CALL INIT_TEB_GREENROOF_n(DTCO, U, TD%MTO, TOP, GRM%O, GRM%DTV, GRM%K, GRM%P, &
                           GRM%NPE%AL(JP), GRM%VD%ND%AL(JP), GRM%VD%NDE%AL(JP), GRM%VD%NDEC%AL(JP), GRM%VD%NDM%AL(JP), &
                           HPROGRAM, HINIT, KI, KSV, PSW_BANDS, JP)
    !
  ENDIF
!-------------------------------------------------------------------------------
!
!*      10.     Infra-red Radiative fields:
!               --------------------------
!
!* snow long-wave properties (not initialized in read_gr_snow)
!
  CALL INIT_SNOW_LW(XEMISSN,NT%AL(JP)%TSNOW_ROOF)
  CALL INIT_SNOW_LW(XEMISSN,NT%AL(JP)%TSNOW_ROAD)
!
  IF (TOP%LGARDEN) THEN
    ZDIR_SW=0. ! night as first guess for albedo computation
    ZSCA_SW=0. !
    CALL TEB_VEG_PROPERTIES(NT%AL(JP)%XGARDEN, GDM%O, GDM%NPE%AL(JP), &
                           ZDIR_SW, ZSCA_SW, PSW_BANDS, KSW,      &
                           ZTS_GARDEN, ZEMIS_GARDEN, ZALB_GARDEN )
  ELSE
    ZALB_GARDEN = XUNDEF
    ZEMIS_GARDEN= XUNDEF
    ZTS_GARDEN  = XUNDEF
  END IF
  !
  IF (TOP%LGREENROOF) THEN
    ZDIR_SW=0. ! night as first guess for albedo computation
    ZSCA_SW=0. !
    CALL TEB_VEG_PROPERTIES(NT%AL(JP)%XGREENROOF, GRM%O, GRM%NPE%AL(JP),         &
                              ZDIR_SW, ZSCA_SW, PSW_BANDS, KSW,              &
                              ZTS_GREENROOF, ZEMIS_GREENROOF, ZALB_GREENROOF )
  ELSE
    ZALB_GREENROOF  = XUNDEF
    ZEMIS_GREENROOF = XUNDEF
    ZTS_GREENROOF   = XUNDEF
  END IF
!
!* averaged albedo, emissivity and radiative temperature
!
  CALL AVERAGED_TSRAD_TEB(NT%AL(JP), NB%AL(JP), ZEMIS_GARDEN, ZTS_GARDEN,  &
                          ZEMIS_GREENROOF, ZTS_GREENROOF, PEMIS, PTSRAD  )
!
!
!*       9.     Visible and near-infra-red Radiative fields:
!               -------------------------------------------
!
  ALLOCATE(ZDIR_ALB(ILU))
  ALLOCATE(ZSCA_ALB(ILU))
!
  CALL AVERAGED_ALBEDO_TEB(TOP,NT%AL(JP),TPN,NB%AL(JP),PZENITH,PAZIM,   &
                           ZALB_GARDEN, ZALB_GREENROOF,ZDIR_ALB, ZSCA_ALB)

  ISWB=SIZE(PSW_BANDS)
  DO JSWB=1,ISWB
    PDIR_ALB(:,JSWB) = ZDIR_ALB(:)
    PSCA_ALB(:,JSWB) = ZSCA_ALB(:)
  END DO
  !
  DEALLOCATE(ZDIR_ALB)
  DEALLOCATE(ZSCA_ALB)
!-------------------------------------------------------------------------------
!
!*      10.     Chemistry /dust
!               ---------------
!
  CALL INIT_CHEMICAL_n(ILUOUT, KSV, HSV, CHT%SVT,          &
                       CHT%CCH_NAMES, CHT%CAER_NAMES,     &
                       HDSTNAMES=CHT%CDSTNAMES, HSLTNAMES=CHT%CSLTNAMES        )
!
!* Initialization of dry deposition scheme (chemistry)
!
  IF (CHT%SVT%NBEQ>0 .AND. CHT%CCH_DRY_DEP=='WES89') THEN
    ALLOCATE(CHT%XDEP(ILU,CHT%SVT%NBEQ))
  ELSE
    ALLOCATE(CHT%XDEP(0,0))
  END IF
!
!-------------------------------------------------------------------------------
END DO ! end of loop on patches
!
IF (HINIT/='ALL') THEN
  CALL END_IO_SURF_n(HPROGRAM)
  IF (LHOOK) CALL DR_HOOK('INIT_TEB_N',1,ZHOOK_HANDLE)
  RETURN
END IF
!-------------------------------------------------------------------------------
!
!*       7.     Canopy air fields:
!               ------------------
!
 CALL READ_SBL_n(DTCO, U, SB, TOP%LCANOPY, HPROGRAM, "TOWN  ")
!
!-------------------------------------------------------------------------------
!
!*      11.     Diagnostics:
!               -----------
!
 CALL DIAG_TEB_INIT_n(TD%O, TD%D, TD%DUT, HPROGRAM,ILU,ISWB)
!
DO JP=1,TOP%NTEB_PATCH
  CALL DIAG_MISC_TEB_INIT_n(TD%NDMTC%AL(JP), TD%NDMT%AL(JP), TD%MTO, TOP, ILU, ISWB)
END DO ! end of loop on patches
!
!-------------------------------------------------------------------------------
!
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)
IF (LHOOK) CALL DR_HOOK('INIT_TEB_N',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE INIT_TEB_n
