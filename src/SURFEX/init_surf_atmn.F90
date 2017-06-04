!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#############################################################
SUBROUTINE INIT_SURF_ATM_n (YSC, HPROGRAM,HINIT, OLAND_USE,             &
                            KI,KSV,KSW, HSV,PCO2,PRHOA,                 &
                             PZENITH,PAZIM,PSW_BANDS,PDIR_ALB,PSCA_ALB, &
                             PEMIS,PTSRAD,PTSURF,                       &
                             KYEAR, KMONTH,KDAY, PTIME, TPDATE_END,     &
                             HATMFILE,HATMFILETYPE, HTEST               )  
!#############################################################
!
!!****  *INIT_SURF_ATM_n* - routine to initialize GROUND
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
!      (P.Tulet )             01/11/03  initialisation of the surface chemistry!
!!     (D.Gazen)    01/12/03  change emissions handling for surf. externalization
!!     (P.LeMoigne) 18/07/05  get 1d mask only if associated tile exists    
!!     (B.Decharme)  03/2009  New keys read for arrange cover by user
!!     (B.Decharme)  04/2009  Read precipitation forcing from the restart file for ARPEGE/ALADIN run
!!     (A. Lemonsu)    2009   New key read for urban green areas
!!     (B.Decharme)  07/2011  Read pgd+prep
!!     (S. Queguiner)  2011   Modif chemistry (2.4)
!!     (B. Decharme)   2013   Read grid only once in AROME case
!!     (G. Tanguy)     2013   Add IF(ALLOCATED(NMASK_FULL))  before deallocate
!!      B. Decharme  04/2013  new coupling variables
!!                            Delete LPROVAR_TO_DIAG check
!!                            Delete NWG_LAYER_TOT
!!     (J.Escobar)      10/06/2013: replace DOUBLE PRECISION by REAL to handle problem for promotion of real on IBM SP
!!     (J.Durand)      2014   add activation of chemical deposition if LCH_EMIS=F
!!      R. Séférian 03/2014   Adding decoupling between CO2 seen by photosynthesis and radiative CO2
!!      M.Leriche & V. Masson 05/16 bug in write emis fields for nest
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_TYPE_DATE_SURF, ONLY : DATE
!
USE MODD_SURFEX_n, ONLY : SURFEX_t
!
USE MODD_SURF_ATM,       ONLY : XCO2UNCPL
!
USE MODD_READ_NAMELIST,  ONLY : LNAM_READ
USE MODD_SURF_CONF,      ONLY : CPROGNAME
USE MODD_DST_SURF,       ONLY : NDSTMDE, NDST_MDEBEG, LVARSIG_DST, LRGFIX_DST 
USE MODD_SLT_SURF,       ONLY : NSLTMDE, NSLT_MDEBEG, LVARSIG_SLT, LRGFIX_SLT                                

USE MODD_DATA_COVER_PAR, ONLY : NTILESFC
USE MODD_DATA_COVER,     ONLY : LCLIM_LAI, XDATA_LAI_ALL_YEARS, XDATA_LAI, &
                                NECO2_START_YEAR, NECO2_END_YEAR  
!
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
USE MODD_CHS_AEROSOL,    ONLY : LVARSIGI, LVARSIGJ
USE MODD_WRITE_SURF_ATM, ONLY : LNOWRITE_CANOPY, LNOWRITE_TEXFILE  
!
USE MODD_SURFEX_MPI, ONLY : XTIME_INIT_SEA, XTIME_INIT_WATER, XTIME_INIT_NATURE, XTIME_INIT_TOWN, &
                            NRANK, NPIO, NSIZE
USE MODD_SURFEX_OMP, ONLY : NBLOCKTOT
!
USE MODD_MASK, ONLY: NMASK_FULL
USE MODN_PREP_SURF_ATM, ONLY : LWRITE_EXTERN
!
USE MODI_INIT_IO_SURF_n
USE MODI_DEFAULT_SSO
USE MODI_DEFAULT_CH_SURF_ATM
USE MODI_DEFAULT_DIAG_SURF_ATM
USE MODI_READ_DEFAULT_SURF_ATM_n
USE MODI_READ_SURF_ATM_CONF_n
USE MODI_READ_SURF_ATM_DATE
USE MODI_READ_NAM_PREP_SURF_n
USE MODI_READ_SURF
USE MODI_SUNPOS
USE MODI_GET_SIZE_FULL_n
USE MODI_READ_COVER_n
USE MODI_READ_SSO_n
USE MODI_SUBSCALE_Z0EFF
USE MODI_READ_SSO_CANOPY_n
USE MODI_READ_DUMMY_n
USE MODI_READ_GRID
USE MODI_READ_GRIDTYPE
USE MODI_END_IO_SURF_n
USE MODI_PREP_CTRL_SURF_ATM
USE MODI_AVERAGE_RAD
USE MODI_AVERAGE_TSURF
USE MODI_INIT_CHEMICAL_n
USE MODI_CH_INIT_DEPCONST
USE MODI_CH_INIT_EMISSION_n
USE MODI_CH_INIT_SNAP_n
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_ABOR1_SFX
USE MODI_ALLOC_DIAG_SURF_ATM_n
USE MODI_GET_1D_MASK
USE MODI_INI_DATA_COVER
USE MODI_INIT_INLAND_WATER_n
USE MODI_INIT_NATURE_n
USE MODI_INIT_SEA_n
USE MODI_INIT_TOWN_n
USE MODI_READ_ARRANGE_COVER
USE MODI_READ_COVER_GARDEN
USE MODI_READ_ECO2_IRRIG
USE MODI_READ_LCLIM_LAI
USE MODI_READ_LECOCLIMAP
USE MODI_SURF_VERSION
USE MODI_GET_LUOUT
USE MODI_SET_SURFEX_FILEIN
!
USE MODI_INIT_CPL_GCM_n
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE 'mpif.h'
#endif
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(SURFEX_t), INTENT(INOUT) :: YSC
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
!*       0.2   Declarations of local variables
!              -------------------------------
!
 CHARACTER(LEN=3)  :: YREAD
!
INTEGER           :: ISWB     ! number of shortwave bands
INTEGER           :: JTILE    ! loop counter on tiles
INTEGER           :: IRESP    ! error return code
INTEGER           :: ILUOUT   ! unit of output listing file
INTEGER           :: ICH      ! unit of input chemical file
INTEGER           :: IVERSION, IBUGFIX       ! surface version
!
INTEGER, DIMENSION(:), ALLOCATABLE :: ISIZE_OMP
!
LOGICAL           :: LZENITH  ! is the PZENITH field initialized ?
!
REAL, DIMENSION(:,:), ALLOCATABLE   :: ZFRAC_TILE     ! fraction of each surface type
REAL, DIMENSION(KI,KSW,NTILESFC)    :: ZDIR_ALB_TILE  ! direct albedo
REAL, DIMENSION(KI,KSW,NTILESFC)    :: ZSCA_ALB_TILE  ! diffuse albedo
REAL, DIMENSION(KI,NTILESFC)        :: ZEMIS_TILE     ! emissivity
REAL, DIMENSION(KI,NTILESFC)        :: ZTSRAD_TILE    ! radiative temperature
REAL, DIMENSION(KI,NTILESFC)        :: ZTSURF_TILE    ! effective temperature
REAL, DIMENSION(KI)                 :: ZZENITH        ! zenith angle
REAL, DIMENSION(KI)                 :: ZAZIM          ! azimuth angle
REAL, DIMENSION(KI)                 :: ZTSUN          ! solar time since midnight
!
REAL, DIMENSION(:),     ALLOCATABLE :: ZP_ZENITH   ! zenithal angle
REAL, DIMENSION(:),     ALLOCATABLE :: ZP_AZIM     ! azimuthal angle
REAL, DIMENSION(:),     ALLOCATABLE :: ZP_CO2      ! air CO2 concentration
REAL, DIMENSION(:),     ALLOCATABLE :: ZP_RHOA     ! air density
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZP_DIR_ALB  ! direct albedo
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZP_SCA_ALB  ! diffuse albedo
REAL, DIMENSION(:),     ALLOCATABLE :: ZP_EMIS     ! emissivity
REAL, DIMENSION(:),     ALLOCATABLE :: ZP_TSRAD    ! radiative temperature
REAL, DIMENSION(:),     ALLOCATABLE :: ZP_TSURF    ! surface effective temperature
!
REAL, DIMENSION(:), ALLOCATABLE :: ZZ0VEG
REAL :: XTIME0
!
INTEGER :: ISIZE_FULL
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INIT_SURF_ATM_N',0,ZHOOK_HANDLE)
!
!
 CPROGNAME=HPROGRAM
!
IF (HTEST/='OK') THEN
   CALL ABOR1_SFX('INIT_SURF_ATMN: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF
!
!-------------------------------------------------------------------------------
!
 CALL SURF_VERSION
!
!-------------------------------------------------------------------------------
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
IF (LNAM_READ) THEN
 !
 !*       0.     Defaults
 !               --------
 !
 !        0.1. Hard defaults
 !      
 CALL DEFAULT_SSO(YSC%USS%CROUGH, YSC%USS%XFRACZ0, YSC%USS%XCOEFBE)
 CALL DEFAULT_CH_SURF_ATM(YSC%CHU%CCHEM_SURF_FILE, YSC%CHU%LCH_SURF_EMIS)
 CALL DEFAULT_DIAG_SURF_ATM(YSC%DUO%N2M, YSC%DUO%LT2MMW, YSC%DUO%LSURF_BUDGET,&
                            YSC%DUO%L2M_MIN_ZS, YSC%DUO%LRAD_BUDGET, YSC%DUO%LCOEF,&
                            YSC%DUO%LSURF_VARS, YSC%DUO%LSURF_BUDGETC, &
                            YSC%DUO%LRESET_BUDGETC, YSC%DUO%LSELECT, &
                            YSC%DUO%LPROVAR_TO_DIAG, YSC%DUO%LDIAG_GRID, &
                            YSC%DUO%LFRAC, YSC%DUO%XDIAG_TSTEP, &
                            YSC%DUO%LSNOWDIMNC, YSC%DUO%LRESETCUMUL )                       
 !
ENDIF
!
!        0.2. Defaults from file header
!    
 CALL READ_DEFAULT_SURF_ATM_n(YSC%CHU, YSC%DUO, YSC%USS, HPROGRAM)
!
!*       1.     Reading of configuration
!               ------------------------
!
!        1.1. general options (diagnostics, etc...)
!
 CALL READ_SURF_ATM_CONF_n(YSC%CHU, YSC%DUO, YSC%USS, HPROGRAM)
!
IF(XCO2UNCPL/=XUNDEF)THEN
  WRITE(ILUOUT,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
  WRITE(ILUOUT,*)'!!!                                           !!!'
  WRITE(ILUOUT,*)'!!!          WARNING    WARNING               !!!'
  WRITE(ILUOUT,*)'!!!                                           !!!'
  WRITE(ILUOUT,*)'!!! Decoupling between CO2 for photosynthesis !!!' 
  WRITE(ILUOUT,*)'!!! and atmospheric CO2 activated             !!!'
  WRITE(ILUOUT,*)'!!! In NAM_SURF_ATM XCO2UNCPL =',XCO2UNCPL,'  !!!'
  WRITE(ILUOUT,*)'!!!                                           !!!'
  WRITE(ILUOUT,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'        
ENDIF
!
!        1.2. Date
!
SELECT CASE (HINIT)
  CASE ('PGD')
    YSC%U%TTIME%TDATE%YEAR = NUNDEF
    YSC%U%TTIME%TDATE%MONTH= NUNDEF
    YSC%U%TTIME%TDATE%DAY  = NUNDEF
    YSC%U%TTIME%TIME       = XUNDEF
        
  CASE ('PRE')
    ! check that diagnostics are off if hinit=='pre'
    CALL PREP_CTRL_SURF_ATM(YSC%DUO, LNOWRITE_TEXFILE, ILUOUT)  
    ! preparation of fields  (date not present in PGD file)
    IF (LNAM_READ) CALL READ_NAM_PREP_SURF_n(HPROGRAM)
    CALL READ_SURF_ATM_DATE(HPROGRAM,HINIT,ILUOUT,HATMFILE,HATMFILETYPE,KYEAR,KMONTH,KDAY,PTIME,YSC%U%TTIME)

  CASE DEFAULT
    CALL INIT_IO_SURF_n(YSC%DTCO, YSC%U, HPROGRAM,'FULL  ','SURF  ','READ ')
    CALL READ_SURF(HPROGRAM,'DTCUR',YSC%U%TTIME,IRESP)
    CALL END_IO_SURF_n(HPROGRAM)
    LWRITE_EXTERN = .FALSE.

END SELECT
!
!-----------------------------------------------------------------------------------------------------
! READ PGD FILE
!-----------------------------------------------------------------------------------------------------
!
!        1.3. Schemes used
!
!         Initialisation for IO
!
 CALL SET_SURFEX_FILEIN(HPROGRAM,'PGD ') ! change input file name to pgd name
 CALL INIT_IO_SURF_n(YSC%DTCO, YSC%U, HPROGRAM,'FULL  ','SURF  ','READ ')
 CALL READ_SURF(HPROGRAM,'DIM_FULL  ',YSC%U%NDIM_FULL,  IRESP)
 CALL END_IO_SURF_n(HPROGRAM)
 CALL INIT_IO_SURF_n(YSC%DTCO, YSC%U, HPROGRAM,'FULL  ','SURF  ','READ ')
                
!
 CALL READ_SURF(HPROGRAM,'VERSION',IVERSION,IRESP)
 CALL READ_SURF(HPROGRAM,'BUG',IBUGFIX,IRESP)
!
IF (IVERSION>7 .OR. IVERSION==7 .AND.IBUGFIX>=2) THEN
  CALL READ_SURF(HPROGRAM,'STORAGETYPE',YREAD,IRESP)
ENDIF
!         reading
!
 CALL READ_SURF(HPROGRAM,'SEA   ',YSC%U%CSEA   ,IRESP)
 CALL READ_SURF(HPROGRAM,'WATER ',YSC%U%CWATER ,IRESP)
 CALL READ_SURF(HPROGRAM,'NATURE',YSC%U%CNATURE,IRESP)
 CALL READ_SURF(HPROGRAM,'TOWN  ',YSC%U%CTOWN  ,IRESP)
!
! 
 CALL READ_SURF(HPROGRAM,'DIM_SEA   ',YSC%U%NDIM_SEA,   IRESP)
 CALL READ_SURF(HPROGRAM,'DIM_NATURE',YSC%U%NDIM_NATURE,IRESP)
 CALL READ_SURF(HPROGRAM,'DIM_WATER ',YSC%U%NDIM_WATER, IRESP)
 CALL READ_SURF(HPROGRAM,'DIM_TOWN  ',YSC%U%NDIM_TOWN,  IRESP)
!
 CALL READ_LECOCLIMAP(HPROGRAM,YSC%U%LECOCLIMAP,YSC%U%LECOSG)
 CALL READ_ARRANGE_COVER(HPROGRAM,YSC%U%LWATER_TO_NATURE,YSC%U%LTOWN_TO_ROCK)
 CALL READ_COVER_GARDEN(HPROGRAM,YSC%U%LGARDEN)
!
!* reads if climatological LAI is used or not for ecoclimap2. If not, looks for year to be used.
 CALL READ_LCLIM_LAI(HPROGRAM,LCLIM_LAI)
IF (.NOT. LCLIM_LAI .AND. YSC%U%TTIME%TDATE%YEAR >= NECO2_START_YEAR &
                    .AND. YSC%U%TTIME%TDATE%YEAR <= NECO2_END_YEAR   ) YSC%DTCO%NYEAR=YSC%U%TTIME%TDATE%YEAR
 CALL INI_DATA_COVER(YSC%DTCO, YSC%U)
 CALL READ_ECO2_IRRIG(YSC%DTCO, HPROGRAM)
!
!*       2.     Cover fields and grid:
!               ---------------------
!
!        2.0. Get number of points on this proc
!
 CALL GET_SIZE_FULL_n(HPROGRAM,YSC%U%NDIM_FULL,YSC%U%NSIZE_FULL,ISIZE_FULL)
 YSC%U%NSIZE_FULL = ISIZE_FULL
!
!        2.1. Read cover
!
 CALL READ_COVER_n(YSC%DTCO, YSC%U, HPROGRAM)
!
!        2.2. Read grid
!
ALLOCATE(YSC%UG%G%XLAT      (YSC%U%NSIZE_FULL))
ALLOCATE(YSC%UG%G%XLON      (YSC%U%NSIZE_FULL))
ALLOCATE(YSC%UG%G%XMESH_SIZE(YSC%U%NSIZE_FULL))
ALLOCATE(YSC%USS%XZ0EFFJPDIR(YSC%U%NSIZE_FULL))
 CALL READ_GRID(HPROGRAM,YSC%UG%G,IRESP,YSC%USS%XZ0EFFJPDIR)
!
!        2.3. Initialize zenith and azimuth angles if not done yet
!
LZENITH = ALL(PZENITH /= XUNDEF)
IF (.NOT. LZENITH) CALL SUNPOS(KYEAR, KMONTH, KDAY, PTIME, YSC%UG%G%XLON, YSC%UG%G%XLAT, ZTSUN, ZZENITH, ZAZIM)
!
IF (HPROGRAM/='AROME '.AND.NRANK==NPIO) THEN
  !
  IF (.NOT.ASSOCIATED(YSC%UG%XGRID_FULL_PAR)) THEN
#ifdef MNH_PARALLEL
    CALL READ_GRIDTYPE(HPROGRAM,YSC%UG%G%CGRID,YSC%UG%G%NGRID_PAR,YSC%U%NSIZE_FULL,.FALSE.,HDIR='H')
    ALLOCATE(YSC%UG%XGRID_FULL_PAR(YSC%UG%G%NGRID_PAR))
    CALL READ_GRIDTYPE(HPROGRAM,YSC%UG%G%CGRID,YSC%UG%G%NGRID_PAR,YSC%U%NSIZE_FULL,.TRUE.,&
                       YSC%UG%XGRID_FULL_PAR,IRESP,HDIR='H')
#else          
    CALL READ_GRIDTYPE(HPROGRAM,YSC%UG%G%CGRID,YSC%UG%NGRID_FULL_PAR,YSC%U%NDIM_FULL,.FALSE.,HDIR='A')
    ALLOCATE(YSC%UG%XGRID_FULL_PAR(YSC%UG%NGRID_FULL_PAR))
    CALL READ_GRIDTYPE(HPROGRAM,YSC%UG%G%CGRID,YSC%UG%NGRID_FULL_PAR,YSC%U%NDIM_FULL,.TRUE.,&
                       YSC%UG%XGRID_FULL_PAR,IRESP,HDIR='A')
#endif               
  ENDIF
  !
ENDIF
!
!*       2.4     Allocation of chemical species name, chemical index of HSV array 
!
 CALL INIT_CHEMICAL_n(ILUOUT, KSV, HSV, YSC%SV,        &
                     YSC%CHU%CCH_NAMES, YSC%CHU%CAER_NAMES     )
!
!        2.4 Initialize Chemical Emissions
!
 CALL READ_SURF(HPROGRAM,'CH_EMIS',YSC%CHU%LCH_EMIS,IRESP)
!
IF (YSC%CHU%LCH_EMIS) THEN
  !
  IF ( IVERSION<7 .OR. IVERSION==7 .AND. IBUGFIX<3 ) THEN
    YSC%CHU%CCH_EMIS='AGGR'
  ELSE
    CALL READ_SURF(HPROGRAM,'CH_EMIS_OPT',YSC%CHU%CCH_EMIS,IRESP)
  END IF
  !
  IF (YSC%CHU%CCH_EMIS=='AGGR') THEN
    CALL CH_INIT_EMISSION_n(YSC%CHE, YSC%CHU%XCONVERSION, YSC%SV%CSV, &
                            HPROGRAM,YSC%U%NSIZE_FULL,HINIT,PRHOA,YSC%CHU%CCHEM_SURF_FILE) 
  ELSE
    CALL CH_INIT_SNAP_n(YSC%CHN, YSC%SV%CSV, &
                        HPROGRAM,YSC%U%NSIZE_FULL,HINIT,PRHOA,YSC%CHU%CCHEM_SURF_FILE)
  END IF
  !
ENDIF
!
!*       2.5 Initialization of dry deposition scheme (chemistry)  
!
IF (YSC%SV%NBEQ .GT. 0) THEN
!
  IF (HINIT=='ALL') CALL CH_INIT_DEPCONST(HPROGRAM,YSC%CHU%CCHEM_SURF_FILE,ILUOUT,YSC%SV%CSV(YSC%SV%NSV_CHSBEG:YSC%SV%NSV_CHSEND))
!
END IF
!
!*       2.5 Subgrid orography
!
 CALL READ_SSO_n(YSC%U%NSIZE_FULL, YSC%U%XSEA, YSC%USS, HPROGRAM)
!
!*       2.6 Orographic roughness length
!
ALLOCATE(YSC%USS%XZ0EFFIP(YSC%U%NSIZE_FULL))
ALLOCATE(YSC%USS%XZ0EFFIM(YSC%U%NSIZE_FULL))
ALLOCATE(YSC%USS%XZ0EFFJP(YSC%U%NSIZE_FULL))
ALLOCATE(YSC%USS%XZ0EFFJM(YSC%U%NSIZE_FULL))
ALLOCATE(YSC%USS%XZ0REL  (YSC%U%NSIZE_FULL))
!
ALLOCATE(ZZ0VEG(YSC%U%NSIZE_FULL))
ZZ0VEG(:) = 0.
!
 CALL SUBSCALE_Z0EFF(YSC%USS,ZZ0VEG,.TRUE.)
!
DEALLOCATE(ZZ0VEG)
!
!*       2.7 Dummy fields
!
 CALL READ_DUMMY_n(YSC%DUU,YSC%U%NSIZE_FULL, HPROGRAM)
!
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)
!
 CALL SET_SURFEX_FILEIN(HPROGRAM,'PREP') ! restore input file name
!
!-----------------------------------------------------------------------------------------------------
! END READ PGD FILE
!-----------------------------------------------------------------------------------------------------
!
!
!         Initialisation for IO
!
 CALL INIT_IO_SURF_n(YSC%DTCO, YSC%U, HPROGRAM,'FULL  ','SURF  ','READ ')
!
!*       2.8 Allocations and Initialization of diagnostics
!
IF (HINIT=='ALL') THEN
  CALL ALLOC_DIAG_SURF_ATM_n(YSC%DUO, YSC%DU, YSC%DUC, YSC%DUP, YSC%DUPC, &
                             YSC%U%NSIZE_FULL, YSC%U%TTIME, HPROGRAM,KSW)
ENDIF
!
!*       Canopy fields if Beljaars et al 2004 parameterization is used
!
IF (YSC%USS%CROUGH=='BE04') THEN
  CALL READ_SSO_CANOPY_n(YSC%DTCO, YSC%SB, YSC%U, HPROGRAM, HINIT)
ENDIF
!
!*       Physical fields need for ARPEGE/ALADIN climate run
!
 CALL INIT_CPL_GCM_n(YSC%U, HPROGRAM,HINIT)
!
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)
!
!-----------------------------------------------------------------------------------------------------
!
!*       4.     Initialization of masks for each surface
!               ----------------------------------------
!
!* number of geographical points
YSC%U%NSIZE_NATURE    = COUNT(YSC%U%XNATURE(:) > 0.0)
YSC%U%NSIZE_TOWN      = COUNT(YSC%U%XTOWN(:)   > 0.0)
YSC%U%NSIZE_WATER     = COUNT(YSC%U%XWATER(:)  > 0.0)
YSC%U%NSIZE_SEA       = COUNT(YSC%U%XSEA(:)    > 0.0)
!
ALLOCATE(YSC%U%NR_NATURE (YSC%U%NSIZE_NATURE))
ALLOCATE(YSC%U%NR_TOWN   (YSC%U%NSIZE_TOWN  ))
ALLOCATE(YSC%U%NR_WATER  (YSC%U%NSIZE_WATER ))
ALLOCATE(YSC%U%NR_SEA    (YSC%U%NSIZE_SEA   ))
!
IF (YSC%U%NSIZE_SEA   >0)CALL GET_1D_MASK( YSC%U%NSIZE_SEA,    YSC%U%NSIZE_FULL, YSC%U%XSEA   , YSC%U%NR_SEA   )
IF (YSC%U%NSIZE_WATER >0)CALL GET_1D_MASK( YSC%U%NSIZE_WATER,  YSC%U%NSIZE_FULL, YSC%U%XWATER , YSC%U%NR_WATER )
IF (YSC%U%NSIZE_TOWN  >0)CALL GET_1D_MASK( YSC%U%NSIZE_TOWN,   YSC%U%NSIZE_FULL, YSC%U%XTOWN  , YSC%U%NR_TOWN  )
IF (YSC%U%NSIZE_NATURE>0)CALL GET_1D_MASK( YSC%U%NSIZE_NATURE, YSC%U%NSIZE_FULL, YSC%U%XNATURE, YSC%U%NR_NATURE)
!
!* number of shortwave spectral bands
ISWB=SIZE(PSW_BANDS)
!
!* tile number
ALLOCATE(ZFRAC_TILE(YSC%U%NSIZE_FULL,NTILESFC))
JTILE = 0
!
!
!*       5.     Default values
!               --------------
!
ZDIR_ALB_TILE = XUNDEF
ZSCA_ALB_TILE = XUNDEF
ZEMIS_TILE    = XUNDEF
ZTSRAD_TILE   = XUNDEF
ZTSURF_TILE   = XUNDEF
!
#ifdef SFX_MPI
XTIME0 = MPI_WTIME()
#endif
!
!*       6.     Initialization of sea
!               ---------------------
!
JTILE               = JTILE + 1
ZFRAC_TILE(:,JTILE) = YSC%U%XSEA(:)
!
! pack variables which are arguments to this routine
 CALL PACK_SURF_INIT_ARG(YSC%U%NSIZE_SEA,YSC%U%NR_SEA)
!
! initialization
IF (YSC%U%NDIM_SEA>0) &
  CALL INIT_SEA_n(YSC%DTCO, YSC%DUO%LREAD_BUDGETC, YSC%UG, YSC%U, YSC%GCP, &
                  YSC%SM, YSC%DLO, YSC%DL, YSC%DLC, &
                  HPROGRAM,HINIT,YSC%U%NSIZE_SEA,KSV,KSW,            &
                  HSV,ZP_CO2,ZP_RHOA,                                &
                  ZP_ZENITH,ZP_AZIM,PSW_BANDS,ZP_DIR_ALB,ZP_SCA_ALB, &
                  ZP_EMIS,ZP_TSRAD,ZP_TSURF,                         &
                  KYEAR,KMONTH,KDAY,PTIME, HATMFILE,HATMFILETYPE,    &
                  'OK'                                               )  
!
!
 CALL UNPACK_SURF_INIT_ARG(JTILE,YSC%U%NSIZE_SEA,YSC%U%NR_SEA)  
!
#ifdef SFX_MPI
XTIME_INIT_SEA = XTIME_INIT_SEA + (MPI_WTIME() - XTIME0)*100./MAX(1,YSC%U%NSIZE_SEA)
XTIME0 = MPI_WTIME()
#endif
!
!*       7.     Initialization of lakes
!               -----------------------
!
!
JTILE               = JTILE + 1
ZFRAC_TILE(:,JTILE) = YSC%U%XWATER(:)
!
! pack variables which are arguments to this routine
 CALL PACK_SURF_INIT_ARG(YSC%U%NSIZE_WATER,YSC%U%NR_WATER)
!
! initialization
IF (YSC%U%NDIM_WATER>0) &
  CALL INIT_INLAND_WATER_n(YSC%DTCO, YSC%DUO%LREAD_BUDGETC, YSC%UG, &
                           YSC%U, YSC%WM, YSC%FM, YSC%DLO, YSC%DL, YSC%DLC,   &
                           HPROGRAM,HINIT,YSC%U%NSIZE_WATER,KSV,KSW,          &
                           HSV,ZP_CO2,ZP_RHOA,                                &
                           ZP_ZENITH,ZP_AZIM,PSW_BANDS,ZP_DIR_ALB,ZP_SCA_ALB, &
                           ZP_EMIS,ZP_TSRAD,ZP_TSURF,                         &
                           KYEAR,KMONTH,KDAY,PTIME, HATMFILE,HATMFILETYPE,    &
                           'OK'                                               )
!
 CALL UNPACK_SURF_INIT_ARG(JTILE,YSC%U%NSIZE_WATER,YSC%U%NR_WATER)
!
#ifdef SFX_MPI
XTIME_INIT_WATER = XTIME_INIT_WATER + (MPI_WTIME() - XTIME0)*100./MAX(1,YSC%U%NSIZE_WATER)
XTIME0 = MPI_WTIME()
#endif
!
!*       8.     Initialization of vegetation scheme
!               -----------------------------------
!
!
JTILE               = JTILE + 1
ZFRAC_TILE(:,JTILE) = YSC%U%XNATURE(:)
!
! pack variables which are arguments to this routine
 CALL PACK_SURF_INIT_ARG(YSC%U%NSIZE_NATURE,YSC%U%NR_NATURE)
!
! initialization
IF (YSC%U%NDIM_NATURE>0) &
  CALL INIT_NATURE_n(YSC%DTCO, YSC%DUO%LREAD_BUDGETC, YSC%UG, YSC%U,    &
                     YSC%USS, YSC%GCP, YSC%IM, YSC%DTZ, YSC%DLO, YSC%DL,&
                     YSC%DLC, YSC%NDST, YSC%SLT, YSC%SV,                &
                     HPROGRAM,HINIT,OLAND_USE,YSC%U%NSIZE_NATURE,       &
                     KSV,KSW, HSV,ZP_CO2,ZP_RHOA,                       &
                     ZP_ZENITH,ZP_AZIM,PSW_BANDS,ZP_DIR_ALB,ZP_SCA_ALB, &
                     ZP_EMIS,ZP_TSRAD,ZP_TSURF,                         &
                     KYEAR,KMONTH,KDAY,PTIME,TPDATE_END,                &
                     HATMFILE,HATMFILETYPE,'OK'      )
!
!
 CALL UNPACK_SURF_INIT_ARG(JTILE,YSC%U%NSIZE_NATURE,YSC%U%NR_NATURE)  
!
#ifdef SFX_MPI
XTIME_INIT_NATURE = XTIME_INIT_NATURE + (MPI_WTIME() - XTIME0)*100./MAX(1,YSC%U%NSIZE_NATURE)
XTIME0 = MPI_WTIME()
#endif
!
!*       9.     Initialization of urban scheme
!               ------------------------------
!
JTILE               = JTILE + 1
ZFRAC_TILE(:,JTILE) = YSC%U%XTOWN(:)
!
! pack variables which are arguments to this routine
 CALL PACK_SURF_INIT_ARG(YSC%U%NSIZE_TOWN,YSC%U%NR_TOWN)
!
! initialization
IF (YSC%U%NDIM_TOWN>0) &
  CALL INIT_TOWN_n(YSC%DTCO, YSC%DUO%LREAD_BUDGETC, YSC%UG, YSC%U, YSC%GCP, &
                   YSC%TM, YSC%GDM, YSC%GRM, YSC%DLO, YSC%DL, YSC%DLC,  &
                   HPROGRAM,HINIT,YSC%U%NSIZE_TOWN,KSV,KSW,             &
                   HSV,ZP_CO2,ZP_RHOA,                                &
                   ZP_ZENITH,ZP_AZIM,PSW_BANDS,ZP_DIR_ALB,ZP_SCA_ALB, &
                   ZP_EMIS,ZP_TSRAD,ZP_TSURF,                         &
                   KYEAR,KMONTH,KDAY,PTIME, HATMFILE,HATMFILETYPE,    &
                   'OK'                                               )  
!
!
 CALL UNPACK_SURF_INIT_ARG(JTILE,YSC%U%NSIZE_TOWN,YSC%U%NR_TOWN)  
!
#ifdef SFX_MPI
XTIME_INIT_TOWN = XTIME_INIT_TOWN + (MPI_WTIME() - XTIME0)*100./MAX(1,YSC%U%NSIZE_TOWN)
#endif
!
!
!*      10.     Output radiative and physical fields
!               ------------------------------------
!
IF (SIZE(PDIR_ALB)>0)                                                   &
  CALL AVERAGE_RAD(ZFRAC_TILE,                                            &
                   ZDIR_ALB_TILE, ZSCA_ALB_TILE, ZEMIS_TILE, ZTSRAD_TILE, &
                   PDIR_ALB,      PSCA_ALB,      PEMIS,      PTSRAD       ) 
!
IF (SIZE(PTSURF)>0) &
  CALL AVERAGE_TSURF(ZFRAC_TILE, ZTSURF_TILE, PTSURF)
!                  
DEALLOCATE(ZFRAC_TILE)
!
!-------------------------------------------------------------------------------
!==============================================================================
IF (LHOOK) CALL DR_HOOK('INIT_SURF_ATM_N',1,ZHOOK_HANDLE)
 CONTAINS
!==============================================================================
SUBROUTINE PACK_SURF_INIT_ARG(KSIZE,KMASK)
!
INTEGER, INTENT(IN)               :: KSIZE
INTEGER, INTENT(IN), DIMENSION(:) :: KMASK
INTEGER :: JJ
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! input arguments:
!
IF (LHOOK) CALL DR_HOOK('PACK_SURF_INIT_ARG',0,ZHOOK_HANDLE)
ALLOCATE(ZP_CO2          (KSIZE))
ALLOCATE(ZP_RHOA         (KSIZE))
ALLOCATE(ZP_ZENITH       (KSIZE))
ALLOCATE(ZP_AZIM         (KSIZE))
!
!
! output arguments:
!
ALLOCATE(ZP_DIR_ALB(KSIZE,ISWB))
ALLOCATE(ZP_SCA_ALB(KSIZE,ISWB))
ALLOCATE(ZP_EMIS   (KSIZE))
ALLOCATE(ZP_TSRAD  (KSIZE))
ALLOCATE(ZP_TSURF  (KSIZE))
!
IF (KSIZE>0) THEN
  ZP_CO2    = 6.E-4
  ZP_RHOA   = 1.2
  ZP_ZENITH = 0.
  ZP_AZIM   = 0.
  ZP_DIR_ALB = XUNDEF
  ZP_SCA_ALB = XUNDEF
  ZP_EMIS    = XUNDEF
  ZP_TSRAD   = XUNDEF
  ZP_TSURF   = XUNDEF
END IF
!
DO JJ=1,KSIZE
IF (SIZE(PCO2)>0) &
     ZP_CO2   (JJ)     = PCO2        (KMASK(JJ))  
IF (SIZE(PRHOA)>0) &
     ZP_RHOA  (JJ)     = PRHOA       (KMASK(JJ))  
IF (SIZE(PZENITH)>0) THEN
    IF (LZENITH) THEN
       ZP_ZENITH(JJ)     = PZENITH     (KMASK(JJ)) 
    ELSE
       ZP_ZENITH(JJ)     = ZZENITH     (KMASK(JJ)) 
    ENDIF
ENDIF
IF (SIZE(PAZIM  )>0) THEN
    IF (LZENITH) THEN
       ZP_AZIM  (JJ)     = PAZIM       (KMASK(JJ)) 
    ELSE
       ZP_AZIM  (JJ)     = ZAZIM       (KMASK(JJ)) 
    ENDIF
ENDIF
ENDDO
IF (LHOOK) CALL DR_HOOK('PACK_SURF_INIT_ARG',1,ZHOOK_HANDLE)
!
END SUBROUTINE PACK_SURF_INIT_ARG
!==============================================================================
SUBROUTINE UNPACK_SURF_INIT_ARG(KTILE,KSIZE,KMASK)
!
INTEGER, INTENT(IN) :: KTILE, KSIZE
!
INTEGER, INTENT(IN), DIMENSION(:) :: KMASK
!
INTEGER :: JJ   ! loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
IF (LHOOK) CALL DR_HOOK('UNPACK_SURF_INIT_ARG',0,ZHOOK_HANDLE)
DO JJ=1,KSIZE
IF (SIZE(ZTSRAD_TILE)>0) &
     ZTSRAD_TILE  (KMASK(JJ),KTILE)  = ZP_TSRAD     (JJ)  
IF (SIZE(ZDIR_ALB_TILE)>0) &
     ZDIR_ALB_TILE(KMASK(JJ),:,KTILE)= ZP_DIR_ALB   (JJ,:)  
IF (SIZE(ZSCA_ALB_TILE)>0) &
     ZSCA_ALB_TILE(KMASK(JJ),:,KTILE)= ZP_SCA_ALB   (JJ,:)  
IF (SIZE(ZEMIS_TILE)>0) &
     ZEMIS_TILE   (KMASK(JJ),KTILE)  = ZP_EMIS      (JJ)
IF (SIZE(ZTSURF_TILE)>0) &
     ZTSURF_TILE  (KMASK(JJ),KTILE)  = ZP_TSURF     (JJ)
ENDDO
!
DEALLOCATE(ZP_CO2    )
DEALLOCATE(ZP_RHOA   )
DEALLOCATE(ZP_ZENITH )
DEALLOCATE(ZP_AZIM   )
DEALLOCATE(ZP_DIR_ALB)
DEALLOCATE(ZP_SCA_ALB)
DEALLOCATE(ZP_EMIS   )
DEALLOCATE(ZP_TSRAD  )
DEALLOCATE(ZP_TSURF  )
IF (LHOOK) CALL DR_HOOK('UNPACK_SURF_INIT_ARG',1,ZHOOK_HANDLE)
!
END SUBROUTINE UNPACK_SURF_INIT_ARG
!==============================================================================
!
END SUBROUTINE INIT_SURF_ATM_n


