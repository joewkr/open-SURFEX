!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #############################################################
SUBROUTINE INIT_FLAKE_n ( DTCO, OREAD_BUDGETC, UG, U, FM,            &
                          HPROGRAM,HINIT,                            &
                          KI,KSV,KSW,                                &
                          HSV,PCO2,PRHOA,                            &
                          PZENITH,PAZIM,PSW_BANDS,PDIR_ALB,PSCA_ALB, &
                          PEMIS,PTSRAD,PTSURF,                       &
                          KYEAR, KMONTH,KDAY, PTIME,                 &
                          HATMFILE,HATMFILETYPE,                     &
                          HTEST                                     )   
!     #############################################################
!
!!****  *INIT_FLAKE_n* - routine to initialize FLAKE model
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
!!      B. Decharme    07/11 : read pgd+prep
!!      Modified    04/2013, P. Le Moigne: FLake chemistry
!!      Modified    04/2013, P. Le Moigne: Coupling with AGCM
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_SURFEX_n, ONLY : FLAKE_MODEL_t
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
!
USE MODD_CSTS,          ONLY : XTT, XPI, XOMEGA 
!
USE MODD_WATER_PAR,     ONLY : XALBWATICE, XALBWATSNOW
USE MODD_SNOW_PAR,      ONLY : XANSMIN, XANSMAX
!
!
!
!
USE MODD_CHS_AEROSOL,    ONLY: LVARSIGI, LVARSIGJ
USE MODD_DST_SURF,       ONLY: LVARSIG_DST, NDSTMDE, NDST_MDEBEG, LRGFIX_DST
USE MODD_SLT_SURF,       ONLY: LVARSIG_SLT, NSLTMDE, NSLT_MDEBEG, LRGFIX_SLT
!
USE MODD_READ_NAMELIST,  ONLY : LNAM_READ
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
!
USE MODI_INIT_IO_SURF_n
USE MODI_DEFAULT_CH_DEP
USE MODI_DEFAULT_FLAKE
USE MODI_DEFAULT_DIAG_FLAKE 
USE MODI_READ_DEFAULT_FLAKE_n
USE MODI_READ_FLAKE_CONF_n
USE MODI_READ_FLAKE_n
USE MODI_READ_PGD_FLAKE_n
USE MODI_DIAG_FLAKE_INIT_n
USE MODI_END_IO_SURF_n
USE MODI_GET_LUOUT
USE MODI_READ_SURF
USE MODI_READ_FLAKE_DATE
USE MODI_READ_NAM_PREP_FLAKE_n
USE MODI_INIT_CHEMICAL_n
USE MODI_PREP_CTRL_FLAKE
USE MODI_UPDATE_RAD_FLAKE
USE MODI_READ_SBL_n
!
USE MODI_SET_SURFEX_FILEIN
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
USE MODI_GET_TYPE_DIM_n
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
TYPE(FLAKE_MODEL_t), INTENT(INOUT) :: FM
!
 CHARACTER(LEN=6),                 INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=3),                 INTENT(IN)  :: HINIT     ! choice of fields to initialize
INTEGER,                          INTENT(IN)  :: KI        ! number of points
INTEGER,                          INTENT(IN)  :: KSV       ! number of scalars
INTEGER,                          INTENT(IN)  :: KSW       ! number of short-wave spectral bands
 CHARACTER(LEN=6), DIMENSION(KI), INTENT(IN)  :: HSV       ! name of all scalar variables
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
INTEGER,                          INTENT(IN)  :: KYEAR     ! current year (UTC)
INTEGER,                          INTENT(IN)  :: KMONTH    ! current month (UTC)
INTEGER,                          INTENT(IN)  :: KDAY      ! current day (UTC)
REAL,                             INTENT(IN)  :: PTIME     ! current time since
                                                          !  midnight (UTC, s)
!
 CHARACTER(LEN=28),                INTENT(IN)  :: HATMFILE    ! atmospheric file name
 CHARACTER(LEN=6),                 INTENT(IN)  :: HATMFILETYPE! atmospheric file type
 CHARACTER(LEN=2),                 INTENT(IN)  :: HTEST       ! must be equal to 'OK'
!
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!
INTEGER           :: ILU    ! sizes of FLAKE arrays
INTEGER           :: ILUOUT ! unit of output listing file
INTEGER           :: IRESP  ! return code
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!

!         Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('INIT_FLAKE_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('INIT_FLAKEN: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF
!
ALLOCATE(FM%DMF%XZWAT_PROFILE(100))
!
!         Others litlle things
!
PDIR_ALB = XUNDEF
PSCA_ALB = XUNDEF
PEMIS    = XUNDEF
PTSRAD   = XUNDEF
PTSURF   = XUNDEF
!
IF (LNAM_READ) THEN
 !
 !*       0.     Defaults
 !               --------
 !
 !        0.1. Hard defaults
 !      
 CALL DEFAULT_FLAKE(FM%F%XTSTEP,FM%F%XOUT_TSTEP,FM%F%LSEDIMENTS,FM%F%CSNOW_FLK,FM%F%CFLK_FLUX,FM%F%CFLK_ALB,&
                    FM%F%LSKINTEMP)
 CALL DEFAULT_CH_DEP(FM%CHF%CCH_DRY_DEP)
 CALL DEFAULT_DIAG_FLAKE(FM%DFO%N2M,FM%DFO%LSURF_BUDGET,FM%DFO%L2M_MIN_ZS,FM%DFO%LRAD_BUDGET,&
                         FM%DFO%LCOEF,FM%DFO%LSURF_VARS, FM%DMF%LWATER_PROFILE,FM%DFO%LSURF_BUDGETC,&
                         FM%DFO%LRESET_BUDGETC,FM%DFO%XDIAG_TSTEP,FM%DMF%XZWAT_PROFILE      )  
 !
ENDIF
!
!        0.2. Defaults from file header
!    
 CALL READ_DEFAULT_FLAKE_n(FM%CHF, FM%DFO, FM%DMF, FM%F, HPROGRAM)

!
!*       1.1    Reading of configuration:
!               -------------------------
!
 CALL READ_FLAKE_CONF_n(FM%CHF, FM%DFO, FM%DMF, FM%F, HPROGRAM)
!
!-------------------------------------------------------------------------------
!
!*       1.     Cover fields and grid:
!               ---------------------
!* date
!
SELECT CASE (HINIT)
  CASE ('PGD')
    FM%F%TTIME%TDATE%YEAR = NUNDEF
    FM%F%TTIME%TDATE%MONTH= NUNDEF
    FM%F%TTIME%TDATE%DAY  = NUNDEF
    FM%F%TTIME%TIME       = XUNDEF

  CASE ('PRE')
    CALL PREP_CTRL_FLAKE(FM%DFO,ILUOUT,FM%DMF%LWATER_PROFILE) 
    IF (LNAM_READ) CALL READ_NAM_PREP_FLAKE_n(HPROGRAM)                            
    CALL READ_FLAKE_DATE(HPROGRAM,HINIT,ILUOUT,HATMFILE,HATMFILETYPE,KYEAR,KMONTH,KDAY,PTIME,FM%F%TTIME)

  CASE DEFAULT
CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'FULL  ','SURF  ','READ ')
    CALL READ_SURF(HPROGRAM,'DTCUR',FM%F%TTIME,IRESP)
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
!
CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'WATER ','FLAKE ','READ ')
!
!         Reading of the fields
!
 CALL READ_PGD_FLAKE_n(DTCO, U, UG, FM%G, FM%F, HPROGRAM)
!
 CALL END_IO_SURF_n(HPROGRAM)
 CALL SET_SURFEX_FILEIN(HPROGRAM,'PREP') ! restore input file name
!
!-----------------------------------------------------------------------------------------------------
! END READ PGD FILE
!-----------------------------------------------------------------------------------------------------
!
!* if only physiographic fields are to be initialized, stop here.
!
IF (HINIT/='ALL') THEN
  IF (LHOOK) CALL DR_HOOK('INIT_FLAKE_N',1,ZHOOK_HANDLE)
  RETURN
END IF
!
!-------------------------------------------------------------------------------
!
!*       2.     Prognostic and cover fields:
!               ---------------------------
!
CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'WATER ','FLAKE ','READ ')
!
 CALL READ_FLAKE_n(DTCO, U, FM%F, HPROGRAM)
!
ILU = SIZE(FM%F%XCOVER,1)
!
!-------------------------------------------------------------------------------
!
!*       3.     Specific fields 
!               ---------------
!
ALLOCATE(FM%F%XCORIO         (ILU))
ALLOCATE(FM%F%XICE_ALB       (ILU))
ALLOCATE(FM%F%XSNOW_ALB      (ILU))
ALLOCATE(FM%F%XEXTCOEF_ICE   (ILU))
ALLOCATE(FM%F%XEXTCOEF_SNOW  (ILU))
!
FM%F%XCORIO(:) = 2*XOMEGA*SIN(FM%G%XLAT(:)*XPI/180.)
!
FM%F%XICE_ALB  = XALBWATICE  ! constant, should be improved latter
FM%F%XSNOW_ALB = XALBWATSNOW ! constant, should be improved latter
!
FM%F%XEXTCOEF_ICE  = XUNDEF !not used
FM%F%XEXTCOEF_SNOW = XUNDEF !not used
!-------------------------------------------------------------------------------
!
!*       4.     Albedo, emissivity and radiative fields on lake
!               -----------------------------------------------
!
ALLOCATE(FM%F%XDIR_ALB (ILU))
ALLOCATE(FM%F%XSCA_ALB (ILU))
ALLOCATE(FM%F%XEMIS    (ILU))
FM%F%XDIR_ALB = 0.0
FM%F%XSCA_ALB = 0.0
FM%F%XEMIS    = 0.0
!
 CALL UPDATE_RAD_FLAKE(FM%F,PZENITH,PDIR_ALB,PSCA_ALB,PEMIS,PTSRAD  )
!
PTSURF(:) = FM%F%XTS(:)
!
!-------------------------------------------------------------------------------
!
!*       6.     SBL air fields:
!               --------------
!
 CALL READ_SBL_n(DTCO, U, FM%SB, FM%F%LSBL, HPROGRAM, "WATER ")
!
!-------------------------------------------------------------------------------
!
!*       6.     Chemistry / dust
!               ----------------
!
!
 CALL INIT_CHEMICAL_n(ILUOUT, KSV, HSV, FM%CHF%SVF,    &      
                     FM%CHF%CCH_NAMES, FM%CHF%CAER_NAMES,      &
                     HDSTNAMES=FM%CHF%CDSTNAMES, HSLTNAMES=FM%CHF%CSLTNAMES  )
!
!* depositiion scheme
!
IF (FM%CHF%SVF%NBEQ>0 .AND. FM%CHF%CCH_DRY_DEP=='WES89') THEN
  ALLOCATE(FM%CHF%XDEP(ILU,FM%CHF%SVF%NBEQ))
ELSE
  ALLOCATE(FM%CHF%XDEP(0,0))
END IF
!
!-------------------------------------------------------------------------------
!
!*       7.     diagnostics initialization
!               --------------------------
!
 CALL DIAG_FLAKE_INIT_n(OREAD_BUDGETC, FM%DFO, FM%DF, FM%DFC, FM%DMF, FM%F, &
                        HPROGRAM,ILU,KSW)
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)
IF (LHOOK) CALL DR_HOOK('INIT_FLAKE_N',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_FLAKE_n
