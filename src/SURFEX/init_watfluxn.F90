!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #############################################################
      SUBROUTINE INIT_WATFLUX_n (DTCO, OREAD_BUDGETC, UG, U, WM, &
                                 HPROGRAM,HINIT,                             &
                                  KI,KSV,KSW,                                &
                                  HSV,PCO2,PRHOA,                            &
                                  PZENITH,PAZIM,PSW_BANDS,PDIR_ALB,PSCA_ALB, &
                                  PEMIS,PTSRAD,PTSURF,                       &
                                  KYEAR, KMONTH,KDAY, PTIME,                 &
                                  HATMFILE,HATMFILETYPE,                     &
                                  HTEST                                      )  
!     #############################################################
!
!!****  *INIT_WATFLUX_n* - routine to initialize WATFLUX
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
!!      B. Decharme 08/2009 : specific treatment for water/ice in the Earth System Model 
!!      B. Decharme 07/2011 : read pgd+prep 
!!       B.Decharme 04/2013 new coupling variables
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURFEX_n, ONLY : WATFLUX_MODEL_t
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_READ_NAMELIST,  ONLY : LNAM_READ
USE MODD_SFX_OASIS,      ONLY : LCPL_SEA, LCPL_SEAICE
!
USE MODD_CSTS,           ONLY : XTT
USE MODD_CHS_AEROSOL,    ONLY: LVARSIGI, LVARSIGJ
USE MODD_DST_SURF,       ONLY: LVARSIG_DST, NDSTMDE, NDST_MDEBEG, LRGFIX_DST
USE MODD_SLT_SURF,       ONLY: LVARSIG_SLT, NSLTMDE, NSLT_MDEBEG, LRGFIX_SLT
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
!
USE MODI_INIT_IO_SURF_n
USE MODI_DEFAULT_CH_DEP
USE MODI_DEFAULT_WATFLUX
USE MODI_DEFAULT_DIAG_WATFLUX
USE MODI_READ_DEFAULT_WATFLUX_n
USE MODI_READ_WATFLUX_CONF_n
USE MODI_READ_WATFLUX_n
USE MODI_READ_PGD_WATFLUX_n
USE MODI_DIAG_WATFLUX_INIT_n
USE MODI_END_IO_SURF_n
USE MODI_GET_LUOUT
USE MODI_READ_SURF
USE MODI_READ_WATFLUX_DATE
USE MODI_READ_NAM_PREP_WATFLUX_n
USE MODI_INIT_CHEMICAL_n
USE MODI_PREP_CTRL
USE MODI_UPDATE_RAD_WATER
!
USE MODI_READ_SBL_n
USE MODI_SET_SURFEX_FILEIN
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
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
TYPE(WATFLUX_MODEL_t), INTENT(INOUT) :: WM
!
 CHARACTER(LEN=6),                 INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=3),                 INTENT(IN)  :: HINIT     ! choice of fields to initialize
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
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: ILU    ! sizes of WATFLUX arrays
INTEGER           :: ILUOUT ! unit of output listing file
INTEGER           :: IRESP  ! return code
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!         Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('INIT_WATFLUX_N',0,ZHOOK_HANDLE)
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('INIT_WATFLUXN: FATAL ERROR DURING ARGUMENT TRANSFER')
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
IF (LNAM_READ) THEN
 !
 !
 !*       0.     Defaults
 !               --------
 !
 !        0.1. Hard defaults
 !      
 CALL DEFAULT_WATFLUX(WM%W%XTSTEP,WM%W%XOUT_TSTEP,WM%W%CWAT_ALB,WM%W%CINTERPOL_TS)
 CALL DEFAULT_CH_DEP(WM%CHW%CCH_DRY_DEP)
 CALL DEFAULT_DIAG_WATFLUX(WM%DWO%N2M,WM%DWO%LSURF_BUDGET,WM%DWO%L2M_MIN_ZS,WM%DWO%LRAD_BUDGET,&
                           WM%DWO%LCOEF,WM%DWO%LSURF_VARS, &
                           WM%DWO%LSURF_BUDGETC,WM%DWO%LRESET_BUDGETC,WM%DWO%XDIAG_TSTEP        )  
 !
ENDIF
!
!        0.2. Defaults from file header
!    
 CALL READ_DEFAULT_WATFLUX_n(WM%CHW, WM%DWO, WM%W, HPROGRAM)
!
!*       1.1    Reading of configuration:
!               -------------------------
!
!
 CALL READ_WATFLUX_CONF_n(WM%CHW, WM%DWO, WM%W, HPROGRAM)
!
WM%W%LINTERPOL_TS=.FALSE.
IF(LCPL_SEA)THEN       
! No TS water interpolation in Earth System Model
  WM%W%CINTERPOL_TS='NONE  '
  WM%W%LINTERPOL_TS=.FALSE.
ELSEIF(WM%W%CINTERPOL_TS/='NONE  ')THEN
  WM%W%LINTERPOL_TS=.TRUE.
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       1.     Cover fields and grid:
!               ---------------------
!* date
!
SELECT CASE (HINIT)
  CASE ('PGD')
    WM%W%TTIME%TDATE%YEAR = NUNDEF
    WM%W%TTIME%TDATE%MONTH= NUNDEF
    WM%W%TTIME%TDATE%DAY  = NUNDEF
    WM%W%TTIME%TIME       = XUNDEF

  CASE ('PRE')
    CALL PREP_CTRL(WM%DWO,ILUOUT )  
    IF (LNAM_READ) CALL READ_NAM_PREP_WATFLUX_n(HPROGRAM)                 
    CALL READ_WATFLUX_DATE(HPROGRAM,HINIT,ILUOUT,HATMFILE,HATMFILETYPE,KYEAR,KMONTH,KDAY,PTIME,WM%W%TTIME)

  CASE DEFAULT
CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'FULL  ','SURF  ','READ ')
    CALL READ_SURF(HPROGRAM,'DTCUR',WM%W%TTIME,IRESP)
    CALL END_IO_SURF_n(HPROGRAM)
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
CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'WATER ','WATFLX','READ ')
!
!         Reading of the fields
!
 CALL READ_PGD_WATFLUX_n(DTCO, U, UG, WM%G, WM%W, HPROGRAM)
!
!-------------------------------------------------------------------------------
!
!* if only physiographic fields are to be initialized, stop here.
!
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)
 CALL SET_SURFEX_FILEIN(HPROGRAM,'PREP') ! restore input file name
!
!-----------------------------------------------------------------------------------------------------
! END READ PGD FILE
!-----------------------------------------------------------------------------------------------------
!
IF (HINIT/='ALL' .AND. HINIT/='SOD') THEN
  IF (LHOOK) CALL DR_HOOK('INIT_WATFLUX_N',1,ZHOOK_HANDLE)
  RETURN
END IF
!
!-------------------------------------------------------------------------------
!
!         Initialisation for IO
!
CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'WATER ','WATFLX','READ ')
!
!
!*       2.     Prognostic and cover fields:
!               ---------------------------
!
 CALL READ_WATFLUX_n(DTCO, U, WM%W, HPROGRAM)
!
IF (HINIT/='ALL') THEN
  CALL END_IO_SURF_n(HPROGRAM)
  IF (LHOOK) CALL DR_HOOK('INIT_WATFLUX_N',1,ZHOOK_HANDLE)
  RETURN
END IF
!
ILU = SIZE(WM%W%XCOVER,1)
!
!
!*       3.     Specific fields when using earth system model (Ice temperature)
!               ---------------------------------------------------------------
!
IF(LCPL_SEAICE)THEN
  ALLOCATE(WM%W%XTICE   (ILU))
  ALLOCATE(WM%W%XICE_ALB(ILU))
  WM%W%XTICE   (:)=XUNDEF
  WM%W%XICE_ALB(:)=XUNDEF
ELSE
  ALLOCATE(WM%W%XTICE   (0))
  ALLOCATE(WM%W%XICE_ALB(0))
ENDIF
!
!*       4.     Albedo, emissivity and temperature fields on open water and ice
!               ---------------------------------------------------------------
!
ALLOCATE(WM%W%XDIR_ALB (ILU))
ALLOCATE(WM%W%XSCA_ALB (ILU))
ALLOCATE(WM%W%XEMIS    (ILU))
WM%W%XDIR_ALB = 0.0
WM%W%XSCA_ALB = 0.0
WM%W%XEMIS    = 0.0
!
 CALL UPDATE_RAD_WATER(WM%W,PZENITH,XTT,PDIR_ALB,PSCA_ALB,PEMIS,PTSRAD )  
!
PTSURF(:) = WM%W%XTS(:)
!
!-------------------------------------------------------------------------------
!
!*       5.     SBL air fields:
!               --------------
!
 CALL READ_SBL_n(DTCO, U, WM%SB, WM%W%LSBL, HPROGRAM, "WATER ")
!
!-------------------------------------------------------------------------------
!
!*       6.     Chemistry / dust
!               ----------------
!
 CALL INIT_CHEMICAL_n(ILUOUT, KSV, HSV, WM%CHW%SVW,         &
                     WM%CHW%CCH_NAMES, WM%CHW%CAER_NAMES,      &
                     HDSTNAMES=WM%CHW%CDSTNAMES, HSLTNAMES=WM%CHW%CSLTNAMES        )
!
!* depositiion scheme
!

IF (WM%CHW%SVW%NBEQ>0 .AND. WM%CHW%CCH_DRY_DEP=='WES89') THEN
  ALLOCATE(WM%CHW%XDEP(ILU,WM%CHW%SVW%NBEQ))
ELSE
  ALLOCATE(WM%CHW%XDEP(0,0))
END IF
!
!-------------------------------------------------------------------------------
!
!*       7.     diagnostics initialization
!               --------------------------
!
 CALL DIAG_WATFLUX_INIT_n(OREAD_BUDGETC, WM%DWO, WM%DW, WM%DWC, WM%W, &
                          HPROGRAM,ILU,KSW)
!
!-------------------------------------------------------------------------------
!
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)
IF (LHOOK) CALL DR_HOOK('INIT_WATFLUX_N',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_WATFLUX_n
