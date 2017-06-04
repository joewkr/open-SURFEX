!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#############################################################
SUBROUTINE INIT_TEB_GARDEN_n (DTCO, UG, U, DMTO, TOP, IO, DTV, K, P, PEK, &
                              DK, DEK, DECK, DMK, HPROGRAM, HINIT, KI, KSW, PSW_BANDS, KPATCH)
!#############################################################
!
!!****  *INIT_TEB_GARDEN_n* - routine to initialize ISBA
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
!!      A. Lemonsu  *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    09/2009
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DIAG_n, ONLY : DIAG_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_DIAG_MISC_TEB_OPTIONS_n, ONLY : DIAG_MISC_TEB_OPTIONS_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_ISBA_n, ONLY : ISBA_P_t, ISBA_K_t, ISBA_PE_t
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_t
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t
!
USE MODD_TYPE_DATE_SURF
USE MODD_TYPE_SNOW
!
USE MODD_DATA_COVER_PAR,  ONLY: NVEGTYPE
USE MODD_SURF_PAR,        ONLY: XUNDEF, NUNDEF

USE MODD_SURF_ATM,        ONLY: LCPL_ARP
!
USE MODI_GET_LUOUT
USE MODI_ALLOCATE_TEB_VEG
USE MODI_ABOR1_SFX
USE MODI_READ_TEB_GARDEN_n
USE MODI_INIT_VEG_n
USE MODI_SOIL_ALBEDO
USE MODI_INIT_FROM_DATA_TEB_VEG_n
USE MODI_AVG_ALBEDO_EMIS_TEB_VEG
USE MODI_DIAG_TEB_VEG_INIT_n
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
TYPE(DIAG_MISC_TEB_OPTIONS_t), INTENT(INOUT) :: DMTO
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTV
TYPE(ISBA_K_t), INTENT(INOUT) :: K
TYPE(ISBA_P_t), INTENT(INOUT) :: P
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
TYPE(DIAG_t), INTENT(INOUT) :: DK
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: DEK
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: DECK
TYPE(DIAG_MISC_ISBA_t), INTENT(INOUT) :: DMK
!
 CHARACTER(LEN=6),                   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=3),                   INTENT(IN)  :: HINIT     ! choice of fields to initialize
INTEGER,                            INTENT(IN)  :: KI        ! number of points
INTEGER,                            INTENT(IN)  :: KSW       ! number of short-wave spectral bands
REAL,             DIMENSION(KSW),   INTENT(IN)  :: PSW_BANDS ! middle wavelength of each band
INTEGER,                            INTENT(IN)  :: KPATCH
!
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: ILUOUT   ! unit of output listing file
!
INTEGER           :: IDECADE  ! decade of simulation
!
 CHARACTER(LEN=3) :: YPATCH ! patch identificator
!
REAL, DIMENSION(KI)               :: ZWG1 ! work array for surface water content
REAL, DIMENSION(KI)               :: ZTG1 ! work array for surface temperature
REAL, DIMENSION(KI,KSW)           :: ZDIR_ALB  ! direct albedo for each band
REAL, DIMENSION(KI,KSW)           :: ZSCA_ALB  ! diffuse albedo for each band
REAL, DIMENSION(KI)               :: ZEMIS     ! emissivity
REAL, DIMENSION(KI)               :: ZTSRAD    ! radiative temperature
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!               Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('INIT_TEB_GARDEN_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
IO%CRAIN = "DEF"
!
!-------------------------------------------------------------------------------
!
 CALL ALLOCATE_TEB_VEG(PEK, KI, IO%NGROUND_LAYER, IO%NNBIOMASS)  
!
!-------------------------------------------------------------------------------
!
IF( IO%CCPSURF=='DRY' .AND. LCPL_ARP ) THEN
  CALL ABOR1_SFX('CCPSURF=DRY must not be used with LCPL_ARP')
ENDIF
!
!-------------------------------------------------------------------------------
!
IF (HINIT/='ALL') THEN
  IF (LHOOK) CALL DR_HOOK('INIT_TEB_GARDEN_N',1,ZHOOK_HANDLE)      
  RETURN
ENDIF
!
!-------------------------------------------------------------------------------
! Variables needed to run isba
!
ALLOCATE(K%XFFLOOD (KI))
ALLOCATE(K%XFF     (KI))
ALLOCATE(K%XFFG    (KI))
ALLOCATE(K%XFFV    (KI))
ALLOCATE(K%XFFROZEN(KI))
ALLOCATE(K%XALBF   (KI))
ALLOCATE(K%XEMISF  (KI))
K%XFFLOOD  = 0.0
K%XFF      = 0.0
K%XFFG     = 0.0
K%XFFV     = 0.0
K%XFFROZEN = 0.0
K%XALBF    = 0.0
K%XEMISF   = 0.0
!
ALLOCATE(K%XFSAT(KI))  
K%XFSAT(:) = 0.0
!
!-------------------------------------------------------------------------------
!
!*      10.     Prognostic and semi-prognostic fields
!               -------------------------------------
!
!* allocation of urban green area variables
!
!
YPATCH='   '
IF (TOP%NTEB_PATCH>1) WRITE(YPATCH,FMT='(A,I1,A)') 'T',KPATCH,'_'
!
 CALL READ_TEB_GARDEN_n(DTCO, U, IO, P, PEK, HPROGRAM,YPATCH)
!
DTV%LIMP_VEG  = .FALSE.
DTV%LIMP_Z0   = .FALSE.
DTV%LIMP_EMIS = .FALSE.
!
P%NSIZE_P = KI
 CALL INIT_VEG_n(IO, K, P, PEK, DTV, DMTO%LSURF_DIAG_ALBEDO, ZDIR_ALB, ZSCA_ALB, ZEMIS, ZTSRAD )
!
ZWG1(:) = PEK%XWG(:,1)
ZTG1(:) = PEK%XTG(:,1)
!
IF (.NOT. IO%LPAR) THEN
  CALL SOIL_ALBEDO(IO%CALBEDO, K%XWSAT(:,1),ZWG1, K, PEK, "ALL" )  
ELSE
  IF (TOP%TTIME%TDATE%MONTH /= NUNDEF) THEN
    IDECADE = 3 * ( TOP%TTIME%TDATE%MONTH - 1 ) + MIN(TOP%TTIME%TDATE%DAY-1,29) / 10 + 1
  ELSE
    IDECADE = 1
  END IF
  CALL INIT_FROM_DATA_TEB_VEG_n(DTV, K, P, PEK, IDECADE, .FALSE., .FALSE., .FALSE., .TRUE. )  
END IF
!

WHERE (PEK%XALBNIR_SOIL(:)==XUNDEF)
  PEK%XALBNIR_SOIL(:)=0.225
  PEK%XALBVIS_SOIL(:)=0.15
  PEK%XALBUV_SOIL (:)=0.07965
ENDWHERE  
!
 CALL AVG_ALBEDO_EMIS_TEB_VEG(PEK, IO%CALBEDO,  ZTG1, PSW_BANDS, ZDIR_ALB, ZSCA_ALB, ZEMIS,ZTSRAD )  
!
 CALL DIAG_TEB_VEG_INIT_n(DK, DEK, DECK, DMK, KI, PEK%TSNOW%NLAYER)
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INIT_TEB_GARDEN_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!
END SUBROUTINE INIT_TEB_GARDEN_n
