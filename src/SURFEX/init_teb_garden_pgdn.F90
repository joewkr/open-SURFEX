!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#############################################################
SUBROUTINE INIT_TEB_GARDEN_PGD_n (DTCO, U, OCH_BIO_FLUX, G, PGARDEN, TOP, IO, S, K, P, PEK, DTV, GB,  &
                                  HPROGRAM, HINIT, OPATCH1, KI, KVERSION, KBUGFIX, PCO2, PRHOA)
!#############################################################
!
!!****  *INIT_TEB_GARDEN_PGD_n* - routine to initialize ISBA
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
!&
!
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
!!  11/2013 (B. Decharme) No exp profile with DIF
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!      ------------
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t, SSO_INIT
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_S_t, ISBA_K_t, ISBA_P_t, ISBA_PE_t
!
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_GR_BIOG_n, ONLY : GR_BIOG_t
!
USE MODD_AGRI_n, ONLY : AGRI_t
!
USE MODD_TYPE_DATE_SURF
USE MODD_TYPE_SNOW
!
USE MODD_DATA_COVER_PAR,  ONLY: NVEGTYPE
USE MODD_SURF_PAR,ONLY: XUNDEF, NUNDEF

USE MODD_SGH_PAR, ONLY: XF_DECAY
!
USE MODI_READ_PREP_GARDEN_SNOW
USE MODI_GET_LUOUT
USE MODI_ALLOCATE_TEB_VEG_PGD
USE MODI_READ_PGD_TEB_GARDEN_n
USE MODI_CONVERT_PATCH_ISBA
USE MODI_INIT_FROM_DATA_TEB_VEG_n
USE MODI_INIT_VEG_PGD_n
USE MODI_EXP_DECAY_SOIL_FR
USE MODI_ABOR1_SFX
USE MODI_AV_PGD
!
USE MODE_TEB_VEG
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!      -------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
LOGICAL, INTENT(IN) :: OCH_BIO_FLUX
TYPE(GRID_t), INTENT(INOUT) :: G
REAL, DIMENSION(:), INTENT(IN) :: PGARDEN
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t), INTENT(INOUT) :: S
TYPE(ISBA_K_t), INTENT(INOUT) :: K
TYPE(ISBA_P_t), INTENT(INOUT) :: P
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
!
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTV
TYPE(GR_BIOG_t), INTENT(INOUT) :: GB
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=3),   INTENT(IN)  :: HINIT     ! choice of fields to initialize
LOGICAL,    INTENT(IN)  :: OPATCH1 ! flag to read PGD fields in the file
INTEGER,    INTENT(IN)  :: KI! number of points
INTEGER,    INTENT(IN)  :: KVERSION  ! version number of the file being read
INTEGER,    INTENT(IN)  :: KBUGFIX
REAL,     DIMENSION(KI),    INTENT(IN)  :: PCO2! CO2 concentration (kg/m3)
REAL,     DIMENSION(KI),    INTENT(IN)  :: PRHOA       ! air density
!
!
!
!*       0.2   Declarations of local variables
!      -------------------------------
!
TYPE(SSO_t) :: YSS
TYPE(AGRI_t) :: YAG
!
INTEGER   :: JILU     ! loop increment
INTEGER   :: ILUOUT   ! unit of output listing file
!
INTEGER   :: IDECADE  ! decade of simulation
!
INTEGER :: JVEG, JI ! loop counter on vegtypes
!
REAL, DIMENSION(KI)       :: ZF
REAL, DIMENSION(KI)       :: ZWORK
!
REAL, DIMENSION(0) :: ZTDEEP_CLI, ZGAMMAT_CLI, ZTHRESHOLD
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!       Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('INIT_TEB_GARDEN_PGD_n',0,ZHOOK_HANDLE)
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
CALL SSO_INIT(YSS)
!
!*       1.     Reading of snow configuration:
!               ------------------------------
!
!* initialization of snow scheme (TSNOW defined in MODD_TEB_GARDEN_n)
!
IF (HINIT=='PRE') THEN
  CALL READ_PREP_GARDEN_SNOW(HPROGRAM,PEK%TSNOW%SCHEME,PEK%TSNOW%NLAYER)
!
  IF (PEK%TSNOW%SCHEME.NE.'3-L' .AND. PEK%TSNOW%SCHEME.NE.'CRO' .AND. IO%CISBA=='DIF') THEN
    CALL ABOR1_SFX("INIT_TEB_GARDEN_n: WITH CISBA = DIF, CSNOW MUST BE 3-L OR CRO")
  ENDIF
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       2.     Physiographic fields
!       --------------------
!
!* allocation of urban green area variables
!
 CALL ALLOCATE_TEB_VEG_PGD(PEK, S, K, P, OPATCH1, KI, NVEGTYPE, IO%NGROUND_LAYER )  
!
!
!*       2.1    Cover, soil and orographic fields:
!       ---------------------------------
!
IF (TOP%TTIME%TDATE%MONTH /= NUNDEF) THEN
  IDECADE = 3 * ( TOP%TTIME%TDATE%MONTH - 1 ) + MIN(TOP%TTIME%TDATE%DAY-1,29) / 10 + 1
ELSE
  IDECADE = 1
END IF
!
IF (OPATCH1) THEN
  !
  CALL READ_PGD_TEB_GARDEN_n(OCH_BIO_FLUX, DTCO, DTV, GB, U, &
                             IO, K, G%NDIM, TOP, HPROGRAM,KVERSION,KBUGFIX)
  !
  ALLOCATE(S%XVEGTYPE(KI,NVEGTYPE))
  IF (IO%LPAR) THEN
    S%XVEGTYPE = DTV%XPAR_VEGTYPE
  ELSE
    !classical ecoclimap case
    DO JVEG=1,NVEGTYPE
      CALL AV_PGD(DTCO, S%XVEGTYPE(:,JVEG),TOP%XCOVER ,DTCO%XDATA_VEGTYPE(:,JVEG),'GRD','ARI',TOP%LCOVER)
    END DO
  ENDIF
  DO JVEG=1,NVEGTYPE
    WHERE (PGARDEN==0)
      S%XVEGTYPE(:,JVEG) = 0.
      S%XVEGTYPE(:,1) = 1.
    END WHERE
  ENDDO
  !
  ALLOCATE(S%XPATCH(KI,1),P%XPATCH(KI))
  ALLOCATE(S%XVEGTYPE_PATCH(KI,NVEGTYPE,1),P%XVEGTYPE_PATCH(KI,NVEGTYPE))
  S%XPATCH(:,1) = 1.
  P%XPATCH(:) = S%XPATCH(:,1)
  S%XVEGTYPE_PATCH(:,:,1) = S%XVEGTYPE
  P%XVEGTYPE_PATCH(:,:) = S%XVEGTYPE_PATCH(:,:,1)
  P%NSIZE_P = KI
  ALLOCATE(P%NR_P(KI))
  DO JI = 1,SIZE(P%NR_P)
    P%NR_P(JI) = JI
  ENDDO
  !
  IF (.NOT. IO%LPAR) THEN
    CALL CONVERT_PATCH_ISBA(DTCO, DTV, IO, IDECADE, IDECADE, TOP%XCOVER, TOP%LCOVER,&
                        .FALSE.,'GRD', 1, K, P, PEK, &
                        .TRUE., .FALSE., .FALSE., .FALSE., .FALSE., .FALSE., &
                        PSOILGRID=IO%XSOILGRID  )   
  ELSE
    CALL INIT_FROM_DATA_TEB_VEG_n(DTV, K, P, PEK, IDECADE, .FALSE., .TRUE., .FALSE.,.FALSE.)
  ENDIF
  !
  ALLOCATE(S%XWSN_WR(0,0,1))
  ALLOCATE(S%XRHO_WR(0,0,1))
  ALLOCATE(S%XALB_WR(0,1))
  ALLOCATE(S%XHEA_WR(0,0,1))
  ALLOCATE(S%XAGE_WR(0,0,1))
  ALLOCATE(S%XSG1_WR(0,0,1))
  ALLOCATE(S%XSG2_WR(0,0,1)) 
  ALLOCATE(S%XHIS_WR(0,0,1))
  !
END IF

!
!
!*       2.3    Physiographic data fields from land cover:
!       -----------------------------------------
!
!
!
!
IF (.NOT. IO%LPAR) THEN
  CALL CONVERT_PATCH_ISBA(DTCO, DTV, IO, IDECADE, IDECADE, TOP%XCOVER, TOP%LCOVER,&
                        .FALSE.,'GRD', 1, K, P, PEK, &
                        .FALSE., .TRUE., .FALSE., .FALSE., .FALSE., .FALSE.  )   
ELSE

  CALL INIT_FROM_DATA_TEB_VEG_n(DTV, K, P, PEK, IDECADE, .FALSE., .FALSE., .TRUE.,.FALSE.)

  IF (IO%CISBA=='DIF') CALL INIT_IF_DIF(IO%NGROUND_LAYER, PGARDEN, P)

END IF
!
 CALL INIT_IF_NOVEG(PGARDEN, IO, S, P, PEK)
!
ALLOCATE(K%XVEGTYPE(KI,NVEGTYPE))
K%XVEGTYPE = S%XVEGTYPE
!
ALLOCATE(YSS%XAOSIP(0))
!
 CALL INIT_VEG_PGD_n(YSS, DTV, IO, S, K, K, P, PEK, YAG, KI,    &
     HPROGRAM, 'TOWN  ',ILUOUT, KI, TOP%TTIME%TDATE%MONTH, &
     .FALSE., .FALSE., ZTDEEP_CLI, ZGAMMAT_CLI, &
     .FALSE., ZTHRESHOLD, HINIT, PCO2, PRHOA ) 
!
!-------------------------------------------------------------------------------
!
IF(IO%CISBA=='DIF'.AND.IO%LSOC)THEN
  CALL ABOR1_SFX('INIT_TEB_GARDEN_PGDn: SUBGRID Soil organic matter'//&
 ' effect (LSOC) NOT YET IMPLEMENTED FOR GARDEN')
ELSEIF (IO%CISBA=='3-L'.AND.IO%CKSAT=='EXP') THEN 
  CALL ABOR1_SFX('INIT_TEB_GARDEN_PGDn: topmodel exponential decay not implemented for garden')
ENDIF
!
IF(IO%CKSAT=='SGH' .AND. IO%CISBA/='DIF' .AND. HINIT/='PRE')THEN 
  ZF(:)=MIN(4.0/P%XDG(:,2),XF_DECAY)
  CALL EXP_DECAY_SOIL_FR(IO%CISBA, ZF, P)
ENDIF
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INIT_TEB_GARDEN_PGD_n',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!
END SUBROUTINE INIT_TEB_GARDEN_PGD_n
