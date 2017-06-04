!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#############################################################
SUBROUTINE INIT_TEB_GREENROOF_PGD_n (DTCO, U, OCH_BIO_FLUX, G, PGREENROOF, TOP, IO, S, K, P, PEK, DTV, GB, &
                                     HPROGRAM, HINIT, OPATCH1, KI, KVERSION, PCO2, PRHOA)
!#############################################################
!
!!****  *INIT_TEB_GREENROOF_PGD_n* - routine to initialize ISBA
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
!!                  11/2013 (B. Decharme) No exp profile with DIF
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
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
USE MODD_DATA_COVER_PAR,       ONLY: NVEGTYPE
USE MODD_SURF_PAR,             ONLY: XUNDEF, NUNDEF

USE MODD_SGH_PAR,              ONLY: XF_DECAY
!
USE MODI_READ_PREP_GREENROOF_SNOW
USE MODI_GET_LUOUT
USE MODI_ALLOCATE_TEB_VEG_PGD
USE MODI_READ_PGD_TEB_GREENROOF_n
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
!              -------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
LOGICAL, INTENT(IN) :: OCH_BIO_FLUX
TYPE(GRID_t), INTENT(INOUT) :: G
REAL, DIMENSION(:), INTENT(IN) :: PGREENROOF
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
 CHARACTER(LEN=6),                   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=3),                   INTENT(IN)  :: HINIT     ! choice of fields to initialize
LOGICAL,                            INTENT(IN)  :: OPATCH1 ! flag to read PGD fields in the file
INTEGER,                            INTENT(IN)  :: KI        ! number of points
INTEGER,                            INTENT(IN)  :: KVERSION  ! version number of the file being read
REAL,             DIMENSION(KI),    INTENT(IN)  :: PCO2        ! CO2 concentration (kg/m3)
REAL,             DIMENSION(KI),    INTENT(IN)  :: PRHOA       ! air density
!
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
TYPE(SSO_t) :: YSS
TYPE(AGRI_t) :: YAG
!
INTEGER           :: JILU     ! loop increment
INTEGER           :: ILUOUT   ! unit of output listing file
!
INTEGER           :: IDECADE  ! decade of simulation
!
INTEGER :: JVEG, JL, JI  ! loop counter on layers
!
REAL, DIMENSION(KI)               :: ZF
REAL, DIMENSION(KI)               :: ZWORK
!
!*       0.3   Soil parameter values for organic matter - from Lawrence and Slater (2008):
!              ----------------------------------------------------------------------------------
!
REAL, PARAMETER   :: ZWSAT_OM      = 0.9       ! Porosity of OM (m3/m3)
REAL, PARAMETER   :: ZCONDSAT_OM   = 2.8E-4    ! Saturated hydraulic conductivity for OM (m/s)
REAL, PARAMETER   :: ZMPOTSAT_OM   = -10.3E-3  ! Saturated matric potential for OM (m)
REAL, PARAMETER   :: ZBCOEF_OM     = 2.7       ! CH78 b-parameter for OM (-)
!
REAL, PARAMETER   :: ZCONDDRY_OM   = 0.05      ! Dry thermal conductivity for OM (W/m/K)
REAL, PARAMETER   :: ZCONDSLD_OM   = 0.25      ! Soil solids thermal conductivity for OM (W/m/K)
REAL, PARAMETER   :: ZHCAPSOIL_OM  = 2.5E+6    ! Soil heat capacity for OM
!
REAL, PARAMETER   :: ZMPOT_WWILT   = -150.     ! Matric potential at wilting point (m)
REAL, PARAMETER   :: ZHYDCOND_WFC  = 1.157E-9  ! Hydraulic conductivity at field capacity (m/s)
!
REAL, DIMENSION(0) :: ZTDEEP_CLI, ZGAMMAT_CLI, ZTHRESHOLD
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!               Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('INIT_TEB_GREENROOF_PGD_n',0,ZHOOK_HANDLE)
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
CALL SSO_INIT(YSS)
!-------------------------------------------------------------------------------
!
!*       1.     Reading of snow configuration:
!               ------------------------------
!
!* initialization of snow scheme (TSNOW defined in MODD_TEB_GREENROOF_n)
!
IF (HINIT=='PRE') THEN
   CALL READ_PREP_GREENROOF_SNOW(HPROGRAM,PEK%TSNOW%SCHEME,PEK%TSNOW%NLAYER)
!
   IF (PEK%TSNOW%SCHEME.NE.'3-L' .AND. PEK%TSNOW%SCHEME.NE.'CRO' .AND. IO%CISBA=='DIF') THEN
    CALL ABOR1_SFX("INIT_TEB_GREENROOF_n: WITH CISBA_GR = DIF, CSNOW MUST BE 3-L OR CRO")
  ENDIF
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       2.     Physiographic fields
!               --------------------
!
!
!*       2.1    Cover, soil and orographic fields:
!               ---------------------------------
!
 CALL ALLOCATE_TEB_VEG_PGD(PEK, S, K, P, OPATCH1, KI, NVEGTYPE, IO%NGROUND_LAYER )
!
IF (TOP%TTIME%TDATE%MONTH /= NUNDEF) THEN
  IDECADE = 3 * ( TOP%TTIME%TDATE%MONTH - 1 ) + MIN(TOP%TTIME%TDATE%DAY-1,29) / 10 + 1
ELSE
  IDECADE = 1
END IF
!
IF (OPATCH1) THEN

  CALL READ_PGD_TEB_GREENROOF_n(OCH_BIO_FLUX, DTCO, DTV, GB, U, &
                                IO, S, K, G%NDIM, HPROGRAM,KVERSION)
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
    WHERE (PGREENROOF==0)
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
!*       2.2    Physiographic data fields from land cover:
!               -----------------------------------------
!
IF (.NOT. IO%LPAR) THEN
  CALL CONVERT_PATCH_ISBA(DTCO, DTV, IO, IDECADE, IDECADE, TOP%XCOVER, TOP%LCOVER,&
                        .FALSE.,'GRD', 1, K, P, PEK, &
                        .FALSE., .TRUE., .FALSE., .FALSE., .FALSE., .FALSE.  )   
ELSE

  CALL INIT_FROM_DATA_TEB_VEG_n(DTV, K, P, PEK, IDECADE, .FALSE., .FALSE., .TRUE.,.FALSE.)

  IF (IO%CISBA=='DIF') CALL INIT_IF_DIF(IO%NGROUND_LAYER, PGREENROOF, P)

END IF
!
 CALL INIT_IF_NOVEG(PGREENROOF, IO, S, P, PEK)
!
ALLOCATE(K%XVEGTYPE(KI,NVEGTYPE))
K%XVEGTYPE = S%XVEGTYPE
!
ALLOCATE(YSS%XAOSIP(0))
!
 CALL INIT_VEG_PGD_n(YSS, DTV, IO, S, K, K, P, PEK, YAG, KI,                     &
                      HPROGRAM, 'TOWN  ',ILUOUT, KI, TOP%TTIME%TDATE%MONTH, &
                      .FALSE., .FALSE., ZTDEEP_CLI, ZGAMMAT_CLI,            &
                      .FALSE., ZTHRESHOLD, HINIT, PCO2, PRHOA  )
!
!-------------------------------------------------------------------------------
!
IF (OPATCH1) THEN
  !
  !*       5.1     Soil thermal characteristics for greenroofs:
  !               ----------------------------------------------
  !
  ! WARNING: must be done before soil hydraulic characteristics (because of WSAT)
  ! Estimation of WSAT_MI for use in HEATCAPZ and THRMCONDZ for mineral fraction
  ! and allow weighted combination with regard to OM & no-OM fractions:
  !
  IF (IO%CSCOND=='PL98' .OR. IO%CISBA=='DIF') THEN
    DO JL=1,IO%NGROUND_LAYER
      K%XHCAPSOIL(:,JL) = S%XSOC(:,JL) * ZHCAPSOIL_OM + (1-S%XSOC(:,JL)) * K%XHCAPSOIL(:,JL)  
      K%XCONDDRY (:,JL) = (ZCONDDRY_OM * K%XCONDDRY(:,JL)) / &
                        ( S%XSOC(:,JL) * K%XCONDDRY(:,JL) + (1-S%XSOC(:,JL)) * ZCONDDRY_OM )
      K%XCONDSLD (:,JL) = (ZCONDSLD_OM * K%XCONDSLD(:,JL)) / &
                        ( S%XSOC(:,JL) * K%XCONDSLD(:,JL) + (1-S%XSOC(:,JL)) * ZCONDSLD_OM )
    ENDDO
  END IF
  !
  ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Validation case : experimental values for Nancy 2011 case
  ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Substrate layer
  DO JL=1,4
    K%XCONDDRY (:,JL) = 0.15
    K%XHCAPSOIL(:,JL) = 1342000.
  ENDDO
  ! Drainage layer
  DO JL=5,6
    K%XCONDDRY (:,JL) = 0.09
    K%XHCAPSOIL(:,JL) = 331500.
  ENDDO
  ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
ENDIF
!
!*       5.2     Soil thermal characteristics:
!               --------------------------------
!
DO JL=1,IO%NGROUND_LAYER
  P%XCONDSAT(:,JL) = S%XSOC(:,JL)* ZCONDSAT_OM + (1-S%XSOC(:,JL)) * P%XCONDSAT(:,JL)
END DO
!
!
IF (OPATCH1) THEN
  !
  ! Note that if ISBA/=DIF, always CDIF = 'BC' and CPEDOTF = 'CH78'
  DO JL=1,IO%NGROUND_LAYER
    K%XBCOEF  (:,JL) = S%XSOC(:,JL) * ZBCOEF_OM   + (1-S%XSOC(:,JL)) * K%XBCOEF(:,JL)
    K%XMPOTSAT(:,JL) = S%XSOC(:,JL) * ZMPOTSAT_OM + (1-S%XSOC(:,JL)) * K%XMPOTSAT(:,JL)
  END DO
  !        
  DO JL=1,IO%NGROUND_LAYER
    K%XWSAT (:,JL) =  S%XSOC(:,JL)* ZWSAT_OM +(1-S%XSOC(:,JL))* K%XWSAT(:,JL)
    K%XWWILT(:,JL) = EXP(((LOG(-1*ZMPOT_WWILT)-LOG(-1*K%XMPOTSAT(:,JL)))   &
                    / (-1*K%XBCOEF(:,JL)))+LOG(K%XWSAT(:,JL)))
    K%XWFC  (:,JL) = EXP(((LOG(ZHYDCOND_WFC)-LOG(P%XCONDSAT(:,JL)))      &
                    / (2*K%XBCOEF(:,JL)+3))+LOG(K%XWSAT(:,JL)))
  END DO
  !
  ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Validation case : experimental values for Nancy 2011 case
  ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Substrate layer
  DO JL=1,4
    K%XWSAT   (:,JL) = 0.674     ! Value tested
    K%XMPOTSAT(:,JL) = -0.932    ! Value tested
    K%XBCOEF  (:,JL) = 3.9       ! Value tested
    K%XWWILT  (:,JL) = 0.15      ! from OBS-NANCY
    K%XWFC    (:,JL) = 0.37      ! from OBS-NANCY
  ENDDO
  ! Drainage layer
  DO JL=5,6
    K%XWSAT   (:,JL) = 0.9       ! Value tested
    K%XMPOTSAT(:,JL) = -0.121    ! Value tested
    K%XBCOEF  (:,JL) = 2.7       ! Value tested
    K%XWWILT  (:,JL) = 0.15      ! sert à initialiser le WG ds la couche
    K%XWFC    (:,JL) = 0.37      ! sert à initialiser le WG ds la couche
  ENDDO
  !
ENDIF
!
! Substrate layer
DO JL=1,4
  P%XCONDSAT(:,JL) = 2.162E-3  ! Value tested
ENDDO
! Drainage layer
DO JL=5,6
  P%XCONDSAT(:,JL) = 3.32E-3   ! Value tested
ENDDO
!
!-------------------------------------------------------------------------------
!
!*       6.1    Initialize of the SGH scheme:'
!               ------------------------------
!
IF(IO%CKSAT=='SGH' .AND. IO%CISBA/='DIF' .AND. HINIT/='PRE')THEN 
  ZF(:)=MIN(4.0/P%XDG(:,2),XF_DECAY)
  CALL EXP_DECAY_SOIL_FR(IO%CISBA,  ZF, P)
ENDIF
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INIT_TEB_GREENROOF_PGD_n',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!
END SUBROUTINE INIT_TEB_GREENROOF_PGD_n
