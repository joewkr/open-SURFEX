!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!#############################################################
MODULE MODI_INIT_VEG_PGD_n
CONTAINS
SUBROUTINE INIT_VEG_PGD_n (ISSK, DTI, IO, S, K, KK, PK, PEK, AGK, KI, &
                           HPROGRAM, HSURF, KLUOUT, KSIZE, KMONTH,    &
                           ODEEPSOIL, OPHYSDOMC, PTDEEP_CLI, PGAMMAT_CLI,     &
                           OAGRIP, PTHRESHOLD, HINIT, PCO2, PRHOA     )
!#############################################################
!
!!****  *INIT_VEG_PGD_n_n* - routine to initialize ISBA
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
!!      23/07/13     (Decharme) Surface / Water table depth coupling
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURFEX_MPI, ONLY : NRANK
!
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_S_t, ISBA_K_t, ISBA_P_t, ISBA_PE_t
USE MODD_AGRI_n, ONLY : AGRI_t
!
USE MODD_SURF_ATM,       ONLY : LCPL_ARP
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
USE MODD_CSTS,           ONLY : XCPD, XLVTT, XLSTT
USE MODD_SNOW_PAR,       ONLY : XEMISSN
USE MODD_ISBA_PAR,       ONLY : XTAU_ICE
!
USE MODD_SGH_PAR,        ONLY : XICE_DEPH_MAX
!
USE MODE_COTWO,          ONLY : GAULEG
!
USE MODI_SURF_PATCH
USE MODI_GET_1D_MASK
USE MODI_CO2_INIT_n
USE MODI_SUBSCALE_Z0EFF
!
USE MODE_SOIL
!
USE MODI_HEATCAPZ
USE MODI_THRMCONDZ
USE MODI_ABOR1_SFX
USE MODI_DIF_LAYER
USE MODI_DRY_WET_SOIL_ALBEDOS
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              --²-----------------------
!
!
TYPE(SSO_t), INTENT(INOUT) :: ISSK
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTI
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t), INTENT(INOUT) :: S
TYPE(ISBA_K_t), INTENT(INOUT) :: K
TYPE(ISBA_K_t), INTENT(INOUT) :: KK
TYPE(ISBA_P_t), INTENT(INOUT) :: PK
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
TYPE(AGRI_t), INTENT(INOUT) :: AGK
!
INTEGER, INTENT(IN) :: KI
!
 CHARACTER(LEN=6), INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=6), INTENT(IN)  :: HSURF     ! Type of surface
INTEGER, INTENT(IN)  :: KLUOUT
!
INTEGER, INTENT(IN)  :: KSIZE
!
INTEGER, INTENT(IN)  :: KMONTH
!
LOGICAL, INTENT(IN) :: ODEEPSOIL
LOGICAL, INTENT(IN) :: OPHYSDOMC
REAL, DIMENSION(:), INTENT(IN) :: PTDEEP_CLI
REAL, DIMENSION(:), INTENT(IN) :: PGAMMAT_CLI
!
LOGICAL, INTENT(IN) :: OAGRIP
REAL, DIMENSION(:), INTENT(IN) :: PTHRESHOLD
!
 CHARACTER(LEN=3), INTENT(IN) :: HINIT
 !
REAL, DIMENSION(:), INTENT(IN) :: PCO2
REAL, DIMENSION(:), INTENT(IN) :: PRHOA
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: JPATCH  ! loop counter on tiles
INTEGER :: JILU,JP, JMAXLOC    ! loop increment
INTEGER :: JL  ! loop counter on layers
!
INTEGER :: IABC
!
REAL, DIMENSION(SIZE(PCO2))       :: ZCO2  ! CO2 concentration  (kg/kg)
!
INTEGER, DIMENSION(:), ALLOCATABLE :: IR_NATURE_P
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!               Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('INIT_VEG_PGD_n',0,ZHOOK_HANDLE)
!
!------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------
!
!        PART 1: fields that are needed unpacked and packed: defined unpacked
!        -------------------------------------------------------------------
!
!*          Soil hydraulic characteristics:
!           -------------------------------
!
IF (.NOT.ASSOCIATED(K%XMPOTSAT)) THEN
  !
  ALLOCATE(K%XMPOTSAT (KI,IO%NGROUND_LAYER))
  ALLOCATE(K%XBCOEF   (KI,IO%NGROUND_LAYER))
  ALLOCATE(K%XWWILT   (KI,IO%NGROUND_LAYER)) ! wilting point
  ALLOCATE(K%XWFC     (KI,IO%NGROUND_LAYER)) ! field capacity
  ALLOCATE(K%XWSAT    (KI,IO%NGROUND_LAYER)) ! saturation
  !
  DO JL=1,IO%NGROUND_LAYER
    IF (DTI%LDATA_BCOEF) THEN
      K%XBCOEF  (:,JL) = DTI%XPAR_BCOEF  (:,JL)
    ELSE
      K%XBCOEF  (:,JL) = BCOEF_FUNC     (K%XCLAY(:,JL),K%XSAND(:,JL),IO%CPEDOTF)
    ENDIF
    IF (DTI%LDATA_MPOTSAT) THEN
      K%XMPOTSAT(:,JL) = DTI%XPAR_MPOTSAT(:,JL)
    ELSE
      K%XMPOTSAT(:,JL) = MATPOTSAT_FUNC (K%XCLAY(:,JL),K%XSAND(:,JL),IO%CPEDOTF)
    ENDIF
    IF (DTI%LDATA_WSAT) THEN
      K%XWSAT   (:,JL) = DTI%XPAR_WSAT   (:,JL)
    ELSE
      K%XWSAT   (:,JL) = WSAT_FUNC      (K%XCLAY(:,JL),K%XSAND(:,JL),IO%CPEDOTF)
    ENDIF
    IF (DTI%LDATA_WWILT) THEN
      K%XWWILT   (:,JL) = DTI%XPAR_WWILT  (:,JL)
    ELSE
      K%XWWILT  (:,JL) = WWILT_FUNC     (K%XCLAY(:,JL),K%XSAND(:,JL),IO%CPEDOTF)
    ENDIF
  END DO
  IF (DTI%LDATA_BCOEF  ) DEALLOCATE(DTI%XPAR_BCOEF)
  IF (DTI%LDATA_MPOTSAT) DEALLOCATE(DTI%XPAR_MPOTSAT)
  IF (DTI%LDATA_WSAT   ) DEALLOCATE(DTI%XPAR_WSAT)
  IF (DTI%LDATA_WWILT  ) DEALLOCATE(DTI%XPAR_WWILT)
  !
  IF (DTI%LDATA_WFC) THEN
    K%XWFC(:,:) = DTI%XPAR_WFC(:,:)
    DEALLOCATE(DTI%XPAR_WFC)
  ELSEIF (IO%CISBA=='2-L' .OR. IO%CISBA=='3-L') THEN
    !  field capacity at hydraulic conductivity = 0.1mm/day
    K%XWFC(:,:) = WFC_FUNC(K%XCLAY(:,:),K%XSAND(:,:),IO%CPEDOTF)
  ELSE IF (IO%CISBA=='DIF') THEN
    !  field capacity at water potential = 0.33bar
    K%XWFC(:,:) = W33_FUNC(K%XCLAY(:,:),K%XSAND(:,:),IO%CPEDOTF)
  END IF
 !
  IF (IO%CISBA=='2-L' .OR. IO%CISBA=='3-L') THEN
    ALLOCATE(K%XCGSAT (KI))
    ALLOCATE(K%XC4B   (KI))
    ALLOCATE(K%XACOEF (KI))
    ALLOCATE(K%XPCOEF (KI))
    K%XCGSAT(:)  = CGSAT_FUNC(K%XCLAY(:,1),K%XSAND(:,1))
    K%XC4B  (:)  = C4B_FUNC  (K%XCLAY(:,1))
    K%XACOEF(:)  = ACOEF_FUNC(K%XCLAY(:,1))
    K%XPCOEF(:)  = PCOEF_FUNC(K%XCLAY(:,1))
  ELSE IF (IO%CISBA=='DIF') THEN
    ALLOCATE(K%XCGSAT (0))
    ALLOCATE(K%XC4B   (0))
    ALLOCATE(K%XACOEF (0))
    ALLOCATE(K%XPCOEF (0))
  ENDIF
  !
  IF(IO%CRUNOFF=='SGH')THEN
    !
    ALLOCATE(K%XWD0   (KI,IO%NGROUND_LAYER))
    ALLOCATE(K%XKANISO(KI,IO%NGROUND_LAYER))
    !
    IF(IO%CISBA=='DIF')THEN
      K%XWD0(:,:) = WFC_FUNC(K%XCLAY(:,:),K%XSAND(:,:),IO%CPEDOTF)
    ELSE
      K%XWD0(:,:) = K%XWWILT(:,:)
    ENDIF
    K%XKANISO(:,:) = ANISO_FUNC(K%XCLAY(:,:))
    !
  ELSE
    !
    ALLOCATE(K%XWD0   (0,0))
    ALLOCATE(K%XKANISO(0,0))
    !
  ENDIF
  !
  IF (IO%CSCOND=='PL98'.OR.IO%CISBA=='DIF') THEN
    ALLOCATE(K%XHCAPSOIL(KI,IO%NGROUND_LAYER))
    ALLOCATE(K%XCONDDRY (KI,IO%NGROUND_LAYER))
    ALLOCATE(K%XCONDSLD (KI,IO%NGROUND_LAYER))
    !
    CALL HEATCAPZ(K%XSAND,K%XHCAPSOIL)
    CALL THRMCONDZ(K%XSAND,K%XWSAT,K%XCONDDRY,K%XCONDSLD)
  ELSE
    ALLOCATE(K%XHCAPSOIL(0,0))
    ALLOCATE(K%XCONDDRY (0,0))
    ALLOCATE(K%XCONDSLD (0,0))
  END IF
  !
ENDIF
!
!CSCOND used in soil.F90 and soildif.F90
!
IF (IO%CSCOND=='NP89'.AND.IO%CISBA=='DIF') THEN
   WRITE(KLUOUT,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
   WRITE(KLUOUT,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
   WRITE(KLUOUT,*)'IF CISBA=DIF, CSCOND=NP89 is not available'
   WRITE(KLUOUT,*)'because not physic. CSCOND is put to PL98 '
   WRITE(KLUOUT,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
   WRITE(KLUOUT,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
ENDIF
!
!CPSURF used in drag.F90
!CPL_ARP used in drag.F90 and e_budget.F90
IF(IO%CCPSURF=='DRY'.AND.LCPL_ARP) THEN
  CALL ABOR1_SFX('CCPSURF=DRY must not be used with LCPL_ARP')
ENDIF
!
!------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------
!
!        PART 2: fields that are needed only packed: defined packed directly
!        -------------------------------------------------------------------
!
!        PART 2: A: fields that don't depend on patches: KK, AGK, ISSK
!        -------------------------------------------------------------
!
!*       2.A.1.    Miscellaneous fields for ISBA:
!        ----------------------------------------
!
!* default value for:
! lateral water flux, deep soil temperature climatology and its relaxation time-scale
!
!these arrays are used only packed: we define them directly packed
ALLOCATE(KK%XTDEEP (KSIZE))
ALLOCATE(KK%XGAMMAT(KSIZE))
KK%XTDEEP (:) = XUNDEF
KK%XGAMMAT(:) = XUNDEF
!
IF (ODEEPSOIL) THEN
  DO JILU = 1, KSIZE
    KK%XTDEEP (JILU) = PTDEEP_CLI (KMONTH)
    KK%XGAMMAT(JILU) = 1. / PGAMMAT_CLI(KMONTH)
  END DO
  !
  WRITE(KLUOUT,*)' LDEEPSOIL = ',ODEEPSOIL,' LPHYSDOMC = ',OPHYSDOMC
  WRITE(KLUOUT,*)' XTDEEP    = ',MINVAL(KK%XTDEEP(:)) ,MAXVAL(KK%XTDEEP(:))
  WRITE(KLUOUT,*)' XGAMMAT   = ',MINVAL(KK%XGAMMAT(:)),MAXVAL(KK%XGAMMAT(:))
ENDIF
!
!
!*         2.A.2. Initialize hydrology
!          ---------------------------
!
IF (IO%CISBA == 'DIF') THEN
  !
  ALLOCATE(KK%XFWTD(KSIZE))
  ALLOCATE(KK%XWTD (KSIZE))
  KK%XFWTD(:) = 0.0
  KK%XWTD (:) = XUNDEF
  !
ELSE
  !
  ALLOCATE(KK%XFWTD(0))
  ALLOCATE(KK%XWTD (0))
  !
ENDIF
!
!
!*         Physiographic Radiative fields:
!               ------------------------------
!
!
!*        2.A.3. dry and wet bare soil albedos
!         ------------------------------------
!
ALLOCATE(KK%XALBNIR_DRY  (KSIZE))
ALLOCATE(KK%XALBVIS_DRY  (KSIZE))
ALLOCATE(KK%XALBUV_DRY   (KSIZE))
ALLOCATE(KK%XALBNIR_WET  (KSIZE))
ALLOCATE(KK%XALBVIS_WET  (KSIZE))
ALLOCATE(KK%XALBUV_WET   (KSIZE))
!
 CALL DRY_WET_SOIL_ALBEDOS(KK )
!

!
!*       2.A.4. Irrigation
!        -----------------
!
IF (OAGRIP) THEN
  !
  ALLOCATE(AGK%NIRRINUM     (KSIZE))
  ALLOCATE(AGK%LIRRIDAY     (KSIZE))
  ALLOCATE(AGK%LIRRIGATE    (KSIZE))
  ALLOCATE(AGK%XTHRESHOLDSPT(KSIZE))
  !
  AGK%NIRRINUM (:) = 1
  AGK%LIRRIDAY (:) = .FALSE.
  AGK%LIRRIGATE(:) = .FALSE.
  !
  DO JILU = 1, KSIZE
    AGK%XTHRESHOLDSPT(JILU) = PTHRESHOLD(AGK%NIRRINUM(JILU))
  END DO
ELSE
  ALLOCATE(AGK%NIRRINUM     (0))
  ALLOCATE(AGK%LIRRIDAY     (0))
  ALLOCATE(AGK%LIRRIGATE    (0))
  ALLOCATE(AGK%XTHRESHOLDSPT(0))
ENDIF
!
!*       2.A.5. Orographic roughness length
!        ----------------------------------
!
ALLOCATE(ISSK%XZ0EFFIP(KSIZE))
ALLOCATE(ISSK%XZ0EFFIM(KSIZE))
ALLOCATE(ISSK%XZ0EFFJP(KSIZE))
ALLOCATE(ISSK%XZ0EFFJM(KSIZE))
!
ISSK%XZ0EFFIP(:) = XUNDEF
ISSK%XZ0EFFIM(:) = XUNDEF
ISSK%XZ0EFFJP(:) = XUNDEF
ISSK%XZ0EFFJM(:) = XUNDEF
!
IF (SIZE(ISSK%XAOSIP)>0) CALL SUBSCALE_Z0EFF(ISSK,PEK%XZ0,.FALSE.)
!
!-----------------------------------------------------------------------
!
!        PART 2: B: fields that depend on patches: PK, PEK
!        -------------------------------------------------
!
!
!*       2.B.1. Additional fields for ISBA-AGS:
!        --------------------------------------
!
IF(IO%CPHOTO /= 'NON' .AND. HINIT == 'ALL') THEN
  !
  IF (.NOT.ASSOCIATED(S%XABC)) THEN
    IF (IO%LTR_ML) THEN
      IABC = 10
    ELSE
      IABC = 3
    ENDIF
    ALLOCATE(S%XABC(IABC))
    ALLOCATE(S%XPOI(IABC))
    S%XABC(:) = 0.
    S%XPOI(:) = 0.
    CALL GAULEG(0.0,1.0,S%XABC,S%XPOI,IABC)
  ENDIF
  !
  ZCO2(:) = PCO2(:) / PRHOA(:)
  ALLOCATE(PK%XANMAX        (KSIZE))
  ALLOCATE(PK%XFZERO        (KSIZE))
  ALLOCATE(PK%XEPSO         (KSIZE))
  ALLOCATE(PK%XGAMM         (KSIZE))
  ALLOCATE(PK%XQDGAMM       (KSIZE))
  ALLOCATE(PK%XQDGMES       (KSIZE))
  ALLOCATE(PK%XT1GMES       (KSIZE))
  ALLOCATE(PK%XT2GMES       (KSIZE))
  ALLOCATE(PK%XAMAX         (KSIZE))
  ALLOCATE(PK%XQDAMAX       (KSIZE))
  ALLOCATE(PK%XT1AMAX       (KSIZE))
  ALLOCATE(PK%XT2AMAX       (KSIZE))
  ALLOCATE(PK%XAH           (KSIZE))
  ALLOCATE(PK%XBH           (KSIZE))
  ALLOCATE(PK%XTAU_WOOD     (KSIZE))
  ALLOCATE(PK%XINCREASE     (KSIZE,IO%NNBIOMASS))
  ALLOCATE(PK%XTURNOVER     (KSIZE,IO%NNBIOMASS))
  CALL CO2_INIT_n(IO, S, PK, PEK, KSIZE, ZCO2  )
  !
ELSEIF(IO%CPHOTO == 'NON' .AND. IO%LTR_ML) THEN ! Case for MEB
   !
   IF (.NOT.ASSOCIATED(S%XABC)) THEN
     IABC = 10
     ALLOCATE (S%XABC(IABC))
     ALLOCATE (S%XPOI(IABC)) ! Working
     S%XABC(:) = 0.
     S%XPOI(:) = 0.
     CALL GAULEG(0.0,1.0,S%XABC,S%XPOI,IABC)
     DEALLOCATE (S%XPOI)
     ALLOCATE   (S%XPOI(0))
   ENDIF
   !
ELSE
  !
  IF (.NOT.ASSOCIATED(S%XABC)) THEN
    ALLOCATE(S%XABC(0))
    ALLOCATE(S%XPOI(0))
  ENDIF
  !
  ALLOCATE(PK%XANMAX        (0))
  ALLOCATE(PK%XFZERO        (0))
  ALLOCATE(PK%XEPSO         (0))
  ALLOCATE(PK%XGAMM         (0))
  ALLOCATE(PK%XQDGAMM       (0))
  ALLOCATE(PK%XQDGMES       (0))
  ALLOCATE(PK%XT1GMES       (0))
  ALLOCATE(PK%XT2GMES       (0))
  ALLOCATE(PK%XAMAX         (0))
  ALLOCATE(PK%XQDAMAX       (0))
  ALLOCATE(PK%XT1AMAX       (0))
  ALLOCATE(PK%XT2AMAX       (0))
  ALLOCATE(PK%XAH           (0))
  ALLOCATE(PK%XBH           (0))
  ALLOCATE(PK%XTAU_WOOD     (0))
  ALLOCATE(PK%XINCREASE     (0,0))
  ALLOCATE(PK%XTURNOVER     (0,0))
  !
END IF
!
!
!*          2.B.2. Soil hydraulic characteristics (rest) :
!           --------------------------------------------
!
!
ALLOCATE(PK%XCONDSAT (KSIZE,IO%NGROUND_LAYER))
ALLOCATE(PK%XTAUICE  (KSIZE))
!
IF (DTI%LDATA_CONDSAT) THEN
  PK%XCONDSAT(:,:) = DTI%XPAR_CONDSAT(:,:)
ELSE
  DO JL=1,IO%NGROUND_LAYER
    PK%XCONDSAT(:,JL) = HYDCONDSAT_FUNC(KK%XCLAY(:,JL),KK%XSAND(:,JL),IO%CPEDOTF)
  END DO
ENDIF
PK%XTAUICE(:) = XTAU_ICE
!
IF (IO%CISBA=='2-L' .OR. IO%CISBA=='3-L') THEN
  !
  ALLOCATE(PK%XC1SAT (KSIZE))
  ALLOCATE(PK%XC2REF (KSIZE))
  ALLOCATE(PK%XC3    (KSIZE,2))
  ALLOCATE(PK%XC4REF (KSIZE))
  PK%XC1SAT(:) = C1SAT_FUNC(KK%XCLAY(:,1))
  PK%XC2REF(:) = C2REF_FUNC(KK%XCLAY(:,1))
  PK%XC3 (:,1) = C3_FUNC   (KK%XCLAY(:,1))
  PK%XC3 (:,2) = C3_FUNC   (KK%XCLAY(:,2))
  !
  PK%XC4REF(:) = C4REF_FUNC(KK%XCLAY(:,1),KK%XSAND(:,1),PK%XDG(:,2), &
                                        PK%XDG(:,IO%NGROUND_LAYER)  )
  !
ELSE IF (IO%CISBA=='DIF') THEN
  !
  ALLOCATE(PK%XC1SAT (0))
  ALLOCATE(PK%XC2REF (0))
  ALLOCATE(PK%XC3    (0,0))
  ALLOCATE(PK%XC4REF (0))
  !
END IF
!
ALLOCATE(PK%XCPS (KSIZE))
ALLOCATE(PK%XLVTT(KSIZE))
ALLOCATE(PK%XLSTT(KSIZE))
PK%XCPS (:) = XCPD
PK%XLVTT(:) = XLVTT
PK%XLSTT(:) = XLSTT
!
!
!*       2.B.3.  Initialize hydrology
!        ----------------------------
!
ALLOCATE(PK%XRUNOFFD (KSIZE))
PK%XRUNOFFD(:)=XUNDEF
!
IF (IO%CISBA == 'DIF') THEN
!
  ALLOCATE(PK%XDZG       (KSIZE,IO%NGROUND_LAYER))
  ALLOCATE(PK%XDZDIF     (KSIZE,IO%NGROUND_LAYER))
  ALLOCATE(PK%XSOILWGHT  (KSIZE,IO%NGROUND_LAYER))
  CALL DIF_LAYER(KSIZE, IO, PK )
!
ELSE
!
  ALLOCATE(PK%XDZG       (0,0))
  ALLOCATE(PK%XDZDIF     (0,0))
  ALLOCATE(PK%XSOILWGHT  (0,0))
  !
  WHERE(PK%XPATCH(:)>0.0)
    PK%XRUNOFFD(:) = PK%XDG(:,2)
  ENDWHERE
!
ENDIF
!
!Horton (also used by the flooding sheme)
!
ALLOCATE(PK%XKSAT_ICE(KSIZE))
!
IF(IO%CISBA/='DIF')THEN
  PK%XD_ICE   (:) = MIN(PK%XDG(:,2),PK%XD_ICE(:))
  PK%XD_ICE   (:) = MAX(XICE_DEPH_MAX,PK%XD_ICE(:))
  PK%XKSAT_ICE(:) = PK%XCONDSAT(:,1)
ELSE
  PK%XD_ICE   (:) = 0.0
  PK%XKSAT_ICE(:) = 0.0
ENDIF
!
!-------------------------------------------------------------------------------
!
!*        Physiographic Radiative fields:
!         ------------------------------
!

!
!*       2.B.4.  Nitrogen version for isbaAgs
!        ------------------------------------
!
IF (IO%CPHOTO=='NIT' .OR. IO%CPHOTO=='NCB') THEN
  ALLOCATE(PK%XBSLAI_NITRO (KSIZE ))
  WHERE ((PEK%XCE_NITRO(:) * PEK%XCNA_NITRO(:) + PEK%XCF_NITRO (:)) /= 0. )
      PK%XBSLAI_NITRO(:) = 1. / (PEK%XCE_NITRO (:)*PEK%XCNA_NITRO(:)+PEK%XCF_NITRO (:))
  ELSEWHERE
      PK%XBSLAI_NITRO(:) = XUNDEF
  ENDWHERE
ELSE
  ALLOCATE(PK%XBSLAI_NITRO (0))
ENDIF
!
IF (LHOOK) CALL DR_HOOK('INIT_VEG_PGD_n',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_VEG_PGD_n
END MODULE MODI_INIT_VEG_PGD_n
