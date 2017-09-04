!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!#############################################################
SUBROUTINE COMPUTE_ISBA_PARAMETERS (DTCO, OREAD_BUDGETC, UG, U, &
                                    IO, DTI, SB, S, IG, K, NK, NIG, NP, NPE,   &
                                    NAG, NISS, ISS, NCHI, CHI, ID, GB, NGB,    &
                                    NDST, SLT, SV, HPROGRAM,HINIT,OLAND_USE,   &
                                    KI,KSV,KSW,HSV,PCO2,PRHOA,                 &
                                    PZENITH,PSW_BANDS,PDIR_ALB,PSCA_ALB,       &
                                    PEMIS,PTSRAD,PTSURF, HTEST             )
!#############################################################
!
!!****  *COMPUTE_ISBA_PARAMETERS_n* - routine to initialize ISBA
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
!!      Modified by B. Decharme  (09/2012): Bug in exponential profile calculation with DIF
!!      F. Bouttier    08/13 : apply random perturbation patterns for ensembles
!!      B. Vincendon   03/14 : bug correction for CISBA=3L and CKSAT=EXP (TOPD coupling)
!!      Modified by B. Decharme  (04/2013): Subsurface runoff if SGH (DIF option only)
!!                                          Delete CTOPREG (never used)
!!                                          Delete NWG_LAYER_TOT, NWG_SIZE
!!                                          water table / Surface coupling
!!      P. Samuelsson  02/14 : MEB
!!      B. Decharme    01/16 : Bug when vegetation veg, z0 and emis are imposed whith interactive vegetation
!!      B. Decharme   10/2016  bug surface/groundwater coupling
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_S_t, ISBA_P_t, ISBA_PE_t, ISBA_K_t, ISBA_NK_t, &
                        ISBA_NP_t, ISBA_NPE_t
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_SFX_GRID_n, ONLY : GRID_t, GRID_NP_t
USE MODD_AGRI_n, ONLY : AGRI_t, AGRI_NP_t
USE MODD_SSO_n, ONLY : SSO_t, SSO_NP_t
USE MODD_CH_ISBA_n, ONLY : CH_ISBA_t, CH_ISBA_NP_t
USE MODD_CANOPY_n, ONLY : CANOPY_t
USE MODD_GR_BIOG_n, ONLY : GR_BIOG_t, GR_BIOG_NP_t
USE MODD_SURFEX_n, ONLY : ISBA_DIAG_t
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_DST_n, ONLY : DST_NP_t, DST_t
USE MODD_SLT_n, ONLY : SLT_t
USE MODD_SV_n, ONLY : SV_t
!
USE MODD_SFX_OASIS,  ONLY : LCPL_LAND, LCPL_FLOOD, LCPL_GW, LCPL_CALVING
!
!
#ifdef TOPD
USE MODD_DUMMY_EXP_PROFILE,ONLY : XC_DEPTH_RATIO
#endif
!
USE MODD_ASSIM, ONLY : CASSIM_ISBA, LASSIM
!
USE MODD_DEEPSOIL,       ONLY : LPHYSDOMC, LDEEPSOIL, XTDEEP_CLI, XGAMMAT_CLI
USE MODD_AGRI,           ONLY : LAGRIP, XTHRESHOLD
!
!
USE MODD_SGH_PAR,        ONLY : NDIMTAB, XICE_DEPH_MAX, XF_DECAY
!
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
USE MODD_SNOW_PAR,       ONLY : XEMISSN
!
USE MODD_TOPD_PAR, ONLY : NUNIT
USE MODD_TOPODYN, ONLY : NNCAT, NMESHT
!
USE MODE_RANDOM
!
USE MODI_GET_Z0REL
USE MODI_GET_LUOUT
USE MODI_ABOR1_SFX
USE MODI_INIT_IO_SURF_n
USE MODI_ALLOCATE_PHYSIO
USE MODI_INIT_ISBA_MIXPAR
USE MODI_CONVERT_PATCH_ISBA
USE MODI_INIT_VEG_PGD_n
USE MODI_INIT_TOP
USE MODI_EXP_DECAY_SOIL_FR
USE MODI_CARBON_INIT
USE MODI_SOILTEMP_ARP_PAR
USE MODI_END_IO_SURF_n
!
USE MODI_MAKE_CHOICE_ARRAY
USE MODI_READ_SURF
USE MODI_READ_ISBA_n
USE MODI_INIT_ISBA_LANDUSE
USE MODI_READ_SBL_n
USE MODI_INIT_VEG_n
USE MODI_INIT_CHEMICAL_n
USE MODI_OPEN_NAMELIST
USE MODI_CH_INIT_DEP_ISBA_n
USE MODI_CLOSE_NAMELIST
USE MODI_INIT_DST
USE MODI_INIT_SLT
USE MODI_AVERAGED_ALBEDO_EMIS_ISBA
USE MODI_DIAG_ISBA_INIT_n
USE MODI_INIT_SURF_TOPD
USE MODI_ISBA_SOC_PARAMETERS
USE MODI_PACK_SAME_RANK
!
USE MODI_READ_AND_SEND_MPI
USE MODI_ISBA_TO_TOPD
USE MODI_OPEN_FILE
USE MODI_CLOSE_FILE
USE MODI_FIX_MEB_VEG
USE MODI_AV_PGD
USE MODI_SURF_PATCH
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
LOGICAL, INTENT(IN) :: OREAD_BUDGETC
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTI
TYPE(CANOPY_t), INTENT(INOUT) :: SB
TYPE(ISBA_S_t), INTENT(INOUT) :: S
TYPE(GRID_t), INTENT(INOUT) :: IG
TYPE(ISBA_K_t), INTENT(INOUT) :: K
TYPE(ISBA_NK_t), INTENT(INOUT) :: NK
TYPE(GRID_NP_t), INTENT(INOUT) :: NIG
TYPE(ISBA_NP_t), INTENT(INOUT) :: NP
TYPE(ISBA_NPE_t), INTENT(INOUT) :: NPE
TYPE(AGRI_NP_t), INTENT(INOUT) :: NAG
TYPE(SSO_NP_t), INTENT(INOUT) :: NISS
TYPE(SSO_t), INTENT(INOUT) :: ISS
TYPE(CH_ISBA_NP_t), INTENT(INOUT) :: NCHI
TYPE(CH_ISBA_t), INTENT(INOUT) :: CHI
TYPE(ISBA_DIAG_t), INTENT(INOUT) :: ID
TYPE(GR_BIOG_t), INTENT(INOUT) :: GB
TYPE(GR_BIOG_NP_t), INTENT(INOUT) :: NGB
!
TYPE(DST_NP_t), INTENT(INOUT) :: NDST
TYPE(SLT_t), INTENT(INOUT) :: SLT
TYPE(SV_t), INTENT(INOUT) :: SV
!
 CHARACTER(LEN=6),                INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=3),                INTENT(IN)  :: HINIT     ! choice of fields to initialize
LOGICAL,                          INTENT(IN)  :: OLAND_USE !
INTEGER,                          INTENT(IN)  :: KI        ! number of points
INTEGER,                          INTENT(IN)  :: KSV       ! number of scalars
INTEGER,                          INTENT(IN)  :: KSW       ! number of short-wave spectral bands
 CHARACTER(LEN=6), DIMENSION(KSV),INTENT(IN)  :: HSV       ! name of all scalar variables
REAL,             DIMENSION(KI),  INTENT(IN)  :: PCO2      ! CO2 concentration (kg/m3)
REAL,             DIMENSION(KI),  INTENT(IN)  :: PRHOA     ! air density
REAL,             DIMENSION(KI),  INTENT(IN)  :: PZENITH   ! solar zenithal angle
REAL,             DIMENSION(KSW), INTENT(IN)  :: PSW_BANDS ! middle wavelength of each band
REAL,             DIMENSION(KI,KSW),INTENT(OUT) :: PDIR_ALB  ! direct albedo for each band
REAL,             DIMENSION(KI,KSW),INTENT(OUT) :: PSCA_ALB  ! diffuse albedo for each band
REAL,             DIMENSION(KI),  INTENT(OUT) :: PEMIS     ! emissivity
REAL,             DIMENSION(KI),  INTENT(OUT) :: PTSRAD    ! radiative temperature
REAL,             DIMENSION(KI),  INTENT(OUT) :: PTSURF    ! surface effective temperature         (K)
!
 CHARACTER(LEN=2),                 INTENT(IN)  :: HTEST       ! must be equal to 'OK'
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
TYPE(GRID_t), POINTER :: GK
TYPE(ISBA_P_t), POINTER :: PK
TYPE(ISBA_K_t), POINTER :: KK
TYPE(ISBA_PE_t), POINTER :: PEK
TYPE(AGRI_t), POINTER :: AGK
TYPE(SSO_t), POINTER :: ISSK
TYPE(DST_t), POINTER :: DSTK
!
REAL, DIMENSION(U%NDIM_FULL)   :: ZF_PARAM, ZC_DEPTH_RATIO
!
REAL, DIMENSION(KI)     :: ZTSRAD_NAT !radiative temperature
REAL, DIMENSION(KI)     :: ZTSURF_NAT !effective temperature
REAL, DIMENSION(KI)     :: ZM
!
REAL, DIMENSION(KI)  :: ZWG1 ! work array for surface water content
REAL, DIMENSION(KI,IO%NPATCH)  :: ZTG1 ! work array for surface temperature
REAL, DIMENSION(KI,IO%NPATCH)  :: ZF
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZWORK
REAL, DIMENSION(:,:), ALLOCATABLE :: ZDG_SOIL, ZDG_SOIL_P
REAL, DIMENSION(:), ALLOCATABLE :: ZSUM_PATCH
!
INTEGER :: ICH     ! unit of input chemistry file
INTEGER           :: JI, JL     ! loop increment
INTEGER           :: ILUOUT   ! unit of output listing file
INTEGER           :: IRESP   ! return code
INTEGER           :: IDECADE, IDECADE2  ! decade of simulation
INTEGER           :: JP  ! loop counter on tiles
INTEGER           :: ISIZE_LMEB_PATCH  ! Number of patches with MEB=true
!
LOGICAL :: GDIM, GCAS1, GCAS2, GCAS3
INTEGER :: JVEG, IVERSION, IBUGFIX, IMASK, JMAXLOC
!
 CHARACTER(LEN=4)  :: YLVL
 CHARACTER(LEN=12) :: YRECFM
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!               Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('COMPUTE_ISBA_PARAMETERS',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('COMPUTE_ISBA_PARAMETERS: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF
!
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
!
!    PART 1 : Arrays of vegtypes & patches
!    -------------------------------------
!
! We need XVEGTYPE, XPATCH and XVEGTYPE_PATCH with dimension "PATCH" for some
! cases: initialized here
!
! Vegtypes first
ALLOCATE(S%XVEGTYPE(KI,NVEGTYPE))
IF (DTI%LDATA_VEGTYPE) THEN
  S%XVEGTYPE = DTI%XPAR_VEGTYPE
ELSE
  !classical ecoclimap case
  DO JVEG=1,NVEGTYPE
    CALL AV_PGD(DTCO, S%XVEGTYPE(:,JVEG),S%XCOVER ,DTCO%XDATA_VEGTYPE(:,JVEG),'NAT','ARI',S%LCOVER)
  END DO
ENDIF
!
! patches come from vegtypes
ALLOCATE(S%XPATCH(KI,IO%NPATCH))
ALLOCATE(S%XVEGTYPE_PATCH(KI,NVEGTYPE,IO%NPATCH))
 CALL SURF_PATCH(IO%NPATCH,S%XVEGTYPE,S%XPATCH,S%XVEGTYPE_PATCH)
!
! removing little fractions of patches must be done of the XPATCH with dimension
! "PATCH"
IF (IO%XRM_PATCH/=0.) THEN
  !
  WRITE(ILUOUT,*) " REMOVE PATCH below 5 % add to dominant patch "
  ! remove small fraction of PATCHES and add to MAIN PATCH
  DO JI = 1,KI
    !1) find most present patch maximum value
    JMAXLOC = MAXVAL(MAXLOC(S%XPATCH(JI,:)))
    !2) FIND small value of cover
    DO JP = 1,IO%NPATCH
      IF ( S%XPATCH(JI,JP)<IO%XRM_PATCH ) THEN
        S%XPATCH(JI,JMAXLOC) = S%XPATCH(JI,JMAXLOC) + S%XPATCH(JI,JP)
        S%XPATCH(JI,JP) = 0.0
       ENDIF
    ENDDO
  ENDDO
  !
ENDIF
!
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
!
!    PART 2 : Things depending only on options and / or needed first
!    --------------------------------------------------------------

!*       Physiographic data fields from land cover:
!         -----------------------------------------
!
IF (S%TTIME%TDATE%MONTH /= NUNDEF) THEN
  IDECADE = 3 * ( S%TTIME%TDATE%MONTH - 1 ) + MIN(S%TTIME%TDATE%DAY-1,29) / 10 + 1
ELSE
  IDECADE = 1
END IF
!
IDECADE2 = IDECADE
!
! concern DATA_ISBA, so no dependence on patches
 CALL INIT_ISBA_MIXPAR(DTCO, DTI, IG%NDIM, IO, IDECADE, IDECADE2, S%XCOVER, S%LCOVER, 'NAT')
!
ISIZE_LMEB_PATCH=COUNT(IO%LMEB_PATCH(:))
IF (ISIZE_LMEB_PATCH>0)  THEN
  CALL FIX_MEB_VEG(DTI, IG%NDIM, IO%LMEB_PATCH, IO%NPATCH)
ENDIF
!
!
!*       Soil carbon
!        -----------
!
IF (HINIT == 'ALL' .AND. IO%CRESPSL=='CNT' .AND. IO%CPHOTO == 'NCB') CALL CARBON_INIT
!
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
!
!    PART 3 : Loop on patches for general initialization
!    --------------------------------------------------
!
! loop on patches
DO JP = 1, IO%NPATCH
  !
  KK => NK%AL(JP)
  PK => NP%AL(JP)
  PEK => NPE%AL(JP)
  AGK => NAG%AL(JP)
  ISSK => NISS%AL(JP)
  !
  ! dimension of the patch
  PK%NSIZE_P = COUNT(S%XPATCH(:,JP) > 0.0)
  !
  ! mask of the patch in tile nature
  ALLOCATE(PK%NR_P    (PK%NSIZE_P))
  CALL GET_1D_MASK(PK%NSIZE_P, KI, S%XPATCH(:,JP), PK%NR_P)
  !
  ! the array of vegtypes, patches and vegtypes by patches reduced on this patches
  ALLOCATE(KK%XVEGTYPE(PK%NSIZE_P,NVEGTYPE))
  CALL PACK_SAME_RANK(PK%NR_P,S%XVEGTYPE,KK%XVEGTYPE)
  !
  ALLOCATE(PK%XPATCH(PK%NSIZE_P))
  ALLOCATE(PK%XVEGTYPE_PATCH (PK%NSIZE_P,NVEGTYPE))
  CALL PACK_SAME_RANK(PK%NR_P,S%XPATCH(:,JP),PK%XPATCH)
  CALL PACK_SAME_RANK(PK%NR_P,S%XVEGTYPE_PATCH(:,:,JP),PK%XVEGTYPE_PATCH)
  !
  !
  ! soon needed packed fields
  !
  IF (IO%LPERM) THEN
    ALLOCATE(KK%XPERM(PK%NSIZE_P))
    CALL PACK_SAME_RANK(PK%NR_P, K%XPERM, KK%XPERM)
  ELSE
    ALLOCATE(KK%XPERM(0))
  ENDIF
  !
  !
  ALLOCATE(KK%XSAND(PK%NSIZE_P,IO%NGROUND_LAYER))
  ALLOCATE(KK%XCLAY(PK%NSIZE_P,IO%NGROUND_LAYER))
  !
  ALLOCATE(ISSK%XAOSIP(PK%NSIZE_P))
  ALLOCATE(ISSK%XAOSIM(PK%NSIZE_P))
  ALLOCATE(ISSK%XAOSJP(PK%NSIZE_P))
  ALLOCATE(ISSK%XAOSJM(PK%NSIZE_P))
  ALLOCATE(ISSK%XHO2IP(PK%NSIZE_P))
  ALLOCATE(ISSK%XHO2IM(PK%NSIZE_P))
  ALLOCATE(ISSK%XHO2JP(PK%NSIZE_P))
  ALLOCATE(ISSK%XHO2JM(PK%NSIZE_P))
  !
  !
  CALL PACK_SAME_RANK(PK%NR_P, K%XSAND, KK%XSAND)
  CALL PACK_SAME_RANK(PK%NR_P, K%XCLAY, KK%XCLAY)
  !
  CALL PACK_SAME_RANK(PK%NR_P,ISS%XAOSIP,ISSK%XAOSIP)
  CALL PACK_SAME_RANK(PK%NR_P,ISS%XAOSIM,ISSK%XAOSIM)
  CALL PACK_SAME_RANK(PK%NR_P,ISS%XAOSJP,ISSK%XAOSJP)
  CALL PACK_SAME_RANK(PK%NR_P,ISS%XAOSJM,ISSK%XAOSJM)
  CALL PACK_SAME_RANK(PK%NR_P,ISS%XHO2IP,ISSK%XHO2IP)
  CALL PACK_SAME_RANK(PK%NR_P,ISS%XHO2IM,ISSK%XHO2IM)
  CALL PACK_SAME_RANK(PK%NR_P,ISS%XHO2JP,ISSK%XHO2JP)
  CALL PACK_SAME_RANK(PK%NR_P,ISS%XHO2JM,ISSK%XHO2JM)
  !
  !
  !*       2.5    Physiographic fields
  !               --------------------
  !
  CALL ALLOCATE_PHYSIO(IO, KK, PK, PEK, NVEGTYPE  )
  !
  CALL CONVERT_PATCH_ISBA(DTCO, DTI, IO, IDECADE, IDECADE2, S%XCOVER, S%LCOVER, &
                          LAGRIP, 'NAT', JP, KK, PK, PEK, &
                          .TRUE., .TRUE., .TRUE., .TRUE., .FALSE., .FALSE., &
                          PSOILGRID=IO%XSOILGRID, PPERM=KK%XPERM  )
  !
  !-------------------------------------------------------------------------------
  !
  ! in init_veg_pgd_n, things needed also by garden and greenroof
  CALL INIT_VEG_PGD_n(ISSK, DTI, IO, S, K, KK, PK, PEK, AGK, KI,  &
                      HPROGRAM, 'NATURE', ILUOUT, PK%NSIZE_P, S%TTIME%TDATE%MONTH, &
                      LDEEPSOIL, LPHYSDOMC, XTDEEP_CLI, XGAMMAT_CLI,   &
                      LAGRIP, XTHRESHOLD, HINIT, PCO2, PRHOA        )
  !
  !-------------------------------------------------------------------------------
  !
  ! Other fields needed to be initialized for isba only
  !
  !Rainfall spatial distribution
  !CRAIN used in HYDRO_VEG and HYDRO_SGH and VEG_SGH_UPDATE
  IF(IO%CRAIN=='SGH')THEN
    ALLOCATE(KK%XMUF(PK%NSIZE_P))
    KK%XMUF(:)=0.0
  ELSE
    ALLOCATE(KK%XMUF(0))
  ENDIF
  !
  ALLOCATE(KK%XFSAT(PK%NSIZE_P))
  KK%XFSAT(:) = 0.0
  !
  ! * Initialize flood scheme :
  !
  ALLOCATE(KK%XFFLOOD (PK%NSIZE_P))
  ALLOCATE(KK%XPIFLOOD(PK%NSIZE_P))
  ALLOCATE(KK%XFF     (PK%NSIZE_P))
  ALLOCATE(KK%XFFG    (PK%NSIZE_P))
  ALLOCATE(KK%XFFV    (PK%NSIZE_P))
  ALLOCATE(KK%XFFROZEN(PK%NSIZE_P))
  ALLOCATE(KK%XALBF   (PK%NSIZE_P))
  ALLOCATE(KK%XEMISF  (PK%NSIZE_P))
  KK%XFFLOOD       = 0.0
  KK%XPIFLOOD      = 0.0
  KK%XFF           = 0.0
  KK%XFFG          = 0.0
  KK%XFFV          = 0.0
  KK%XFFROZEN      = 0.0
  KK%XALBF         = 0.0
  KK%XEMISF        = 0.0
  !
ENDDO
!
IF (DTI%LDATA_CONDSAT) DEALLOCATE(DTI%XPAR_CONDSAT)
!
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
!
!    PART 4 : Initialization not depending on patches
!    ------------------------------------------------
!
! Fields needed also unpacked
!
IF(IO%CRAIN=='SGH')THEN
  ALLOCATE(K%XMUF(KI))
  K%XMUF(:)=0.0
ENDIF
!
!
ALLOCATE(ISS%XZ0REL(KI))
 CALL GET_Z0REL(ISS)
!
!-------------------------------------------------------------------------------
!
!        PART 5:  Initialize Chemical Deposition
!            -----------------------------------
!
!        3.1 Chemical gazes
!            --------------
!
    !* for the time being, chemistry on vegetation works only for
    ! ISBA on nature tile (not for gardens), because subroutine INIT_CHEMICAL_n
    ! contains explicitely modules from ISBAn. It should be cleaned in a future
    ! version.
 CALL INIT_CHEMICAL_n(ILUOUT, KSV, HSV, CHI%SVI, CHI%CCH_NAMES, CHI%CAER_NAMES,  &
                      HDSTNAMES=CHI%CDSTNAMES, HSLTNAMES=CHI%CSLTNAMES        )
!
IF (KSV /= 0) THEN
  !
  IF (CHI%SVI%NBEQ > 0) THEN
    !* for the time being, chemistry deposition on vegetation works only for
    ! ISBA on nature tile (not for gardens), because subroutine CH_INIT_DEP_ISBA_n
    ! contains explicitely modules from ISBAn. It should be cleaned in a future
    ! version.
    CALL OPEN_NAMELIST(HPROGRAM, ICH, HFILE=CHI%CCHEM_SURF_FILE)
    CALL CH_INIT_DEP_ISBA_n(CHI, NCHI, NP, DTCO, IO%NPATCH, S%LCOVER, S%XCOVER, ICH, ILUOUT, KI)
    CALL CLOSE_NAMELIST(HPROGRAM, ICH)
  END IF
  !
  DO JP = 1,IO%NPATCH
    !
    DSTK => NDST%AL(JP)
    !
    IF (CHI%SVI%NDSTEQ >=1) THEN
      !
      ALLOCATE (DSTK%XSFDST (PK%NSIZE_P, CHI%SVI%NDSTEQ))  !Output array
      ALLOCATE (DSTK%XSFDSTM(PK%NSIZE_P, CHI%SVI%NDSTEQ))  !Output array
      DSTK%XSFDST (:,:)  = 0.
      DSTK%XSFDSTM(:,:) = 0.
      CALL INIT_DST(DSTK, U, HPROGRAM, PK%NSIZE_P, PK%NR_P, PK%XVEGTYPE_PATCH)
    ELSE
      ALLOCATE(DSTK%XSFDST (0,0))
      ALLOCATE(DSTK%XSFDSTM(0,0))
    END IF
    !
  ENDDO
  !
  IF (CHI%SVI%NSLTEQ >=1) THEN
    CALL INIT_SLT(SLT, HPROGRAM)
  END IF
  !
ENDIF
!
!-------------------------------------------------------------------------------
!
!        PART 6:  Specific options
!        --------------------------

!6.A. DIF option :
!---------------
!    Anisotropy coeficient for hydraulic conductivity for topmodel drainage (Fan et al. 2006)
!    Soil organic matter effect and/or Exponential decay for DIF option
!    Must be call before INIT_TOP
!
!
IF(IO%CISBA=='DIF' .AND. IO%CKSAT=='SGH') THEN
  !
  WRITE(ILUOUT,*)'THE KSAT EXP PROFILE WITH ISBA-DF IS NOT PHYSIC AND HAS BEEN REMOVED FOR NOW'
  WRITE(ILUOUT,*)'A NEW PHYSICAL APPROACH WILL BE DEVELLOPED ACCOUNTING FOR COMPACTION IN ALL '
  WRITE(ILUOUT,*)'HYDRODYNAMIC PARAMETERS (WSAT, PSISAT, KSAT, B) AND NOT ONLY IN KSAT        '
  CALL ABOR1_SFX('CKSAT=SGH is not physic with ISBA-DF and has been removed for now')
  !
ENDIF
!
IF(IO%CISBA=='DIF' .AND. IO%LSOC)THEN
  !
  IF(.NOT.IO%LSOCP)THEN
    WRITE(ILUOUT,*)'LSOC = T can be activated only if SOC data given in PGD fields'
    CALL ABOR1_SFX('LSOC = T can be activated only if SOC data given in PGD fields')
  ENDIF
  !
  ALLOCATE(S%XFRACSOC(KI,IO%NGROUND_LAYER))
  CALL ISBA_SOC_PARAMETERS(IO%CRUNOFF, S%XSOC, K, NP, S%XFRACSOC, &
                           K%XWSAT, K%XWFC, K%XWWILT, IO%NPATCH )
  !
ELSE
  ALLOCATE(S%XFRACSOC(0,0))
ENDIF
!
!
!6.B. Topmodel
!--------------
!
ZF   (:,:) = XUNDEF
ZM   (:)   = XUNDEF
!
!CRUNOFF used in hydro_sgh and isba_sgh_update
IF( IO%CRUNOFF=='SGH '.AND. HINIT/='PRE' .AND. .NOT.LASSIM ) THEN
  !
  ! Subsurface flow by layer (m/s)
  DO JP = 1,IO%NPATCH
    PK => NP%AL(JP)
    IF(IO%CISBA=='DIF') THEN
      ALLOCATE(PK%XTOPQS(PK%NSIZE_P,IO%NGROUND_LAYER))
      PK%XTOPQS(:,:) = 0.0
    ELSE
      ALLOCATE(PK%XTOPQS(0,0))
    ENDIF
  ENDDO
  !
  ALLOCATE(S%XTAB_FSAT(KI,NDIMTAB))
  ALLOCATE(S%XTAB_WTOP(KI,NDIMTAB))
  ALLOCATE(S%XTAB_QTOP(KI,NDIMTAB))
  S%XTAB_FSAT(:,:) = 0.0
  S%XTAB_WTOP(:,:) = 0.0
  S%XTAB_QTOP(:,:) = 0.0
  !
  WHERE(K%XCLAY(:,1)==XUNDEF.AND.S%XTI_MEAN(:)/=XUNDEF) S%XTI_MEAN(:)=XUNDEF
  CALL INIT_TOP(IO, S, K, NK, NP, ILUOUT, ZM )
  !
ELSE
  !
  DO JP = 1,IO%NPATCH
    PK => NP%AL(JP)
    ALLOCATE(PK%XTOPQS(0,0))
  ENDDO
  !
  ALLOCATE(S%XTAB_FSAT(0,0))
  ALLOCATE(S%XTAB_WTOP(0,0))
  ALLOCATE(S%XTAB_QTOP(0,0))
  !
ENDIF
!
!
!Exponential decay for ISBA-FR option
!CKSAT used in hydro_soil.F90 and soil.F90
IF ( IO%CISBA/='DIF' .AND. HINIT/='PRE' .AND. .NOT.LASSIM ) THEN
  !
  GCAS1 = (IO%CKSAT=='EXP' .AND. IO%CISBA=='3-L')
  GCAS2 = (IO%CKSAT=='SGH')
  GCAS3 = (HPROGRAM/='AROME ' .AND. HPROGRAM/='MESONH ')
  !
  IF ( GCAS1 .OR. GCAS2 ) THEN
    !
    ALLOCATE(S%XF_PARAM (KI))
    S%XF_PARAM(:) = XUNDEF
    !
    IF ( GCAS1 .AND. GCAS3 ) THEN
      !
      !reading of XF_PARAM in external file
      CALL OPEN_FILE('ASCII ',NUNIT,HFILE='carte_f_dc.txt',HFORM='FORMATTED',HACTION='READ ')
      DO JI = 1,U%NDIM_FULL
        READ(NUNIT,*) ZF_PARAM(JI), ZC_DEPTH_RATIO(JI)
      ENDDO
      CALL CLOSE_FILE('ASCII ',NUNIT)
      CALL READ_AND_SEND_MPI(ZF_PARAM,S%XF_PARAM,U%NR_NATURE)
#ifdef TOPD
      IF (.NOT.ALLOCATED(XC_DEPTH_RATIO))  ALLOCATE(XC_DEPTH_RATIO (KI))
      XC_DEPTH_RATIO(:) = XUNDEF
      CALL READ_AND_SEND_MPI(ZC_DEPTH_RATIO,XC_DEPTH_RATIO,U%NR_NATURE)
#endif
      !
    ELSEIF ( GCAS1 ) THEN
      WRITE(ILUOUT,*) "COMPUTE_ISBA_PARAMETERS: WITH CKSAT=EXP, IN NOT OFFLINE "//&
                      "MODE, TOPMODEL FILE FOR F_PARAM IS NOT READ "
    ENDIF
    !
    ! definition of ZF functions of options
    !
    ! Exponential decay factor calculate using soil properties
    ! (eq. 11, Decharme et al., J. Hydrometeor, 2006)
    DO JP = 1,IO%NPATCH
      PK => NP%AL(JP)
      !
      DO JI = 1,PK%NSIZE_P
        IMASK = PK%NR_P(JI)

        IF ( GCAS2 .AND. IO%CRUNOFF=='SGH' .AND. ZM(IMASK)/=XUNDEF ) THEN
          ZF(JI,JP) = (K%XWSAT(IMASK,1)-K%XWD0(IMASK,1)) / ZM(IMASK)
        ELSEIF ( GCAS1 ) THEN
          ZF(JI,JP) = S%XF_PARAM(IMASK)
         ENDIF
      ENDDO
    ENDDO
    !
    DO JP = 1,IO%NPATCH
      PK => NP%AL(JP)
      !
      WHERE ( ZF(1:PK%NSIZE_P,JP)==XUNDEF.AND.PK%XDG(:,2)/=XUNDEF )
        ZF(1:PK%NSIZE_P,JP) = 4.0/PK%XDG(:,2)
      ENDWHERE
      ZF(1:PK%NSIZE_P,JP) = MIN(ZF(1:PK%NSIZE_P,JP),XF_DECAY)
      !
      ZC_DEPTH_RATIO(1:PK%NSIZE_P) = 1.
#ifdef TOPD
      IF (ALLOCATED(XC_DEPTH_RATIO)) THEN
        CALL PACK_SAME_RANK(PK%NR_P,XC_DEPTH_RATIO,ZC_DEPTH_RATIO(1:PK%NSIZE_P))
      ENDIF
#endif
      CALL EXP_DECAY_SOIL_FR(IO%CISBA, ZF(1:PK%NSIZE_P,JP), PK, ZC_DEPTH_RATIO(1:PK%NSIZE_P))
    ENDDO
    !
    IF ( GCAS2 ) THEN
      !
      DO JI = 1,NP%AL(1)%NSIZE_P
        IMASK = NP%AL(1)%NR_P(JI)
        S%XF_PARAM(IMASK) = ZF(JI,1)
      ENDDO
      !
    ENDIF
    !
  ENDIF
  !
ENDIF
!
!
! 6.C. Initialize required coupling fields :
!-------------------------------------------
!
IO%LCPL_RRM = .FALSE.
IO%LFLOOD   = .FALSE.
IO%LWTD     = .FALSE.
!
IF(LCPL_LAND)THEN
!
  IO%LCPL_RRM = .TRUE.
!
  IF(LCPL_GW)THEN
    IO%LWTD = .TRUE.
  ENDIF
!
  ALLOCATE(S%XCPL_DRAIN (KI))
  ALLOCATE(S%XCPL_RUNOFF(KI))
  S%XCPL_DRAIN (:) = 0.0
  S%XCPL_RUNOFF(:) = 0.0
!
  IF(IO%LGLACIER)THEN
     ALLOCATE(S%XCPL_ICEFLUX(KI))
     S%XCPL_ICEFLUX(:) = 0.0
  ELSE
     ALLOCATE(S%XCPL_ICEFLUX(0))
  ENDIF
!
  IF(LCPL_FLOOD)THEN
     IO%LFLOOD = .TRUE.
     ALLOCATE(S%XCPL_EFLOOD(KI))
     ALLOCATE(S%XCPL_PFLOOD(KI))
     ALLOCATE(S%XCPL_IFLOOD(KI))
     S%XCPL_EFLOOD(:)= 0.0
     S%XCPL_PFLOOD(:)= 0.0
     S%XCPL_IFLOOD(:)= 0.0
  ELSE
    ALLOCATE(S%XCPL_EFLOOD(0))
    ALLOCATE(S%XCPL_PFLOOD(0))
    ALLOCATE(S%XCPL_IFLOOD(0))
  ENDIF
!
ELSE
!
  ALLOCATE(S%XCPL_RUNOFF  (0))
  ALLOCATE(S%XCPL_DRAIN   (0))
  ALLOCATE(S%XCPL_ICEFLUX (0))
  ALLOCATE(S%XCPL_EFLOOD  (0))
  ALLOCATE(S%XCPL_PFLOOD  (0))
  ALLOCATE(S%XCPL_IFLOOD  (0))
!
ENDIF
!
!
IF (LCPL_LAND) THEN
  !
  ALLOCATE(K%XFWTD(KI))
  ALLOCATE(K%XWTD (KI))
  K%XFWTD(:) = 0.0
  K%XWTD (:) = XUNDEF
  !
  IF(LCPL_FLOOD)THEN
    ALLOCATE(K%XFFLOOD (KI))
    ALLOCATE(K%XPIFLOOD(KI))
    K%XFFLOOD (:) = 0.0
    K%XPIFLOOD(:) = 0.0
    !
  ELSE
    !
    ALLOCATE(K%XFFLOOD (0))
    ALLOCATE(K%XPIFLOOD(0))
    !
  ENDIF
  !
ELSE
  !
  ALLOCATE(K%XFWTD(0))
  ALLOCATE(K%XWTD (0))
  ALLOCATE(K%XFFLOOD (0))
  ALLOCATE(K%XPIFLOOD(0))
  !
ENDIF
!
! * Check some key :
!
IF(LCPL_CALVING)THEN
   IF(.NOT.IO%LGLACIER)THEN
     CALL ABOR1_SFX('COMPUTE_ISBA_PARAMETERS: LGLACIER MUST BE ACTIVATED IF LCPL_CALVING')
   ENDIF
ENDIF
!
!-------------------------------------------------------------------------------
!
!*   6.D. ISBA time-varying deep force-restore temperature initialization
!    --------------------------------------------------------------------
!
 CALL SOILTEMP_ARP_PAR(IO, HPROGRAM)
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
!        PART 7:  We packed needed fields and free unless ones
!        -----------------------------------------------------
!
!
DO JP = 1,IO%NPATCH
  !
  KK => NK%AL(JP)
  PK => NP%AL(JP)
  ISSK => NISS%AL(JP)
  GK => NIG%AL(JP)
  !
  ALLOCATE(KK%XMPOTSAT(PK%NSIZE_P,IO%NGROUND_LAYER))
  ALLOCATE(KK%XBCOEF  (PK%NSIZE_P,IO%NGROUND_LAYER))
  ! needed to be written as diagnostics, so not free
  ALLOCATE(KK%XWWILT  (PK%NSIZE_P,IO%NGROUND_LAYER))
  ALLOCATE(KK%XWFC    (PK%NSIZE_P,IO%NGROUND_LAYER))
  ALLOCATE(KK%XWSAT   (PK%NSIZE_P,IO%NGROUND_LAYER))
  !
  CALL PACK_SAME_RANK(PK%NR_P,K%XMPOTSAT,KK%XMPOTSAT)
  CALL PACK_SAME_RANK(PK%NR_P,K%XBCOEF,KK%XBCOEF)
  !
  CALL PACK_SAME_RANK(PK%NR_P,K%XWWILT,KK%XWWILT)
  CALL PACK_SAME_RANK(PK%NR_P,K%XWFC,KK%XWFC)
  CALL PACK_SAME_RANK(PK%NR_P,K%XWSAT,KK%XWSAT)
  !
  IF (IO%CISBA=='2-L' .OR. IO%CISBA=='3-L') THEN
    ALLOCATE(KK%XCGSAT(PK%NSIZE_P))
    ALLOCATE(KK%XC4B  (PK%NSIZE_P))
    ALLOCATE(KK%XACOEF(PK%NSIZE_P))
    ALLOCATE(KK%XPCOEF(PK%NSIZE_P))
    CALL PACK_SAME_RANK(PK%NR_P,K%XCGSAT,KK%XCGSAT)
    CALL PACK_SAME_RANK(PK%NR_P,K%XC4B,  KK%XC4B)
    CALL PACK_SAME_RANK(PK%NR_P,K%XACOEF,KK%XACOEF)
    CALL PACK_SAME_RANK(PK%NR_P,K%XPCOEF,KK%XPCOEF)
  ENDIF
  !
  IF (IO%CSCOND=='PL98'.OR.IO%CISBA=='DIF') THEN
    ALLOCATE(KK%XHCAPSOIL(PK%NSIZE_P,IO%NGROUND_LAYER))
    ALLOCATE(KK%XCONDDRY (PK%NSIZE_P,IO%NGROUND_LAYER))
    ALLOCATE(KK%XCONDSLD (PK%NSIZE_P,IO%NGROUND_LAYER))
    CALL PACK_SAME_RANK(PK%NR_P,K%XHCAPSOIL,KK%XHCAPSOIL)
    CALL PACK_SAME_RANK(PK%NR_P,K%XCONDDRY ,KK%XCONDDRY)
    CALL PACK_SAME_RANK(PK%NR_P,K%XCONDSLD ,KK%XCONDSLD)
  ENDIF
  !
  ALLOCATE(KK%XWDRAIN (PK%NSIZE_P))
  ALLOCATE(KK%XRUNOFFB(PK%NSIZE_P))
  CALL PACK_SAME_RANK(PK%NR_P,K%XWDRAIN,KK%XWDRAIN)
  CALL PACK_SAME_RANK(PK%NR_P,K%XRUNOFFB,KK%XRUNOFFB)
  !
  ! needed to be written as diagnostics, so not free
  ALLOCATE(ISSK%XZ0REL    (PK%NSIZE_P))
  ALLOCATE(ISSK%XSSO_SLOPE(PK%NSIZE_P))
  !
  CALL PACK_SAME_RANK(PK%NR_P,ISS%XZ0REL,ISSK%XZ0REL)
  CALL PACK_SAME_RANK(PK%NR_P,ISS%XSSO_SLOPE,ISSK%XSSO_SLOPE)
  !
  ALLOCATE(GK%XLAT(PK%NSIZE_P))
  ALLOCATE(GK%XLON(PK%NSIZE_P))
  !
  CALL PACK_SAME_RANK(PK%NR_P,IG%XLAT,GK%XLAT)
  CALL PACK_SAME_RANK(PK%NR_P,IG%XLON,GK%XLON)
  !
ENDDO
!
! Useledd fields from now on
ISS%XAOSIP => NULL()
ISS%XAOSIM => NULL()
ISS%XAOSJP => NULL()
ISS%XAOSJM => NULL()
ISS%XHO2IP => NULL()
ISS%XHO2IM => NULL()
ISS%XHO2JP => NULL()
ISS%XHO2JM => NULL()
!
K%XMPOTSAT => NULL()
K%XBCOEF   => NULL()
!
K%XCGSAT => NULL()
K%XC4B   => NULL()
K%XACOEF => NULL()
K%XPCOEF => NULL()
!
K%XHCAPSOIL => NULL()
K%XCONDDRY  => NULL()
K%XCONDSLD  => NULL()
!
K%XWDRAIN  => NULL()
K%XRUNOFFB => NULL()
!
!-------------------------------------------------------------------------------
!
!* if only physiographic fields are to be initialized, stop here.
!
IF (HINIT/='ALL' .AND. HINIT/='SOD') THEN
  IF (LHOOK) CALL DR_HOOK('COMPUTE_ISBA_PARAMETERS',1,ZHOOK_HANDLE)
  RETURN
END IF
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
!        PART 8: Reading of prognostic variables
!        ----------------------------------------
!
IF (CASSIM_ISBA=="ENKF ") CALL INIT_RANDOM_SEED()
!
!
CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'NATURE','ISBA  ','READ ')
!
!*      10.     Prognostic and semi-prognostic fields
!               -------------------------------------
!
 CALL READ_ISBA_n(DTCO, IO, S, NP, NPE, K%XCLAY, U, HPROGRAM)
!
IF (HINIT/='ALL') THEN
  CALL END_IO_SURF_n(HPROGRAM)
  IF (LHOOK) CALL DR_HOOK('COMPUTE_ISBA_PARAMETERS',1,ZHOOK_HANDLE)
  RETURN
END IF
!
IF (HINIT=='PRE' .AND. NPE%AL(1)%TSNOW%SCHEME.NE.'3-L' .AND. &
        NPE%AL(1)%TSNOW%SCHEME.NE.'CRO' .AND. IO%CISBA=='DIF') &
   CALL ABOR1_SFX("INIT_ISBAN: WITH CISBA = DIF, CSNOW MUST BE 3-L OR CRO")
!
!
!*       Extrapolation of the prognostic and semi-prognostic fields
!                           LAND USE case
!               -------------------------------------
!
IF (OLAND_USE) THEN
  !
  CALL READ_SURF(HPROGRAM,'VERSION',IVERSION,IRESP)
  CALL READ_SURF(HPROGRAM,'BUG',IBUGFIX,IRESP)
  GDIM = (IVERSION>8 .OR. IVERSION==8 .AND. IBUGFIX>0)
  IF (GDIM) CALL READ_SURF(HPROGRAM,'SPLIT_PATCH',GDIM,IRESP)
  !
  ALLOCATE(ZWORK(KI,IO%NPATCH))
  !
  !* read old patch fraction
  !
  DO JP = 1,IO%NPATCH
    ALLOCATE(NP%AL(JP)%XPATCH_OLD(NP%AL(JP)%NSIZE_P))
  ENDDO
  !
  CALL MAKE_CHOICE_ARRAY(HPROGRAM, IO%NPATCH, GDIM, 'PATCH', ZWORK)
  DO JP = 1,IO%NPATCH
    CALL PACK_SAME_RANK(NP%AL(JP)%NR_P,ZWORK(:,JP),NP%AL(JP)%XPATCH_OLD(:))
  ENDDO
  !
  !* read old soil layer thicknesses (m)
  !
  DO JP = 1,IO%NPATCH
    ALLOCATE(NP%AL(JP)%XDG_OLD(NP%AL(JP)%NSIZE_P,IO%NGROUND_LAYER))
  ENDDO
  !
  DO JL=1,IO%NGROUND_LAYER
    WRITE(YLVL,'(I4)') JL
    YRECFM='OLD_DG'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    CALL MAKE_CHOICE_ARRAY(HPROGRAM, IO%NPATCH, GDIM, YRECFM, ZWORK)
    DO JP = 1,IO%NPATCH
      CALL PACK_SAME_RANK(NP%AL(JP)%NR_P,ZWORK(:,JP),NP%AL(JP)%XDG_OLD(:,JL))
    ENDDO
  END DO
  DEALLOCATE(ZWORK)
  !
   CALL INIT_ISBA_LANDUSE(DTCO, UG, U, IO, NK, NP, NPE, IG%XMESH_SIZE, &
                          HPROGRAM)
END IF
!
!
!*      12.     Canopy air fields:
!               -----------------
!
 CALL READ_SBL_n(DTCO, U, SB, IO%LCANOPY, HPROGRAM, "NATURE")
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
!        PART 9: initialize radiative and physical properties
!        ----------------------------------------------------
!
DO JP=1,IO%NPATCH
  PK => NP%AL(JP)
  KK => NK%AL(JP)
  PEK => NPE%AL(JP)
  !
  ALLOCATE(KK%XDIR_ALB_WITH_SNOW(PK%NSIZE_P,KSW))
  ALLOCATE(KK%XSCA_ALB_WITH_SNOW(PK%NSIZE_P,KSW))
  KK%XDIR_ALB_WITH_SNOW = 0.0
  KK%XSCA_ALB_WITH_SNOW = 0.0
  !
  CALL INIT_VEG_n(IO, KK, PK, PEK, DTI, ID%DM%LSURF_DIAG_ALBEDO, PDIR_ALB, PSCA_ALB, PEMIS, PTSRAD )
  !
  ZWG1(1:PK%NSIZE_P)    = PEK%XWG(:,1)
  ZTG1(1:PK%NSIZE_P,JP) = PEK%XTG(:,1)
  !
  CALL CONVERT_PATCH_ISBA(DTCO, DTI, IO, IDECADE, IDECADE2, S%XCOVER, S%LCOVER,&
                          LAGRIP, 'NAT', JP, KK, PK, PEK, &
                          .FALSE., .FALSE., .FALSE., .FALSE., .TRUE., .FALSE., &
                          PWG1=ZWG1(1:PK%NSIZE_P), PWSAT=KK%XWSAT)
  !
ENDDO
!
!
! Load randomly perturbed fields. Perturbation ratios are saved in case fields are reset later.
IF(IO%LPERTSURF) THEN
  !
  CALL READ_SURF(HPROGRAM,'VERSION',IVERSION,IRESP)
  CALL READ_SURF(HPROGRAM,'BUG',IBUGFIX,IRESP)
  GDIM = (IVERSION>8 .OR. IVERSION==8 .AND. IBUGFIX>0)
  !
  ALLOCATE(ZWORK(KI,IO%NPATCH))
  !
  CALL MAKE_CHOICE_ARRAY(HPROGRAM, IO%NPATCH, GDIM, 'VEG', ZWORK)
  ALLOCATE(S%XPERTVEG(KI))
  S%XPERTVEG(:)=ZWORK(:,1)
!
  CALL MAKE_CHOICE_ARRAY(HPROGRAM, IO%NPATCH, GDIM, 'LAI', ZWORK)
  ALLOCATE(S%XPERTLAI(KI))
  S%XPERTLAI(:)=ZWORK(:,1)
!
  CALL MAKE_CHOICE_ARRAY(HPROGRAM, IO%NPATCH, GDIM, 'CV', ZWORK)
  ALLOCATE(S%XPERTCV(KI))
  S%XPERTCV(:)=ZWORK(:,1)
!
  CALL MAKE_CHOICE_ARRAY(HPROGRAM, IO%NPATCH, GDIM, 'PERTALB', ZWORK)
  ALLOCATE(S%XPERTALB(KI))
  S%XPERTALB(:)=ZWORK(:,1)

  PEK => NPE%AL(1)
  ISSK => NISS%AL(1)

  WHERE(PEK%XALBNIR_VEG (:)/=XUNDEF)  PEK%XALBNIR_VEG(:) = PEK%XALBNIR_VEG (:) *( 1.+ S%XPERTALB(:) )
  WHERE(PEK%XALBVIS_VEG (:)/=XUNDEF)  PEK%XALBVIS_VEG(:) = PEK%XALBVIS_VEG (:) *( 1.+ S%XPERTALB(:) )
  WHERE(PEK%XALBUV_VEG  (:)/=XUNDEF)  PEK%XALBUV_VEG (:) = PEK%XALBUV_VEG  (:) *( 1.+ S%XPERTALB(:) )
  WHERE(PEK%XALBNIR_SOIL(:)/=XUNDEF) PEK%XALBNIR_SOIL(:) = PEK%XALBNIR_SOIL(:) *( 1.+ S%XPERTALB(:) )
  WHERE(PEK%XALBVIS_SOIL(:)/=XUNDEF) PEK%XALBVIS_SOIL(:) = PEK%XALBVIS_SOIL(:) *( 1.+ S%XPERTALB(:) )
  WHERE(PEK%XALBUV_SOIL (:)/=XUNDEF) PEK%XALBUV_SOIL (:) = PEK%XALBUV_SOIL (:) *( 1.+ S%XPERTALB(:) )
!
  CALL MAKE_CHOICE_ARRAY(HPROGRAM, IO%NPATCH, GDIM, 'PERTZ0LAND', ZWORK)
  ALLOCATE(S%XPERTZ0(KI))
  S%XPERTZ0(:)=ZWORK(:,1)
  WHERE(PEK%XZ0(:)/=XUNDEF)      PEK%XZ0(:)        = PEK%XZ0(:)      *( 1.+ S%XPERTZ0(:) )
  WHERE(ISSK%XZ0EFFIP(:)/=XUNDEF) ISSK%XZ0EFFIP(:) = ISSK%XZ0EFFIP(:)*( 1.+ S%XPERTZ0(:) )
  WHERE(ISSK%XZ0EFFIM(:)/=XUNDEF) ISSK%XZ0EFFIM(:) = ISSK%XZ0EFFIM(:)*( 1.+ S%XPERTZ0(:) )
  WHERE(ISSK%XZ0EFFJP(:)/=XUNDEF) ISSK%XZ0EFFJP(:) = ISSK%XZ0EFFJP(:)*( 1.+ S%XPERTZ0(:) )
  WHERE(ISSK%XZ0EFFJM(:)/=XUNDEF) ISSK%XZ0EFFJM(:) = ISSK%XZ0EFFJM(:)*( 1.+ S%XPERTZ0(:) )
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       14.    Output radiative fields
!               -----------------------
!
ALLOCATE(S%XEMIS_NAT   (KI))
S%XEMIS_NAT (:) = XUNDEF
!
 CALL AVERAGED_ALBEDO_EMIS_ISBA(IO, S, NK, NP, NPE,                           &
                                PZENITH, ZTG1, PSW_BANDS, PDIR_ALB, PSCA_ALB, &
                                S%XEMIS_NAT, ZTSRAD_NAT, ZTSURF_NAT        )
!
PEMIS  = S%XEMIS_NAT
PTSRAD = ZTSRAD_NAT
PTSURF = ZTSURF_NAT
!
!-------------------------------------------------------------------------------
!
!*      15.     ISBA diagnostics initialization
!               -------------------------------
!
IF(IO%NPATCH<=1) ID%O%LPATCH_BUDGET=.FALSE.
!
 CALL DIAG_ISBA_INIT_n(CHI, ID%DE, ID%DEC, ID%NDE, ID%NDEC, ID%O, &
                       ID%D, ID%DC, ID%ND, ID%NDC, ID%DM, ID%NDM, &
                       OREAD_BUDGETC, NGB, GB, IO, NP, NPE%AL(1)%TSNOW%SCHEME, &
                       NPE%AL(1)%TSNOW%NLAYER, SIZE(S%XABC), HPROGRAM,KI,KSW)
!
!-------------------------------------------------------------------------------
!
 CALL INIT_SURF_TOPD(ID%DEC, IO, S, K, NP, NPE, UG, U, HPROGRAM, U%NDIM_FULL)
!
!-------------------------------------------------------------------------------
!
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)
!
IF (LHOOK) CALL DR_HOOK('COMPUTE_ISBA_PARAMETERS',1,ZHOOK_HANDLE)
!
END SUBROUTINE COMPUTE_ISBA_PARAMETERS


