!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_PGD_ISBA_n (CHI, DTCO, DTV, DTZ, GB, IG, ISS, IO, S, K, &
                                  UG, U, USS, GCP, SV, HPROGRAM, OLAND_USE, TPDATE_END)
!     #########################################
!
!!****  *READ_PGD_ISBA_n* - routine to initialise ISBA physiographic variables 
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
!!      P. Le Moigne  12/2004 : add type of photosynthesis
!!      B. Decharme      2008 : add XWDRAIN
!!      B. Decharme   06/2009 : add topographic index statistics
!!      A.L. Gibelin 04/2009 : dimension NBIOMASS for ISBA-A-gs
!!      B. Decharme  07/2012  : files of data for permafrost area and for SOC top and sub soil
!!                   11/2013  : same for groundwater distribution
!!                   11/2014  : Read XSOILGRID as a series of real 
!!      P. Samuelsson 10/2014 : MEB
!!    10/2016 B. Decharme : bug surface/groundwater coupling   
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_CH_ISBA_n, ONLY : CH_ISBA_t
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_DATA_TSZ0_n, ONLY : DATA_TSZ0_t
USE MODD_GR_BIOG_n, ONLY : GR_BIOG_t
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODD_SSO_n, ONLY : SSO_t
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_S_t, ISBA_K_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
USE MODD_SV_n, ONLY : SV_t
!
USE MODD_TYPE_DATE_SURF
!
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_ISBA_PAR,    ONLY : XOPTIMGRID
!
USE MODI_READ_NAM_PGD_ISBA
!
USE MODI_INIT_IO_SURF_n
USE MODI_END_IO_SURF_n
!
USE MODI_READ_SURF
USE MODI_PACK_INIT
USE MODI_PACK_SSO
USE MODI_READ_PGD_ISBA_PAR_n
USE MODI_READ_PGD_TSZ0_PAR_n
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_TYPE_DIM_n
USE MODI_READ_LECOCLIMAP
!
USE MODI_ABOR1_SFX
!
USE MODI_GET_LUOUT
USE MODI_PACK_SAME_RANK
USE MODI_GET_SURF_MASK_n
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(CH_ISBA_t), INTENT(INOUT) :: CHI
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTV
TYPE(DATA_TSZ0_t), INTENT(INOUT) :: DTZ
TYPE(GR_BIOG_t), INTENT(INOUT) :: GB
TYPE(GRID_t), INTENT(INOUT) :: IG
TYPE(SSO_t), INTENT(INOUT) :: ISS
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t), INTENT(INOUT) :: S
TYPE(ISBA_K_t), INTENT(INOUT) :: K
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
TYPE(SV_t), INTENT(INOUT) :: SV
!
CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
LOGICAL,           INTENT(IN)  :: OLAND_USE ! 
TYPE(DATE), INTENT(IN) :: TPDATE_END
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER, DIMENSION(:), POINTER :: IMASK  ! mask for packing from complete field to nature field
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZWORK
!
CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
CHARACTER(LEN=4 ) :: YLVL
!
INTEGER :: ISIZE_LMEB_PATCH  ! Number of patches with MEB=true
INTEGER :: ILU    ! expected physical size of full surface array
INTEGER :: ILUOUT ! output listing logical unit
INTEGER :: IRESP  ! Error code after redding
INTEGER :: JLAYER ! loop counter on layers
INTEGER :: IVERSION, IBUGFIX   ! surface version
!
INTEGER                  :: IPATCH           ! number of patches
INTEGER                  :: IGROUND_LAYER    ! number of soil layers
INTEGER                  :: JVEGTYPE
CHARACTER(LEN=3)         :: YISBA            ! ISBA option
CHARACTER(LEN=4)         :: YPEDOTF          ! Pedo transfert function for DIF
CHARACTER(LEN=3)         :: YPHOTO           ! photosynthesis option
LOGICAL                  :: GTR_ML           ! new radiative transfert
CHARACTER(LEN=4)         :: YALBEDO
REAL                     :: ZRM_PATCH        ! threshold to remove little fractions of patches
CHARACTER(LEN=28)        :: YSAND            ! file name for sand fraction
CHARACTER(LEN=28)        :: YCLAY            ! file name for clay fraction
CHARACTER(LEN=28)        :: YSOC_TOP         ! file name for organic carbon top soil
CHARACTER(LEN=28)        :: YSOC_SUB         ! file name for organic carbon sub soil
CHARACTER(LEN=28)        :: YCTI             ! file name for topographic index
CHARACTER(LEN=28)        :: YRUNOFFB         ! file name for runoffb parameter
CHARACTER(LEN=28)        :: YWDRAIN          ! file name for wdrain parameter
CHARACTER(LEN=28)        :: YPERM            ! file name for permafrost distribution
CHARACTER(LEN=6)         :: YSANDFILETYPE    ! sand data file type
CHARACTER(LEN=6)         :: YCLAYFILETYPE    ! clay data file type
CHARACTER(LEN=6)         :: YSOCFILETYPE     ! organic carbon data file type
CHARACTER(LEN=6)         :: YCTIFILETYPE     ! topographic index data file type
CHARACTER(LEN=6)         :: YRUNOFFBFILETYPE ! subgrid runoff data file type
CHARACTER(LEN=6)         :: YWDRAINFILETYPE  ! subgrid drainage data file type
CHARACTER(LEN=6)         :: YPERMFILETYPE    ! permafrost distribution data file type
REAL                     :: XUNIF_SAND       ! uniform value of sand fraction  (-)
REAL                     :: XUNIF_CLAY       ! uniform value of clay fraction  (-)
REAL                     :: XUNIF_SOC_TOP    ! uniform value of organic carbon top soil (kg/m2)
REAL                     :: XUNIF_SOC_SUB    ! uniform value of organic carbon sub soil (kg/m2)
REAL                     :: XUNIF_RUNOFFB    ! uniform value of subgrid runoff coefficient
REAL                     :: XUNIF_WDRAIN     ! uniform subgrid drainage parameter
REAL                     :: XUNIF_PERM       ! uniform permafrost distribution
LOGICAL                  :: LIMP_SAND        ! Imposed maps of Sand
LOGICAL                  :: LIMP_CLAY        ! Imposed maps of Clay
LOGICAL                  :: LIMP_SOC         ! Imposed maps of organic carbon
LOGICAL                  :: LIMP_CTI         ! Imposed maps of topographic index statistics
LOGICAL                  :: LIMP_PERM        ! Imposed maps of permafrost distribution
REAL, DIMENSION(150)     :: ZSOILGRID        ! Soil grid reference for DIF
CHARACTER(LEN=28)        :: YPH           ! file name for pH
CHARACTER(LEN=28)        :: YFERT         ! file name for fertilisation rate
CHARACTER(LEN=6)         :: YPHFILETYPE   ! pH data file type
CHARACTER(LEN=6)         :: YFERTFILETYPE ! fertilisation data file type
REAL                     :: XUNIF_PH      ! uniform value of pH
REAL                     :: XUNIF_FERT    ! uniform value of fertilisation rate
LOGICAL                  :: GMEB      ! Multi-energy balance (MEB)
!
LOGICAL :: GECOSG
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!* 1D physical dimension
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_ISBA_N',0,ZHOOK_HANDLE)
YRECFM='SIZE_NATURE'
 CALL GET_TYPE_DIM_n(DTCO, U, 'NATURE',IG%NDIM)
!
YRECFM='VERSION'
 CALL READ_SURF(HPROGRAM,YRECFM,IVERSION,IRESP)
!
YRECFM='BUG'
 CALL READ_SURF(HPROGRAM,YRECFM,IBUGFIX,IRESP)
!
!*       2.     Dimension initializations:
!               -------------------------
!
!* soil scheme
!
YRECFM='ISBA'
 CALL READ_SURF(HPROGRAM,YRECFM,IO%CISBA,IRESP)
!
IF (IVERSION>=7) THEN
  !
  !* Pedo-transfert function
  !
  YRECFM='PEDOTF'
  CALL READ_SURF(HPROGRAM,YRECFM,IO%CPEDOTF,IRESP)
  !
ELSE
  IO%CPEDOTF = 'CH78'
ENDIF
!
!* type of photosynthesis
!
YRECFM='PHOTO'
 CALL READ_SURF(HPROGRAM,YRECFM,IO%CPHOTO,IRESP)
!
!* new radiative transfert
!
IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=2) THEN
  !
  YRECFM='TR_ML'
  CALL READ_SURF(HPROGRAM,YRECFM,IO%LTR_ML,IRESP)
  !
ELSE 
  IO%LTR_ML = .FALSE.
ENDIF
!
IF (IVERSION>8 .OR. IVERSION==8 .AND. IBUGFIX>=1) THEN
  !
  YRECFM='ALBEDO'
  CALL READ_SURF(HPROGRAM,YRECFM,IO%CALBEDO,IRESP)
  !
ELSE
  !
  CALL READ_NAM_PGD_ISBA(HPROGRAM, IPATCH, IGROUND_LAYER,                        &
                       YISBA,  YPEDOTF, YPHOTO, GTR_ML, YALBEDO, ZRM_PATCH,      &
                       YCLAY, YCLAYFILETYPE, XUNIF_CLAY, LIMP_CLAY,              &
                       YSAND, YSANDFILETYPE, XUNIF_SAND, LIMP_SAND,              &
                       YSOC_TOP, YSOC_SUB, YSOCFILETYPE, XUNIF_SOC_TOP,          &
                       XUNIF_SOC_SUB, LIMP_SOC, YCTI, YCTIFILETYPE, LIMP_CTI,    &
                       YPERM, YPERMFILETYPE, XUNIF_PERM, LIMP_PERM, GMEB,        &                       
                       YRUNOFFB, YRUNOFFBFILETYPE, XUNIF_RUNOFFB,                &
                       YWDRAIN,  YWDRAINFILETYPE , XUNIF_WDRAIN, ZSOILGRID,      &
                       YPH, YPHFILETYPE, XUNIF_PH, YFERT, YFERTFILETYPE,         &
                       XUNIF_FERT                          )  
  IO%CALBEDO = YALBEDO
  !
ENDIF
!
!* threshold to remove little fractions of patches
!
IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
  !
  YRECFM='RM_PATCH'
  CALL READ_SURF(HPROGRAM,YRECFM,IO%XRM_PATCH,IRESP)
  !
ELSE 
  IO%XRM_PATCH = 0.0
ENDIF
!
!* number of soil layers
!
YRECFM='GROUND_LAYER'
 CALL READ_SURF(HPROGRAM,YRECFM,IO%NGROUND_LAYER,IRESP)
!
!* Reference grid for DIF
!
IF(IO%CISBA=='DIF') THEN
  ALLOCATE(IO%XSOILGRID(IO%NGROUND_LAYER))
  IO%XSOILGRID=XUNDEF
  IF (IVERSION>=8) THEN
    DO JLAYER=1,IO%NGROUND_LAYER
      WRITE(YLVL,'(I4)') JLAYER
      YRECFM='SOILGRID'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
      CALL READ_SURF(HPROGRAM,YRECFM,IO%XSOILGRID(JLAYER),IRESP)
    ENDDO    
  ELSEIF (IVERSION==7 .AND. IBUGFIX>=2) THEN
    YRECFM='SOILGRID'
    CALL READ_SURF(HPROGRAM,YRECFM,IO%XSOILGRID,IRESP,HDIR='-')
  ELSE
    IO%XSOILGRID(1:IO%NGROUND_LAYER)=XOPTIMGRID(1:IO%NGROUND_LAYER)
  ENDIF
ELSE
  ALLOCATE(IO%XSOILGRID(0))
ENDIF
!
!* number of biomass pools
!
IF (IVERSION>=6) THEN
  YRECFM='NBIOMASS'
  CALL READ_SURF(HPROGRAM,YRECFM,IO%NNBIOMASS,IRESP)
ELSE
  SELECT CASE (IO%CPHOTO)
    CASE ('AST')
      IO%NNBIOMASS = 1
    CASE ('NIT')
      IO%NNBIOMASS = 3
    CASE ('NCB')
      IO%NNBIOMASS = 6
  END SELECT
ENDIF
!
!* number of tiles
!
YRECFM='PATCH_NUMBER'
 CALL READ_SURF(HPROGRAM,YRECFM,IO%NPATCH,IRESP)
!
!* logical vector indicating for which patches MEB should be applied
!
ALLOCATE(IO%LMEB_PATCH(IO%NPATCH))
!
IF (IVERSION>=8) THEN
!
  YRECFM='MEB_PATCH'
  CALL READ_SURF(HPROGRAM,YRECFM,IO%LMEB_PATCH(:),IRESP,HDIR='-')
!
  ISIZE_LMEB_PATCH = COUNT(IO%LMEB_PATCH(:))
!
  IF (ISIZE_LMEB_PATCH>0)THEN
    YRECFM='FORC_MEASURE'
    CALL READ_SURF(HPROGRAM,YRECFM,IO%LFORC_MEASURE,IRESP)
    YRECFM='MEB_LITTER'
    CALL READ_SURF(HPROGRAM,YRECFM,IO%LMEB_LITTER,IRESP)
    YRECFM='MEB_GNDRES'
    CALL READ_SURF(HPROGRAM,YRECFM,IO%LMEB_GNDRES,IRESP)      
  ELSE
    IO%LFORC_MEASURE=.FALSE.
    IO%LMEB_LITTER  =.FALSE. 
    IO%LMEB_GNDRES  =.FALSE.    
  ENDIF
!
ELSE
  IO%LMEB_PATCH(:)=.FALSE.
  IO%LFORC_MEASURE=.FALSE.
  IO%LMEB_LITTER  =.FALSE.
  IO%LMEB_GNDRES  =.FALSE.  
ENDIF
!
!
!*       3.     Physiographic data fields:
!               -------------------------
!
!
!*       3.1    Cover classes :
!               -------------
!
ALLOCATE(S%LCOVER(JPCOVER))
ALLOCATE(S%XZS(IG%NDIM))
!
ALLOCATE(IG%XLAT       (IG%NDIM))
ALLOCATE(IG%XLON       (IG%NDIM))
ALLOCATE(IG%XMESH_SIZE (IG%NDIM))
!
ALLOCATE(ISS%XZ0EFFJPDIR(IG%NDIM))
!
CALL PACK_INIT(DTCO, U, UG, HPROGRAM, 'NATURE', IG, S%LCOVER, S%XCOVER, S%XZS, ISS%XZ0EFFJPDIR )
!
!* clay fraction : attention, seul un niveau est present dans le fichier
!* on rempli tout les niveaux de  XCLAY avec les valeurs du fichiers
!
ALLOCATE(K%XCLAY(IG%NDIM,IO%NGROUND_LAYER))
YRECFM='CLAY'
 CALL READ_SURF(HPROGRAM,YRECFM,K%XCLAY(:,1),IRESP)
DO JLAYER=2,IO%NGROUND_LAYER
  K%XCLAY(:,JLAYER) = K%XCLAY(:,1)
END DO
!
!* sand fraction
!
ALLOCATE(K%XSAND(IG%NDIM,IO%NGROUND_LAYER))
YRECFM='SAND'
 CALL READ_SURF(HPROGRAM,YRECFM,K%XSAND(:,1),IRESP)
DO JLAYER=2,IO%NGROUND_LAYER
  K%XSAND(:,JLAYER) = K%XSAND(:,1)
END DO
!
!* Soil organic carbon profile
!
IF (IVERSION>7 .OR. (IVERSION==7 .AND. IBUGFIX>=3)) THEN
   YRECFM='SOCP'
   CALL READ_SURF(HPROGRAM,YRECFM,IO%LSOCP,IRESP)
ELSE
   IO%LSOCP=.FALSE.
ENDIF
!
IF(IO%LSOCP)THEN
!  
  ALLOCATE(S%XSOC (IG%NDIM,IO%NGROUND_LAYER))
!
  YRECFM='SOC_TOP'
  CALL READ_SURF(HPROGRAM,YRECFM,S%XSOC(:,1),IRESP)
  YRECFM='SOC_SUB'
  CALL READ_SURF(HPROGRAM,YRECFM,S%XSOC(:,2),IRESP)
!
  DO JLAYER=2,IO%NGROUND_LAYER
    S%XSOC (:,JLAYER)=S%XSOC (:,2)
  END DO
!
ELSE
!  
  ALLOCATE(S%XSOC (0,1))
!
ENDIF
!
!* permafrost distribution
!
IF (IVERSION>7 .OR. (IVERSION==7 .AND. IBUGFIX>=3)) THEN
   YRECFM='PERMAFROST'
   CALL READ_SURF(HPROGRAM,YRECFM,IO%LPERM,IRESP)
ELSE
   IO%LPERM=.FALSE.
ENDIF
!
IF(IO%LPERM)THEN
!  
  ALLOCATE(K%XPERM (IG%NDIM))
!
  YRECFM='PERM'
  CALL READ_SURF(HPROGRAM,YRECFM,K%XPERM(:),IRESP)
!
ELSE
!  
  ALLOCATE(K%XPERM (0))
!
ENDIF
!
!SOILNOX
!
IF (IVERSION>7 .OR. (IVERSION==7 .AND. IBUGFIX>=3)) THEN
   YRECFM='NO'
   CALL READ_SURF(HPROGRAM,YRECFM,IO%LNOF,IRESP)
ELSE
   IO%LNOF = .FALSE.
ENDIF
!
IF (CHI%LCH_NO_FLUX) THEN
  !
  IF (IO%LNOF) THEN
    !
    ALLOCATE(S%XPH(IG%NDIM))
    YRECFM='PH'
    CALL READ_SURF(HPROGRAM,YRECFM,S%XPH(:),IRESP)
    !
    ALLOCATE(S%XFERT(IG%NDIM))
    YRECFM='FERT'
    CALL READ_SURF(HPROGRAM,YRECFM,S%XFERT(:),IRESP)
    !
  ELSE
    CALL ABOR1_SFX("READ_PGD_ISBAn: WITH LCH_NO_FLUX=T, PH AND FERT FIELDS ARE GIVEN AT PGD STEP")
  ENDIF
  !
ELSE
  ALLOCATE(S%XPH (0))
  ALLOCATE(S%XFERT(0))
END IF
!
!* subgrid-scale orography parameters to compute dynamical roughness length
!
ALLOCATE(ISS%XAOSIP(IG%NDIM))
ALLOCATE(ISS%XAOSIM(IG%NDIM))
ALLOCATE(ISS%XAOSJP(IG%NDIM))
ALLOCATE(ISS%XAOSJM(IG%NDIM))
ALLOCATE(ISS%XHO2IP(IG%NDIM))
ALLOCATE(ISS%XHO2IM(IG%NDIM))
ALLOCATE(ISS%XHO2JP(IG%NDIM))
ALLOCATE(ISS%XHO2JM(IG%NDIM))
ALLOCATE(ISS%XSSO_SLOPE(IG%NDIM))
ALLOCATE(ISS%XSSO_STDEV(IG%NDIM))
!
 CALL PACK_SSO(USS,HPROGRAM,U%NR_NATURE, ISS)
!
!* orographic runoff coefficient
!
ALLOCATE(K%XRUNOFFB(IG%NDIM))
YRECFM='RUNOFFB'
 CALL READ_SURF(HPROGRAM,YRECFM,K%XRUNOFFB,IRESP)
!
!* subgrid drainage coefficient
!
ALLOCATE(K%XWDRAIN(IG%NDIM))
IF (IVERSION<=3) THEN
  K%XWDRAIN = 0.
ELSE
  YRECFM='WDRAIN'
  CALL READ_SURF(HPROGRAM,YRECFM,K%XWDRAIN,IRESP)
ENDIF
!
!* topographic index statistics
!
IF(IO%CRUNOFF=='SGH ' .AND. IVERSION>=5) THEN 
!
  YRECFM='CTI'
  CALL READ_SURF(HPROGRAM,YRECFM,IO%LCTI,IRESP)        
!
  IF (.NOT.IO%LCTI) CALL ABOR1_SFX("READ_PGD_ISBA_n:WITH CRUNOFF=SGH, CTI MAPS MUST BE GIVEN TO PGD")
  !
  ALLOCATE(S%XTI_MIN(IG%NDIM))
  ALLOCATE(S%XTI_MAX(IG%NDIM))
  ALLOCATE(S%XTI_MEAN(IG%NDIM))
  ALLOCATE(S%XTI_STD(IG%NDIM))
  ALLOCATE(S%XTI_SKEW(IG%NDIM))
!
  YRECFM='TI_MIN'
  CALL READ_SURF(HPROGRAM,YRECFM,S%XTI_MIN,IRESP)
!
  YRECFM='TI_MAX'
  CALL READ_SURF(HPROGRAM,YRECFM,S%XTI_MAX,IRESP)
!
  YRECFM='TI_MEAN'
  CALL READ_SURF(HPROGRAM,YRECFM,S%XTI_MEAN,IRESP)
!
  YRECFM='TI_STD'
  CALL READ_SURF(HPROGRAM,YRECFM,S%XTI_STD,IRESP)
!
  YRECFM='TI_SKEW'
  CALL READ_SURF(HPROGRAM,YRECFM,S%XTI_SKEW,IRESP)
!
ELSE
!
  ALLOCATE(S%XTI_MIN(0))
  ALLOCATE(S%XTI_MAX(0))
  ALLOCATE(S%XTI_MEAN(0))
  ALLOCATE(S%XTI_STD(0))
  ALLOCATE(S%XTI_SKEW(0))
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!* biogenic chemical emissions
!
IF (CHI%LCH_BIO_FLUX) THEN
  ALLOCATE(ZWORK(U%NSIZE_FULL,1))
  !
  CALL END_IO_SURF_n(HPROGRAM)
CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'FULL  ','SURF  ','READ ')
  !
  CALL GET_LUOUT(HPROGRAM,ILUOUT)
  ALLOCATE(IMASK(IG%NDIM))
  ILU=0
  CALL GET_SURF_MASK_n(DTCO, U, 'NATURE',IG%NDIM,IMASK,ILU,ILUOUT)
  ALLOCATE(GB%XISOPOT(IG%NDIM))
  ALLOCATE(GB%XMONOPOT(IG%NDIM))
  !
  ZWORK(:,:) = 0.  
  YRECFM='E_ISOPOT'
  CALL READ_SURF(HPROGRAM,YRECFM,ZWORK,IRESP)
  CALL PACK_SAME_RANK(IMASK,ZWORK(:,1),GB%XISOPOT(:))
  !
  ZWORK(:,:) = 0.  
  YRECFM='E_MONOPOT'
  CALL READ_SURF(HPROGRAM,YRECFM,ZWORK,IRESP)
  CALL PACK_SAME_RANK(IMASK,ZWORK(:,1),GB%XMONOPOT(:))
  !
  CALL END_IO_SURF_n(HPROGRAM)
CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'NATURE','ISBA  ','READ ')
  !
  DEALLOCATE(ZWORK)
ELSE
  ALLOCATE(GB%XISOPOT (0))
  ALLOCATE(GB%XMONOPOT(0))
END IF
!
!-------------------------------------------------------------------------------
!
!*       4.     Physiographic data fields not to be computed by ecoclimap
!               ---------------------------------------------------------
!
 CALL READ_LECOCLIMAP(HPROGRAM,IO%LECOCLIMAP,GECOSG)
!
 CALL READ_PGD_ISBA_PAR_n(DTCO, U, GCP, DTV, IG%NDIM, IO, HPROGRAM,IG%NDIM, OLAND_USE, S%TTIME%TDATE, TPDATE_END)
IF (U%CNATURE == 'TSZ0') CALL READ_PGD_TSZ0_PAR_n(DTZ, HPROGRAM)
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_ISBA_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_PGD_ISBA_n
