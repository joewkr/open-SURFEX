!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
#ifdef RS6K
@PROCESS NOOPTIMIZE
#endif
!     #########################
MODULE MODI_INI_DATA_COVER
CONTAINS
      SUBROUTINE INI_DATA_COVER (DTCO, U)
!     #########################
!
!!**** *INI_DATA_COVER* initializes cover-field correspondance arrays
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    15/12/97
!!    F.solmon    01/06/00 adaptation for patch approach
!!    B.Decharme  01/03/09 Arrange cover by user
!!    G.Pigeon      08/12 add ROUGH_WALL/ROUGH_ROOF
!!    V. Masson     04/13 merges Arrange cover & garden use option in arrange_cover routine
!!    R.Alkama      05/15 Add 7 new vegtype (19 rather than 12)
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_TYPE_DATE_SURF
!
USE MODD_SURFEX_MPI,     ONLY : WLOG_MPI
!
USE MODD_SURFEX_OMP,     ONLY : IDC
!
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
!
!
USE MODD_DATA_COVER,     ONLY : XDATA_TOWN, XDATA_NATURE, XDATA_SEA, XDATA_WATER, &
                                  XDATA_LAI, XDATA_VEGTYPE, XDATA_H_TREE,           &
                                  XDATA_ALB_VEG_VIS, XDATA_ALB_VEG_NIR,             &
                                  XDATA_ALB_SOIL_VIS, XDATA_ALB_SOIL_NIR,           &
                                  XDATA_ALBNIR_VEG, XDATA_ALBVIS_VEG,               &
                                  XDATA_ALBUV_VEG, XDATA_RSMIN,                     &
                                  XDATA_RGL, XDATA_CV, XDATA_GAMMA, XDATA_GMES,     &
                                  XDATA_GC, XDATA_BSLAI, XDATA_SEFOLD, XDATA_LAIMIN,&
                                  XDATA_DMAX, XDATA_STRESS, XDATA_F2I,              &
                                  XDATA_VEG, XDATA_GREEN, XDATA_Z0, XDATA_Z0_O_Z0H, &
                                  XDATA_EMIS_ECO, XDATA_WRMAX_CF,                   &
                                  XDATA_CE_NITRO,XDATA_CF_NITRO,XDATA_CNA_NITRO,    &
                                  XDATA_GROUND_DEPTH, XDATA_ROOT_DEPTH,             &
                                  XDATA_ROOT_EXTINCTION, XDATA_ROOT_LIN,            &
                                  XDATA_SOILRC_SO2, XDATA_SOILRC_O3,                &
                                  XDATA_Z0_TOWN, XDATA_Z0H_TOWN, XDATA_ALB_ROOF,    &
                                  XDATA_EMIS_ROOF, XDATA_HC_ROOF, XDATA_TC_ROOF,    &
                                  XDATA_D_ROOF, XDATA_ALB_ROAD, XDATA_EMIS_ROAD,    &
                                  XDATA_HC_ROAD, XDATA_TC_ROAD, XDATA_D_ROAD,       &
                                  XDATA_ALB_WALL, XDATA_EMIS_WALL, XDATA_HC_WALL,   &
                                  XDATA_TC_WALL, XDATA_D_WALL, XDATA_BLD_HEIGHT,    &
                                  XDATA_WALL_O_HOR, XDATA_BLD, XDATA_CAN_HW_RATIO,  &
                                  XDATA_GARDEN, XDATA_DICE,                         &
                                  XDATA_H_TRAFFIC, XDATA_LE_TRAFFIC,                &
                                  XDATA_H_INDUSTRY, XDATA_LE_INDUSTRY, XDATA_RE25,  &
                                  XDATA_GMES_ST, XDATA_BSLAI_ST, XDATA_SEFOLD_ST,   &
                                  XDATA_GC_ST, XDATA_DMAX_ST, TDATA_SEED,           &
                                  TDATA_REAP, XDATA_WATSUP, XDATA_IRRIG,            &
                                  XDATA_LAI_ALL_YEARS, LREAD_DATA_COVER,            &
                                  XDATA_HC_FLOOR, XDATA_TC_FLOOR, XDATA_D_FLOOR,    &
                                  XDATA_TCOOL_TARGET, XDATA_THEAT_TARGET,           &
                                  XDATA_F_WASTE_CAN, XDATA_EFF_HEAT, XDATA_QIN,     &
                                  XDATA_QIN_FRAD, XDATA_SHGC, XDATA_U_WIN, XDATA_GR,&
                                  XDATA_SHGC_SH, XDATA_FLOOR_HEIGHT, XDATA_INF,     &
                                  XDATA_F_WATER_COND, XDATA_QIN_FLAT,               &
                                  XDATA_HR_TARGET, XDATA_V_VENT, XDATA_CAP_SYS_HEAT,&
                                  XDATA_CAP_SYS_RAT, XDATA_T_ADP, XDATA_M_SYS_RAT,  &
                                  XDATA_COP_RAT, XDATA_T_SIZE_MAX, XDATA_T_SIZE_MIN,&
                                  XDATA_SHADE, XDATA_NATVENT, XDATA_ROUGH_ROOF,     &
                                  XDATA_ROUGH_WALL, XDATA_FRAC_GR,XDATA_RESIDENTIAL,&
                                  XDATA_EMIS_PANEL, XDATA_ALB_PANEL,                &
                                  XDATA_EFF_PANEL, XDATA_FRAC_PANEL,                &
                                  XDATA_GNDLITTER, XDATA_Z0LITTER, XDATA_H_VEG
!
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE, NVEGTYPE_OLD, NVEGTYPE_ECOSG,   &
                                  NVT_NO, NVT_ROCK, NVT_SNOW,             &
                                  NVT_TEBD, NVT_BONE, NVT_TRBE, NVT_C3,   &
                                  NVT_C4, NVT_IRR, NVT_GRAS, NVT_TROG,    &
                                  NVT_PARK, NVT_TRBD, NVT_TEBE, NVT_TENE, &
                                  NVT_BOBD, NVT_BOND, NVT_BOGR, NVT_SHRB, &
                                  JPCOVER, NCOVER, NTYPE, NDATA_ROAD_LAYER,  &
                                  NDATA_WALL_LAYER, NDATA_ROOF_LAYER,     &
                                  NDATA_FLOOR_LAYER, CNAMES, NBARE_SOIL,  &
                                  NROCK, NSEA, NWATER, NPERMSNOW, NUT_CPHR, &
                                  NUT_CPMR, NUT_CPLR, NUT_OPHR, NUT_OPMR, &
                                  NUT_OPLR, NUT_LWLR, NUT_LALR, NUT_SPAR, &
                                  NUT_INDU, NVT_C3W, NVT_C3S, NVT_FLTR, NVT_FLGR
!
USE MODD_WRITE_COVER_TEX,ONLY : CNAME, CLANG
!
!
USE MODE_POS_SURF
!
USE MODI_READ_COVERS_PARAM
USE MODI_INIT_TYPES_PARAM
!
USE MODI_ABOR1_SFX
!
USE MODI_DEFAULT_DATA_COVER
!
USE MODI_DEFAULT_LAI_ECO1_01
USE MODI_DEFAULT_LAI_ECO1_02
USE MODI_DEFAULT_LAI_ECO1_03
USE MODI_DEFAULT_LAI_ECO1_04
USE MODI_DEFAULT_LAI_ECO1_05
USE MODI_DEFAULT_LAI_ECO1_06
USE MODI_DEFAULT_LAI_ECO1_07
USE MODI_DEFAULT_LAI_ECO1_08
USE MODI_DEFAULT_LAI_ECO1_09
USE MODI_DEFAULT_LAI_ECO1_10
USE MODI_DEFAULT_LAI_ECO1_11
USE MODI_DEFAULT_LAI_ECO1_12
USE MODI_DEFAULT_LAI_ECO1_13
USE MODI_DEFAULT_LAI_ECO1_14
USE MODI_DEFAULT_LAI_ECO1_15
USE MODI_DEFAULT_LAI_ECO1_16
USE MODI_DEFAULT_LAI_ECO1_17
USE MODI_DEFAULT_LAI_ECO1_18
USE MODI_DEFAULT_LAI_ECO1_19
!
USE MODI_DEFAULT_ALB_SOIL_ECO1
USE MODI_DEFAULT_ALB_SOIL_ECO2
!
USE MODI_DEFAULT_ALB_VEG_ECO1_01
USE MODI_DEFAULT_ALB_VEG_ECO1_02
USE MODI_DEFAULT_ALB_VEG_ECO1_03
USE MODI_DEFAULT_ALB_VEG_ECO1_04
USE MODI_DEFAULT_ALB_VEG_ECO1_05
USE MODI_DEFAULT_ALB_VEG_ECO1_06
USE MODI_DEFAULT_ALB_VEG_ECO1_07
USE MODI_DEFAULT_ALB_VEG_ECO1_08
USE MODI_DEFAULT_ALB_VEG_ECO1_09
USE MODI_DEFAULT_ALB_VEG_ECO1_10
USE MODI_DEFAULT_ALB_VEG_ECO1_11
USE MODI_DEFAULT_ALB_VEG_ECO1_12
USE MODI_DEFAULT_ALB_VEG_ECO1_13
USE MODI_DEFAULT_ALB_VEG_ECO1_14
USE MODI_DEFAULT_ALB_VEG_ECO1_15
USE MODI_DEFAULT_ALB_VEG_ECO1_16
USE MODI_DEFAULT_ALB_VEG_ECO1_17
USE MODI_DEFAULT_ALB_VEG_ECO1_18
USE MODI_DEFAULT_ALB_VEG_ECO1_19
!
USE MODI_ARRANGE_COVER
USE MODI_COVER301_573
USE MODI_ECOCLIMAP2_LAI
USE MODI_INI_DATA_PARAM
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER         :: JCOV, JVEG, JDEC       ! loop counters on covers and decades
INTEGER         :: ICPT_SEA, ICPT_WATER
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*    0.3    Declaration of namelists
!            ------------------------
!
!-------------------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('INI_DATA_COVER',0,ZHOOK_HANDLE)
!
IF (U%LECOSG) THEN
  JPCOVER = SUM(NTYPE)
ELSE
  JPCOVER = NCOVER
ENDIF
!
IF (ALLOCATED(XDATA_TOWN)) THEN
  IF (SIZE(XDATA_TOWN)/=JPCOVER) THEN
    IDC = 0
    DEALLOCATE(XDATA_TOWN,XDATA_GARDEN,XDATA_NATURE,XDATA_WATER,XDATA_SEA)
    DEALLOCATE(XDATA_LAI,XDATA_LAI_ALL_YEARS,XDATA_VEGTYPE)
    DEALLOCATE(XDATA_H_TREE,XDATA_GROUND_DEPTH,XDATA_ROOT_DEPTH,XDATA_DICE)
    DEALLOCATE(XDATA_ROOT_EXTINCTION,XDATA_ROOT_LIN)
    DEALLOCATE(XDATA_ALBNIR_VEG,XDATA_ALBVIS_VEG,XDATA_ALBUV_VEG)
    DEALLOCATE(XDATA_ALB_VEG_VIS,XDATA_ALB_VEG_NIR,XDATA_ALB_SOIL_VIS,XDATA_ALB_SOIL_NIR)
    DEALLOCATE(XDATA_RSMIN,XDATA_GAMMA,XDATA_WRMAX_CF,XDATA_RGL,XDATA_CV)
    DEALLOCATE(XDATA_GMES,XDATA_GMES_ST,XDATA_RE25,XDATA_GC,XDATA_GC_ST)
    DEALLOCATE(XDATA_F2I,XDATA_BSLAI,XDATA_BSLAI_ST,XDATA_DMAX,XDATA_DMAX_ST,XDATA_STRESS)
    DEALLOCATE(XDATA_SEFOLD,XDATA_SEFOLD_ST,XDATA_LAIMIN,XDATA_VEG,XDATA_GREEN)
    DEALLOCATE(XDATA_Z0,XDATA_Z0_O_Z0H,XDATA_EMIS_ECO,XDATA_SOILRC_SO2,XDATA_SOILRC_O3)
    DEALLOCATE(XDATA_CE_NITRO,XDATA_CF_NITRO,XDATA_CNA_NITRO,TDATA_SEED,TDATA_REAP)
    DEALLOCATE(XDATA_IRRIG,XDATA_WATSUP,XDATA_GNDLITTER,XDATA_Z0LITTER,XDATA_H_VEG)
    DEALLOCATE(XDATA_Z0_TOWN,XDATA_ALB_ROOF,XDATA_ALB_ROAD,XDATA_ALB_WALL)
    DEALLOCATE(XDATA_EMIS_ROOF,XDATA_EMIS_ROAD,XDATA_EMIS_WALL)
    DEALLOCATE(XDATA_HC_ROOF,XDATA_HC_ROAD,XDATA_HC_WALL,XDATA_HC_FLOOR)
    DEALLOCATE(XDATA_TC_ROOF,XDATA_TC_ROAD,XDATA_TC_WALL,XDATA_TC_FLOOR)
    DEALLOCATE(XDATA_D_ROOF,XDATA_D_ROAD,XDATA_D_WALL,XDATA_D_FLOOR)
    DEALLOCATE(XDATA_BLD_HEIGHT,XDATA_WALL_O_HOR,XDATA_BLD,XDATA_CAN_HW_RATIO)
    DEALLOCATE(XDATA_H_TRAFFIC,XDATA_LE_TRAFFIC,XDATA_H_INDUSTRY,XDATA_LE_INDUSTRY)
    DEALLOCATE(XDATA_TCOOL_TARGET,XDATA_THEAT_TARGET,XDATA_F_WASTE_CAN)
    DEALLOCATE(XDATA_EFF_HEAT,XDATA_QIN,XDATA_QIN_FRAD,XDATA_SHGC,XDATA_U_WIN,XDATA_GR)
    DEALLOCATE(XDATA_SHGC_SH,XDATA_FLOOR_HEIGHT,XDATA_INF,XDATA_F_WATER_COND)
    DEALLOCATE(XDATA_QIN_FLAT,XDATA_HR_TARGET,XDATA_V_VENT,XDATA_CAP_SYS_HEAT)
    DEALLOCATE(XDATA_CAP_SYS_RAT,XDATA_T_ADP,XDATA_M_SYS_RAT,XDATA_COP_RAT)
    DEALLOCATE(XDATA_T_SIZE_MAX,XDATA_T_SIZE_MIN,XDATA_SHADE,XDATA_NATVENT)
    DEALLOCATE(XDATA_ROUGH_ROOF,XDATA_ROUGH_WALL,XDATA_RESIDENTIAL,XDATA_FRAC_GR)
    DEALLOCATE(XDATA_EMIS_PANEL,XDATA_ALB_PANEL,XDATA_EFF_PANEL,XDATA_FRAC_PANEL)
    DEALLOCATE(NSEA,NWATER,CNAMES,CNAME)
  ENDIF
ENDIF
!
IF (IDC==0) THEN
!
!*    1.1    artificial surfaces fraction
!            ----------------------------
!
ALLOCATE(XDATA_TOWN(JPCOVER))
!
XDATA_TOWN = 0.
!
ALLOCATE(XDATA_GARDEN(JPCOVER))
!
XDATA_GARDEN = 0.
!
!-------------------------------------------------------------------------------
!
!*    1.2    natural and cultivated surfaces fraction
!            ----------------------------------------
!
ALLOCATE(XDATA_NATURE(JPCOVER))
!
XDATA_NATURE = 0.
!
!-------------------------------------------------------------------------------
!
!*    1.3    inland waters surfaces fraction
!            -------------------------------
!
ALLOCATE(XDATA_WATER(JPCOVER))
!
XDATA_WATER = 0.
!
!-------------------------------------------------------------------------------
!
!*    1.4    sea surface fraction
!            --------------------
!
ALLOCATE(XDATA_SEA(JPCOVER))
!
XDATA_SEA = 0.
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
!!possible patches correspond to  vegetation types
!
!*    2.0    vegetation type fractions
!
!New name  N   Nold   Comments
!-----------------------------
IF (U%LECOSG) THEN
  NVEGTYPE = NVEGTYPE_ECOSG
  NVT_C3   = 0
  NVT_PARK = 0
  NVT_IRR  = 0
  NVT_NO   = 1   ! 1  ! no vegetation (smooth)
  NVT_ROCK = 2   ! 2  ! no vegetation (rocks)
  NVT_SNOW = 3   ! 3  ! permanent snow and ice
  NVT_BOBD = 4   ! 4  ! boreal broadleaf cold-deciduous summergreen (TREE)
  NVT_TEBD = 5   ! 4  ! temperate broadleaf cold-deciduous summergreen (TREE)
  NVT_TRBD = 6   ! 4  ! tropical broadleaf deciduous (TREE)
  NVT_TEBE = 7   ! 4  ! temperate broadleaf evergreen (TREE)
  NVT_TRBE = 8   ! 6  ! tropical broadleaf evergreen (EVER)
  NVT_BONE = 9   ! 5  ! boreal needleleaf evergreen  (CONI)
  NVT_TENE =10   ! 5  ! temperate needleleaf evergreen (CONI)
  NVT_BOND =11   ! 5  ! boreal needleleaf cold-deciduous summergreen (CONI)
  NVT_SHRB =12   ! 4  ! shrub (TREE)
  NVT_BOGR =13   !10  ! boreal grass (GRAS)
  NVT_GRAS =14   !10  ! grassland
  NVT_TROG =15   !11  ! tropical grassland
  NVT_C3W  =16   ! 7  ! C3W cultures types
  NVT_C3S  =17   ! 7  ! C3S cultures types
  NVT_C4   =18   ! 8  ! C4 cultures types
  NVT_FLTR =19   !12  ! flooded trees
  NVT_FLGR =20   !12  ! flooded grassland
ELSE
  NVEGTYPE = NVEGTYPE_OLD
  NVT_C3W  = 0
  NVT_C3S  = 0
  NVT_FLTR = 0
  NVT_FLGR = 0
  NVT_NO   = 1   ! 1  ! no vegetation (smooth)
  NVT_ROCK = 2   ! 2  ! no vegetation (rocks)
  NVT_SNOW = 3   ! 3  ! permanent snow and ice
  NVT_TEBD = 4   ! 4  ! temperate broadleaf cold-deciduous summergreen (TREE)
  NVT_BONE = 5   ! 5  ! boreal needleleaf evergreen  (CONI)
  NVT_TRBE = 6   ! 6  ! tropical broadleaf evergreen (EVER)
  NVT_C3   = 7   ! 7  ! C3 cultures types
  NVT_C4   = 8   ! 8  ! C4 cultures types
  NVT_IRR  = 9   ! 9  ! irrigated crops
  NVT_GRAS =10   !10  ! grassland
  NVT_TROG =11   !11  ! tropical grassland
  NVT_PARK =12   !12  ! peat bogs, parks and gardens (irrigated grass)
  NVT_TRBD =13   ! 4  ! tropical broadleaf deciduous (TREE)
  NVT_TEBE =14   ! 4  ! temperate broadleaf evergreen (TREE)
  NVT_TENE =15   ! 5  ! temperate needleleaf evergreen (CONI)
  NVT_BOBD =16   ! 4  ! boreal broadleaf cold-deciduous summergreen (TREE)
  NVT_BOND =17   ! 5  ! boreal needleleaf cold-deciduous summergreen (CONI)
  NVT_BOGR =18   !10  ! boreal grass (GRAS)
  NVT_SHRB =19   ! 4  ! shrub (TREE)
ENDIF
!
!*    2.1    leaf area index
!            ---------------
!
ALLOCATE(XDATA_LAI(JPCOVER,36,NVEGTYPE))
!
XDATA_LAI (:,:,:) = XUNDEF
!
!*  LAI from 2002 to 2007
ALLOCATE(XDATA_LAI_ALL_YEARS(JPCOVER,5*36,NVEGTYPE))
!
XDATA_LAI_ALL_YEARS (:,:,:) = XUNDEF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
ALLOCATE(XDATA_VEGTYPE(JPCOVER,NVEGTYPE))
!
XDATA_VEGTYPE(:,:) = 0.
!
!-------------------------------------------------------------------------------
!
!*    2.3   height of trees (m)
!            ---------------
!
ALLOCATE(XDATA_H_TREE(JPCOVER,NVEGTYPE))
!
XDATA_H_TREE (:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    2.4    ground depth
!            ------------
!
ALLOCATE(XDATA_GROUND_DEPTH(JPCOVER,NVEGTYPE))
!
XDATA_GROUND_DEPTH(:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    2.4    root depth
!            ----------
!
ALLOCATE(XDATA_ROOT_DEPTH(JPCOVER,NVEGTYPE))
!
XDATA_ROOT_DEPTH(:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    2.4    ice depth for runoff
!            --------------------
!
ALLOCATE(XDATA_DICE(JPCOVER,NVEGTYPE))
!
XDATA_DICE(:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    2.4    root extinction
!            ---------------
!
ALLOCATE(XDATA_ROOT_EXTINCTION(JPCOVER,NVEGTYPE))
!
XDATA_ROOT_EXTINCTION(:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    2.4    ponderation coefficient between root formulations
!            -------------------------------------------------
!
ALLOCATE(XDATA_ROOT_LIN(JPCOVER,NVEGTYPE))
!
XDATA_ROOT_LIN(:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    2.5    albnir (veg only)
!            ------
!
ALLOCATE(XDATA_ALBNIR_VEG(JPCOVER,NVEGTYPE))
!
XDATA_ALBNIR_VEG (:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    2.6    albvis (veg only)
!            ------
!
ALLOCATE(XDATA_ALBVIS_VEG(JPCOVER,NVEGTYPE))
!
XDATA_ALBVIS_VEG (:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    2.6    albUV (veg only)
!            -----
!
ALLOCATE(XDATA_ALBUV_VEG(JPCOVER,NVEGTYPE))
!
XDATA_ALBUV_VEG (:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
ALLOCATE(XDATA_ALB_VEG_VIS(JPCOVER,36,NVEGTYPE))
!
XDATA_ALB_VEG_VIS (:,:,:) = XUNDEF
!
ALLOCATE(XDATA_ALB_VEG_NIR(JPCOVER,36,NVEGTYPE))
!
XDATA_ALB_VEG_NIR (:,:,:) = XUNDEF
!
ALLOCATE(XDATA_ALB_SOIL_VIS(JPCOVER,36,NVEGTYPE))
!
XDATA_ALB_SOIL_VIS (:,:,:) = XUNDEF
!
ALLOCATE(XDATA_ALB_SOIL_NIR(JPCOVER,36,NVEGTYPE))
!
XDATA_ALB_SOIL_NIR (:,:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    2.7    Rsmin
!            -----
!
ALLOCATE(XDATA_RSMIN(JPCOVER,NVEGTYPE))
!
XDATA_RSMIN (:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    2.8    Gamma
!            -----
!
ALLOCATE(XDATA_GAMMA(JPCOVER,NVEGTYPE))
!
XDATA_GAMMA (:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    2.8    Wrmax_cf
!            --------
!
ALLOCATE(XDATA_WRMAX_CF(JPCOVER,NVEGTYPE))
!
XDATA_WRMAX_CF (:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    2.9    Rgl
!            ---
!
ALLOCATE(XDATA_RGL(JPCOVER,NVEGTYPE))
!
XDATA_RGL (:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    2.10   Cv
!            --
!
ALLOCATE(XDATA_CV(JPCOVER,NVEGTYPE))
!
XDATA_CV (:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    2.11   mesophyll conductance (m s-1)
!            -----------------------------
!
ALLOCATE(XDATA_GMES(JPCOVER,NVEGTYPE))
!
XDATA_GMES(:,:) = XUNDEF
!
ALLOCATE(XDATA_GMES_ST(JPCOVER,NVEGTYPE))
!
XDATA_GMES_ST(:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    2.11   Ecosystem respiration (kg/kg.m.s-1)
!            -----------------------------------
!
ALLOCATE(XDATA_RE25(JPCOVER,NVEGTYPE))
!
XDATA_RE25(:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    2.11   cuticular conductance (m s-1)
!            -----------------------------
!
ALLOCATE(XDATA_GC(JPCOVER,NVEGTYPE))
!
XDATA_GC(:,:) = XUNDEF
!
ALLOCATE(XDATA_GC_ST(JPCOVER,NVEGTYPE))
!
XDATA_GC_ST(:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    2.11   critical normilized soil water content for stress parameterisation
!            ------------------------------------------------------------------
!
ALLOCATE(XDATA_F2I(JPCOVER,NVEGTYPE))
!
XDATA_F2I(:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    2.12   ratio d(biomass)/d(lai) (kg/m2)
!            -----------------------
!
ALLOCATE(XDATA_BSLAI(JPCOVER,NVEGTYPE))
!
XDATA_BSLAI (:,:) = XUNDEF
!
ALLOCATE(XDATA_BSLAI_ST(JPCOVER,NVEGTYPE))
!
XDATA_BSLAI_ST(:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    2.12   maximum air saturation deficit tolerate by vegetation (kg/kg)
!            -------------------------------------------------------------
!
ALLOCATE(XDATA_DMAX(JPCOVER,NVEGTYPE))
!
XDATA_DMAX (:,:) = XUNDEF
!
ALLOCATE(XDATA_DMAX_ST(JPCOVER,NVEGTYPE))
!
XDATA_DMAX_ST(:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    2.11   Defensive/Offensive strategy
!            ----------------------------
!
ALLOCATE(XDATA_STRESS(JPCOVER,NVEGTYPE))
!
XDATA_STRESS(:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    2.13   e-folding time for senescence (days)
!            ------------------------------------
!
ALLOCATE(XDATA_SEFOLD(JPCOVER,NVEGTYPE))
!
XDATA_SEFOLD (:,:) = XUNDEF
!
ALLOCATE(XDATA_SEFOLD_ST(JPCOVER,NVEGTYPE))
!
XDATA_SEFOLD_ST(:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    2.14   Minimum LAI (m2/m2)
!            -------------------
!
ALLOCATE(XDATA_LAIMIN(JPCOVER,NVEGTYPE))
!
XDATA_LAIMIN (:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    2.15   vegetation and greeness fraction fractions
!            ------------------------------------------
!
ALLOCATE(XDATA_VEG(JPCOVER,36,NVEGTYPE))
!
XDATA_VEG (:,:,:) = XUNDEF
!
ALLOCATE(XDATA_GREEN(JPCOVER,36,NVEGTYPE))
!
XDATA_GREEN (:,:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    2.16   z0veg
!            -----
!
ALLOCATE(XDATA_Z0(JPCOVER,36,NVEGTYPE))
!
XDATA_Z0 (:,:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    2.17   z0hveg
!            ------
!
ALLOCATE(XDATA_Z0_O_Z0H(JPCOVER,NVEGTYPE))
!
XDATA_Z0_O_Z0H (:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    2.18   emissivity
!            ----------
!
ALLOCATE(XDATA_EMIS_ECO(JPCOVER,36,NVEGTYPE))
!
XDATA_EMIS_ECO (:,:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    2.19   for chemistry deposition
!            ------------------------
!
ALLOCATE(XDATA_SOILRC_SO2(JPCOVER,NVEGTYPE))
ALLOCATE(XDATA_SOILRC_O3 (JPCOVER,NVEGTYPE))
!
XDATA_SOILRC_SO2 (:,:) = XUNDEF
XDATA_SOILRC_O3  (:,:) = XUNDEF
!
!------------------------------------------------------------------------
!
!*    2.20   leaf aera ratio sensitivity to nitrogen concentration
!            -----------------------------------------------------
!
ALLOCATE(XDATA_CE_NITRO(JPCOVER,NVEGTYPE))
!
XDATA_CE_NITRO (:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    2.21   lethal minimum value of leaf area ratio
!            ---------------------------------------
!
ALLOCATE(XDATA_CF_NITRO(JPCOVER,NVEGTYPE))
!
XDATA_CF_NITRO (:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    2.22   nitrogen concentration of active biomass
!            ----------------------------------------
!
ALLOCATE(XDATA_CNA_NITRO(JPCOVER,NVEGTYPE))
!
XDATA_CNA_NITRO (:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    2.24   seeding and reaping dates
!            -------------------------
!
ALLOCATE(TDATA_SEED(JPCOVER,NVEGTYPE))
!
TDATA_SEED (:,:)%TDATE%YEAR  = 9999
TDATA_SEED (:,:)%TDATE%MONTH = NUNDEF
TDATA_SEED (:,:)%TDATE%DAY   = NUNDEF
TDATA_SEED (:,:)%TIME        = 0.
!
ALLOCATE(TDATA_REAP(JPCOVER,NVEGTYPE))
!
TDATA_REAP (:,:)%TDATE%YEAR  = 9999
TDATA_REAP (:,:)%TDATE%MONTH = NUNDEF
TDATA_REAP (:,:)%TDATE%DAY   = NUNDEF
TDATA_REAP (:,:)%TIME        = 0.
!
!-------------------------------------------------------------------------------
!
!*    2.25   irrigated fraction
!            ------------------
!
ALLOCATE(XDATA_IRRIG(JPCOVER,NVEGTYPE))
!
XDATA_IRRIG (:,:) = 0.
!
!-------------------------------------------------------------------------------
!
!*    2.25   water supply
!            ------------
!
ALLOCATE(XDATA_WATSUP(JPCOVER,NVEGTYPE))
!
XDATA_WATSUP (:,:) = 0.
!
!-------------------------------------------------------------------------------
!
!*    2.26   For multi-energy balance (MEB)
!            ------------------------------
!
ALLOCATE(XDATA_GNDLITTER(JPCOVER,36,NVEGTYPE))
XDATA_GNDLITTER (:,:,:) = XUNDEF
!
ALLOCATE(XDATA_Z0LITTER(JPCOVER,36,NVEGTYPE))
XDATA_Z0LITTER (:,:,:) = XUNDEF
!
ALLOCATE(XDATA_H_VEG(JPCOVER,36,NVEGTYPE))
XDATA_H_VEG (:,:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
NUT_CPHR = SUM(NTYPE(1:3)) + 1
NUT_CPMR = SUM(NTYPE(1:3)) + 2
NUT_CPLR = SUM(NTYPE(1:3)) + 3
NUT_OPHR = SUM(NTYPE(1:3)) + 4
NUT_OPMR = SUM(NTYPE(1:3)) + 5
NUT_OPLR = SUM(NTYPE(1:3)) + 6
NUT_LWLR = SUM(NTYPE(1:3)) + 7
NUT_LALR = SUM(NTYPE(1:3)) + 8
NUT_SPAR = SUM(NTYPE(1:3)) + 9
NUT_INDU = SUM(NTYPE(1:3)) + 10
!
!*    3.1    z0 for artificial surfaces
!            --------------------------
!
ALLOCATE(XDATA_Z0_TOWN(JPCOVER))
!
XDATA_Z0_TOWN (:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    3.2    albedo for artificial surfaces
!            ------------------------------
!
ALLOCATE(XDATA_ALB_ROOF(JPCOVER))
!
XDATA_ALB_ROOF (:) = XUNDEF
!
ALLOCATE(XDATA_ALB_ROAD(JPCOVER))
!
XDATA_ALB_ROAD (:) = XUNDEF
!
ALLOCATE(XDATA_ALB_WALL(JPCOVER))
!
XDATA_ALB_WALL (:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    3.4    emissivity for artificial surfaces
!            ----------------------------------
!
ALLOCATE(XDATA_EMIS_ROOF(JPCOVER))
!
XDATA_EMIS_ROOF (:) = XUNDEF
!
ALLOCATE(XDATA_EMIS_ROAD(JPCOVER))
!
XDATA_EMIS_ROAD (:) = XUNDEF
!
ALLOCATE(XDATA_EMIS_WALL(JPCOVER))
!
XDATA_EMIS_WALL (:) = XUNDEF
!
!-------------------------------------------------------------------------------
NDATA_ROOF_LAYER=3
NDATA_ROAD_LAYER=3
NDATA_WALL_LAYER=3
NDATA_FLOOR_LAYER=3
!-------------------------------------------------------------------------------
!
!*    3.5    heat capacity for artificial surfaces
!            -------------------------------------
!
ALLOCATE(XDATA_HC_ROOF(JPCOVER,NDATA_ROOF_LAYER))
!
XDATA_HC_ROOF (:,:) = XUNDEF
!
ALLOCATE(XDATA_HC_ROAD(JPCOVER,NDATA_ROAD_LAYER))
!
XDATA_HC_ROAD (:,:) = XUNDEF
!
ALLOCATE(XDATA_HC_WALL(JPCOVER,NDATA_WALL_LAYER))
!
XDATA_HC_WALL (:,:) = XUNDEF
!
ALLOCATE(XDATA_HC_FLOOR(JPCOVER,NDATA_FLOOR_LAYER))
!
XDATA_HC_FLOOR (:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    3.6    thermal conductivity for artificial surfaces
!            --------------------------------------------
!
ALLOCATE(XDATA_TC_ROOF(JPCOVER,NDATA_ROOF_LAYER))
!
XDATA_TC_ROOF (:,:) = XUNDEF
!
ALLOCATE(XDATA_TC_ROAD(JPCOVER,NDATA_ROAD_LAYER))
!
XDATA_TC_ROAD (:,:) = XUNDEF
!
ALLOCATE(XDATA_TC_WALL(JPCOVER,NDATA_WALL_LAYER))
!
XDATA_TC_WALL (:,:) = XUNDEF
!
ALLOCATE(XDATA_TC_FLOOR(JPCOVER,NDATA_FLOOR_LAYER))
!
XDATA_TC_FLOOR (:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    3.7    depth for artificial surfaces layers
!            ------------------------------------
!
ALLOCATE(XDATA_D_ROOF(JPCOVER,NDATA_ROOF_LAYER))
!
XDATA_D_ROOF (:,:) = XUNDEF
!
ALLOCATE(XDATA_D_ROAD(JPCOVER,NDATA_ROAD_LAYER))
!
XDATA_D_ROAD (:,:) = XUNDEF
!
ALLOCATE(XDATA_D_WALL(JPCOVER,NDATA_WALL_LAYER))
!
XDATA_D_WALL (:,:) = XUNDEF
!
ALLOCATE(XDATA_D_FLOOR(JPCOVER,NDATA_FLOOR_LAYER))
!
XDATA_D_FLOOR (:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    3.8    building height
!            ---------------
!
ALLOCATE(XDATA_BLD_HEIGHT(JPCOVER))
!
XDATA_BLD_HEIGHT (:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    3.9    building shape
!            --------------
!
ALLOCATE(XDATA_WALL_O_HOR(JPCOVER))
!
XDATA_WALL_O_HOR (:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    3.10   building fraction
!            -----------------
!
ALLOCATE(XDATA_BLD(JPCOVER))
!
XDATA_BLD (:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    3.11   canyon shape
!            ------------
!
ALLOCATE(XDATA_CAN_HW_RATIO(JPCOVER))
!
XDATA_CAN_HW_RATIO (:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    3.12   anthropogenic fluxes
!            --------------------
!
ALLOCATE(XDATA_H_TRAFFIC  (JPCOVER))
ALLOCATE(XDATA_LE_TRAFFIC (JPCOVER))
ALLOCATE(XDATA_H_INDUSTRY (JPCOVER))
ALLOCATE(XDATA_LE_INDUSTRY(JPCOVER))
!
XDATA_H_TRAFFIC  (:) = XUNDEF
XDATA_LE_TRAFFIC (:) = XUNDEF
XDATA_H_INDUSTRY (:) = XUNDEF
XDATA_LE_INDUSTRY(:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    3.13   For TEB-BEM
!            ------------
!
ALLOCATE(XDATA_TCOOL_TARGET (JPCOVER))
ALLOCATE(XDATA_THEAT_TARGET (JPCOVER))
!
XDATA_TCOOL_TARGET (:) = XUNDEF
XDATA_THEAT_TARGET (:) = XUNDEF
!
ALLOCATE(XDATA_F_WASTE_CAN (JPCOVER))
ALLOCATE(XDATA_EFF_HEAT    (JPCOVER))
ALLOCATE(XDATA_QIN         (JPCOVER))
ALLOCATE(XDATA_QIN_FRAD    (JPCOVER))
ALLOCATE(XDATA_SHGC        (JPCOVER))
ALLOCATE(XDATA_U_WIN       (JPCOVER))
ALLOCATE(XDATA_GR          (JPCOVER))
ALLOCATE(XDATA_SHGC_SH     (JPCOVER))
ALLOCATE(XDATA_FLOOR_HEIGHT(JPCOVER))
ALLOCATE(XDATA_INF         (JPCOVER))
!
XDATA_F_WASTE_CAN (:) = XUNDEF
XDATA_EFF_HEAT    (:) = XUNDEF
XDATA_QIN         (:) = XUNDEF
XDATA_QIN_FRAD    (:) = XUNDEF
XDATA_SHGC        (:) = XUNDEF
XDATA_U_WIN       (:) = XUNDEF
XDATA_GR          (:) = XUNDEF
XDATA_SHGC_SH     (:) = XUNDEF
XDATA_FLOOR_HEIGHT(:) = XUNDEF
XDATA_INF         (:) = XUNDEF
!
ALLOCATE(XDATA_F_WATER_COND(JPCOVER))
ALLOCATE(XDATA_QIN_FLAT    (JPCOVER))
ALLOCATE(XDATA_HR_TARGET   (JPCOVER))
ALLOCATE(XDATA_V_VENT      (JPCOVER))
ALLOCATE(XDATA_CAP_SYS_HEAT(JPCOVER))
ALLOCATE(XDATA_CAP_SYS_RAT (JPCOVER))
ALLOCATE(XDATA_T_ADP       (JPCOVER))
ALLOCATE(XDATA_M_SYS_RAT   (JPCOVER))
ALLOCATE(XDATA_COP_RAT     (JPCOVER))
ALLOCATE(XDATA_T_SIZE_MAX  (JPCOVER))
ALLOCATE(XDATA_T_SIZE_MIN  (JPCOVER))
ALLOCATE(XDATA_SHADE       (JPCOVER))
ALLOCATE(XDATA_NATVENT     (JPCOVER))
!
XDATA_F_WATER_COND(:) = XUNDEF
XDATA_QIN_FLAT    (:) = XUNDEF
XDATA_HR_TARGET   (:) = XUNDEF
XDATA_V_VENT      (:) = XUNDEF
XDATA_CAP_SYS_HEAT(:) = XUNDEF
XDATA_CAP_SYS_RAT (:) = XUNDEF
XDATA_T_ADP       (:) = XUNDEF
XDATA_M_SYS_RAT   (:) = XUNDEF
XDATA_COP_RAT     (:) = XUNDEF
XDATA_T_SIZE_MAX  (:) = XUNDEF
XDATA_T_SIZE_MIN  (:) = XUNDEF
XDATA_SHADE       (:) = 0.
XDATA_NATVENT     (:) = 0.
!
ALLOCATE(XDATA_ROUGH_ROOF (JPCOVER))
ALLOCATE(XDATA_ROUGH_WALL (JPCOVER))
XDATA_ROUGH_ROOF(:) = XUNDEF
XDATA_ROUGH_WALL(:) = XUNDEF
!
ALLOCATE(XDATA_RESIDENTIAL (JPCOVER))
XDATA_RESIDENTIAL(:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    3.13   For greenroof
!            -------------
!
ALLOCATE(XDATA_FRAC_GR (JPCOVER))
!
XDATA_FRAC_GR (:) = 0.
!
!-------------------------------------------------------------------------------
!
!*    3.14   For solar panels
!            ----------------
!
ALLOCATE(XDATA_EMIS_PANEL (JPCOVER))
ALLOCATE(XDATA_ALB_PANEL  (JPCOVER))
ALLOCATE(XDATA_EFF_PANEL  (JPCOVER))
ALLOCATE(XDATA_FRAC_PANEL (JPCOVER))
!
XDATA_EMIS_PANEL (:) = XUNDEF
XDATA_ALB_PANEL  (:) = XUNDEF
XDATA_EFF_PANEL  (:) = XUNDEF
XDATA_FRAC_PANEL (:) = XUNDEF
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
!*    4.     CALL to INITIALIZATION ROUTINES
!            -------------------------------
!
!-------------------------------------------------------------------------------
!
!*   4.1   first version of ecoclimap (global)
!          -----------------------------------
!
!* Global data set
!
IF (U%LECOSG) THEN
  CALL INIT_TYPES_PARAM
ELSEIF (LREAD_DATA_COVER) THEN
  CALL READ_COVERS_PARAM(1)
ELSE
  CALL DEFAULT_DATA_COVER(XDATA_TOWN,XDATA_NATURE,XDATA_WATER,XDATA_SEA,        &
         XDATA_Z0_TOWN,XDATA_BLD_HEIGHT,XDATA_WALL_O_HOR,XDATA_BLD,XDATA_GARDEN,&
         XDATA_ALB_ROOF,XDATA_ALB_ROAD,XDATA_ALB_WALL,XDATA_EMIS_ROOF,          &
         XDATA_EMIS_ROAD,XDATA_EMIS_WALL,XDATA_HC_ROOF,XDATA_TC_ROOF,           &
         XDATA_D_ROOF,XDATA_HC_ROAD,XDATA_TC_ROAD,XDATA_D_ROAD,                 &
         XDATA_HC_WALL,XDATA_TC_WALL,XDATA_D_WALL,XDATA_H_TRAFFIC,              &
         XDATA_LE_TRAFFIC,XDATA_H_INDUSTRY,XDATA_LE_INDUSTRY,                   &
         XDATA_VEGTYPE,XDATA_H_TREE,XDATA_WATSUP,XDATA_IRRIG,                   &
         XDATA_ROOT_DEPTH,XDATA_GROUND_DEPTH,XDATA_DICE,TDATA_SEED,             &
         TDATA_REAP)
  !
  CALL DEFAULT_LAI_ECO1_01
  CALL DEFAULT_LAI_ECO1_02
  CALL DEFAULT_LAI_ECO1_03
  CALL DEFAULT_LAI_ECO1_04
  CALL DEFAULT_LAI_ECO1_05
  CALL DEFAULT_LAI_ECO1_06
  CALL DEFAULT_LAI_ECO1_07
  CALL DEFAULT_LAI_ECO1_08
  CALL DEFAULT_LAI_ECO1_09
  CALL DEFAULT_LAI_ECO1_10
  CALL DEFAULT_LAI_ECO1_11
  CALL DEFAULT_LAI_ECO1_12
  CALL DEFAULT_LAI_ECO1_13
  CALL DEFAULT_LAI_ECO1_14
  CALL DEFAULT_LAI_ECO1_15
  CALL DEFAULT_LAI_ECO1_16
  CALL DEFAULT_LAI_ECO1_17
  CALL DEFAULT_LAI_ECO1_18
  CALL DEFAULT_LAI_ECO1_19
  !
  CALL DEFAULT_ALB_SOIL_ECO1
  CALL DEFAULT_ALB_SOIL_ECO2
  !
  CALL DEFAULT_ALB_VEG_ECO1_01
  CALL DEFAULT_ALB_VEG_ECO1_02
  CALL DEFAULT_ALB_VEG_ECO1_03
  CALL DEFAULT_ALB_VEG_ECO1_04
  CALL DEFAULT_ALB_VEG_ECO1_05
  CALL DEFAULT_ALB_VEG_ECO1_06
  CALL DEFAULT_ALB_VEG_ECO1_07
  CALL DEFAULT_ALB_VEG_ECO1_08
  CALL DEFAULT_ALB_VEG_ECO1_09
  CALL DEFAULT_ALB_VEG_ECO1_10
  CALL DEFAULT_ALB_VEG_ECO1_11
  CALL DEFAULT_ALB_VEG_ECO1_12
  CALL DEFAULT_ALB_VEG_ECO1_13
  CALL DEFAULT_ALB_VEG_ECO1_14
  CALL DEFAULT_ALB_VEG_ECO1_15
  CALL DEFAULT_ALB_VEG_ECO1_16
  CALL DEFAULT_ALB_VEG_ECO1_17
  CALL DEFAULT_ALB_VEG_ECO1_18
  CALL DEFAULT_ALB_VEG_ECO1_19
ENDIF
!
IF (.NOT.U%LECOSG) CALL COVER301_573
!
!-------------------------------------------------------------------------------
!
!default values for ECOSG
IF (U%LECOSG) THEN

  !lai
  DO JDEC = 1,SIZE(XDATA_LAI,2)
    WHERE(XDATA_VEGTYPE(:,1)>0.) XDATA_LAI(:,JDEC,1) = 0.
    WHERE(XDATA_VEGTYPE(:,2)>0.) XDATA_LAI(:,JDEC,2) = 0.
    WHERE(XDATA_VEGTYPE(:,3)>0.) XDATA_LAI(:,JDEC,3) = 0.
    WHERE(XDATA_VEGTYPE(:,4)>0.) XDATA_LAI(:,JDEC,4) = 4.
    WHERE(XDATA_VEGTYPE(:,5)>0.) XDATA_LAI(:,JDEC,5) = 4.
    WHERE(XDATA_VEGTYPE(:,6)>0.) XDATA_LAI(:,JDEC,6) = 4.
    WHERE(XDATA_VEGTYPE(:,7)>0.) XDATA_LAI(:,JDEC,7) = 4.
    WHERE(XDATA_VEGTYPE(:,8)>0.) XDATA_LAI(:,JDEC,8) = 4.
    WHERE(XDATA_VEGTYPE(:,9)>0.) XDATA_LAI(:,JDEC,9) = 4.
    WHERE(XDATA_VEGTYPE(:,10)>0.) XDATA_LAI(:,JDEC,10) = 4.
    WHERE(XDATA_VEGTYPE(:,11)>0.) XDATA_LAI(:,JDEC,11) = 4.
    WHERE(XDATA_VEGTYPE(:,12)>0.) XDATA_LAI(:,JDEC,12) = 4.
    WHERE(XDATA_VEGTYPE(:,13)>0.) XDATA_LAI(:,JDEC,13) = 4.
    WHERE(XDATA_VEGTYPE(:,14)>0.) XDATA_LAI(:,JDEC,14) = 3.
    WHERE(XDATA_VEGTYPE(:,15)>0.) XDATA_LAI(:,JDEC,15) = 3.
    WHERE(XDATA_VEGTYPE(:,16)>0.) XDATA_LAI(:,JDEC,16) = 2.
    WHERE(XDATA_VEGTYPE(:,17)>0.) XDATA_LAI(:,JDEC,17) = 2.
    WHERE(XDATA_VEGTYPE(:,18)>0.) XDATA_LAI(:,JDEC,18) = 2.
    WHERE(XDATA_VEGTYPE(:,19)>0.) XDATA_LAI(:,JDEC,19) = 4.
    WHERE(XDATA_VEGTYPE(:,20)>0.) XDATA_LAI(:,JDEC,20) = 3.
  ENDDO

  !root_depth
  WHERE(XDATA_VEGTYPE(:,1)>0.) XDATA_ROOT_DEPTH(:,1) = 0.5
  WHERE(XDATA_VEGTYPE(:,2)>0.) XDATA_ROOT_DEPTH(:,2) = 0.2
  WHERE(XDATA_VEGTYPE(:,3)>0.) XDATA_ROOT_DEPTH(:,3) = 0.2
  WHERE(XDATA_VEGTYPE(:,4)>0.) XDATA_ROOT_DEPTH(:,4) = 2.0
  WHERE(XDATA_VEGTYPE(:,5)>0.) XDATA_ROOT_DEPTH(:,5) = 2.0
  WHERE(XDATA_VEGTYPE(:,6)>0.) XDATA_ROOT_DEPTH(:,6) = 3.0
  WHERE(XDATA_VEGTYPE(:,7)>0.) XDATA_ROOT_DEPTH(:,7) = 2.0
  WHERE(XDATA_VEGTYPE(:,8)>0.) XDATA_ROOT_DEPTH(:,8) = 5.0
  WHERE(XDATA_VEGTYPE(:,9)>0.) XDATA_ROOT_DEPTH(:,9) = 2.0
  WHERE(XDATA_VEGTYPE(:,10)>0.) XDATA_ROOT_DEPTH(:,10) = 2.0
  WHERE(XDATA_VEGTYPE(:,11)>0.) XDATA_ROOT_DEPTH(:,11) = 2.0
  WHERE(XDATA_VEGTYPE(:,12)>0.) XDATA_ROOT_DEPTH(:,12) = 2.0
  WHERE(XDATA_VEGTYPE(:,13)>0.) XDATA_ROOT_DEPTH(:,13) = 1.0
  WHERE(XDATA_VEGTYPE(:,14)>0.) XDATA_ROOT_DEPTH(:,14) = 1.0
  WHERE(XDATA_VEGTYPE(:,15)>0.) XDATA_ROOT_DEPTH(:,15) = 1.5
  WHERE(XDATA_VEGTYPE(:,16)>0.) XDATA_ROOT_DEPTH(:,16) = 1.5
  WHERE(XDATA_VEGTYPE(:,17)>0.) XDATA_ROOT_DEPTH(:,17) = 1.5
  WHERE(XDATA_VEGTYPE(:,18)>0.) XDATA_ROOT_DEPTH(:,18) = 1.5
  WHERE(XDATA_VEGTYPE(:,19)>0.) XDATA_ROOT_DEPTH(:,19) = 2.0
  WHERE(XDATA_VEGTYPE(:,20)>0.) XDATA_ROOT_DEPTH(:,20) = 1.0

  !soil_depth
  WHERE(XDATA_VEGTYPE(:,1)>0.) XDATA_GROUND_DEPTH(:,1) = 1.0
  WHERE(XDATA_VEGTYPE(:,2)>0.) XDATA_GROUND_DEPTH(:,2) = 0.2
  WHERE(XDATA_VEGTYPE(:,3)>0.) XDATA_GROUND_DEPTH(:,3) = 0.2
  WHERE(XDATA_VEGTYPE(:,4)>0.) XDATA_GROUND_DEPTH(:,4) = 3.0
  WHERE(XDATA_VEGTYPE(:,5)>0.) XDATA_GROUND_DEPTH(:,5) = 3.0
  WHERE(XDATA_VEGTYPE(:,6)>0.) XDATA_GROUND_DEPTH(:,6) = 3.0
  WHERE(XDATA_VEGTYPE(:,7)>0.) XDATA_GROUND_DEPTH(:,7) = 3.0
  WHERE(XDATA_VEGTYPE(:,8)>0.) XDATA_GROUND_DEPTH(:,8) = 8.0
  WHERE(XDATA_VEGTYPE(:,9)>0.) XDATA_GROUND_DEPTH(:,9) = 3.0
  WHERE(XDATA_VEGTYPE(:,10)>0.) XDATA_GROUND_DEPTH(:,10) = 3.0
  WHERE(XDATA_VEGTYPE(:,11)>0.) XDATA_GROUND_DEPTH(:,11) = 3.0
  WHERE(XDATA_VEGTYPE(:,12)>0.) XDATA_GROUND_DEPTH(:,12) = 3.0
  WHERE(XDATA_VEGTYPE(:,13)>0.) XDATA_GROUND_DEPTH(:,13) = 1.5
  WHERE(XDATA_VEGTYPE(:,14)>0.) XDATA_GROUND_DEPTH(:,14) = 1.5
  WHERE(XDATA_VEGTYPE(:,15)>0.) XDATA_GROUND_DEPTH(:,15) = 2.0
  WHERE(XDATA_VEGTYPE(:,16)>0.) XDATA_GROUND_DEPTH(:,16) = 2.0
  WHERE(XDATA_VEGTYPE(:,17)>0.) XDATA_GROUND_DEPTH(:,17) = 2.0
  WHERE(XDATA_VEGTYPE(:,18)>0.) XDATA_GROUND_DEPTH(:,18) = 2.0
  WHERE(XDATA_VEGTYPE(:,19)>0.) XDATA_GROUND_DEPTH(:,19) = 2.0
  WHERE(XDATA_VEGTYPE(:,20)>0.) XDATA_GROUND_DEPTH(:,20) = 1.5

  !height_trees
  WHERE(XDATA_VEGTYPE(:,4)>0.) XDATA_H_TREE(:,4) = 15.0
  WHERE(XDATA_VEGTYPE(:,5)>0.) XDATA_H_TREE(:,5) = 10.0
  WHERE(XDATA_VEGTYPE(:,6)>0.) XDATA_H_TREE(:,6) = 30.0
  WHERE(XDATA_VEGTYPE(:,7)>0.) XDATA_H_TREE(:,7) = 10.0
  WHERE(XDATA_VEGTYPE(:,8)>0.) XDATA_H_TREE(:,8) = 30.0
  WHERE(XDATA_VEGTYPE(:,9)>0.) XDATA_H_TREE(:,9) = 15.0
  WHERE(XDATA_VEGTYPE(:,10)>0.) XDATA_H_TREE(:,10) = 10.0
  WHERE(XDATA_VEGTYPE(:,11)>0.) XDATA_H_TREE(:,11) = 15.0
  WHERE(XDATA_VEGTYPE(:,12)>0.) XDATA_H_TREE(:,12) = 1.0

  !ice_depth
  WHERE(XDATA_VEGTYPE(:,1)>0.) XDATA_DICE(:,1) = 0.5
  WHERE(XDATA_VEGTYPE(:,2)>0.) XDATA_DICE(:,2) = 0.2
  WHERE(XDATA_VEGTYPE(:,3)>0.) XDATA_DICE(:,3) = 0.2
  WHERE(XDATA_VEGTYPE(:,4)>0.) XDATA_DICE(:,4) = 1.0
  WHERE(XDATA_VEGTYPE(:,5)>0.) XDATA_DICE(:,5) = 1.5
  WHERE(XDATA_VEGTYPE(:,6)>0.) XDATA_DICE(:,6) = 3.0
  WHERE(XDATA_VEGTYPE(:,7)>0.) XDATA_DICE(:,7) = 2.0
  WHERE(XDATA_VEGTYPE(:,8)>0.) XDATA_DICE(:,8) = 8.0
  WHERE(XDATA_VEGTYPE(:,9)>0.) XDATA_DICE(:,9) = 1.0
  WHERE(XDATA_VEGTYPE(:,10)>0.) XDATA_DICE(:,10) = 1.5
  WHERE(XDATA_VEGTYPE(:,11)>0.) XDATA_DICE(:,11) = 0.3
  WHERE(XDATA_VEGTYPE(:,12)>0.) XDATA_DICE(:,12) = 1.5
  WHERE(XDATA_VEGTYPE(:,13)>0.) XDATA_DICE(:,13) = 0.3
  WHERE(XDATA_VEGTYPE(:,14)>0.) XDATA_DICE(:,14) = 1.0
  WHERE(XDATA_VEGTYPE(:,15)>0.) XDATA_DICE(:,15) = 1.5
  WHERE(XDATA_VEGTYPE(:,16)>0.) XDATA_DICE(:,16) = 1.0
  WHERE(XDATA_VEGTYPE(:,17)>0.) XDATA_DICE(:,17) = 1.2
  WHERE(XDATA_VEGTYPE(:,18)>0.) XDATA_DICE(:,18) = 1.5
  WHERE(XDATA_VEGTYPE(:,19)>0.) XDATA_DICE(:,19) = 1.5
  WHERE(XDATA_VEGTYPE(:,20)>0.) XDATA_DICE(:,20) = 1.0

  DO JCOV = 1,JPCOVER
    DO JVEG = 1,NVEGTYPE
      IF (XDATA_VEGTYPE(JCOV,JVEG)/=0.) THEN
        !alb_soil_nir
        XDATA_ALB_SOIL_NIR(JCOV,:,JVEG) = 0.3
        !alb_soil_vis
        XDATA_ALB_SOIL_VIS(JCOV,:,JVEG) = 0.1
        !alb_veg_nir
        XDATA_ALB_VEG_NIR(JCOV,:,JVEG) = 0.3
        !alb_veg_vis
        XDATA_ALB_VEG_VIS(JCOV,:,JVEG) = 0.1
      ENDIF
    ENDDO
  ENDDO

  WHERE(XDATA_TOWN(:)>0.)
    XDATA_ALB_ROOF  (:) = 0.15
    XDATA_ALB_ROAD  (:) = 0.08
    XDATA_ALB_WALL  (:) = 0.25
    XDATA_EMIS_ROOF (:) = 0.90
    XDATA_EMIS_ROAD (:) = 0.94
    XDATA_EMIS_WALL (:) = 0.85
    XDATA_HC_ROOF (:,1) = 2.11 * 1.E6
    XDATA_HC_ROOF (:,2) = 0.28 * 1.E6
    XDATA_HC_ROOF (:,3) = 0.29 * 1.E6
    XDATA_TC_ROOF (:,1) = 1.5100
    XDATA_TC_ROOF (:,2) = 0.0800
    XDATA_TC_ROOF (:,3) = 0.0500
    XDATA_D_ROOF  (:,1) = 0.050
    XDATA_D_ROOF  (:,2) = 0.400
    XDATA_D_ROOF  (:,3) = 0.100
    XDATA_HC_ROAD (:,1) = 1.94 * 1.E6
    XDATA_HC_ROAD (:,2) = 1.28 * 1.E6
    XDATA_HC_ROAD (:,3) = 1.28 * 1.E6
    XDATA_TC_ROAD (:,1) = 0.7454
    XDATA_TC_ROAD (:,2) = 0.2513
    XDATA_TC_ROAD (:,3) = 0.2513
    XDATA_D_ROAD  (:,1) = 0.050
    XDATA_D_ROAD  (:,2) = 0.100
    XDATA_D_ROAD  (:,3) = 1.000
    XDATA_HC_WALL (:,1) = 1.55 * 1.E6
    XDATA_HC_WALL (:,2) = 1.55 * 1.E6
    XDATA_HC_WALL (:,3) = 0.29 * 1.E6
    XDATA_TC_WALL (:,1) = 0.9338
    XDATA_TC_WALL (:,2) = 0.9338
    XDATA_TC_WALL (:,3) = 0.0500
    XDATA_D_WALL  (:,1) = 0.020
    XDATA_D_WALL  (:,2) = 0.125
    XDATA_D_WALL  (:,3) = 0.050
    XDATA_LE_TRAFFIC(:) = 0.
    XDATA_LE_INDUSTRY(:)= 0.
  END WHERE
  !
  WHERE(XDATA_VEGTYPE(:,8)>0.)
    XDATA_WATSUP(:,8) = 0.
    XDATA_IRRIG (:,8) = 0.00
  END WHERE
  !
  WHERE(XDATA_VEGTYPE(:,9)>0.)
    XDATA_WATSUP(:,9) = 30.
    XDATA_IRRIG (:,9) = 1.00
    TDATA_SEED  (:,9)%TDATE%DAY   = 10
    TDATA_SEED  (:,9)%TDATE%MONTH = 05
    TDATA_REAP  (:,9)%TDATE%DAY   = 01
    TDATA_REAP  (:,9)%TDATE%MONTH = 08
  END WHERE
  !
ELSE

  !For one cover, the soil albedo from CM13 is the same for each vegtype
  DO JVEG=2,NVEGTYPE
    DO JCOV = 1, JPCOVER
      XDATA_ALB_SOIL_NIR(JCOV,:,JVEG) = XDATA_ALB_SOIL_NIR(JCOV,:,1)
      XDATA_ALB_SOIL_VIS(JCOV,:,JVEG) = XDATA_ALB_SOIL_VIS(JCOV,:,1)
    ENDDO
  ENDDO

ENDIF
!
!-------------------------------------------------------------------------------
!
ICPT_SEA = 0
ICPT_WATER = 0
!
IF (U%LECOSG) THEN
  ALLOCATE(NSEA(1))
  ALLOCATE(NWATER(2))
ELSE
  ALLOCATE(NSEA(3))
  ALLOCATE(NWATER(2))
ENDIF
!
NSEA(:) = 0
NWATER(:) = 0
!
DO JCOV = 1, JPCOVER
  !
  IF (XDATA_NATURE(JCOV)==1.) THEN
    IF (XDATA_VEGTYPE(JCOV,NVT_NO  )==1.) NBARE_SOIL = JCOV
    IF (XDATA_VEGTYPE(JCOV,NVT_ROCK)==1.) NROCK = JCOV
    IF (XDATA_VEGTYPE(JCOV,NVT_SNOW)==1.) NPERMSNOW = JCOV
  END IF
  !
  IF (XDATA_SEA(JCOV)==1.) THEN
    ICPT_SEA = ICPT_SEA + 1
    IF(ICPT_SEA>SIZE(NSEA))THEN
      CALL ABOR1_SFX('INI_DATA_COVER: problem with ecoclimap param : ICPT_SEA > SIZE(NSEA) ')
    ENDIF
    NSEA(ICPT_SEA) = JCOV
  ENDIF
  !
  IF (XDATA_WATER(JCOV)==1.) THEN
    ICPT_WATER = ICPT_WATER + 1
    IF(ICPT_WATER>SIZE(NWATER))THEN
      CALL ABOR1_SFX('INI_DATA_COVER: problem with ecoclimap param : ICPT_WATER > SIZE(NWATER) ')
    ENDIF
    NWATER(ICPT_WATER) = JCOV
  ENDIF
  !
  IF (XDATA_TOWN(JCOV)==0.) CYCLE
  !
  XDATA_CAN_HW_RATIO(JCOV) = 0.5 * XDATA_WALL_O_HOR(JCOV) / (1.-XDATA_BLD (JCOV))
  !
  !* Building Energy Model variables
  !
  XDATA_HC_FLOOR(JCOV,:) = 2016000.
  XDATA_TC_FLOOR(JCOV,:) = 1.95
  XDATA_D_FLOOR(JCOV,1) = 0.01
  XDATA_D_FLOOR(JCOV,2) = 0.04
  XDATA_D_FLOOR(JCOV,3) = 0.10
  !
  XDATA_TCOOL_TARGET(JCOV) = 297.16
  XDATA_THEAT_TARGET(JCOV) = 292.16
  XDATA_F_WASTE_CAN(JCOV)  = 1.0
  XDATA_EFF_HEAT(JCOV)     = 0.9
  XDATA_QIN(JCOV)          = 5.8
  XDATA_QIN_FRAD(JCOV)     = 0.2
  XDATA_QIN_FLAT(JCOV)     = 0.2
  XDATA_SHGC(JCOV)         = 0.763
  XDATA_U_WIN(JCOV)        = 2.716
  XDATA_GR(JCOV)           = 0.3
  XDATA_SHGC_SH(JCOV)      = 0.763
  XDATA_FLOOR_HEIGHT(JCOV) = 3.0
  XDATA_INF(JCOV)          = 0.5
  XDATA_F_WATER_COND(JCOV) = 0.
  XDATA_QIN_FLAT(JCOV)     = 0.2
  XDATA_HR_TARGET(JCOV)    = 0.5
  XDATA_V_VENT(JCOV)       = 0.0
  XDATA_CAP_SYS_HEAT(JCOV) = 100.
  XDATA_CAP_SYS_RAT(JCOV)  = 90.
  XDATA_T_ADP(JCOV)        = 285.66
  XDATA_M_SYS_RAT(JCOV)    = 0.0067
  XDATA_COP_RAT(JCOV)      = 2.5
  XDATA_T_SIZE_MAX(JCOV)   = 301.95
  XDATA_T_SIZE_MIN(JCOV)   = 268.96
  XDATA_SHADE(JCOV)        = 0.0
  XDATA_NATVENT(JCOV)      = 0.0
  XDATA_ROUGH_ROOF(JCOV)   = 1.52
  XDATA_ROUGH_WALL(JCOV)   = 1.52
  XDATA_RESIDENTIAL(JCOV)  = 1.
  !
  XDATA_EMIS_PANEL (JCOV) = 0.9
  XDATA_ALB_PANEL  (JCOV) = 0.1
  XDATA_EFF_PANEL  (JCOV) = 0.14
  XDATA_FRAC_PANEL (JCOV) = 0.
  !
  IF (XDATA_GARDEN(JCOV)/=0.) THEN
    DO JVEG=1,NVEGTYPE
      IF (XDATA_VEGTYPE(JCOV,JVEG)/=0.) THEN
        XDATA_ALB_SOIL_NIR(JCOV,:,JVEG) = 0.3
        XDATA_ALB_SOIL_VIS(JCOV,:,JVEG) = 0.1
        XDATA_ALB_VEG_NIR (JCOV,:,JVEG) = 0.3
        XDATA_ALB_VEG_VIS (JCOV,:,JVEG) = 0.1
      ENDIF
    ENDDO
  ENDIF
  !
END DO
!
IF(ICPT_SEA<SIZE(NSEA))THEN
  CALL ABOR1_SFX('INI_DATA_COVER: problem with ecoclimap param : ICPT_SEA < SIZE(NSEA) ')
ENDIF
IF(ICPT_WATER<SIZE(NWATER))THEN
  CALL ABOR1_SFX('INI_DATA_COVER: problem with ecoclimap param : ICPT_WATER < SIZE(NWATER) ')
ENDIF
!
!-------------------------------------------------------------------------------
ALLOCATE(CNAMES(JPCOVER,2))
CNAMES(:,:) = ' '
!-------------------------------------------------------------------------------
!
IF (U%LECOSG) THEN

  CNAMES(1,1) = 'No vegetation (smooth)'
  CNAMES(2,1) = 'No vegetation (rocks) '
  CNAMES(3,1) = 'Permanent snow and ice'
  CNAMES(4,1) = 'temperate broadleaf cold-deciduous summergreen'
  CNAMES(5,1) = 'boreal needleleaf evergreen'
  CNAMES(6,1) = 'tropical broadleaf evergreen'
  CNAMES(7,1) = 'C3 cultures types'
  CNAMES(8,1) = 'C4 cultures types'
  CNAMES(9,1) = 'irrigated crops'
  CNAMES(10,1) = 'temperate grassland'
  CNAMES(11,1) = 'tropical grassland'
  CNAMES(12,1) = 'peat bogs, parks and gardens (irrigated grass)'
  CNAMES(13,1) = 'tropical broadleaf deciduous'
  CNAMES(14,1) = 'temperate broadleaf evergreen'
  CNAMES(15,1) = 'temperate needleleaf evergreen'
  CNAMES(16,1) = 'boreal broadleaf cold-deciduous summergreen'
  CNAMES(17,1) = 'boreal needleleaf cold-deciduous summergreen'
  CNAMES(18,1) = 'boreal grass'
  CNAMES(19,1) = 'shrub'
!
  CNAMES(20,1) = 'urban areas'
  CNAMES(21,1) = 'sea and oceans'
  CNAMES(22,1) = 'lakes'
  CNAMES(23,1) = 'rivers'
!
!-------------------------------------------------------------------------------
!
  CNAMES(1,2) = 'Sol nu'
  CNAMES(2,2) = 'Roche nue'
  CNAMES(3,2) = 'Neige permanente et glaciers'
  CNAMES(4,2) = 'Feuillus decidus en zone temperee'
  CNAMES(5,2) = 'Coniferes sempervirents en zone boreale'
  CNAMES(6,2) = 'Feuillus sempervirentsn en zone tropicale'
  CNAMES(7,2) = 'Cultures C3'
  CNAMES(8,2) = 'Cultures C4'
  CNAMES(9,2) = 'Cultures irriguées'
  CNAMES(10,2) = 'Prairies en zone temperee'
  CNAMES(11,2) = 'Prairies en zone tropicale'
  CNAMES(12,2) = 'Zone humides, parcs et jardins'
  CNAMES(13,2) = 'Feuillus decidus en zone tropicale'
  CNAMES(14,2) = 'Feuillus sempervirents en zone temperee'
  CNAMES(15,2) = 'Coniferes sempervirents en zone temperee'
  CNAMES(16,2) = 'Feuillus boreal broadleaf cold-deciduous summergreen'
  CNAMES(17,2) = 'Coniferes decidus en zone boreale'
  CNAMES(18,2) = 'Prairies en zone boreale'
  CNAMES(19,2) = 'Buissons'
!
  CNAMES(20,2) = 'Zones urbaines'
  CNAMES(21,2) = 'Mer et oceans'
  CNAMES(22,2) = 'Lacs'
  CNAMES(23,2) = 'Rivieres'

ELSE

  CNAMES(1,1) = 'Sea and ocean'
  CNAMES(2,1) = 'Lakes'
  CNAMES(3,1) = 'Rivers'
  CNAMES(4,1) = 'Bare land'
  CNAMES(5,1) = 'Rocks'
  CNAMES(6,1) = 'Permanent snow and ice'
  CNAMES(7,1) = 'Urban and built-up'

  CNAMES(8,1) = 'Tropical undefined islands'
  CNAMES(9,1) = 'Subpolar undefined islands'
!
  CNAMES(10,1) = 'S-America cool ENF'
  CNAMES(11,1) = 'Boreal ENF'
  CNAMES(12,1) = 'Asia subtropical ENF'
  CNAMES(13,1) = 'American Continental ENF'
  CNAMES(14,1) = 'American Subtropical ENF'
  CNAMES(15,1) = 'American Cool Marine ENF'

  CNAMES(16,1) = 'Africa Equatorial EBF'
  CNAMES(17,1) = 'Africa Tr. wind EBF'
  CNAMES(18,1) = 'Oceanian Equatorial EBF'
  CNAMES(19,1) = 'Asia tropical EBF'
  CNAMES(20,1) = 'Oceania tropical EBF'
  CNAMES(21,1) = 'Amazonian EBF'
  CNAMES(22,1) = 'SH subtropical EBF'
  CNAMES(23,1) = 'Cent. America Tr. wind EBF'

  CNAMES(24,1) = 'Asian boreal DNF'

  CNAMES(25,1) = 'S-America tropical DBF'
  CNAMES(26,1) = 'N-America humid continental DBF'
  CNAMES(27,1) = 'Cent. America Tr. wind DBF'
  CNAMES(28,1) = 'S-America humid subtropical DBF'

  CNAMES(29,1) = 'Africa dry tropical MF'
  CNAMES(30,1) = 'S-America cool MF'
  CNAMES(31,1) = 'NH Subpolar MF'
  CNAMES(32,1) = 'NH Humid subtropical MF'
  CNAMES(33,1) = 'NH Continental MF'

  CNAMES(34,1) = 'NH Africa WL'
  CNAMES(35,1) = 'SH Africa WL'
  CNAMES(36,1) = 'Tr. wind humid and subtrop. WL'
  CNAMES(37,1) = 'Oceanian Equatorial WL'
  CNAMES(38,1) = 'Asia wet tropical WL'
  CNAMES(39,1) = 'S-America tropical WL'
  CNAMES(40,1) = 'S-America humid subtropical WL'
  CNAMES(41,1) = 'NH Subpolar WL'
  CNAMES(42,1) = 'NH Continental WL'
  CNAMES(43,1) = 'Asia humid subtropical WL'
  CNAMES(44,1) = 'N-America Semi arid WL'
  CNAMES(45,1) = 'N-America moderate polar WL'
  CNAMES(46,1) = 'S-America moderate polar WL'
  CNAMES(47,1) = 'N-America humid subtropical WL'

  CNAMES(48,1) = 'NH Africa semiarid WG'
  CNAMES(49,1) = 'NH Africa dry tropical WG'
  CNAMES(50,1) = 'Africa dry equatorial WG'
  CNAMES(51,1) = 'SH Africa dry tropical WG'
  CNAMES(52,1) = 'Oceania tropical WG'
  CNAMES(53,1) = 'Oceania semiarid WG'
  CNAMES(54,1) = 'Oceania subtrop. cool marine WG'
  CNAMES(55,1) = 'Asia humid and subtropical WG'
  CNAMES(56,1) = 'S-America trop. and subtrop. WG'
  CNAMES(57,1) = 'S-America Tr. wind WG'
  CNAMES(58,1) = 'S-America semiarid WG'
  CNAMES(59,1) = 'NH Subpolar WG'
  CNAMES(60,1) = 'NH Continental WG'
  CNAMES(61,1) = 'Asia wet and dry tropical WG'
  CNAMES(62,1) = 'N-America semi arid WG'
  CNAMES(63,1) = 'N-America humid subtropical WG'
  CNAMES(64,1) = 'S-America moderate polar WG'
  CNAMES(65,1) = 'Cent. Amer. Tr. wind \& trop. WG'
  CNAMES(66,1) = 'NH Africa dry summer subtrop. WG'

  CNAMES(67,1) = 'NH Africa arid CS'
  CNAMES(68,1) = 'NH Africa semiarid CS'
  CNAMES(69,1) = 'SH Africa semiarid CS'
  CNAMES(70,1) = 'Oceania arid CS'
  CNAMES(71,1) = 'Oceania, S-America semiarid CS'
  CNAMES(72,1) = 'Oceania Tr. wind CS'
  CNAMES(73,1) = 'SH dry summer subtropical CS'
  CNAMES(74,1) = 'Asia polar CS'
  CNAMES(75,1) = 'Asia continental CS'
  CNAMES(76,1) = 'Asia tropical CS'
  CNAMES(77,1) = 'N-America polar CS'
  CNAMES(78,1) = 'N-America continental CS'
  CNAMES(79,1) = 'NH Africa dry summer subtrop. CS'

  CNAMES(80,1) = 'NH arid OS'
  CNAMES(81,1) = 'NH semiarid tropical OS'
  CNAMES(82,1) = 'SH Africa and Oceania arid OS'
  CNAMES(83,1) = 'S-America semiarid tropical OS'
  CNAMES(84,1) = 'Asia dry tropical OS'
  CNAMES(85,1) = 'NH Polar OS'
  CNAMES(86,1) = 'N-America Subpolar OS'
  CNAMES(87,1) = 'N-America semiarid continental OS'

  CNAMES(88,1) = 'Africa wet Tropical G'
  CNAMES(89,1) = 'NH Africa Semiarid G'
  CNAMES(90,1) = 'SH Africa Semiarid G'
  CNAMES(91,1) = 'S-America, Oceania equatorial G'
  CNAMES(92,1) = 'S-America, Oceania Semiarid G'
  CNAMES(93,1) = 'Oceania cool littoral G'
  CNAMES(94,1) = 'Asia wet and dry tropical G'
  CNAMES(95,1) = 'NH S-America wet tropical G'
  CNAMES(96,1) = 'SH S-America wet tropical G'
  CNAMES(97,1) = 'S-America semiarid G'
  CNAMES(98,1) = 'S-America moderate polar G'
  CNAMES(99,1) = 'NH semiarid Continental G'
  CNAMES(100,1) = 'Asia Subpolar G'
  CNAMES(101,1) = 'Asia humid Continental G'
  CNAMES(102,1) = 'Asia semiarid tropical G'
  CNAMES(103,1) = 'N-America continental G'
  CNAMES(104,1) = 'Asia humid subtropical G'

  CNAMES(105,1) = 'NH Africa arid C'
  CNAMES(106,1) = 'NH Africa, Asia wet and dry trop. C'
  CNAMES(107,1) = 'SH Africa wet and dry tropical C'
  CNAMES(108,1) = 'SH Afr. Tr. wind \& semiarid trop. C'
  CNAMES(109,1) = 'Oceania dry summer subtropical C'
  CNAMES(110,1) = 'Cent. \& S-Amer., Oceania Tr. wind C'
  CNAMES(111,1) = 'S-America humid subtropical C'
  CNAMES(112,1) = 'SH S-America tropical C'
  CNAMES(113,1) = 'N-Amer., Asia semiarid continental C'
  CNAMES(114,1) = 'Asia humid continental C'
  CNAMES(115,1) = 'Asia humid subtropical C'
  CNAMES(116,1) = 'Asia subpolar C'
  CNAMES(117,1) = 'Asia semiarid tropical C'
  CNAMES(118,1) = 'N-America humid continental C'
  CNAMES(119,1) = 'N-America humid subtropical C'
  CNAMES(120,1) = 'NH dry summer subtropical C'
  CNAMES(121,1) = 'NH Africa dry summer subtropical C'
  CNAMES(122,1) = 'SH Africa dry summer subtropical C'

  CNAMES(123,1) = 'Bare soil with sparse polar vegetation'

  CNAMES(124,1) = 'Warm subtropical wetlands'
  CNAMES(125,1) = 'Subpolar wetlands'
!
  CNAMES(151,1) = 'Dense urban'
  CNAMES(152,1) = 'Mediterranean sub-urban'
  CNAMES(153,1) = 'Temperate sub-urban'
  CNAMES(154,1) = 'Cold sub-urban'
  CNAMES(155,1) = 'Industries and commercial areas'
  CNAMES(156,1) = 'Road and rail networks'
  CNAMES(157,1) = 'Port facilities'
  CNAMES(158,1) = 'Airport'
  CNAMES(159,1) = 'Mineral extraction, construction sites'
  CNAMES(160,1) = 'Urban parks'
  CNAMES(161,1) = 'Sport facilities'
!
  CNAMES(162,1) = 'Spanish crops'
  CNAMES(163,1) = 'Estremadura crops'
  CNAMES(164,1) = 'Mediterranean crops'
  CNAMES(165,1) = 'Atlantic coast crops'
  CNAMES(166,1) = 'Temperate crops'
  CNAMES(167,1) = 'Po plain crops'
  CNAMES(168,1) = 'Warm temperate crops'
  CNAMES(169,1) = 'Ukrainian crops'
  CNAMES(170,1) = 'Subpolar crops'
  CNAMES(171,1) = 'Mountain crops'
  CNAMES(172,1) = 'Central Europe crops'
  CNAMES(173,1) = 'Turkish crops'
!
  CNAMES(174,1) = 'Mediterranean irrigated crops'
  CNAMES(175,1) = 'Irrigated crops'
  CNAMES(176,1) = 'Rice fields'
!
  CNAMES(177,1) = 'Mediterranean vineyards'
  CNAMES(178,1) = 'Temperate vineyards'
  CNAMES(179,1) = 'Mediterranean fruit trees'
  CNAMES(180,1) = 'Temperate fruit trees'
  CNAMES(181,1) = 'Olive groves'
!
  CNAMES(182,1) = 'Temperate pastures'
  CNAMES(183,1) = 'Atlantic border pastures'
  CNAMES(184,1) = 'Central and Eastern Europe pastures'
  CNAMES(185,1) = 'Ukrainian pastures'
  CNAMES(186,1) = 'Subpolar pastures'
!
  CNAMES(187,1) = 'Spanish complex cultivation pattern'
  CNAMES(188,1) = 'Mediter. complex cultivation pat.'
  CNAMES(189,1) = 'Temperate complex cultivation pat.'
  CNAMES(190,1) = 'French complex cultivation pat.'
  CNAMES(191,1) = 'Balkanish complex cultivation pat.'
!
  CNAMES(192,1) = 'Mediterranean crops and woodland'
  CNAMES(193,1) = 'Crops and woodland'
  CNAMES(194,1) = 'French crops and woodland'
  CNAMES(195,1) = 'Balkanish crops and woodland'
  CNAMES(196,1) = 'Spanish crops and woodland'
  CNAMES(197,1) = 'Baltic states crops and woodland'
!
  CNAMES(198,1) = 'Agro-forestry areas'
!
  CNAMES(199,1) = 'Spanish broad-leaved forest'
  CNAMES(200,1) = 'Estremadura broad-leaved forest'
  CNAMES(201,1) = 'Mediterranean broad-leaved forest'
  CNAMES(202,1) = 'Atlantic coast broad-leaved forest'
  CNAMES(203,1) = 'Temperate broad-leaved forest'
  CNAMES(204,1) = 'Moutain broad-leaved forest'
  CNAMES(205,1) = 'Balkanish broad-leaved forest'
  CNAMES(206,1) = 'Subpolar broad-leaved forest'
  CNAMES(207,1) = 'Black Sea broad-leaved forest'
!
  CNAMES(208,1) = 'Mediterranean pines'
  CNAMES(209,1) = 'Landes forest'
  CNAMES(210,1) = 'Moutain coniferous forest'
  CNAMES(211,1) = 'Temperate coniferous forest'
  CNAMES(212,1) = 'Subpolar Taiga'
  CNAMES(213,1) = 'Russian Taiga'
  CNAMES(214,1) = 'Turkish coniferous forest'
!
  CNAMES(215,1) = 'Mediterranean mixed forest'
  CNAMES(216,1) = 'Atlantic coast \& french mixed forest'
  CNAMES(217,1) = 'Subpolar mixed forest'
  CNAMES(218,1) = 'Mountain mixed forest'
  CNAMES(219,1) = 'Eastern Europe mixed forest'
!
  CNAMES(220,1) = 'Mediterranean GR'
  CNAMES(221,1) = 'Atlantic coast GR'
  CNAMES(222,1) = 'Balkanish GR'
  CNAMES(223,1) = 'Estremadura GR'
  CNAMES(224,1) = 'Subpolar GR'
  CNAMES(225,1) = 'Tundra'
!
  CNAMES(226,1) = 'Turkish moors'
  CNAMES(227,1) = 'Mediter. moors \& heath lands'
  CNAMES(228,1) = 'Moutain moors \& heath lands'
  CNAMES(229,1) = 'Atlantic coast moors \& heath lands'
!
  CNAMES(230,1) = 'Turkish shrubland'
  CNAMES(231,1) = 'Mediterranean maquis'
  CNAMES(232,1) = 'Moutain maquis'
!
  CNAMES(233,1) = 'Spanish woodland'
  CNAMES(234,1) = 'Mediterranean woodland'
  CNAMES(235,1) = 'Temperate woodland'
!
  CNAMES(236,1) = 'Sparsely vegetated areas'
  CNAMES(237,1) = 'Burnt areas'
  CNAMES(238,1) = 'Temperate wetlands'
  CNAMES(239,1) = 'Subpolar wetlands'
  CNAMES(240,1) = 'Peat bogs'
  CNAMES(241,1) = 'Salines and salt marshes'
!
  CNAMES(242,1) = 'Intertidal flats'
  CNAMES(243,1) = 'Coastal lagoons'
!
  CNAMES(301,1)='N SCANDINAVIA TUNDRA1'
  CNAMES(302,1)='OURAL BF1'
  CNAMES(303,1)='CARELIE BF1'
  CNAMES(304,1)='NORTH RUSSIAN TAIGA1'
  CNAMES(305,1)='NORTH RUSSIAN TAIGA2'
  CNAMES(306,1)='CARELIE BF2'
  CNAMES(307,1)='RUSSIAN TAIGA3'
  CNAMES(308,1)='RUSSIAN BF1'
  CNAMES(309,1)='RUSSIAN TAIGA4'
  CNAMES(310,1)='S SCANDINAVIA TAIGA1'
  CNAMES(311,1)='SOUTH FINLANDIA MF1'
  CNAMES(312,1)='SOUTH NORWAY MF1'
  CNAMES(313,1)='BALTIC BF1'
  CNAMES(314,1)='BALTIC MF1'
  CNAMES(315,1)='SOUTH SWEDEN CF1'
  CNAMES(316,1)='BALTIC MF2'
  CNAMES(317,1)='SOUTH SWEDEN CF2'
  CNAMES(318,1)='SOUTH SWEDEN CF3'
  CNAMES(319,1)='SOUTH SWEDEN MF1'
  CNAMES(320,1)='MOUNTAIN MF1'
  CNAMES(321,1)='MOUNTAIN BF1'
  CNAMES(322,1)='TEMPERATE BF1'
  CNAMES(323,1)='TEMPERATE COMPLEX1'
  CNAMES(324,1)='MOUNTAIN CF1'
  CNAMES(325,1)='TEMP HERBACEOUS CF1'
  CNAMES(326,1)='ATLANTIC COAST BF1'
  CNAMES(327,1)='TURKISH CF1'
  CNAMES(328,1)='BALKAN CF1'
  CNAMES(329,1)='N SPAIN HERBAC MF1'
  CNAMES(330,1)='TEMP SW HERBAC CF1'
  CNAMES(331,1)='ATLANTIC COMPLEX1'
  CNAMES(332,1)='N SPAIN HERBAC MF2'
  CNAMES(333,1)='MEDITER COMPLEX1'
  CNAMES(334,1)='MEDITER COMPLEX2'
  CNAMES(335,1)='MEDITER COMPLEX3'
  CNAMES(336,1)='MEDITER COMPLEX4'
  CNAMES(337,1)='MEDITER COMPLEX5'
  CNAMES(338,1)='BURNT PORT HERBAC CF1'
  CNAMES(339,1)='BURNT PORT HERBAC BF1'
  CNAMES(340,1)='EGEE COAST COMPLEX1'
  CNAMES(341,1)='W MED COAST COMPLEX1'
  CNAMES(342,1)='MAGHR HERBACEOUS MF1'
  CNAMES(343,1)='ESTREM HERBACEOUS MF1'

!herbaceous / shrub covers
  CNAMES(344,1)='POLAR MOUNT TUNDRA1'
  CNAMES(345,1)='POLAR MOUNT TUNDRA2'
  CNAMES(346,1)='S SCANDINAVIA TUNDRA1'
  CNAMES(347,1)='NORTH TUNDRA1'
  CNAMES(348,1)='S SCANDINAVIA TUNDRA2'
  CNAMES(349,1)='NORTH RUSSIA TUNDRA1'
  CNAMES(350,1)='ARAL CONTINENTAL GR1'
  CNAMES(351,1)='MOUNTAIN TAIGA MOORS1'
  CNAMES(352,1)='SCOTTISH SWAMP MOORS1'
  CNAMES(353,1)='ATLANTIC COMPLEX2'
  CNAMES(354,1)='ATLANTIC GR1'
  CNAMES(355,1)='IR SCOT SWAMP MOORS1'
  CNAMES(356,1)='ASIAN SPARSE GR1'
  CNAMES(357,1)='AS SPARSE SW COMPLEX1'
  CNAMES(358,1)='N CASPIAN DES OS1'
  CNAMES(359,1)='ATLAS AS SPARSE COMP1'
  CNAMES(360,1)='SPARSE SCO CEN EU GR1'
  CNAMES(361,1)='TEMPERATE COMPLEX2'
  CNAMES(362,1)='ATLANTIC COMPLEX3'
  CNAMES(363,1)='ATLANTIC COMPLEX4'
  CNAMES(364,1)='N ATLANTIC PASTURES1'
  CNAMES(365,1)='SPARSE SCO CEN EU GR2'
  CNAMES(366,1)='SPARSE MOUNT E EU GR1'
  CNAMES(367,1)='TUR N CASP CONT GR1'
  CNAMES(368,1)='N CASPIAN CONT GR1'
  CNAMES(369,1)='IRA N CASP CONT GR1'
  CNAMES(370,1)='TUR IRA MOUNT CONT GR1'
  CNAMES(371,1)='E CASPIAN DES OS1'
  CNAMES(372,1)='N CASPIAN COMPLEX1'
  CNAMES(373,1)='IRAN MOUNT CONT GR1'
  CNAMES(374,1)='ASIAN SPARSE DES OS1'
  CNAMES(375,1)='E CASPIAN DES OS2'
  CNAMES(376,1)='N MEDITER COMPLEX1'
  CNAMES(377,1)='N MEDITER COMPLEX2'
  CNAMES(378,1)='ASIAN MEDIT CONT GR1'
  CNAMES(379,1)='SOUTH RUSSIA CONT GR1'
  CNAMES(380,1)='BLSEA SPARSE CONT GR1'
  CNAMES(381,1)='BLSEA SPARSE CONT GR2'
  CNAMES(382,1)='TURK MOUNT CONT GR1'
  CNAMES(383,1)='TURKISH COMPLEX1'
  CNAMES(384,1)='CAUCASIAN COMPLEX1'
  CNAMES(385,1)='N CASPIAN CONT GR2'
  CNAMES(386,1)='VOLGA VALLEY CONT GR1'
  CNAMES(387,1)='VOLGA VALLEY CONT GR2'
  CNAMES(388,1)='W CASPIAN CONT GR1'
  CNAMES(389,1)='CAUCASIAN COMPLEX2'
  CNAMES(390,1)='CAUCASIAN COMPLEX3'
  CNAMES(391,1)='BLSEA SPARSE CONT GR3'
  CNAMES(392,1)='CENT MASSIF COMPLEX1'
  CNAMES(393,1)='CENT MASSIF COMPLEX2'
  CNAMES(394,1)='TURK COAST COMPLEX1'
  CNAMES(395,1)='MESOPOTAMIA GR1'
  CNAMES(396,1)='TURK CILICIA COMPLEX1'
  CNAMES(397,1)='ASIAN COMPLEX1'
  CNAMES(398,1)='N MED SPARSE COMPLEX1'
  CNAMES(399,1)='MEDITER COMPLEX6'
  CNAMES(400,1)='MEDIT SPARSE COMPLEX1'
  CNAMES(401,1)='MEDIT SPARSE COMPLEX2'
  CNAMES(402,1)='MEDIT SPARSE COMPLEX3'
  CNAMES(403,1)='MEDIT SPARSE COMPLEX4'
  CNAMES(404,1)='N MED HERBACEOUS CF1'
  CNAMES(405,1)='ESTREMADURA GR1'
  CNAMES(406,1)='TUNISIA COMPLEX1'
  CNAMES(407,1)='TUNISIA HERBACEOUS1'
  CNAMES(408,1)='ALGERIA HERBACEOUS1'
  CNAMES(409,1)='DESERTIC HERBACEOUS1'
  CNAMES(410,1)='DESERTIC HERBACEOUS2'
  CNAMES(411,1)='SPAIN DES COMPLEX1'
  CNAMES(412,1)='MED SPARSE COMPLEX5'
  CNAMES(413,1)='MED SPARSE COMPLEX6'
  CNAMES(414,1)='MED SPARSE COMPLEX7'
  CNAMES(415,1)='ME SPARSE DES COMPL1'
  CNAMES(416,1)='NORTH ARABIA GR1'
  CNAMES(417,1)='N ARABIA DES COMPLEX1'
  CNAMES(418,1)='N ARABIA DESERTIC GR1'
  CNAMES(419,1)='MOROCCO HERBACEOUS1'
  CNAMES(420,1)='S MED COAST HERBAC1'
  CNAMES(421,1)='W MEDITER WOODLAND1'
  CNAMES(422,1)='S MED COAST HERBAC2'
  CNAMES(423,1)='MESOP DES HERBACEOUS1'
  CNAMES(424,1)='MAG COAST DES HERBAC1'
  CNAMES(425,1)='TU AR SPARSE HERBAC1'
  CNAMES(426,1)='MEDIT SPARSE COMPLEX8'
  CNAMES(427,1)='MED SPARSE HERBAC1'
  CNAMES(428,1)='MEDIT SPARSE COMPLEX9'
  CNAMES(429,1)='SPAIN SPARSE COMPLEX1'
  CNAMES(430,1)='N MED SPARSE COMPLEX2'
  CNAMES(431,1)='N MED SPARSE COMPLEX3'
  CNAMES(432,1)='MAGHRE DES HERBAC1'
  CNAMES(433,1)='MAGHRE DES HERBAC2'
  CNAMES(434,1)='MAGHRE DES HERBAC3'
  CNAMES(435,1)='N ARAB DES HERBAC1'
  CNAMES(436,1)='MESOPO DES HERBAC2'
  CNAMES(437,1)='TOURAN DES HERBAC1'
  CNAMES(438,1)='MESOPO DES HERBAC2'
  CNAMES(439,1)='TOURAN DES HERBAC2'
  CNAMES(440,1)='NEW ZEMBLE HERBAC1'
  CNAMES(441,1)='NEW ZEMBLE HERBAC2'

!crops
  CNAMES(442,1)='TRANS SIBERIAN CROPS1'
  CNAMES(443,1)='PO PLAIN CROPS1'
  CNAMES(444,1)='PO PLAIN CROPS2'
  CNAMES(445,1)='SPANISH FRENCH CROPS1'
  CNAMES(446,1)='SPANISH FR ITAL CROPS1'
  CNAMES(447,1)='DANUBE PLAIN CROPS1'
  CNAMES(448,1)='N MED SPARSE COMPLEX4'
  CNAMES(449,1)='BALKAN CROPS1'
  CNAMES(450,1)='SPAIN FR ITAL CROPS2'
  CNAMES(451,1)='ATLANTIC CROPS1'
  CNAMES(452,1)='FR MED SPARSE CROPS1'
  CNAMES(453,1)='FR MED SPARSE CROPS2'
  CNAMES(454,1)='ATL MED SPARSE CROPS1'
  CNAMES(455,1)='BENE BLACK SEA CROPS1'
  CNAMES(456,1)='FRENCH ITALIAN CROPS1'
  CNAMES(457,1)='FR MED SPARSE CROPS3'
  CNAMES(458,1)='MEDITER SPARSE CROPS1'
  CNAMES(459,1)='ATLANTIC CROPS2'
  CNAMES(460,1)='NORTH ATLANTIC CROPS1'
  CNAMES(461,1)='SOUTH RUSSIA CROPS1'
  CNAMES(462,1)='S RUSSIA BALTIC CROPS1'
  CNAMES(463,1)='UKRAINIAN CROPS1'
  CNAMES(464,1)='EAST CARPATES CROPS1'
  CNAMES(465,1)='E CENT EUROPE CROPS1'
  CNAMES(466,1)='W CENT EU SW CROPS1'
  CNAMES(467,1)='HUNGARIAN CROPS1'
  CNAMES(468,1)='N BLACK SEA CROPS1'
  CNAMES(469,1)='HUNG BULG CAUC CROPS1'
  CNAMES(470,1)='SOUTH SWEDEN CROPS1'
  CNAMES(471,1)='SW RUSSIA CROPS1'
  CNAMES(472,1)='SOUTH RUSSIA CROPS1'
  CNAMES(473,1)='IRAN N CASPIAN CROPS1'
  CNAMES(474,1)='FR TEMP SPARSE CROPS1'
  CNAMES(475,1)='BULGARIAN CROPS1'
  CNAMES(476,1)='BULGARIAN CROPS2'
  CNAMES(477,1)='SP TURK SPARSE CROPS1'
  CNAMES(478,1)='FRENCH CENT EU CROPS1'
  CNAMES(479,1)='N BLACK SEA CROPS2'
  CNAMES(480,1)='BULGARIAN CROPS3'
  CNAMES(481,1)='POLE CROPS1'
  CNAMES(482,1)='POLE CROPS2'
  CNAMES(483,1)='N BLACK SEA CROPS3'
  CNAMES(484,1)='CENT EU SPARSE CROPS1'
  CNAMES(485,1)='GERMAN CROPS1'
  CNAMES(486,1)='BEAUCE CROPS1'
  CNAMES(487,1)='DANE CROPS1'
  CNAMES(488,1)='DANE CROPS2'
  CNAMES(489,1)='NEU ATL SPARSE CROPS1'
  CNAMES(490,1)='SYRIAN CROPS1'
  CNAMES(491,1)='GERMAN CROPS2'
  CNAMES(492,1)='CHANNEL CROPS1'
  CNAMES(493,1)='CHANNEL CROPS2'
  CNAMES(494,1)='ITALIAN CROPS1'
  CNAMES(495,1)='TURKISH CROPS1'
  CNAMES(496,1)='N MEDIT SPARSE CROPS1'
  CNAMES(497,1)='SPAIN TUR ARAB CROPS1'
  CNAMES(498,1)='NORTH SPAIN CROPS1'
  CNAMES(499,1)='MOROCCO TUNIS CROPS1'
  CNAMES(500,1)='MOROCCO CROPS1'
  CNAMES(501,1)='MOROCCO CROPS2'
  CNAMES(502,1)='ALGERIAN CROPS1'
  CNAMES(503,1)='MOROCCO CROPS3'
  CNAMES(504,1)='WEST SPAIN CROPS1'
  CNAMES(505,1)='MOROCCO CROPS4'
  CNAMES(506,1)='NORTH MEDITER CROPS1'
  CNAMES(507,1)='SOUTH SPANISH CROPS1'
  CNAMES(508,1)='SICILIAN CROPS1'
  CNAMES(509,1)='MAGHREB SPARSE CROPS1'
  CNAMES(510,1)='N MEDIT SPARSE CROPS2'
  CNAMES(511,1)='N MEDIT SPARSE CROPS3'
  CNAMES(512,1)='SP IT WCOAST CROPS1'
  CNAMES(513,1)='ESTREMADURA CROPS1'
  CNAMES(514,1)='ESTREMADURA CROPS2'
  CNAMES(515,1)='SP IT WCOAST CROPS2'
  CNAMES(516,1)='ESTREMADURA CROPS3'
  CNAMES(517,1)='MEDIT ISLANDS CROPS1'
  CNAMES(518,1)='SPAIN W COAST CROPS1'
  CNAMES(519,1)='ESTREMADURA CROPS4'
  CNAMES(520,1)='MECOAST SPARSE CROPS1'
  CNAMES(521,1)='BRITTANY CROPS1'
  CNAMES(522,1)='SYRIAN CROPS2'

!irrigated crops
  CNAMES(523,1)='NIL VALLEY CROPS1'
  CNAMES(524,1)='NIL VALLEY CROPS2'
  CNAMES(525,1)='NIL VALLEY CROPS3'
  CNAMES(526,1)='NIL VALLEY CROPS4'
  CNAMES(527,1)='SPANISH IRR CROPS1'
  CNAMES(528,1)='NIL VALLEY CROPS5'
  CNAMES(529,1)='EGEE IRR CROPS1'
  CNAMES(530,1)='MEDITER IRR CROPS1'
  CNAMES(531,1)='S SPAIN IRR CROPS1'
  CNAMES(532,1)='NIL VALLEY CROPS6'

!bare land
  CNAMES(533,1)='BARE ROCK1'
  CNAMES(534,1)='BARE ROCK2'
  CNAMES(535,1)='SANDY DESERT1'
  CNAMES(536,1)='BARE LAND1'
  CNAMES(537,1)='BARE LAND2'
  CNAMES(538,1)='BARE LAND3'
  CNAMES(539,1)='BARE LAND4'
  CNAMES(540,1)='BARE LAND5'
  CNAMES(541,1)='BARE LAND6'
  CNAMES(542,1)='BARE LAND7'
  CNAMES(543,1)='BARE LAND8'
  CNAMES(544,1)='BARE LAND9'
  CNAMES(545,1)='BARE LAND10'
  CNAMES(546,1)='BARE LAND11'
  CNAMES(547,1)='BARE LAND12'
  CNAMES(548,1)='PERMANENT SNOW1'

!Estuaries and swamp areas
  CNAMES(549,1)='WADDEN SEA'
  CNAMES(550,1)='ESTUARY1'
  CNAMES(551,1)='ESTUARY2'
  CNAMES(552,1)='POLAR WETLANDS1'
  CNAMES(553,1)='ESTUARY3'
  CNAMES(554,1)='ESTUARY4'
  CNAMES(555,1)='ESTUARY5'
  CNAMES(556,1)='ESTUARY6'
  CNAMES(557,1)='POLAR WETLANDS2'
  CNAMES(558,1)='SUBPOLAR WETLANDS1'
  CNAMES(559,1)='SUBPOLAR WETLANDS2'
  CNAMES(560,1)='SUBPOLAR WETLANDS3'

!urban
  CNAMES(561,1)='TEMPERATE SUBURBAN1'
  CNAMES(562,1)='TEMPERATE SUBURBAN2'
  CNAMES(563,1)='TEMPERATE SUBURBAN3'
  CNAMES(564,1)='TEMPERATE SUBURBAN4'
  CNAMES(565,1)='TEMPERATE SUBURBAN5'
  CNAMES(566,1)='COLD SUBURBAN1'
  CNAMES(567,1)='WARM SUBURBAN1'
  CNAMES(568,1)='WARM SUBURBAN2'
  CNAMES(569,1)='TEMPERATE SUBURBAN6'
  CNAMES(570,1)='TEMPERATE SUBURBAN7'
  CNAMES(571,1)='WARM SUBURBAN3'

!added classes of permanent crops
  CNAMES(572,1)='SPANISH VINEYARDS1'
  CNAMES(573,1)='LANGUEDOC VINEYARDS1'

!-------------------------------------------------------------------------------
!

  CNAMES(1,2) = "Mers et oc\'eans"
  CNAMES(2,2) = "Lacs"
  CNAMES(3,2) = "rivi\`eres"
  CNAMES(4,2) = "Sol nu"
  CNAMES(5,2) = "Rochers"
  CNAMES(6,2) = "Neiges perp\'etuelles"
  CNAMES(7,2) = "Ville"

  CNAMES(8,2) = "\^iles non-d\'efinies (tropicales)"
  CNAMES(9,2) = "\^iles non-d\'efinies (subpolaires)"
!
  CNAMES(10,2) = "For\^et persistante de Patagonie "
  CNAMES(11,2) = 'Taiga'
  CNAMES(12,2) = "For\^et de conif\`eres d'Extr\^eme-Orient "
  CNAMES(13,2) = "For\^et de conif\`eres continentale am\'ericaine"
  CNAMES(14,2) = "For\^et de conif\`eres subtropicale am\'ericaine"
  CNAMES(15,2) = '"Rain forest"'

  CNAMES(16,2) = "For\^et \'equatoriale africaine"
  CNAMES(17,2) = "For\^et de Madagascar (aliz\'es)"
  CNAMES(18,2) = "For\^et \'equatoriale d'Oc\'eanie"
  CNAMES(19,2) = "For\^et persistante de Mousson"
  CNAMES(20,2) = "For\^et tropicale humide d'Oc\'eanie"
  CNAMES(21,2) = "For\^et amazonienne"
  CNAMES(22,2) = "For\^et primaire de Tasmanie"
  CNAMES(23,2) = "For\^et pan-am\'ericaine"

  CNAMES(24,2) = "Taiga sib\'erienne caduque"

  CNAMES(25,2) = "For\^et sub-amazonienne"
  CNAMES(26,2) = "For\^et caduque am\'ericaine"
  CNAMES(27,2) = "For\^et du Yucatan"
  CNAMES(28,2) = "For\^et sub-tropicale br\'esilienne"

  CNAMES(29,2) = "For\^et m\'elang\'ee tropicale s\`eche d'Afrique"
  CNAMES(30,2) = "For\^et m\'elang\'ee caduque de Patagonie"
  CNAMES(31,2) = "For\^et m\'elang\'ee sub-polaire"
  CNAMES(32,2) = "For\^et m\'elang\'ee humide sub-tropicale"
  CNAMES(33,2) = "For\^et m\'elang\'ee continentale"

  CNAMES(34,2) = "Prairie bois\'ee sah\'elienne"
  CNAMES(35,2) = "Prairie bois\'ee d'Afrique m\'eridionale"
  CNAMES(36,2) = "Prairie bois\'ee \`a feuilles persistante"
  CNAMES(37,2) = "Prairie bois\'ee \'equatoriale d'Oc\'eanie"
  CNAMES(38,2) = "Prairie bois\'ee de Mousson"
  CNAMES(39,2) = "Cerradao"
  CNAMES(40,2) = "Cerradao d'Argentine"
  CNAMES(41,2) = "Prairie bois\'ee polaire"
  CNAMES(42,2) = "Prairie bois\'ee continentale"
  CNAMES(43,2) = "Prairie bois\'ee d'Extr\^eme-Orient"
  CNAMES(44,2) = "Prairie bois\'ee semi-aride"
  CNAMES(45,2) = "Prairie bois\'ee des Rocheuses"
  CNAMES(46,2) = "Prairie bois\'ee de Patagonie"
  CNAMES(47,2) = "Prairie bois\'ee des Etats du Sud"

  CNAMES(48,2) = "Savane arbor\'ee sub-saharienne"
  CNAMES(49,2) = "Savane arbor\'ee sah\'elienne"
  CNAMES(50,2) = "Prairie arbor\'ee \'equatoriale africaine"
  CNAMES(51,2) = "Prairie arbor\'ee subtropicale africaine"
  CNAMES(52,2) = "Prairie arbor\'ee nord-australienne"
  CNAMES(53,2) = "Savane arbor\'ee australienne aride"
  CNAMES(54,2) = "Prairie arbor\'ee n\'eo-z\'elandaise"
  CNAMES(55,2) = "Prairie arbor\'ee d'Extr\^eme-Orient"
  CNAMES(56,2) = "Cerrado"
  CNAMES(57,2) = "Cerrado c\^otier"
  CNAMES(58,2) = "Sertao"
  CNAMES(59,2) = "Prairie arbor\'ee polaire"
  CNAMES(60,2) = "Prairie arbor\'ee continentale"
  CNAMES(61,2) = "Prairie arbor\'ee de Mousson"
  CNAMES(62,2) = "Prairie arbor\'ee pan-am\'ericaine"
  CNAMES(63,2) = "Prairie arbor\'ee des Etats du sud"
  CNAMES(64,2) = "Prairie arbor\'ee de la cordill\`ere des Andes"
  CNAMES(65,2) = "Prairie arbor\'ee Amazonienne"
  CNAMES(66,2) = "Prairie arbor\'ee du Maghreb"

  CNAMES(67,2) = "Savane arbustive d'Ethiopie"
  CNAMES(68,2) = "Savane arbustive sah\'elienne"
  CNAMES(69,2) = '"Bush" africain'
  CNAMES(70,2) = '"Bush" australien'
  CNAMES(71,2) = "Savane arbustive semi-d\'esertique"
  CNAMES(72,2) = "Savane arbustive dense c\^oti\`ere d'Oc\'eanie"
  CNAMES(73,2) = '"Bush" dense sud-australien'
  CNAMES(74,2) = "Tundra arbustive dense"
  CNAMES(75,2) = "Steppe asiatique arbustive dense"
  CNAMES(76,2) = "Prairie arbustive de Mousson"
  CNAMES(77,2) = "Tundra dense am\'ericaine"
  CNAMES(78,2) = "Prairie arbustive d'altitude (Rocheuses)"
  CNAMES(79,2) = "Prairie arbustive du Maghreb"

  CNAMES(80,2) = "Savane \'eparse semi-d\'esertique"
  CNAMES(81,2) = "Savane Sah\'elienne"
  CNAMES(82,2) = "Bush \'epars"
  CNAMES(83,2) = "Pampa"
  CNAMES(84,2) = "Prairie \'eparse de Mousson"
  CNAMES(85,2) = "Tundra polaire"
  CNAMES(86,2) = "Tundra sub-polaire"
  CNAMES(87,2) = "Prairie d'altitude (Rocheuses)"

  CNAMES(88,2) = "Prairie tropicale africaine"
  CNAMES(89,2) = "Prairie africaine semi-aride"
  CNAMES(90,2) = "Prairie d'Afrique m\'eridionale"
  CNAMES(91,2) = "Prairie \'equatoriale c\^oti\`ere"
  CNAMES(92,2) = "Prairie semi-d\'esertique"
  CNAMES(93,2) = "Prairie n\'eo-z\'elandaise"
  CNAMES(94,2) = "Prairie de Mousson"
  CNAMES(95,2) = "Prairie v\'en\'ezu\'elienne"
  CNAMES(96,2) = "Prairie sub-amazonienne"
  CNAMES(97,2) = "Pampa et prairie"
  CNAMES(98,2) = "Prairie de Patagonie"
  CNAMES(99,2) = "Steppes"
  CNAMES(100,2) = "Prairie polaire"
  CNAMES(101,2) = "Prairie d'Extr\^eme-Orient"
  CNAMES(102,2) = "Prairie indienne"
  CNAMES(103,2) = "Prairie d'altitude (Rocheuses)"
  CNAMES(104,2) = "Prairie chinoise"

  CNAMES(105,2) = "Cultures d\'esertique et le long du Nil"
  CNAMES(106,2) = "Cultures sah\'elienne"
  CNAMES(107,2) = "Cultures tropicales"
  CNAMES(108,2) = "Cultures c\^oti\`eres africaines"
  CNAMES(109,2) = "Cultures sud-australiennes"
  CNAMES(110,2) = "Cultures pan-am\'ericaines"
  CNAMES(111,2) = "Cultures de la plaine du Parana"
  CNAMES(112,2) = "Zones cultiv\'ees sub-amazoniennes"
  CNAMES(113,2) = "Zones cultiv\'ees trans-siberiennes"
  CNAMES(114,2) = "Zones cultiv\'ees de Mandchourie"
  CNAMES(115,2) = "Zones cultiv\'ees de Chine"
  CNAMES(116,2) = "Zones cultiv\'ees sub-polaires"
  CNAMES(117,2) = "Zones cultiv\'ees sub-himalayennes"
  CNAMES(118,2) = "Cultures des Grandes Plaines"
  CNAMES(119,2) = "Cultures des Etats du Sud"
  CNAMES(120,2) = "Cultures californiennes"
  CNAMES(121,2) = "Zones cultiv\'ees du Maghreb"
  CNAMES(122,2) = "Zones cultiv\'ees d'Afrique du Sud"

  CNAMES(123,2) = "Tundra \'eparse"

  CNAMES(124,2) = "Marais tropicaux et sub-tropicaux"
  CNAMES(125,2) = "Marais polaires et sub-polaires"
!
  CNAMES(151,2) = "Urbain dense"
  CNAMES(152,2) = "Suburbain m\'editerran\'een"
  CNAMES(153,2) = "Suburbain temp\'er\'e"
  CNAMES(154,2) = "Suburbain froid"
  CNAMES(155,2) = "Zones industrielles et commerciales"
  CNAMES(156,2) = "R\'eseaux routiers et ferroviaires"
  CNAMES(157,2) = "Zones portuaires"
  CNAMES(158,2) = "A\'eroports"
  CNAMES(159,2) = "Extraction de mat\'eriaux, sites de construction"
  CNAMES(160,2) = "Parcs urbains"
  CNAMES(161,2) = "Equipements sportifs et de loisirs"
!
  CNAMES(162,2) = "Cultures espagnoles"
  CNAMES(163,2) = "Cultures d\'Estremadure"
  CNAMES(164,2) = "Cultures m\'editerran\'eennes"
  CNAMES(165,2) = "Cultures de la c\^ote atlantique"
  CNAMES(166,2) = "Cultures temp\'er\'ees"
  CNAMES(167,2) = "Cultures de la plaine du P\^o"
  CNAMES(168,2) = "Cultures temp\'er\'ees chaudes"
  CNAMES(169,2) = "Cultures ukrainiennes"
  CNAMES(170,2) = "Cultures subpolaires"
  CNAMES(171,2) = "Cultures montagneuses"
  CNAMES(172,2) = "Cultures d'Europe Centrale"
  CNAMES(173,2) = "Cultures turques"
!
  CNAMES(174,2) = "Cultures m\'editerran\'eennes irrigu\'ees"
  CNAMES(175,2) = "Cultures irrigu\'ees"
  CNAMES(176,2) = "Rizi\`eres"
!
  CNAMES(177,2) = "Vignobles m\'editerran\'eens"
  CNAMES(178,2) = "Vignobles temp\'er\'es"
  CNAMES(179,2) = "Arbres fruitiers m\'editerran\'eens"
  CNAMES(180,2) = "Arbres fruitiers temp\'er\'es"
  CNAMES(181,2) = "Oliveraies"
!
  CNAMES(182,2) = "P\^atures temp\'er\'ees"
  CNAMES(183,2) = "P\^atures c\^oti\`eres atlantiques"
  CNAMES(184,2) = "P\^atures d'Europe Centrale et orientale"
  CNAMES(185,2) = "P\^atures ukrainiennes"
  CNAMES(186,2) = "P\^atures subpolaires"
!
  CNAMES(187,2) = "Cultures complexes espagnoles"
  CNAMES(188,2) = "Cultures complexes m\'editerran\'eennes"
  CNAMES(189,2) = "Cultures complexes temp\'er\'ees"
  CNAMES(190,2) = "Cultures complexes françaises"
  CNAMES(191,2) = "Cultures complexes balkaniques"
!
  CNAMES(192,2) = "Cultures bois\'ees m\'editerran\'eennes"
  CNAMES(193,2) = "Cultures bois\'ees"
  CNAMES(194,2) = "Cultures bois\'ees françaises"
  CNAMES(195,2) = "Cultures bois\'ees m\'editerran\'eennes"
  CNAMES(196,2) = "Cultures bois\'ees espagnoles"
  CNAMES(197,2) = "Cultures bois\'ees des pays baltes"
!
  CNAMES(198,2) = "Zones agro-foresti\`eres"
!
  CNAMES(199,2) = "For\^et de feuillus espagnole"
  CNAMES(200,2) = "For\^et de feuillus d\'Estremadure"
  CNAMES(201,2) = "For\^et de feuillus m\'editerran\'eennes"
  CNAMES(202,2) = "For\^et de feuillus de la c\^ote atlantique"
  CNAMES(203,2) = "For\^et de feuillus temp\'er\'ee"
  CNAMES(204,2) = "For\^et de feuillus montagneuses"
  CNAMES(205,2) = "For\^et de feuillus balkanique"
  CNAMES(206,2) = "For\^et de feuillus balkanique"
  CNAMES(207,2) = "For\^et de feuillus de la mer Noire"
!
  CNAMES(208,2) = "Pins m\'editerran\'eens"
  CNAMES(209,2) = "For\^et des Landes"
  CNAMES(210,2) = "For\^et de conif\`eres montagneuse"
  CNAMES(211,2) = "For\^et de conif\`eres temp\'er\'ee"
  CNAMES(212,2) = "Taiga subpolaire"
  CNAMES(213,2) = "Taiga russe"
  CNAMES(214,2) = "For\^et de conif\`eres turque"
!
  CNAMES(215,2) = "For\^et mixte m\'editerran\'eenne"
  CNAMES(216,2) = "For\^et mixte française \& c\^oti\`ere atlantique"
  CNAMES(217,2) = "For\^et mixte subpolaire"
  CNAMES(218,2) = "For\^et mixte montagneuses"
  CNAMES(219,2) = "For\^et mixte d'Europe orientale"
!
  CNAMES(220,2) = "Prairie m\'editerran\'eenne"
  CNAMES(221,2) = "Prairie c\^oti\`ere atlantique"
  CNAMES(222,2) = "Prairie balkanique"
  CNAMES(223,2) = "Prairie d\'Estremadure"
  CNAMES(224,2) = "Prairie subpolaire"
  CNAMES(225,2) = "Tundra"
!
  CNAMES(226,2) = "Landes turques"
  CNAMES(227,2) = "Landes et broussailles m\'editerran\'eennes"
  CNAMES(228,2) = "Landes et broussailles montagneuses"
  CNAMES(229,2) = "Landes et broussailles c\^oti\`eres atlantiques"
!
  CNAMES(230,2) = "V\'eg\'etation arbustive turque"
  CNAMES(231,2) = "Maquis m\'editerran\'een"
  CNAMES(232,2) = "Maquis montagneux"
!
  CNAMES(233,2) = "Zones bois\'ees espagnoles"
  CNAMES(234,2) = "Zones bois\'ees m\'editerran\'eennes"
  CNAMES(235,2) = "Zones bois\'ees temp\'er\'ees"
!
  CNAMES(236,2) = "V\'eg\'etation clairsem\'ee"
  CNAMES(237,2) = "Zones br\^ul\'ees"
  CNAMES(238,2) = "Zones humides temp\'er\'ees"
  CNAMES(239,2) = "Zones humides subpolaires"
  CNAMES(240,2) = "Tourbi\`eres"
  CNAMES(241,2) = "Marais salants"

!
  CNAMES(242,2) = "Zones intertidales"
  CNAMES(243,2) = "Lagunes littorales"
!
  CNAMES(301,2)='N SCANDINAVIA TUNDRA1'
  CNAMES(302,2)='OURAL BF1'
  CNAMES(303,2)='CARELIE BF1'
  CNAMES(304,2)='NORTH RUSSIAN TAIGA1'
  CNAMES(305,2)='NORTH RUSSIAN TAIGA2'
  CNAMES(306,2)='CARELIE BF2'
  CNAMES(307,2)='RUSSIAN TAIGA3'
  CNAMES(308,2)='RUSSIAN BF1'
  CNAMES(309,2)='RUSSIAN TAIGA4'
  CNAMES(310,2)='S SCANDINAVIA TAIGA1'
  CNAMES(311,2)='SOUTH FINLANDIA MF1'
  CNAMES(312,2)='SOUTH NORWAY MF1'
  CNAMES(313,2)='BALTIC BF1'
  CNAMES(314,2)='BALTIC MF1'
  CNAMES(315,2)='SOUTH SWEDEN CF1'
  CNAMES(316,2)='BALTIC MF2'
  CNAMES(317,2)='SOUTH SWEDEN CF2'
  CNAMES(318,2)='SOUTH SWEDEN CF3'
  CNAMES(319,2)='SOUTH SWEDEN MF1'
  CNAMES(320,2)='MOUNTAIN MF1'
  CNAMES(321,2)='MOUNTAIN BF1'
  CNAMES(322,2)='TEMPERATE BF1'
  CNAMES(323,2)='TEMPERATE COMPLEX1'
  CNAMES(324,2)='MOUNTAIN CF1'
  CNAMES(325,2)='TEMP HERBACEOUS CF1'
  CNAMES(326,2)='ATLANTIC COAST BF1'
  CNAMES(327,2)='TURKISH CF1'
  CNAMES(328,2)='BALKAN CF1'
  CNAMES(329,2)='N SPAIN HERBAC MF1'
  CNAMES(330,2)='TEMP SW HERBAC CF1'
  CNAMES(331,2)='ATLANTIC COMPLEX1'
  CNAMES(332,2)='N SPAIN HERBAC MF2'
  CNAMES(333,2)='MEDITER COMPLEX1'
  CNAMES(334,2)='MEDITER COMPLEX2'
  CNAMES(335,2)='MEDITER COMPLEX3'
  CNAMES(336,2)='MEDITER COMPLEX4'
  CNAMES(337,2)='MEDITER COMPLEX5'
  CNAMES(338,2)='BURNT PORT HERBAC CF1'
  CNAMES(339,2)='BURNT PORT HERBAC BF1'
  CNAMES(340,2)='EGEE COAST COMPLEX1'
  CNAMES(341,2)='W MED COAST COMPLEX1'
  CNAMES(342,2)='MAGHR HERBACEOUS MF1'
  CNAMES(343,2)='ESTREM HERBACEOUS MF1'

!herbaceous / shrub covers
  CNAMES(344,2)='POLAR MOUNT TUNDRA1'
  CNAMES(345,2)='POLAR MOUNT TUNDRA2'
  CNAMES(346,2)='S SCANDINAVIA TUNDRA1'
  CNAMES(347,2)='NORTH TUNDRA1'
  CNAMES(348,2)='S SCANDINAVIA TUNDRA2'
  CNAMES(349,2)='NORTH RUSSIA TUNDRA1'
  CNAMES(350,2)='ARAL CONTINENTAL GR1'
  CNAMES(351,2)='MOUNTAIN TAIGA MOORS1'
  CNAMES(352,2)='SCOTTISH SWAMP MOORS1'
  CNAMES(353,2)='ATLANTIC COMPLEX2'
  CNAMES(354,2)='ATLANTIC GR1'
  CNAMES(355,2)='IR SCOT SWAMP MOORS1'
  CNAMES(356,2)='ASIAN SPARSE GR1'
  CNAMES(357,2)='AS SPARSE SW COMPLEX1'
  CNAMES(358,2)='N CASPIAN DES OS1'
  CNAMES(359,2)='ATLAS AS SPARSE COMP1'
  CNAMES(360,2)='SPARSE SCO CEN EU GR1'
  CNAMES(361,2)='TEMPERATE COMPLEX2'
  CNAMES(362,2)='ATLANTIC COMPLEX3'
  CNAMES(363,2)='ATLANTIC COMPLEX4'
  CNAMES(364,2)='N ATLANTIC PASTURES1'
  CNAMES(365,2)='SPARSE SCO CEN EU GR2'
  CNAMES(366,2)='SPARSE MOUNT E EU GR1'
  CNAMES(367,2)='TUR N CASP CONT GR1'
  CNAMES(368,2)='N CASPIAN CONT GR1'
  CNAMES(369,2)='IRA N CASP CONT GR1'
  CNAMES(370,2)='TUR IRA MOUNT CONT GR1'
  CNAMES(371,2)='E CASPIAN DES OS1'
  CNAMES(372,2)='N CASPIAN COMPLEX1'
  CNAMES(373,2)='IRAN MOUNT CONT GR1'
  CNAMES(374,2)='ASIAN SPARSE DES OS1'
  CNAMES(375,2)='E CASPIAN DES OS2'
  CNAMES(376,2)='N MEDITER COMPLEX1'
  CNAMES(377,2)='N MEDITER COMPLEX2'
  CNAMES(378,2)='ASIAN MEDIT CONT GR1'
  CNAMES(379,2)='SOUTH RUSSIA CONT GR1'
  CNAMES(380,2)='BLSEA SPARSE CONT GR1'
  CNAMES(381,2)='BLSEA SPARSE CONT GR2'
  CNAMES(382,2)='TURK MOUNT CONT GR1'
  CNAMES(383,2)='TURKISH COMPLEX1'
  CNAMES(384,2)='CAUCASIAN COMPLEX1'
  CNAMES(385,2)='N CASPIAN CONT GR2'
  CNAMES(386,2)='VOLGA VALLEY CONT GR1'
  CNAMES(387,2)='VOLGA VALLEY CONT GR2'
  CNAMES(388,2)='W CASPIAN CONT GR1'
  CNAMES(389,2)='CAUCASIAN COMPLEX2'
  CNAMES(390,2)='CAUCASIAN COMPLEX3'
  CNAMES(391,2)='BLSEA SPARSE CONT GR3'
  CNAMES(392,2)='CENT MASSIF COMPLEX1'
  CNAMES(393,2)='CENT MASSIF COMPLEX2'
  CNAMES(394,2)='TURK COAST COMPLEX1'
  CNAMES(395,2)='MESOPOTAMIA GR1'
  CNAMES(396,2)='TURK CILICIA COMPLEX1'
  CNAMES(397,2)='ASIAN COMPLEX1'
  CNAMES(398,2)='N MED SPARSE COMPLEX1'
  CNAMES(399,2)='MEDITER COMPLEX6'
  CNAMES(400,2)='MEDIT SPARSE COMPLEX1'
  CNAMES(401,2)='MEDIT SPARSE COMPLEX2'
  CNAMES(402,2)='MEDIT SPARSE COMPLEX3'
  CNAMES(403,2)='MEDIT SPARSE COMPLEX4'
  CNAMES(404,2)='N MED HERBACEOUS CF1'
  CNAMES(405,2)='ESTREMADURA GR1'
  CNAMES(406,2)='TUNISIA COMPLEX1'
  CNAMES(407,2)='TUNISIA HERBACEOUS1'
  CNAMES(408,2)='ALGERIA HERBACEOUS1'
  CNAMES(409,2)='DESERTIC HERBACEOUS1'
  CNAMES(410,2)='DESERTIC HERBACEOUS2'
  CNAMES(411,2)='SPAIN DES COMPLEX1'
  CNAMES(412,2)='MED SPARSE COMPLEX5'
  CNAMES(413,2)='MED SPARSE COMPLEX6'
  CNAMES(414,2)='MED SPARSE COMPLEX7'
  CNAMES(415,2)='ME SPARSE DES COMPL1'
  CNAMES(416,2)='NORTH ARABIA GR1'
  CNAMES(417,2)='N ARABIA DES COMPLEX1'
  CNAMES(418,2)='N ARABIA DESERTIC GR1'
  CNAMES(419,2)='MOROCCO HERBACEOUS1'
  CNAMES(420,2)='S MED COAST HERBAC1'
  CNAMES(421,2)='W MEDITER WOODLAND1'
  CNAMES(422,2)='S MED COAST HERBAC2'
  CNAMES(423,2)='MESOP DES HERBACEOUS1'
  CNAMES(424,2)='MAG COAST DES HERBAC1'
  CNAMES(425,2)='TU AR SPARSE HERBAC1'
  CNAMES(426,2)='MEDIT SPARSE COMPLEX8'
  CNAMES(427,2)='MED SPARSE HERBAC1'
  CNAMES(428,2)='MEDIT SPARSE COMPLEX9'
  CNAMES(429,2)='SPAIN SPARSE COMPLEX1'
  CNAMES(430,2)='N MED SPARSE COMPLEX2'
  CNAMES(431,2)='N MED SPARSE COMPLEX3'
  CNAMES(432,2)='MAGHRE DES HERBAC1'
  CNAMES(433,2)='MAGHRE DES HERBAC2'
  CNAMES(434,2)='MAGHRE DES HERBAC3'
  CNAMES(435,2)='N ARAB DES HERBAC1'
  CNAMES(436,2)='MESOPO DES HERBAC2'
  CNAMES(437,2)='TOURAN DES HERBAC1'
  CNAMES(438,2)='MESOPO DES HERBAC2'
  CNAMES(439,2)='TOURAN DES HERBAC2'
  CNAMES(440,2)='NEW ZEMBLE HERBAC1'
  CNAMES(441,2)='NEW ZEMBLE HERBAC2'

!crops
  CNAMES(442,2)='TRANS SIBERIAN CROPS1'
  CNAMES(443,2)='PO PLAIN CROPS1'
  CNAMES(444,2)='PO PLAIN CROPS2'
  CNAMES(445,2)='SPANISH FRENCH CROPS1'
  CNAMES(446,2)='SPANISH FR ITAL CROPS1'
  CNAMES(447,2)='DANUBE PLAIN CROPS1'
  CNAMES(448,2)='N MED SPARSE COMPLEX4'
  CNAMES(449,2)='BALKAN CROPS1'
  CNAMES(450,2)='SPAIN FR ITAL CROPS2'
  CNAMES(451,2)='ATLANTIC CROPS1'
  CNAMES(452,2)='FR MED SPARSE CROPS1'
  CNAMES(453,2)='FR MED SPARSE CROPS2'
  CNAMES(454,2)='ATL MED SPARSE CROPS1'
  CNAMES(455,2)='BENE BLACK SEA CROPS1'
  CNAMES(456,2)='FRENCH ITALIAN CROPS1'
  CNAMES(457,2)='FR MED SPARSE CROPS3'
  CNAMES(458,2)='MEDITER SPARSE CROPS1'
  CNAMES(459,2)='ATLANTIC CROPS2'
  CNAMES(460,2)='NORTH ATLANTIC CROPS1'
  CNAMES(461,2)='SOUTH RUSSIA CROPS1'
  CNAMES(462,2)='S RUSSIA BALTIC CROPS1'
  CNAMES(463,2)='UKRAINIAN CROPS1'
  CNAMES(464,2)='EAST CARPATES CROPS1'
  CNAMES(465,2)='E CENT EUROPE CROPS1'
  CNAMES(466,2)='W CENT EU SW CROPS1'
  CNAMES(467,2)='HUNGARIAN CROPS1'
  CNAMES(468,2)='N BLACK SEA CROPS1'
  CNAMES(469,2)='HUNG BULG CAUC CROPS1'
  CNAMES(470,2)='SOUTH SWEDEN CROPS1'
  CNAMES(471,2)='SW RUSSIA CROPS1'
  CNAMES(472,2)='SOUTH RUSSIA CROPS1'
  CNAMES(473,2)='IRAN N CASPIAN CROPS1'
  CNAMES(474,2)='FR TEMP SPARSE CROPS1'
  CNAMES(475,2)='BULGARIAN CROPS1'
  CNAMES(476,2)='BULGARIAN CROPS2'
  CNAMES(477,2)='SP TURK SPARSE CROPS1'
  CNAMES(478,2)='FRENCH CENT EU CROPS1'
  CNAMES(479,2)='N BLACK SEA CROPS2'
  CNAMES(480,2)='BULGARIAN CROPS3'
  CNAMES(481,2)='POLE CROPS1'
  CNAMES(482,2)='POLE CROPS2'
  CNAMES(483,2)='N BLACK SEA CROPS3'
  CNAMES(484,2)='CENT EU SPARSE CROPS1'
  CNAMES(485,2)='GERMAN CROPS1'
  CNAMES(486,2)='BEAUCE CROPS1'
  CNAMES(487,2)='DANE CROPS1'
  CNAMES(488,2)='DANE CROPS2'
  CNAMES(489,2)='NEU ATL SPARSE CROPS1'
  CNAMES(490,2)='SYRIAN CROPS1'
  CNAMES(491,2)='GERMAN CROPS2'
  CNAMES(492,2)='CHANNEL CROPS1'
  CNAMES(493,2)='CHANNEL CROPS2'
  CNAMES(494,2)='ITALIAN CROPS1'
  CNAMES(495,2)='TURKISH CROPS1'
  CNAMES(496,2)='N MEDIT SPARSE CROPS1'
  CNAMES(497,2)='SPAIN TUR ARAB CROPS1'
  CNAMES(498,2)='NORTH SPAIN CROPS1'
  CNAMES(499,2)='MOROCCO TUNIS CROPS1'
  CNAMES(500,2)='MOROCCO CROPS1'
  CNAMES(501,2)='MOROCCO CROPS2'
  CNAMES(502,2)='ALGERIAN CROPS1'
  CNAMES(503,2)='MOROCCO CROPS3'
  CNAMES(504,2)='WEST SPAIN CROPS1'
  CNAMES(505,2)='MOROCCO CROPS4'
  CNAMES(506,2)='NORTH MEDITER CROPS1'
  CNAMES(507,2)='SOUTH SPANISH CROPS1'
  CNAMES(508,2)='SICILIAN CROPS1'
  CNAMES(509,2)='MAGHREB SPARSE CROPS1'
  CNAMES(510,2)='N MEDIT SPARSE CROPS2'
  CNAMES(511,2)='N MEDIT SPARSE CROPS3'
  CNAMES(512,2)='SP IT WCOAST CROPS1'
  CNAMES(513,2)='ESTREMADURA CROPS1'
  CNAMES(514,2)='ESTREMADURA CROPS2'
  CNAMES(515,2)='SP IT WCOAST CROPS2'
  CNAMES(516,2)='ESTREMADURA CROPS3'
  CNAMES(517,2)='MEDIT ISLANDS CROPS1'
  CNAMES(518,2)='SPAIN W COAST CROPS1'
  CNAMES(519,2)='ESTREMADURA CROPS4'
  CNAMES(520,2)='MECOAST SPARSE CROPS1'
  CNAMES(521,2)='BRITTANY CROPS1'
  CNAMES(522,2)='SYRIAN CROPS2'

!irrigated crops
  CNAMES(523,2)='NIL VALLEY CROPS1'
  CNAMES(524,2)='NIL VALLEY CROPS2'
  CNAMES(525,2)='NIL VALLEY CROPS3'
  CNAMES(526,2)='NIL VALLEY CROPS4'
  CNAMES(527,2)='SPANISH IRR CROPS1'
  CNAMES(528,2)='NIL VALLEY CROPS5'
  CNAMES(529,2)='EGEE IRR CROPS1'
  CNAMES(530,2)='MEDITER IRR CROPS1'
  CNAMES(531,2)='S SPAIN IRR CROPS1'
  CNAMES(532,2)='NIL VALLEY CROPS6'

!bare land
  CNAMES(533,2)='BARE ROCK1'
  CNAMES(534,2)='BARE ROCK2'
  CNAMES(535,2)='SANDY DESERT1'
  CNAMES(536,2)='BARE LAND1'
  CNAMES(537,2)='BARE LAND2'
  CNAMES(538,2)='BARE LAND3'
  CNAMES(539,2)='BARE LAND4'
  CNAMES(540,2)='BARE LAND5'
  CNAMES(541,2)='BARE LAND6'
  CNAMES(542,2)='BARE LAND7'
  CNAMES(543,2)='BARE LAND8'
  CNAMES(544,2)='BARE LAND9'
  CNAMES(545,2)='BARE LAND10'
  CNAMES(546,2)='BARE LAND11'
  CNAMES(547,2)='BARE LAND12'
  CNAMES(548,2)='PERMANENT SNOW1'

!Estuaries and swamp areas
  CNAMES(549,2)='WADDEN SEA'
  CNAMES(550,2)='ESTUARY1'
  CNAMES(551,2)='ESTUARY2'
  CNAMES(552,2)='POLAR WETLANDS1'
  CNAMES(553,2)='ESTUARY3'
  CNAMES(554,2)='ESTUARY4'
  CNAMES(555,2)='ESTUARY5'
  CNAMES(556,2)='ESTUARY6'
  CNAMES(557,2)='POLAR WETLANDS2'
  CNAMES(558,2)='SUBPOLAR WETLANDS1'
  CNAMES(559,2)='SUBPOLAR WETLANDS2'
  CNAMES(560,2)='SUBPOLAR WETLANDS3'

!urban
  CNAMES(561,2)='TEMPERATE SUBURBAN1'
  CNAMES(562,2)='TEMPERATE SUBURBAN2'
  CNAMES(563,2)='TEMPERATE SUBURBAN3'
  CNAMES(564,2)='TEMPERATE SUBURBAN4'
  CNAMES(565,2)='TEMPERATE SUBURBAN5'
  CNAMES(566,2)='COLD SUBURBAN1'
  CNAMES(567,2)='WARM SUBURBAN1'
  CNAMES(568,2)='WARM SUBURBAN2'
  CNAMES(569,2)='TEMPERATE SUBURBAN6'
  CNAMES(570,2)='TEMPERATE SUBURBAN7'
  CNAMES(571,2)='WARM SUBURBAN3'

!added classes of permanent crops
  CNAMES(572,2)='SPANISH VINEYARDS1'
  CNAMES(573,2)='LANGUEDOC VINEYARDS1'
ENDIF

IF (.NOT. ALLOCATED(CNAME)) ALLOCATE(CNAME(JPCOVER))
IF (CLANG=='EN') THEN
  DO JCOV=1,JPCOVER
    CNAME(JCOV) =   CNAMES(JCOV,1)
  ENDDO
ELSE
  DO JCOV=1,JPCOVER
    CNAME(JCOV) =   CNAMES(JCOV,2)
  ENDDO
END IF
!
ENDIF
!-------------------------------------------------------------------------------
!
!*    9.     Arrange cover (optional nam_pgd_arrange_cover & option to use !gardens or not)
!            ------------------------------------------------------------------------------
!
 CALL ARRANGE_COVER(DTCO, U%LWATER_TO_NATURE, U%LTOWN_TO_ROCK,                   &
                    XDATA_NATURE,XDATA_TOWN,XDATA_SEA,XDATA_WATER,XDATA_VEGTYPE, &
                    XDATA_GARDEN,U%LGARDEN, XDATA_BLD, XDATA_WALL_O_HOR            )
!
!-------------------------------------------------------------------------------
!
!*   10.     LAI for ecoclimap2: climatological or not
!            -----------------------------------------
!
IF (.NOT.U%LECOSG) CALL ECOCLIMAP2_LAI(DTCO%NYEAR)
!
!-------------------------------------------------------------------------------
!
!*    11.    Secondary variables on natural covers
!            -------------------------------------
!
 CALL INI_DATA_PARAM(PH_TREE=XDATA_H_TREE,PLAI=XDATA_LAI, &
                     PALBNIR_VEG=XDATA_ALBNIR_VEG, PALBVIS_VEG=XDATA_ALBVIS_VEG,                    &
                     PALBUV_VEG=XDATA_ALBUV_VEG, PRSMIN=XDATA_RSMIN,                                &
                     PRGL=XDATA_RGL, PCV=XDATA_CV, PGAMMA=XDATA_GAMMA,                              &
                     PGMES=XDATA_GMES, PGC=XDATA_GC, PBSLAI=XDATA_BSLAI,                            &
                     PSEFOLD=XDATA_SEFOLD, PLAIMIN_OUT=XDATA_LAIMIN, PDMAX=XDATA_DMAX,              &
                     PSTRESS=XDATA_STRESS, PF2I=XDATA_F2I, PVEG_OUT=XDATA_VEG,                      &
                     PGREEN=XDATA_GREEN, PZ0=XDATA_Z0, PZ0_O_Z0H=XDATA_Z0_O_Z0H,                    &
                     PEMIS_ECO=XDATA_EMIS_ECO, PWRMAX_CF=XDATA_WRMAX_CF,                            &
                     PROOT_LIN=XDATA_ROOT_LIN, PROOT_EXTINCTION=XDATA_ROOT_EXTINCTION,              &
                     PSOILRC_SO2=XDATA_SOILRC_SO2, PSOILRC_O3=XDATA_SOILRC_O3, PRE25=XDATA_RE25,    &
                     PCE_NITRO=XDATA_CE_NITRO,PCF_NITRO=XDATA_CF_NITRO,PCNA_NITRO=XDATA_CNA_NITRO,  &
                     PGMES_ST=XDATA_GMES_ST, PGC_ST=XDATA_GC_ST, PBSLAI_ST=XDATA_BSLAI_ST,          &
                     PSEFOLD_ST=XDATA_SEFOLD_ST, PDMAX_ST=XDATA_DMAX_ST,                            &
                     PGNDLITTER=XDATA_GNDLITTER, PH_VEG=XDATA_H_VEG, PZ0LITTER=XDATA_Z0LITTER      )
!
IDC = 1
!
!
! type dominant par cover
! veg roughness length par cover
! sand & clay par cover
! veg par cover
! albedo par cover (min max si varie)
! LAI par cover
! rsmin par cover
! root depth par cover
! orography par covers
!
IF (LHOOK) CALL DR_HOOK('INI_DATA_COVER',1,ZHOOK_HANDLE)
!
!------------------------------------------------------------------------------
!
END SUBROUTINE INI_DATA_COVER
END MODULE MODI_INI_DATA_COVER
