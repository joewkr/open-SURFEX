!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
MODULE MODI_PGD_ISBA_PAR
CONTAINS
      SUBROUTINE PGD_ISBA_PAR (DTCO, UG, U, USS, DTV, IO, S, KDIM, HPROGRAM)
!     ##############################################################
!
!!**** *PGD_ISBA_PAR* monitor for averaging and interpolations of cover fractions
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!
!
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
!!    Original    10/12/97
!!
!!       Modified 08/12/05, P. Le Moigne: user defined fields
!!                 05/2012  R. Alkama   : 19 vegtypes rather than 12
!!       Modified 02/2012,  P. Samuelsson: MEB
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_S_t
!
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODD_AGRI,        ONLY : LAGRIP
!
USE MODI_AV_PGD
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_INI_VAR_FROM_DATA
USE MODI_EXTRAPOL_FIELDS
!
USE MODE_POS_SURF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTV
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t), INTENT(INOUT) :: S
!
INTEGER, INTENT(IN) :: KDIM
!
 CHARACTER(LEN=6),    INTENT(IN)    :: HPROGRAM     ! Type of program
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
REAL, DIMENSION(KDIM,NVEGTYPE)     :: TEST,TEST2,TEST3
INTEGER               :: ILUOUT    ! output listing logical unit
INTEGER               :: ILUNAM    ! namelist file  logical unit
INTEGER               :: IHGROUND_LAYER ! Half number of NGROUND_LAYER
INTEGER               :: IIH       ! Ground layer counter
LOGICAL               :: GFOUND    ! true if namelist is found
!
INTEGER               :: JVEG, JL  ! loop counter on patch
LOGICAL               :: GPAR_STRESS   ! type of stress
!
INTEGER               :: ISIZE_LMEB_PATCH  ! Number of patches with MEB=true
!
!*    0.3    Declaration of namelists
!            ------------------------
!
INTEGER :: NTIME
INTEGER, PARAMETER :: NTIME_MAX    = 36
INTEGER, PARAMETER :: NGROUND_MAX  = 150
INTEGER, PARAMETER :: NVEGTYPE_MAX = 20
!
REAL, DIMENSION(NVEGTYPE_MAX)   :: XSTRESS   ! 1. if defensive /0. if offensive
!
REAL, DIMENSION(NVEGTYPE_MAX)             :: XUNIF_VEGTYPE    ! fractions of each vegtypes
!
REAL, DIMENSION(NVEGTYPE_MAX,NTIME_MAX)     :: XUNIF_VEG        ! vegetation fraction
REAL, DIMENSION(NVEGTYPE_MAX,NTIME_MAX)     :: XUNIF_LAI        ! LAI
REAL, DIMENSION(NVEGTYPE_MAX,NTIME_MAX)     :: XUNIF_Z0         ! roughness length of vegetation
REAL, DIMENSION(NVEGTYPE_MAX,NTIME_MAX)     :: XUNIF_EMIS       ! emissivity
!
REAL, DIMENSION(NVEGTYPE_MAX,NGROUND_MAX)   :: XUNIF_DG         ! soil depths
REAL, DIMENSION(NVEGTYPE_MAX,NGROUND_MAX)   :: XUNIF_ROOTFRAC   ! root fraction profiles
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_GROUND_DEPTH! ground depth
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_ROOT_DEPTH ! root depth
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_ROOT_EXTINCTION! root extinction parameter
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_ROOT_LIN   ! root linear parameter
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_DICE       ! soil ice depth for runoff
!
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_RSMIN      ! minimal stomatal resistance
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_GAMMA      ! gamma parameter
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_WRMAX_CF   ! coefficient for interception
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_RGL        !Rgl
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_CV         ! Cv
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_Z0_O_Z0H   ! ratio of roughness lengths
REAL, DIMENSION(NVEGTYPE_MAX,NTIME_MAX)     :: XUNIF_ALBNIR_VEG ! albedo of vegetation (near-infra-red)
REAL, DIMENSION(NVEGTYPE_MAX,NTIME_MAX)     :: XUNIF_ALBVIS_VEG ! albedo of vegetation (visible)
REAL, DIMENSION(NVEGTYPE_MAX,NTIME_MAX)     :: XUNIF_ALBUV_VEG  ! albedo of vegetation (UV)
REAL, DIMENSION(NVEGTYPE_MAX,NTIME_MAX)     :: XUNIF_ALBNIR_SOIL! albedo of soil (near-infra-red)
REAL, DIMENSION(NVEGTYPE_MAX,NTIME_MAX)     :: XUNIF_ALBVIS_SOIL! albedo of soil (visible)
REAL, DIMENSION(NVEGTYPE_MAX,NTIME_MAX)     :: XUNIF_ALBUV_SOIL ! albedo of soil (UV)
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_GMES       ! Gmes
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_BSLAI      ! Biomass over LAI
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_LAIMIN     ! minimum LAI
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_SEFOLD     ! Sefold
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_GC         ! Gc
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_DMAX       ! Dmax
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_F2I        ! F2I
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_H_TREE     ! height of trees
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_RE25       ! soil respiration parameter
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_CE_NITRO   ! CE for nitrogen
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_CF_NITRO   ! CF for nitrogen
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_CNA_NITRO  ! CNA for nitrogen
!
REAL, DIMENSION(NVEGTYPE_MAX,NTIME_MAX)     :: XUNIF_IRRIG
REAL, DIMENSION(NVEGTYPE_MAX,NTIME_MAX)     :: XUNIF_WATSUP
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_SEED_M
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_SEED_D
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_REAP_M
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_REAP_D
!
REAL, DIMENSION(NVEGTYPE_MAX,NTIME_MAX)     :: XUNIF_Z0LITTER
REAL, DIMENSION(NVEGTYPE_MAX,NTIME_MAX)     :: XUNIF_GNDLITTER
REAL, DIMENSION(NVEGTYPE_MAX,NTIME_MAX)     :: XUNIF_H_VEG
!
LOGICAL, DIMENSION(NVEGTYPE_MAX)            :: LUNIF_STRESS     ! stress type
!
REAL, DIMENSION(NGROUND_MAX)   :: XUNIF_CONDSAT
REAL, DIMENSION(NGROUND_MAX)   :: XUNIF_MPOTSAT
REAL, DIMENSION(NGROUND_MAX)   :: XUNIF_BCOEF
REAL, DIMENSION(NGROUND_MAX)   :: XUNIF_WWILT
REAL, DIMENSION(NGROUND_MAX)   :: XUNIF_WFC
REAL, DIMENSION(NGROUND_MAX)   :: XUNIF_WSAT
!
! name of files containing data
!
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)           :: CFNAM_VEGTYPE    ! fractions of each vegtypes
!
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFNAM_VEG        ! vegetation fraction
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFNAM_LAI        ! LAI
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFNAM_Z0         ! roughness length
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFNAM_EMIS       ! emissivity
!
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NGROUND_MAX) :: CFNAM_DG         ! soil depth
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NGROUND_MAX) :: CFNAM_ROOTFRAC   ! root fraction profiles
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_GROUND_DEPTH! ground depth
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_ROOT_DEPTH ! root depth
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_ROOT_EXTINCTION! root extinction parameter
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_ROOT_LIN   ! root linear parameter
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_DICE       ! soil ice depth for runoff (m)
!
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_RSMIN      ! minimal stomatal resistance
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_GAMMA      ! gamma parameter
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_WRMAX_CF   ! coefficient for interception
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_RGL        ! Rgl
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_CV         ! Cv
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_Z0_O_Z0H   ! ratio of roughness lengths
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFNAM_ALBNIR_VEG ! albedo of vegetation (near-infra-red)
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFNAM_ALBVIS_VEG ! albedo of vegetation (visible)
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFNAM_ALBUV_VEG  ! albedo of vegetation (UV)
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFNAM_ALBNIR_SOIL! albedo of soil (near-infra-red)
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFNAM_ALBVIS_SOIL! albedo of soil (visible)
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFNAM_ALBUV_SOIL ! albedo of soil (UV)
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_GMES       ! Gmes
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_BSLAI      ! Biomass over LAI
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_LAIMIN     ! minimum LAI
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_SEFOLD     ! e-folding time for senesence
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_GC         ! cuticular conductance
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_DMAX       ! Dmax
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_F2I        ! F2I
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_H_TREE     ! height of trees
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_RE25       ! soil respiration parameter
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_CE_NITRO   ! CE for nitrogen
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_CF_NITRO   ! CF for nitrogen
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_CNA_NITRO  ! CNA for nitrogen
!
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFNAM_IRRIG  !
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFNAM_WATSUP  !
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_SEED_M  !
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_SEED_D  !
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_REAP_M  !
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_REAP_D  !
!
CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFNAM_Z0LITTER
CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFNAM_GNDLITTER
CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFNAM_H_VEG
!
CHARACTER(LEN=28), DIMENSION(NGROUND_MAX)   :: CFNAM_CONDSAT
CHARACTER(LEN=28), DIMENSION(NGROUND_MAX)   :: CFNAM_MPOTSAT
CHARACTER(LEN=28), DIMENSION(NGROUND_MAX)   :: CFNAM_BCOEF
CHARACTER(LEN=28), DIMENSION(NGROUND_MAX)   :: CFNAM_WWILT
CHARACTER(LEN=28), DIMENSION(NGROUND_MAX)   :: CFNAM_WFC
CHARACTER(LEN=28), DIMENSION(NGROUND_MAX)   :: CFNAM_WSAT
!
! types of file containing data
!
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)           :: CFTYP_VEGTYPE    ! fractions of each vegtypes
!
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFTYP_VEG        ! vegetation fraction
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFTYP_LAI        ! LAI
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFTYP_Z0         ! roughness length
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFTYP_EMIS       ! emissivity
!
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NGROUND_MAX) :: CFTYP_DG         ! soil depth
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NGROUND_MAX) :: CFTYP_ROOTFRAC   ! root fraction profiles
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_GROUND_DEPTH! ground depth
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_ROOT_DEPTH ! root depth
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_ROOT_EXTINCTION! root extinction parameter
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_ROOT_LIN   ! root linear parameter
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_DICE       ! soil ice depth for runoff
!
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_RSMIN      ! minimal stomatal resistance
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_GAMMA      ! gamma parameter
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_WRMAX_CF   ! coefficient for interception
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_RGL        ! Rgl
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_CV         ! Cv
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_Z0_O_Z0H   ! ratio of roughness lengths
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFTYP_ALBNIR_VEG ! albedo of vegetation (near-infra-red)
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFTYP_ALBVIS_VEG ! albedo of vegetation (visible)
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFTYP_ALBUV_VEG  ! albedo of vegetation (UV)
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFTYP_ALBNIR_SOIL! albedo of soil (near-infra-red)
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFTYP_ALBVIS_SOIL! albedo of soil (visible)
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFTYP_ALBUV_SOIL ! albedo of soil (UV)
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_GMES       ! Gmes
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_BSLAI      ! Biomass over LAI
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_LAIMIN     ! minimum LAI
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_SEFOLD     ! e-folding time for senesence
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_GC         ! cuticular conductance
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_DMAX       ! Dmax
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_F2I        ! F2I
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_H_TREE     ! height of trees
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_RE25       ! soil respiration parameter
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_CE_NITRO   ! CE for nitrogen
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_CF_NITRO   ! CF for nitrogen
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_CNA_NITRO  ! CNA for nitrogen
!
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFTYP_IRRIG
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFTYP_WATSUP
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_SEED_M
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_SEED_D
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_REAP_M
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_REAP_D
!
CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFTYP_Z0LITTER
CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFTYP_GNDLITTER
CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFTYP_H_VEG
!
CHARACTER(LEN=6), DIMENSION(NGROUND_MAX)   :: CFTYP_CONDSAT
CHARACTER(LEN=6), DIMENSION(NGROUND_MAX)   :: CFTYP_MPOTSAT
CHARACTER(LEN=6), DIMENSION(NGROUND_MAX)   :: CFTYP_BCOEF
CHARACTER(LEN=6), DIMENSION(NGROUND_MAX)   :: CFTYP_WWILT
CHARACTER(LEN=6), DIMENSION(NGROUND_MAX)   :: CFTYP_WFC
CHARACTER(LEN=6), DIMENSION(NGROUND_MAX)   :: CFTYP_WSAT
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
NAMELIST/NAM_DATA_ISBA/NTIME, XUNIF_VEGTYPE, XUNIF_DG, XUNIF_ROOTFRAC, XUNIF_DICE,                  &
                         XUNIF_GROUND_DEPTH, XUNIF_ROOT_DEPTH, XUNIF_ROOT_EXTINCTION,               &
                         XUNIF_ROOT_LIN, XUNIF_LAI, XUNIF_VEG, XUNIF_Z0, XUNIF_EMIS,                &
                         XUNIF_RSMIN, XUNIF_GAMMA, XUNIF_WRMAX_CF, XUNIF_RGL,                       &
                         XUNIF_CV, XUNIF_Z0_O_Z0H,                                                  &
                         XUNIF_ALBNIR_VEG, XUNIF_ALBVIS_VEG, XUNIF_ALBUV_VEG,                       &
                         XUNIF_ALBNIR_SOIL, XUNIF_ALBVIS_SOIL, XUNIF_ALBUV_SOIL,                    &
                         XUNIF_GMES, XUNIF_BSLAI, XUNIF_LAIMIN, XUNIF_SEFOLD,                       &
                         XUNIF_GC, XUNIF_DMAX, XUNIF_F2I, LUNIF_STRESS, XUNIF_H_TREE, XUNIF_RE25,   &
                         XUNIF_CE_NITRO, XUNIF_CF_NITRO, XUNIF_CNA_NITRO,                           &
                         XUNIF_IRRIG, XUNIF_WATSUP, XUNIF_SEED_M, XUNIF_SEED_D, XUNIF_REAP_M, XUNIF_REAP_D, &
                         CFNAM_VEG,CFNAM_LAI,CFNAM_RSMIN,CFNAM_GAMMA,CFNAM_WRMAX_CF,                &
                         CFNAM_RGL,CFNAM_CV,CFNAM_DG,CFNAM_DICE,CFNAM_Z0,CFNAM_Z0_O_Z0H,            &
                         CFNAM_ALBNIR_VEG,CFNAM_ALBVIS_VEG,CFNAM_ALBUV_VEG,                         &
                         CFNAM_ALBNIR_SOIL,CFNAM_ALBVIS_SOIL,CFNAM_ALBUV_SOIL,                      &
                         CFNAM_EMIS,                                                                &
                         CFNAM_VEGTYPE,CFNAM_ROOTFRAC,                                              &
                         CFNAM_GROUND_DEPTH,CFNAM_ROOT_DEPTH,CFNAM_ROOT_EXTINCTION,CFNAM_ROOT_LIN,  &
                         CFNAM_GMES,CFNAM_BSLAI,CFNAM_LAIMIN,CFNAM_SEFOLD,CFNAM_GC,                 &
                         CFNAM_DMAX,CFNAM_F2I, CFNAM_H_TREE,CFNAM_RE25,                             &
                         CFNAM_CE_NITRO,CFNAM_CF_NITRO,CFNAM_CNA_NITRO,                             &
                         CFNAM_IRRIG, CFNAM_WATSUP, CFNAM_SEED_M, CFNAM_SEED_D, CFNAM_REAP_M, CFNAM_REAP_D, &
                         CFTYP_VEG,CFTYP_LAI,CFTYP_RSMIN,CFTYP_GAMMA,CFTYP_WRMAX_CF,                &
                         CFTYP_RGL,CFTYP_CV,CFTYP_DG,CFTYP_DICE,CFTYP_Z0,CFTYP_Z0_O_Z0H,            &
                         CFTYP_ALBNIR_VEG,CFTYP_ALBVIS_VEG,CFTYP_ALBUV_VEG,                         &
                         CFTYP_ALBNIR_SOIL,CFTYP_ALBVIS_SOIL,CFTYP_ALBUV_SOIL,                      &
                         CFTYP_EMIS,                                                                &
                         CFTYP_VEGTYPE,CFTYP_ROOTFRAC,                                              &
                         CFTYP_GROUND_DEPTH,CFTYP_ROOT_DEPTH,CFTYP_ROOT_EXTINCTION,CFTYP_ROOT_LIN,  &
                         CFTYP_GMES,CFTYP_BSLAI,CFTYP_LAIMIN,CFTYP_SEFOLD,CFTYP_GC,                 &
                         CFTYP_DMAX,CFTYP_F2I, CFTYP_H_TREE,CFTYP_RE25,                             &
                         CFTYP_CE_NITRO,CFTYP_CF_NITRO,CFTYP_CNA_NITRO,                             &
                         CFTYP_IRRIG, CFTYP_WATSUP, CFTYP_SEED_M, CFTYP_SEED_D, CFTYP_REAP_M, CFTYP_REAP_D, &
                         XUNIF_Z0LITTER, XUNIF_GNDLITTER, XUNIF_H_VEG, CFNAM_Z0LITTER,              &
                         CFNAM_GNDLITTER, CFNAM_H_VEG, CFTYP_Z0LITTER, CFTYP_GNDLITTER, CFTYP_H_VEG, &
                         XUNIF_CONDSAT, CFNAM_CONDSAT, CFTYP_CONDSAT, XUNIF_MPOTSAT, CFNAM_MPOTSAT, &
                         CFTYP_MPOTSAT, XUNIF_BCOEF, CFNAM_BCOEF, CFTYP_BCOEF, XUNIF_WWILT, CFNAM_WWILT, &
                         CFTYP_WWILT, XUNIF_WFC, CFNAM_WFC, CFTYP_WFC, XUNIF_WSAT, CFNAM_WSAT, CFTYP_WSAT

!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK) CALL DR_HOOK('PGD_ISBA_PAR',0,ZHOOK_HANDLE)
!
IF (U%LECOSG) THEN
             !4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
  XSTRESS = (/1.,1.,1.,0.,0.,0.,0.,0.,1.,1.,1.,0.,0.,0.,0.,1.,1.,0.,0.,0./)
ELSE
             !1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
  XSTRESS = (/1.,1.,1.,0.,1.,0.,1.,0.,1.,0.,0.,0.,0.,0.,1.,0.,1.,0.,0.,0./)
ENDIF
!
DTV%NVEGTYPE = NVEGTYPE
!
NTIME                 = 36
XUNIF_VEG             = XUNDEF ! vegetation fraction
XUNIF_LAI             = XUNDEF ! LAI
XUNIF_RSMIN           = XUNDEF ! minimal stomatal resistance
XUNIF_GAMMA           = XUNDEF ! gamma parameter
XUNIF_WRMAX_CF        = XUNDEF ! coefficient for interception
XUNIF_RGL             = XUNDEF ! Rgl
XUNIF_CV              = XUNDEF ! Cv
XUNIF_DG              = XUNDEF ! soil depths
XUNIF_DICE            = XUNDEF ! soil ice depths for runoff
XUNIF_Z0              = XUNDEF ! roughness length of vegetation
XUNIF_Z0_O_Z0H        = XUNDEF ! ratio of roughness lengths
XUNIF_ALBNIR_VEG      = XUNDEF ! albedo of vegetation (near-infra-red)
XUNIF_ALBVIS_VEG      = XUNDEF ! albedo of vegetation (visible)
XUNIF_ALBUV_VEG       = XUNDEF ! albedo of vegetation (UV)
XUNIF_ALBNIR_SOIL     = XUNDEF ! albedo of soil (near-infra-red)
XUNIF_ALBVIS_SOIL     = XUNDEF ! albedo of soil (visible)
XUNIF_ALBUV_SOIL      = XUNDEF ! albedo of soil (UV)
XUNIF_EMIS            = XUNDEF ! emissivity of vegetation
XUNIF_VEGTYPE         = XUNDEF ! fractions of each vegtypes
XUNIF_ROOTFRAC        = XUNDEF ! root fraction profiles
XUNIF_GROUND_DEPTH    = XUNDEF ! ground depth
XUNIF_ROOT_DEPTH      = XUNDEF ! root depth
XUNIF_ROOT_EXTINCTION = XUNDEF ! root extinction parameter
XUNIF_ROOT_LIN        = XUNDEF ! root linear parameter
XUNIF_GMES            = XUNDEF ! Gmes
XUNIF_BSLAI           = XUNDEF ! Biomass over LAI
XUNIF_LAIMIN          = XUNDEF ! minimum LAI
XUNIF_SEFOLD          = XUNDEF ! Sefold
XUNIF_GC              = XUNDEF ! Gc
XUNIF_DMAX            = XUNDEF ! Dmax
XUNIF_F2I             = XUNDEF ! F2I
LUNIF_STRESS          = .TRUE.! stress type
XUNIF_H_TREE          = XUNDEF ! height of trees
XUNIF_RE25            = XUNDEF ! soil respiration parameter
XUNIF_CE_NITRO        = XUNDEF ! CE for nitrogen
XUNIF_CF_NITRO        = XUNDEF ! CF for nitrogen
XUNIF_CNA_NITRO       = XUNDEF ! CNA for nitrogen
!
XUNIF_IRRIG           = XUNDEF
XUNIF_WATSUP          = XUNDEF
XUNIF_SEED_M          = XUNDEF
XUNIF_SEED_D          = XUNDEF
XUNIF_REAP_M          = XUNDEF
XUNIF_REAP_D          = XUNDEF
!
XUNIF_Z0LITTER        = XUNDEF
XUNIF_GNDLITTER       = XUNDEF
XUNIF_H_VEG           = XUNDEF
!
XUNIF_CONDSAT         = XUNDEF
XUNIF_MPOTSAT         = XUNDEF
XUNIF_BCOEF           = XUNDEF
XUNIF_WWILT           = XUNDEF
XUNIF_WFC             = XUNDEF
XUNIF_WSAT            = XUNDEF
!
CFNAM_VEGTYPE (:)     = '                            '
!
CFNAM_VEG  (:,:)      = '                            '
CFNAM_LAI  (:,:)      = '                            '
CFNAM_Z0   (:,:)      = '                            '
CFNAM_EMIS (:,:)      = '                            '
!
CFNAM_DG       (:,:)  = '                            '
CFNAM_ROOTFRAC (:,:)  = '                            '
CFNAM_DICE     (:)    = '                            '
!
CFNAM_GROUND_DEPTH    (:) = '                            '
CFNAM_ROOT_DEPTH      (:) = '                            '
CFNAM_ROOT_EXTINCTION (:) = '                            '
CFNAM_ROOT_LIN        (:) = '                            '
!
CFNAM_RSMIN       (:) = '                            '
CFNAM_GAMMA       (:) = '                            '
CFNAM_WRMAX_CF    (:) = '                            '
CFNAM_RGL         (:) = '                            '
CFNAM_CV          (:) = '                            '
CFNAM_Z0_O_Z0H    (:) = '                            '
CFNAM_ALBNIR_VEG  (:,:) = '                            '
CFNAM_ALBVIS_VEG  (:,:) = '                            '
CFNAM_ALBUV_VEG   (:,:) = '                            '
CFNAM_ALBNIR_SOIL (:,:) = '                            '
CFNAM_ALBVIS_SOIL (:,:) = '                            '
CFNAM_ALBUV_SOIL  (:,:) = '                            '
CFNAM_GMES        (:) = '                            '
CFNAM_BSLAI       (:) = '                            '
CFNAM_LAIMIN      (:) = '                            '
CFNAM_SEFOLD      (:) = '                            '
CFNAM_GC          (:) = '                            '
CFNAM_DMAX        (:) = '                            '
CFNAM_F2I         (:) = '                            '
CFNAM_H_TREE      (:) = '                            '
CFNAM_RE25        (:) = '                            '
CFNAM_CE_NITRO    (:) = '                            '
CFNAM_CF_NITRO    (:) = '                            '
CFNAM_CNA_NITRO   (:) = '                            '
!
CFNAM_IRRIG       (:,:) = '                            '
CFNAM_WATSUP      (:,:) = '                            '
CFNAM_SEED_M      (:) = '                            '
CFNAM_SEED_D      (:) = '                            '
CFNAM_REAP_M      (:) = '                            '
CFNAM_REAP_D      (:) = '                            '
!
CFNAM_Z0LITTER    (:,:) = '                            '
CFNAM_GNDLITTER   (:,:) = '                            '
CFNAM_H_VEG       (:,:) = '                            '
!
CFNAM_CONDSAT     (:) = '                            '
CFNAM_MPOTSAT     (:) = '                            '
CFNAM_BCOEF       (:) = '                            '
CFNAM_WWILT       (:) = '                            '
CFNAM_WFC         (:) = '                            '
CFNAM_WSAT        (:) = '                            '
!
CFTYP_VEGTYPE (:)     = '      '
!
CFTYP_VEG  (:,:)      = '      '
CFTYP_LAI  (:,:)      = '      '
CFTYP_Z0   (:,:)      = '      '
CFTYP_EMIS (:,:)      = '      '
!
CFTYP_DG       (:,:)  = '      '
CFTYP_ROOTFRAC (:,:)  = '      '
CFTYP_DICE     (:)    = '      '
!
CFTYP_GROUND_DEPTH    (:) = '      '
CFTYP_ROOT_DEPTH      (:) = '      '
CFTYP_ROOT_EXTINCTION (:) = '      '
CFTYP_ROOT_LIN        (:) = '      '
!
CFTYP_RSMIN       (:) = '      '
CFTYP_GAMMA       (:) = '      '
CFTYP_WRMAX_CF    (:) = '      '
CFTYP_RGL         (:) = '      '
CFTYP_CV          (:) = '      '
CFTYP_Z0_O_Z0H    (:) = '      '
CFTYP_ALBNIR_VEG  (:,:) = '      '
CFTYP_ALBVIS_VEG  (:,:) = '      '
CFTYP_ALBUV_VEG   (:,:) = '      '
CFTYP_ALBNIR_SOIL (:,:) = '      '
CFTYP_ALBVIS_SOIL (:,:) = '      '
CFTYP_ALBUV_SOIL  (:,:) = '      '
CFTYP_GMES        (:) = '      '
CFTYP_BSLAI       (:) = '      '
CFTYP_LAIMIN      (:) = '      '
CFTYP_SEFOLD      (:) = '      '
CFTYP_GC          (:) = '      '
CFTYP_DMAX        (:) = '      '
CFTYP_F2I         (:) = '      '
CFTYP_H_TREE      (:) = '      '
CFTYP_RE25        (:) = '      '
CFTYP_CE_NITRO    (:) = '      '
CFTYP_CF_NITRO    (:) = '      '
CFTYP_CNA_NITRO   (:) = '      '
!
CFTYP_IRRIG       (:,:) = '      '
CFTYP_WATSUP      (:,:) = '      '
CFTYP_SEED_M      (:) = '      '
CFTYP_SEED_D      (:) = '      '
CFTYP_REAP_M      (:) = '      '
CFTYP_REAP_D      (:) = '      '
!
CFTYP_Z0LITTER    (:,:) = '      '
CFTYP_GNDLITTER   (:,:) = '      '
CFTYP_H_VEG       (:,:) = '      '
!
CFTYP_CONDSAT     (:) = '      '
CFTYP_MPOTSAT     (:) = '      '
CFTYP_BCOEF       (:) = '      '
CFTYP_WWILT       (:) = '      '
CFTYP_WFC         (:) = '      '
CFTYP_WSAT        (:) = '      '
!
ISIZE_LMEB_PATCH=COUNT(IO%LMEB_PATCH(:))
!-------------------------------------------------------------------------------
!
!*    2.      Input file for cover types
!             --------------------------
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
 CALL POSNAM(ILUNAM,'NAM_DATA_ISBA',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_DATA_ISBA)
!
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
DTV%NTIME = NTIME
!
!-------------------------------------------------------------------------------
IF (NVEGTYPE_MAX < NVEGTYPE) THEN
  WRITE(ILUOUT,*) '------------------------------------'
  WRITE(ILUOUT,*) 'Please update pgd_isba_par.f90 routine : '
  WRITE(ILUOUT,*) 'The maximum number of VEGTYPE  '
  WRITE(ILUOUT,*) 'in the declaration of the namelist variables '
  WRITE(ILUOUT,*) 'must be increased to : ', NVEGTYPE
  WRITE(ILUOUT,*) '------------------------------------'
  CALL ABOR1_SFX('PGD_ISBA_PAR: MAXIMUM NUMBER OF VEGTYPE MUST BE INCREASED IN NAMELIST DECLARATION')
END IF
!-------------------------------------------------------------------------------
IF (NGROUND_MAX < IO%NGROUND_LAYER) THEN
  WRITE(ILUOUT,*) '------------------------------------'
  WRITE(ILUOUT,*) 'Please update pgd_isba_par.f90 routine : '
  WRITE(ILUOUT,*) 'The maximum number of soil layers  '
  WRITE(ILUOUT,*) 'in the declaration of the namelist variables '
  WRITE(ILUOUT,*) 'must be increased to : ', IO%NGROUND_LAYER
  WRITE(ILUOUT,*) '------------------------------------'
  CALL ABOR1_SFX('PGD_ISBA_PAR: MAXIMUM NUMBER OF SOIL LAYERS MUST BE INCREASED IN NAMELIST DECLARATION')
END IF
!-------------------------------------------------------------------------------
!
IF (NTIME/=36 .AND. NTIME/=12 .AND. NTIME/=2 .AND. NTIME/=1) &
   CALL ABOR1_SFX('PGD_ISBA_PAR: WRONG VALUE FOR NTIME (POSSIBLE VALUES ARE 1, 12 OR 36')
!
!-------------------------------------------------------------------------------
!
!*    3.      Uniform fields are prescribed
!             -----------------------------
!
!-------------------------------------vegtypes-----------------------------------------
!
ALLOCATE(DTV%XPAR_VEGTYPE(KDIM,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS,  &
                       HPROGRAM,'ARI','VEGTYPE: vegetation type','NAT',CFNAM_VEGTYPE,   &
                       CFTYP_VEGTYPE,XUNIF_VEGTYPE,DTV%XPAR_VEGTYPE,DTV%LDATA_VEGTYPE)
IF (.NOT. DTV%LDATA_VEGTYPE ) THEN
  DO JVEG = 1,NVEGTYPE
    CALL AV_PGD(DTCO, DTV%XPAR_VEGTYPE(:,JVEG),S%XCOVER,DTCO%XDATA_VEGTYPE(:,JVEG),'NAT','ARI',S%LCOVER)
  ENDDO
ENDIF
!
IF (.NOT.IO%LECOCLIMAP .AND. .NOT.DTV%LDATA_VEGTYPE) THEN
  !
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) '* Error in PGD field preparation of field VEGTYPE         *'
  WRITE(ILUOUT,*) '* There is no prescribed value and no input file          *'
  WRITE(ILUOUT,*) '* Without ECOCLIMAP, this field must be prescribed        *'
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) ' '
  CALL ABOR1_SFX('PGD_ISBA_PAR: NO PRESCRIBED VALUE NOR INPUT FILE FOR VEGTYPE')
  !
ELSEIF (DTV%LDATA_VEGTYPE) THEN
  !
  WHERE (DTV%XPAR_VEGTYPE(:,:)==XUNDEF) DTV%XPAR_VEGTYPE(:,:)=0.
  WHERE (DTV%XPAR_VEGTYPE(:,:)/=0.) DTV%XPAR_VEGTYPE(:,:) = DTV%XPAR_VEGTYPE(:,:) / &
                                                    SPREAD(SUM(DTV%XPAR_VEGTYPE(:,:),2),2,NVEGTYPE)
  !
ENDIF
!
!--------------------------------temporal fields-----------------------------------
!
ALLOCATE(DTV%LDATA_VEG(NTIME*NVEGTYPE))
ALLOCATE(DTV%XPAR_VEG(KDIM,NTIME,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                        HPROGRAM,'ARI','VEG: vegetation fraction','NAT',CFNAM_VEG,CFTYP_VEG,XUNIF_VEG, &
                        DTV%XPAR_VEG,DTV%LDATA_VEG)
IF (ALL(.NOT.DTV%LDATA_VEG)) DEALLOCATE(DTV%XPAR_VEG)
!
ALLOCATE(DTV%LDATA_LAI(NTIME*NVEGTYPE))
ALLOCATE(DTV%XPAR_LAI(KDIM,NTIME,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                        HPROGRAM,'ARI','LAI: leaf area index','NAT',CFNAM_LAI,CFTYP_LAI,XUNIF_LAI, &
                        DTV%XPAR_LAI,DTV%LDATA_LAI)
IF (ALL(.NOT.DTV%LDATA_LAI)) DEALLOCATE(DTV%XPAR_LAI)
!
ALLOCATE(DTV%LDATA_H_VEG(NTIME*NVEGTYPE))
ALLOCATE(DTV%XPAR_H_VEG(KDIM,NTIME,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                        HPROGRAM,'ARI','H_VEG: vegetation height','NAT',CFNAM_H_VEG,CFTYP_H_VEG,XUNIF_H_VEG, &
                        DTV%XPAR_H_VEG,DTV%LDATA_H_VEG)
IF (ALL(.NOT.DTV%LDATA_H_VEG)) DEALLOCATE(DTV%XPAR_H_VEG)
!
ALLOCATE(DTV%LDATA_Z0(NTIME*NVEGTYPE))
ALLOCATE(DTV%XPAR_Z0(KDIM,NTIME,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                        HPROGRAM,'CDN','Z0: roughness length','NAT',CFNAM_Z0,CFTYP_Z0,XUNIF_Z0, &
                        DTV%XPAR_Z0,DTV%LDATA_Z0)
IF (ALL(.NOT.DTV%LDATA_Z0)) DEALLOCATE(DTV%XPAR_Z0)
!
ALLOCATE(DTV%LDATA_EMIS(NTIME*NVEGTYPE))
ALLOCATE(DTV%XPAR_EMIS(KDIM,NTIME,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                        HPROGRAM,'ARI','EMIS: emissivity','NAT',CFNAM_EMIS,CFTYP_EMIS,XUNIF_EMIS, &
                        DTV%XPAR_EMIS,DTV%LDATA_EMIS)
IF (ALL(.NOT.DTV%LDATA_EMIS)) DEALLOCATE(DTV%XPAR_EMIS)
!
IF (.NOT.IO%LECOCLIMAP .AND. .NOT.(ANY(DTV%LDATA_VEG) .AND. ANY(DTV%LDATA_LAI) .AND. &
                                   ANY(DTV%LDATA_Z0) .AND. ANY(DTV%LDATA_EMIS))) THEN
  !
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) '* Error in PGD field preparation of temporal fields       *'
  WRITE(ILUOUT,*) '* There is no prescribed value and no input file :        *'
  IF (ALL(.NOT.DTV%LDATA_VEG )) WRITE(ILUOUT,*) '* for VEG                            *'
  IF (ALL(.NOT.DTV%LDATA_LAI )) WRITE(ILUOUT,*) '* for LAI                            *'
  IF (ALL(.NOT.DTV%LDATA_Z0  )) WRITE(ILUOUT,*) '* for Z0                             *'
  IF (ALL(.NOT.DTV%LDATA_EMIS)) WRITE(ILUOUT,*) '* for EMIS                           *'
  WRITE(ILUOUT,*) '* Without ECOCLIMAP, these fields must be prescribed      *'
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) ' '
  CALL ABOR1_SFX('PGD_ISBA_PAR: NO PRESCRIBED VALUE NOR INPUT FILE FOR TEMPORAL PARAMETERS')
  !
ENDIF
!
ALLOCATE(DTV%LDATA_ALBNIR_VEG(NTIME*NVEGTYPE))
ALLOCATE(DTV%XPAR_ALBNIR_VEG(KDIM,NTIME,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                       HPROGRAM,'ARI','ALBNIR_VEG: NIR albedo of vegetation','NAT',CFNAM_ALBNIR_VEG,   &
                       CFTYP_ALBNIR_VEG,XUNIF_ALBNIR_VEG,DTV%XPAR_ALBNIR_VEG,DTV%LDATA_ALBNIR_VEG)
IF (ALL(.NOT.DTV%LDATA_ALBNIR_VEG)) DEALLOCATE(DTV%XPAR_ALBNIR_VEG)
!
ALLOCATE(DTV%LDATA_ALBVIS_VEG(NTIME*NVEGTYPE))
ALLOCATE(DTV%XPAR_ALBVIS_VEG(KDIM,NTIME,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                       HPROGRAM,'ARI','ALBVIS_VEG: VIS albedo of vegetation','NAT',CFNAM_ALBVIS_VEG,   &
                       CFTYP_ALBVIS_VEG,XUNIF_ALBVIS_VEG,DTV%XPAR_ALBVIS_VEG,DTV%LDATA_ALBVIS_VEG)
IF (ALL(.NOT.DTV%LDATA_ALBVIS_VEG)) DEALLOCATE(DTV%XPAR_ALBVIS_VEG)
!
ALLOCATE(DTV%LDATA_ALBUV_VEG(NTIME*NVEGTYPE))
ALLOCATE(DTV%XPAR_ALBUV_VEG(KDIM,NTIME,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                       HPROGRAM,'ARI','ALBUV_VEG: UV albedo of vegetation','NAT',CFNAM_ALBUV_VEG,   &
                       CFTYP_ALBUV_VEG,XUNIF_ALBUV_VEG,DTV%XPAR_ALBUV_VEG,DTV%LDATA_ALBUV_VEG)
IF (ALL(.NOT.DTV%LDATA_ALBUV_VEG)) DEALLOCATE(DTV%XPAR_ALBUV_VEG)
!
ALLOCATE(DTV%LDATA_ALBNIR_SOIL(NTIME*NVEGTYPE))
ALLOCATE(DTV%XPAR_ALBNIR_SOIL(KDIM,NTIME,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                       HPROGRAM,'ARI','ALBNIR_SOIL: NIR albedo of SOIL','NAT',CFNAM_ALBNIR_SOIL,   &
                       CFTYP_ALBNIR_SOIL,XUNIF_ALBNIR_SOIL,DTV%XPAR_ALBNIR_SOIL,DTV%LDATA_ALBNIR_SOIL)
IF (ALL(.NOT.DTV%LDATA_ALBNIR_SOIL)) DEALLOCATE(DTV%XPAR_ALBNIR_SOIL)
!
ALLOCATE(DTV%LDATA_ALBVIS_SOIL(NTIME*NVEGTYPE))
ALLOCATE(DTV%XPAR_ALBVIS_SOIL(KDIM,NTIME,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                       HPROGRAM,'ARI','ALBVIS_SOIL: VIS albedo of SOIL','NAT',CFNAM_ALBVIS_SOIL,   &
                       CFTYP_ALBVIS_SOIL,XUNIF_ALBVIS_SOIL,DTV%XPAR_ALBVIS_SOIL,DTV%LDATA_ALBVIS_SOIL)
IF (ALL(.NOT.DTV%LDATA_ALBVIS_SOIL)) DEALLOCATE(DTV%XPAR_ALBVIS_SOIL)
!
ALLOCATE(DTV%LDATA_ALBUV_SOIL(NTIME*NVEGTYPE))
ALLOCATE(DTV%XPAR_ALBUV_SOIL(KDIM,NTIME,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                       HPROGRAM,'ARI','ALBUV_SOIL: UV albedo of SOIL','NAT',CFNAM_ALBUV_SOIL,   &
                       CFTYP_ALBUV_SOIL,XUNIF_ALBUV_SOIL,DTV%XPAR_ALBUV_SOIL,DTV%LDATA_ALBUV_SOIL)
IF (ALL(.NOT.DTV%LDATA_ALBUV_SOIL)) DEALLOCATE(DTV%XPAR_ALBUV_SOIL)
!
!
! ------------ Begin MEB parameters ---------------------
!
ALLOCATE(DTV%LDATA_GNDLITTER(NTIME*NVEGTYPE))
ALLOCATE(DTV%LDATA_Z0LITTER (NTIME*NVEGTYPE))
DTV%LDATA_GNDLITTER(:) = .FALSE.
DTV%LDATA_Z0LITTER (:) = .FALSE.
!
IF(ISIZE_LMEB_PATCH>0) THEN
  !
  ALLOCATE(DTV%XPAR_GNDLITTER(KDIM,NTIME,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                         HPROGRAM,'ARI','GNDLITTER: ground litter fraction','NAT',&
                         CFNAM_GNDLITTER,CFTYP_GNDLITTER,XUNIF_GNDLITTER,DTV%XPAR_GNDLITTER,DTV%LDATA_GNDLITTER)
  IF (ALL(.NOT.DTV%LDATA_GNDLITTER)) DEALLOCATE(DTV%XPAR_GNDLITTER)
  !
  ALLOCATE(DTV%XPAR_Z0LITTER(KDIM,NTIME,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                         HPROGRAM,'CDN','Z0LITTER: ground litter roughness length','NAT', &
                         CFNAM_Z0LITTER,CFTYP_Z0LITTER,XUNIF_Z0LITTER,DTV%XPAR_Z0LITTER,DTV%LDATA_Z0LITTER)
  IF (ALL(.NOT.DTV%LDATA_Z0LITTER)) DEALLOCATE(DTV%XPAR_Z0LITTER)
  !
ENDIF
! ------------ End MEB parameters ---------------------
!
!--------------------------------depths fields-----------------------------------
!
ALLOCATE(DTV%XPAR_DG(KDIM,IO%NGROUND_LAYER,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                         HPROGRAM,'ARI','DG: ground depth','NAT',CFNAM_DG,CFTYP_DG,&
                         XUNIF_DG,DTV%XPAR_DG,DTV%LDATA_DG(1:NVEGTYPE))
IF (ALL(.NOT.DTV%LDATA_DG)) DEALLOCATE(DTV%XPAR_DG)
!
ALLOCATE(DTV%XPAR_ROOT_DEPTH(KDIM,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                       HPROGRAM,'ARI','ROOT_DEPTH: root depth','NAT',CFNAM_ROOT_DEPTH,CFTYP_ROOT_DEPTH,&
                       XUNIF_ROOT_DEPTH,DTV%XPAR_ROOT_DEPTH,DTV%LDATA_ROOT_DEPTH)
IF (ALL(.NOT.DTV%LDATA_ROOT_DEPTH)) DEALLOCATE(DTV%XPAR_ROOT_DEPTH)
ALLOCATE(DTV%XPAR_GROUND_DEPTH(KDIM,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                        HPROGRAM,'ARI','GROUND_DEPTH: ground depth','NAT',CFNAM_GROUND_DEPTH,CFTYP_GROUND_DEPTH,&
      XUNIF_GROUND_DEPTH,DTV%XPAR_GROUND_DEPTH,DTV%LDATA_GROUND_DEPTH)
IF (ALL(.NOT.DTV%LDATA_GROUND_DEPTH)) DEALLOCATE(DTV%XPAR_GROUND_DEPTH)
!
IF(IO%CISBA=='DIF')THEN
  !
  ALLOCATE(DTV%XPAR_ROOTFRAC(KDIM,IO%NGROUND_LAYER,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                        HPROGRAM,'ARI','ROOTFRAC: root fraction','NAT',&
                        CFNAM_ROOTFRAC,CFTYP_ROOTFRAC,XUNIF_ROOTFRAC,&
                        DTV%XPAR_ROOTFRAC,DTV%LDATA_ROOTFRAC(1:NVEGTYPE))
  IF (ALL(.NOT.DTV%LDATA_ROOTFRAC)) DEALLOCATE(DTV%XPAR_ROOTFRAC)
  !
  ALLOCATE(DTV%XPAR_ROOT_EXTINCTION(KDIM,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                        HPROGRAM,'ARI','ROOT_EXTINCTION: root extinction','NAT',CFNAM_ROOT_EXTINCTION,CFTYP_ROOT_EXTINCTION,&
        XUNIF_ROOT_EXTINCTION,DTV%XPAR_ROOT_EXTINCTION,DTV%LDATA_ROOT_EXTINCTION)
  IF (ALL(.NOT.DTV%LDATA_ROOT_EXTINCTION)) DEALLOCATE(DTV%XPAR_ROOT_EXTINCTION)
  !
  ALLOCATE(DTV%XPAR_ROOT_LIN(KDIM,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                        HPROGRAM,'ARI','ROOT_LIN: root linear','NAT',CFNAM_ROOT_LIN,CFTYP_ROOT_LIN,&
        XUNIF_ROOT_LIN,DTV%XPAR_ROOT_LIN,DTV%LDATA_ROOT_LIN)
  IF (ALL(.NOT.DTV%LDATA_ROOT_LIN)) DEALLOCATE(DTV%XPAR_ROOT_LIN)
  !
  IF (.NOT.IO%LECOCLIMAP) THEN
    IF(ALL(.NOT.DTV%LDATA_DG) .AND. ALL(.NOT.DTV%LDATA_ROOTFRAC) .AND. &
       (ALL(.NOT.DTV%LDATA_ROOT_DEPTH).OR.ALL(.NOT.DTV%LDATA_ROOT_EXTINCTION).OR.ALL(.NOT.DTV%LDATA_ROOT_LIN))) THEN
      WRITE(ILUOUT,*) ' '
      WRITE(ILUOUT,*) '****************************************************************************'
      WRITE(ILUOUT,*) '* Without ECOCLIMAP, Error in PGD field preparation for ISBA-DIF           *'
      WRITE(ILUOUT,*) '* There is no prescribed value and no input file                           *'
      WRITE(ILUOUT,*) '*  (1) XUNIF_ROOTFRAC must be given.                                       *'
      WRITE(ILUOUT,*) '*  (2) Other solution, give all these fields:                              *'
      WRITE(ILUOUT,*) '*     - XUNIF_ROOT_DEPTH      (soil root depth)                            *'
      WRITE(ILUOUT,*) '*     - XUNIF_ROOT_EXTINCTION (root extinction parameter [Jackson 1996])   *'
      WRITE(ILUOUT,*) '*     - XUNIF_ROOT_LIN        (0.05 usually; 1=uniform root distribution!!)*'
      WRITE(ILUOUT,*) '****************************************************************************'
      WRITE(ILUOUT,*) ' '
      CALL ABOR1_SFX("PGD_ISBA_PAR: PROBLEM IN SOIL GRID COMPUTATION")
    ELSEIF( .NOT.ALL(IO%XSOILGRID(:)==XUNDEF) .AND. &
           (ALL(.NOT.DTV%LDATA_GROUND_DEPTH)   .OR.ALL(.NOT.DTV%LDATA_ROOT_DEPTH).OR. &
            ALL(.NOT.DTV%LDATA_ROOT_EXTINCTION).OR.ALL(.NOT.DTV%LDATA_ROOT_LIN)  )) THEN
      WRITE(ILUOUT,*) ' '
      WRITE(ILUOUT,*) '****************************************************************************'
      WRITE(ILUOUT,*) '* Without ECOCLIMAP, Error in PGD field preparation for ISBA-DIF           *'
      WRITE(ILUOUT,*) '* There is no prescribed value and no input file.                          *'
      WRITE(ILUOUT,*) '* When XSOILGRID is given, other field are needed :                        *'
      WRITE(ILUOUT,*) '*     - XUNIF_GROUND_DEPTH    (soil ground depth for moisture)             *'
      WRITE(ILUOUT,*) '*     - XUNIF_ROOT_DEPTH      (soil root depth)                            *'
      WRITE(ILUOUT,*) '*     - XUNIF_ROOT_LIN        (0.05 usually; 1=uniform root distribution!!)*'
      WRITE(ILUOUT,*) '*     - XUNIF_ROOT_EXTINCTION (root extinction parameter [Jackson 1996])   *'
      WRITE(ILUOUT,*) '****************************************************************************'
      WRITE(ILUOUT,*) ' '
      CALL ABOR1_SFX("PGD_ISBA_PAR: PROBLEM IN SOIL GRID COMPUTATION")
    ENDIF
    IF(ALL(.NOT.DTV%LDATA_DG) .AND.ALL(IO%XSOILGRID(:)==XUNDEF))THEN
      WRITE(ILUOUT,*) ' '
      WRITE(ILUOUT,*) '****************************************************************************'
      WRITE(ILUOUT,*) '* Without ECOCLIMAP, Error in PGD field preparation for ISBA-DIF           *'
      WRITE(ILUOUT,*) '* There is no prescribed value to compute vertical soil grid.              *'
      WRITE(ILUOUT,*) '* 2 solutions:                                                             *'
      WRITE(ILUOUT,*) '* (1) Give XUNIF_DG in NAM_DATA_ISBA.                                      *'
      WRITE(ILUOUT,*) '*  OR                                                                      *'
      WRITE(ILUOUT,*) '* (2) Give XSOILGRID in NAM_ISBA                                           *'
      WRITE(ILUOUT,*) '****************************************************************************'
      WRITE(ILUOUT,*) ' '
      CALL ABOR1_SFX("PGD_ISBA_PAR: PROBLEM IN SOIL GRID COMPUTATION")
    ENDIF
  ENDIF
  !
ELSE
  !
  IF ( .NOT.IO%LECOCLIMAP .AND. ALL(.NOT.DTV%LDATA_DG) .AND. &
      (ALL(.NOT.DTV%LDATA_GROUND_DEPTH).OR.ALL(.NOT.DTV%LDATA_ROOT_DEPTH)) ) THEN
    WRITE(ILUOUT,*) ' '
    WRITE(ILUOUT,*) '****************************************************************************'
    WRITE(ILUOUT,*) '* Without ECOCLIMAP, Error in PGD field preparation                        *'
    WRITE(ILUOUT,*) '* There is no prescribed value and no input file                           *'
    WRITE(ILUOUT,*) '* XUNIF_DG or both XUNIF_GROUND_DEPTH and XUNIF_ROOT_DEPTH must be given.  *'
    WRITE(ILUOUT,*) '****************************************************************************'
    WRITE(ILUOUT,*) ' '
    CALL ABOR1_SFX("PGD_ISBA_PAR: PROBLEM IN SOIL GRID COMPUTATION")
  ENDIF
  !
ENDIF
!
ALLOCATE(DTV%XPAR_DICE(KDIM,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                       HPROGRAM,'ARI','DICE: ice depth for runoff','NAT',CFNAM_DICE,CFTYP_DICE,&
                       XUNIF_DICE,DTV%XPAR_DICE,DTV%LDATA_DICE)
!
IF (.NOT.IO%LECOCLIMAP.AND.ALL(.NOT.DTV%LDATA_DICE)) THEN
  DTV%LDATA_DICE(:) = .FALSE.
  IF(IO%CISBA/='DIF' .AND. (ANY(DTV%LDATA_DG).OR.ANY(DTV%LDATA_ROOT_DEPTH))) THEN
    DO JVEG = 1,NVEGTYPE
      IF(DTV%LDATA_DG(JVEG))THEN
        DTV%LDATA_DICE(JVEG)=.TRUE.
        WHERE(DTV%XPAR_DG(:,2,JVEG)/=XUNDEF) DTV%XPAR_DICE(:,JVEG) = MAX(0.2,0.8*DTV%XPAR_DG(:,2,JVEG))
      ELSEIF(DTV%LDATA_ROOT_DEPTH(JVEG))THEN
        DTV%LDATA_DICE(JVEG)=.TRUE.
        WHERE(DTV%XPAR_ROOT_DEPTH(:,JVEG)/=XUNDEF) DTV%XPAR_DICE(:,JVEG) = MAX(0.2,0.8*DTV%XPAR_ROOT_DEPTH(:,JVEG))
      ENDIF
    ENDDO
  ELSEIF (IO%CISBA=='DIF') THEN
    DTV%XPAR_DICE(:,:) = 0.0
    DTV%LDATA_DICE(:)  =.TRUE.
  ELSE
    WRITE(ILUOUT,*) ' '
    WRITE(ILUOUT,*) '***********************************************************'
    WRITE(ILUOUT,*) '* Error in PGD field preparation of field DICE            *'
    WRITE(ILUOUT,*) '* There is no prescribed value and no input file          *'
    WRITE(ILUOUT,*) '* Without ECOCLIMAP, this field must be prescribed        *'
    WRITE(ILUOUT,*) '***********************************************************'
    WRITE(ILUOUT,*) ' '
    CALL ABOR1_SFX('PGD_ISBA_PAR: NO PRESCRIBED VALUE NOR INPUT FILE FOR DICE')
  ENDIF
ENDIF
IF (ALL(.NOT.DTV%LDATA_DICE)) DEALLOCATE(DTV%XPAR_DICE)
!
!---------------------classical fields---------------------------------------------
!
ALLOCATE(DTV%XPAR_RSMIN(KDIM,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                       HPROGRAM,'INV','RSMIN: minimal stomatal resistance','NAT',CFNAM_RSMIN,   &
                       CFTYP_RSMIN,XUNIF_RSMIN,DTV%XPAR_RSMIN,DTV%LDATA_RSMIN)
IF (ALL(.NOT.DTV%LDATA_RSMIN)) DEALLOCATE(DTV%XPAR_RSMIN)
!
ALLOCATE(DTV%XPAR_GAMMA(KDIM,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                       HPROGRAM,'ARI','GAMMA: gamma coefficient','NAT',CFNAM_GAMMA,   &
                       CFTYP_GAMMA,XUNIF_GAMMA,DTV%XPAR_GAMMA,DTV%LDATA_GAMMA)
IF (ALL(.NOT.DTV%LDATA_GAMMA)) DEALLOCATE(DTV%XPAR_GAMMA)
!
ALLOCATE(DTV%XPAR_WRMAX_CF(KDIM,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                       HPROGRAM,'ARI','WRMAX_CF: coeff. for max WR','NAT',CFNAM_WRMAX_CF,   &
                       CFTYP_WRMAX_CF,XUNIF_WRMAX_CF,DTV%XPAR_WRMAX_CF,DTV%LDATA_WRMAX_CF)
IF (ALL(.NOT.DTV%LDATA_WRMAX_CF)) DEALLOCATE(DTV%XPAR_WRMAX_CF)
!
ALLOCATE(DTV%XPAR_RGL(KDIM,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                       HPROGRAM,'ARI','RGL: max SW rad. for photosynthesis','NAT',CFNAM_RGL,   &
                       CFTYP_RGL,XUNIF_RGL,DTV%XPAR_RGL,DTV%LDATA_RGL)
IF (ALL(.NOT.DTV%LDATA_RGL)) DEALLOCATE(DTV%XPAR_RGL)
!
ALLOCATE(DTV%XPAR_CV(KDIM,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                       HPROGRAM,'INV','CV: thermal inertia for vegetation','NAT',CFNAM_CV,   &
                       CFTYP_CV,XUNIF_CV,DTV%XPAR_CV,DTV%LDATA_CV)
IF (ALL(.NOT.DTV%LDATA_CV)) DEALLOCATE(DTV%XPAR_CV)
!
ALLOCATE(DTV%XPAR_Z0_O_Z0H(KDIM,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                       HPROGRAM,'ARI','Z0_O_Z0H: ratio of roughness lengths','NAT',CFNAM_Z0_O_Z0H,   &
                       CFTYP_Z0_O_Z0H,XUNIF_Z0_O_Z0H,DTV%XPAR_Z0_O_Z0H,DTV%LDATA_Z0_O_Z0H)
IF (ALL(.NOT.DTV%LDATA_Z0_O_Z0H)) DEALLOCATE(DTV%XPAR_Z0_O_Z0H)
!
IF (.NOT.IO%LECOCLIMAP .AND. .NOT.(ANY(DTV%LDATA_RSMIN).AND.ANY(DTV%LDATA_GAMMA).AND.ANY(DTV%LDATA_WRMAX_CF).AND.&
         ANY(DTV%LDATA_RGL).AND.ANY(DTV%LDATA_CV).AND.ANY(DTV%LDATA_Z0_O_Z0H).AND.ANY(DTV%LDATA_ALBNIR_VEG).AND. &
         ANY(DTV%LDATA_ALBVIS_VEG).AND.ANY(DTV%LDATA_ALBUV_VEG).AND.ANY(DTV%LDATA_ALBNIR_SOIL).AND.&
         ANY(DTV%LDATA_ALBVIS_SOIL).AND.ANY(DTV%LDATA_ALBUV_SOIL))) THEN
  !
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) '* Error in PGD field preparation of classical fields      *'
  WRITE(ILUOUT,*) '* There is no prescribed value and no input file :        *'
  IF (ALL(.NOT.DTV%LDATA_RSMIN       )) WRITE(ILUOUT,*) '* for RSMIN                  *'
  IF (ALL(.NOT.DTV%LDATA_GAMMA       )) WRITE(ILUOUT,*) '* for GAMMA                  *'
  IF (ALL(.NOT.DTV%LDATA_WRMAX_CF    )) WRITE(ILUOUT,*) '* for WRMAX_CF               *'
  IF (ALL(.NOT.DTV%LDATA_RGL         )) WRITE(ILUOUT,*) '* for RGL                    *'
  IF (ALL(.NOT.DTV%LDATA_CV          )) WRITE(ILUOUT,*) '* for CV                     *'
  IF (ALL(.NOT.DTV%LDATA_Z0_O_Z0H    )) WRITE(ILUOUT,*) '* for Z0_O_Z0H               *'
  IF (ALL(.NOT.DTV%LDATA_ALBNIR_VEG  )) WRITE(ILUOUT,*) '* for ALBNIR_VEG             *'
  IF (ALL(.NOT.DTV%LDATA_ALBVIS_VEG  )) WRITE(ILUOUT,*) '* for ALBVIS_VEG             *'
  IF (ALL(.NOT.DTV%LDATA_ALBUV_VEG   )) WRITE(ILUOUT,*) '* for ALBUV_VEG              *'
  IF (ALL(.NOT.DTV%LDATA_ALBNIR_SOIL )) WRITE(ILUOUT,*) '* for ALBNIR_SOIL            *'
  IF (ALL(.NOT.DTV%LDATA_ALBVIS_SOIL )) WRITE(ILUOUT,*) '* for ALBVIS_SOIL            *'
  IF (ALL(.NOT.DTV%LDATA_ALBUV_SOIL  )) WRITE(ILUOUT,*) '* for ALBUV_SOIL             *'
  WRITE(ILUOUT,*) '* Without ECOCLIMAP, these fields must be prescribed      *'
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) ' '
  CALL ABOR1_SFX('PGD_ISBA_PAR: NO PRESCRIBED VALUE NOR INPUT FILE FOR CLASSICAL PARAMETERS')
  !
ENDIF
!
!--------------------------------------AGS parameters----------------------------
!
IF (IO%CPHOTO/='NON' .OR. &
    (ALL(.NOT.DTV%LDATA_Z0).AND.(ANY(DTV%LDATA_LAI).OR.DTV%LDATA_VEGTYPE)) .OR. ISIZE_LMEB_PATCH>0) THEN
  !
  ALLOCATE(DTV%XPAR_H_TREE(KDIM,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                        HPROGRAM,'ARI','H_TREE: height of trees','NAT',CFNAM_H_TREE,   &
                        CFTYP_H_TREE,XUNIF_H_TREE,DTV%XPAR_H_TREE,DTV%LDATA_H_TREE)
  IF (ALL(.NOT.DTV%LDATA_H_TREE)) THEN
    DEALLOCATE(DTV%XPAR_H_TREE)
  ELSE
    IF (U%LECOSG) THEN
      DTV%LDATA_H_TREE(1:3)   = .FALSE.
      DTV%LDATA_H_TREE(13:18) = .FALSE.
      DTV%LDATA_H_TREE(20)    = .FALSE.
    ELSE
      DTV%LDATA_H_TREE (1:3)  = .FALSE.
      DTV%LDATA_H_TREE (7:12) = .FALSE.
      DTV%LDATA_H_TREE(18:19) = .FALSE.
    ENDIF
  ENDIF
  !
ENDIF

IF (IO%CPHOTO/='NON' .OR. ISIZE_LMEB_PATCH>0) THEN
  ALLOCATE(DTV%XPAR_BSLAI(KDIM,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                       HPROGRAM,'ARI','BSLAI: biomass over LAI','NAT',CFNAM_BSLAI,   &
                       CFTYP_BSLAI,XUNIF_BSLAI,DTV%XPAR_BSLAI,DTV%LDATA_BSLAI)
  IF (ALL(.NOT.DTV%LDATA_BSLAI)) DEALLOCATE(DTV%XPAR_BSLAI)
ENDIF
!
IF (.NOT.IO%LECOCLIMAP .AND. ISIZE_LMEB_PATCH>0 ) THEN
  IF (.NOT.(ANY(DTV%LDATA_H_TREE).AND.ANY(DTV%LDATA_GNDLITTER).AND.ANY(DTV%LDATA_Z0LITTER)  &
      .AND. ANY(DTV%LDATA_BSLAI))) THEN
    WRITE(ILUOUT,*) ' '
    WRITE(ILUOUT,*) '***********************************************************'
    WRITE(ILUOUT,*) '* Error in PGD field preparation of MEB fields            *'
    WRITE(ILUOUT,*) '* There is no prescribed value and no input file :        *'
    IF (ALL(.NOT.DTV%LDATA_GNDLITTER   )) WRITE(ILUOUT,*) '* for GNDLITTER              *'
    IF (ALL(.NOT.DTV%LDATA_Z0LITTER    )) WRITE(ILUOUT,*) '* for Z0LITTER               *'
    IF (ALL(.NOT.DTV%LDATA_H_TREE      )) WRITE(ILUOUT,*) '* for H_TREE                 *'
    IF (ALL(.NOT.DTV%LDATA_BSLAI       )) WRITE(ILUOUT,*) '* for BSLAI                  *'
    WRITE(ILUOUT,*) '* Without ECOCLIMAP, these fields must be prescribed      *'
    WRITE(ILUOUT,*) '***********************************************************'
    WRITE(ILUOUT,*) ' '
    CALL ABOR1_SFX('PGD_ISBA_PAR: NO PRESCRIBED VALUE NOR INPUT FILE FOR MEB PARAMETERS')
  ENDIF
  !
ENDIF
!
ALLOCATE(DTV%LDATA_IRRIG (NTIME*NVEGTYPE))
ALLOCATE(DTV%LDATA_WATSUP(NTIME*NVEGTYPE))
DTV%LDATA_IRRIG (:) = .FALSE.
DTV%LDATA_WATSUP(:) = .FALSE.
!
IF (IO%CPHOTO/='NON') THEN
  !
  ALLOCATE(DTV%XPAR_RE25 (KDIM,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                         HPROGRAM,'ARI','RE25: ecosystem respiration','NAT',CFNAM_RE25,   &
                         CFTYP_RE25,XUNIF_RE25,DTV%XPAR_RE25,DTV%LDATA_RE25)
  IF (ALL(.NOT.DTV%LDATA_RE25)) DEALLOCATE(DTV%XPAR_RE25)
  !
  ALLOCATE(DTV%XPAR_LAIMIN(KDIM,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                         HPROGRAM,'ARI','LAIMIN: minimum LAI','NAT',CFNAM_LAIMIN,   &
                         CFTYP_LAIMIN,XUNIF_LAIMIN,DTV%XPAR_LAIMIN,DTV%LDATA_LAIMIN)
  IF (ALL(.NOT.DTV%LDATA_LAIMIN)) DEALLOCATE(DTV%XPAR_LAIMIN)
  !
  ALLOCATE(DTV%XPAR_SEFOLD(KDIM,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                         HPROGRAM,'ARI','SEFOLD: e-folding time for senescence','NAT',CFNAM_SEFOLD,   &
                         CFTYP_SEFOLD,XUNIF_SEFOLD,DTV%XPAR_SEFOLD,DTV%LDATA_SEFOLD)
  IF (ALL(.NOT.DTV%LDATA_SEFOLD)) DEALLOCATE(DTV%XPAR_SEFOLD)
  !
  ALLOCATE(DTV%XPAR_GMES(KDIM,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                         HPROGRAM,'ARI','GMES: mesophyl conductance','NAT',CFNAM_GMES,   &
                         CFTYP_GMES,XUNIF_GMES,DTV%XPAR_GMES,DTV%LDATA_GMES)
  IF (ALL(.NOT.DTV%LDATA_GMES)) DEALLOCATE(DTV%XPAR_GMES)
  !
  ALLOCATE(DTV%XPAR_GC(KDIM,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                         HPROGRAM,'ARI','GC: cuticular conductance','NAT',CFNAM_GC,   &
                         CFTYP_GC,XUNIF_GC,DTV%XPAR_GC,DTV%LDATA_GC)
  IF (ALL(.NOT.DTV%LDATA_GC)) DEALLOCATE(DTV%XPAR_GC)
  !
  IF (.NOT.IO%LECOCLIMAP .AND. .NOT.(ANY(DTV%LDATA_H_TREE).AND.ANY(DTV%LDATA_RE25).AND.&
          ANY(DTV%LDATA_LAIMIN).AND.ANY(DTV%LDATA_BSLAI).AND.ANY(DTV%LDATA_SEFOLD).AND.&
          ANY(DTV%LDATA_GMES).AND.ANY(DTV%LDATA_GC))) THEN
    !
    WRITE(ILUOUT,*) ' '
    WRITE(ILUOUT,*) '***********************************************************'
    WRITE(ILUOUT,*) '* Error in PGD field preparation of AGS fields            *'
    WRITE(ILUOUT,*) '* There is no prescribed value and no input file :        *'
    IF (ALL(.NOT.DTV%LDATA_H_TREE )) WRITE(ILUOUT,*) '* for H_TREE                      *'
    IF (ALL(.NOT.DTV%LDATA_RE25   )) WRITE(ILUOUT,*) '* for RE25                        *'
    IF (ALL(.NOT.DTV%LDATA_LAIMIN )) WRITE(ILUOUT,*) '* for LAIMIN                      *'
    IF (ALL(.NOT.DTV%LDATA_BSLAI  )) WRITE(ILUOUT,*) '* for BSLAI                       *'
    IF (ALL(.NOT.DTV%LDATA_SEFOLD )) WRITE(ILUOUT,*) '* for SEFOLD                      *'
    IF (ALL(.NOT.DTV%LDATA_GMES   )) WRITE(ILUOUT,*) '* for GMES                        *'
    IF (ALL(.NOT.DTV%LDATA_GC     )) WRITE(ILUOUT,*) '* for GC                          *'
    WRITE(ILUOUT,*) '* Without ECOCLIMAP, these fields must be prescribed      *'
    WRITE(ILUOUT,*) '***********************************************************'
    WRITE(ILUOUT,*) ' '
    CALL ABOR1_SFX('PGD_ISBA_PAR: NO PRESCRIBED VALUE NOR INPUT FILE FOR AGS PARAMETERS')
    !
  ENDIF
  !
  !--------------------------------------AGS Stress parameters----------------------------
  !
  ALLOCATE(DTV%XPAR_F2I(KDIM,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                         HPROGRAM,'ARI','F2I: critical normalized soil water content (stress)','NAT',CFNAM_F2I,   &
                         CFTYP_F2I,XUNIF_F2I,DTV%XPAR_F2I,DTV%LDATA_F2I)
  IF (ALL(.NOT.DTV%LDATA_F2I)) DEALLOCATE(DTV%XPAR_F2I)
  !
  ALLOCATE(DTV%XPAR_DMAX(KDIM,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                         HPROGRAM,'ARI','DMAX: maximum air saturation deficit','NAT',CFNAM_DMAX,   &
                         CFTYP_DMAX,XUNIF_DMAX,DTV%XPAR_DMAX,DTV%LDATA_DMAX)
  IF (ALL(.NOT.DTV%LDATA_DMAX)) DEALLOCATE(DTV%XPAR_DMAX)
  !
  ALLOCATE(DTV%LPAR_STRESS(KDIM,NVEGTYPE))
  DO JVEG=1,NVEGTYPE
    GPAR_STRESS = LUNIF_STRESS(JVEG)
    IF (XSTRESS(JVEG)<1.) GPAR_STRESS = .FALSE.
    IF (XSTRESS(JVEG)==1. .AND. .NOT.GPAR_STRESS) DTV%LDATA_STRESS=.TRUE.
    DTV%LPAR_STRESS(:,JVEG) = GPAR_STRESS
  ENDDO
  IF (ALL(.NOT.DTV%LDATA_STRESS)) DEALLOCATE(DTV%LPAR_STRESS)
  !
  IF (.NOT.IO%LECOCLIMAP .AND. .NOT.(ANY(DTV%LDATA_F2I).AND.ANY(DTV%LDATA_DMAX))) THEN
    !
    WRITE(ILUOUT,*) ' '
    WRITE(ILUOUT,*) '***********************************************************'
    WRITE(ILUOUT,*) '* Error in PGD field preparation of AGS Stress fields     *'
    WRITE(ILUOUT,*) '* There is no prescribed value and no input file :        *'
    IF (ALL(.NOT.DTV%LDATA_F2I  )) WRITE(ILUOUT,*) '* for F2I                           *'
    IF (ALL(.NOT.DTV%LDATA_DMAX )) WRITE(ILUOUT,*) '* for DMAX                          *'
    WRITE(ILUOUT,*) '* Without ECOCLIMAP, these fields must be prescribed      *'
    WRITE(ILUOUT,*) '***********************************************************'
    WRITE(ILUOUT,*) ' '
    CALL ABOR1_SFX('PGD_ISBA_PAR: NO PRESCRIBED VALUE NOR INPUT FILE FOR AGS STRESS PARAMETERS')
    !
  ENDIF
  !
  !--------------------------------------AGS Nitrogen parameters----------------------------
  !
  IF (IO%CPHOTO=='NIT' .OR. IO%CPHOTO=='NCB') THEN
    !
    ALLOCATE(DTV%XPAR_CE_NITRO(KDIM,NVEGTYPE))
    CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                           HPROGRAM,'ARI','CE_NITRO: leaf area ratio sensitivity to nitrogen ccion','NAT',&
                           CFNAM_CE_NITRO, CFTYP_CE_NITRO,XUNIF_CE_NITRO,DTV%XPAR_CE_NITRO,DTV%LDATA_CE_NITRO)
    IF (ALL(.NOT.DTV%LDATA_CE_NITRO)) DEALLOCATE(DTV%XPAR_CE_NITRO)
    !
    ALLOCATE(DTV%XPAR_CF_NITRO(KDIM,NVEGTYPE))
    CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                           HPROGRAM,'ARI','CF_NITRO: lethal minimum value of leaf area ratio','NAT',&
                           CFNAM_CF_NITRO,CFTYP_CF_NITRO,XUNIF_CF_NITRO,DTV%XPAR_CF_NITRO,DTV%LDATA_CF_NITRO)
    IF (ALL(.NOT.DTV%LDATA_CF_NITRO)) DEALLOCATE(DTV%XPAR_CF_NITRO)
    !
    ALLOCATE(DTV%XPAR_CNA_NITRO(KDIM,NVEGTYPE))
    CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                           HPROGRAM,'ARI','CNA_NITRO: nitrogen ccion of active biomass','NAT',&
                           CFNAM_CNA_NITRO,CFTYP_CNA_NITRO,XUNIF_CNA_NITRO,DTV%XPAR_CNA_NITRO,DTV%LDATA_CNA_NITRO)
    IF (ALL(.NOT.DTV%LDATA_CNA_NITRO)) DEALLOCATE(DTV%XPAR_CNA_NITRO)
    !
    IF (.NOT.IO%LECOCLIMAP .AND. &
         .NOT.(ANY(DTV%LDATA_CE_NITRO).AND.ANY(DTV%LDATA_CF_NITRO).AND.ANY(DTV%LDATA_CNA_NITRO))) THEN
      !
      WRITE(ILUOUT,*) ' '
      WRITE(ILUOUT,*) '***********************************************************'
      WRITE(ILUOUT,*) '* Error in PGD field preparation of AGS Nitrogen fields   *'
      WRITE(ILUOUT,*) '* There is no prescribed value and no input file :        *'
      IF (ALL(.NOT.DTV%LDATA_CE_NITRO  )) WRITE(ILUOUT,*) '* for CE_NITRO                 *'
      IF (ALL(.NOT.DTV%LDATA_CF_NITRO  )) WRITE(ILUOUT,*) '* for CF_NITRO                 *'
      IF (ALL(.NOT.DTV%LDATA_CNA_NITRO )) WRITE(ILUOUT,*) '* for CNA_NITRO                *'
      WRITE(ILUOUT,*) '* Without ECOCLIMAP, these fields must be prescribed      *'
      WRITE(ILUOUT,*) '***********************************************************'
      WRITE(ILUOUT,*) ' '
      CALL ABOR1_SFX('PGD_ISBA_PAR: NO PRESCRIBED VALUE NOR INPUT FILE FOR AGS NITROGEN PARAMETERS')
      !
    ENDIF
    !
    IF (LAGRIP) THEN
      !
      ALLOCATE(DTV%XPAR_IRRIG(KDIM,DTV%NTIME,NVEGTYPE))
      CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                             HPROGRAM,'ARI','IRRIG: flag for irrigation','NAT',&
                             CFNAM_IRRIG,CFTYP_IRRIG,XUNIF_IRRIG,DTV%XPAR_IRRIG,DTV%LDATA_IRRIG)
      IF (ALL(.NOT.DTV%LDATA_IRRIG)) DEALLOCATE(DTV%XPAR_IRRIG)
      !
      ALLOCATE(DTV%XPAR_WATSUP(KDIM,DTV%NTIME,NVEGTYPE))
      CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                             HPROGRAM,'ARI','WATSUP: water supply during irr. (mm)','NAT',&
                             CFNAM_WATSUP,CFTYP_WATSUP,XUNIF_WATSUP,DTV%XPAR_WATSUP,DTV%LDATA_WATSUP)
      IF (ALL(.NOT.DTV%LDATA_WATSUP)) DEALLOCATE(DTV%XPAR_WATSUP)
      !
      ALLOCATE(DTV%XPAR_SEED_M(KDIM,NVEGTYPE))
      CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                             HPROGRAM,'MAJ','SEED_M: month of seeding','NAT',&
                             CFNAM_SEED_M,CFTYP_SEED_M,XUNIF_SEED_M,DTV%XPAR_SEED_M,DTV%LDATA_SEED_M)
      IF (ALL(.NOT.DTV%LDATA_SEED_M)) DEALLOCATE(DTV%XPAR_SEED_M)
      !
      ALLOCATE(DTV%XPAR_SEED_D(KDIM,NVEGTYPE))
      CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                             HPROGRAM,'MAJ','SEED_D: day of seeding','NAT',&
                             CFNAM_SEED_D,CFTYP_SEED_D,XUNIF_SEED_D,DTV%XPAR_SEED_D,DTV%LDATA_SEED_D)
      IF (ALL(.NOT.DTV%LDATA_SEED_D)) DEALLOCATE(DTV%XPAR_SEED_D)
      !
      ALLOCATE(DTV%XPAR_REAP_M(KDIM,NVEGTYPE))
      CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                             HPROGRAM,'MAJ','REAP_M: month of reaping','NAT',&
                             CFNAM_REAP_M,CFTYP_REAP_M,XUNIF_REAP_M,DTV%XPAR_REAP_M,DTV%LDATA_REAP_M)
      IF (ALL(.NOT.DTV%LDATA_REAP_M)) DEALLOCATE(DTV%XPAR_REAP_M)
      !
      ALLOCATE(DTV%XPAR_REAP_D(KDIM,NVEGTYPE))
      CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                             HPROGRAM,'MAJ','REAP_D: day of reaping','NAT',&
                             CFNAM_REAP_D,CFTYP_REAP_D,XUNIF_REAP_D,DTV%XPAR_REAP_D,DTV%LDATA_REAP_D)
      IF (ALL(.NOT.DTV%LDATA_REAP_D)) DEALLOCATE(DTV%XPAR_REAP_D)
      !
    ENDIF
    !
    IF ((ANY(DTV%LDATA_IRRIG).OR.ANY(DTV%LDATA_WATSUP).OR.ANY(DTV%LDATA_SEED_M).OR.&
         ANY(DTV%LDATA_SEED_D).OR.ANY(DTV%LDATA_REAP_M).OR.ANY(DTV%LDATA_REAP_D)) .AND. &
        (ALL(.NOT.DTV%LDATA_IRRIG).OR.ALL(.NOT.DTV%LDATA_WATSUP).OR.ALL(.NOT.DTV%LDATA_SEED_M).OR.&
         ALL(.NOT.DTV%LDATA_SEED_D).OR.ALL(.NOT.DTV%LDATA_REAP_M).OR.ALL(.NOT.DTV%LDATA_REAP_D))) THEN
      !
      WRITE(ILUOUT,*) ' '
      WRITE(ILUOUT,*) '***********************************************************'
      WRITE(ILUOUT,*) '* Error in PGD field preparation of irrigation parameters   *'
      WRITE(ILUOUT,*) '* There is no prescribed value and no input file :          *'
      WRITE(ILUOUT,*) '* Some are prescribed and some are not. If you prescribe    *'
      WRITE(ILUOUT,*) '* one of IRRIG, WATSUP, SEED_M, SEED_D, REAP_M and REAP_D,  *'
      WRITE(ILUOUT,*) '* you need to prescribe all the others.  *'
      WRITE(ILUOUT,*) '***********************************************************'
      WRITE(ILUOUT,*) ' '
      CALL ABOR1_SFX('PGD_ISBA_PAR: MISSING PRESCRIBED VALUE OR INPUT FILE FOR IRRIGATION PARAMETERS')
      !
    ENDIF
    !
    IF (.NOT.IO%LECOCLIMAP .AND. .NOT.(ANY(DTV%LDATA_IRRIG).AND.ANY(DTV%LDATA_WATSUP).AND.&
            ANY(DTV%LDATA_SEED_M).AND.ANY(DTV%LDATA_SEED_D).AND.ANY(DTV%LDATA_REAP_M).AND.&
            ANY(DTV%LDATA_REAP_D))) THEN
      !
      WRITE(ILUOUT,*) ' '
      WRITE(ILUOUT,*) '***********************************************************'
      WRITE(ILUOUT,*) '* Error in PGD field preparation of irrigation fields      *'
      WRITE(ILUOUT,*) '* There is no prescribed value and no input file :         *'
      IF (ALL(.NOT.DTV%LDATA_IRRIG   )) WRITE(ILUOUT,*) '* for IRRIG                   *'
      IF (ALL(.NOT.DTV%LDATA_WATSUP  )) WRITE(ILUOUT,*) '* for WATSUP                  *'
      IF (ALL(.NOT.DTV%LDATA_SEED_M  )) WRITE(ILUOUT,*) '* for SEED_M                  *'
      IF (ALL(.NOT.DTV%LDATA_SEED_D  )) WRITE(ILUOUT,*) '* for SEED_D                  *'
      IF (ALL(.NOT.DTV%LDATA_REAP_M  )) WRITE(ILUOUT,*) '* for REAP_M                  *'
      IF (ALL(.NOT.DTV%LDATA_REAP_D  )) WRITE(ILUOUT,*) '* for REAP_D                  *'
      WRITE(ILUOUT,*) '* Without ECOCLIMAP, these fields must be prescribed      *'
      WRITE(ILUOUT,*) '***********************************************************'
      WRITE(ILUOUT,*) ' '
      CALL ABOR1_SFX('PGD_ISBA_PAR: NO PRESCRIBED VALUE NOR INPUT FILE FOR IRRIGATION PARAMETERS')
      !
    ENDIF

    !
  ENDIF
  !
ENDIF
!
!--------------------------------hydrological fields-----------------------------------
!
ALLOCATE(DTV%XPAR_CONDSAT(KDIM,IO%NGROUND_LAYER))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS,  &
                         HPROGRAM,'ARI','CONDSAT: ','NAT',CFNAM_CONDSAT,CFTYP_CONDSAT,&
                         XUNIF_CONDSAT,DTV%XPAR_CONDSAT,DTV%LDATA_CONDSAT)
IF (.NOT.DTV%LDATA_CONDSAT) DEALLOCATE(DTV%XPAR_CONDSAT)
!
ALLOCATE(DTV%XPAR_MPOTSAT(KDIM,IO%NGROUND_LAYER))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS,  &
                         HPROGRAM,'ARI','MPOTSAT: ','NAT',CFNAM_MPOTSAT,CFTYP_MPOTSAT,&
                         XUNIF_MPOTSAT,DTV%XPAR_MPOTSAT,DTV%LDATA_MPOTSAT)
IF (.NOT.DTV%LDATA_MPOTSAT) DEALLOCATE(DTV%XPAR_MPOTSAT)
!
ALLOCATE(DTV%XPAR_BCOEF(KDIM,IO%NGROUND_LAYER))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS,  &
                         HPROGRAM,'ARI','BCOEF: ','NAT',CFNAM_BCOEF,CFTYP_BCOEF,&
                         XUNIF_BCOEF,DTV%XPAR_BCOEF,DTV%LDATA_BCOEF)
IF (.NOT.DTV%LDATA_BCOEF) DEALLOCATE(DTV%XPAR_BCOEF)
!
ALLOCATE(DTV%XPAR_WWILT(KDIM,IO%NGROUND_LAYER))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS,  &
                         HPROGRAM,'ARI','WWILT: ','NAT',CFNAM_WWILT,CFTYP_WWILT,&
                         XUNIF_WWILT,DTV%XPAR_WWILT,DTV%LDATA_WWILT)
IF (.NOT.DTV%LDATA_WWILT) DEALLOCATE(DTV%XPAR_WWILT)
!
ALLOCATE(DTV%XPAR_WFC(KDIM,IO%NGROUND_LAYER))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS,  &
                         HPROGRAM,'ARI','WFC: ','NAT',CFNAM_WFC,CFTYP_WFC,&
                         XUNIF_WFC,DTV%XPAR_WFC,DTV%LDATA_WFC)
IF (.NOT.DTV%LDATA_WFC) DEALLOCATE(DTV%XPAR_WFC)
!
ALLOCATE(DTV%XPAR_WSAT(KDIM,IO%NGROUND_LAYER))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS,  &
                         HPROGRAM,'ARI','WSAT: ','NAT',CFNAM_WSAT,CFTYP_WSAT,&
                         XUNIF_WSAT,DTV%XPAR_WSAT,DTV%LDATA_WSAT)
IF (.NOT.DTV%LDATA_WSAT) DEALLOCATE(DTV%XPAR_WSAT)
!
!----------------------------------------------------------------------------------------
!
IF (IO%LECOCLIMAP .AND. DTV%LDATA_VEGTYPE) THEN
  !
  CALL EXTRAPOL_FIELDS(DTCO, DTV, KDIM, IO, S, UG, U, HPROGRAM,ILUOUT)
  !
ENDIF
!

!----------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PGD_ISBA_PAR',1,ZHOOK_HANDLE)
!
END SUBROUTINE PGD_ISBA_PAR
END MODULE MODI_PGD_ISBA_PAR
