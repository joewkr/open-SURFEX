!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_TEB_PAR (DTCO, UG, U, USS, BDD, DTT, KDIM, &
                              HPROGRAM,OGARDEN,OGREENROOF,HBLD_ATYPE)
!     ##############################################################
!
!!**** *PGD_TEB_PAR* monitor for averaging and interpolations of cover fractions
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
!!    G. Pigeon      09/2012: add ROUGH_WALL/ROUGH_ROOF for outdoor convection
!!    V. Masson      08/2013: adds solar panels
!!    V. Masson      10/2013: adds residential fraction
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
USE MODD_BLD_DESCRIPTION_n, ONLY : BLD_DESC_t
USE MODD_DATA_TEB_n, ONLY : DATA_TEB_t
!
USE MODD_SURF_PAR,   ONLY : XUNDEF, NUNDEF
!
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_INI_VAR_FROM_DATA_0D
USE MODI_INI_VAR_FROM_DATA
USE MODI_TEST_NAM_VAR_SURF
USE MODI_READ_CSVDATA_TEB
USE MODI_BLDCODE
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
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
TYPE(BLD_DESC_t), INTENT(INOUT) :: BDD
TYPE(DATA_TEB_t), INTENT(INOUT) :: DTT
!
INTEGER, INTENT(IN) :: KDIM
!
 CHARACTER(LEN=6),    INTENT(IN)    :: HPROGRAM     ! Type of program
LOGICAL,             INTENT(IN)    :: OGARDEN      ! T if urban green areas
LOGICAL,             INTENT(IN)    :: OGREENROOF   ! T if greenroofs option is activated
 CHARACTER(LEN=3),    INTENT(OUT)   :: HBLD_ATYPE    ! Type of building averaging
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER               :: ILUOUT    ! output listing logical unit
INTEGER               :: ILUNAM    ! namelist file  logical unit
LOGICAL               :: GFOUND    ! true if namelist is found
!
REAL, DIMENSION(KDIM) :: ZWORK
REAL                  :: ZUNIF     ! temporary variable
!
!*    0.3    Declaration of namelists
!            ------------------------
!
INTEGER, PARAMETER :: NROOF_MAX  = 9
INTEGER, PARAMETER :: NROAD_MAX  = 9
INTEGER, PARAMETER :: NWALL_MAX  = 9
INTEGER            :: NPAR_ROAD_LAYER ! number of road layers
INTEGER            :: NPAR_ROOF_LAYER ! number of roof layers
INTEGER            :: NPAR_WALL_LAYER ! number of wall layers
!
! Geometric Parameters:
!
INTEGER                                 :: NUNIF_BLDTYPE
 CHARACTER(LEN=28)                       :: CFNAM_BLDTYPE
 CHARACTER(LEN=6)                        :: CFTYP_BLDTYPE
INTEGER                                 :: NUNIF_BLD_AGE
 CHARACTER(LEN=28)                       :: CFNAM_BLD_AGE
 CHARACTER(LEN=6)                        :: CFTYP_BLD_AGE
 CHARACTER(LEN=28)                       :: CCSVDATAFILE
INTEGER                                 :: NUNIF_USETYPE
 CHARACTER(LEN=28)                       :: CFNAM_USETYPE
 CHARACTER(LEN=6)                        :: CFTYP_USETYPE
 CHARACTER(LEN=3)                        :: CBLD_ATYPE         ! type of averaging for buildings

!
REAL                                    :: XUNIF_BLD          ! fraction of buildings            (-)
REAL                                    :: XUNIF_BLD_HEIGHT   ! buildings height 'h'             (m)
REAL                                    :: XUNIF_WALL_O_HOR   ! wall surf. / hor. surf.          (-)
REAL                                    :: XUNIF_Z0_TOWN      ! roughness length for momentum    (m)
REAL                                    :: XUNIF_GARDEN       ! fraction of veg in the streets   (-)
REAL                                    :: XUNIF_GREENROOF    ! fraction of greenroofs on roofs  (-)
REAL                                    :: XUNIF_ROAD_DIR     ! road direction (deg from North, clockwise)
 CHARACTER(LEN=28)                       :: CFNAM_BLD          ! file name for BLD 
 CHARACTER(LEN=28)                       :: CFNAM_BLD_HEIGHT   ! file name for BLD_HEIGHT
 CHARACTER(LEN=28)                       :: CFNAM_WALL_O_HOR   ! file name for WALL_O_HOR
 CHARACTER(LEN=28)                       :: CFNAM_Z0_TOWN      ! file name for Z0_TOWN
 CHARACTER(LEN=28)                       :: CFNAM_GARDEN       ! file name for GARDEN  
 CHARACTER(LEN=28)                       :: CFNAM_GREENROOF    ! file name for GREENROOF
 CHARACTER(LEN=28)                       :: CFNAM_ROAD_DIR     ! file name for ROAD_DIR  
 CHARACTER(LEN=6)                        :: CFTYP_BLD          ! file type for BLD 
 CHARACTER(LEN=6)                        :: CFTYP_BLD_HEIGHT   ! file type for BLD_HEIGHT
 CHARACTER(LEN=6)                        :: CFTYP_WALL_O_HOR   ! file type for WALL_O_HOR
 CHARACTER(LEN=6)                        :: CFTYP_Z0_TOWN      ! file type for Z0_TOWN
 CHARACTER(LEN=6)                        :: CFTYP_GARDEN       ! file type for GARDEN  
 CHARACTER(LEN=6)                        :: CFTYP_GREENROOF    ! file type for GREENROOF
 CHARACTER(LEN=6)                        :: CFTYP_ROAD_DIR     ! file type for ROAD_DIR
!
! Roof parameters
!
REAL                                    :: XUNIF_ALB_ROOF     ! roof albedo                      (-)
REAL                                    :: XUNIF_EMIS_ROOF    ! roof emissivity                  (-)
 CHARACTER(LEN=28)                       :: CFNAM_ALB_ROOF     ! file name for ALB_ROOF
 CHARACTER(LEN=28)                       :: CFNAM_EMIS_ROOF    ! file name for EMIS_ROOF
 CHARACTER(LEN=6)                        :: CFTYP_ALB_ROOF     ! file name for ALB_ROOF   
 CHARACTER(LEN=6)                        :: CFTYP_EMIS_ROOF    ! file name for EMIS_ROOF  
REAL, DIMENSION(NROOF_MAX)              :: XUNIF_HC_ROOF      ! roof layers heat capacity        (J/K/m3)
REAL, DIMENSION(NROOF_MAX)              :: XUNIF_TC_ROOF      ! roof layers thermal conductivity (W/K/m)
REAL, DIMENSION(NROOF_MAX)              :: XUNIF_D_ROOF       ! depth of roof layers             (m)
 CHARACTER(LEN=28), DIMENSION(NROOF_MAX) :: CFNAM_HC_ROOF      ! file name for HC_ROOF   
 CHARACTER(LEN=28), DIMENSION(NROOF_MAX) :: CFNAM_TC_ROOF      ! file name for TC_ROOF
 CHARACTER(LEN=28), DIMENSION(NROOF_MAX) :: CFNAM_D_ROOF       ! file name for D_ROOF
 CHARACTER(LEN=6),  DIMENSION(NROOF_MAX) :: CFTYP_HC_ROOF      ! file type for HC_ROOF   
 CHARACTER(LEN=6),  DIMENSION(NROOF_MAX) :: CFTYP_TC_ROOF      ! file type for TC_ROOF
 CHARACTER(LEN=6),  DIMENSION(NROOF_MAX) :: CFTYP_D_ROOF       ! file type for D_ROOF
REAL                                    :: XUNIF_ROUGH_ROOF  ! roof roughness coef
 CHARACTER(LEN=28)                       :: CFNAM_ROUGH_ROOF  ! file name for ROUGH_ROOF
 CHARACTER(LEN=6)                        :: CFTYP_ROUGH_ROOF  ! file type for ROUGH_ROOF
!
!
! Road parameters
!
REAL                                    :: XUNIF_ALB_ROAD     ! road albedo                      (-)
REAL                                    :: XUNIF_EMIS_ROAD    ! road emissivity                  (-)
 CHARACTER(LEN=28)                       :: CFNAM_ALB_ROAD     ! file name for ALB_ROAD
 CHARACTER(LEN=28)                       :: CFNAM_EMIS_ROAD    ! file name for EMIS_ROAD
 CHARACTER(LEN=6)                        :: CFTYP_ALB_ROAD     ! file type for ALB_ROAD
 CHARACTER(LEN=6)                        :: CFTYP_EMIS_ROAD    ! file type for EMIS_ROAD
REAL, DIMENSION(NROAD_MAX)              :: XUNIF_HC_ROAD      ! road layers heat capacity        (J/K/m3)
REAL, DIMENSION(NROAD_MAX)              :: XUNIF_TC_ROAD      ! road layers thermal conductivity (W/K/m)
REAL, DIMENSION(NROAD_MAX)              :: XUNIF_D_ROAD       ! depth of road layers             (m)
 CHARACTER(LEN=28), DIMENSION(NROAD_MAX) :: CFNAM_HC_ROAD      ! file name for HC_ROAD   
 CHARACTER(LEN=28), DIMENSION(NROAD_MAX) :: CFNAM_TC_ROAD      ! file name for TC_ROAD
 CHARACTER(LEN=28), DIMENSION(NROAD_MAX) :: CFNAM_D_ROAD       ! file name for D_ROAD
 CHARACTER(LEN=6),  DIMENSION(NROAD_MAX) :: CFTYP_HC_ROAD      ! file type for HC_ROAD   
 CHARACTER(LEN=6),  DIMENSION(NROAD_MAX) :: CFTYP_TC_ROAD      ! file type for TC_ROAD
 CHARACTER(LEN=6),  DIMENSION(NROAD_MAX) :: CFTYP_D_ROAD       ! file type for D_ROAD
!
! Wall parameters
!
REAL                                    :: XUNIF_ALB_WALL     ! wall albedo                      (-)
REAL                                    :: XUNIF_EMIS_WALL    ! wall emissivity                  (-)
 CHARACTER(LEN=28)                       :: CFNAM_ALB_WALL     ! file name for ALB_WALL
 CHARACTER(LEN=28)                       :: CFNAM_EMIS_WALL    ! file name for EMIS_WALL
 CHARACTER(LEN=6)                        :: CFTYP_ALB_WALL     ! file type for ALB_WALL
 CHARACTER(LEN=6)                        :: CFTYP_EMIS_WALL    ! file type for EMIS_WALL
REAL, DIMENSION(NWALL_MAX)              :: XUNIF_HC_WALL      ! wall layers heat capacity        (J/K/m3)
REAL, DIMENSION(NWALL_MAX)              :: XUNIF_TC_WALL      ! wall layers thermal conductivity (W/K/m)
REAL, DIMENSION(NWALL_MAX)              :: XUNIF_D_WALL       ! depth of wall layers             (m)
 CHARACTER(LEN=28), DIMENSION(NWALL_MAX) :: CFNAM_HC_WALL      ! file name for HC_WALL   
 CHARACTER(LEN=28), DIMENSION(NWALL_MAX) :: CFNAM_TC_WALL      ! file name for TC_WALL
 CHARACTER(LEN=28), DIMENSION(NWALL_MAX) :: CFNAM_D_WALL       ! file name for D_WALL
 CHARACTER(LEN=6),  DIMENSION(NWALL_MAX) :: CFTYP_HC_WALL      ! file type for HC_WALL   
 CHARACTER(LEN=6),  DIMENSION(NWALL_MAX) :: CFTYP_TC_WALL      ! file type for TC_WALL
 CHARACTER(LEN=6),  DIMENSION(NWALL_MAX) :: CFTYP_D_WALL       ! file type for D_WALL
REAL                                    :: XUNIF_ROUGH_WALL  ! wall roughness coef
 CHARACTER(LEN=28)                       :: CFNAM_ROUGH_WALL  ! file name for ROUGH_WALL
 CHARACTER(LEN=6)                        :: CFTYP_ROUGH_WALL  ! file type for ROUGH_WALL
REAL                                    :: XUNIF_RESIDENTIAL ! residential fraction
 CHARACTER(LEN=28)                       :: CFNAM_RESIDENTIAL ! file name for RESIDENTIAL
 CHARACTER(LEN=6)                        :: CFTYP_RESIDENTIAL ! file type for RESIDENTIAL
!
! anthropogenic fluxes
!
REAL                                    :: XUNIF_H_TRAFFIC    ! anthropogenic sensible
!                                                             ! heat fluxes due to traffic       (W/m2)
REAL                                    :: XUNIF_LE_TRAFFIC   ! anthropogenic latent
!                                                             ! heat fluxes due to traffic       (W/m2)
REAL                                    :: XUNIF_H_INDUSTRY   ! anthropogenic sensible                   
!                                                             ! heat fluxes due to factories     (W/m2)
REAL                                    :: XUNIF_LE_INDUSTRY  ! anthropogenic latent
!                                                             ! heat fluxes due to factories     (W/m2)
 CHARACTER(LEN=28)                       :: CFNAM_H_TRAFFIC    ! file name for H_TRAFFIC
 CHARACTER(LEN=28)                       :: CFNAM_LE_TRAFFIC   ! file name for LE_TRAFFIC
 CHARACTER(LEN=28)                       :: CFNAM_H_INDUSTRY   ! file name for H_INDUSTRY
 CHARACTER(LEN=28)                       :: CFNAM_LE_INDUSTRY  ! file name for LE_INDUSTRY
 CHARACTER(LEN=6)                        :: CFTYP_H_TRAFFIC    ! file type for H_TRAFFIC
 CHARACTER(LEN=6)                        :: CFTYP_LE_TRAFFIC   ! file type for LE_TRAFFIC
 CHARACTER(LEN=6)                        :: CFTYP_H_INDUSTRY   ! file type for H_INDUSTRY
 CHARACTER(LEN=6)                        :: CFTYP_LE_INDUSTRY  ! file type for LE_INDUSTRY
!
! Solar panels parameters
!
REAL                                    :: XUNIF_EMIS_PANEL    ! emissivity of solar panel       (-)
REAL                                    :: XUNIF_ALB_PANEL     ! albedo     of solar panel       (-)
REAL                                    :: XUNIF_EFF_PANEL     ! efficiency of solar panel       (-)
REAL                                    :: XUNIF_FRAC_PANEL    ! fraction   of solar panel       (-)
 CHARACTER(LEN=28)                       :: CFNAM_EMIS_PANEL   ! file name for EMIS_PANEL
 CHARACTER(LEN=28)                       :: CFNAM_ALB_PANEL    ! file name for ALB_PANEL
 CHARACTER(LEN=28)                       :: CFNAM_EFF_PANEL    ! file name for EFF_PANEL
 CHARACTER(LEN=28)                       :: CFNAM_FRAC_PANEL   ! file name for FRAC_PANEL
 CHARACTER(LEN=6)                        :: CFTYP_EMIS_PANEL   ! file type for EMIS_PANEL
 CHARACTER(LEN=6)                        :: CFTYP_ALB_PANEL    ! file type for ALB_PANEL
 CHARACTER(LEN=6)                        :: CFTYP_EFF_PANEL    ! file type for EFF_PANEL
 CHARACTER(LEN=6)                        :: CFTYP_FRAC_PANEL   ! file type for FRAC_PANEL

REAL(KIND=JPRB) :: ZHOOK_HANDLE
!

NAMELIST/NAM_DATA_TEB/      NPAR_ROOF_LAYER, NPAR_ROAD_LAYER, NPAR_WALL_LAYER,&
                              CBLD_ATYPE,                                     &
                              NUNIF_BLDTYPE, CFNAM_BLDTYPE, CFTYP_BLDTYPE,    &
                              NUNIF_BLD_AGE, CFNAM_BLD_AGE, CFTYP_BLD_AGE,    &
                              CCSVDATAFILE,                                   &
                              NUNIF_USETYPE, CFNAM_USETYPE, CFTYP_USETYPE,    &
                              XUNIF_ALB_ROOF,                                 &
                              XUNIF_EMIS_ROOF, XUNIF_HC_ROOF, XUNIF_TC_ROOF,  &
                              XUNIF_D_ROOF, XUNIF_ALB_ROAD, XUNIF_EMIS_ROAD,  &
                              XUNIF_HC_ROAD, XUNIF_TC_ROAD, XUNIF_D_ROAD,     &
                              XUNIF_ALB_WALL, XUNIF_EMIS_WALL, XUNIF_HC_WALL, &
                              XUNIF_TC_WALL, XUNIF_D_WALL,                    &
                              XUNIF_Z0_TOWN, XUNIF_BLD, XUNIF_BLD_HEIGHT,     &
                              XUNIF_WALL_O_HOR,                               &
                              XUNIF_H_TRAFFIC, XUNIF_LE_TRAFFIC,              &
                              XUNIF_H_INDUSTRY, XUNIF_LE_INDUSTRY,            &
                              XUNIF_GARDEN, XUNIF_GREENROOF,                  &
                              XUNIF_ROAD_DIR,                                 &
                              CFNAM_ALB_ROOF,                                 &
                              CFNAM_EMIS_ROOF, CFNAM_HC_ROOF, CFNAM_TC_ROOF,  &
                              CFNAM_D_ROOF, CFNAM_ALB_ROAD, CFNAM_EMIS_ROAD,  &
                              CFNAM_HC_ROAD, CFNAM_TC_ROAD, CFNAM_D_ROAD,     &
                              CFNAM_ALB_WALL, CFNAM_EMIS_WALL, CFNAM_HC_WALL, &
                              CFNAM_TC_WALL, CFNAM_D_WALL,                    &
                              CFNAM_Z0_TOWN, CFNAM_BLD, CFNAM_BLD_HEIGHT,     &
                              CFNAM_WALL_O_HOR,                               &
                              CFNAM_H_TRAFFIC, CFNAM_LE_TRAFFIC,              &
                              CFNAM_H_INDUSTRY, CFNAM_LE_INDUSTRY,            &
                              CFNAM_GARDEN, CFNAM_ROAD_DIR, CFNAM_GREENROOF,  &
                              CFTYP_ALB_ROOF,                                 &
                              CFTYP_EMIS_ROOF, CFTYP_HC_ROOF, CFTYP_TC_ROOF,  &
                              CFTYP_D_ROOF, CFTYP_ALB_ROAD, CFTYP_EMIS_ROAD,  &
                              CFTYP_HC_ROAD, CFTYP_TC_ROAD, CFTYP_D_ROAD,     &
                              CFTYP_ALB_WALL, CFTYP_EMIS_WALL, CFTYP_HC_WALL, &
                              CFTYP_TC_WALL, CFTYP_D_WALL,                    &
                              CFTYP_Z0_TOWN, CFTYP_BLD, CFTYP_BLD_HEIGHT,     &
                              CFTYP_WALL_O_HOR,                               &
                              CFTYP_H_TRAFFIC, CFTYP_LE_TRAFFIC,              &
                              CFTYP_H_INDUSTRY, CFTYP_LE_INDUSTRY,            &
                              CFTYP_GARDEN, CFTYP_ROAD_DIR, CFTYP_GREENROOF,  &
                              XUNIF_ROUGH_ROOF, CFNAM_ROUGH_ROOF, CFTYP_ROUGH_ROOF, &
                              XUNIF_ROUGH_WALL, CFNAM_ROUGH_WALL, CFTYP_ROUGH_WALL, &
                              XUNIF_RESIDENTIAL,CFNAM_RESIDENTIAL,CFTYP_RESIDENTIAL,&
                              XUNIF_EMIS_PANEL, CFNAM_EMIS_PANEL, CFTYP_EMIS_PANEL, &
                              XUNIF_ALB_PANEL,  CFNAM_ALB_PANEL,  CFTYP_ALB_PANEL,  &
                              XUNIF_EFF_PANEL,  CFNAM_EFF_PANEL,  CFTYP_EFF_PANEL,  &
                              XUNIF_FRAC_PANEL, CFNAM_FRAC_PANEL, CFTYP_FRAC_PANEL

!
!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK)   CALL DR_HOOK('PGD_TEB_PAR',0,ZHOOK_HANDLE)
NPAR_ROOF_LAYER=0
NPAR_ROAD_LAYER=0
NPAR_WALL_LAYER=0
CBLD_ATYPE ='MAJ'
NUNIF_BLDTYPE      = NUNDEF
NUNIF_BLD_AGE      = NUNDEF
NUNIF_USETYPE      = NUNDEF
XUNIF_BLD          = XUNDEF
XUNIF_BLD_HEIGHT   = XUNDEF
XUNIF_WALL_O_HOR   = XUNDEF
XUNIF_Z0_TOWN      = XUNDEF
XUNIF_ALB_ROOF     = XUNDEF
XUNIF_EMIS_ROOF    = XUNDEF
XUNIF_HC_ROOF      = XUNDEF
XUNIF_TC_ROOF      = XUNDEF
XUNIF_D_ROOF       = XUNDEF
XUNIF_ALB_ROAD     = XUNDEF
XUNIF_EMIS_ROAD    = XUNDEF
XUNIF_HC_ROAD      = XUNDEF
XUNIF_TC_ROAD      = XUNDEF
XUNIF_D_ROAD       = XUNDEF
XUNIF_ALB_WALL     = XUNDEF
XUNIF_EMIS_WALL    = XUNDEF
XUNIF_HC_WALL      = XUNDEF
XUNIF_TC_WALL      = XUNDEF
XUNIF_D_WALL       = XUNDEF
XUNIF_H_TRAFFIC    = XUNDEF
XUNIF_LE_TRAFFIC   = XUNDEF
XUNIF_H_INDUSTRY   = XUNDEF
XUNIF_LE_INDUSTRY  = XUNDEF
XUNIF_GARDEN       = XUNDEF
XUNIF_GREENROOF    = XUNDEF
XUNIF_ROAD_DIR     = XUNDEF
XUNIF_ROUGH_ROOF   = XUNDEF
XUNIF_ROUGH_WALL   = XUNDEF
XUNIF_RESIDENTIAL  = XUNDEF
XUNIF_EMIS_PANEL   = XUNDEF
XUNIF_ALB_PANEL    = XUNDEF
XUNIF_EFF_PANEL    = XUNDEF
XUNIF_FRAC_PANEL   = XUNDEF

CFNAM_BLDTYPE      = '                            '
CFNAM_BLD_AGE      = '                            '
CFNAM_USETYPE      = '                            '
CCSVDATAFILE       ='                            '
CFNAM_BLD          = '                            '
CFNAM_BLD_HEIGHT   = '                            '
CFNAM_WALL_O_HOR   = '                            '
CFNAM_Z0_TOWN      = '                            '

CFNAM_ALB_ROOF (:) = '                            '
CFNAM_EMIS_ROOF(:) = '                            '
CFNAM_HC_ROOF  (:) = '                            '
CFNAM_TC_ROOF  (:) = '                            '
CFNAM_D_ROOF   (:) = '                            '
CFNAM_ROUGH_ROOF(:) = '                            '
CFNAM_ROUGH_WALL(:) = '                            '
CFNAM_RESIDENTIAL(:)= '                            '
CFNAM_ALB_ROAD (:) = '                            '
CFNAM_EMIS_ROAD(:) = '                            '
CFNAM_HC_ROAD  (:) = '                            '
CFNAM_TC_ROAD  (:) = '                            '
CFNAM_D_ROAD   (:) = '                            '
CFNAM_ALB_WALL (:) = '                            '
CFNAM_EMIS_WALL(:) = '                            '
CFNAM_HC_WALL  (:) = '                            '
CFNAM_TC_WALL  (:) = '                            '
CFNAM_D_WALL   (:) = '                            '

CFNAM_H_TRAFFIC    = '                            '
CFNAM_LE_TRAFFIC   = '                            '
CFNAM_H_INDUSTRY   = '                            '
CFNAM_LE_INDUSTRY  = '                            '

CFNAM_GARDEN       = '                            '
CFNAM_GREENROOF    = '                            '
CFNAM_ROAD_DIR     = '                            '

CFNAM_EMIS_PANEL   = '                            '
CFNAM_ALB_PANEL    = '                            '
CFNAM_EFF_PANEL    = '                            '
CFNAM_FRAC_PANEL   = '                            '

CFTYP_BLDTYPE      = '      '
CFTYP_BLD_AGE      = '      '
CFTYP_USETYPE      = '      '
CFTYP_BLD          = '      '
CFTYP_BLD_HEIGHT   = '      '
CFTYP_WALL_O_HOR   = '      '
CFTYP_Z0_TOWN      = '      '
CFTYP_ALB_ROOF(:)  = '      '
CFTYP_EMIS_ROOF(:) = '      '
CFTYP_HC_ROOF(:)   = '      '
CFTYP_TC_ROOF(:)   = '      '
CFTYP_D_ROOF(:)    = '      '
CFTYP_ROUGH_ROOF(:)    = '      '
CFTYP_ROUGH_WALL(:)    = '      '
CFTYP_RESIDENTIAL(:)   = '      '
CFTYP_ALB_ROAD(:)  = '      '
CFTYP_EMIS_ROAD(:) = '      '
CFTYP_HC_ROAD(:)   = '      '
CFTYP_TC_ROAD(:)   = '      '
CFTYP_D_ROAD(:)    = '      '
CFTYP_ALB_WALL(:)  = '      '
CFTYP_EMIS_WALL(:) = '      '
CFTYP_HC_WALL(:)   = '      '
CFTYP_TC_WALL(:)   = '      '
CFTYP_D_WALL(:)    = '      '
CFTYP_H_TRAFFIC    = '      '
CFTYP_LE_TRAFFIC   = '      '
CFTYP_H_INDUSTRY   = '      '
CFTYP_LE_INDUSTRY  = '      '
CFTYP_GARDEN       = '      '
CFTYP_GREENROOF    = '      '
CFTYP_ROAD_DIR     = '      '
!
CFTYP_EMIS_PANEL   = '      '
CFTYP_ALB_PANEL    = '      '
CFTYP_EFF_PANEL    = '      '
CFTYP_FRAC_PANEL   = '      '
!
!-------------------------------------------------------------------------------
!
!*    2.      Input file for cover types
!             --------------------------
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
 CALL POSNAM(ILUNAM,'NAM_DATA_TEB',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_DATA_TEB)
!
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CBLD_ATYPE',CBLD_ATYPE,'ARI','MAJ')
!
DTT%NPAR_ROOF_LAYER = NPAR_ROOF_LAYER
DTT%NPAR_ROAD_LAYER = NPAR_ROAD_LAYER
DTT%NPAR_WALL_LAYER = NPAR_WALL_LAYER
!
HBLD_ATYPE = CBLD_ATYPE
!-------------------------------------------------------------------------------
!
!* coherence check
!
IF ((     ANY(XUNIF_HC_ROAD/=XUNDEF) .OR. ANY(LEN_TRIM(CFNAM_HC_ROAD)>0) &
     .OR. ANY(XUNIF_TC_ROAD/=XUNDEF) .OR. ANY(LEN_TRIM(CFNAM_TC_ROAD)>0) &
     .OR. ANY(XUNIF_D_ROAD /=XUNDEF) .OR. ANY(LEN_TRIM(CFNAM_D_ROAD )>0) &
    ) .AND. NPAR_ROAD_LAYER<1                                  ) THEN
  CALL ABOR1_SFX('In order to initialize road thermal quantities, please specify NPAR_ROAD_LAYER in namelist NAM_DATA_TEB')
END IF
!
IF ((     ANY(XUNIF_HC_ROOF/=XUNDEF) .OR. ANY(LEN_TRIM(CFNAM_HC_ROOF)>0) &
     .OR. ANY(XUNIF_TC_ROOF/=XUNDEF) .OR. ANY(LEN_TRIM(CFNAM_TC_ROOF)>0) &
     .OR. ANY(XUNIF_D_ROOF /=XUNDEF) .OR. ANY(LEN_TRIM(CFNAM_D_ROOF )>0) &
    ) .AND. NPAR_ROOF_LAYER<1                                  ) THEN
  CALL ABOR1_SFX('In order to initialize ROOF thermal quantities, please specify NPAR_ROOF_LAYER in namelist NAM_DATA_TEB')
END IF
!
IF ((     ANY(XUNIF_HC_WALL/=XUNDEF) .OR. ANY(LEN_TRIM(CFNAM_HC_WALL)>0) &
     .OR. ANY(XUNIF_TC_WALL/=XUNDEF) .OR. ANY(LEN_TRIM(CFNAM_TC_WALL)>0) &
     .OR. ANY(XUNIF_D_WALL /=XUNDEF) .OR. ANY(LEN_TRIM(CFNAM_D_WALL )>0) &
    ) .AND. NPAR_WALL_LAYER<1                                  ) THEN
  CALL ABOR1_SFX('In order to initialize WALL thermal quantities, please specify NPAR_WALL_LAYER in namelist NAM_DATA_TEB')
END IF
!-------------------------------------------------------------------------------
IF (NROOF_MAX < NPAR_ROOF_LAYER) THEN
  WRITE(ILUOUT,*) '---------------------------------------------'
  WRITE(ILUOUT,*) 'Please update pgd_teb_par.f90 routine :      '
  WRITE(ILUOUT,*) 'The maximum number of ROOF LAYER             '
  WRITE(ILUOUT,*) 'in the declaration of the namelist variables '
  WRITE(ILUOUT,*) 'must be increased to : ', NPAR_ROOF_LAYER
  WRITE(ILUOUT,*) '---------------------------------------------'
  CALL ABOR1_SFX('PGD_TEB_PAR: MAXIMUM NUMBER OF NROOF_LAYER MUST BE INCREASED')
ENDIF
!-------------------------------------------------------------------------------
IF (NROAD_MAX < NPAR_ROAD_LAYER) THEN
  WRITE(ILUOUT,*) '---------------------------------------------'
  WRITE(ILUOUT,*) 'Please update pgd_teb_par.f90 routine :      '
  WRITE(ILUOUT,*) 'The maximum number of ROAD LAYER             '
  WRITE(ILUOUT,*) 'in the declaration of the namelist variables '
  WRITE(ILUOUT,*) 'must be increased to : ', NPAR_ROAD_LAYER
  WRITE(ILUOUT,*) '---------------------------------------------'
  CALL ABOR1_SFX('PGD_TEB_PAR: MAXIMUM NUMBER OF NROAD_LAYER MUST BE INCREASED')
ENDIF
!-------------------------------------------------------------------------------
IF (NWALL_MAX < NPAR_WALL_LAYER) THEN
  WRITE(ILUOUT,*) '---------------------------------------------'
  WRITE(ILUOUT,*) 'Please update pgd_teb_par.f90 routine :      '
  WRITE(ILUOUT,*) 'The maximum number of WALL LAYER             '
  WRITE(ILUOUT,*) 'in the declaration of the namelist variables '
  WRITE(ILUOUT,*) 'must be increased to : ', NPAR_WALL_LAYER
  WRITE(ILUOUT,*) '---------------------------------------------'
  CALL ABOR1_SFX('PGD_TEB_PAR: MAXIMUM NUMBER OF NWALL_LAYER MUST BE INCREASED')
ENDIF

!-------------------------------------------------------------------------------
!
!*    3.      user defined fields are prescribed
!             ----------------------------------
!
!* building's type
ZUNIF = XUNDEF
IF (NUNIF_BLDTYPE/=NUNDEF) ZUNIF=FLOAT(NUNIF_BLDTYPE)
ALLOCATE(DTT%NPAR_BLDTYPE     (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, HPROGRAM,'MAJ','BLDTYPE    ','TWN', &
        CFNAM_BLDTYPE,CFTYP_BLDTYPE,ZUNIF,ZWORK(:),DTT%LDATA_BLDTYPE )
IF (.NOT. DTT%LDATA_BLDTYPE) THEN
  DEALLOCATE(DTT%NPAR_BLDTYPE)
ELSE
  DTT%NPAR_BLDTYPE = NINT(ZWORK)
END IF
!
!* building's age
ZUNIF = XUNDEF
IF (NUNIF_BLD_AGE/=NUNDEF) ZUNIF=FLOAT(NUNIF_BLD_AGE)
ALLOCATE(DTT%NPAR_BLD_AGE     (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, HPROGRAM,'ARI','BLD_AGE    ','TWN', &
        CFNAM_BLD_AGE,CFTYP_BLD_AGE,ZUNIF,ZWORK(:),DTT%LDATA_BLD_AGE )
IF (.NOT. DTT%LDATA_BLD_AGE) THEN
  DEALLOCATE(DTT%NPAR_BLD_AGE)
ELSE
  DTT%NPAR_BLD_AGE = NINT(ZWORK)
END IF
!
IF (DTT%LDATA_BLDTYPE .AND. .NOT. DTT%LDATA_BLD_AGE) THEN
  WRITE(ILUOUT,*) '---------------------------------------------'
  WRITE(ILUOUT,*) ' You chose to define building types :        '
  IF (NUNIF_BLDTYPE/=NUNDEF) THEN
    WRITE(ILUOUT,*) ' NUNIF_BLDTYPE=', NUNIF_BLDTYPE
  ELSE
    WRITE(ILUOUT,*) ' CFNAM_BLDTYPE =',CFNAM_BLDTYPE 
    WRITE(ILUOUT,*) ' CFTYP_BLDTYPE =',CFTYP_BLDTYPE 
  END IF
  WRITE(ILUOUT,*) ' But            '
  WRITE(ILUOUT,*) " You did not chose to define building's age"
  WRITE(ILUOUT,*) '- - - - - - - - - - - - - - - - - - - - - - -'
  WRITE(ILUOUT,*) ' Please define the construction date of the buildings. '
  WRITE(ILUOUT,*) ' To do so, use either :'
  WRITE(ILUOUT,*) ' NUNIF_BLD_AGE   (to have a uniform construction date for all buildings'
  WRITE(ILUOUT,*) ' or CFNAM_BLD_AGE and CFTYP_BLD_AGE (to incorporate spatial data ) '
  WRITE(ILUOUT,*) '---------------------------------------------'
  CALL ABOR1_SFX("PGD_TEB_PAR: Building's age data is missing")
END IF
!
!* building's use
ZUNIF = XUNDEF
IF (NUNIF_USETYPE/=NUNDEF) ZUNIF=FLOAT(NUNIF_USETYPE)
ALLOCATE(DTT%NPAR_USETYPE     (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, HPROGRAM,'MAJ','USETYPE    ','TWN', &
        CFNAM_USETYPE,CFTYP_USETYPE,ZUNIF,ZWORK(:),DTT%LDATA_USETYPE )
IF (.NOT. DTT%LDATA_USETYPE) THEN
  DEALLOCATE(DTT%NPAR_USETYPE)
ELSE
  DTT%NPAR_USETYPE = NINT(ZWORK)
END IF
!
IF (DTT%LDATA_BLDTYPE .OR. DTT%LDATA_BLD_AGE .OR. DTT%LDATA_USETYPE) &
        CALL READ_CSVDATA_TEB(BDD, HPROGRAM,CCSVDATAFILE)
!
!* building's code
IF (ASSOCIATED(DTT%NPAR_BLDTYPE)) THEN
  ALLOCATE(DTT%NPAR_BLDCODE     (KDIM))
  DTT%NPAR_BLDCODE(:) = BLDCODE(BDD, DTT%NPAR_BLDTYPE,DTT%NPAR_BLD_AGE)
ENDIF
!
!
!* other building parameters
ALLOCATE(DTT%XPAR_BLD         (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, HPROGRAM,'ARI','BLD        ','TWN', &
        CFNAM_BLD,CFTYP_BLD,XUNIF_BLD,DTT%XPAR_BLD,DTT%LDATA_BLD )
IF (.NOT.DTT%LDATA_BLD) DEALLOCATE(DTT%XPAR_BLD)
!
ALLOCATE(DTT%XPAR_BLD_HEIGHT  (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, HPROGRAM,'ARI','BLD_HEIGHT ','TWN',&
        CFNAM_BLD_HEIGHT,CFTYP_BLD_HEIGHT,XUNIF_BLD_HEIGHT,&
        DTT%XPAR_BLD_HEIGHT,DTT%LDATA_BLD_HEIGHT)
IF (.NOT.DTT%LDATA_BLD_HEIGHT) DEALLOCATE(DTT%XPAR_BLD_HEIGHT)
!
ALLOCATE(DTT%XPAR_WALL_O_HOR  (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, HPROGRAM,'ARI','WALL_O_HOR ','TWN',&
        CFNAM_WALL_O_HOR,CFTYP_WALL_O_HOR,XUNIF_WALL_O_HOR,DTT%XPAR_WALL_O_HOR,DTT%LDATA_WALL_O_HOR)
IF (.NOT.DTT%LDATA_WALL_O_HOR) DEALLOCATE(DTT%XPAR_WALL_O_HOR)
!
ALLOCATE(DTT%XPAR_Z0_TOWN     (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, HPROGRAM,'CDN','Z0_TOWN    ','TWN',&
        CFNAM_Z0_TOWN,CFTYP_Z0_TOWN,XUNIF_Z0_TOWN,DTT%XPAR_Z0_TOWN,DTT%LDATA_Z0_TOWN)
IF (.NOT.DTT%LDATA_Z0_TOWN) DEALLOCATE(DTT%XPAR_Z0_TOWN)
!
ALLOCATE(DTT%XPAR_ALB_ROOF    (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, HPROGRAM,CBLD_ATYPE,'ALB_ROOF   ','TWN',&
          CFNAM_ALB_ROOF,CFTYP_ALB_ROOF,XUNIF_ALB_ROOF  ,DTT%XPAR_ALB_ROOF,DTT%LDATA_ALB_ROOF)
IF (.NOT.DTT%LDATA_ALB_ROOF) DEALLOCATE(DTT%XPAR_ALB_ROOF)
!
ALLOCATE(DTT%XPAR_EMIS_ROOF   (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, HPROGRAM,CBLD_ATYPE,'EMIS_ROOF  ','TWN',&
        CFNAM_EMIS_ROOF,CFTYP_EMIS_ROOF,XUNIF_EMIS_ROOF ,DTT%XPAR_EMIS_ROOF,DTT%LDATA_EMIS_ROOF)
IF (.NOT.DTT%LDATA_EMIS_ROOF) DEALLOCATE(DTT%XPAR_EMIS_ROOF)
!
ALLOCATE(DTT%XPAR_HC_ROOF     (KDIM,NPAR_ROOF_LAYER))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, HPROGRAM,CBLD_ATYPE,'HC_ROOF  ','TWN',&
        CFNAM_HC_ROOF,CFTYP_HC_ROOF, XUNIF_HC_ROOF,DTT%XPAR_HC_ROOF,DTT%LDATA_HC_ROOF ) 
IF (.NOT.DTT%LDATA_HC_ROOF) DEALLOCATE(DTT%XPAR_HC_ROOF)
! 
ALLOCATE(DTT%XPAR_TC_ROOF     (KDIM,NPAR_ROOF_LAYER))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, HPROGRAM,CBLD_ATYPE,'TC_ROOF  ','TWN',&
        CFNAM_TC_ROOF,CFTYP_TC_ROOF, XUNIF_TC_ROOF ,DTT%XPAR_TC_ROOF, DTT%LDATA_TC_ROOF ) 
IF (.NOT.DTT%LDATA_TC_ROOF) DEALLOCATE(DTT%XPAR_TC_ROOF)
! 
ALLOCATE(DTT%XPAR_D_ROOF      (KDIM,NPAR_ROOF_LAYER))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, HPROGRAM,CBLD_ATYPE,'D_ROOF   ','TWN',&
        CFNAM_D_ROOF,CFTYP_D_ROOF, XUNIF_D_ROOF  ,DTT%XPAR_D_ROOF , DTT%LDATA_D_ROOF ) 
IF (.NOT.DTT%LDATA_D_ROOF) DEALLOCATE(DTT%XPAR_D_ROOF)
! 
ALLOCATE(DTT%XPAR_ALB_ROAD    (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, HPROGRAM,'ARI','ALB_ROAD   ','TWN',&
        CFNAM_ALB_ROAD  ,CFTYP_ALB_ROAD  ,XUNIF_ALB_ROAD  ,DTT%XPAR_ALB_ROAD, DTT%LDATA_ALB_ROAD  )
IF (.NOT.DTT%LDATA_ALB_ROAD) DEALLOCATE(DTT%XPAR_ALB_ROAD)
!
ALLOCATE(DTT%XPAR_EMIS_ROAD   (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, HPROGRAM,'ARI','EMIS_ROAD  ','TWN',&
        CFNAM_EMIS_ROAD ,CFTYP_EMIS_ROAD ,XUNIF_EMIS_ROAD ,DTT%XPAR_EMIS_ROAD, DTT%LDATA_EMIS_ROAD )
IF (.NOT.DTT%LDATA_EMIS_ROAD) DEALLOCATE(DTT%XPAR_EMIS_ROAD)
!
ALLOCATE(DTT%XPAR_HC_ROAD     (KDIM,NPAR_ROAD_LAYER))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, &
                        HPROGRAM,CBLD_ATYPE,'HC_ROAD  ','TWN',CFNAM_HC_ROAD ,CFTYP_HC_ROAD , &
                   XUNIF_HC_ROAD ,DTT%XPAR_HC_ROAD, DTT%LDATA_HC_ROAD  )  
IF (.NOT.DTT%LDATA_HC_ROAD) DEALLOCATE(DTT%XPAR_HC_ROAD)
!
ALLOCATE(DTT%XPAR_TC_ROAD     (KDIM,NPAR_ROAD_LAYER))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, &
                        HPROGRAM,CBLD_ATYPE,'TC_ROAD  ','TWN',CFNAM_TC_ROAD ,CFTYP_TC_ROAD , &
                   XUNIF_TC_ROAD ,DTT%XPAR_TC_ROAD, DTT%LDATA_TC_ROAD  )  
IF (.NOT.DTT%LDATA_TC_ROAD) DEALLOCATE(DTT%XPAR_TC_ROAD)
!
ALLOCATE(DTT%XPAR_D_ROAD      (KDIM,NPAR_ROAD_LAYER))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, &
                        HPROGRAM,'ARI','D_ROAD   ','TWN',CFNAM_D_ROAD  ,CFTYP_D_ROAD  , &
                   XUNIF_D_ROAD  ,DTT%XPAR_D_ROAD , DTT%LDATA_D_ROAD  )
IF (.NOT.DTT%LDATA_D_ROAD) DEALLOCATE(DTT%XPAR_D_ROAD)
!  
ALLOCATE(DTT%XPAR_ALB_WALL    (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,CBLD_ATYPE,'ALB_WALL   ','TWN',CFNAM_ALB_WALL  ,CFTYP_ALB_WALL  ,XUNIF_ALB_WALL  ,&
        DTT%XPAR_ALB_WALL, DTT%LDATA_ALB_WALL   )
IF (.NOT.DTT%LDATA_ALB_WALL) DEALLOCATE(DTT%XPAR_ALB_WALL)
!
ALLOCATE(DTT%XPAR_EMIS_WALL   (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,CBLD_ATYPE,'EMIS_WALL  ','TWN',CFNAM_EMIS_WALL ,CFTYP_EMIS_WALL ,XUNIF_EMIS_WALL ,&
        DTT%XPAR_EMIS_WALL, DTT%LDATA_EMIS_WALL  )
IF (.NOT.DTT%LDATA_EMIS_WALL) DEALLOCATE(DTT%XPAR_EMIS_WALL)
!
ALLOCATE(DTT%XPAR_HC_WALL     (KDIM,NPAR_WALL_LAYER))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, &
                        HPROGRAM,CBLD_ATYPE,'HC_WALL  ','TWN',CFNAM_HC_WALL ,CFTYP_HC_WALL , &
                   XUNIF_HC_WALL ,DTT%XPAR_HC_WALL, DTT%LDATA_HC_WALL  ) 
IF (.NOT.DTT%LDATA_HC_WALL) DEALLOCATE(DTT%XPAR_HC_WALL)
! 
ALLOCATE(DTT%XPAR_TC_WALL     (KDIM,NPAR_WALL_LAYER))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, &
                        HPROGRAM,CBLD_ATYPE,'TC_WALL  ','TWN',CFNAM_TC_WALL ,CFTYP_TC_WALL , &
                   XUNIF_TC_WALL ,DTT%XPAR_TC_WALL, DTT%LDATA_TC_WALL  ) 
IF (.NOT.DTT%LDATA_TC_WALL) DEALLOCATE(DTT%XPAR_TC_WALL)
! 
ALLOCATE(DTT%XPAR_D_WALL      (KDIM,NPAR_WALL_LAYER))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, &
                        HPROGRAM,CBLD_ATYPE,'D_WALL   ','TWN',CFNAM_D_WALL  ,CFTYP_D_WALL  , &
                   XUNIF_D_WALL  ,DTT%XPAR_D_WALL , DTT%LDATA_D_WALL  ) 
IF (.NOT.DTT%LDATA_D_WALL) DEALLOCATE(DTT%XPAR_D_WALL)
! 
ALLOCATE(DTT%XPAR_H_TRAFFIC   (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','H_TRAFFIC  ','TWN',CFNAM_H_TRAFFIC  ,CFTYP_H_TRAFFIC  ,XUNIF_H_TRAFFIC  ,&
        DTT%XPAR_H_TRAFFIC, DTT%LDATA_H_TRAFFIC   )
IF (.NOT.DTT%LDATA_H_TRAFFIC) DEALLOCATE(DTT%XPAR_H_TRAFFIC)
!
ALLOCATE(DTT%XPAR_LE_TRAFFIC  (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','LE_TRAFFIC ','TWN',CFNAM_LE_TRAFFIC ,CFTYP_LE_TRAFFIC ,XUNIF_LE_TRAFFIC ,&
        DTT%XPAR_LE_TRAFFIC, DTT%LDATA_LE_TRAFFIC  )
IF (.NOT.DTT%LDATA_LE_TRAFFIC) DEALLOCATE(DTT%XPAR_LE_TRAFFIC)
!
ALLOCATE(DTT%XPAR_H_INDUSTRY  (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','H_INDUSTRY ','TWN',CFNAM_H_INDUSTRY ,CFTYP_H_INDUSTRY ,XUNIF_H_INDUSTRY ,&
        DTT%XPAR_H_INDUSTRY, DTT%LDATA_H_INDUSTRY  )
IF (.NOT.DTT%LDATA_H_INDUSTRY) DEALLOCATE(DTT%XPAR_H_INDUSTRY)
!
ALLOCATE(DTT%XPAR_LE_INDUSTRY (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','LE_INDUSTRY','TWN',CFNAM_LE_INDUSTRY,CFTYP_LE_INDUSTRY,XUNIF_LE_INDUSTRY,&
        DTT%XPAR_LE_INDUSTRY, DTT%LDATA_LE_INDUSTRY )
IF (.NOT.DTT%LDATA_LE_INDUSTRY) DEALLOCATE(DTT%XPAR_LE_INDUSTRY)
!
ALLOCATE(DTT%XPAR_ROUGH_ROOF    (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,CBLD_ATYPE,'ROUGH_ROOF','TWN',CFNAM_ROUGH_ROOF,CFTYP_ROUGH_ROOF,XUNIF_ROUGH_ROOF ,&
        DTT%XPAR_ROUGH_ROOF,DTT%LDATA_ROUGH_ROOF)
IF (.NOT.DTT%LDATA_ROUGH_ROOF) DEALLOCATE(DTT%XPAR_ROUGH_ROOF)
!
ALLOCATE(DTT%XPAR_ROUGH_WALL    (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,CBLD_ATYPE,'ROUGH_WALL','TWN',CFNAM_ROUGH_WALL,CFTYP_ROUGH_WALL,XUNIF_ROUGH_WALL ,&
        DTT%XPAR_ROUGH_WALL,DTT%LDATA_ROUGH_WALL)
IF (.NOT.DTT%LDATA_ROUGH_WALL) DEALLOCATE(DTT%XPAR_ROUGH_WALL)
!
ALLOCATE(DTT%XPAR_RESIDENTIAL   (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,CBLD_ATYPE,'RESIDENTIAL','TWN',CFNAM_RESIDENTIAL,CFTYP_RESIDENTIAL,XUNIF_RESIDENTIAL ,&
        DTT%XPAR_RESIDENTIAL,DTT%LDATA_RESIDENTIAL)
IF (.NOT.DTT%LDATA_RESIDENTIAL) DEALLOCATE(DTT%XPAR_RESIDENTIAL)
!-------------------------------------------------------------------------------
!
!* coherence checks
!
 CALL COHERENCE_THERMAL_DATA('ROAD',DTT%LDATA_HC_ROAD,DTT%LDATA_TC_ROAD,DTT%LDATA_D_ROAD)
 CALL COHERENCE_THERMAL_DATA('ROOF',DTT%LDATA_HC_ROOF,DTT%LDATA_TC_ROOF,DTT%LDATA_D_ROOF)
 CALL COHERENCE_THERMAL_DATA('WALL',DTT%LDATA_HC_WALL,DTT%LDATA_TC_WALL,DTT%LDATA_D_WALL)

!-------------------------------------------------------------------------------
!
!* road directions
!
ALLOCATE(DTT%XPAR_ROAD_DIR    (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','ROAD_DIR   ','TWN',CFNAM_ROAD_DIR  ,CFTYP_ROAD_DIR    ,XUNIF_ROAD_DIR   ,&
        DTT%XPAR_ROAD_DIR, DTT%LDATA_ROAD_DIR    )
IF (.NOT.DTT%LDATA_ROAD_DIR) DEALLOCATE(DTT%XPAR_ROAD_DIR)
!
!-------------------------------------------------------------------------------
!
!* solar panels
!
ALLOCATE(DTT%XPAR_EMIS_PANEL  (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','EMIS_PANEL ','BLD',CFNAM_EMIS_PANEL,CFTYP_EMIS_PANEL,XUNIF_EMIS_PANEL,&
       DTT%XPAR_EMIS_PANEL, DTT%LDATA_EMIS_PANEL    )
IF (.NOT.DTT%LDATA_EMIS_PANEL) DEALLOCATE(DTT%XPAR_EMIS_PANEL)
ALLOCATE(DTT%XPAR_ALB_PANEL   (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','ALB_PANEL  ','BLD',CFNAM_ALB_PANEL ,CFTYP_ALB_PANEL ,XUNIF_ALB_PANEL ,&
       DTT%XPAR_ALB_PANEL , DTT%LDATA_ALB_PANEL     )
IF (.NOT.DTT%LDATA_ALB_PANEL ) DEALLOCATE(DTT%XPAR_ALB_PANEL )
ALLOCATE(DTT%XPAR_EFF_PANEL   (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','EFF_PANEL  ','BLD',CFNAM_EFF_PANEL ,CFTYP_EFF_PANEL ,XUNIF_EFF_PANEL ,&
       DTT%XPAR_EFF_PANEL , DTT%LDATA_EFF_PANEL     )
IF (.NOT.DTT%LDATA_EFF_PANEL ) DEALLOCATE(DTT%XPAR_EFF_PANEL )
ALLOCATE(DTT%XPAR_FRAC_PANEL  (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','FRAC_PANEL ','BLD',CFNAM_FRAC_PANEL,CFTYP_FRAC_PANEL,XUNIF_FRAC_PANEL,&
       DTT%XPAR_FRAC_PANEL, DTT%LDATA_FRAC_PANEL    )
IF (.NOT.DTT%LDATA_FRAC_PANEL) DEALLOCATE(DTT%XPAR_FRAC_PANEL)
!
!-------------------------------------------------------------------------------
!
!* greenroof fraction
!
IF (OGREENROOF) THEN
   ALLOCATE(DTT%XPAR_GREENROOF   (KDIM))
  CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,CBLD_ATYPE,'GREENROOF','BLD',CFNAM_GREENROOF,CFTYP_GREENROOF,XUNIF_GREENROOF ,&
        DTT%XPAR_GREENROOF,DTT%LDATA_GREENROOF)
  IF (.NOT.DTT%LDATA_GREENROOF) DEALLOCATE(DTT%XPAR_GREENROOF)
ELSE IF ( (XUNIF_GREENROOF/=0. .AND. XUNIF_GREENROOF/=XUNDEF) .OR. LEN_TRIM(CFNAM_GREENROOF)/=0) THEN
  WRITE(ILUOUT,*) '---------------------------------------------'
  WRITE(ILUOUT,*) ' You chose not to include greenroofs in urban areas : LGREENROOF=.FALSE.     '
  WRITE(ILUOUT,*) ' But            '
  IF (XUNIF_GREENROOF/=0. .AND. XUNIF_GREENROOF/=XUNDEF) THEN
    WRITE(ILUOUT,*) ' You also chose a greenroof fraction that is not zero : XUNIF_GREENROOF=',XUNIF_GREENROOF
  ELSE
    WRITE(ILUOUT,*) ' You also chose a greenroof fraction that is not zero : CFNAM_GREENROOF=',CFNAM_GREENROOF
  END IF
  WRITE(ILUOUT,*) '- - - - - - - - - - - - - - - - - - - - - - -'
  WRITE(ILUOUT,*) ' Please choose either:'
  WRITE(ILUOUT,*) ' LGREENROOF=.TRUE. or set GREENROOF fraction to zero (XUNIF_GREENROOF=0.) in namelist PGD_TEB_PAR'
  WRITE(ILUOUT,*) '---------------------------------------------'
  CALL ABOR1_SFX('PGD_TEB_PAR: GREENROOF flag and GREENROOF fraction not coherent')
END IF
!
!-------------------------------------------------------------------------------
!
!* gardens
!
IF (OGARDEN) THEN
   ALLOCATE(DTT%XPAR_GARDEN      (KDIM))
  CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','GARDEN     ','TWN',CFNAM_GARDEN    ,CFTYP_GARDEN    ,XUNIF_GARDEN    ,&
        DTT%XPAR_GARDEN, DTT%LDATA_GARDEN    )
  IF (.NOT.DTT%LDATA_GARDEN) DEALLOCATE(DTT%XPAR_GARDEN)
ELSE IF ( (XUNIF_GARDEN/=0. .AND. XUNIF_GARDEN/=XUNDEF) .OR. LEN_TRIM(CFNAM_GARDEN)/=0) THEN
  WRITE(ILUOUT,*) '---------------------------------------------'
  WRITE(ILUOUT,*) ' You chose not to include gardens in urban areas : LGARDEN=.FALSE.     '
  WRITE(ILUOUT,*) ' But            '
  IF (XUNIF_GARDEN/=0. .AND. XUNIF_GARDEN/=XUNDEF) THEN
    WRITE(ILUOUT,*) ' You also chose a garden fraction that is not zero : XUNIF_GARDEN=',XUNIF_GARDEN
  ELSE
    WRITE(ILUOUT,*) ' You also chose a garden fraction that is not zero : CFNAM_GARDEN=',CFNAM_GARDEN
  END IF
  WRITE(ILUOUT,*) '- - - - - - - - - - - - - - - - - - - - - - -'
  WRITE(ILUOUT,*) ' Please choose either:'
  WRITE(ILUOUT,*) ' LGARDEN=.TRUE. or set GARDEN fraction to zero (XUNIF_GARDEN=0.) in namelist PGD_TEB_PAR'
  WRITE(ILUOUT,*) '- - - - - - - - - - - - - - - - - - - - - - -'
  WRITE(ILUOUT,*) ' Beware that in this case, it may be necessary to change the'
  WRITE(ILUOUT,*) ' road fraction if you want to keep the same canyon aspect ratio'
  WRITE(ILUOUT,*) '---------------------------------------------'
  CALL ABOR1_SFX('PGD_TEB_PAR: GARDEN flag and GARDEN fraction not coherent')
END IF
!
!
!-------------------------------------------------------------------------------
IF (LHOOK)   CALL DR_HOOK('PGD_TEB_PAR',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
CONTAINS
SUBROUTINE COHERENCE_THERMAL_DATA(HTYPE,ODATA_HC,ODATA_TC,ODATA_D)
 CHARACTER(LEN=4), INTENT(IN) :: HTYPE
LOGICAL,          INTENT(IN) :: ODATA_HC
LOGICAL,          INTENT(IN) :: ODATA_TC
LOGICAL,          INTENT(IN) :: ODATA_D
!
IF (ODATA_HC .OR. ODATA_TC .OR. ODATA_D) THEN
  IF (.NOT. (ODATA_HC .AND. ODATA_TC .AND. ODATA_D)) THEN
    WRITE(ILUOUT,*) '*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*'
    WRITE(ILUOUT,*) 'When specifying data for thermal ',TRIM(HTYPE),' characteristics,'
    WRITE(ILUOUT,*) 'All three parameters MUST be defined:'
    WRITE(ILUOUT,*) 'Heat capacity, Thermal conductivity and depths of layers'
    WRITE(ILUOUT,*) ' '
    WRITE(ILUOUT,*) 'In your case :'
    IF (ODATA_HC) THEN
      WRITE(ILUOUT,*) 'Heat capacity is defined'
    ELSE
      WRITE(ILUOUT,*) 'Heat capacity is NOT defined'
    END IF
    IF (ODATA_TC) THEN
      WRITE(ILUOUT,*) 'Thermal conductivity is defined'
    ELSE
      WRITE(ILUOUT,*) 'Thermal conductivity is NOT defined'
    END IF
    IF (ODATA_D) THEN
      WRITE(ILUOUT,*) 'Depths of layers are defined'
    ELSE
      WRITE(ILUOUT,*) 'Depths of layers are NOT defined'
    END IF
    WRITE(ILUOUT,*) '*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*'
    CALL ABOR1_SFX('Heat capacity, Thermal conductivity and depths of layers MUST all be defined for '//HTYPE)
  END IF
END IF
END SUBROUTINE COHERENCE_THERMAL_DATA
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_TEB_PAR
