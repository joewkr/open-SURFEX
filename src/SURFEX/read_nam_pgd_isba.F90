!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_NAM_PGD_ISBA(HPROGRAM, KPATCH, KGROUND_LAYER,                         &
                                   HISBA, HPEDOTF, HPHOTO, OTR_ML, HALBEDO, PRM_PATCH,      &
                                   HCLAY, HCLAYFILETYPE, PUNIF_CLAY, OIMP_CLAY,             &
                                   HSAND, HSANDFILETYPE, PUNIF_SAND, OIMP_SAND,             &
                                   HSOC_TOP, HSOC_SUB, HSOCFILETYPE, PUNIF_SOC_TOP,         &
                                   PUNIF_SOC_SUB, OIMP_SOC, HCTI, HCTIFILETYPE, OIMP_CTI,   &
                                   HPERM, HPERMFILETYPE, PUNIF_PERM, OIMP_PERM, OMEB,       &          
                                   HRUNOFFB, HRUNOFFBFILETYPE, PUNIF_RUNOFFB,               &
                                   HWDRAIN,  HWDRAINFILETYPE , PUNIF_WDRAIN, PSOILGRID,     &
                                   HPH, HPHFILETYPE, PUNIF_PH, HFERT, HFERTFILETYPE,        &
                                   PUNIF_FERT      )  
!     ##############################################################
!
!!**** *READ_NAM_PGD_ISBA* reads namelist for ISBA
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
!OTR_ML,                        !
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
!!    Original    01/2005
!!       2008 B. Decharme : uniform value of subgrid drainage coefficient
!!    12/2008 E. Martin   : files of data for subgrid drainage 
!!                          and subgridrunoff
!!    06/2009 B. Decharme : files of data for topographic index
!!    07/2012 B. Decharme : files of data for permafrost area and for SOC top and sub soil
!!    10/2014 P. Samuelsson: MEB
!!    10/2016 B. Decharme : bug surface/groundwater coupling 
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR, ONLY : XUNDEF, NUNDEF
!
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
!
USE MODE_POS_SURF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=6),    INTENT(IN)    :: HPROGRAM      ! Type of program
INTEGER,             INTENT(OUT)   :: KPATCH        ! number of patches
INTEGER,             INTENT(OUT)   :: KGROUND_LAYER ! number of soil layers
 CHARACTER(LEN=3),    INTENT(OUT)   :: HISBA         ! ISBA option
 CHARACTER(LEN=4),    INTENT(OUT)   :: HPEDOTF       ! Pedo-transfert function for DIF
 CHARACTER(LEN=3),    INTENT(OUT)   :: HPHOTO        ! photosynthesis option
LOGICAL,             INTENT(OUT)   :: OTR_ML        ! new radiative transfert
 CHARACTER(LEN=4),   INTENT(OUT)    :: HALBEDO
REAL,                INTENT(OUT)   :: PRM_PATCH     ! threshold to remove little fractions of patches
 CHARACTER(LEN=28),   INTENT(OUT)   :: HSAND         ! file name for sand fraction
 CHARACTER(LEN=28),   INTENT(OUT)   :: HCLAY         ! file name for clay fraction
 CHARACTER(LEN=28),   INTENT(OUT)   :: HCTI          ! file name for topographic index
 CHARACTER(LEN=28),   INTENT(OUT)   :: HPERM         ! file name for permafrost distribution
 CHARACTER(LEN=28),   INTENT(OUT)   :: HRUNOFFB      ! file name for runoffb parameter
 CHARACTER(LEN=28),   INTENT(OUT)   :: HWDRAIN       ! file name for wdrain parameter
 CHARACTER(LEN=6),    INTENT(OUT)   :: HSANDFILETYPE ! sand data file type
 CHARACTER(LEN=6),    INTENT(OUT)   :: HCLAYFILETYPE ! clay data file type
 CHARACTER(LEN=6),    INTENT(OUT)   :: HCTIFILETYPE  ! topographic index data file type
 CHARACTER(LEN=6),    INTENT(OUT)   :: HPERMFILETYPE    ! permafrost distribution data file type
 CHARACTER(LEN=6),    INTENT(OUT)   :: HRUNOFFBFILETYPE ! subgrid runoff data file type
 CHARACTER(LEN=6),    INTENT(OUT)   :: HWDRAINFILETYPE  ! subgrid drainage data file type
REAL,                INTENT(OUT)   :: PUNIF_SAND    ! uniform value of sand fraction
REAL,                INTENT(OUT)   :: PUNIF_CLAY    ! uniform value of clay fraction
REAL,                INTENT(OUT)   :: PUNIF_RUNOFFB ! uniform value of subgrid runoff coefficient
REAL,                INTENT(OUT)   :: PUNIF_WDRAIN  ! uniform value of subgrid drainage coefficient
REAL,                INTENT(OUT)   :: PUNIF_PERM    ! uniform value of permafrost distribution
LOGICAL,             INTENT(OUT)   :: OIMP_SAND     ! Imposed values for Sand
LOGICAL,             INTENT(OUT)   :: OIMP_CLAY     ! Imposed values for Clay
LOGICAL,             INTENT(OUT)   :: OIMP_CTI      ! Imposed values for topographic index statistics
LOGICAL,             INTENT(OUT)   :: OMEB          ! MEB
LOGICAL,             INTENT(OUT)   :: OIMP_PERM     ! Imposed maps of permafrost distribution
 CHARACTER(LEN=28),   INTENT(OUT)   :: HSOC_TOP      ! file name for organic carbon
 CHARACTER(LEN=28),   INTENT(OUT)   :: HSOC_SUB      ! file name for organic carbon
 CHARACTER(LEN=6),    INTENT(OUT)   :: HSOCFILETYPE  ! organic carbon data file type
REAL,                INTENT(OUT)   :: PUNIF_SOC_TOP ! uniform value of organic carbon top soil (kg/m2)
REAL,                INTENT(OUT)   :: PUNIF_SOC_SUB ! uniform value of organic carbon sub soil (kg/m2)
LOGICAL,             INTENT(OUT)   :: OIMP_SOC      ! Imposed maps of organic carbon
REAL, DIMENSION(:),  INTENT(OUT)   :: PSOILGRID     ! Soil layer thickness for DIF
 CHARACTER(LEN=28),   INTENT(OUT)   :: HPH           ! file name for pH
 CHARACTER(LEN=28),   INTENT(OUT)   :: HFERT         ! file name for fertilisation rate
 CHARACTER(LEN=6),    INTENT(OUT)   :: HPHFILETYPE   ! pH data file type
 CHARACTER(LEN=6),    INTENT(OUT)   :: HFERTFILETYPE ! fertilisation data file type
REAL,                INTENT(OUT)   :: PUNIF_PH      ! uniform value of pH
REAL,                INTENT(OUT)   :: PUNIF_FERT    ! uniform value of fertilisation rate
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                           :: ILUOUT    ! output listing logical unit
INTEGER                           :: ILUNAM    ! namelist file logical unit
LOGICAL                           :: GFOUND    ! flag when namelist is present
!
!*    0.3    Declaration of namelists
!            ------------------------
!
INTEGER                  :: NPATCH           ! number of patches
INTEGER                  :: NGROUND_LAYER    ! number of soil layers
 CHARACTER(LEN=3)         :: CISBA            ! ISBA option
 CHARACTER(LEN=4)         :: CPEDO_FUNCTION   ! Pedo-transfert function for DIF
 CHARACTER(LEN=3)         :: CPHOTO           ! photosynthesis option
LOGICAL                  :: LTR_ML           ! new radiative transfert
 CHARACTER(LEN=4)         :: CALBEDO
REAL                     :: XRM_PATCH        ! threshold to remove little fractions of patches
 CHARACTER(LEN=28)        :: YSAND            ! file name for sand fraction
 CHARACTER(LEN=28)        :: YCLAY            ! file name for clay fraction
 CHARACTER(LEN=28)        :: YCTI             ! file name for topographic index
 CHARACTER(LEN=28)        :: YPERM            ! file name for permafrost distribution
 CHARACTER(LEN=28)        :: YRUNOFFB         ! file name for runoffb parameter
 CHARACTER(LEN=28)        :: YWDRAIN          ! file name for wdrain parameter
 CHARACTER(LEN=28)        :: YPH              ! file name for pH
 CHARACTER(LEN=28)        :: YFERT            ! file name for fertilisation rate
 CHARACTER(LEN=6)         :: YSANDFILETYPE    ! sand data file type
 CHARACTER(LEN=6)         :: YCLAYFILETYPE    ! clay data file type
 CHARACTER(LEN=6)         :: YCTIFILETYPE     ! topographic index data file type
 CHARACTER(LEN=6)         :: YPERMFILETYPE    ! permafrost distribution data file type
 CHARACTER(LEN=6)         :: YRUNOFFBFILETYPE ! subgrid runoff data file type
 CHARACTER(LEN=6)         :: YWDRAINFILETYPE  ! subgrid drainage data file type
 CHARACTER(LEN=6)         :: YPHFILETYPE      ! pH data file type
 CHARACTER(LEN=6)         :: YFERTFILETYPE    ! fertilisation data file type
LOGICAL                  :: LIMP_SAND        ! Imposed maps of Sand from another PGD file
LOGICAL                  :: LIMP_CLAY        ! Imposed maps of Clay from another PGD file
LOGICAL                  :: LIMP_CTI         ! Imposed values for topographic index statistics from another PGD file
LOGICAL                  :: LMEB             ! MEB
LOGICAL                  :: LIMP_PERM        ! Imposed maps of permafrost distribution
REAL                     :: XUNIF_SAND    ! uniform value of sand fraction
REAL                     :: XUNIF_CLAY    ! uniform value of clay fraction
REAL                     :: XUNIF_RUNOFFB ! uniform value of subgrid runoff coefficient
REAL                     :: XUNIF_WDRAIN  ! uniform value of subgrid drainage coefficient
REAL                     :: XUNIF_PERM    ! uniform value of permafrost distribution
REAL                     :: XUNIF_PH      ! uniform value of pH
REAL                     :: XUNIF_FERT    ! uniform value of fertilisation rate
!
REAL, DIMENSION(150)     :: XSOILGRID     ! Soil layer thickness for DIF
!
 CHARACTER(LEN=28)        :: YSOC_TOP      ! file name for organic carbon expressed in kg/m2
 CHARACTER(LEN=28)        :: YSOC_SUB      ! file name for organic carbon expressed in kg/m2
 CHARACTER(LEN=6)         :: YSOCFILETYPE  ! organic carbon data file type
REAL                     :: XUNIF_SOC_TOP ! uniform value of organic carbon (kg/m2)
REAL                     :: XUNIF_SOC_SUB ! uniform value of organic carbon (kg/m2)
LOGICAL                  :: LIMP_SOC      ! Imposed maps of organic carbon
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
NAMELIST/NAM_ISBA/ NPATCH, NGROUND_LAYER, CISBA, CPEDO_FUNCTION, CPHOTO,   &
                   LTR_ML, CALBEDO, XRM_PATCH, YCLAY, YCLAYFILETYPE, XUNIF_CLAY,  &
                   LIMP_CLAY, YSAND, YSANDFILETYPE, XUNIF_SAND, LIMP_SAND, &
                   YSOC_TOP, YSOC_SUB, YSOCFILETYPE, XUNIF_SOC_TOP,        &
                   XUNIF_SOC_SUB, LIMP_SOC, YCTI, YCTIFILETYPE, LIMP_CTI,  &
                   YPERM, YPERMFILETYPE, XUNIF_PERM, LIMP_PERM, LMEB,      &                   
                   YRUNOFFB, YRUNOFFBFILETYPE, XUNIF_RUNOFFB,              &
                   YWDRAIN,  YWDRAINFILETYPE,  XUNIF_WDRAIN, XSOILGRID,    &
                   YPH, YPHFILETYPE, XUNIF_PH, YFERT, YFERTFILETYPE,       &
                   XUNIF_FERT   
!
!-------------------------------------------------------------------------------
!
!*    1.      Initializations of defaults
!             ---------------------------
!
!#####################
IF (LHOOK) CALL DR_HOOK('READ_NAM_PGD_ISBA',0,ZHOOK_HANDLE)
NPATCH         = 1
NGROUND_LAYER  = NUNDEF
CISBA          = '3-L'
CPEDO_FUNCTION = 'CH78'
CPHOTO         = 'NON'
LTR_ML         = .FALSE.
CALBEDO        = 'DRY '
XSOILGRID(:)   = XUNDEF
XRM_PATCH      = 0.0
!#####################
!
XUNIF_CLAY       = 0.33
XUNIF_SAND       = 0.33
XUNIF_SOC_TOP    = XUNDEF
XUNIF_SOC_SUB    = XUNDEF
XUNIF_RUNOFFB    = 0.5
XUNIF_WDRAIN     = 0.
XUNIF_PERM       = XUNDEF
XUNIF_PH         = XUNDEF
XUNIF_FERT       = XUNDEF
!
YCLAY            = '                          '
YSAND            = '                          '
YSOC_TOP         = '                          '
YSOC_SUB         = '                          '
YCTI             = '                          '
YPERM            = '                          '
YRUNOFFB         = '                          '
YWDRAIN          = '                          '
YPH              = '                          '
YFERT            = '                          '
!
YCLAYFILETYPE    = '      '
YSANDFILETYPE    = '      '
YSOCFILETYPE     = '      '
YCTIFILETYPE     = '      '
YPERMFILETYPE    = '      '
YRUNOFFBFILETYPE = '      '
YWDRAINFILETYPE  = '      ' 
YPHFILETYPE      = '      '
YPHFILETYPE      = '      '
!
LIMP_CLAY        = .FALSE.
LIMP_SAND        = .FALSE.
LIMP_SOC         = .FALSE.
LIMP_CTI         = .FALSE.
LMEB             = .FALSE.
LIMP_PERM        = .FALSE.
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
!*    2.      Reading of namelist
!             -------------------
!
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
 CALL POSNAM(ILUNAM,'NAM_ISBA',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_ISBA)
!
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
!-------------------------------------------------------------------------------
!
KPATCH           = NPATCH           ! number of patches
KGROUND_LAYER    = NGROUND_LAYER    ! number of soil layers
PSOILGRID        = XSOILGRID        ! soil layer tickness for DIF
HISBA            = CISBA            ! ISBA option
HPEDOTF          = CPEDO_FUNCTION   ! Pedo-transfert function for DIF
HPHOTO           = CPHOTO           ! photosynthesis option
OTR_ML           = LTR_ML           ! new radiative transfert
HALBEDO          = CALBEDO 
PRM_PATCH        = XRM_PATCH        ! threshol to remove little fractions of patches
HSAND            = YSAND            ! file name for sand fraction
HCLAY            = YCLAY            ! file name for clay fraction
HSOC_TOP         = YSOC_TOP         ! file name for organic carbon
HSOC_SUB         = YSOC_SUB         ! file name for organic carbon
HCTI             = YCTI             ! file name for topographic index
HPERM            = YPERM            ! file name for permafrost distribution
HRUNOFFB         = YRUNOFFB         ! file name for subgrid runoff
HWDRAIN          = YWDRAIN          ! file name for subgrid drainage
HSANDFILETYPE    = YSANDFILETYPE    ! sand data file type
HCLAYFILETYPE    = YCLAYFILETYPE    ! clay data file type
HSOCFILETYPE     = YSOCFILETYPE     ! organic carbon data file type
HCTIFILETYPE     = YCTIFILETYPE     ! topographic index data file type
HPERMFILETYPE    = YPERMFILETYPE    ! permafrost distribution data file type
HRUNOFFBFILETYPE = YRUNOFFBFILETYPE ! subgrid runoff data file type
HWDRAINFILETYPE  = YWDRAINFILETYPE  ! subgrid drainage data file type
PUNIF_SAND       = XUNIF_SAND       ! uniform value of sand fraction
PUNIF_CLAY       = XUNIF_CLAY       ! uniform value of clay fraction
PUNIF_SOC_TOP    = XUNIF_SOC_TOP    ! uniform value of organic carbon top soil
PUNIF_SOC_SUB    = XUNIF_SOC_SUB    ! uniform value of organic carbon sub soil
PUNIF_RUNOFFB    = XUNIF_RUNOFFB    ! uniform value of subgrid runoff coefficient
PUNIF_WDRAIN     = XUNIF_WDRAIN     ! uniform value of subgrid drainage coefficient
PUNIF_PERM       = XUNIF_PERM       ! uniform value of permafrost distribution
OIMP_SAND        = LIMP_SAND        ! Imposed values for SAND
OIMP_CLAY        = LIMP_CLAY        ! Imposed values for CLAY
OIMP_SOC         = LIMP_SOC         ! Imposed values for organic carbon
OIMP_CTI         = LIMP_CTI         ! Imposed values for topographic index statistics
OIMP_PERM        = LIMP_PERM        ! Imposed values for permafrost distribution
OMEB             = LMEB             ! MEB
!
HPH           = YPH           ! file name for pH value
HFERT         = YFERT         ! file name for fertilisation data
HPHFILETYPE   = YPHFILETYPE   ! pH data file type
HFERTFILETYPE = YFERTFILETYPE ! Fertilisation data file type
PUNIF_PH      = XUNIF_PH      ! uniform value of pH
PUNIF_FERT    = XUNIF_FERT    ! uniform value of fertilisation rate
!
IF (LHOOK) CALL DR_HOOK('READ_NAM_PGD_ISBA',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_NAM_PGD_ISBA
