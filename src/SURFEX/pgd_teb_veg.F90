!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_TEB_VEG (DTCO, UG, U, USS, GDO, GDK, DTGD, GDIR, &
                              GRO, GRS, GRK, DTGR, TOP, KDIM, HPROGRAM)
!     ##############################################################
!
!!**** *PGD_TEB_VEG* monitor for averaging and interpolations of physiographic fields
!!                   for natural covers of TEB
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
!!    Original    03/2010
!!
!!    J.Escobar   11/2013   Add USE MODI_PGD_TEB_GREENROOF
!!    V. Masson   04/2014   Adds Irrigation
!!    P. Samuelsson 02/2014 Introduced dummy variable in call to READ_NAM_PGD_ISBA for MEB
!!    B. Decharme     08/16 : soil grdi optimization key
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_S_t, ISBA_K_t
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_TEB_IRRIG_n, ONLY : TEB_IRRIG_t
!
!
USE MODD_PGD_GRID,          ONLY : NL
USE MODD_DATA_COVER_PAR,    ONLY : NVEGTYPE
!
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
USE MODD_ISBA_PAR,       ONLY : NOPTIMLAYER, XOPTIMGRID
!
USE MODI_GET_LUOUT
USE MODI_READ_NAM_PGD_ISBA
USE MODI_PGD_FIELD
USE MODI_TEST_NAM_VAR_SURF
!
USE MODI_PGD_TEB_GREENROOF
USE MODI_PGD_TEB_GARDEN_PAR
USE MODI_PGD_TEB_IRRIG
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
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: GDO
TYPE(ISBA_K_t), INTENT(INOUT) :: GDK
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTGD
TYPE(TEB_IRRIG_t), INTENT(INOUT) :: GDIR
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: GRO
TYPE(ISBA_S_t), INTENT(INOUT) :: GRS
TYPE(ISBA_K_t), INTENT(INOUT) :: GRK
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTGR
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
!
INTEGER, INTENT(IN) :: KDIM
!
 CHARACTER(LEN=6), INTENT(IN)  :: HPROGRAM   ! Type of program
!                                           ! F if all parameters must be specified
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                  :: ILUOUT    ! output listing logical unit
INTEGER                  :: JLAYER    ! loop counter
!
!*    0.3    Declaration of namelists
!            ------------------------
!
INTEGER                  :: IPATCH           ! number of patches
INTEGER                  :: IGROUND_LAYER    ! number of soil layers
 CHARACTER(LEN=3)         :: YISBA            ! ISBA option
 CHARACTER(LEN=4)         :: YPEDOTF          ! Pedo-transfert function for DIF
 CHARACTER(LEN=3)         :: YPHOTO           ! photosynthesis option
LOGICAL                  :: GTR_ML           ! new radiative transfert
 CHARACTER(LEN=4)         :: YALBEDO
REAL                     :: ZRM_PATCH        ! threshold to remove little fractions of patches
 CHARACTER(LEN=28)        :: YSAND            ! file name for sand fraction
 CHARACTER(LEN=28)        :: YCLAY            ! file name for clay fraction
 CHARACTER(LEN=28)        :: YCTI             ! file name for topographic index
 CHARACTER(LEN=28)        :: YRUNOFFB         ! file name for runoffb parameter
 CHARACTER(LEN=28)        :: YWDRAIN          ! file name for wdrain parameter
 CHARACTER(LEN=6)         :: YSANDFILETYPE    ! sand data file type
 CHARACTER(LEN=6)         :: YCLAYFILETYPE    ! clay data file type
 CHARACTER(LEN=6)         :: YCTIFILETYPE     ! topographic index data file type
 CHARACTER(LEN=6)         :: YRUNOFFBFILETYPE ! subgrid runoff data file type
 CHARACTER(LEN=6)         :: YWDRAINFILETYPE  ! subgrid drainage data file type
REAL                     :: XUNIF_SAND       ! uniform value of sand fraction
REAL                     :: XUNIF_CLAY       ! uniform value of clay fraction
REAL                     :: XUNIF_RUNOFFB    ! uniform value of subgrid runoff coefficient
REAL                     :: XUNIF_WDRAIN     ! uniform subgrid drainage parameter
LOGICAL                  :: LIMP_SAND        ! Imposed maps of Sand
LOGICAL                  :: LIMP_CLAY        ! Imposed maps of Clay
LOGICAL                  :: LIMP_CTI         ! Imposed maps of topographic index statistics
REAL, DIMENSION(150)     :: ZSOILGRID        ! Soil layer thickness for DIF
!
! Not used in TEB garden
!
 CHARACTER(LEN=28)        :: YSOC_TOP      ! file name for organic carbon
 CHARACTER(LEN=28)        :: YSOC_SUB      ! file name for organic carbon
 CHARACTER(LEN=28)        :: YPERM         ! file name for permafrost distribution
 CHARACTER(LEN=6)         :: YSOCFILETYPE  ! organic carbon data file type
 CHARACTER(LEN=6)         :: YPERMFILETYPE ! permafrost distribution data file type
REAL                     :: XUNIF_SOC_TOP ! uniform value of organic carbon top soil (kg/m2)
REAL                     :: XUNIF_SOC_SUB ! uniform value of organic carbon sub soil (kg/m2)
REAL                     :: XUNIF_PERM    ! uniform permafrost distribution
LOGICAL                  :: LIMP_SOC      ! Imposed maps of organic carbon
LOGICAL                  :: LIMP_PERM     ! Imposed maps of permafrost distribution
LOGICAL                  :: GMEB          ! Multi-energy balance (MEB)
 CHARACTER(LEN=28)        :: YPH           ! file name for pH
 CHARACTER(LEN=28)        :: YFERT         ! file name for fertilisation rate
 CHARACTER(LEN=6)         :: YPHFILETYPE   ! pH data file type
 CHARACTER(LEN=6)         :: YFERTFILETYPE ! fertilisation data file type
REAL                     :: XUNIF_PH      ! uniform value of pH
REAL                     :: XUNIF_FERT    ! uniform value of fertilisation rate
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PGD_TEB_VEG',0,ZHOOK_HANDLE)
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------R-----------------------------------------
!
!*    1.      Reading of namelist NAM_ISBA for general options of vegetation
!             --------------------------------------------------------------
!
CALL READ_NAM_PGD_ISBA(HPROGRAM, IPATCH, IGROUND_LAYER,                         &
                       YISBA, YPEDOTF, YPHOTO,  GTR_ML, YALBEDO, ZRM_PATCH,     &
                       YCLAY, YCLAYFILETYPE, XUNIF_CLAY, LIMP_CLAY,             &
                       YSAND, YSANDFILETYPE, XUNIF_SAND, LIMP_SAND,             &
                       YSOC_TOP, YSOC_SUB, YSOCFILETYPE, XUNIF_SOC_TOP,         &
                       XUNIF_SOC_SUB, LIMP_SOC, YCTI, YCTIFILETYPE, LIMP_CTI,   &
                       YPERM, YPERMFILETYPE, XUNIF_PERM, LIMP_PERM, GMEB,       & 
                       YRUNOFFB, YRUNOFFBFILETYPE, XUNIF_RUNOFFB,               &
                       YWDRAIN,  YWDRAINFILETYPE , XUNIF_WDRAIN, ZSOILGRID,     &
                       YPH, YPHFILETYPE, XUNIF_PH, YFERT, YFERTFILETYPE,        &
                       XUNIF_FERT                                               )  
!
GDO%NPATCH = 1
GDO%NGROUND_LAYER = IGROUND_LAYER
GDO%CISBA         = YISBA
GDO%CPEDOTF       = YPEDOTF
GDO%CPHOTO        = YPHOTO
GDO%LTR_ML        = GTR_ML
GDO%CALBEDO       = YALBEDO
!
!-------------------------------------------------------------------------------
!
!*    2.      Coherence of options
!             --------------------
!
  CALL TEST_NAM_VAR_SURF(ILUOUT,'CISBA',GDO%CISBA,'2-L','3-L','DIF')
  CALL TEST_NAM_VAR_SURF(ILUOUT,'CPEDOTF',GDO%CPEDOTF,'CH78','CO84')
  CALL TEST_NAM_VAR_SURF(ILUOUT,'CPHOTO',GDO%CPHOTO,'NON','AST','NIT','NCB')
  !
  IF (GDO%CPHOTO=='NCB') THEN
    GDO%CPHOTO = 'NIT'
    WRITE(ILUOUT,*) '****************************************************************'
    WRITE(ILUOUT,*) '* FOR GARDENS, AGS OPTION HAS BEEN CHANGED FROM "NCB" TO "NIT" *'
    WRITE(ILUOUT,*) '****************************************************************'
  END IF
!
  SELECT CASE (GDO%CISBA)
    CASE ('2-L')
      GDO%NGROUND_LAYER = 2
      GDO%CPEDOTF       ='CH78'       
      WRITE(ILUOUT,*) '*****************************************'
      WRITE(ILUOUT,*) '* With option CISBA = ',GDO%CISBA,'         *'
      WRITE(ILUOUT,*) '* the number of soil layers is set to 2 *'
      WRITE(ILUOUT,*) '* theta(psi) function = Brook and Corey *'
      WRITE(ILUOUT,*) '* Pedo transfert function = CH78        *'          
      WRITE(ILUOUT,*) '*****************************************'
    CASE ('3-L')
      GDO%NGROUND_LAYER = 3
      GDO%CPEDOTF       ='CH78'         
      WRITE(ILUOUT,*) '*****************************************'
      WRITE(ILUOUT,*) '* With option CISBA = ',GDO%CISBA,'         *'
      WRITE(ILUOUT,*) '* the number of soil layers is set to 3 *'
      WRITE(ILUOUT,*) '* theta(psi) function = Brook and Corey *'
      WRITE(ILUOUT,*) '* Pedo transfert function = CH78        *'        
      WRITE(ILUOUT,*) '*****************************************'
    CASE ('DIF')
      IF(GDO%NGROUND_LAYER==NUNDEF)THEN
        IF(TOP%LECOCLIMAP)THEN
          GDO%NGROUND_LAYER=NOPTIMLAYER
        ELSE
          WRITE(ILUOUT,*) '****************************************'
          WRITE(ILUOUT,*) '* Number of ground layer not specified *'
          WRITE(ILUOUT,*) '****************************************'
          CALL ABOR1_SFX('PGD_TEB_GARDEN: NGROUND_LAYER MUST BE DONE IN NAM_ISBA')
        ENDIF
      ENDIF
! 
      ALLOCATE(GDO%XSOILGRID(GDO%NGROUND_LAYER))
      GDO%XSOILGRID(:)=XUNDEF
      GDO%XSOILGRID(:)=ZSOILGRID(1:GDO%NGROUND_LAYER) 
      IF(ALL(ZSOILGRID(:)==XUNDEF))THEN
        IF(TOP%LECOCLIMAP) &
                GDO%XSOILGRID(1:GDO%NGROUND_LAYER)=XOPTIMGRID(1:GDO%NGROUND_LAYER)
      ELSEIF(COUNT(GDO%XSOILGRID/=XUNDEF)/=GDO%NGROUND_LAYER)THEN
        WRITE(ILUOUT,*) '********************************************************'
        WRITE(ILUOUT,*) '* Soil grid reference values /= number of ground layer *'
        WRITE(ILUOUT,*) '********************************************************'
        CALL ABOR1_SFX('PGD_TEB_GARDEN: XSOILGRID must be coherent with NGROUND_LAYER in NAM_ISBA')            
      ENDIF
!
      WRITE(ILUOUT,*) '*****************************************'
      WRITE(ILUOUT,*) '* Option CISBA            = ',GDO%CISBA
      WRITE(ILUOUT,*) '* Pedo transfert function = ',GDO%CPEDOTF    
      WRITE(ILUOUT,*) '* Number of soil layers   = ',GDO%NGROUND_LAYER
      IF(TOP%LECOCLIMAP)THEN
        WRITE(ILUOUT,*) '* Soil layers grid (m)    = ',GDO%XSOILGRID(1:GDO%NGROUND_LAYER)
      ENDIF
      WRITE(ILUOUT,*) '*****************************************' 

  END SELECT
!
  SELECT CASE (GDO%CPHOTO)
    CASE ('AST')
      GDO%NNBIOMASS = 1
    CASE ('NIT')
      GDO%NNBIOMASS = 3
  END SELECT
  WRITE(ILUOUT,*) '*****************************************'
  WRITE(ILUOUT,*) '* With option CPHOTO = ',GDO%CPHOTO,'               *'
  WRITE(ILUOUT,*) '* the number of biomass pools is set to ', GDO%NNBIOMASS
  WRITE(ILUOUT,*) '*****************************************'
!
!-------------------------------------------------------------------------------
!
!*    3.      Sand fraction
!             -------------
!
ALLOCATE(GDK%XSAND(KDIM,GDO%NGROUND_LAYER))
!
IF(LIMP_SAND)THEN
!
  CALL ABOR1_SFX('PGD_TEB_VEG: LIMP_SAND IS NOT CONSISTENT WITH TEB_GARDEN')
!
ELSE
!
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'sand fraction','TWN',YSAND,YSANDFILETYPE,XUNIF_SAND,GDK%XSAND(:,1))
ENDIF
!
DO JLAYER=1,GDO%NGROUND_LAYER
  GDK%XSAND(:,JLAYER) = GDK%XSAND(:,1)
END DO
!-------------------------------------------------------------------------------
!
!*    4.      Clay fraction
!             -------------
!
ALLOCATE(GDK%XCLAY(KDIM,GDO%NGROUND_LAYER))
!
IF(LIMP_CLAY)THEN
!
  CALL ABOR1_SFX('PGD_TEB_VEG: LIMP_SAND IS NOT CONSISTENT WITH TEB_GARDEN')
!
ELSE
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'clay fraction','TWN',YCLAY,YCLAYFILETYPE,XUNIF_CLAY,GDK%XCLAY(:,1))
ENDIF
!
DO JLAYER=1,GDO%NGROUND_LAYER
  GDK%XCLAY(:,JLAYER) = GDK%XCLAY(:,1)
END DO
!-------------------------------------------------------------------------------
!
!*    5.      Subgrid runoff 
!             --------------
!
ALLOCATE(GDK%XRUNOFFB(KDIM))
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'subgrid runoff','TWN',YRUNOFFB,YRUNOFFBFILETYPE,XUNIF_RUNOFFB,GDK%XRUNOFFB(:))
!
!-------------------------------------------------------------------------------
!
!*    6.      Drainage coefficient
!             --------------------
!
ALLOCATE(GDK%XWDRAIN(KDIM))
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'subgrid drainage','TWN',YWDRAIN,YWDRAINFILETYPE,XUNIF_WDRAIN,GDK%XWDRAIN(:))
!
!-------------------------------------------------------------------------------
!
!*    7.      Interpolation of GARDEN physiographic fields
!             --------------------------------------------
!
DTGD%NTIME = 12
 CALL PGD_TEB_GARDEN_PAR(DTCO, UG, U, USS, KDIM, GDO, DTGD, HPROGRAM)
!
!-------------------------------------------------------------------------------
!
!*    8.      Case of greenroofs
!             ------------------
!
IF (TOP%LGREENROOF) CALL PGD_TEB_GREENROOF(DTCO, UG, U, USS, GRO, GRS, GRK, DTGR, KDIM, HPROGRAM)
!
!-------------------------------------------------------------------------------
!
!*    9.      Irrigation of gardens and greenroofs
!             ------------------------------------
!
CALL PGD_TEB_IRRIG(DTCO, UG, U, USS, KDIM, GDIR, HPROGRAM)
!
!-------------------------------------------------------------------------------
!
!*    9.      Case of urban hydrology
!             -----------------------
!
IF (TOP%LHYDRO) print*," CALL PGD_TEB_URBHYDRO(HPROGRAM,LECOCLIMAP)"
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PGD_TEB_GARDEN',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
!
END SUBROUTINE PGD_TEB_VEG
