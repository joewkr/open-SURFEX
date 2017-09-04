!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_ISBA (DTCO, DTV, IG, IO, S, K, ISS, UG, U, USS, HPROGRAM)
!     ##############################################################
!
!!**** *PGD_ISBA* monitor for averaging and interpolations of ISBA physiographic fields
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
!!    P. Le Moigne  12/2004 : add type of photosynthesis and correct computation
!!                            of ground layers number in diffusion case
!!    P. Le Moigne  09/2005 : AGS modifs of L. Jarlan
!!    B. Decharme      2008 :  XWDRAIN
!!    E. Martin     12/2008 : files of data for runoffb and wdrain
!!    B. Decharme   06/2009 : files of data for topographic index
!!    A.L. Gibelin  04/2009 : dimension NBIOMASS for ISBA-A-gs
!!    R. Alkama     05/2012 : npatch must be 12 or 19 if CPHOTO/='NON'
!!    B. Decharme   11/2013 : groundwater distribution for water table/surface coupling
!!    P. Samuelsson 02/2012 : MEB
!!    B. Decharme    10/2016  bug surface/groundwater coupling
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
#ifdef SFX_OL
USE MODN_IO_OFFLINE,     ONLY : LWR_VEGTYPE
#endif
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DATA_COVER,     ONLY : XDATA_VEGTYPE
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_S_t, ISBA_K_t
USE MODD_SSO_n, ONLY : SSO_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
USE MODD_PGD_GRID,       ONLY : NL
USE MODD_PGDWORK,        ONLY : CATYPE
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE, JPCOVER
!
USE MODD_ISBA_PAR,       ONLY : NOPTIMLAYER, XOPTIMGRID
!
USE MODI_AV_PGD
!
USE MODI_GET_LUOUT
USE MODI_READ_NAM_PGD_ISBA
USE MODI_READ_NAM_PGD_ISBA_MEB
USE MODI_PGD_FIELD
USE MODI_TEST_NAM_VAR_SURF
!
USE MODI_GET_AOS_n
USE MODI_GET_SSO_n
USE MODI_GET_SURF_SIZE_n
USE MODI_PACK_PGD_ISBA
USE MODI_PACK_PGD
USE MODI_WRITE_COVER_TEX_ISBA
USE MODI_WRITE_COVER_TEX_ISBA_PAR
USE MODI_PGD_TOPO_INDEX
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_PGD_ISBA_PAR
USE MODI_PGD_TOPD
USE MODE_POS_SURF
!
USE MODI_READ_SURF
USE MODI_INIT_IO_SURF_n
USE MODI_END_IO_SURF_n
!
USE MODI_READ_NAMELISTS_ISBA
!
#ifdef SFX_ASC
USE MODD_IO_SURF_ASC, ONLY : CFILEIN
#endif
#ifdef SFX_FA
USE MODD_IO_SURF_FA,  ONLY : CFILEIN_FA
#endif
#ifdef SFX_LFI
USE MODD_IO_SURF_LFI, ONLY : CFILEIN_LFI
#endif
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
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTV
TYPE(GRID_t), INTENT(INOUT) :: IG
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t), INTENT(INOUT) :: S
TYPE(ISBA_K_t), INTENT(INOUT) :: K
TYPE(SSO_t), INTENT(INOUT) :: ISS
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
!
 CHARACTER(LEN=6), INTENT(IN)  :: HPROGRAM   ! program calling surf. schemes
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                           :: ILUOUT    ! output listing logical unit
INTEGER                           :: JLAYER    ! loop counter
INTEGER                           :: ILU       ! number of points
INTEGER                           :: ILUNAM    ! namelist file logical unit
REAL, DIMENSION(NL)               :: ZAOSIP    ! A/S i+ on all surface points
REAL, DIMENSION(NL)               :: ZAOSIM    ! A/S i- on all surface points
REAL, DIMENSION(NL)               :: ZAOSJP    ! A/S j+ on all surface points
REAL, DIMENSION(NL)               :: ZAOSJM    ! A/S j- on all surface points
REAL, DIMENSION(NL)               :: ZHO2IP    ! h/2 i+ on all surface points
REAL, DIMENSION(NL)               :: ZHO2IM    ! h/2 i- on all surface points
REAL, DIMENSION(NL)               :: ZHO2JP    ! h/2 j+ on all surface points
REAL, DIMENSION(NL)               :: ZHO2JM    ! h/2 j- on all surface points
REAL, DIMENSION(NL)               :: ZSSO_SLOPE! subgrid slope on all surface points
INTEGER                           :: IRESP     ! error code
LOGICAL                           :: GMEB      ! Multi-energy balance (MEB)
LOGICAL                           :: GFOUND    ! flag when namelist is present
!
!*    0.3    Declaration of namelists
!            ------------------------
!
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
LOGICAL, DIMENSION(19)   :: GMEB_PATCH
LOGICAL, DIMENSION(19)   :: GMEB_PATCH_REC ! Recommended MEB patch settings
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PGD_ISBA',0,ZHOOK_HANDLE)
!
CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
!*    2.1      Reading of ISBA namelist
!             -------------------------
!
CALL READ_NAM_PGD_ISBA(HPROGRAM, IPATCH, IGROUND_LAYER,                          &
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
!
IO%NPATCH        = IPATCH
IO%NGROUND_LAYER = IGROUND_LAYER
IO%CISBA         = YISBA
IO%CPEDOTF       = YPEDOTF
IO%CPHOTO        = YPHOTO
IO%LTR_ML        = GTR_ML
IO%CALBEDO       = YALBEDO
IO%XRM_PATCH     = MAX(MIN(ZRM_PATCH,1.),0.)
!
!
 CALL READ_NAMELISTS_ISBA(HPROGRAM)
!
!-------------------------------------------------------------------------------
!
!*    2.2      Reading of ISBA MEB namelist
!             -----------------------------
!
IF (IO%NPATCH<1 .OR. IO%NPATCH>NVEGTYPE) THEN
  WRITE(ILUOUT,*) '*****************************************'
  WRITE(ILUOUT,*) '* Number of patch must be between 1 and ', NVEGTYPE
  WRITE(ILUOUT,*) '* You have chosen NPATCH = ', IO%NPATCH
  WRITE(ILUOUT,*) '*****************************************'
  CALL ABOR1_SFX('PGD_ISBA: NPATCH MUST BE BETWEEN 1 AND NVEGTYPE')
END IF
!
ALLOCATE(IO%LMEB_PATCH(IO%NPATCH))
!
IO%LMEB_PATCH(:) = .FALSE.
IO%LFORC_MEASURE = .FALSE.
IO%LMEB_LITTER   = .FALSE.
IO%LMEB_GNDRES   = .FALSE.

IF(GMEB)THEN

  IO%LTR_ML      = .TRUE. ! Always use this SW radiative transfer option with MEB

  CALL READ_NAM_PGD_ISBA_MEB(HPROGRAM,ILUOUT,GMEB_PATCH,IO%LFORC_MEASURE,IO%LMEB_LITTER,IO%LMEB_GNDRES)

  ! Current recommendation is to use MEB for tree patches only.
  ! Here follows a test in which non-tree patches in LMEB_PATCH are set to FALSE.
  ! Thus, if you wish to test MEB for non-tree patches you can set
  ! GMEB_PATCH_REC(:)=.TRUE.
  ! in the following line:

  GMEB_PATCH_REC(:)=.FALSE.

  IF(IO%NPATCH==1 .AND. GMEB_PATCH(1))THEN
    WRITE(ILUOUT,*) '*****************************************'
    WRITE(ILUOUT,*) '* WARNING!'
    WRITE(ILUOUT,*) '* Using MEB for one patch only is not recommended.'
    WRITE(ILUOUT,*) '* LMEB_PATCH(1) has been set to .FALSE.'
    WRITE(ILUOUT,*) '*****************************************'
  ELSEIF(IO%NPATCH>=2 .AND. IO%NPATCH<=6)THEN
    GMEB_PATCH_REC(2)=.TRUE.  ! Only the tree patch (number 2) is allowed to be TRUE
  ELSEIF(IO%NPATCH>=7 .AND. IO%NPATCH<=8)THEN
    GMEB_PATCH_REC(3)=.TRUE.  ! Only the tree patch (number 3) is allowed to be TRUE
  ELSEIF(IO%NPATCH==9)THEN
    GMEB_PATCH_REC(3:4)=(/.TRUE.,.TRUE./)  ! Only the tree patches (numbers 3-4) are allowed to be TRUE
  ELSEIF(IO%NPATCH==10)THEN
    GMEB_PATCH_REC(3:5)=(/.TRUE.,.TRUE.,.TRUE./)  ! Only the tree patches (numbers 3-5) are allowed to be TRUE
  ELSEIF(IO%NPATCH>=11 .AND. IO%NPATCH<=12)THEN
    GMEB_PATCH_REC(4:6)=(/.TRUE.,.TRUE.,.TRUE./)  ! Only the tree patches (numbers 4-6) are allowed to be TRUE
  ELSEIF(IO%NPATCH==19)THEN
    GMEB_PATCH_REC(4:6)=(/.TRUE.,.TRUE.,.TRUE./)  ! The "old" tree patches (numbers 4-6) are allowed to be TRUE
    GMEB_PATCH_REC(13:17)=(/.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE./)  ! The "new" tree patches (numbers 13-17) are allowed to be TRUE
  ENDIF

  IF(COUNT(.NOT.GMEB_PATCH_REC(:) .AND. GMEB_PATCH(:))>0)THEN
    WRITE(ILUOUT,*) '*****************************************'
    WRITE(ILUOUT,*) '* WARNING!'
    WRITE(ILUOUT,*) '* Using MEB for non-tree patches is not yet recommended.'
    WRITE(ILUOUT,*) '* Therefor, LMEB_PATCH for non-tree patches has been set to .FALSE.'
    WRITE(ILUOUT,*) '* The final LMEB_PATCH vector becomes:'
    WRITE(ILUOUT,*) GMEB_PATCH(1:IO%NPATCH).AND.GMEB_PATCH_REC(1:IO%NPATCH)
    WRITE(ILUOUT,*) '*****************************************'
  ENDIF
  GMEB_PATCH(:)=GMEB_PATCH(:).AND.GMEB_PATCH_REC(:)

  IO%LMEB_PATCH(1:IO%NPATCH) = GMEB_PATCH(1:IO%NPATCH)

  IF (IO%LMEB_LITTER) IO%LMEB_GNDRES = .FALSE.

ENDIF
!
!-------------------------------------------------------------------------------
!
!*    3.      Coherence of options
!             --------------------
!
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CISBA',IO%CISBA,'2-L','3-L','DIF')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CPEDOTF',IO%CPEDOTF,'CH78','CO84')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CPHOTO',IO%CPHOTO,'NON','AST','NIT','NCB')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CALBEDO',IO%CALBEDO,'EVOL','DRY ','WET ','MEAN','USER','CM13')
!
SELECT CASE (IO%CISBA)
!
  CASE ('2-L')
!
    IO%NGROUND_LAYER = 2
    IO%CPEDOTF       ='CH78'
    ALLOCATE(IO%XSOILGRID(0))
    WRITE(ILUOUT,*) '*****************************************'
    WRITE(ILUOUT,*) '* With option CISBA = ',IO%CISBA,'         *'
    WRITE(ILUOUT,*) '* the number of soil layers is set to 2 *'
    WRITE(ILUOUT,*) '* Pedo transfert function = CH78        *'
    WRITE(ILUOUT,*) '*****************************************'
!
  CASE ('3-L')
!
    IO%NGROUND_LAYER = 3
    IO%CPEDOTF       ='CH78'
    ALLOCATE(IO%XSOILGRID(0))
    WRITE(ILUOUT,*) '*****************************************'
    WRITE(ILUOUT,*) '* With option CISBA = ',IO%CISBA,'         *'
    WRITE(ILUOUT,*) '* the number of soil layers is set to 3 *'
    WRITE(ILUOUT,*) '* Pedo transfert function = CH78        *'
    WRITE(ILUOUT,*) '*****************************************'
!
  CASE ('DIF')
!
    IF(IO%NGROUND_LAYER==NUNDEF)THEN
      IF(U%LECOCLIMAP)THEN
        IO%NGROUND_LAYER=NOPTIMLAYER
      ELSE
        WRITE(ILUOUT,*) '****************************************'
        WRITE(ILUOUT,*) '* Number of ground layer not specified *'
        WRITE(ILUOUT,*) '****************************************'
        CALL ABOR1_SFX('PGD_ISBA: NGROUND_LAYER MUST BE DONE IN NAM_ISBA')
      ENDIF
    ENDIF
!
    ALLOCATE(IO%XSOILGRID(IO%NGROUND_LAYER))
    IO%XSOILGRID(:)=0.
    IO%XSOILGRID(:)=ZSOILGRID(1:IO%NGROUND_LAYER)
    IF (ALL(ZSOILGRID(:)==XUNDEF)) THEN
      IF(U%LECOCLIMAP) IO%XSOILGRID(1:IO%NGROUND_LAYER)=XOPTIMGRID(1:IO%NGROUND_LAYER)
    ELSEIF (COUNT(IO%XSOILGRID/=XUNDEF)/=IO%NGROUND_LAYER) THEN
      WRITE(ILUOUT,*) '********************************************************'
      WRITE(ILUOUT,*) '* Soil grid reference values /= number of ground layer *'
      WRITE(ILUOUT,*) '********************************************************'
      CALL ABOR1_SFX('PGD_ISBA: XSOILGRID must be coherent with NGROUND_LAYER in NAM_ISBA')
    ELSEIF (IO%XSOILGRID(1).GT.0.01) THEN
      CALL ABOR1_SFX('PGD_ISBA: First layer of XSOILGRID must be lower than 1cm')
    ENDIF
!
    WRITE(ILUOUT,*) '*****************************************'
    WRITE(ILUOUT,*) '* Option CISBA            = ',IO%CISBA
    WRITE(ILUOUT,*) '* Pedo transfert function = ',IO%CPEDOTF
    WRITE(ILUOUT,*) '* Number of soil layers   = ',IO%NGROUND_LAYER
    IF(U%LECOCLIMAP)THEN
      WRITE(ILUOUT,*) '* Soil layers grid (m)    = ',IO%XSOILGRID(1:IO%NGROUND_LAYER)
    ENDIF
    WRITE(ILUOUT,*) '*****************************************'
!
END SELECT
!
SELECT CASE (IO%CPHOTO)
  CASE ('AST')
    IO%NNBIOMASS = 1
  CASE ('NIT')
    IO%NNBIOMASS = 3
  CASE ('NCB')
    IO%NNBIOMASS = 6
END SELECT
WRITE(ILUOUT,*) '*****************************************'
WRITE(ILUOUT,*) '* With option CPHOTO = ',IO%CPHOTO,'               *'
WRITE(ILUOUT,*) '* the number of biomass pools is set to ', IO%NNBIOMASS
WRITE(ILUOUT,*) '*****************************************'
!
IF ( IO%CPHOTO/='NON' .AND. IO%NPATCH/=12 .AND. IO%NPATCH/=19 ) THEN
  WRITE(ILUOUT,*) '*****************************************'
  WRITE(ILUOUT,*) '* With option CPHOTO = ', IO%CPHOTO
  WRITE(ILUOUT,*) '* Number of patch must be equal to 12 or 19'
  WRITE(ILUOUT,*) '* But you have chosen NPATCH = ', IO%NPATCH
  WRITE(ILUOUT,*) '*****************************************'
  CALL ABOR1_SFX('PGD_ISBA: CPHOTO='//IO%CPHOTO//' REQUIRES NPATCH=12 or 19')
END IF
!
IF ( IO%CPHOTO=='NON' .AND. IO%LTR_ML .AND. .NOT. GMEB) THEN
  WRITE(ILUOUT,*) '*****************************************'
  WRITE(ILUOUT,*) '* With option CPHOTO == NON      '
  WRITE(ILUOUT,*) '* And With MEB = F               '
  WRITE(ILUOUT,*) '* New radiative transfert TR_ML  '
  WRITE(ILUOUT,*) '* cant be used '
  WRITE(ILUOUT,*) '*****************************************'
  CALL ABOR1_SFX('PGD_ISBA: WITH CPHOTO= NON LTR_ML MUST BE FALSE')
END IF
!
!-------------------------------------------------------------------------------
!
!*    4.      Number of points and packing of general fields
!             ----------------------------------------------
!
 CALL GET_SURF_SIZE_n(DTCO, U, 'NATURE',ILU)
!
ALLOCATE(S%LCOVER      (JPCOVER))
ALLOCATE(S%XZS         (ILU))
ALLOCATE(IG%XLAT       (ILU))
ALLOCATE(IG%XLON       (ILU))
ALLOCATE(IG%XMESH_SIZE (ILU))
ALLOCATE(ISS%XZ0EFFJPDIR(ILU))
!
 CALL PACK_PGD(DTCO, U, HPROGRAM, 'NATURE',  IG, S%LCOVER, S%XCOVER, S%XZS  )
!
!-------------------------------------------------------------------------------
!
!*    5.      Packing of ISBA specific fields
!             -------------------------------
!
 CALL GET_AOS_n(USS, HPROGRAM,NL,ZAOSIP,ZAOSIM,ZAOSJP,ZAOSJM,ZHO2IP,ZHO2IM,ZHO2JP,ZHO2JM)
 CALL GET_SSO_n(USS, HPROGRAM,NL,ZSSO_SLOPE)
!
 CALL PACK_PGD_ISBA(DTCO, IG%NDIM, ISS, U, HPROGRAM,              &
                     ZAOSIP, ZAOSIM, ZAOSJP, ZAOSJM,              &
                     ZHO2IP, ZHO2IM, ZHO2JP, ZHO2JM,              &
                     ZSSO_SLOPE                                   )
!
!-------------------------------------------------------------------------------
!
!*   15.      ISBA specific fields
!             --------------------
!
IO%LECOCLIMAP = U%LECOCLIMAP
!
 CALL PGD_ISBA_PAR(DTCO, UG, U, USS, DTV, IO, S, IG%NDIM, HPROGRAM)
!
!-------------------------------------------------------------------------------
!
#ifdef SFX_OL
IF (LWR_VEGTYPE) THEN
  ALLOCATE(K%XVEGTYPE(ILU,NVEGTYPE))
  IF (DTV%LDATA_VEGTYPE) THEN
    K%XVEGTYPE(:,:) = DTV%XPAR_VEGTYPE(:,:)
  ELSE
    DO JVEGTYPE=1,NVEGTYPE
      CALL AV_PGD(DTCO,K%XVEGTYPE(:,JVEGTYPE),S%XCOVER,XDATA_VEGTYPE(:,JVEGTYPE),'NAT','ARI',S%LCOVER)
    ENDDO
  ENDIF
ENDIF
#endif
!
DEALLOCATE(S%XCOVER)
!
!-------------------------------------------------------------------------------
!
!*    6.      Topographic index for TOPMODEL
!             ------------------------------
!
 CALL PGD_TOPO_INDEX(DTCO, UG, U, USS, S, IO%LCTI, &
                     HPROGRAM,ILU,YCTI,YCTIFILETYPE,LIMP_CTI)
!
!-------------------------------------------------------------------------------
!
!*    7.      Sand fraction
!             -------------
!
CATYPE='ARI'
!
ALLOCATE(K%XSAND(ILU,IO%NGROUND_LAYER))
!
 CALL GET_FIELD(YSANDFILETYPE,YSAND,"SAND",LIMP_SAND,XUNIF_SAND,K%XSAND(:,1))
!
DO JLAYER=1,IO%NGROUND_LAYER
  K%XSAND(:,JLAYER) = K%XSAND(:,1)
END DO
!-------------------------------------------------------------------------------
!
!*    8.      Clay fraction
!             -------------
!
ALLOCATE(K%XCLAY(ILU,IO%NGROUND_LAYER))
!
 CALL GET_FIELD(YCLAYFILETYPE,YCLAY,"CLAY",LIMP_CLAY,XUNIF_CLAY,K%XCLAY(:,1))
!
DO JLAYER=1,IO%NGROUND_LAYER
  K%XCLAY(:,JLAYER) = K%XCLAY(:,1)
END DO
!
!-------------------------------------------------------------------------------
!
!*    9.      organic carbon profile
!             ----------------------
!
IF(LEN_TRIM(YSOCFILETYPE)/=0.OR.(XUNIF_SOC_TOP/=XUNDEF.AND.XUNIF_SOC_SUB/=XUNDEF))THEN
!
  ALLOCATE(S%XSOC(ILU,IO%NGROUND_LAYER))
!
  IO%LSOCP=.TRUE.
!
  IF((LEN_TRIM(YSOC_TOP)==0.AND.LEN_TRIM(YSOC_SUB)/=0).OR.(LEN_TRIM(YSOC_TOP)/=0.AND.LEN_TRIM(YSOC_SUB)==0))THEN
    WRITE(ILUOUT,*) ' '
    WRITE(ILUOUT,*) '***********************************************************'
    WRITE(ILUOUT,*) '* Error in soil organic carbon preparation                *'
    WRITE(ILUOUT,*) '* If used, sub and top soil input file must be given      *'
    WRITE(ILUOUT,*) '***********************************************************'
    WRITE(ILUOUT,*) ' '
    CALL ABOR1_SFX('PGD_ISBA: TOP AND SUB SOC INPUT FILE REQUIRED')
  ENDIF
!
 CALL GET_FIELD(YSOCFILETYPE,YSOC_TOP,"SOC_TOP",LIMP_SOC,XUNIF_SOC_TOP,S%XSOC(:,1))
!
 CALL GET_FIELD(YSOCFILETYPE,YSOC_SUB,"SOC_SUB",LIMP_SOC,XUNIF_SOC_SUB,S%XSOC(:,2))
!
  DO JLAYER=2,IO%NGROUND_LAYER
    S%XSOC(:,JLAYER) = S%XSOC(:,2)
  END DO
!
ELSE
!
  IO%LSOCP=.FALSE.
  ALLOCATE(S%XSOC(0,0))
!
ENDIF
!
!*    10.     Permafrost distribution
!             -----------------------
!
IF(LEN_TRIM(YPERM)/=0.OR.XUNIF_PERM/=XUNDEF)THEN
!
  ALLOCATE(K%XPERM(ILU))
!
  IO%LPERM=.TRUE.
!
 CALL GET_FIELD(YPERMFILETYPE,YPERM,"PERM",LIMP_PERM,XUNIF_PERM,K%XPERM(:))
!
ELSE
!
  IO%LPERM=.FALSE.
  ALLOCATE(K%XPERM(0))
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!*    12.  pH and fertlisation data
!             --------------------------
!
IF((LEN_TRIM(YPHFILETYPE)/=0.OR.XUNIF_PH/=XUNDEF) .AND. &
   (LEN_TRIM(YFERTFILETYPE)/=0.OR.XUNIF_FERT/=XUNDEF)) THEN
  !
  ALLOCATE(S%XPH(ILU))
  ALLOCATE(S%XFERT(ILU))
  !
  IO%LNOF = .TRUE.
  !
  CALL PGD_FIELD(DTCO, UG, U, USS, &
                  HPROGRAM,'pH value','NAT',YPH,YPHFILETYPE,XUNIF_PH,S%XPH(:))
  CALL PGD_FIELD(DTCO, UG, U, USS, &
                  HPROGRAM,'fertilisation','NAT',YFERT,YFERTFILETYPE,XUNIF_FERT,S%XFERT(:))
  !
ENDIF
!
!-------------------------------------------------------------------------------
!
!*    13.      Subgrid runoff
!             --------------
!
ALLOCATE(K%XRUNOFFB(ILU))
 CALL PGD_FIELD(DTCO, UG, U, USS, HPROGRAM,'subgrid runoff','NAT',YRUNOFFB,YRUNOFFBFILETYPE,&
                        XUNIF_RUNOFFB,K%XRUNOFFB(:))
!
!-------------------------------------------------------------------------------
!
!*    14.     Drainage coefficient
!             --------------------
!
ALLOCATE(K%XWDRAIN(ILU))
 CALL PGD_FIELD(DTCO, UG, U, USS, HPROGRAM,'subgrid drainage','NAT',YWDRAIN,YWDRAINFILETYPE,&
                        XUNIF_WDRAIN,K%XWDRAIN(:))
!
!-------------------------------------------------------------------------------
!
 CALL PGD_TOPD(IO%CISBA, UG%G%CGRID, UG%G%XGRID_PAR, U%NDIM_FULL, USS%XSSO_SLOPE, HPROGRAM)
!
!-------------------------------------------------------------------------------
!
!*   16.     Prints of cover parameters in a tex file
!            ----------------------------------------
!
IF (U%LECOCLIMAP) THEN
  CALL WRITE_COVER_TEX_ISBA    (IO%NPATCH,IO%NGROUND_LAYER,IO%CISBA)
  CALL WRITE_COVER_TEX_ISBA_PAR(DTCO, IO%CALBEDO, IO%LTR_ML, &
                                IO%NPATCH,IO%NGROUND_LAYER,IO%CISBA,IO%CPHOTO,IO%XSOILGRID)
END IF
IF (LHOOK) CALL DR_HOOK('PGD_ISBA',1,ZHOOK_HANDLE)
!
CONTAINS
!
SUBROUTINE GET_FIELD(HFILETYPE,HFILE,HFIELD,OIMP,PUNIF,PFIELD)
!
IMPLICIT NONE
!
 CHARACTER(LEN=*), INTENT(INOUT) :: HFILETYPE
 CHARACTER(LEN=*), INTENT(IN) :: HFILE
 CHARACTER(LEN=*), INTENT(IN) :: HFIELD
LOGICAL, INTENT(IN) :: OIMP
REAL, INTENT(IN) :: PUNIF
REAL, DIMENSION(:), INTENT(OUT) :: PFIELD
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('PGD_ISBA:GET_FIELD',0,ZHOOK_HANDLE)
!
IF(OIMP)THEN
!
  IF(HFILETYPE=='NETCDF')THEN
     CALL ABOR1_SFX('Use another format than netcdf for '//TRIM(HFIELD)//' input file with LIMP ')
  ELSE
#ifdef SFX_ASC
     CFILEIN     = ADJUSTL(ADJUSTR(HFILE)//'.txt')
#endif
#ifdef SFX_FA
     CFILEIN_FA  = ADJUSTL(ADJUSTR(HFILE)//'.fa')
#endif
#ifdef SFX_LFI
     CFILEIN_LFI = ADJUSTL(HFILE)
#endif
CALL INIT_IO_SURF_n(DTCO, U, HFILETYPE,'NATURE','ISBA  ','READ ')
  ENDIF
!
  CALL READ_SURF(HFILETYPE,TRIM(HFIELD),PFIELD,IRESP)
!
  CALL END_IO_SURF_n(HFILETYPE)
!
ELSE
   CALL PGD_FIELD(DTCO, UG, U, USS, &
                  HPROGRAM,HFIELD,'NAT',HFILE,HFILETYPE,PUNIF,PFIELD)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('PGD_ISBA:GET_FIELD',1,ZHOOK_HANDLE)
!
END SUBROUTINE GET_FIELD
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_ISBA
