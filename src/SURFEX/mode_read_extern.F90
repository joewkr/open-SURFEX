!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #####################
MODULE MODE_READ_EXTERN
!     #####################
!-------------------------------------------------------------------
!
USE MODD_SURF_PAR,       ONLY : NUNDEF, XUNDEF
USE MODD_DATA_COVER_PAR, ONLY : NCOVER, NTYPE, NVEGTYPE, JPCOVER, NVEGTYPE_OLD, NVEGTYPE_ECOSG
!
USE MODE_READ_SURF_COV, ONLY : READ_SURF_COV
!
USE MODI_READ_LECOCLIMAP
!
USE MODI_OLD_NAME
USE MODI_OPEN_AUX_IO_SURF
USE MODI_CLOSE_AUX_IO_SURF
USE MODI_READ_SURF
USE MODI_MAKE_CHOICE_ARRAY
USE MODI_ABOR1_SFX
USE MODI_READ_PGD_COVER_GARDEN
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
CONTAINS
!
!---------------------------------------------------------------------------------------
!
!     #######################
      SUBROUTINE READ_EXTERN_DEPTH (U, DTCO, GCP, IO, &
                                    HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,&
                                    KLUOUT,HISBA,HNAT,HFIELD,KNI,KLAYER, &
                                   KPATCH,PSOILGRID,PDEPTH,KVERSION,KWG_LAYER          )
!     #######################
!
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
!
USE MODD_SURFEX_MPI, ONLY : NRANK,NPIO
!
USE MODI_OPEN_AUX_IO_SURF
USE MODI_CLOSE_AUX_IO_SURF
USE MODI_READ_SURF_ISBA_PAR_n
USE MODI_CONVERT_COVER_ISBA
USE MODI_GARDEN_SOIL_DEPTH
USE MODI_READ_ARRANGE_COVER
!
! Modifications :
! P.Marguinaud : 11-09-2012 : shorten field name
! G.Delautier : 24-06-2015 : bug for arome compressed files
!
IMPLICIT NONE
!
!* dummy arguments
!  ---------------
!
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
!
 CHARACTER(LEN=28),    INTENT(IN)    :: HFILE  ! type of input file
 CHARACTER(LEN=6),     INTENT(IN)    :: HFILETYPE  ! type of input file
 CHARACTER(LEN=28),    INTENT(IN)    :: HFILEPGD  ! type of input file
 CHARACTER(LEN=6),     INTENT(IN)    :: HFILEPGDTYPE  ! type of input file
INTEGER,              INTENT(IN)    :: KLUOUT    ! logical unit of output listing
CHARACTER(LEN=3),     INTENT(IN)    :: HISBA     ! type of ISBA soil scheme
CHARACTER(LEN=3),     INTENT(IN)    :: HNAT      ! type of surface (nature, gardens)
CHARACTER(LEN=7),     INTENT(IN)    :: HFIELD    ! field name
INTEGER,              INTENT(IN)    :: KNI       ! number of points
INTEGER,              INTENT(IN)    :: KLAYER    ! number of layers
INTEGER,              INTENT(IN)    :: KPATCH    ! number of patch
INTEGER,              INTENT(IN)    :: KVERSION  ! surface version
REAL, DIMENSION(:),   INTENT(IN)    :: PSOILGRID !
REAL, DIMENSION(:,:,:), POINTER     :: PDEPTH    ! depth of each layer over each patches
INTEGER, DIMENSION(:,:), INTENT(OUT):: KWG_LAYER
!
!* local variables
!  ---------------
!
 CHARACTER(LEN=4 ) :: YLVL
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=16) :: YRECFM2
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
 CHARACTER(LEN=6) :: YSURF
INTEGER           :: IRESP          ! reading return code
INTEGER           :: JL         ! loop counter
INTEGER           :: JP         ! loop counter
INTEGER           :: JJ, JI, IEND
INTEGER           :: IVERSION
INTEGER           :: IBUGFIX
!
LOGICAL, DIMENSION(:), ALLOCATABLE   :: GCOVER ! flag to read the covers
REAL,    DIMENSION(:,:), ALLOCATABLE :: ZCOVER ! cover fractions
REAL,    DIMENSION(:,:), ALLOCATABLE :: ZGROUND_DEPTH ! cover fractions
REAL, DIMENSION(:), ALLOCATABLE      :: ZSOILGRID
REAL,  DIMENSION(KNI)                :: ZHVEG  ! high vegetation fraction
REAL,  DIMENSION(KNI)                :: ZLVEG  ! low  vegetation fraction
REAL,  DIMENSION(KNI)                :: ZNVEG  ! no   vegetation fraction
REAL,  DIMENSION(KNI)                :: ZPERM  ! permafrost distribution
 CHARACTER(LEN=4)                    :: YHVEG  ! type of high vegetation
 CHARACTER(LEN=4)                    :: YLVEG  ! type of low  vegetation
 CHARACTER(LEN=4)                    :: YNVEG  ! type of no   vegetation
INTEGER                              :: INVEGTYPE_SAVE, IJPCOVER_SAVE
LOGICAL                              :: GECOCLIMAP ! T if ecoclimap is used
LOGICAL                              :: GECOSG
LOGICAL                              :: GPAR_GARDEN! T if garden data are used
LOGICAL, DIMENSION(NVEGTYPE_ECOSG)   :: GDATA_DG
LOGICAL, DIMENSION(NVEGTYPE_ECOSG)   :: GDATA_GROUND_DEPTH, GDATA_ROOT_DEPTH
LOGICAL                              :: GPERM
LOGICAL                              :: GREAD_EXT
LOGICAL      :: GREAD_OK, GDIM, GDIM2, GWATER_TO_NATURE, GTOWN_TO_ROCK, GGARDEN
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_EXTERN:READ_EXTERN_DEPTH',0,ZHOOK_HANDLE)
!
 CALL OPEN_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE,'FULL  ')

 YRECFM='VERSION'
 CALL READ_SURF(HFILEPGDTYPE,YRECFM,IVERSION,IRESP,HDIR='-')
!
YRECFM='BUG'
 CALL READ_SURF(HFILEPGDTYPE,YRECFM,IBUGFIX,IRESP,HDIR='-')
!
GDIM = (IVERSION>8 .OR. IVERSION==8 .AND. IBUGFIX>0)
GDIM2 = GDIM
IF (GDIM) CALL READ_SURF(HFILEPGDTYPE,'SPLIT_PATCH',GDIM2,IRESP)
!
 CALL READ_LECOCLIMAP(HFILEPGDTYPE,GECOCLIMAP,GECOSG,HDIR='-')
IF (HNAT=='NAT') THEN
  YSURF = "NATURE"
  CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
ELSE
  CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
  CALL OPEN_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE,'TOWN  ')
  CALL READ_SURF(HFILEPGDTYPE,'PAR_GARDEN',GPAR_GARDEN,IRESP,HDIR='-')
  GECOCLIMAP = .NOT. GPAR_GARDEN
  IF (.NOT.GECOCLIMAP) GECOSG = .FALSE.
  YSURF = "TOWN  "
  CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
END IF
!
INVEGTYPE_SAVE = NVEGTYPE
IJPCOVER_SAVE = JPCOVER
IF (GECOSG) THEN
  NVEGTYPE = NVEGTYPE_ECOSG
  JPCOVER = SUM(NTYPE)
ELSE
  NVEGTYPE = NVEGTYPE_OLD
  JPCOVER = NCOVER
ENDIF
!
!------------------------------------------------------------------------------
!
ALLOCATE(PDEPTH(KNI,KLAYER,KPATCH))
PDEPTH(:,:,:) = XUNDEF
!
KWG_LAYER(:,:) = NUNDEF
!
 CALL OPEN_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE,YSURF)
!
!-------------------------------------------------------------------
!
GREAD_OK = .FALSE.
!
!
IEND = 1
IF (GDIM) IEND = NVEGTYPE_ECOSG
!
IF (HNAT=='NAT' .AND. (IVERSION>=7 .OR. .NOT.GECOCLIMAP)) THEN
  !
  !* directly read soil layers in the file for nature ISBA soil layers
  !
  GDATA_DG(:) = .FALSE.
  IF (.NOT.GECOCLIMAP) GDATA_DG(:) = .TRUE.
  IF (IVERSION>=7) THEN
    YRECFM='L_DG'
    YCOMMENT=YRECFM
    CALL READ_SURF(HFILEPGDTYPE,YRECFM,GDATA_DG(1:IEND),IRESP,HCOMMENT=YCOMMENT,HDIR='-')
  ENDIF
  !
  IF (ANY(GDATA_DG(1:IEND))) THEN
    !
    DO JL=1,KLAYER
      IF (GDIM) THEN
        WRITE(YRECFM,FMT='(A6,I2.2)') 'D_DG_L',JL
      ELSE
        IF (JL<10)  WRITE(YRECFM,FMT='(A4,I1.1)') 'D_DG',JL
        IF (JL>=10) WRITE(YRECFM,FMT='(A4,I2.2)') 'D_DG',JL
      ENDIF
      CALL READ_SURF_ISBA_PAR_n(DTCO, U, GCP, KPATCH, HFILEPGDTYPE, YRECFM, KLUOUT, KNI, &
                                IVERSION, IBUGFIX, GDATA_DG, PDEPTH(:,JL,:),IRESP,HDIR='E')
    END DO
    GREAD_OK = .TRUE.
    !
  ENDIF
  !
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=2) THEN
    !
    !cas when root_depth and ground_depth were extrapolated in extrapol_field
    !during pgd step
    IF (.NOT.ANY(GDATA_DG(1:IEND)) .AND. HISBA=="3-L") THEN
      !
      YRECFM2='L_ROOT_DEPTH'
      YCOMMENT=YRECFM2
      CALL READ_SURF(HFILEPGDTYPE,YRECFM2,GDATA_ROOT_DEPTH(1:IEND),IRESP,HCOMMENT=YCOMMENT,HDIR='-')
      !
      IF (ANY(GDATA_ROOT_DEPTH(1:IEND))) THEN
        IF (GDIM) THEN
          YRECFM2='D_RTDPT_'
        ELSE
          YRECFM2='D_ROOT_DEPTH'
        ENDIF
        CALL READ_SURF_ISBA_PAR_n(DTCO, U, GCP, KPATCH, HFILEPGDTYPE, YRECFM2, KLUOUT, KNI, &
                                  IVERSION, IBUGFIX, GDATA_ROOT_DEPTH, PDEPTH(:,2,:),IRESP,HDIR='E')
      ENDIF
      !
    ENDIF
    !
    YRECFM2='L_GROUND_DEPTH'
    IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM2='L_GROUND_DPT'
    YCOMMENT=YRECFM2
    CALL READ_SURF(HFILEPGDTYPE,YRECFM2,GDATA_GROUND_DEPTH(1:IEND),IRESP,HCOMMENT=YCOMMENT,HDIR='-')
    !
    IF (ANY(GDATA_GROUND_DEPTH(1:IEND))) THEN
      !
      ALLOCATE(ZGROUND_DEPTH(KNI,KPATCH))
      IF (GDIM) THEN
        YRECFM2='D_GRDPT_'
      ELSE
        YRECFM2='D_GROUND_DEPTH'
        IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM2='D_GROUND_DPT'
      ENDIF
      CALL READ_SURF_ISBA_PAR_n(DTCO, U, GCP, KPATCH, HFILEPGDTYPE, YRECFM2, KLUOUT, KNI, &
                                IVERSION, IBUGFIX, GDATA_GROUND_DEPTH, ZGROUND_DEPTH(:,:),IRESP,HDIR='E')
      !
      IF (.NOT.ANY(GDATA_DG(1:IEND))) THEN
        !
        IF (HISBA=="2-L") THEN
          !
          PDEPTH(:,2,:) = ZGROUND_DEPTH(:,:)
          PDEPTH(:,1,:) = XUNDEF
          WHERE (ZGROUND_DEPTH(:,:)/=XUNDEF) PDEPTH(:,1,:) = 0.01
          GREAD_OK = .TRUE.
          !
        ELSEIF (HISBA=="3-L") THEN
          !
          PDEPTH(:,3,:) = ZGROUND_DEPTH(:,:)
          PDEPTH(:,1,:) = XUNDEF
          WHERE (ZGROUND_DEPTH(:,:)/=XUNDEF) PDEPTH(:,1,:) = 0.01
          IF (ANY(GDATA_ROOT_DEPTH(1:IEND))) GREAD_OK = .TRUE.
          !
        ELSEIF (HISBA=="DIF") THEN
          !
          ALLOCATE(ZSOILGRID(KLAYER))
          DO JL=1,KLAYER
            WRITE(YLVL,'(I4)') JL
            YRECFM2='SOILGRID'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
            CALL READ_SURF(HFILEPGDTYPE,YRECFM,ZSOILGRID(JL),IRESP)
            PDEPTH(:,JL,:) = ZSOILGRID(JL)
          ENDDO
          DEALLOCATE(ZSOILGRID)
          GREAD_OK = .TRUE.
          !
        ENDIF
      ENDIF
      !
      DO JP=1,KPATCH
        DO JJ=1,KNI
          DO JL=1,KLAYER
            IF ( PDEPTH(JJ,JL,JP) <= ZGROUND_DEPTH(JJ,JP) .AND. ZGROUND_DEPTH(JJ,JP) < XUNDEF ) &
                KWG_LAYER(JJ,JP) = JL
          ENDDO
        ENDDO
      ENDDO
      DEALLOCATE(ZGROUND_DEPTH)
      !
    ENDIF
    !
  ENDIF
  !
ELSE IF (HNAT=='GRD' .AND. .NOT.GECOCLIMAP ) THEN
  !
  !* computes soil layers from vegetation fractions read in the file
  !
  CALL READ_SURF(HFILEPGDTYPE,'D_TYPE_HVEG',YHVEG,IRESP,HDIR='E')
  CALL READ_SURF(HFILEPGDTYPE,'D_TYPE_LVEG',YLVEG,IRESP,HDIR='E')
  CALL READ_SURF(HFILEPGDTYPE,'D_TYPE_NVEG',YNVEG,IRESP,HDIR='E')
  CALL READ_SURF(HFILEPGDTYPE,'D_FRAC_HVEG',ZHVEG,IRESP,HDIR='E')
  CALL READ_SURF(HFILEPGDTYPE,'D_FRAC_LVEG',ZLVEG,IRESP,HDIR='E')
  CALL READ_SURF(HFILEPGDTYPE,'D_FRAC_NVEG',ZNVEG,IRESP,HDIR='E')
  ! Ground layers
  CALL GARDEN_SOIL_DEPTH(YNVEG,YLVEG,YHVEG,ZNVEG,ZLVEG,ZHVEG,PDEPTH)
  !
END IF
!
CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
!
IF (GECOCLIMAP .AND. .NOT.GREAD_OK ) THEN
  !
  IF (IVERSION>8 .OR. (IVERSION==8 .AND. IBUGFIX>=1)) THEN
    CALL OPEN_AUX_IO_SURF(HFILE,HFILETYPE,'FULL  ')
    CALL READ_SURF(HFILETYPE,'WRITE_EXT  ',GREAD_EXT,IRESP,HDIR='-')
    CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
  ELSE
    GREAD_EXT = .FALSE.
  ENDIF
  !
  IF (GREAD_EXT) THEN
    !
    CALL OPEN_AUX_IO_SURF(HFILE,HFILETYPE,YSURF)
    YRECFM='VERSION'
    CALL READ_SURF(HFILETYPE,YRECFM,IVERSION,IRESP)
    YRECFM='BUG'
    CALL READ_SURF(HFILETYPE,YRECFM,IBUGFIX,IRESP)
    GDIM = (IVERSION>8 .OR. IVERSION==8 .AND. IBUGFIX>0)
    GDIM2 = GDIM
    IF (GDIM) CALL READ_SURF(HFILETYPE,'SPLIT_PATCH',GDIM2,IRESP)
    DO JL=1,KLAYER
      WRITE(YLVL,'(I4)') JL
      YRECFM='DG'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
      CALL MAKE_CHOICE_ARRAY(HFILETYPE, KPATCH, GDIM2, YRECFM, PDEPTH(:,JL,:),HDIR='E')
    END DO
    CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
    !
  ELSE
    !
    CALL OPEN_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE,'FULL  ')
    !
    !* reading of the cover to obtain the depth of inter-layers
    !
    IF (GDIM.AND.GECOSG) THEN
      ALLOCATE(GCOVER(SUM(NTYPE)))
    ELSE
      ALLOCATE(GCOVER(NCOVER))
    ENDIF
    !
    CALL OLD_NAME(HFILEPGDTYPE,'COVER_LIST      ',YRECFM,HDIR='-')
    CALL READ_SURF(HFILEPGDTYPE,YRECFM,GCOVER(:),IRESP,HDIR='-')
    !
    ALLOCATE(ZCOVER(KNI,COUNT(GCOVER)))
    YRECFM='COVER'
    CALL READ_SURF_COV(HFILEPGDTYPE,YRECFM,ZCOVER(:,:),GCOVER(:),IRESP,HDIR='E')
    !
    CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
    !
    !* computes soil layers
    !
    !* permafrost distribution for soil depth
    !
    GPERM   =.FALSE.
    ZPERM(:)=0.0
    !
    IF (HNAT=='NAT'.AND.(IVERSION>7 .OR. (IVERSION==7 .AND. IBUGFIX>3)))THEN
      CALL OPEN_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE,'NATURE')
      YRECFM='PERMAFROST'
      CALL READ_SURF(HFILEPGDTYPE,YRECFM,GPERM,IRESP,HDIR='-')
      IF(GPERM)THEN
        YRECFM='PERM'
        CALL READ_SURF(HFILEPGDTYPE,YRECFM,ZPERM(:),IRESP,HDIR='E')
      ENDIF
      CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
    ENDIF
    !
    IF (SIZE(GCOVER)/=JPCOVER) THEN
      CALL OPEN_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE,'NATURE')
      CALL READ_PGD_COVER_GARDEN(HFILEPGDTYPE,GGARDEN)
      CALL READ_ARRANGE_COVER(HFILEPGDTYPE,GWATER_TO_NATURE,GTOWN_TO_ROCK,'A')
      CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
    ELSE
      GGARDEN = .FALSE.
      GWATER_TO_NATURE = .FALSE.
      GTOWN_TO_ROCK    = .FALSE.
    ENDIF
    !
    IF (NRANK==NPIO) THEN
      CALL CONVERT_COVER_ISBA(DTCO, IO%CALBEDO, &
                              HISBA,IO%LTR_ML,1,ZCOVER,GCOVER,'   ',HNAT,PSOILGRID=PSOILGRID, &
                              PPERM=ZPERM,PDG=PDEPTH,KWG_LAYER=KWG_LAYER, &
                              OWATER_TO_NATURE=GWATER_TO_NATURE, OTOWN_TO_ROCK=GTOWN_TO_ROCK, &
                              OGARDEN=GGARDEN )
    ENDIF
    !
    DEALLOCATE(GCOVER,ZCOVER)
    !
  ENDIF
  !
ENDIF
!
NVEGTYPE = INVEGTYPE_SAVE
JPCOVER = IJPCOVER_SAVE
!-------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_EXTERN:READ_EXTERN_DEPTH',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------
!
END SUBROUTINE READ_EXTERN_DEPTH
!
!
!-------------------------------------------------------------------
!---------------------------------------------------------------------------------------
!
!     #######################
      SUBROUTINE READ_EXTERN_ISBA (U, DTCO, GCP, IO, &
                                   HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,&
                                  KLUOUT,KNI,HFIELD,HNAME,PFIELD,PDEPTH,OKEY)
!     #######################
!
!
USE MODD_ISBA_n, ONLY : ISBA_NP_t, ISBA_K_t, ISBA_P_t, ISBA_NP_INIT, ISBA_K_INIT
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
!
USE MODD_SURFEX_MPI, ONLY : NRANK
USE MODD_ISBA_PAR,    ONLY : XOPTIMGRID
!
USE MODE_SOIL
USE MODI_READ_SURF
USE MODI_ISBA_SOC_PARAMETERS
USE MODI_PACK_SAME_RANK
!
IMPLICIT NONE
!
!* dummy arguments
!  ---------------
!
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
!
CHARACTER(LEN=28),    INTENT(IN)  :: HFILE     ! name of file
CHARACTER(LEN=6),     INTENT(IN)  :: HFILETYPE ! type of input file
CHARACTER(LEN=28),    INTENT(IN)  :: HFILEPGD     ! name of file
CHARACTER(LEN=6),     INTENT(IN)  :: HFILEPGDTYPE ! type of input file
INTEGER,              INTENT(IN)  :: KLUOUT    ! logical unit of output listing
INTEGER,              INTENT(IN)  :: KNI       ! number of points
CHARACTER(LEN=7),     INTENT(IN)  :: HFIELD    ! field name
CHARACTER(LEN=*),     INTENT(IN)  :: HNAME     ! field name in the file
REAL, DIMENSION(:,:,:), POINTER   :: PFIELD    ! field to initialize
REAL, DIMENSION(:,:,:), POINTER   :: PDEPTH    ! depth of each inter-layer
LOGICAL, OPTIONAL,  INTENT(INOUT) :: OKEY
!
!
!* local variables
!  ---------------
!
TYPE(ISBA_K_t) :: YK
TYPE(ISBA_NP_t) :: YNP
TYPE(ISBA_P_t), POINTER :: PK
!
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
#ifdef MNH_PARALLEL
 CHARACTER(LEN=8)  :: YPATCH
#endif
 CHARACTER(LEN=4)  :: YLVL
 CHARACTER(LEN=4)  :: YPEDOTF        ! type of pedo-transfert function
 CHARACTER(LEN=3)  :: YISBA          ! type of ISBA soil scheme
 CHARACTER(LEN=3)  :: YNAT           ! type of surface (nature, garden)
!
INTEGER           :: IRESP          ! reading return code
INTEGER           :: ILAYER, ILAYER_SAVE         ! number of layers
INTEGER           :: JL         ! loop counter
INTEGER           :: IPATCH         ! number of patch
INTEGER           :: JP         ! loop counter
INTEGER           :: JVEGTYPE       ! loop counter
LOGICAL           :: GTEB           ! TEB field
INTEGER           :: IWORK          ! work integer
INTEGER           :: JI
!
REAL,  DIMENSION(:),   ALLOCATABLE   :: ZCLAY     ! clay fraction
REAL,  DIMENSION(:),   ALLOCATABLE   :: ZSAND     ! sand fraction
REAL,  DIMENSION(:),   ALLOCATABLE   :: ZSOILGRID
REAL,  DIMENSION(:),   ALLOCATABLE   :: ZNAT      ! natural surface fraction
!
REAL,  DIMENSION(:,:),   ALLOCATABLE :: ZWWILT    ! wilting point
REAL,  DIMENSION(:,:),   ALLOCATABLE :: ZWFC      ! field capacity
REAL,  DIMENSION(:,:),   ALLOCATABLE :: ZWSAT     ! saturation
REAL,  DIMENSION(:,:),   ALLOCATABLE :: ZPATCH
!
REAL,  DIMENSION(KNI,2)              :: ZSOC
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZWORK, ZFRACSOC
INTEGER, DIMENSION(:,:), ALLOCATABLE :: IWG_LAYER
!
LOGICAL                              :: GTEMP_ARP ! Arpege soil temperature profile
LOGICAL                              :: GSOC_DATA ! Soil organic carbon (data in pgd)
LOGICAL                              :: GSOC      ! Soil organic carbon (physical option)
!
INTEGER :: IVERSION   ! surface version
INTEGER :: IBUGFIX, ISIZE
LOGICAL :: GDIM, GDIM2
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_READ_EXTERN:READ_EXTERN_ISBA',0,ZHOOK_HANDLE)
WRITE  (KLUOUT,*) ' | Reading ',HFIELD,' in externalized file'
!
!------------------------------------------------------------------------------
! Init
!
GTEB = (HNAME(1:3)=='TWN' .OR. HNAME(1:3)=='GD_' .OR. HNAME(1:3)=='GR_' &
        .OR. HNAME(4:6)=='GD_' .OR. HNAME(4:6)=='GR_')
!
GTEMP_ARP = .FALSE.
GSOC      = .FALSE.
GSOC_DATA = .FALSE.
!
!------------------------------------------------------------------------------
!
CALL OPEN_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE,'FULL  ')
YRECFM='VERSION'
CALL READ_SURF(HFILEPGDTYPE,YRECFM,IVERSION,IRESP,HDIR='-')
YRECFM='BUG'
CALL READ_SURF(HFILEPGDTYPE,YRECFM,IBUGFIX,IRESP,HDIR='-')
CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
!
IF (GTEB) THEN
  CALL OPEN_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE,'TOWN  ')
ELSE
  CALL OPEN_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE,'NATURE')
ENDIF
!
!* Read number of soil layers
!
YRECFM='GROUND_LAYER'
IF (GTEB) THEN
  YRECFM='TWN_LAYER'
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM='GD_LAYER'
ENDIF
CALL READ_SURF(HFILEPGDTYPE,YRECFM,ILAYER,IRESP,HDIR='-')
!
!* number of tiles
!
IPATCH=1
IF (.NOT. GTEB) THEN
  YRECFM='PATCH_NUMBER'
  CALL READ_SURF(HFILEPGDTYPE,YRECFM,IPATCH,IRESP,HDIR='-')
END IF
!
!* soil scheme
!
YRECFM='ISBA'
IF (GTEB) THEN
  YRECFM='TWN_ISBA'
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM='GD_ISBA'
ENDIF
CALL READ_SURF(HFILEPGDTYPE,YRECFM,YISBA,IRESP,HDIR='-')
IF(YISBA=='DIF'.AND.PRESENT(OKEY))THEN
  OKEY=.FALSE.
ENDIF
!
IF (IVERSION>=7) THEN
  !
  !* Pedo-transfert function
  !
  YRECFM='PEDOTF'
  IF (GTEB) THEN
    YRECFM='TWN_PEDOTF'
    IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM='GD_PEDOTF'
  ENDIF
  CALL READ_SURF(HFILEPGDTYPE,YRECFM,YPEDOTF,IRESP,HDIR='-')
  !
ELSE
  YPEDOTF = 'CH78'
ENDIF
!
!Only Brook and Corey with Force-Restore scheme
IF(YISBA/='DIF')THEN
  YPEDOTF='CH78'
ENDIF
!
!-------------------------------------------------------------------------------
!
! *.  Read clay fraction
!     ------------------
!
ALLOCATE(ZCLAY(KNI))
YRECFM='CLAY'
IF (GTEB) THEN
  YRECFM='TWN_CLAY'
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM='GD_CLAY'
ENDIF
 CALL READ_SURF(HFILEPGDTYPE,YRECFM,ZCLAY(:),IRESP,HDIR='E')
!
!-------------------------------------------------------------------------------
!
! *.  Read sand fraction
!     ------------------
!
ALLOCATE(ZSAND(KNI))
YRECFM='SAND'
IF (GTEB) THEN
  YRECFM='TWN_SAND'
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM='GD_SAND'
ENDIF
 CALL READ_SURF(HFILEPGDTYPE,YRECFM,ZSAND(:),IRESP,HDIR='E')
!
!-------------------------------------------------------------------------------
!
!
! *.  Soil organic carbon profile
!     ---------------------------
!
IF ( (.NOT.GTEB).AND.(IVERSION>7.OR.(IVERSION==7.AND.IBUGFIX>3)) &
     .AND.(YISBA=='DIF').AND.(HFIELD=='WG    '.OR.HFIELD=='WGI   ') ) THEN
   YRECFM='SOCP'
   CALL READ_SURF(HFILEPGDTYPE,YRECFM,GSOC_DATA,IRESP)
   IF(GSOC_DATA)THEN
     YRECFM='SOC_TOP'
     CALL READ_SURF(HFILEPGDTYPE,YRECFM,ZSOC(:,1),IRESP,HDIR='E')
     YRECFM='SOC_SUB'
     CALL READ_SURF(HFILEPGDTYPE,YRECFM,ZSOC(:,2),IRESP,HDIR='E')
     WHERE(ZSOC(:,:)==XUNDEF)ZSOC(:,:)=0.0
   ENDIF
ENDIF
!
!-------------------------------------------------------------------------------
!
! *.  Read soil grid
!     --------------
!
!* Reference grid for DIF
!
IF(YISBA=='DIF') THEN
  ALLOCATE(ZSOILGRID(ILAYER))
  ZSOILGRID=XUNDEF
  IF (IVERSION>=8) THEN
     DO JL=1,ILAYER
        WRITE(YLVL,'(I4)') JL
        YRECFM='SOILGRID'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
        IF (GTEB) THEN
           YRECFM='GD_SGRID'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
        ENDIF
        CALL READ_SURF(HFILEPGDTYPE,YRECFM,ZSOILGRID(JL),IRESP,HDIR='A')
     ENDDO
  ELSEIF (IVERSION==7 .AND. IBUGFIX>=2) THEN
    YRECFM='SOILGRID'
    IF (GTEB) THEN
      YRECFM='TWN_SOILGRID'
      IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM='GD_SOILGRID'
    ENDIF
    CALL READ_SURF(HFILEPGDTYPE,YRECFM,ZSOILGRID,IRESP,HDIR='A')
  ELSE
    ZSOILGRID(1:ILAYER) = XOPTIMGRID(1:ILAYER)
  ENDIF
ELSE
  ALLOCATE(ZSOILGRID(0))
ENDIF
!
ALLOCATE(IWG_LAYER(KNI,IPATCH))
IWG_LAYER(:,:) = NUNDEF
!
! *.  Read fraction of nature
!     --------------
!
ALLOCATE(ZNAT(KNI))
IF (IVERSION>=7) THEN
  IF (GTEB) THEN
    CALL READ_SURF(HFILEPGDTYPE,'FRAC_TOWN',ZNAT,IRESP,HDIR='E')
  ELSE
    CALL READ_SURF(HFILEPGDTYPE,'FRAC_NATURE',ZNAT,IRESP,HDIR='E')
  ENDIF
ELSE
  ZNAT=1.0
ENDIF
!
!
CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
!
!
IF (.NOT.GTEB .AND. HFIELD=='TG    ' .AND. (YISBA=='2-L' .OR. YISBA=='3-L') ) THEN
  IF (IVERSION>7) THEN
     YRECFM='TEMPARP'
     CALL OPEN_AUX_IO_SURF(HFILE,HFILETYPE,'NATURE')
     CALL READ_SURF(HFILETYPE,YRECFM,GTEMP_ARP,IRESP,HDIR='-')
     IF(GTEMP_ARP)THEN
       YRECFM = 'NTEMPLARP'
       CALL READ_SURF(HFILETYPE,YRECFM,ILAYER,IRESP,HDIR='-')
     ENDIF
     CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
   ENDIF
ENDIF
!
!
IF ((HFIELD=='TG    ') .AND. (YISBA=='2-L' .OR. YISBA=='3-L')) THEN
  ALLOCATE(PDEPTH    (KNI,ILAYER,NVEGTYPE))
  DO JVEGTYPE=1,NVEGTYPE
    PDEPTH(:,1,JVEGTYPE) = 0.01
    PDEPTH(:,2,JVEGTYPE) = 0.40
    IF (ILAYER==3) PDEPTH(:,3,JVEGTYPE) = 5.00
!   GTEMP_ARP case
    IF (GTEMP_ARP) THEN
       PDEPTH(:,3,JVEGTYPE) = 1.0
       DO JL=4,ILAYER
          PDEPTH(:,JL,JVEGTYPE) = PDEPTH(:,JL-1,JVEGTYPE)+1.
       ENDDO
    ENDIF
  END DO
ELSE
  YNAT='NAT'
  IF (GTEB) YNAT='GRD'
  !
  CALL READ_EXTERN_DEPTH(U, DTCO,  GCP, IO,                      &
                         HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,  &
                         KLUOUT,YISBA,YNAT,HFIELD,KNI,           &
                         ILAYER,IPATCH,ZSOILGRID,PDEPTH,IVERSION,IWG_LAYER)
  !
END IF
!
DEALLOCATE(ZSOILGRID)
!
!
! *.  Read soil variable profile
!     --------------------------
!
IF (GTEB) THEN
  CALL OPEN_AUX_IO_SURF(HFILE,HFILETYPE,'TOWN  ')
ELSE
  CALL OPEN_AUX_IO_SURF(HFILE,HFILETYPE,'NATURE')
ENDIF
!
YRECFM='VERSION'
 CALL READ_SURF(HFILETYPE,YRECFM,IVERSION,IRESP)
YRECFM='BUG'
 CALL READ_SURF(HFILETYPE,YRECFM,IBUGFIX,IRESP)
GDIM = (IVERSION>8 .OR. IVERSION==8 .AND. IBUGFIX>0)
IF (GTEB) GDIM=.FALSE.
GDIM2 = GDIM
IF (GDIM) CALL READ_SURF(HFILETYPE,'SPLIT_PATCH',GDIM2,IRESP)
!
IWORK=ILAYER
IF(YISBA=='2-L'.OR.YISBA=='3-L') THEN
  SELECT CASE(HFIELD)
    CASE('TG    ')
      IF(GTEMP_ARP)THEN
        IWORK=ILAYER
      ELSE
        IWORK=2
      ENDIF
    CASE('WGI   ')
      IWORK=2
  END SELECT
ENDIF
!
ALLOCATE(PFIELD(KNI,ILAYER,IPATCH))
!
DO JL=1,IWORK
  WRITE(YLVL,'(I4)') JL
  YRECFM=TRIM(HNAME)//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  CALL MAKE_CHOICE_ARRAY(HFILETYPE, IPATCH, GDIM2, YRECFM, PFIELD(:,JL,:),HDIR='E')
END DO
!
DO JP=1,IPATCH
  DO JL=1,IWORK
    WHERE (ZNAT(:)==0.) PFIELD(:,JL,JP) = XUNDEF
  ENDDO
END DO
!
DEALLOCATE (ZNAT)
!
IF(YISBA=='3-L') THEN
  SELECT CASE(HFIELD)
    CASE('TG    ')
      IF(.NOT.GTEMP_ARP)PFIELD(:,3,:)=PFIELD(:,2,:)
    CASE('WGI   ')
     PFIELD(:,3,:)=PFIELD(:,2,:)
  END SELECT
ENDIF
!
 CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
!
!
! *.  Compute relative humidity from units kg/m^2 (SWI)
!     ------------------------------------------------
!
IF (HFIELD=='WG    ' .OR. HFIELD=='WGI   ') THEN
  !
  ! Compute ISBA model constants
  !
  ALLOCATE (ZWFC  (KNI,ILAYER))
  ALLOCATE (ZWWILT(KNI,ILAYER))
  ALLOCATE (ZWSAT (KNI,ILAYER))
  !
  ZWSAT (:,1) = WSAT_FUNC (ZCLAY(:),ZSAND(:),YPEDOTF)
  ZWWILT(:,1) = WWILT_FUNC(ZCLAY(:),ZSAND(:),YPEDOTF)
  IF(YISBA=='DIF')THEN
    ZWFC(:,1) = W33_FUNC(ZCLAY(:),ZSAND(:),YPEDOTF)
  ELSE
    ZWFC(:,1) = WFC_FUNC(ZCLAY(:),ZSAND(:),YPEDOTF)
  ENDIF
  DO JL=2,ILAYER
     ZWSAT (:,JL) = ZWSAT (:,1)
     ZWFC  (:,JL) = ZWFC  (:,1)
     ZWWILT(:,JL) = ZWWILT(:,1)
  ENDDO
  !
  DEALLOCATE (ZSAND)
  DEALLOCATE (ZCLAY)
  !
  IF(GSOC_DATA)THEN
    !
    ALLOCATE(ZWORK(KNI,IPATCH))
    !
    CALL OPEN_AUX_IO_SURF(HFILE,HFILETYPE,'NATURE')
    !
    YRECFM='SOC'
    CALL READ_SURF(HFILETYPE,YRECFM,GSOC,IRESP,HDIR='-')
    YRECFM='PATCH'
    CALL MAKE_CHOICE_ARRAY(HFILETYPE, IPATCH, GDIM2, YRECFM, ZWORK,HDIR='E')
    WHERE(ZWORK(:,:)==XUNDEF)ZWORK(:,:)=0.0
    CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
    !
    IF(GSOC)THEN
      !
      ALLOCATE(YNP%AL(IPATCH))
      DO JP = 1,IPATCH
        PK => YNP%AL(JP)
        PK%NSIZE_P = KNI
        ALLOCATE(PK%NR_P(KNI))
        DO JI = 1,KNI
          PK%NR_P(JI) = JI
        ENDDO
        ALLOCATE(PK%XPATCH(KNI))
        PK%XPATCH = ZWORK(:,JP)
        ALLOCATE(PK%XDG(KNI,ILAYER))
        PK%XDG(:,:) = PDEPTH(:,:,JP)
        ALLOCATE(PK%XCONDSAT(KNI,ILAYER))
        PK%XCONDSAT(:,:) = 0.0
      ENDDO
      ALLOCATE(YK%XBCOEF   (KNI,ILAYER))
      ALLOCATE(YK%XMPOTSAT (KNI,ILAYER))
      ALLOCATE(YK%XHCAPSOIL(KNI,ILAYER))
      ALLOCATE(YK%XCONDDRY (KNI,ILAYER))
      ALLOCATE(YK%XCONDSLD (KNI,ILAYER))
      ALLOCATE(YK%XWD0     (KNI,ILAYER))
      ALLOCATE(YK%XKANISO  (KNI,ILAYER))
      ALLOCATE(ZFRACSOC (KNI,ILAYER))
      YK%XBCOEF   (:,:)=0.0
      YK%XMPOTSAT (:,:)=0.0
      YK%XHCAPSOIL(:,:)=0.0
      YK%XCONDDRY (:,:)=XUNDEF
      YK%XCONDSLD (:,:)=XUNDEF
      YK%XWD0     (:,:)=0.0
      YK%XKANISO   (:,:)=0.0
      ZFRACSOC (:,:)=0.0
      CALL ISBA_SOC_PARAMETERS ('NONE', ZSOC, YK, YNP, ZFRACSOC, &
                                ZWSAT, ZWFC, ZWWILT, IPATCH)
      CALL ISBA_K_INIT(YK)
      CALL ISBA_NP_INIT(YNP,IPATCH)
      !
    ENDIF
    !
  ENDIF
  !
  IF(YISBA=='DIF')THEN
    !
    ! extrapolation of deep layers
    DO JP=1,IPATCH
      DO JI=1,KNI
        IWORK=IWG_LAYER(JI,JP)
        IF(IWORK<ILAYER)THEN
          DO JL=IWORK+1,ILAYER
            PFIELD(JI,JL,JP)=PFIELD(JI,IWORK,JP)
          ENDDO
        ENDIF
      ENDDO
    ENDDO
  ENDIF
  !
  IF (HFIELD=='WG    ') THEN
    DO JP=1,IPATCH
      DO JL=1,ILAYER
        DO JI=1,KNI
          IF(PFIELD(JI,JL,JP)/=XUNDEF)THEN
            PFIELD(JI,JL,JP) = MAX(MIN(PFIELD(JI,JL,JP),ZWSAT(JI,JL)),0.)
            !
            PFIELD(JI,JL,JP) = (PFIELD(JI,JL,JP) - ZWWILT(JI,JL)) / &
                                (ZWFC(JI,JL) - ZWWILT(JI,JL))
          ENDIF
        END DO
      END DO
    END DO
  ELSE IF (HFIELD=='WGI   ') THEN
    DO JP=1,IPATCH
      DO JL=1,ILAYER
        WHERE(PFIELD(:,JL,JP)/=XUNDEF)
          PFIELD(:,JL,JP) = PFIELD(:,JL,JP) / ZWSAT(:,JL)
        END WHERE
      END DO
    END DO
  END IF
!
  DEALLOCATE (ZWSAT)
  DEALLOCATE (ZWWILT)
  DEALLOCATE (ZWFC)
!
END IF
!
DEALLOCATE(IWG_LAYER)
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_EXTERN:READ_EXTERN_ISBA',1,ZHOOK_HANDLE)
!
!------------------------------------------------------------------------------
!
END SUBROUTINE READ_EXTERN_ISBA
!
!------------------------------------------------------------------------------
!
END MODULE MODE_READ_EXTERN
