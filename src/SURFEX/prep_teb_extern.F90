!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_TEB_EXTERN (DTCO, GCP, &
                            HPROGRAM,HSURF,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,KLUOUT,KPATCH,PFIELD)
!     #################################################################################
!
!
!!    MODIFICATIONS
!!    -------------
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO
USE MODD_TYPE_DATE_SURF
!
USE MODI_PREP_GRID_EXTERN
USE MODI_READ_SURF
USE MODI_GET_TEB_DEPTHS
USE MODI_INTERP_GRID
USE MODI_OPEN_AUX_IO_SURF
USE MODI_CLOSE_AUX_IO_SURF
USE MODI_TOWN_PRESENCE
USE MODI_READ_TEB_PATCH
USE MODI_MAKE_CHOICE_ARRAY
!
USE MODD_PREP,       ONLY : CINGRID_TYPE, CINTERP_TYPE
USE MODD_PREP_TEB,   ONLY : XGRID_ROAD, XGRID_WALL, XGRID_ROOF, &
                            XGRID_FLOOR, XWS_ROOF, XWS_ROAD, &
                            XTI_BLD_DEF, XWS_ROOF_DEF, XWS_ROAD_DEF, XHUI_BLD_DEF
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER
USE MODD_SURF_PAR, ONLY: XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILE     ! name of file
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILETYPE ! type of input file
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILEPGD     ! name of file
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILEPGDTYPE ! type of input file
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
INTEGER,            INTENT(IN)  :: KPATCH
REAL,DIMENSION(:,:), POINTER    :: PFIELD    ! field to interpolate horizontally
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZFIELD         ! field read
REAL, DIMENSION(:,:), ALLOCATABLE :: ZDEPTH         ! depth of each layer
REAL :: ZDEPTH_TOT     ! total depth of surface
!
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZD  ! intermediate array
!
REAL, DIMENSION(:), ALLOCATABLE :: ZMASK
!
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
INTEGER           :: IRESP          ! reading return code
INTEGER           :: ILAYER         ! number of layers
INTEGER           :: JLAYER, JI         ! loop counter
INTEGER           :: IVERSION_PGD, IVERSION_PREP       ! SURFEX version
INTEGER           :: IBUGFIX_PGD, IBUGFIX_PREP        ! SURFEX bug version
LOGICAL           :: GOLD_NAME      ! old name flag for temperatures
 CHARACTER(LEN=4)  :: YWALL_OPT      ! option of walls
 CHARACTER(LEN=6)  :: YSURF          ! Surface type
 CHARACTER(LEN=3)  :: YBEM ! key of the building energy model DEF for DEFault (Masson et al. 2002) ,
                          ! BEM for Building Energy Model (Bueno et al. 2012)
!
INTEGER           :: INI            ! total 1D dimension
!
LOGICAL :: GDIM
LOGICAL                              :: GTEB      ! flag if TEB fields are present
INTEGER                              :: IPATCH    ! number of soil temperature patches
INTEGER                              :: ITEB_PATCH! number of TEB patches in file
 CHARACTER(LEN=3)                     :: YPATCH    ! indentificator for TEB patch
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
!*      1.     Preparation of IO for reading in the file
!              -----------------------------------------
!
!* Note that all points are read, even those without physical meaning.
!  These points will not be used during the horizontal interpolation step.
!  Their value must be defined as XUNDEF.
!
IF (LHOOK) CALL DR_HOOK('PREP_TEB_EXTERN',0,ZHOOK_HANDLE)
!
!
 CALL OPEN_AUX_IO_SURF(HFILE,HFILETYPE,'FULL  ')
 CALL READ_SURF(HFILETYPE,'VERSION',IVERSION_PREP,IRESP,HDIR='-')
 CALL READ_SURF(HFILETYPE,'BUG',IBUGFIX_PREP,IRESP,HDIR='-')
 GDIM = (IVERSION_PREP>8 .OR. IVERSION_PREP==8 .AND. IBUGFIX_PREP>0)
 IF (GDIM) CALL READ_SURF(HFILETYPE,'SPLIT_PATCH',GDIM,IRESP)
 CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
 !
!* reading of version of the file being read
CALL OPEN_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE,'FULL  ')
CALL READ_SURF(HFILEPGDTYPE,'VERSION',IVERSION_PGD,IRESP,HDIR='-')
CALL READ_SURF(HFILEPGDTYPE,'BUG',IBUGFIX_PGD,IRESP,HDIR='-')
!
!-------------------------------------------------------------------------------------
!
!*      2.     Reading of grid
!              ---------------
!
!* reads the grid
CALL PREP_GRID_EXTERN(GCP,HFILEPGDTYPE,KLUOUT,CINGRID_TYPE,CINTERP_TYPE,INI)
!
IF (NRANK/=NPIO) INI = 0
!
!* reads if TEB fields exist in the input file
 CALL TOWN_PRESENCE(HFILEPGDTYPE,GTEB,HDIR='-')
!
ALLOCATE(ZMASK(INI))
IF (IVERSION_PGD>=7.AND.GTEB) THEN
  YRECFM='FRAC_TOWN'
  CALL READ_SURF(HFILEPGDTYPE,YRECFM,ZMASK,IRESP,HDIR='A')
ELSE
  ZMASK(:) = 1.
ENDIF
!
 CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
!
!---------------------------------------------------------------------------------------
!
!*     3.      Orography
!              ---------
!
IF (HSURF=='ZS     ') THEN
  !
  ALLOCATE(PFIELD(INI,1))
  YRECFM='ZS'
  CALL OPEN_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE,'FULL  ')
  CALL READ_SURF(HFILEPGDTYPE,YRECFM,PFIELD(:,1),IRESP,HDIR='E')
  CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
  !
  !---------------------------------------------------------------------------------------
ELSE
!---------------------------------------------------------------------------------------
!
!*     4.     TEB fields are read
!             -------------------
!
  IF (GTEB) THEN
!
    CALL OPEN_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE,'TOWN  ')
    GOLD_NAME=(IVERSION_PGD<7 .OR. (IVERSION_PGD==7 .AND. IBUGFIX_PGD<3))
    IF (.NOT.GOLD_NAME.AND.GTEB) THEN
      YRECFM='BEM'
      CALL READ_SURF(HFILEPGDTYPE,YRECFM,YBEM,IRESP,HDIR='-')
    ELSE
      YBEM='DEF'
    ENDIF
    CALL READ_TEB_PATCH(HFILEPGD,HFILEPGDTYPE,IVERSION_PGD,IBUGFIX_PGD,ITEB_PATCH,HDIR='-')
    YPATCH='   '
    IF (ITEB_PATCH>1) THEN
      WRITE(YPATCH,FMT='(A,I1,A)') 'T',MIN(KPATCH,ITEB_PATCH),'_'
    END IF
    CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
!
!---------------------------------------------------------------------------------------
    SELECT CASE(HSURF)
!---------------------------------------------------------------------------------------
!
!*     4.1    Profile of temperatures in roads, roofs or walls
!             ------------------------------------------------
!
    CASE('T_ROAD','T_ROOF','T_WALLA','T_WALLB','T_FLOOR','T_MASS')
      CALL OPEN_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE,'TOWN  ')
      YSURF=HSURF(1:6)
      !* reading of number of layers
      IF (YSURF=='T_ROAD') YRECFM='ROAD_LAYER'
      IF (YSURF=='T_ROOF') YRECFM='ROOF_LAYER'
      IF (YSURF=='T_WALL') YRECFM='WALL_LAYER'
      IF (YSURF=='T_FLOO' .OR. YSURF=='T_MASS') THEN
        IF (YBEM=='DEF') THEN
          YRECFM='ROAD_LAYER'
        ELSE
          YRECFM='FLOOR_LAYER'
        END IF
      END IF
      CALL READ_SURF(HFILEPGDTYPE,YRECFM,ILAYER,IRESP,HDIR='-')
      CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
      !
      ALLOCATE(ZD(INI,ILAYER))
      IF (YSURF=='T_ROAD') CALL GET_TEB_DEPTHS(DTCO,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,PD_ROAD=ZD,HDIR='E')
      IF (YSURF=='T_ROOF') CALL GET_TEB_DEPTHS(DTCO,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,PD_ROOF=ZD,HDIR='E')
      IF (YSURF=='T_WALL') CALL GET_TEB_DEPTHS(DTCO,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,PD_WALL=ZD,HDIR='E')
      IF (YSURF=='T_WALLA') CALL GET_TEB_DEPTHS(DTCO,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,PD_WALL=ZD,HDIR='E')
      IF (YSURF=='T_WALLB') CALL GET_TEB_DEPTHS(DTCO,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,PD_WALL=ZD,HDIR='E')
      IF (YSURF=='T_MASS') CALL GET_TEB_DEPTHS(DTCO,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,PD_FLOOR=ZD,HDIR='E')
      IF (YSURF=='T_FLOO') CALL GET_TEB_DEPTHS(DTCO,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,PD_FLOOR=ZD,HDIR='E')
      !
      !* reading of version of the file being read
      GOLD_NAME=(IVERSION_PREP<7 .OR. (IVERSION_PREP==7 .AND. IBUGFIX_PREP<3))
      !
      CALL OPEN_AUX_IO_SURF(HFILE,HFILETYPE,'TOWN  ')
      !
      !* reading option for road orientation
      YWALL_OPT = 'UNIF'
      IF (YSURF =='T_WALL' .AND. .NOT. GOLD_NAME) THEN
        CALL READ_SURF(HFILETYPE,'WALL_OPT',YWALL_OPT,IRESP,HDIR='-')
      END IF
      !
      !* reading of the profile
      ALLOCATE(ZFIELD(INI,ILAYER))
      DO JLAYER=1,ILAYER
        !
        IF (GOLD_NAME) THEN
          WRITE(YRECFM,'(A6,I1.1)') HSURF(1:6),JLAYER
        ELSE
          !
          IF (YSURF =='T_WALL' .AND. YWALL_OPT/='UNIF') THEN
            WRITE(YRECFM,'(A1,A5,I1.1)') HSURF(1:1),HSURF(3:7),JLAYER
          ELSEIF ((YSURF=='T_FLOO' .OR. YSURF=='T_MASS') .AND. YBEM=='DEF') THEN
            IF (YSURF=='T_FLOO' .AND. JLAYER>1) THEN
              WRITE(YRECFM,'(A5,I1.1)') 'TROAD',JLAYER
            ELSE
              WRITE(YRECFM,'(A6)') 'TI_BLD'
            ENDIF
          ELSE
            WRITE(YRECFM,'(A1,A4,I1.1)') HSURF(1:1),HSURF(3:6),JLAYER
          END IF
          !
        END IF
        !
        YRECFM=YPATCH//YRECFM
        YRECFM=ADJUSTL(YRECFM)
        CALL READ_SURF(HFILETYPE,YRECFM,ZFIELD(:,JLAYER),IRESP,HDIR='E')
        !
      END DO
      CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
      !
      DO JLAYER=1,SIZE(ZFIELD,2)
        WHERE (ZMASK(:)==0.) ZFIELD(:,JLAYER) = XUNDEF
      ENDDO
      !
      IF (NRANK==NPIO) THEN
        !
        !* recovers middle layer depth (from the surface)
        ALLOCATE(ZDEPTH    (INI,ILAYER))
        DO JI=1,INI
          !
          ZDEPTH    (JI,1)= ZD(JI,1)/2.
          ZDEPTH_TOT      = ZD(JI,1)
          DO JLAYER=2,ILAYER
            ZDEPTH    (JI,JLAYER) = ZDEPTH_TOT + ZD(JI,JLAYER)/2.
            ZDEPTH_TOT = ZDEPTH_TOT + ZD(JI,JLAYER)
          ENDDO
          !
          !* in case of wall or roof, normalizes by total wall or roof thickness
          IF (YSURF=='T_ROOF' .OR. YSURF=='T_WALL' .OR. YSURF == 'T_FLOO' .OR. YSURF == 'T_MASS') THEN
            DO JLAYER=1,ILAYER
              ZDEPTH(JI,JLAYER) = ZDEPTH(JI,JLAYER) / ZDEPTH_TOT
            END DO
          END IF
          !
        ENDDO
        !
        DEALLOCATE(ZD)
        !
        !* interpolation on the fine vertical grid
        IF (YSURF=='T_ROAD') THEN
          ALLOCATE(PFIELD(SIZE(ZFIELD,1),SIZE(XGRID_ROAD)))
          CALL INTERP_GRID(ZDEPTH,ZFIELD,XGRID_ROAD,PFIELD)
        ELSEIF (YSURF=='T_ROOF') THEN
          ALLOCATE(PFIELD(SIZE(ZFIELD,1),SIZE(XGRID_ROOF)))
          CALL INTERP_GRID(ZDEPTH,ZFIELD,XGRID_ROOF,PFIELD)
        ELSEIF (YSURF=='T_WALL') THEN
          ALLOCATE(PFIELD(SIZE(ZFIELD,1),SIZE(XGRID_WALL)))
          CALL INTERP_GRID(ZDEPTH,ZFIELD,XGRID_WALL,PFIELD)
        ELSEIF (YSURF=='T_FLOO' .OR. YSURF=='T_MASS') THEN
          ALLOCATE(PFIELD(SIZE(ZFIELD,1),SIZE(XGRID_FLOOR)))
          CALL INTERP_GRID(ZDEPTH,ZFIELD,XGRID_FLOOR,PFIELD)
        END IF
        DEALLOCATE(ZDEPTH)
        !
      ENDIF
      !
      !* end
      DEALLOCATE(ZFIELD)
!---------------------------------------------------------------------------------------
!
!*      4.2    Internal moisture
!              ---------------
!
    CASE('QI_BLD ')
      ALLOCATE(PFIELD(INI,1))
      IF (YBEM=='BEM') THEN
        YRECFM='QI_BLD'
        YRECFM=YPATCH//YRECFM
        YRECFM=ADJUSTL(YRECFM)
        CALL OPEN_AUX_IO_SURF(HFILE,HFILETYPE,'TOWN  ')
        CALL READ_SURF(HFILETYPE,YRECFM,PFIELD(:,1),IRESP,HDIR='E')
        CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
        WHERE (ZMASK(:)==0.) PFIELD(:,1) = XUNDEF
      ELSE
        IF (INI>0) PFIELD(:,1) = XUNDEF
      ENDIF
!
!---------------------------------------------------------------------------------------
!
!*      4.2    Other variables
!              ---------------
!
    CASE DEFAULT
      ALLOCATE(PFIELD(INI,1))
      CALL OPEN_AUX_IO_SURF(HFILE,HFILETYPE,'TOWN  ')
      YRECFM=HSURF
      GOLD_NAME=(IVERSION_PREP<7 .OR. (IVERSION_PREP==7 .AND. IBUGFIX_PREP<3))
      IF (HSURF=='T_CAN  ') THEN
        YRECFM='TCANYON'
        IF (GOLD_NAME) YRECFM='T_CANYON'
      ELSEIF (HSURF=='Q_CAN  ') THEN
        YRECFM='QCANYON'
        IF (GOLD_NAME) YRECFM='Q_CANYON'
      ELSEIF (HSURF=='T_WIN2 ' .OR. HSURF=='T_WIN1') THEN
        IF (YBEM=='BEM') THEN
          YRECFM=HSURF
        ELSE
          YRECFM='TI_BLD'
        ENDIF
      ENDIF
      YRECFM=YPATCH//YRECFM
      YRECFM=ADJUSTL(YRECFM)
      CALL READ_SURF(HFILETYPE,YRECFM,PFIELD(:,1),IRESP,HDIR='E')
      CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
      WHERE (ZMASK(:)==0.) PFIELD(:,1) = XUNDEF
!
!---------------------------------------------------------------------------------------
    END SELECT
!---------------------------------------------------------------------------------------
!
!*     5.     Subtitutes if TEB fields do not exist
!             -------------------------------------
!
  ELSE

    SELECT CASE(HSURF)

    !* temperature profiles
    CASE('T_ROAD','T_ROOF','T_WALL','T_WIN1','T_FLOOR','T_CAN','TI_ROAD','T_WALLA','T_WALLB')
      YSURF=HSURF(1:6)
      !
      !* reading of the soil surface temperature
      CALL OPEN_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE,'NATURE')
      IPATCH = 0
      CALL READ_SURF(HFILEPGDTYPE,'PATCH_NUMBER',IPATCH,IRESP,HDIR='-')
      CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
      !
      ALLOCATE(ZFIELD(INI,IPATCH))
      !
      CALL OPEN_AUX_IO_SURF(HFILE,HFILETYPE,'NATURE')
      IF (YSURF=='T_FLOO' .OR. YSURF=='T_CAN ' .OR. YSURF=='TI_ROA') THEN
        CALL MAKE_CHOICE_ARRAY(HFILETYPE, IPATCH, GDIM, 'TG2', ZFIELD(:,:),HDIR='E')
      ELSE
        CALL MAKE_CHOICE_ARRAY(HFILETYPE, IPATCH, GDIM, 'TG1', ZFIELD(:,:),HDIR='E')
      ENDIF
      CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
      DO JLAYER=1,SIZE(ZFIELD,2)
        WHERE (ZMASK(:)==0.) ZFIELD(:,JLAYER) = XUNDEF
      ENDDO
      !* fills the whole temperature profile by this soil temperature
      IF (YSURF=='T_ROAD') ILAYER=SIZE(XGRID_ROAD)
      IF (YSURF=='T_ROOF') ILAYER=SIZE(XGRID_ROOF)
      IF (YSURF=='T_WALL') ILAYER=SIZE(XGRID_WALL)
      IF (YSURF=='T_FLOO') ILAYER=SIZE(XGRID_FLOOR)
      IF (YSURF=='T_WIN1' .OR. YSURF=='T_CAN ' .OR. YSURF=='TI_ROA') ILAYER=1
      ALLOCATE(PFIELD(INI,ILAYER))
      IF (YSURF=='T_FLOO') THEN
        !* sets the temperature equal to this deep soil temperature
        PFIELD(:,1) = XTI_BLD_DEF
      ELSE
        PFIELD(:,1) = ZFIELD(:,1)
      ENDIF
      DO JLAYER=2,ILAYER
        PFIELD(:,JLAYER) = ZFIELD(:,1)
      END DO
      DEALLOCATE(ZFIELD)

    CASE('T_MASS','TI_BLD','T_WIN2')
      YSURF=HSURF(1:6)
      IF (YSURF=='T_MASS') ILAYER = SIZE(XGRID_FLOOR)
      IF (YSURF=='TI_BLD'.OR.YSURF=='T_WIN2') ILAYER=1
      ALLOCATE(PFIELD(INI, ILAYER))
      PFIELD(:,:) = XTI_BLD_DEF

    !* building moisture
    CASE('QI_BLD ')
      ALLOCATE(PFIELD(INI,1))
      PFIELD(:,1) = XUNDEF

    !* water reservoirs
    CASE('WS_ROOF','WS_ROAD')
      ALLOCATE(PFIELD(INI,1))
      IF (HSURF=='WS_ROOF') PFIELD = XWS_ROOF_DEF
      IF (HSURF=='WS_ROAD') PFIELD = XWS_ROAD_DEF

   !* other fields
    CASE DEFAULT
      ALLOCATE(PFIELD(INI,1))
      PFIELD = 0.

    END SELECT

  END IF
!-------------------------------------------------------------------------------------
END IF
!-------------------------------------------------------------------------------------
!
DEALLOCATE(ZMASK)
!
!*      6.     End of IO
!              ---------
!
IF (LHOOK) CALL DR_HOOK('PREP_TEB_EXTERN',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_TEB_EXTERN
