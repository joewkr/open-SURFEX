!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_TEB_DEPTHS (DTCO,  &
                                 HFILE, HFILETYPE, HFILEPGD, HFILEPGDTYPE, &
                                 PD_ROOF, PD_ROAD, PD_WALL, PD_FLOOR,HDIR)
!     ##############################################################
!
!!**** *CONVERT_COVER*
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
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
!!    Original    01/2004
!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO
USE MODD_DATA_COVER,     ONLY : XDATA_D_ROOF, XDATA_D_ROAD, XDATA_D_WALL, XDATA_D_FLOOR
USE MODD_DATA_COVER_PAR, ONLY : NCOVER, NTYPE, NDATA_ROOF_LAYER, NDATA_ROAD_LAYER, &
                                NDATA_WALL_LAYER, NDATA_FLOOR_LAYER
!
USE MODI_OPEN_AUX_IO_SURF
USE MODI_CLOSE_AUX_IO_SURF
USE MODE_READ_SURF_COV, ONLY : READ_SURF_COV
!
USE MODI_READ_SURF
USE MODI_AV_PGD
USE MODI_OLD_NAME
USE MODI_THERMAL_LAYERS_CONF
USE MODI_OPEN_AUX_IO_SURF
USE MODI_CLOSE_AUX_IO_SURF
USE MODI_READ_LECOCLIMAP
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
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
!
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILE ! type of input file
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILETYPE ! type of input file
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILEPGD  ! type of input file
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILEPGDTYPE ! type of input file
!
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL   :: PD_ROOF
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL   :: PD_ROAD
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL   :: PD_WALL
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL   :: PD_FLOOR
!
 CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: HDIR
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
LOGICAL, DIMENSION(:), ALLOCATABLE   :: GCOVER ! flag to read the covers
REAL,    DIMENSION(:,:), ALLOCATABLE :: ZCOVER ! cover fractions
REAL,    DIMENSION(:,:), ALLOCATABLE :: ZD     ! depth of surface layers
REAL,    DIMENSION(:,:), ALLOCATABLE :: ZPAR_D ! depth of data_surface layers
!
REAL, DIMENSION(SIZE(XDATA_D_ROOF,1),SIZE(XDATA_D_ROOF,2)) :: ZDATA
!
INTEGER           :: IVERSION_PGD, IVERSION_PREP       ! surface version
INTEGER           :: IBUGFIX_PGD, IBUGFIX_PREP        ! surface bugfix version
INTEGER           :: IVERSION       ! surface version
INTEGER           :: IBUGFIX        ! surface bugfix version
 CHARACTER(LEN=1) :: YDIR
 CHARACTER(LEN=3)  :: YAREA          ! Area where field is to be averaged
 CHARACTER(LEN=5)  :: YSURF          ! Type of surface
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=12) :: YRECFM0        ! Name of the article to be read
 CHARACTER(LEN=12) :: YRECFM1        ! Name of the article to be read
 CHARACTER(LEN=12) :: YRECFM2        ! Name of the article to be read
 CHARACTER(LEN=12) :: YRECFM3        ! Name of the article to be read
INTEGER :: IRESP          ! reading return code
INTEGER :: ILAYER                   ! number of surface layers
INTEGER :: JLAYER                   ! loop counter on surface layers
INTEGER :: IPAR_LAYER               ! number of data surface layers
INTEGER :: IDATA_LAYER              ! number of data surface layers from ecoclimap
INTEGER :: ILU                      ! number of points
LOGICAL           :: GECOCLIMAP, GECOSG
LOGICAL           :: GDATA, GDIM          ! T if depth is to be read in the file
LOGICAL           :: GREAD_EXT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*    2.      SECONDARY VARIABLES
!             -------------------
!
!*    2.2     fields on artificial surfaces only
!             ----------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_TEB_DEPTHS',0,ZHOOK_HANDLE)
!
YDIR = 'H'
IF (PRESENT(HDIR)) YDIR = HDIR
!
 CALL OPEN_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE,'FULL  ')
YRECFM='VERSION'
 CALL READ_SURF(HFILEPGDTYPE,YRECFM,IVERSION_PGD,IRESP,HDIR='-')
YRECFM='BUG'
 CALL READ_SURF(HFILEPGDTYPE,YRECFM,IBUGFIX_PGD,IRESP,HDIR='-')
 CALL READ_LECOCLIMAP(HFILEPGDTYPE,GECOCLIMAP,GECOSG,HDIR='-')
 CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
GDIM = (IVERSION_PGD>8 .OR. IVERSION_PGD==8 .AND. IBUGFIX_PGD>0)
!
 CALL OPEN_AUX_IO_SURF(HFILE,HFILETYPE,'FULL  ')
YRECFM='VERSION'
 CALL READ_SURF(HFILETYPE,YRECFM,IVERSION_PREP,IRESP,HDIR='-')
YRECFM='BUG'
 CALL READ_SURF(HFILETYPE,YRECFM,IBUGFIX_PREP,IRESP,HDIR='-')
 CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
!
IF (PRESENT(PD_ROOF)) THEN
  YSURF='ROOF '
  ZDATA = XDATA_D_ROOF
  YRECFM0 = 'PAR_RF_LAYER'
  YRECFM1 = 'L_D_ROOF'
  YRECFM2 = 'D_D_ROOF'
  YRECFM3 = 'D_ROOF'
  IDATA_LAYER = NDATA_ROOF_LAYER
  ILU     = SIZE(PD_ROOF,1)
  ILAYER  = SIZE(PD_ROOF,2)
  YAREA   = 'BLD'
END IF
IF (PRESENT(PD_WALL)) THEN
  YSURF='WALL '
  ZDATA = XDATA_D_WALL
  YRECFM0 = 'PAR_WL_LAYER'
  YRECFM1 = 'L_D_WALL'
  YRECFM2 = 'D_D_WALL'
  YRECFM3 = 'D_WALL'
  IDATA_LAYER = NDATA_WALL_LAYER
  ILU     = SIZE(PD_WALL,1)
  ILAYER  = SIZE(PD_WALL,2)
  YAREA   = 'BLD'
END IF
IF (PRESENT(PD_ROAD)) THEN
  YSURF='ROAD '
  ZDATA = XDATA_D_ROAD
  YRECFM0 = 'PAR_RD_LAYER'
  YRECFM1 = 'L_D_ROAD'
  YRECFM2 = 'D_D_ROAD'
  YRECFM3 = 'D_ROAD'
  IDATA_LAYER = NDATA_ROAD_LAYER
  ILU     = SIZE(PD_ROAD,1)
  ILAYER  = SIZE(PD_ROAD,2)
  YAREA   = 'STR'
END IF
IF (PRESENT(PD_FLOOR)) THEN
  YSURF='FLOOR'
  ZDATA = XDATA_D_FLOOR
  YRECFM0 = 'PAR_FL_LAYER'
  YRECFM1 = 'L_D_FLOOR'
  YRECFM2 = 'D_D_FLOOR'
  YRECFM3 = 'D_FLOOR'
  IDATA_LAYER = NDATA_FLOOR_LAYER
  ILU     = SIZE(PD_FLOOR,1)
  ILAYER  = SIZE(PD_FLOOR,2)
  YAREA   = 'BLD'
END IF
!
!* read if the depths description are written in the file
IF (IVERSION_PGD<7 .OR. (IVERSION_PGD==7 .AND. IBUGFIX_PGD<=2)) THEN
  GDATA = .FALSE.
ELSE
  !
  CALL OPEN_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE,'TOWN  ')
  !
  CALL READ_SURF(HFILEPGDTYPE,YRECFM1,GDATA,IRESP,HDIR='-')
  !* depths are read in the file
  IF (GDATA) THEN
    !* gets number of data layers
    IPAR_LAYER = 0
    CALL READ_SURF(HFILEPGDTYPE,YRECFM0,IPAR_LAYER,IRESP,HDIR='-')
    !* gets the data layers depths
    ALLOCATE(ZPAR_D(ILU,IPAR_LAYER))
    DO JLAYER=1,IPAR_LAYER
      WRITE(YRECFM,FMT='(A,I1)') TRIM(YRECFM2),JLAYER
      CALL READ_SURF(HFILEPGDTYPE,YRECFM,ZPAR_D(:,JLAYER),IRESP,HDIR=YDIR)
    END DO
  ENDIF
  !
  CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
  !
END IF
!
!* depths are read in the file
IF (.NOT.GDATA) THEN
  !
!* depths are deduced from the cover types
  ALLOCATE(ZD(ILU,ILAYER))
  !
  IF (IVERSION_PREP>8 .OR. (IVERSION_PREP==8 .AND. IBUGFIX_PREP>=1)) THEN
    CALL OPEN_AUX_IO_SURF(HFILE,HFILETYPE,'TOWN  ')
    CALL READ_SURF(HFILETYPE,'WRITE_EXT  ',GREAD_EXT,IRESP,HDIR='-')
    IF (GREAD_EXT) THEN
      DO JLAYER=1,ILAYER
        WRITE(YRECFM,FMT='(A,I1)') TRIM(YRECFM3),JLAYER
        CALL READ_SURF(HFILETYPE,YRECFM,ZD(:,JLAYER),IRESP,HDIR=YDIR)
      END DO
    ENDIF
    CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
  ELSE
    GREAD_EXT = .FALSE.
  ENDIF
  !
  IF (.NOT.GREAD_EXT) THEN
    !
    IF (GDIM.AND.GECOSG) THEN
      ALLOCATE(GCOVER(SUM(NTYPE)))
    ELSE
      ALLOCATE(GCOVER(NCOVER))
    ENDIF
    !
    CALL OPEN_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE,'FULL  ')
    !* reading of the cover to obtain the thickness of layers
    CALL OLD_NAME(HFILEPGDTYPE,'COVER_LIST      ',YRECFM,'-')
    CALL READ_SURF(HFILEPGDTYPE,YRECFM,GCOVER(:),IRESP,HDIR='-')
    !* reading of the cover fractions
    ALLOCATE(ZCOVER(ILU,COUNT(GCOVER)))
    YRECFM='COVER'
    CALL READ_SURF_COV(HFILEPGDTYPE,YRECFM,ZCOVER(:,:),GCOVER,IRESP,&
             HDIR=YDIR)
    CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
    !
    ALLOCATE(ZPAR_D(ILU,IDATA_LAYER))
    IF (NRANK==NPIO) THEN
      !* deduces the depths of each layer
      DO JLAYER=1,IDATA_LAYER
        CALL AV_PGD (DTCO,ZPAR_D(:,JLAYER), ZCOVER, ZDATA(:,JLAYER),YAREA,'ARI',GCOVER)
      END DO
    ENDIF
    DEALLOCATE(GCOVER,ZCOVER)
    !
    IF (IVERSION_PREP<7 .OR. (IVERSION_PREP==7 .AND. IBUGFIX_PREP<=2)) THEN
      !* ind version of TEB, the computational grid was equal to the data grid
      ZD(:,:) = ZPAR_D(:,:)
    ELSEIF (NRANK==NPIO) THEN
      !* recomputes the grid from the available data
      CALL THERMAL_LAYERS_CONF(YSURF,ZPAR_D,ZD)
    END IF
    DEALLOCATE(ZPAR_D)
    !
  ENDIF
  !
ENDIF
!
IF (PRESENT(PD_ROOF )) PD_ROOF  = ZD
IF (PRESENT(PD_WALL )) PD_WALL  = ZD
IF (PRESENT(PD_ROAD )) PD_ROAD  = ZD
IF (PRESENT(PD_FLOOR)) PD_FLOOR = ZD
!
DEALLOCATE(ZD)
!
IF (LHOOK) CALL DR_HOOK('GET_TEB_DEPTHS',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_TEB_DEPTHS
