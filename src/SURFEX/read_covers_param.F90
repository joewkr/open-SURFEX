!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ############################
      SUBROUTINE READ_COVERS_PARAM(KFILE)
!     ############################
!
!!**** *READ_COVERS_PARAM* initializes cover-field correspondance arrays
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
!!    S.Faroux        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    23/03/11
!!
!!    R. Alkama    05/2012 : read 19 vegtypes rather than 12
!     10/2014 : add status='old' for ecoclimap.bin files E. Martin
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------

USE MODD_TYPE_DATE_SURF
!
USE MODD_DATA_COVER,     ONLY : XDATA_TOWN, XDATA_NATURE, XDATA_SEA, XDATA_WATER,   &
                                  XDATA_VEGTYPE, XDATA_LAI, XDATA_H_TREE,           &
                                  XDATA_ROOT_DEPTH, XDATA_GROUND_DEPTH, XDATA_DICE, &
                                  XDATA_LAI_ALL_YEARS, TDATA_SEED, TDATA_REAP,      &
                                  XDATA_ALB_SOIL_NIR, XDATA_ALB_SOIL_VIS,           &
                                  XDATA_ALB_VEG_NIR, XDATA_ALB_VEG_VIS,             &                                  
                                  XDATA_WATSUP, XDATA_IRRIG,                        &
                                  XDATA_Z0_TOWN, XDATA_BLD_HEIGHT, XDATA_WALL_O_HOR,&
                                  XDATA_BLD, XDATA_GARDEN,                          &
                                  XDATA_ALB_ROOF, XDATA_ALB_ROAD, XDATA_ALB_WALL,   &
                                  XDATA_EMIS_ROOF, XDATA_EMIS_ROAD, XDATA_EMIS_WALL,&
                                  XDATA_HC_ROOF, XDATA_HC_ROAD, XDATA_HC_WALL,      &
                                  XDATA_TC_ROOF, XDATA_TC_ROAD, XDATA_TC_WALL,      &
                                  XDATA_D_ROOF, XDATA_D_ROAD, XDATA_D_WALL,         &
                                  XDATA_H_TRAFFIC, XDATA_LE_TRAFFIC,                &
                                  XDATA_H_INDUSTRY, XDATA_LE_INDUSTRY,              &
                                  NECO2_START_YEAR, NECO2_END_YEAR
!
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE, JPCOVER, NCOVER_ECO1_END, NCOVER_ECO2_START
!
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
INTEGER, INTENT(IN)   :: KFILE
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER               :: IERR_OPEN
INTEGER               :: INB_COVER, INB_AN
INTEGER               :: ICOVER, IREC
INTEGER               :: JCOVER
!
!*    0.3    Declaration of namelists
!            ------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('READ_COVERS_PARAM',0,ZHOOK_HANDLE)
!
!opening of the file
IF (KFILE==1) THEN
  OPEN(41,FILE='ecoclimapI_covers_param.bin',FORM='UNFORMATTED',ACCESS='DIRECT', &
          recl=20*8,STATUS='OLD',IOSTAT=IERR_OPEN)  
  IF (IERR_OPEN /= 0 ) THEN
          CALL ABOR1_SFX ('ERROR WHILE OPENING ''ecoclimapI_covers_param.bin'' THIS FILE IS NEEDED AND MUST BE'// &
                  ' IN (OR LINKED TO) THE RUN DIRECTORY')
  ENDIF        
  INB_COVER = NCOVER_ECO1_END
  INB_AN = 1
ELSEIF (KFILE==2) THEN
  OPEN(41,FILE='ecoclimapII_eu_covers_param.bin',FORM='UNFORMATTED',ACCESS='DIRECT',  &
          recl=20*8,STATUS='OLD',IOSTAT=IERR_OPEN)
  IF (IERR_OPEN /= 0 ) THEN
          CALL ABOR1_SFX ('ERROR WHILE OPENING ''ecoclimapII_eu_covers_param.bin'' THIS FILE IS NEEDED AND MUST BE'// &
                  ' IN (OR LINKED TO) THE RUN DIRECTORY')
  ENDIF
  INB_COVER = JPCOVER - NCOVER_ECO2_START + 1
  INB_AN = NECO2_END_YEAR - NECO2_START_YEAR + 1
ENDIF
!
IREC=0
DO JCOVER = 1,INB_COVER
  IREC = IREC+1
  READ(41,REC=IREC) ICOVER
  !fractions of tiles
  IREC=IREC+1
  READ(41,REC=IREC) XDATA_TOWN(ICOVER),XDATA_NATURE(ICOVER),XDATA_WATER(ICOVER),XDATA_SEA(ICOVER)
  !natural part
  IF (XDATA_NATURE(ICOVER).NE.0.) CALL READ_NATURE
  !urban part
  IF (XDATA_TOWN(ICOVER).NE.0.) THEN
    !main town parameters
    IREC=IREC+1
    READ(41,REC=IREC) XDATA_Z0_TOWN(ICOVER),XDATA_BLD_HEIGHT(ICOVER),XDATA_WALL_O_HOR(ICOVER),&
        XDATA_BLD(ICOVER),XDATA_GARDEN(ICOVER)
    !town albedos
    IREC=IREC+1
    READ(41,rec=IREC) XDATA_ALB_ROOF(ICOVER),XDATA_ALB_ROAD(ICOVER),XDATA_ALB_WALL(ICOVER)
    !town emissivities
    IREC=IREC+1
    READ(41,rec=IREC) XDATA_EMIS_ROOF(ICOVER),XDATA_EMIS_ROAD(ICOVER),XDATA_EMIS_WALL(ICOVER)
    !town heat capacity
    IREC=IREC+1
    READ(41,rec=IREC) XDATA_HC_ROOF(ICOVER,:)
    IREC=IREC+1
    READ(41,rec=IREC) XDATA_HC_ROAD(ICOVER,:)
    IREC=IREC+1
    READ(41,rec=IREC) XDATA_HC_WALL(ICOVER,:)
    !town thermal conductivity
    IREC=IREC+1
    READ(41,rec=IREC) XDATA_TC_ROOF(ICOVER,:)
    IREC=IREC+1
    READ(41,rec=IREC) XDATA_TC_ROAD(ICOVER,:)
    IREC=IREC+1
    READ(41,rec=IREC) XDATA_TC_WALL(ICOVER,:)
    !town depths
    IREC=IREC+1
    READ(41,rec=IREC) XDATA_D_ROOF(ICOVER,:)
    IREC=IREC+1
    READ(41,rec=IREC) XDATA_D_ROAD(ICOVER,:)
    IREC=IREC+1
    READ(41,rec=IREC) XDATA_D_WALL(ICOVER,:)
    !traffic and industry fluxes
    IREC=IREC+1
    READ(41,rec=IREC) XDATA_H_TRAFFIC(ICOVER),XDATA_LE_TRAFFIC(ICOVER),XDATA_H_INDUSTRY(ICOVER),XDATA_LE_INDUSTRY(ICOVER)
    IF (XDATA_GARDEN(ICOVER).NE.0. .AND. XDATA_NATURE(ICOVER).EQ.0.) CALL READ_NATURE
  ENDIF
ENDDO
CLOSE(41)
!
IF (LHOOK) CALL DR_HOOK('READ_COVERS_PARAM',1,ZHOOK_HANDLE)
!------------------------------------------------------------------------------
CONTAINS
!
SUBROUTINE READ_NATURE
!
REAL, DIMENSION(12) :: ZINTER
INTEGER               :: JVEGTYPE, JLAI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('READ_COVERS_PARAM:READ_NATURE',0,ZHOOK_HANDLE)
!
!fractions of vegtypes
IREC=IREC+1
READ(41,REC=IREC) XDATA_VEGTYPE(ICOVER,:)
!
!albedos for the soil
IF (KFILE<=2 .AND. XDATA_NATURE(ICOVER)/=0.) THEN
  IREC=IREC+1
  READ(41,REC=IREC) ZINTER(:)
  XDATA_ALB_SOIL_NIR(ICOVER,1:12,1) = ZINTER(:)
  IREC=IREC+1
  READ(41,REC=IREC) ZINTER(:)
  XDATA_ALB_SOIL_NIR(ICOVER,13:24,1) = ZINTER(:)
  IREC=IREC+1
  READ(41,REC=IREC) ZINTER(:) 
  XDATA_ALB_SOIL_NIR(ICOVER,25:36,1) = ZINTER(:)
  IREC=IREC+1
  READ(41,REC=IREC) ZINTER(:)
  XDATA_ALB_SOIL_VIS(ICOVER,1:12,1) = ZINTER(:)
  IREC=IREC+1
  READ(41,REC=IREC) ZINTER(:) 
  XDATA_ALB_SOIL_VIS(ICOVER,13:24,1) = ZINTER(:)
  IREC=IREC+1
  READ(41,REC=IREC) ZINTER(:) 
  XDATA_ALB_SOIL_VIS(ICOVER,25:36,1) = ZINTER(:)
ENDIF
!
DO JVEGTYPE=1,NVEGTYPE
  !not null fraction of vegtype
  IF (XDATA_VEGTYPE(ICOVER,JVEGTYPE).NE.0.) THEN
    !root and soil depths
    IREC=IREC+1      
    READ(41,REC=IREC) XDATA_ROOT_DEPTH(ICOVER,JVEGTYPE), XDATA_GROUND_DEPTH(ICOVER,JVEGTYPE), XDATA_DICE(ICOVER,JVEGTYPE)
    IF (JVEGTYPE.GT.3) THEN
      !LAI
      DO JLAI=1,INB_AN*3
        IREC=IREC+1
        IF (KFILE==1) THEN
          READ(41,REC=IREC) ZINTER(:)
          XDATA_LAI(ICOVER,(JLAI-1)*12+1:JLAI*12,JVEGTYPE) = ZINTER(:)
        ELSEIF (KFILE==2) THEN
          READ(41,REC=IREC) ZINTER(:)
          XDATA_LAI_ALL_YEARS(ICOVER,(JLAI-1)*12+1:JLAI*12,JVEGTYPE) = ZINTER(:)
        ENDIF
      ENDDO
      !Heights of trees
      IF ((JVEGTYPE < 7) .OR. (JVEGTYPE > 12 .AND. JVEGTYPE /= 18)) THEN
        IREC=IREC+1
        READ(41,REC=IREC) XDATA_H_TREE(ICOVER,JVEGTYPE)
      ENDIF
      !albedos for the vegetation
      IF (KFILE<=2 .AND. XDATA_NATURE(ICOVER)/=0.) THEN
        IREC=IREC+1
        READ(41,REC=IREC) ZINTER(:) 
        XDATA_ALB_VEG_NIR(ICOVER,1:12,JVEGTYPE) = ZINTER(:)
        IREC=IREC+1
        READ(41,REC=IREC) ZINTER(:) 
        XDATA_ALB_VEG_NIR(ICOVER,13:24,JVEGTYPE) = ZINTER(:)
        IREC=IREC+1
        READ(41,REC=IREC) ZINTER(:) 
        XDATA_ALB_VEG_NIR(ICOVER,25:36,JVEGTYPE) = ZINTER(:)
        IREC=IREC+1
        READ(41,REC=IREC) ZINTER(:) 
        XDATA_ALB_VEG_VIS(ICOVER,1:12,JVEGTYPE) = ZINTER(:)
        IREC=IREC+1
        READ(41,REC=IREC) ZINTER(:) 
        XDATA_ALB_VEG_VIS(ICOVER,13:24,JVEGTYPE) = ZINTER(:)
        IREC=IREC+1
        READ(41,REC=IREC) ZINTER(:) 
        XDATA_ALB_VEG_VIS(ICOVER,25:36,JVEGTYPE) = ZINTER(:)
      ENDIF
    ELSE
      !LAI for bare areas
      IF (KFILE==1) THEN
        XDATA_LAI(ICOVER,:,JVEGTYPE) = 0.
      ELSEIF (KFILE==2) THEN
        XDATA_LAI_ALL_YEARS(ICOVER,:,JVEGTYPE) = 0.
      ENDIF
      XDATA_ALB_VEG_NIR(ICOVER,:,JVEGTYPE) = 0.3
      XDATA_ALB_VEG_VIS(ICOVER,:,JVEGTYPE) = 0.1
    ENDIF
    !irrigation
    IF (JVEGTYPE.EQ.8 .AND. KFILE.EQ.1 .OR. JVEGTYPE.EQ.9 .AND. KFILE.EQ.2) THEN
      IREC=IREC+1
      READ(41,REC=IREC) TDATA_SEED(ICOVER,JVEGTYPE)%TDATE%MONTH, TDATA_SEED(ICOVER,JVEGTYPE)%TDATE%DAY, &
        TDATA_REAP(ICOVER,JVEGTYPE)%TDATE%MONTH, TDATA_REAP(ICOVER,JVEGTYPE)%TDATE%DAY, &
        XDATA_WATSUP(ICOVER,JVEGTYPE),XDATA_IRRIG(ICOVER,JVEGTYPE)
    ENDIF
  ENDIF
ENDDO
!
IF (LHOOK) CALL DR_HOOK('READ_COVERS_PARAM:READ_NATURE',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_NATURE
!
END SUBROUTINE READ_COVERS_PARAM
