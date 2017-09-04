!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########################
      SUBROUTINE ARRANGE_COVER (DTCO, OWATER_TO_NATURE, OTOWN_TO_ROCK, &
                                PDATA_NATURE,PDATA_TOWN,PDATA_SEA,PDATA_WATER,PDATA_VEGTYPE, &
                               PDATA_GARDEN, OGARDEN, PDATA_BLD, PDATA_WALL_O_HOR           )
!     #########################
!
!!**** *ARRANGE_COVER*
!!
!!    PURPOSE
!!    -------
!!
!!    change water and intertidal (not lake) to nature and/or town to rock : arrange cover properly
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
!!    B. Decharme        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    03/2009
!!                04/2013 (V. Masson) Fusion of Arrange_cover & update_data_frac
!!                        to allow distinct cover change options between submodels (_n)
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
!
USE MODD_DATA_COVER,     ONLY : XDATA_ROOT_DEPTH, XDATA_GROUND_DEPTH, XDATA_DICE, &
                                XDATA_LAI, XDATA_LAI_ALL_YEARS,                   &
                                XDATA_ALB_VEG_NIR, XDATA_ALB_VEG_VIS,             &
                                XDATA_ALB_SOIL_NIR, XDATA_ALB_SOIL_VIS
!
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE, JPCOVER, NROCK, NVT_ROCK
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
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
!
LOGICAL, INTENT(IN) :: OWATER_TO_NATURE
LOGICAL, INTENT(IN) :: OTOWN_TO_ROCK
REAL, DIMENSION(:), INTENT(IN)  :: PDATA_NATURE
REAL, DIMENSION(:), INTENT(IN)  :: PDATA_TOWN
REAL, DIMENSION(:), INTENT(IN)  :: PDATA_SEA
REAL, DIMENSION(:), INTENT(IN)  :: PDATA_WATER
REAL, DIMENSION(:), INTENT(IN)  :: PDATA_GARDEN
REAL, DIMENSION(:,:),INTENT(IN) :: PDATA_VEGTYPE
LOGICAL,            INTENT(IN)  :: OGARDEN
REAL, DIMENSION(:), INTENT(IN)  :: PDATA_BLD
REAL, DIMENSION(:), INTENT(IN)  :: PDATA_WALL_O_HOR
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
REAL     :: ZWORK
!
INTEGER  :: JCOVER, JVEGTYPE, JL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('ARRANGE_COVER',0,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
! Allocate fields from submodel module
!-------------------------------------------------------------------------------
!
IF (ASSOCIATED(DTCO%XDATA_NATURE)) DEALLOCATE(DTCO%XDATA_NATURE)
IF (ASSOCIATED(DTCO%XDATA_TOWN)) DEALLOCATE(DTCO%XDATA_TOWN)
IF (ASSOCIATED(DTCO%XDATA_SEA)) DEALLOCATE(DTCO%XDATA_SEA)
IF (ASSOCIATED(DTCO%XDATA_WATER)) DEALLOCATE(DTCO%XDATA_WATER)
IF (ASSOCIATED(DTCO%XDATA_VEGTYPE)) DEALLOCATE(DTCO%XDATA_VEGTYPE)
IF (ASSOCIATED(DTCO%XDATA_GARDEN)) DEALLOCATE(DTCO%XDATA_GARDEN)
IF (ASSOCIATED(DTCO%XDATA_BLD)) DEALLOCATE(DTCO%XDATA_BLD)
IF (ASSOCIATED(DTCO%XDATA_WALL_O_HOR)) DEALLOCATE(DTCO%XDATA_WALL_O_HOR)
!
ALLOCATE(DTCO%XDATA_NATURE    (JPCOVER))
ALLOCATE(DTCO%XDATA_TOWN      (JPCOVER))
ALLOCATE(DTCO%XDATA_SEA       (JPCOVER))
ALLOCATE(DTCO%XDATA_WATER     (JPCOVER))
ALLOCATE(DTCO%XDATA_VEGTYPE   (JPCOVER,NVEGTYPE))
ALLOCATE(DTCO%XDATA_GARDEN    (JPCOVER))
ALLOCATE(DTCO%XDATA_BLD       (JPCOVER))
ALLOCATE(DTCO%XDATA_WALL_O_HOR(JPCOVER))
!
!-------------------------------------------------------------------------------
! Default values
!-------------------------------------------------------------------------------
!
DTCO%LGARDEN = OGARDEN
!
DTCO%XDATA_NATURE     = PDATA_NATURE
DTCO%XDATA_TOWN       = PDATA_TOWN
DTCO%XDATA_SEA        = PDATA_SEA
DTCO%XDATA_WATER      = PDATA_WATER
DTCO%XDATA_VEGTYPE    = PDATA_VEGTYPE
DTCO%XDATA_GARDEN     = PDATA_GARDEN
DTCO%XDATA_BLD        = PDATA_BLD
DTCO%XDATA_WALL_O_HOR = PDATA_WALL_O_HOR
!
!-------------------------------------------------------------------------------
! Change water (not lake) to nature
!-------------------------------------------------------------------------------
!
IF(OWATER_TO_NATURE)THEN
  DO JCOVER=1,JPCOVER
     IF(DTCO%XDATA_WATER(JCOVER)>0.0.AND.DTCO%XDATA_WATER(JCOVER)<1.0)THEN
       DTCO%XDATA_NATURE(JCOVER)=DTCO%XDATA_NATURE(JCOVER)+DTCO%XDATA_WATER(JCOVER)
       DTCO%XDATA_WATER (JCOVER)=0.0
     ENDIF
  ENDDO
ENDIF
!
!-------------------------------------------------------------------------------
! Change town to rock but keep other natural fraction
!-------------------------------------------------------------------------------
!
IF(OTOWN_TO_ROCK)THEN
!
  DO JCOVER=1,JPCOVER
     IF(DTCO%XDATA_TOWN(JCOVER)>0.0.OR.DTCO%XDATA_GARDEN(JCOVER)>0.0)THEN
!
       DTCO%XDATA_NATURE(JCOVER) = DTCO%XDATA_NATURE(JCOVER) + DTCO%XDATA_GARDEN(JCOVER) * DTCO%XDATA_TOWN(JCOVER)
       DTCO%XDATA_TOWN  (JCOVER) = DTCO%XDATA_TOWN  (JCOVER) * ( 1. - DTCO%XDATA_GARDEN(JCOVER))
       DTCO%XDATA_GARDEN(JCOVER) = 0.0
!
       ZWORK=DTCO%XDATA_NATURE(JCOVER)+DTCO%XDATA_TOWN(JCOVER)
!
       DO JVEGTYPE=1,NVEGTYPE
             DTCO%XDATA_VEGTYPE(JCOVER,JVEGTYPE)=DTCO%XDATA_VEGTYPE(JCOVER,JVEGTYPE)*DTCO%XDATA_NATURE(JCOVER)/ZWORK
       ENDDO
!
       DTCO%XDATA_VEGTYPE(JCOVER,NVT_ROCK) = DTCO%XDATA_VEGTYPE(JCOVER,NVT_ROCK)+DTCO%XDATA_TOWN(JCOVER)/ZWORK
!
       DTCO%XDATA_NATURE(JCOVER)=DTCO%XDATA_NATURE(JCOVER)+DTCO%XDATA_TOWN(JCOVER)
!
       DTCO%XDATA_TOWN  (JCOVER)=0.0
!
!      Initialise some variables
       XDATA_LAI          (JCOVER,:,NVT_ROCK) = 0.0
       XDATA_LAI_ALL_YEARS(JCOVER,:,NVT_ROCK) = 0.0
       XDATA_ROOT_DEPTH   (JCOVER,  NVT_ROCK) = 0.2
       XDATA_GROUND_DEPTH (JCOVER,  NVT_ROCK) = 0.2
       XDATA_DICE         (JCOVER,  NVT_ROCK) = 0.2
       XDATA_ALB_VEG_NIR  (JCOVER,:,NVT_ROCK) = 0.3
       XDATA_ALB_VEG_VIS  (JCOVER,:,NVT_ROCK) = 0.1
       XDATA_ALB_SOIL_NIR (JCOVER,:,NVT_ROCK) = 0.3
       XDATA_ALB_SOIL_VIS (JCOVER,:,NVT_ROCK) = 0.1
!
     ENDIF
  ENDDO
!
ELSE
!-------------------------------------------------------------------------------
! Town is kept, but if gardens are not treated specifically,
! they are included into nature fraction.
!-------------------------------------------------------------------------------
!
  IF (.NOT. OGARDEN) THEN
    DTCO%XDATA_NATURE     = DTCO%XDATA_NATURE + DTCO%XDATA_GARDEN * DTCO%XDATA_TOWN
    DTCO%XDATA_TOWN       = DTCO%XDATA_TOWN   * ( 1. - DTCO%XDATA_GARDEN)
    DTCO%XDATA_GARDEN     = 0.
    DTCO%XDATA_BLD        = DTCO%XDATA_BLD / (1. - DTCO%XDATA_GARDEN)
    DTCO%XDATA_WALL_O_HOR = DTCO%XDATA_WALL_O_HOR / (1. - DTCO%XDATA_GARDEN)
  END IF
!
ENDIF
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ARRANGE_COVER',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE ARRANGE_COVER
