!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_TEB_UNIF(KLUOUT,HSURF,PFIELD)
!     #################################################################################
!
!!****  *PREP_TEB_UNIF* - prepares TEB field from prescribed values
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!------------------------------------------------------------------
!
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_PREP,       ONLY : CINTERP_TYPE, XZS_LS
USE MODD_PREP_TEB,   ONLY : XGRID_ROAD, XGRID_WALL, XGRID_ROOF, XGRID_FLOOR,    &
                              XWS_ROOF, XWS_ROAD, XTS_ROAD, XTS_ROOF, XTS_WALL, &
                              XTI_BLD, XTI_ROAD, XT_CAN, XQ_CAN, XHUI_BLD
USE MODD_CSTS, ONLY : XG, XP00
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
USE MODE_THERMOS
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER,            INTENT(IN)  :: KLUOUT    ! output listing logical unit
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
REAL, POINTER, DIMENSION(:,:)   :: PFIELD    ! field to interpolate horizontally
!
!*      0.2    declarations of local variables
REAL, DIMENSION(:), ALLOCATABLE :: ZPS       ! surface pressure
REAL, DIMENSION(:), ALLOCATABLE :: ZTI_BLD   ! indoor building temperature
REAL, PARAMETER                 :: ZRHOA=1.19! air volumic mass at 20C and 1015hPa
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_TEB_UNIF',0,ZHOOK_HANDLE)
SELECT CASE(HSURF)
!
!*      3.0    Orography
!
  CASE('ZS     ')
    ALLOCATE(PFIELD(1,1))
    PFIELD = 0.
!
!*      3.1    Profile of temperatures in roads
!
  CASE('T_ROAD ')
    ALLOCATE(PFIELD(1,SIZE(XGRID_ROAD)))
    CALL PUT_UNIF_ON_REF_GRID('ROAD',XGRID_ROAD)

!*      3.2    Profile of temperatures in walls

  CASE('T_WALLA','T_WALLB')
    ALLOCATE(PFIELD(1,SIZE(XGRID_WALL)))
    CALL PUT_UNIF_ON_REF_GRID('WALL',XGRID_WALL)

!*      3.3    Profile of temperatures in roofs

  CASE('T_ROOF ')
    ALLOCATE(PFIELD(1,SIZE(XGRID_ROOF)))
    CALL PUT_UNIF_ON_REF_GRID('ROOF',XGRID_ROOF)

!*      3.4bis Profile of temperatures in floors

  CASE('T_FLOOR')
    ALLOCATE(PFIELD(1,SIZE(XGRID_FLOOR)))
    CALL PUT_UNIF_ON_REF_GRID('FLOO',XGRID_FLOOR)

  CASE('T_MASS')
    ALLOCATE(PFIELD(1,SIZE(XGRID_FLOOR)))
    CALL PUT_UNIF_ON_REF_GRID('MASS',XGRID_FLOOR)

!*      3.4    Other quantities

  CASE('WS_ROOF')
    ALLOCATE(PFIELD(1,1))
    PFIELD = XWS_ROOF

  CASE('WS_ROAD')
    ALLOCATE(PFIELD(1,1))
    PFIELD = XWS_ROAD

  CASE('TI_BLD  ')
    ALLOCATE(PFIELD(1,1))
    PFIELD = XTI_BLD

  CASE('QI_BLD  ')
    ALLOCATE(PFIELD(MAX(1,SIZE(XZS_LS)),1))
    ALLOCATE(ZPS(SIZE(XZS_LS)))
    ALLOCATE(ZTI_BLD(SIZE(XZS_LS)))
    ZPS = XP00 - ZRHOA*XG*XZS_LS
    ZTI_BLD = XTI_BLD
    PFIELD(:SIZE(XZS_LS),1) = XHUI_BLD * QSAT(ZTI_BLD, ZPS)
    DEALLOCATE(ZPS)
    DEALLOCATE(ZTI_BLD)

  CASE('T_WIN1  ')
    ALLOCATE(PFIELD(1,1))
    PFIELD = XTS_WALL

  CASE('T_WIN2  ')
    ALLOCATE(PFIELD(1,1))
    PFIELD = XTI_BLD

  CASE('TI_ROAD')
    ALLOCATE(PFIELD(1,1))
    PFIELD = XTI_ROAD

  CASE('T_CAN  ')
    ALLOCATE(PFIELD(1,1))
    PFIELD = XT_CAN

  CASE('Q_CAN  ')
    ALLOCATE(PFIELD(1,1))
    PFIELD = XQ_CAN

END SELECT
!
!*      4.     Interpolation method
!              --------------------
!
CINTERP_TYPE='UNIF  '
!
!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_TEB_UNIF',1,ZHOOK_HANDLE)
CONTAINS
!
!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
SUBROUTINE PUT_UNIF_ON_REF_GRID(HSURFTYPE,PGRID)
!-------------------------------------------------------------------------------------
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODI_INTERP_GRID
!
 CHARACTER(LEN=4),   INTENT(IN) :: HSURFTYPE ! surface type
REAL, DIMENSION(:), INTENT(IN) :: PGRID     ! reference grid
!
REAL               :: ZTS! surface temperature
REAL               :: ZTI! internal temperature
REAL, DIMENSION(1,2) :: ZT ! temperature profile
REAL, DIMENSION(1,2) :: ZD ! normalized depth profile
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------

!* get surface temperature

IF (LHOOK) CALL DR_HOOK('PUT_UNIF_ON_REF_GRID',0,ZHOOK_HANDLE)
SELECT CASE(HSURFTYPE)
  CASE('ROOF')
    ZTS = XTS_ROOF
  CASE('ROAD')
    ZTS = XTS_ROAD
  CASE('WALL')
    ZTS = XTS_WALL
  CASE('FLOO')
    ZTS = XTI_BLD
  CASE('MASS')
    ZTS = XTI_BLD
END SELECT

!* get deep road or building interior temperature

SELECT CASE(HSURFTYPE)
  CASE('ROOF', 'WALL', 'MASS')
    ZTI = XTI_BLD
  CASE('ROAD', 'FLOO')
    IF (XTI_ROAD/= XUNDEF) THEN
      ZTI = XTI_ROAD
    ELSE
      WRITE(KLUOUT,*) 'Error in PREParation of TEB fields'
      WRITE(KLUOUT,*) 'When Road Surface Temperature is prescribed,'
      WRITE(KLUOUT,*) 'Deep Road Temperature XTI_ROAD must also be prescribed'
      CALL ABOR1_SFX('PREP_TEB_UNIF: XTI_ROAD MUST BE PRESCRIBED')
    END IF
END SELECT

!* group all this information in one profile

ZT(1,1) = ZTS
ZT(1,2) = ZTI

ZD(1,1) = 0.
ZD(1,2) = 1.

!* interpolate this field on the required grid
!
 CALL INTERP_GRID(ZD,ZT,PGRID,PFIELD)
IF (LHOOK) CALL DR_HOOK('PUT_UNIF_ON_REF_GRID',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PUT_UNIF_ON_REF_GRID
!
!-------------------------------------------------------------------------------------
END SUBROUTINE PREP_TEB_UNIF
