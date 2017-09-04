!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_TEB_BUFFER(HPROGRAM,HSURF,KLUOUT,PFIELD)
!     #################################################################################
!
!!****  *PREP_TEB_BUFFER* - prepares TEB field from operational BUFFER
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
!!     S. Malardel
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    03/2005
!!------------------------------------------------------------------
!

!
USE MODD_TYPE_DATE_SURF
!
USE MODI_PREP_BUFFER_GRID
USE MODE_READ_BUFFER
USE MODI_INTERP_GRID
!
USE MODD_PREP,       ONLY : CINTERP_TYPE
USE MODD_GRID_BUFFER,  ONLY : NNI
USE MODD_PREP_TEB,   ONLY : XGRID_ROAD, XGRID_WALL, XGRID_ROOF, XGRID_FLOOR, &
                            XTI_BLD, XTI_ROAD, XHUI_BLD, XTI_BLD_DEF
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
REAL,DIMENSION(:,:), POINTER    :: PFIELD    ! field to interpolate horizontally
!
!*      0.2    declarations of local variables
!
TYPE (DATE_TIME)                :: TZTIME_BUF    ! current date and time
 CHARACTER(LEN=6)                :: YINMODEL ! model from which BUFFER originates
REAL, DIMENSION(:),   POINTER   :: ZFIELD1D ! 1D field read
REAL, DIMENSION(:,:), POINTER   :: ZFIELD   ! field read
REAL, DIMENSION(:,:), POINTER   :: ZD             ! depth of field in the soil
REAL                            :: ZTI_BLD  ! internal building temperature
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
!*      1.     Reading of grid
!              ---------------
!
IF (LHOOK) CALL DR_HOOK('PREP_TEB_BUFFER',0,ZHOOK_HANDLE)
 CALL PREP_BUFFER_GRID(KLUOUT,YINMODEL,TZTIME_BUF)
!
IF (HSURF=='T_FLOOR' .OR. HSURF=='T_WALL' .OR. HSURF=='T_ROOF' .OR.  HSURF=='T_WIN2' .OR. HSURF=='TI_BLD') THEN
   ZTI_BLD = XTI_BLD_DEF
   IF (XTI_BLD/=XUNDEF) ZTI_BLD=XTI_BLD
ENDIF
!---------------------------------------------------------------------------------------
SELECT CASE(HSURF)
!---------------------------------------------------------------------------------------
!
!*     2.      Orography
!              ---------
!
  CASE('ZS     ')
    SELECT CASE (YINMODEL)
      CASE ('ALADIN')
        CALL READ_BUFFER_ZS(KLUOUT,YINMODEL,ZFIELD1D)
        ALLOCATE(PFIELD(NNI,1))
        PFIELD(:,1) = ZFIELD1D(:)
        DEALLOCATE(ZFIELD1D)
    END SELECT
!
!*      3.     Profile of temperatures in roads
!              --------------------------------
!
  CASE('T_ROAD')
     !* reading of the profile and its depth definition
     SELECT CASE(YINMODEL)
       CASE('ALADIN')
         CALL READ_BUFFER_TG(KLUOUT,YINMODEL,ZFIELD,ZD)
     END SELECT
     !* if deep road temperature is prescribed
     IF (XTI_ROAD/=XUNDEF) THEN
       ZFIELD(:,2:) = XTI_ROAD
     END IF
     CALL TEB_PROFILE_BUFFER(XGRID_ROAD)
!
!*      3.bis     Profile of temperatures in floors
!                 --------------------------------

  CASE('T_FLOOR')
     !* reading of the profile and its depth definition
     SELECT CASE(YINMODEL)
       CASE('ALADIN')
         CALL READ_BUFFER_TF_TEB(KLUOUT,YINMODEL,ZTI_BLD,ZFIELD,ZD)
     END SELECT
     !* if deep road temperature is prescribed
     IF (XTI_ROAD/=XUNDEF) THEN
       ZFIELD(:,2:) = XTI_ROAD
     END IF
     CALL TEB_PROFILE_BUFFER(XGRID_FLOOR)

!*      4.     Profile of temperatures in walls
!              --------------------------------

  CASE('T_WALLA','T_WALLB')
     CALL READ_BUFFER_T_TEB(KLUOUT,YINMODEL,ZTI_BLD,ZFIELD,ZD)
     CALL TEB_PROFILE_BUFFER(XGRID_WALL)

  CASE('T_WIN1')
    SELECT CASE (YINMODEL)
      CASE ('ALADIN')
        CALL READ_BUFFER_TS(KLUOUT,YINMODEL,ZFIELD1D)
        ALLOCATE(PFIELD(NNI,1))
        PFIELD(:,1) = ZFIELD1D(:)
        DEALLOCATE(ZFIELD1D)
    END SELECT

!*      5.     Profile of temperatures in roofs
!              --------------------------------

  CASE('T_ROOF')
     CALL READ_BUFFER_T_TEB(KLUOUT,YINMODEL,ZTI_BLD,ZFIELD,ZD)
     CALL TEB_PROFILE_BUFFER(XGRID_ROOF)

!*      5.bis    Profile of temperatures in thermal mass
!              -----------------------------------------
!
  CASE('T_MASS')
     ALLOCATE(PFIELD(NNI,3))
     PFIELD(:,:) = ZTI_BLD
     CALL TEB_PROFILE_BUFFER(XGRID_FLOOR)
!
!*      6.     Canyon air temperature
!              ----------------------
!
  CASE('T_CAN  ')
    SELECT CASE (YINMODEL)
      CASE ('ALADIN')
        CALL READ_BUFFER_T2(KLUOUT,YINMODEL,ZFIELD1D)
        ALLOCATE(PFIELD(NNI,1))
        PFIELD(:,1) = ZFIELD1D(:)
        DEALLOCATE(ZFIELD1D)
    END SELECT
!
!*      7.      Canyon air humidity
!               -------------------
!
  CASE('Q_CAN  ')
    SELECT CASE (YINMODEL)
      CASE ('ALADIN')
        ALLOCATE(PFIELD(NNI,1))
        PFIELD(:,1) = 0.01
    END SELECT

!
!*      9.     Deep road temperature
!              ---------------------

  CASE('TI_ROAD')
     IF (XTI_ROAD==XUNDEF) THEN
       CALL READ_BUFFER_T2(KLUOUT,YINMODEL,ZFIELD1D)
       ALLOCATE(PFIELD(NNI,1))
       PFIELD(:,1) = ZFIELD1D(:)
       DEALLOCATE(ZFIELD1D)
     ELSE
       ALLOCATE(PFIELD(NNI,1))
       PFIELD = XTI_ROAD
     END IF


!*      9.     Building temperatures/moisture
!              --------------------

  CASE('TI_BLD ')
     ALLOCATE(PFIELD(NNI,1))
     PFIELD(:,:) = ZTI_BLD
!
  CASE('T_WIN2')
     ALLOCATE(PFIELD(NNI,1))
     PFIELD(:,:) = ZTI_BLD

  CASE('QI_BLD ')
     ALLOCATE(PFIELD(NNI,1))
     PFIELD(:,1) = XUNDEF

!*     10.     Other quantities (water reservoirs)
!              ----------------

  CASE DEFAULT
    ALLOCATE(PFIELD(NNI,1))
    PFIELD = 0.

END SELECT
!
!*      4.     Interpolation method
!              --------------------
!
CINTERP_TYPE='BUFFER'
!
!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_TEB_BUFFER',1,ZHOOK_HANDLE)
CONTAINS
!
!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
SUBROUTINE TEB_PROFILE_BUFFER(PGRID)
!-------------------------------------------------------------------------------------
!
REAL, DIMENSION(:),   INTENT(IN)  :: PGRID  ! destination grid
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
!* interpolation on fine vertical grid
IF (LHOOK) CALL DR_HOOK('TEB_PROFILE_BUFFER',0,ZHOOK_HANDLE)
ALLOCATE(PFIELD(SIZE(ZFIELD,1),SIZE(PGRID)))
 CALL INTERP_GRID(ZD,ZFIELD,PGRID,PFIELD)
!
!* end
DEALLOCATE(ZFIELD)
DEALLOCATE(ZD)
IF (LHOOK) CALL DR_HOOK('TEB_PROFILE_BUFFER',1,ZHOOK_HANDLE)

END SUBROUTINE TEB_PROFILE_BUFFER
!
!-------------------------------------------------------------------------------------
END SUBROUTINE PREP_TEB_BUFFER
