!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_TEB_GRIB(HPROGRAM,HSURF,HFILE,KLUOUT,PFIELD)
!     #################################################################################
!
!!****  *PREP_TEB_GRIB* - prepares TEB field from operational GRIB
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
USE MODD_TYPE_DATE_SURF
!
USE MODE_READ_GRIB
USE MODI_INTERP_GRID
!
USE MODD_GRID_GRIB,  ONLY : CGRIB_FILE, NNI, CINMODEL
USE MODD_PREP_TEB,   ONLY : XGRID_ROAD, XGRID_WALL, XGRID_ROOF, XGRID_FLOOR, &
                            XTI_BLD, XTI_ROAD, XHUI_BLD, XTI_BLD_DEF,        &
                            XHUI_BLD_DEF
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
!
!USE YOMHOOK ,ONLY : LHOOK, DR_HOOK
!USE PARKIND1 ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILE     ! name of file
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
REAL,DIMENSION(:,:), POINTER    :: PFIELD    ! field to interpolate horizontally
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(:)  , POINTER   :: ZMASK => NULL()          ! Land mask
REAL, DIMENSION(:),   POINTER   :: ZFIELD1D => NULL() ! 1D field read
REAL, DIMENSION(:,:), POINTER   :: ZFIELD => NULL()   ! field read
REAL, DIMENSION(:,:), POINTER   :: ZD => NULL()             ! depth of field in the soil
REAL                            :: ZTI_BLD !indoor air temperature
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
!*      1.     Reading of grid
!              ---------------
!
!IF (LHOOK) CALL DR_HOOK('PREP_TEB_GRIB',0,ZHOOK_HANDLE)
!
IF (TRIM(HFILE).NE.CGRIB_FILE) CGRIB_FILE=""
!
 CALL READ_GRIB_LAND_MASK(HFILE,KLUOUT,CINMODEL,ZMASK)
!
IF (HSURF=='T_FLOOR' .OR. HSURF(1:6)=='T_WALL' .OR. HSURF=='T_ROOF' .OR.  &
    HSURF=='T_WIN2' .OR. HSURF=='TI_BLD' .OR. HSURF=='T_MASS') THEN
  ZTI_BLD = XTI_BLD_DEF
  IF (XTI_BLD/=XUNDEF) ZTI_BLD=XTI_BLD
ENDIF
!
!---------------------------------------------------------------------------------------
SELECT CASE(HSURF)
!---------------------------------------------------------------------------------------
!
!*     2.      Orography
!              ---------
!
  CASE('ZS     ')
    SELECT CASE (CINMODEL)
      CASE ('ECMWF ','ARPEGE','ALADIN','MOCAGE','HIRLAM')
        CALL READ_GRIB_ZS_LAND(HFILE,KLUOUT,CINMODEL,ZMASK,ZFIELD1D)
        ALLOCATE(PFIELD(SIZE(ZFIELD1D),1))
        PFIELD(:,1) = ZFIELD1D(:)
        DEALLOCATE(ZFIELD1D)
    END SELECT
!
!*      3.     Profile of temperatures in roads
!              --------------------------------
!
  CASE('T_ROAD')
     !* reading of the profile and its depth definition
     SELECT CASE(CINMODEL)
       CASE('ECMWF ')
         CALL READ_GRIB_TG_ECMWF(HFILE,KLUOUT,CINMODEL,ZMASK,ZFIELD,ZD)
       CASE('ARPEGE','ALADIN','MOCAGE')
         CALL READ_GRIB_TG_METEO_FRANCE(HFILE,KLUOUT,CINMODEL,ZMASK,ZFIELD,ZD)
       CASE('HIRLAM')
         CALL READ_GRIB_TG_HIRLAM(HFILE,KLUOUT,CINMODEL,ZMASK,ZFIELD,ZD)           
     END SELECT
     !* if deep road temperature is prescribed
     IF (XTI_ROAD/=XUNDEF) THEN
       ZFIELD(:,2:) = XTI_ROAD 
     END IF
     CALL TEB_PROFILE_GRIB(XGRID_ROAD)
!
!*      3.bis  Profile of temperatures in floors
!              --------------------------------

  CASE('T_FLOOR')    
     !* reading of the profile and its depth definition
     SELECT CASE(CINMODEL)
       CASE('ECMWF ','ARPEGE','ALADIN','MOCAGE','HIRLAM')
         CALL READ_GRIB_TF_TEB(HFILE,KLUOUT,CINMODEL,ZTI_BLD,ZMASK,ZFIELD,ZD)
     END SELECT
     !* if deep road temperature is prescribed
     IF (XTI_ROAD/=XUNDEF) THEN
       ZFIELD(:,2:) = XTI_ROAD 
     END IF
     CALL TEB_PROFILE_GRIB(XGRID_FLOOR)
!
!*      4.     Profile of temperatures in walls
!              --------------------------------

  CASE('T_WALLA','T_WALLB')
     CALL READ_GRIB_T_TEB(HFILE,KLUOUT,CINMODEL,ZTI_BLD,ZMASK,ZFIELD,ZD)
     CALL TEB_PROFILE_GRIB(XGRID_WALL)

  CASE('T_WIN1')
    SELECT CASE (CINMODEL)
      CASE ('ECMWF ','ARPEGE','ALADIN','MOCAGE','HIRLAM')
        CALL READ_GRIB_TS(HFILE,KLUOUT,CINMODEL,ZMASK,ZFIELD1D)
        ALLOCATE(PFIELD(NNI,1))
        PFIELD(:,1) = ZFIELD1D(:)
        DEALLOCATE(ZFIELD1D)
    END SELECT
!
!*      5.     Profile of temperatures in roofs
!              --------------------------------
!
  CASE('T_ROOF')    
     CALL READ_GRIB_T_TEB(HFILE,KLUOUT,CINMODEL,ZTI_BLD,ZMASK,ZFIELD,ZD)
     CALL TEB_PROFILE_GRIB(XGRID_ROOF)
!
!*      5.bis    Profile of temperatures in thermal mass
!              -----------------------------------------
!
  CASE('T_MASS')    
     ALLOCATE(PFIELD(NNI,3))
     PFIELD(:,:) = ZTI_BLD
!
!*      6.     Canyon air temperature
!              ----------------------
!
  CASE('T_CAN  ')
    SELECT CASE (CINMODEL)
      CASE ('ECMWF ','ARPEGE','ALADIN','MOCAGE','HIRLAM')
        CALL READ_GRIB_T2_LAND(HFILE,KLUOUT,CINMODEL,ZMASK,ZFIELD1D)
        ALLOCATE(PFIELD(SIZE(ZFIELD1D),1))
        PFIELD(:,1) = ZFIELD1D(:)
        DEALLOCATE(ZFIELD1D)
    END SELECT
!
!*      7.      Canyon air humidity
!               -------------------
!
  CASE('Q_CAN  ')
    SELECT CASE (CINMODEL)
      CASE ('ECMWF ','ARPEGE','ALADIN','MOCAGE','HIRLAM')
        ALLOCATE(PFIELD(NNI,1))
        PFIELD(:,1) = 0.01
    END SELECT

!
!*      9.     Deep road temperature
!              ---------------------

  CASE('TI_ROAD')    
     IF (XTI_ROAD==XUNDEF) THEN
       CALL READ_GRIB_T2_LAND(HFILE,KLUOUT,CINMODEL,ZMASK,ZFIELD1D)
       ALLOCATE(PFIELD(SIZE(ZFIELD1D),1))
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
     PFIELD = ZTI_BLD

  CASE('T_WIN2')
     ALLOCATE(PFIELD(NNI,1))
     PFIELD = ZTI_BLD

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
DEALLOCATE(ZMASK)
!
!*      4.     Interpolation method
!              --------------------
!
!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
!
!IF (LHOOK) CALL DR_HOOK('PREP_TEB_GRIB',1,ZHOOK_HANDLE)
CONTAINS
!
!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
SUBROUTINE TEB_PROFILE_GRIB(PGRID)
!-------------------------------------------------------------------------------------
!
REAL, DIMENSION(:),   INTENT(IN)  :: PGRID  ! destination grid
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!-------------------------------------------------------------------------------------
!
!* interpolation on fine vertical grid
!IF (LHOOK) CALL DR_HOOK('TEB_PROFILE_GRIB',0,ZHOOK_HANDLE)
ALLOCATE(PFIELD(SIZE(ZFIELD,1),SIZE(PGRID)))
 CALL INTERP_GRID(ZD,ZFIELD,PGRID,PFIELD)
!
!* end
DEALLOCATE(ZFIELD)
DEALLOCATE(ZD)
!IF (LHOOK) CALL DR_HOOK('TEB_PROFILE_GRIB',1,ZHOOK_HANDLE)

END SUBROUTINE TEB_PROFILE_GRIB
!
!-------------------------------------------------------------------------------------
END SUBROUTINE PREP_TEB_GRIB
