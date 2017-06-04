!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_ISBA_GRIB(HPROGRAM,HSURF,HFILE,KLUOUT,PFIELD,OKEY)
!     #################################################################################
!
!!****  *PREP_ISBA_GRIB* - initializes ISBA fields from operational GRIB
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
!!      S. Riette   04/2010 READ_GRIB_WGI_ECMWF interface changed
!!------------------------------------------------------------------
!
USE MODE_READ_GRIB
!
USE MODD_TYPE_DATE_SURF
!
USE MODI_INTERP_GRID_NAT
!
USE MODD_PREP_ISBA,      ONLY : XGRID_SOIL, XWR_DEF, XWRV_DEF,     &
                                XWRVN_DEF, XQC_DEF
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_GRID_GRIB,      ONLY : CGRIB_FILE, NNI, CINMODEL
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
CHARACTER(LEN=6),   INTENT(IN)    :: HPROGRAM  ! program calling surf. schemes
CHARACTER(LEN=7),   INTENT(IN)    :: HSURF     ! type of field
CHARACTER(LEN=28),  INTENT(IN)    :: HFILE     ! name of file
INTEGER,            INTENT(IN)    :: KLUOUT    ! logical unit of output listing
REAL,DIMENSION(:,:,:), POINTER    :: PFIELD    ! field to interpolate horizontally
LOGICAL, OPTIONAL,  INTENT(INOUT) :: OKEY
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(:)  , POINTER   :: ZMASK => NULL()          ! Land mask
REAL, DIMENSION(:,:), POINTER   :: ZFIELD => NULL()         ! field read
REAL, DIMENSION(:),   POINTER   :: ZFIELD1D => NULL()       ! field read
REAL, DIMENSION(:,:), POINTER   :: ZD => NULL()             ! layer thicknesses
INTEGER                         :: JVEGTYPE       ! loop counter on vegtypes
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
!*      1.     Reading of grid
!              ---------------
!
IF (LHOOK) CALL DR_HOOK('PREP_ISBA_GRIB',0,ZHOOK_HANDLE)
!
IF (TRIM(HFILE).NE.CGRIB_FILE) CGRIB_FILE=""
!
 CALL READ_GRIB_LAND_MASK(HFILE,KLUOUT,CINMODEL,ZMASK)
!
!
!*      2.     Reading of field
!              ----------------
!
!*      3.     Transformation into physical quantity to be interpolated
!              --------------------------------------------------------
!
SELECT CASE(HSURF)
!
!*      3.1    Profile of temperature in the soil
!
  CASE('TG    ')
     !* reading of the profile and its depth definition
     SELECT CASE(CINMODEL)
       CASE('ECMWF ')
         IF(PRESENT(OKEY))OKEY=.FALSE.
         CALL READ_GRIB_TG_ECMWF(HFILE,KLUOUT,CINMODEL,ZMASK,ZFIELD,ZD)
       CASE('ARPEGE','ALADIN','MOCAGE')
         CALL READ_GRIB_TG_METEO_FRANCE(HFILE,KLUOUT,CINMODEL,ZMASK,ZFIELD,ZD)
       CASE('HIRLAM')
         CALL READ_GRIB_TG_HIRLAM(HFILE,KLUOUT,CINMODEL,ZMASK,ZFIELD,ZD)
     END SELECT
     CALL SOIL_PROFILE_GRIB

  CASE('WG    ')
     !* reading of the profile and its depth definition
     SELECT CASE(CINMODEL)
       CASE('ECMWF ')
         IF(PRESENT(OKEY))OKEY=.FALSE.
         CALL READ_GRIB_WG_ECMWF(HFILE,KLUOUT,CINMODEL,ZMASK,ZFIELD,ZD)
       CASE('ARPEGE','ALADIN','MOCAGE')
         CALL READ_GRIB_WG_METEO_FRANCE(HFILE,KLUOUT,CINMODEL,ZMASK,ZFIELD,ZD)
       CASE('HIRLAM')
         CALL READ_GRIB_WG_HIRLAM(HFILE,KLUOUT,CINMODEL,ZMASK,ZFIELD,ZD)
     END SELECT
     CALL SOIL_PROFILE_GRIB

!*      3.3    Profile of soil ice content

  CASE('WGI   ')    
     !* reading of the profile and its depth definition
     SELECT CASE(CINMODEL)
       CASE('ECMWF ')
         IF(PRESENT(OKEY))OKEY=.FALSE.
         CALL READ_GRIB_WGI_ECMWF(HFILE,KLUOUT,CINMODEL,ZMASK,ZFIELD,ZD)
       CASE('ARPEGE','ALADIN','MOCAGE')
         CALL READ_GRIB_WGI_METEO_FRANCE(HFILE,KLUOUT,CINMODEL,ZMASK,ZFIELD,ZD)
       CASE('HIRLAM')
         CALL READ_GRIB_WGI_HIRLAM(HFILE,KLUOUT,ZFIELD,ZD)
     END SELECT
     CALL SOIL_PROFILE_GRIB
!
!*      3.4    Water content intercepted on leaves, LAI
!
  CASE('WR     ')
     ALLOCATE(PFIELD(NNI,1,1))
     PFIELD(:,:,:) = XWR_DEF
!
  CASE('LAI    ')
     ALLOCATE(PFIELD(NNI,1,1))
     PFIELD(:,:,:) = XUNDEF
!
!
!*      3.5    Other fields
!
  CASE('ZS     ')
     CALL READ_GRIB_ZS_LAND(HFILE,KLUOUT,CINMODEL,ZMASK,ZFIELD1D)
     ALLOCATE(PFIELD(SIZE(ZFIELD1D,1),1,1))
     PFIELD(:,1,1)=ZFIELD1D(:)
     DEALLOCATE(ZFIELD1D)
!
  CASE('ICE_STO')
     ALLOCATE(PFIELD(NNI,1,1))
     PFIELD(:,:,:) = 0.0
!
!*      3.6    MEB fields
!
  CASE('WRV    ')
     ALLOCATE(PFIELD(NNI,1,1))
     PFIELD(:,:,:) = XWRV_DEF
!
  CASE('WRVN   ')
     ALLOCATE(PFIELD(NNI,1,1))
     PFIELD(:,:,:) = XWRVN_DEF
!
  CASE('QC     ')
     ALLOCATE(PFIELD(NNI,1,1))
     PFIELD(:,:,:) = XQC_DEF
!
  CASE('TV     ','TC     ')
     !* reading of the profile and its depth definition
     SELECT CASE(CINMODEL)
       CASE('ECMWF ')
         IF(PRESENT(OKEY))OKEY=.FALSE.
         CALL READ_GRIB_TG_ECMWF(HFILE,KLUOUT,CINMODEL,ZMASK,ZFIELD,ZD)
       CASE('ARPEGE','ALADIN','MOCAGE')
         CALL READ_GRIB_TG_METEO_FRANCE(HFILE,KLUOUT,CINMODEL,ZMASK,ZFIELD,ZD)
       CASE('HIRLAM')
         CALL READ_GRIB_TG_HIRLAM(HFILE,KLUOUT,CINMODEL,ZMASK,ZFIELD,ZD)
     END SELECT
     ALLOCATE(PFIELD(NNI,1,1))
     PFIELD(:,1,1) =ZFIELD(:,1)
     DEALLOCATE(ZFIELD)
     DEALLOCATE(ZD)
!
  CASE DEFAULT
    CALL ABOR1_SFX('PREP_ISBA_GRIB: '//TRIM(HSURF)//" initialization not implemented !")
!
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
IF (LHOOK) CALL DR_HOOK('PREP_ISBA_GRIB',1,ZHOOK_HANDLE)
CONTAINS
!
!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
SUBROUTINE SOIL_PROFILE_GRIB
!-------------------------------------------------------------------------------------
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZOUT   ! work array
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
     !
     !* interpolation on fine vertical grid
     IF (LHOOK) CALL DR_HOOK('SOIL_PROFILE_GRIB',0,ZHOOK_HANDLE)
     ALLOCATE(ZOUT  (SIZE(ZFIELD,1),SIZE(XGRID_SOIL)))
     CALL INTERP_GRID_NAT(ZD,ZFIELD,XGRID_SOIL,ZOUT)
     !
     !* extends definition to all vegtypes.
     ALLOCATE(PFIELD(SIZE(ZFIELD,1),SIZE(XGRID_SOIL),1))
     PFIELD(:,:,1)=ZOUT(:,:)
     !* end
     DEALLOCATE(ZOUT)
     DEALLOCATE(ZFIELD)
     DEALLOCATE(ZD)
IF (LHOOK) CALL DR_HOOK('SOIL_PROFILE_GRIB',1,ZHOOK_HANDLE)

END SUBROUTINE SOIL_PROFILE_GRIB
!
!-------------------------------------------------------------------------------------
END SUBROUTINE PREP_ISBA_GRIB
