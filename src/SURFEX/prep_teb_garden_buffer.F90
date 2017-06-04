!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_TEB_GARDEN_BUFFER(HPROGRAM,HSURF,KLUOUT,PFIELD)
!     #################################################################################
!
!!****  *PREP_TEB_GARDEN_BUFFER* - initializes ISBA fields from operational BUFFER
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
USE MODE_READ_BUFFER
!
USE MODD_TYPE_DATE_SURF
!
USE MODI_PREP_BUFFER_GRID
USE MODI_INTERP_GRID
!
USE MODD_PREP,           ONLY : CINTERP_TYPE
USE MODD_PREP_TEB_GARDEN,ONLY : XGRID_SOIL, XWR_DEF
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_GRID_BUFFER,    ONLY : NNI
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
REAL,DIMENSION(:,:,:), POINTER    :: PFIELD    ! field to interpolate horizontally
!
!*      0.2    declarations of local variables
!
TYPE (DATE_TIME)                :: TZTIME_BUF    ! current date and time
 CHARACTER(LEN=6)                :: YINMODEL       ! model from which buffer originates
REAL, DIMENSION(:,:), POINTER   :: ZFIELD         ! field read
REAL, DIMENSION(:),   POINTER   :: ZFIELD1D       ! field read
REAL, DIMENSION(:,:), POINTER   :: ZD             ! depth of field in the soil
INTEGER                         :: JVEGTYPE       ! loop counter on vegtypes
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
!*      1.     Reading of grid
!              ---------------
!
IF (LHOOK) CALL DR_HOOK('PREP_TEB_GARDEN_BUFFER',0,ZHOOK_HANDLE)
 CALL PREP_BUFFER_GRID(KLUOUT,YINMODEL,TZTIME_BUF)

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
     SELECT CASE(YINMODEL)
     CASE('ALADIN')
        CALL READ_BUFFER_TG(KLUOUT,YINMODEL,ZFIELD,ZD)
     END SELECT
     
     CALL SOIL_PROFILE_BUFFER

  CASE('WG    ')
     !* reading of the profile and its depth definition
     SELECT CASE(YINMODEL)
     CASE('ARPEGE','ALADIN','MOCAGE')
        CALL READ_BUFFER_WG(KLUOUT,YINMODEL,ZFIELD,ZD)
     END SELECT
     CALL SOIL_PROFILE_BUFFER


!*      3.3    Profile of soil ice content

  CASE('WGI   ')    
     !* reading of the profile and its depth definition
     SELECT CASE(YINMODEL)
       CASE('ALADIN')
         CALL READ_BUFFER_WGI(KLUOUT,YINMODEL,ZFIELD,ZD)
     END SELECT
     CALL SOIL_PROFILE_BUFFER
!
!*      3.4    Water content intercepted on leaves, LAI
!
  CASE('WR     ')
     ALLOCATE(PFIELD(NNI,1,NVEGTYPE))
     PFIELD(:,:,:) = XWR_DEF
!
  CASE('LAI    ')
     ALLOCATE(PFIELD(NNI,1,NVEGTYPE))
     PFIELD(:,:,:) = XUNDEF
!
!
!*      3.5    Other fields
!
  CASE('ZS     ')
!GH
!    CALL READ_BUFFER_ZS_LAND(KLUOUT,YINMODEL,ZFIELD1D)
     CALL READ_BUFFER_ZS(KLUOUT,YINMODEL,ZFIELD1D)
!END GH
     ALLOCATE(PFIELD(SIZE(ZFIELD1D,1),1,1))
     PFIELD(:,1,1)=ZFIELD1D(:)
     DEALLOCATE(ZFIELD1D)
END SELECT
!
!*      4.     Interpolation method
!              --------------------
!
CINTERP_TYPE='BUFFER'
!
!
!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_TEB_GARDEN_BUFFER',1,ZHOOK_HANDLE)
CONTAINS
!
!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
SUBROUTINE SOIL_PROFILE_BUFFER
!-------------------------------------------------------------------------------------
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZOUT   ! work array
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
     !
     !* interpolation on fine vertical grid
     IF (LHOOK) CALL DR_HOOK('SOIL_PROFILE_BUFFER',0,ZHOOK_HANDLE)
     ALLOCATE(ZOUT  (SIZE(ZFIELD,1),SIZE(XGRID_SOIL)))
     CALL INTERP_GRID(ZD,ZFIELD,XGRID_SOIL,ZOUT)
     !
     !* extends definition to all vegtypes.
     ALLOCATE(PFIELD(SIZE(ZFIELD,1),SIZE(XGRID_SOIL),NVEGTYPE))
     DO JVEGTYPE=1,NVEGTYPE
       PFIELD(:,:,JVEGTYPE)=ZOUT(:,:)
     END DO
     !* end
     DEALLOCATE(ZOUT)
     DEALLOCATE(ZFIELD)
     DEALLOCATE(ZD)
IF (LHOOK) CALL DR_HOOK('SOIL_PROFILE_BUFFER',1,ZHOOK_HANDLE)

END SUBROUTINE SOIL_PROFILE_BUFFER
!
!-------------------------------------------------------------------------------------
END SUBROUTINE PREP_TEB_GARDEN_BUFFER
