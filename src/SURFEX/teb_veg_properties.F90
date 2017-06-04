!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE TEB_VEG_PROPERTIES (PMASK, IO, PEK, PDIR_SW, PSCA_SW, PSW_BANDS, KSW, &
                                     PTS, PEMIS, PALB, PTA, PALBNIR_TVEG, PALBVIS_TVEG,&
                                     PALBNIR_TSOIL, PALBVIS_TSOIL      )  
!     ##########################################################################
!
!!****  *GARDEN_PROPERTIES*  
!!
!!    PURPOSE
!!    -------
!
!     Calculates grid-averaged albedo and emissivity (according to snow scheme)
!         
!!    EXTERNAL
!!    --------
!!
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!      
!!    AUTHOR
!!    ------
!!
!!      S. Belair           * Meteo-France *
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_PE_t
!
USE MODD_SURF_PAR, ONLY : XUNDEF
!
USE MODI_ISBA_PROPERTIES
USE MODI_FLAG_TEB_VEG_n
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
REAL, DIMENSION(:), INTENT(IN) :: PMASK
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
!
REAL, DIMENSION(:,:), INTENT(IN)   :: PDIR_SW            ! direct incoming solar radiation
REAL, DIMENSION(:,:), INTENT(IN)   :: PSCA_SW            ! diffus incoming solar radiation
REAL, DIMENSION(:)  , INTENT(IN)   :: PSW_BANDS          ! mean wavelength of each shortwave band (m)
INTEGER,              INTENT(IN)   :: KSW                ! number of short-wave spectral bands
!
REAL, DIMENSION(:)  , INTENT(OUT)  :: PTS                ! radiative surface temperature
REAL, DIMENSION(:)  , INTENT(OUT)  :: PEMIS              ! green areas emissivity
REAL, DIMENSION(:)  , INTENT(OUT)  :: PALB               ! green areas albedo
!
REAL, DIMENSION(:)  , INTENT(IN), OPTIONAL :: PTA        ! Air temperature (K)
!
REAL, DIMENSION(:)  , INTENT(OUT), OPTIONAL  :: PALBNIR_TVEG       ! nearIR  veg tot albedo
REAL, DIMENSION(:)  , INTENT(OUT), OPTIONAL  :: PALBVIS_TVEG       ! visible veg tot albedo
REAL, DIMENSION(:)  , INTENT(OUT), OPTIONAL  :: PALBNIR_TSOIL      ! nearIR  soil tot albedo
REAL, DIMENSION(:)  , INTENT(OUT), OPTIONAL  :: PALBVIS_TSOIL      ! visible soil tot albedo
!
!-------------------------------------------------------------------------------
!
!*      0.2    Local variables
!              ---------------
!
INTEGER                        :: JLAYER
INTEGER                        :: JSWB
!
REAL, DIMENSION(SIZE(PALB))    :: ZTSNOSNOW ! surf. temp. on snow free part
REAL, DIMENSION(SIZE(PALB))    :: ZTSSNOW   ! surf. temp. on snow covered part
REAL, DIMENSION(SIZE(PALB))    :: ZANOSNOW  ! snow-free surface albedo
REAL, DIMENSION(SIZE(PALB))    :: ZASNOW    ! snow albedo
REAL, DIMENSION(SIZE(PALB))    :: ZENOSNOW  ! snow-free surface emissivity
REAL, DIMENSION(SIZE(PALB))    :: ZESNOW    ! snow emissivity
!
REAL, DIMENSION(SIZE(PALB))    :: ZALBNIR_TVEG       ! nearIR  veg tot albedo
REAL, DIMENSION(SIZE(PALB))    :: ZALBVIS_TVEG       ! visible veg tot albedo
REAL, DIMENSION(SIZE(PALB))    :: ZALBNIR_TSOIL      ! nearIR  soil tot albedo
REAL, DIMENSION(SIZE(PALB))    :: ZALBVIS_TSOIL      ! visible soil tot albedo
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TEB_VEG_PROPERTIES',0,ZHOOK_HANDLE)
!
!*      1.     Set physical values for points where there is no garden
!              -------------------------------------------------------
!
! This way, ISBA can run without problem for these points
!
 CALL FLAG_TEB_VEG_n(PEK, IO, PMASK, 1)
!
!
!*      2.     Computes several properties of gardens
!              --------------------------------------
!
 CALL ISBA_PROPERTIES(IO, PEK, PDIR_SW, PSCA_SW, PSW_BANDS, KSW,                &
                      ZASNOW, ZANOSNOW, ZESNOW, ZENOSNOW, ZTSSNOW, ZTSNOSNOW,   &
                      ZALBNIR_TVEG, ZALBVIS_TVEG, ZALBNIR_TSOIL, ZALBVIS_TSOIL)         
!
PEK%XSNOWFREE_ALB(:) = ZANOSNOW
!
!* averaged albedo
PALB =  PEK%XPSN(:) * ZASNOW              + (1.-PEK%XPSN(:)) * ZANOSNOW
!* averaged emissivity
PEMIS=  PEK%XPSN(:) * ZESNOW              + (1.-PEK%XPSN(:)) * ZENOSNOW
!* averaged surface radiative temperature
!  (recomputed from emitted long wave)
PTS  =((PEK%XPSN(:) * ZESNOW * ZTSSNOW**4 + (1.-PEK%XPSN(:)) * ZENOSNOW * ZTSNOSNOW**4) / PEMIS)**0.25
!
IF(PRESENT(PALBNIR_TVEG))PALBNIR_TVEG  (:) = ZALBNIR_TVEG (:)
IF(PRESENT(PALBVIS_TVEG))PALBVIS_TVEG  (:) = ZALBVIS_TVEG (:)
IF(PRESENT(PALBNIR_TSOIL))PALBNIR_TSOIL(:) = ZALBNIR_TSOIL(:)
IF(PRESENT(PALBVIS_TSOIL))PALBVIS_TSOIL(:) = ZALBVIS_TSOIL(:)
!
IF (LHOOK) CALL DR_HOOK('TEB_VEG_PROPERTIES',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE TEB_VEG_PROPERTIES

