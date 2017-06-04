!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE AVG_ALBEDO_EMIS_TEB_VEG (PEK, HALBEDO, PTG1, PSW_BANDS, PDIR_ALB,PSCA_ALB, PEMIS, PTSRAD )  
!     ###################################################
!
!!**** ** computes radiative fields used in TEB_VEG
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
!!     A. Bogatchev 09/2005 EBA snow option
!!     B. Decharme  2008    The fraction of vegetation covered by snow must be
!                            <= to XPSNG
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODD_ISBA_n, ONLY : ISBA_PE_t
!
USE MODD_SURF_PAR,  ONLY : XUNDEF
!
USE MODD_TYPE_SNOW
!
USE MODD_SNOW_PAR,   ONLY : XEMISSN
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
!
USE MODI_ALBEDO
USE MODI_ALBEDO_FROM_NIR_VIS
USE MODI_ISBA_SNOW_FRAC
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
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
!
 CHARACTER(LEN=4),       INTENT(IN)   :: HALBEDO     ! albedo type
! Albedo dependance with surface soil water content
!   "EVOL" = albedo evolves with soil wetness
!   "DRY " = constant albedo value for dry soil
!   "WET " = constant albedo value for wet soil
!   "MEAN" = constant albedo value for medium soil wetness
!
REAL, DIMENSION(:),   INTENT(IN)   :: PTG1        ! soil surface temperature
REAL, DIMENSION(:),     INTENT(IN)   :: PSW_BANDS   ! middle wavelength of each band 
!
REAL, DIMENSION(:,:),   INTENT(OUT)  :: PDIR_ALB    ! averaged direct albedo  (per wavelength)
REAL, DIMENSION(:,:),   INTENT(OUT)  :: PSCA_ALB    ! averaged diffuse albedo (per wavelength)
REAL, DIMENSION(:),     INTENT(OUT)  :: PEMIS       ! averaged emissivity
REAL, DIMENSION(:),     INTENT(OUT)  :: PTSRAD      ! averaged radiaitve temp.
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!
REAL, DIMENSION(SIZE(PEK%XALBNIR_VEG(:))) :: ZALBNIR ! near-infra-red albedo with snow
REAL, DIMENSION(SIZE(PEK%XALBVIS_VEG(:))) :: ZALBVIS ! visible albedo with snow
REAL, DIMENSION(SIZE(PEK%XALBUV_VEG(:) )) :: ZALBUV  ! UV albedo with snow
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!
!*    1.      averaged albedo on natural continental surfaces (except prognostic snow)
!             -----------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('AVG_ALBEDO_EMIS_TEB_VEG',0,ZHOOK_HANDLE)
!
 CALL ALBEDO(HALBEDO, PEK )  

!
!*    2.      averaged albedo and emis. on natural continental surfaces (with prognostic snow)
!             ---------------------------------------------------------
!
ZALBNIR(:)=0.
ZALBVIS(:)=0.
ZALBUV (:)=0.
!
PDIR_ALB(:,:)=0.
PSCA_ALB(:,:)=0.
PEMIS   (:)  =0.
PTSRAD  (:)  =0.
!   
!
 CALL ISBA_SNOW_FRAC(PEK%TSNOW%SCHEME, PEK%TSNOW%WSNOW, PEK%TSNOW%RHO, PEK%TSNOW%ALB,  &
                     PEK%XVEG, PEK%XLAI, PEK%XZ0,PEK%XPSN, PEK%XPSNV_A, PEK%XPSNG, PEK%XPSNV )
!
WHERE (PEK%XVEG/=XUNDEF)
  !
  ! albedo on this tile
  !
  ZALBNIR(:) = (1.-PEK%XPSN)*PEK%XALBNIR + PEK%XPSN * PEK%TSNOW%ALB   
      
  ZALBVIS(:) = (1.-PEK%XPSN)*PEK%XALBVIS + PEK%XPSN * PEK%TSNOW%ALB   
      
  ZALBUV(:)  = (1.-PEK%XPSN)*PEK%XALBUV  + PEK%XPSN * PEK%TSNOW%ALB   
END WHERE
!
!* albedo for each wavelength
!
 CALL ALBEDO_FROM_NIR_VIS(PSW_BANDS,ZALBNIR, ZALBVIS, ZALBUV, PDIR_ALB, PSCA_ALB)  
!
! emissivity
!
WHERE (PEK%XEMIS/=XUNDEF)
  PEMIS(:)   = (1.-PEK%XPSN)*PEK%XEMIS + PEK%XPSN *XEMISSN  
END WHERE
!
!* radiative surface temperature
!
IF (PEK%TSNOW%SCHEME=='D95' .OR. PEK%TSNOW%SCHEME=='EBA') THEN
  PTSRAD(:) = PTG1(:)
ELSE IF (PEK%TSNOW%SCHEME=='3-L' .OR. PEK%TSNOW%SCHEME=='CRO') THEN
  WHERE (PEK%XEMIS/=XUNDEF)
    PTSRAD(:) =( ( (1.-PEK%XPSN)*PEMIS(:)       *PTG1(:)      **4            &
                  +    PEK%XPSN *PEK%TSNOW%EMIS * PEK%TSNOW%TS**4 ) )**0.25  &
                             / PEMIS(:)**0.25  
  END WHERE
END IF
!
IF (LHOOK) CALL DR_HOOK('AVG_ALBEDO_EMIS_TEB_VEG',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVG_ALBEDO_EMIS_TEB_VEG
