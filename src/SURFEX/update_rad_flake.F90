!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE UPDATE_RAD_FLAKE(F,PZENITH,PDIR_ALB_ATMOS,PSCA_ALB_ATMOS,PEMIS_ATMOS,PTRAD )  
!     #######################################################################
!
!!****  *UPDATE_RAD_FLAKE * - update the radiative properties at time t+1 (see by the atmosphere) 
!                           in order to close the energy budget between surfex and the atmosphere
 
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
!!     B. Decharme 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    04/2013
!!------------------------------------------------------------------
!
USE MODD_FLAKE_n, ONLY : FLAKE_t
!
USE MODD_WATER_PAR, ONLY : XALBSCA_WAT, XALBWAT, XEMISWAT, XEMISWATICE
!
USE modd_flake_parameters , ONLY : h_Snow_min_flk, h_Ice_min_flk
!
USE MODD_SNOW_PAR, ONLY : XEMISSN
!
USE MODI_ALBEDO_TA96
USE MODI_ALBEDO_MK10
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(FLAKE_t), INTENT(INOUT) :: F
REAL, DIMENSION(:),     INTENT(IN)   :: PZENITH   ! Zenithal angle at t+1
REAL, DIMENSION(:,:),   INTENT(OUT)  :: PDIR_ALB_ATMOS ! Direct albedo at t+1 for the atmosphere
REAL, DIMENSION(:,:),   INTENT(OUT)  :: PSCA_ALB_ATMOS ! Diffuse albedo at t+1 for the atmosphere
REAL, DIMENSION(:),     INTENT(OUT)  :: PEMIS_ATMOS    ! Emissivity at t+1 for the atmosphere
REAL, DIMENSION(:),     INTENT(OUT)  :: PTRAD          ! radiative temp at t+1 for the atmosphere
!
!*      0.2    declarations of local variables
!
INTEGER :: JSWB
!
REAL, DIMENSION(SIZE(F%XTS)) :: ZALBDIR
REAL, DIMENSION(SIZE(F%XTS)) :: ZALBSCA
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('UPDATE_RAD_FLAKE',0,ZHOOK_HANDLE)
!
ZALBDIR(:) = 0.
ZALBSCA(:) = 0.
!
IF (F%CFLK_ALB=='TA96') THEN
  ZALBDIR(:) = ALBEDO_TA96(PZENITH(:))
  ZALBSCA(:) = XALBSCA_WAT
ELSEIF (F%CFLK_ALB=='MK10') THEN
  ZALBDIR(:) = ALBEDO_MK10(PZENITH(:))
  ZALBSCA(:) = XALBSCA_WAT
ELSE
  ZALBDIR(:) = XALBWAT
  ZALBSCA(:) = XALBWAT
ENDIF
!
WHERE (F%XH_SNOW(:)>=h_Snow_min_flk)
!* snow
  F%XDIR_ALB  (:) = F%XSNOW_ALB(:)
  F%XSCA_ALB  (:) = F%XSNOW_ALB(:)
  F%XEMIS     (:) = XEMISSN
ELSEWHERE(F%XH_ICE(:)>=h_ice_min_flk)
!* ice
  F%XDIR_ALB(:) = F%XICE_ALB(:)
  F%XSCA_ALB(:) = F%XICE_ALB(:)
  F%XEMIS   (:) = XEMISWATICE
ELSEWHERE
!* open water
  F%XDIR_ALB  (:) = ZALBDIR(:)
  F%XSCA_ALB  (:) = ZALBSCA(:)
  F%XEMIS     (:) = XEMISWAT    
END WHERE
!
!-------------------------------------------------------------------------------------
!
DO JSWB=1,SIZE(PDIR_ALB_ATMOS,2)
  PDIR_ALB_ATMOS(:,JSWB) = F%XDIR_ALB(:)
  PSCA_ALB_ATMOS(:,JSWB) = F%XSCA_ALB(:)
END DO
!
PEMIS_ATMOS(:) = F%XEMIS(:)
PTRAD      (:) = F%XTS  (:)
!
IF (LHOOK) CALL DR_HOOK('UPDATE_RAD_FLAKE',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE UPDATE_RAD_FLAKE

