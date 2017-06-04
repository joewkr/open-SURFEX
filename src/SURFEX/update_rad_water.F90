!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE UPDATE_RAD_WATER(W,PZENITH,PTT,PDIR_ALB_ATMOS,PSCA_ALB_ATMOS,PEMIS_ATMOS,PTRAD)  
!     #######################################################################
!
!!****  *UPDATE_RAD_WATER * - update the radiative properties at time t+1 (see by the atmosphere) 
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
!!      Original    09/2009
!!      Modified    03/2011 : E. Bazile (MK10) albedo from Marat Khairoutdinov
!!      Modified    02/2014 : split from update_rad_seawat.F90
!!------------------------------------------------------------------
!
USE MODD_WATFLUX_n, ONLY : WATFLUX_t
!
USE MODD_WATER_PAR, ONLY : XEMISWAT, XEMISWATICE, &
                           XALBWAT, XALBSCA_WAT,  &
                           XALBWATICE
!
USE MODN_SFX_OASIS,  ONLY : LWATER
USE MODD_SFX_OASIS,  ONLY : LCPL_SEA
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
TYPE(WATFLUX_t), INTENT(INOUT) :: W
!
REAL, DIMENSION(:),     INTENT(IN)   :: PZENITH   ! Zenithal angle at t+1
REAL,                   INTENT(IN)   :: PTT       ! Sea/ice transition temperature (different according to sea or inland water)
!
REAL, DIMENSION(:,:),   INTENT(OUT)  :: PDIR_ALB_ATMOS ! Direct albedo at t+1 for the atmosphere
REAL, DIMENSION(:,:),   INTENT(OUT)  :: PSCA_ALB_ATMOS ! Diffuse albedo at t+1 for the atmosphere
REAL, DIMENSION(:),     INTENT(OUT)  :: PEMIS_ATMOS    ! Emissivity at t+1 for the atmosphere
REAL, DIMENSION(:),     INTENT(OUT)  :: PTRAD          ! radiative temp at t+1 for the atmosphere
!
!*      0.2    declarations of local variables
!
INTEGER :: JSWB
REAL, DIMENSION(SIZE(W%XTS)) :: ZALBEDO
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('UPDATE_RAD_WATER',0,ZHOOK_HANDLE)
!
ZALBEDO(:) = 0.
IF (W%CWAT_ALB=='TA96') THEN
  ZALBEDO(:) = ALBEDO_TA96(PZENITH(:))
ELSEIF (W%CWAT_ALB=='MK10') THEN
  ZALBEDO(:) = ALBEDO_MK10(PZENITH(:))
ENDIF
!
IF(LCPL_SEA.AND.LWATER)THEN !Earth System Model
!
!Sea and/or ice albedo already given by oceanic model
!Except for Taylor et al (1996) formulation
!
  !
  WHERE (W%XTS(:)>=PTT  )
    !* open water
    W%XEMIS   (:) = XEMISWAT
  ELSEWHERE
    !* sea ice
    W%XEMIS   (:) = XEMISWATICE
  END WHERE
  !
  IF (W%CWAT_ALB=='TA96' .OR. W%CWAT_ALB=='MK10') THEN
    !* Taylor et al 1996
    !* open water
    WHERE (W%XTS(:)>=PTT) W%XDIR_ALB(:) = ZALBEDO(:)
    WHERE (W%XTS(:)>=PTT) W%XSCA_ALB(:) = XALBSCA_WAT
  ENDIF
  !
ELSE
  !
  IF (W%CWAT_ALB=='UNIF') THEN
  !* uniform albedo
    WHERE (W%XTS(:)>=PTT  )
    !* open water
      W%XDIR_ALB  (:) = XALBWAT
      W%XSCA_ALB  (:) = XALBWAT
      W%XEMIS     (:) = XEMISWAT
    ELSEWHERE
    !* sea ice
      W%XDIR_ALB(:) = XALBWATICE
      W%XSCA_ALB(:) = XALBWATICE
      W%XEMIS   (:) = XEMISWATICE
    END WHERE
  !
  ELSE IF (W%CWAT_ALB=='TA96' .OR. W%CWAT_ALB=='MK10') THEN
    !* Taylor et al 1996
    WHERE (W%XTS(:)>=PTT) W%XDIR_ALB(:) = ZALBEDO(:)
    !
    WHERE (W%XTS(:)>=PTT)
    !* open water
      W%XSCA_ALB  (:) = XALBSCA_WAT
      W%XEMIS     (:) = XEMISWAT
    ELSEWHERE
    !* sea ice
      W%XDIR_ALB(:) = XALBWATICE
      W%XSCA_ALB(:) = XALBWATICE
      W%XEMIS   (:) = XEMISWATICE
    END WHERE
    !
  ENDIF
  !
ENDIF
!
!-------------------------------------------------------------------------------------
!
DO JSWB=1,SIZE(PDIR_ALB_ATMOS,2)
  PDIR_ALB_ATMOS(:,JSWB) = W%XDIR_ALB(:)
  PSCA_ALB_ATMOS(:,JSWB) = W%XSCA_ALB(:)
END DO
!
PEMIS_ATMOS(:) = W%XEMIS(:)
PTRAD      (:) = W%XTS (:)
!
IF (LHOOK) CALL DR_HOOK('UPDATE_RAD_WATER',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE UPDATE_RAD_WATER

