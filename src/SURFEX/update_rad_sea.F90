!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE UPDATE_RAD_SEA(S,PZENITH,PTT,PDIR_ALB_ATMOS,PSCA_ALB_ATMOS,PEMIS_ATMOS,PTRAD,PU,PV)  
!     #######################################################################
!
!!****  *UPDATE_RAD_SEA * - update the radiative properties at time t+1 (see by the atmosphere) 
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
!!      Modified    01/2014 : S. Senesi : handle fractional seaice
!!      Modified    02/2014 : split from update_rad_seawat.F90
!!      Modified    01/2015 : introduce interactive ocean surface albedo (R.Séférian)
!!------------------------------------------------------------------
!
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
!
USE MODD_WATER_PAR, ONLY : XEMISWAT, XEMISWATICE, &
                           XALBWAT, XALBSCA_WAT,  &
                           XALBSEAICE
!
USE MODD_SFX_OASIS, ONLY : LCPL_SEA
!
USE MODI_ALBEDO_TA96
USE MODI_ALBEDO_MK10
USE MODI_ALBEDO_RS14
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
!
REAL, DIMENSION(:),     INTENT(IN)   :: PZENITH   ! Zenithal angle at t+1
REAL,                   INTENT(IN)   :: PTT       ! Sea/ice transition temperature (different according to sea or inland water)
!
REAL, DIMENSION(:,:),   INTENT(OUT)  :: PDIR_ALB_ATMOS ! Direct albedo at t+1 for the atmosphere
REAL, DIMENSION(:,:),   INTENT(OUT)  :: PSCA_ALB_ATMOS ! Diffuse albedo at t+1 for the atmosphere
REAL, DIMENSION(:),     INTENT(OUT)  :: PEMIS_ATMOS    ! Emissivity at t+1 for the atmosphere
REAL, DIMENSION(:),     INTENT(OUT)  :: PTRAD          ! radiative temp at t+1 for the atmosphere
!
REAL, DIMENSION(:),     INTENT(IN)   , OPTIONAL :: PU        ! zonal wind (m/s)
REAL, DIMENSION(:),     INTENT(IN)   , OPTIONAL :: PV        ! meridian wind (m/s)
!
!*      0.2    declarations of local variables
!
INTEGER :: JSWB
REAL, DIMENSION(SIZE(PZENITH)) :: ZALBDIR
REAL, DIMENSION(SIZE(PZENITH)) :: ZALBSCA
REAL, DIMENSION(SIZE(PZENITH)) :: ZWIND
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('UPDATE_RAD_SEA',0,ZHOOK_HANDLE)
!
ZALBDIR(:) = 0.
ZALBSCA(:) = 0.
!
IF (S%CSEA_ALB=='TA96') THEN
!        
  ZALBDIR(:) = ALBEDO_TA96(PZENITH(:))
  ZALBSCA(:) = XALBSCA_WAT
!  
ELSEIF (S%CSEA_ALB=='MK10') THEN
!        
  ZALBDIR(:) = ALBEDO_MK10(PZENITH(:))
  ZALBSCA(:) = XALBSCA_WAT
!  
ELSEIF (S%CSEA_ALB=='RS14') THEN
!        
  IF (PRESENT(PU).AND.PRESENT(PV)) THEN
     ZWIND(:) = SQRT(PU(:)**2+PV(:)**2)
     CALL ALBEDO_RS14(PZENITH(:),ZWIND(:),ZALBDIR(:),ZALBSCA(:))
  ELSE
     ZALBDIR(:) = S%XDIR_ALB(:)
     ZALBSCA(:) = S%XSCA_ALB(:)
  ENDIF
!
ENDIF
!
IF(LCPL_SEA)THEN !Earth System Model
!
!Sea and/or ice albedo already given by coupled seaice model
!Except for Taylor et al (1996) and MK10 formulation
!
  WHERE (S%XSST(:)>=PTT  )
    !* open water
    S%XEMIS   (:) = XEMISWAT
  ELSEWHERE
    !* sea ice
    S%XEMIS   (:) = XEMISWATICE
  END WHERE
  !
  IF (S%CSEA_ALB=='TA96' .OR. S%CSEA_ALB=='MK10' .OR. S%CSEA_ALB=='RS14') THEN
    !* Taylor et al 1996
    !* open water
    WHERE (S%XSST(:)>=PTT) S%XDIR_ALB(:) = ZALBDIR(:)
    WHERE (S%XSST(:)>=PTT) S%XSCA_ALB(:) = ZALBSCA(:)
  ENDIF
  !
ELSEIF(S%LHANDLE_SIC) THEN 
  ! Returned values are an average of open sea and seaice properties
  ! weighted by the seaice cover
  S%XEMIS   (:) = ( 1 - S%XSIC(:)) * XEMISWAT    + S%XSIC(:) * XEMISWATICE
  IF (S%CSEA_ALB=='UNIF') THEN
     S%XDIR_ALB(:) = ( 1 - S%XSIC(:)) * XALBWAT     + S%XSIC(:) * S%XICE_ALB(:)
     S%XSCA_ALB(:) = ( 1 - S%XSIC(:)) * XALBWAT     + S%XSIC(:) * S%XICE_ALB(:)
  ELSE IF (S%CSEA_ALB=='TA96' .OR. S%CSEA_ALB=='MK10' .OR. S%CSEA_ALB=='RS14') THEN
     S%XDIR_ALB(:) = ( 1 - S%XSIC(:)) * ZALBDIR(:) + S%XSIC(:) * S%XICE_ALB(:)
     S%XSCA_ALB(:) = ( 1 - S%XSIC(:)) * ZALBSCA(:) + S%XSIC(:) * S%XICE_ALB(:)
  ENDIF
ELSE
  !
  IF (S%CSEA_ALB=='UNIF') THEN
  !* uniform albedo
    WHERE (S%XSST(:)>=PTT  )
    !* open water
      S%XDIR_ALB  (:) = XALBWAT
      S%XSCA_ALB  (:) = XALBWAT
      S%XEMIS     (:) = XEMISWAT
    ELSEWHERE
    !* sea ice
      S%XDIR_ALB(:) = XALBSEAICE
      S%XSCA_ALB(:) = XALBSEAICE
      S%XEMIS   (:) = XEMISWATICE
    END WHERE
  !
  ELSE IF (S%CSEA_ALB=='TA96' .OR. S%CSEA_ALB=='MK10' .OR. S%CSEA_ALB=='RS14') THEN
    !* Taylor et al 1996
    !
    WHERE (S%XSST(:)>=PTT)
    !* open water
      S%XDIR_ALB  (:) = ZALBDIR(:)
      S%XSCA_ALB  (:) = ZALBSCA(:)
      S%XEMIS     (:) = XEMISWAT
    ELSEWHERE
    !* sea ice
      S%XDIR_ALB(:) = XALBSEAICE
      S%XSCA_ALB(:) = XALBSEAICE
      S%XEMIS   (:) = XEMISWATICE
    END WHERE
    !
  ENDIF
  !
ENDIF
!
!-------------------------------------------------------------------------------------
!
DO JSWB=1,SIZE(PDIR_ALB_ATMOS,2)
  PDIR_ALB_ATMOS(:,JSWB) = S%XDIR_ALB(:)
  PSCA_ALB_ATMOS(:,JSWB) = S%XSCA_ALB(:)
END DO
!
PEMIS_ATMOS(:) = S%XEMIS(:)
IF(S%LHANDLE_SIC) THEN 
   PTRAD(:) = (((1 - S%XSIC(:)) * XEMISWAT    * S%XSST (:)**4 + &
                     S%XSIC(:)  * XEMISWATICE * S%XTICE(:)**4)/ &
              S%XEMIS(:)) ** 0.25
ELSE
   PTRAD(:) = S%XSST (:)
END IF
!
IF (LHOOK) CALL DR_HOOK('UPDATE_RAD_SEA',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE UPDATE_RAD_SEA

