!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE AVERAGE_FLUX(PFRAC_TILE,             &
                   PSFTH_TILE, PSFTQ_TILE,              &
                   PSFTS_TILE, PSFCO2_TILE,             &
                   PSFU_TILE, PSFV_TILE,                &
                   PSFTH, PSFTQ, PSFTS, PSFCO2,         &
                   PSFU, PSFV                           )  
!     ######################################################################
!
!
!!****  *AVERAGE_FLUX*  
!!
!!    PURPOSE
!!    -------
!      Average the fluxes from the land and water surfaces depending on the
!      fraction of each surface cover type in the mesh area.
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!      
!!    REFERENCE
!!    ---------
!!      
!!    AUTHOR
!!    ------
!!      S. Belair           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/03/95 
!!      V.Masson    20/03/96  remove abnormal averages and average TS**4 instead
!!                            of TS
!!      (J.Stein)   27/03/96 use only H and LE in the soil scheme
!!      A. Boone    27/11/02 revised to output ALMA variables, and general applications
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:), INTENT(IN) :: PFRAC_TILE ! Fraction in a mesh-area of 
!                                              ! a given surface
REAL, DIMENSION(:,:), INTENT(IN) :: PSFTH_TILE ! pot. temp. flux  (mK/s)
REAL, DIMENSION(:,:), INTENT(IN) :: PSFTQ_TILE ! water vapor flux (m kg/kg/s)
REAL, DIMENSION(:,:), INTENT(IN) :: PSFCO2_TILE! CO2 flux         (m kg/kg/s)
REAL, DIMENSION(:,:,:),INTENT(IN):: PSFTS_TILE ! scalar flux      (m kg/kg/s)
REAL, DIMENSION(:,:), INTENT(IN) :: PSFU_TILE  ! zonal momentum flux    (pa)
REAL, DIMENSION(:,:), INTENT(IN) :: PSFV_TILE  ! meridian momentum flux (pa)
REAL, DIMENSION(:),   INTENT(OUT):: PSFTH      ! pot. temp. flux  (mK/s)
REAL, DIMENSION(:),   INTENT(OUT):: PSFTQ      ! water vapor flux (m kg/kg/s)
REAL, DIMENSION(:,:), INTENT(OUT):: PSFTS      ! scalar flux      (m kg/kg/s)
REAL, DIMENSION(:),   INTENT(OUT):: PSFCO2     ! CO2 flux         (m kg/kg/s)
REAL, DIMENSION(:),   INTENT(OUT):: PSFU       ! zonal momentum flux    (pa)
REAL, DIMENSION(:),   INTENT(OUT):: PSFV       ! meridian momentum flux (pa)
!
!*      0.2    declarations of local variables
!
INTEGER :: JSV   ! scalar loop counter
INTEGER :: JTILE ! tile loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!       0.     Initialization
!              --------------
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_FLUX',0,ZHOOK_HANDLE)
PSFTH      (:)   = 0.
PSFTQ      (:)   = 0.
PSFCO2     (:)   = 0.
PSFU       (:)   = 0.
PSFV       (:)   = 0.
PSFTS      (:,:) = 0.
!
!       1.     Grid-Box average 1d fluxes
!              --------------------------
!
!
DO JTILE = 1, SIZE(PSFTH_TILE,2)
!
!  potential temperature flux:
!
   PSFTH(:)  = PSFTH(:) + PFRAC_TILE(:,JTILE) * PSFTH_TILE(:,JTILE)
!
!  water vapor flux:
!
   PSFTQ(:)  = PSFTQ(:) + PFRAC_TILE(:,JTILE) * PSFTQ_TILE(:,JTILE)
!
!  carbon flux:
!
   PSFCO2(:)  = PSFCO2(:) + PFRAC_TILE(:,JTILE) * PSFCO2_TILE(:,JTILE)
!
!  wind surface friction:
!
   PSFU(:)  = PSFU(:) + PFRAC_TILE(:,JTILE) * PSFU_TILE(:,JTILE)
   PSFV(:)  = PSFV(:) + PFRAC_TILE(:,JTILE) * PSFV_TILE(:,JTILE)
!   
END DO
!
!
!
!       2.     Grid-Box average 2d fluxes
!              --------------------------
!
DO JSV = 1, SIZE(PSFTS_TILE,2)
!
   DO JTILE = 1, SIZE(PSFTS_TILE,3)
!
!     scalar flux
!
      PSFTS(:,JSV) = PSFTS(:,JSV) + PFRAC_TILE(:,JTILE) * PSFTS_TILE(:,JSV,JTILE)
!
   END DO
!
END DO
IF (LHOOK) CALL DR_HOOK('AVERAGE_FLUX',1,ZHOOK_HANDLE)

!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGE_FLUX
