!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
       SUBROUTINE DIAG_SURF_BUDGET_FLAKE (D, PRHOA, PSFTH, PDIR_SW, PSCA_SW, PLW,  &
                                          PDIR_ALB, PSCA_ALB, PLWUP, PSFZON, PSFMER  )  
!     ###############################################################################
!
!!****  *DIAG_SURF_BUDGET_FLAKE * - Computes diagnostics over lake
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
USE MODD_DIAG_n, ONLY : DIAG_t
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(DIAG_t), INTENT(INOUT) :: D
!
REAL, DIMENSION(:), INTENT(IN) :: PRHOA     ! air density
REAL, DIMENSION(:), INTENT(IN) :: PSFTH     ! heat flux
REAL, DIMENSION(:,:),INTENT(IN):: PDIR_SW   ! direct  solar radiation (on horizontal surf.)
!                                           !                                       (W/m2)
REAL, DIMENSION(:,:),INTENT(IN):: PSCA_SW   ! diffuse solar radiation (on horizontal surf.)
!                                           !                                       (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PLW       ! longwave radiation (on horizontal surf.)
REAL, DIMENSION(:), INTENT(IN) :: PLWUP     ! upward longwave radiation             (W/m2)
REAL, DIMENSION(:,:),INTENT(IN):: PDIR_ALB  ! direct albedo for each spectral band  (-)
REAL, DIMENSION(:,:),INTENT(IN):: PSCA_ALB  ! diffuse albedo for each spectral band (-)
REAL, DIMENSION(:), INTENT(IN) :: PSFZON    ! zonal friction
REAL, DIMENSION(:), INTENT(IN) :: PSFMER    ! meridional friction
!
!*      0.2    declarations of local variables
!
INTEGER                      :: ISWB ! number of SW bands
INTEGER                      :: JSWB ! loop counter on number of SW bands
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
!
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_BUDGET_FLAKE',0,ZHOOK_HANDLE)
ISWB = SIZE(PDIR_SW,2)
! 
!* total incoming and outgoing SW
!
DO JSWB=1,ISWB
  D%XSWBD(:,JSWB) = PDIR_SW(:,JSWB)                    + PSCA_SW(:,JSWB)
  D%XSWBU(:,JSWB) = PDIR_SW(:,JSWB) * PDIR_ALB(:,JSWB) + PSCA_SW(:,JSWB) * PSCA_ALB(:,JSWB) 
ENDDO
!
D%XSWD(:) = 0.
D%XSWU(:) = 0.
DO JSWB=1,ISWB
   D%XSWD(:)=D%XSWD(:)+D%XSWBD(:,JSWB)
   D%XSWU(:)=D%XSWU(:)+D%XSWBU(:,JSWB)
ENDDO
!
!*incoming outgoing LW
!
D%XLWD(:)=PLW  (:)
D%XLWU(:)=PLWUP(:)
!
!* net radiation
!
D%XRN    =   D%XSWD(:) - D%XSWU(:) + D%XLWD(:) - D%XLWU(:)
!
!* sensible heat flux
!
D%XH     = PSFTH(:)
!
!* storage flux
!
D%XGFLUX = D%XRN - D%XH - D%XLE
!
!* wind stress
!
D%XFMU = PSFZON
!
D%XFMV = PSFMER
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_BUDGET_FLAKE',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_SURF_BUDGET_FLAKE
