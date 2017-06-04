!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
       SUBROUTINE DIAG_CPL_ESM_WATER (W, D, OCPL_SEAICE, PTSTEP, PSFTQ, PRAIN, PSNOW, PLW,   &
                                      PSFTH_ICE, PSFTQ_ICE, PDIR_SW, PSCA_SW    )  
!     #####################################################################
!
!!****  *DIAG_CPL_ESM_WATER * - Computes diagnostics over sea for 
!!                                Earth system model coupling
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
!!      Original    08/2009
!!------------------------------------------------------------------
!
!
USE MODD_DIAG_n, ONLY : DIAG_t
USE MODD_WATFLUX_n, ONLY : WATFLUX_t
!
USE MODD_CSTS,      ONLY : XSTEFAN, XLSTT
USE MODD_WATER_PAR, ONLY : XEMISWATICE
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
TYPE(DIAG_t), INTENT(INOUT) :: D
TYPE(WATFLUX_t), INTENT(INOUT) :: W
!
LOGICAL,            INTENT(IN) :: OCPL_SEAICE ! sea-ice / ocean key
REAL,               INTENT(IN) :: PTSTEP    ! atmospheric time-step
REAL, DIMENSION(:), INTENT(IN) :: PSFTQ     ! water flux
REAL, DIMENSION(:), INTENT(IN) :: PRAIN     ! Rainfall
REAL, DIMENSION(:), INTENT(IN) :: PSNOW     ! Snowfall
REAL, DIMENSION(:), INTENT(IN) :: PLW       ! longwave radiation (on horizontal surf.)
REAL, DIMENSION(:), INTENT(IN) :: PSFTH_ICE ! heat flux  (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PSFTQ_ICE ! water flux (kg/m2/s)
REAL, DIMENSION(:,:),INTENT(IN):: PDIR_SW   ! direct  solar radiation (on horizontal surf.)
REAL, DIMENSION(:,:),INTENT(IN):: PSCA_SW   ! diffuse solar radiation (on horizontal surf.)
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(W%XICE_ALB)) :: ZSWU, ZTICE4
!
INTEGER                      :: ISWB ! number of SW bands
INTEGER                      :: JSWB ! loop counter on number of SW bands
INTEGER                      :: INI  ! number of points
INTEGER                      :: JI   ! loop counter on number of points
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('DIAG_CPL_ESM_WATER',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
! Total or free-ice water flux
!-------------------------------------------------------------------------------------
!
!* 10m wind speed (m)
!
W%XCPL_WATER_WIND(:) = W%XCPL_WATER_WIND(:) + PTSTEP * SQRT(D%XZON10M(:)**2+D%XMER10M(:)**2)
! 
!* wind stress (Pa.s)
!
W%XCPL_WATER_FWSU(:) = W%XCPL_WATER_FWSU(:) + PTSTEP * D%XFMU(:)
W%XCPL_WATER_FWSV(:) = W%XCPL_WATER_FWSV(:) + PTSTEP * D%XFMV(:)
W%XCPL_WATER_FWSM(:) = W%XCPL_WATER_FWSM(:) + PTSTEP * SQRT(D%XFMU(:)**2+D%XFMV(:)**2)
!
!* Solar net heat flux (J/m2)
!
W%XCPL_WATER_SNET(:) = W%XCPL_WATER_SNET(:) + PTSTEP * (D%XSWD(:) - D%XSWU(:))
!
!* Non solar heat flux (J/m2)
!
W%XCPL_WATER_HEAT(:) = W%XCPL_WATER_HEAT(:) + PTSTEP * (D%XGFLUX(:) + D%XSWU(:) - D%XSWD(:)) 
!
!* Evaporation (kg/m2)
!
W%XCPL_WATER_EVAP(:) = W%XCPL_WATER_EVAP(:) + PTSTEP * PSFTQ(:)
!
!* Precip (kg/m2)
! 
W%XCPL_WATER_RAIN(:) = W%XCPL_WATER_RAIN(:) + PTSTEP * PRAIN(:) 
W%XCPL_WATER_SNOW(:) = W%XCPL_WATER_SNOW(:) + PTSTEP * PSNOW(:)
!
!-------------------------------------------------------------------------------------
! Ice flux
!-------------------------------------------------------------------------------------
!
IF (OCPL_SEAICE) THEN
!
  INI  = SIZE(PDIR_SW,1)
  ISWB = SIZE(PDIR_SW,2)
!
!* Solar net heat flux (J/m2)
!
  ZSWU(:)=0.0
  DO JSWB=1,ISWB
     DO JI=1,INI
      ZSWU(JI) = ZSWU(JI) + (PDIR_SW(JI,JSWB)+PSCA_SW(JI,JSWB)) * W%XICE_ALB(JI)
     ENDDO
  ENDDO
!
  W%XCPL_WATERICE_SNET(:) = W%XCPL_WATERICE_SNET(:) + PTSTEP * (D%XSWD(:) - ZSWU(:))
!
!* Non solar heat flux (J/m2)
!
  ZTICE4(:)=W%XTICE(:)**4
!
  W%XCPL_WATERICE_HEAT(:) = W%XCPL_WATERICE_HEAT(:) + PTSTEP * ( XEMISWATICE*(PLW(:)-XSTEFAN*ZTICE4(:)) &
                                                             - PSFTH_ICE(:) - XLSTT*PSFTQ_ICE(:)  )  
!
!* Sublimation (kg/m2)
!
  W%XCPL_WATERICE_EVAP(:) = W%XCPL_WATERICE_EVAP(:) + PTSTEP * PSFTQ_ICE(:)
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('DIAG_CPL_ESM_WATER',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_CPL_ESM_WATER
