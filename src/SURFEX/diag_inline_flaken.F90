!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
       SUBROUTINE DIAG_INLINE_FLAKE_n (DGO, D, DC, F, &
                                       PTSTEP, PTA, PQA, PPA, PPS, PRHOA, PZONA,  &
                                       PMERA, PHT, PHW, PRAIN, PSNOW,             &
                                       PCD, PCDN, PCH, PRI, PHU,                  &
                                       PZ0H, PQSAT, PSFTH, PSFTQ, PSFZON, PSFMER, &
                                       PDIR_SW, PSCA_SW, PLW, PDIR_ALB, PSCA_ALB, &
                                       PLE, PLEI, PSUBL, PLWUP, PALB, PSWE        )  
!     ###############################################################################
!
!!****  *DIAG_INLINE_FLAKE_n * - computes diagnostics during FLAKE time-step
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
!!      S. Riette    06/2009 CLS_2M becomes CLS_TQ, CLS_TQ and CLS_WIND have one
!!                           more argument (height of diagnostic)
!!      P. Le Moigne 04/2013 : Accumulated diagnostics
!!                             Coupling for ESM
!!------------------------------------------------------------------
!

!
!
!
USE MODD_DIAG_n, ONLY : DIAG_t, DIAG_OPTIONS_t
USE MODD_FLAKE_n, ONLY : FLAKE_t
!
USE MODD_CSTS,         ONLY : XTT
USE MODD_SURF_PAR,     ONLY : XUNDEF
USE MODD_SFX_OASIS,    ONLY : LCPL_LAKE
!
USE MODI_CLS_TQ
USE MODI_CLS_WIND
USE MODI_DIAG_SURF_BUDGET_FLAKE
USE MODI_DIAG_SURF_BUDGETC
USE MODI_DIAG_CPL_ESM_FLAKE
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(DIAG_OPTIONS_t), INTENT(IN) :: DGO
TYPE(DIAG_t), INTENT(INOUT) :: D
TYPE(DIAG_t), INTENT(INOUT) :: DC
TYPE(FLAKE_t), INTENT(INOUT) :: F
!
REAL              , INTENT(IN) :: PTSTEP ! atmospheric time-step (s)
REAL, DIMENSION(:), INTENT(IN) :: PTA    ! atmospheric temperature
REAL, DIMENSION(:), INTENT(IN) :: PQA    ! atmospheric specific humidity
REAL, DIMENSION(:), INTENT(IN) :: PPA    ! atmospheric level pressure
REAL, DIMENSION(:), INTENT(IN) :: PPS    ! surface pressure
REAL, DIMENSION(:), INTENT(IN) :: PRHOA  ! air density
REAL, DIMENSION(:), INTENT(IN) :: PZONA  ! zonal wind
REAL, DIMENSION(:), INTENT(IN) :: PMERA  ! meridian wind
REAL, DIMENSION(:), INTENT(IN) :: PHT    ! atmospheric level height
REAL, DIMENSION(:), INTENT(IN) :: PHW    ! atmospheric level height for wind
REAL, DIMENSION(:), INTENT(IN) :: PRAIN  ! Rainfall (kg/m2/s)
REAL, DIMENSION(:), INTENT(IN) :: PSNOW  ! Snowfall (kg/m2/s)
REAL, DIMENSION(:), INTENT(IN) :: PCD    ! drag coefficient for momentum
REAL, DIMENSION(:), INTENT(IN) :: PCDN   ! neutral drag coefficient
REAL, DIMENSION(:), INTENT(IN) :: PCH    ! drag coefficient for heat
REAL, DIMENSION(:), INTENT(IN) :: PRI    ! Richardson number
REAL, DIMENSION(:), INTENT(IN) :: PHU    ! near-surface humidity
REAL, DIMENSION(:), INTENT(IN) :: PZ0H   ! roughness length for heat
REAL, DIMENSION(:), INTENT(IN) :: PQSAT  ! humidity at saturation
REAL, DIMENSION(:), INTENT(IN) :: PSFZON ! zonal friction
REAL, DIMENSION(:), INTENT(IN) :: PSFMER ! meridian friction
REAL, DIMENSION(:), INTENT(IN) :: PSFTH  ! heat flux  (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PSFTQ  ! water flux (kg/m2/s)
REAL, DIMENSION(:,:),INTENT(IN):: PDIR_SW   ! direct  solar radiation (on horizontal surf.)
!                                           !                                       (W/m2)
REAL, DIMENSION(:,:),INTENT(IN):: PSCA_SW   ! diffuse solar radiation (on horizontal surf.)
!                                           !                                       (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PLW       ! longwave radiation (on horizontal surf.) (W/m2)
REAL, DIMENSION(:,:),INTENT(IN):: PDIR_ALB  ! direct albedo for each spectral band  (-)
REAL, DIMENSION(:,:),INTENT(IN):: PSCA_ALB  ! diffuse albedo for each spectral band (-)
REAL, DIMENSION(:), INTENT(IN) :: PLE       ! total latent heat flux (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PLEI      ! sublimation heat flux (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PSUBL     ! sublimation (kg/m2/s)
REAL, DIMENSION(:), INTENT(IN) :: PLWUP     ! upward longwave radiation (W/m2)
!
REAL, DIMENSION(:), INTENT(IN) :: PALB      ! Flake total albedo
REAL, DIMENSION(:), INTENT(IN) :: PSWE      ! Flake snow water equivalent (kg.m-2)
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PTA)) :: ZH
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_INLINE_FLAKE_N',0,ZHOOK_HANDLE)
!
D%XTS(:) = F%XTS(:)
!
IF (.NOT. F%LSBL) THEN
!
  IF (DGO%N2M==2) THEN
    ZH(:)=2.          
    CALL CLS_TQ(PTA, PQA, PPA, PPS, PHT, PCD, PCH, PRI, &
                  F%XTS, PHU, PZ0H, ZH, D%XT2M, D%XQ2M, D%XHU2M )  
    ZH(:)=10.                
    CALL CLS_WIND(PZONA, PMERA, PHW, PCD, PCDN, PRI, ZH, D%XZON10M, D%XMER10M )  
  END IF
!
  IF (DGO%N2M>=1) THEN
    !
    D%XT2M_MIN(:) = MIN(D%XT2M_MIN(:),D%XT2M(:))
    D%XT2M_MAX(:) = MAX(D%XT2M_MAX(:),D%XT2M(:))
    !
    D%XHU2M_MIN(:) = MIN(D%XHU2M_MIN(:),D%XHU2M(:))
    D%XHU2M_MAX(:) = MAX(D%XHU2M_MAX(:),D%XHU2M(:))
    !
    D%XWIND10M(:) = SQRT(D%XZON10M(:)**2+D%XMER10M(:)**2)
    D%XWIND10M_MAX(:) = MAX(D%XWIND10M_MAX(:),D%XWIND10M(:))
    !
    !* Richardson number
    D%XRI = PRI
    !
  ENDIF
!
ELSE
  !
  IF (DGO%N2M>=1) THEN
    D%XT2M    = XUNDEF
    D%XQ2M    = XUNDEF
    D%XHU2M   = XUNDEF
    D%XZON10M = XUNDEF
    D%XMER10M = XUNDEF
    D%XRI     = PRI
  ENDIF
ENDIF
!
IF (DGO%LSURF_BUDGET.OR.DGO%LSURF_BUDGETC) THEN
  !
  D%XLE  (:) = PLE  (:)
  D%XLEI (:) = PLEI (:)
  D%XEVAP(:) = PSFTQ(:)
  D%XSUBL(:) = PSUBL(:)
  D%XALBT(:) = PALB (:)
  D%XSWE (:) = PSWE (:)
  !
  CALL  DIAG_SURF_BUDGET_FLAKE (D, PRHOA, PSFTH, PDIR_SW, PSCA_SW, PLW, &
                                PDIR_ALB, PSCA_ALB, PLWUP, PSFZON, PSFMER )  
  !
END IF
!
IF(DGO%LSURF_BUDGETC)THEN
  CALL DIAG_SURF_BUDGETC(D, DC, PTSTEP, .TRUE.)  
ENDIF
!
IF (DGO%LCOEF) THEN
  !
  !* Transfer coefficients
  !
  D%XCD = PCD
  D%XCH = PCH
  D%XCE = PCH
  !
  !* Roughness lengths
  !
  D%XZ0  = F%XZ0
  D%XZ0H = PZ0H
  !
END IF
!
IF (DGO%LSURF_VARS) THEN
  !
  !* Humidity at saturation
  !
  D%XQS = PQSAT
  !
END IF
!
! Diag for Earth System Model coupling
!
IF (LCPL_LAKE) THEN
!
  CALL DIAG_CPL_ESM_FLAKE(F,PTSTEP,PRAIN,PSNOW,PSFTQ)
! 
ENDIF
!
IF (LHOOK) CALL DR_HOOK('DIAG_INLINE_FLAKE_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_INLINE_FLAKE_n
