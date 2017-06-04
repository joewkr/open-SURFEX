!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
 SUBROUTINE DIAG_INLINE_ISBA_n (DGO, KK, DK, OCANOPY, PTA, PQA, PPA, PPS, PRHOA, PZONA, PMERA, &
                                  PHT, PHW, PSFTH, PSFTQ, PSFZON, PSFMER, PDIR_SW, PSCA_SW, PLW )  
!     ###############################################################################
!
!!****  *DIAG_INLINE_ISBA_n * - computes diagnostics during ISBA time-step
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
!!      B. Decharme 08/2009 caculate cumulated diag LSURF_BUDGETC
!!      S. Riette   06/2009 CLS_2M becomes CLS_TQ, CLS_TQ and CLS_WIND have one
!!                          more argument (height of diagnostic)
!!------------------------------------------------------------------
!
!
!
!
USE MODD_DIAG_n, ONLY : DIAG_t, DIAG_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_K_t
!
USE MODD_SURF_PAR,         ONLY : XUNDEF
!
USE MODI_CLS_TQ
USE MODI_CLS_WIND
USE MODI_DIAG_SURF_BUDGET_ISBA
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
TYPE(ISBA_K_t), INTENT(INOUT) :: KK
TYPE(DIAG_t), INTENT(INOUT) :: DK
!
LOGICAL, INTENT(IN) :: OCANOPY
!
REAL, DIMENSION(:), INTENT(IN)       :: PTA      ! atmospheric temperature
REAL, DIMENSION(:), INTENT(IN)       :: PQA      ! atmospheric specific humidity
REAL, DIMENSION(:), INTENT(IN)       :: PPA      ! atmospheric level pressure
REAL, DIMENSION(:), INTENT(IN)       :: PPS      ! surface pressure
REAL, DIMENSION(:), INTENT(IN)       :: PRHOA    ! air density
REAL, DIMENSION(:), INTENT(IN)       :: PZONA    ! zonal wind
REAL, DIMENSION(:), INTENT(IN)       :: PMERA    ! meridian wind
REAL, DIMENSION(:), INTENT(IN)       :: PHT      ! atmospheric level height
REAL, DIMENSION(:), INTENT(IN)       :: PHW      ! atmospheric level height for wind
REAL, DIMENSION(:,:), INTENT(IN)     :: PDIR_SW  ! direct  solar radiation (on horizontal surf.)
REAL, DIMENSION(:,:), INTENT(IN)     :: PSCA_SW  ! diffuse solar radiation (on horizontal surf.)
REAL, DIMENSION(:), INTENT(IN)       :: PLW      ! longwave radiation (on horizontal surf.)
!
REAL, DIMENSION(:), INTENT(IN)       :: PSFZON   ! zonal friction
REAL, DIMENSION(:), INTENT(IN)       :: PSFMER   ! meridian friction
REAL, DIMENSION(:), INTENT(IN)       :: PSFTH    ! heat flux (W/m2)
REAL, DIMENSION(:), INTENT(IN)       :: PSFTQ    ! water flux (kg/m2/s)
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PTA)) :: ZH
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_INLINE_ISBA_N',0,ZHOOK_HANDLE)
!
! * Near surface atmospheric variables
!
IF (.NOT. OCANOPY) THEN
  !        
  IF (DGO%N2M==2) THEN
    ZH(:)=2.          
    CALL CLS_TQ(PTA, PQA, PPA, PPS, PHT, DK%XCD, DK%XCH, DK%XRI, &
                DK%XTS, DK%XHU, DK%XZ0H, ZH, DK%XT2M, DK%XQ2M, DK%XHU2M )  
    ZH(:)=10.                
    CALL CLS_WIND(PZONA, PMERA, PHW, DK%XCD, DK%XCDN, DK%XRI, ZH, &
                 DK%XZON10M, DK%XMER10M  )  
  END IF
  !
  IF (DGO%N2M>=1) DK%XWIND10M(:) = SQRT(DK%XZON10M(:)**2 + DK%XMER10M(:)**2)
  !
ELSE
  !        
  IF (DGO%N2M>=1) THEN
    DK%XT2M    = XUNDEF
    DK%XQ2M    = XUNDEF
    DK%XHU2M   = XUNDEF
    DK%XZON10M = XUNDEF
    DK%XMER10M = XUNDEF
    DK%XWIND10M= XUNDEF
  ENDIF
  !        
ENDIF
!
! * Surface energy budget
!
IF (DGO%LSURF_BUDGET.OR.DGO%LSURF_BUDGETC) THEN
   !
   CALL DIAG_SURF_BUDGET_ISBA(PDIR_SW, PSCA_SW, PLW, KK, DK)          
   !
   DK%XFMU = PSFZON
   DK%XFMV = PSFMER
   !
END IF
!
IF (DGO%LCOEF) THEN
  !
  !* Transfer coefficient
  !
  DK%XCD = DK%XCD
  DK%XCH = DK%XCH
  DK%XCE = DK%XCH
  !
  !* Roughness lengths
  !
  DK%XZ0    = DK%XZ0
  DK%XZ0H   = DK%XZ0H
  DK%XZ0EFF = DK%XZ0EFF
  !
ENDIF
!
IF (DGO%LSURF_VARS) THEN
  !
  !* Humidity at surface
  !
  DK%XQS = DK%XQS
  !
ENDIF
IF (LHOOK) CALL DR_HOOK('DIAG_INLINE_ISBA_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_INLINE_ISBA_n
