!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE AVERAGE_DIAG_ISBA_n (DGO, D, DC, ND, NDC, NP, KNPATCH, OSURF_BUDGETC, &
                                      OCANOPY, PHW, PHT ,PSFCO2, PTRAD)
!     #######################################
!
!
!!****  *AVERAGE_DIAG_ISBA_n*  
!!
!!    PURPOSE
!!    -------
!      Average the diagnostics from all ISBA tiles
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
!!	S. Belair           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/03/95 
!!      V.Masson    20/03/96  remove abnormal averages and average TS**4 instead
!!                            of TS
!!      (J.Stein)   27/03/96 use only H and LE in the soil scheme
!!      A. Boone    27/11/02 revised to output ALMA variables, and general applications
!!      B. Decharme 17/08/09 cumulative radiatif budget
!!      V. Masson   10/2013  coherence between canopy and min/max T2M diagnostics
!!      B. Decharme    04/13 Averaged Trad already done in average_diag.F90
!!                           Good dimension for CO2 flux
!!      P. Samuelsson  10/13 Added min max for XT2M
!!      B. Decharme    02/15 No dependence on HW for 10M Wind diags
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_DIAG_n, ONLY : DIAG_t, DIAG_NP_t, DIAG_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_NP_t
!
USE MODD_SURF_PAR,    ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DGO
TYPE(DIAG_t), INTENT(INOUT) :: D
TYPE(DIAG_t), INTENT(INOUT) :: DC
TYPE(DIAG_NP_t), INTENT(INOUT) :: ND
TYPE(DIAG_NP_t), INTENT(INOUT) :: NDC
TYPE(ISBA_NP_t), INTENT(INOUT) :: NP
INTEGER, INTENT(IN) :: KNPATCH
!
LOGICAL, INTENT(IN) :: OSURF_BUDGETC
LOGICAL, INTENT(IN) :: OCANOPY
!
REAL, DIMENSION(:), INTENT(IN)       :: PHW    ! atmospheric level height for wind (m)
REAL, DIMENSION(:), INTENT(IN)       :: PHT    ! atmospheric level height (m)
REAL, DIMENSION(:), INTENT(IN)       :: PSFCO2 ! CO2 flux (m/s*kg_CO2/kg_air)
REAL, DIMENSION(:), INTENT(IN)       :: PTRAD  ! Radiative temperature (K)
!
!*      0.2    declarations of local variables
!
INTEGER                              :: JP, JI, IMASK ! tile loop counter
INTEGER                              :: JSWB   ! band loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!       0.     Initialization
!              --------------
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG_ISBA_N',0,ZHOOK_HANDLE)
!
!       1.     Energy fluxes
!              -------------
!
IF (DGO%LSURF_BUDGET) THEN
  !
  CALL MAKE_AVERAGE(D,ND)
  !
  D%XSWBD(:,:) = 0.
  D%XSWBU(:,:) = 0.
  !
  DO JP=1,KNPATCH
    DO JI = 1,NP%AL(JP)%NSIZE_P
      IMASK = NP%AL(JP)%NR_P(JI)

      DO JSWB =1,SIZE(D%XSWBD,2)
        !
        ! Downwards SW radiation for each spectral band
        D%XSWBD(IMASK,JSWB) = D%XSWBD(IMASK,JSWB) + NP%AL(JP)%XPATCH(JI) * ND%AL(JP)%XSWBD(JI,JSWB)
        !
        ! Upwards SW radiation for each spectral band
        D%XSWBU(IMASK,JSWB) = D%XSWBU(IMASK,JSWB) + NP%AL(JP)%XPATCH(JI) * ND%AL(JP)%XSWBU(JI,JSWB)
        !
      END DO

    ENDDO
  END DO
  !
END IF
!
IF (OSURF_BUDGETC) THEN
  !
  CALL MAKE_AVERAGE(DC,NDC)
  !
ENDIF    
!
!       2.     surface temperature and 2 meters parameters
!              -------------------------------------------
!
D%XTS  (:) = 0.0
D%XALBT(:) = 0.0
DO JP=1,KNPATCH
  DO JI = 1,NP%AL(JP)%NSIZE_P
    IMASK = NP%AL(JP)%NR_P(JI)

    D%XTS(IMASK) = D%XTS(IMASK) + NP%AL(JP)%XPATCH(JI) * ND%AL(JP)%XTS(JI)
    !   Total albedo
    D%XALBT(IMASK) = D%XALBT(IMASK) + NP%AL(JP)%XPATCH(JI) * ND%AL(JP)%XALBT(JI)  

  ENDDO
END DO
!
IF (.NOT. OCANOPY .AND. DGO%N2M>=1) THEN

  D%XT2M(:)  = 0.
  D%XQ2M(:)  = 0.
  D%XHU2M(:)  = 0.
  !
  DO JP=1,KNPATCH
    DO JI = 1,NP%AL(JP)%NSIZE_P
      IMASK = NP%AL(JP)%NR_P(JI) 
      !
      ! 2 meters temperature
      D%XT2M(IMASK) = D%XT2M(IMASK) + NP%AL(JP)%XPATCH(JI) * ND%AL(JP)%XT2M(JI)
      !
      ! 2 meters humidity
      D%XQ2M(IMASK) = D%XQ2M(IMASK) + NP%AL(JP)%XPATCH(JI) * ND%AL(JP)%XQ2M(JI)
      !
      ! 2 meters relative humidity
      D%XHU2M(IMASK) = D%XHU2M(IMASK) + NP%AL(JP)%XPATCH(JI) * ND%AL(JP)%XHU2M(JI)
      ! 
    ENDDO
  END DO
  !
  ! 10 meters wind
  !
  D%XZON10M (:)  = 0.
  D%XMER10M (:)  = 0.
  D%XWIND10M(:)  = 0.
  DO JP=1,KNPATCH
    DO JI = 1,NP%AL(JP)%NSIZE_P
      IMASK = NP%AL(JP)%NR_P(JI)  

      D%XZON10M(IMASK)  = D%XZON10M (IMASK) + NP%AL(JP)%XPATCH(JI) * ND%AL(JP)%XZON10M (JI)
      D%XMER10M(IMASK)  = D%XMER10M (IMASK) + NP%AL(JP)%XPATCH(JI) * ND%AL(JP)%XMER10M (JI)
      D%XWIND10M(IMASK) = D%XWIND10M(IMASK) + NP%AL(JP)%XPATCH(JI) * ND%AL(JP)%XWIND10M(JI)
    ENDDO
  ENDDO
  !
  ! min and max of XT2M
  !
  DO JP=1,KNPATCH
    ND%AL(JP)%XT2M_MIN(:) = MIN(ND%AL(JP)%XT2M_MIN(:),ND%AL(JP)%XT2M(:))
    ND%AL(JP)%XT2M_MAX(:) = MAX(ND%AL(JP)%XT2M_MAX(:),ND%AL(JP)%XT2M(:))
  ENDDO
  !  
  D%XT2M_MIN(:) = MIN(D%XT2M_MIN(:),D%XT2M(:))
  D%XT2M_MAX(:) = MAX(D%XT2M_MAX(:),D%XT2M(:))
  !
  D%XHU2M_MIN(:) = MIN(D%XHU2M_MIN(:),D%XHU2M(:))
  D%XHU2M_MAX(:) = MAX(D%XHU2M_MAX(:),D%XHU2M(:))
  !
  D%XWIND10M_MAX(:) = MAX(D%XWIND10M_MAX(:),D%XWIND10M(:))
  !
END IF
!
! Richardson number
!
IF (DGO%N2M>=1) THEN

  D%XRI(:)  = 0.
  D%XSFCO2(:)  = PSFCO2(:)
  DO JP=1,KNPATCH
    DO JI = 1,NP%AL(JP)%NSIZE_P
      IMASK = NP%AL(JP)%NR_P(JI)    
      D%XRI(IMASK) = D%XRI(IMASK) + NP%AL(JP)%XPATCH(JI) * ND%AL(JP)%XRI(JI)
    ENDDO
  END DO
  !
END IF
!
!       3.     Transfer coefficients
!              ---------------------
!
IF (DGO%LCOEF) THEN
  !
  D%XCD   (:) = 0.
  D%XCH   (:) = 0.
  D%XCE   (:) = 0.
  D%XZ0   (:) = 0.
  D%XZ0H  (:) = 0.
  D%XZ0EFF(:) = 0.
  !
  DO JP=1,KNPATCH
    DO JI = 1,NP%AL(JP)%NSIZE_P
      IMASK = NP%AL(JP)%NR_P(JI)    
      !
      D%XCD(IMASK)  = D%XCD(IMASK) + NP%AL(JP)%XPATCH(JI) * ND%AL(JP)%XCD(JI)
      D%XCH(IMASK)  = D%XCH(IMASK) + NP%AL(JP)%XPATCH(JI) * ND%AL(JP)%XCH(JI)
      D%XCE(IMASK)  = D%XCE(IMASK) + NP%AL(JP)%XPATCH(JI) * ND%AL(JP)%XCE(JI)
      !            
      D%XZ0(IMASK)    = D%XZ0(IMASK)    + NP%AL(JP)%XPATCH(JI) * &
              1./(LOG(PHW(IMASK)/ND%AL(JP)%XZ0 (JI)))**2  
      D%XZ0H(IMASK)   = D%XZ0H(IMASK)   + NP%AL(JP)%XPATCH(JI) * &
              1./(LOG(PHT(IMASK)/ND%AL(JP)%XZ0H(JI)))**2   
      D%XZ0EFF(IMASK) = D%XZ0EFF(IMASK) + NP%AL(JP)%XPATCH(JI) * &
              1./(LOG(PHW(IMASK)/ND%AL(JP)%XZ0EFF(JI)))**2
      !      
    ENDDO
  END DO
  !
  D%XZ0(:)    = PHW(:) *  EXP( - SQRT(1./D%XZ0(:)) )
  !
  D%XZ0H(:)   = PHT(:) *  EXP( - SQRT(1./D%XZ0H(:)) )
  !
  D%XZ0EFF(:) = PHW(:) *  EXP( - SQRT(1./D%XZ0EFF(:)) )
  !
END IF
!
IF (DGO%LSURF_VARS) THEN
  D%XQS(:)  = 0.
  !
  DO JP=1,KNPATCH
    DO JI = 1,NP%AL(JP)%NSIZE_P  
      IMASK = NP%AL(JP)%NR_P(JI)    
      !
      ! specific humidity at surface
      D%XQS(IMASK) = D%XQS(IMASK) + NP%AL(JP)%XPATCH(JI) * ND%AL(JP)%XQS(JI)
      !
    ENDDO
  END DO
END IF
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG_ISBA_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
CONTAINS
!
SUBROUTINE MAKE_AVERAGE(DA,NDA)
!
TYPE(DIAG_t), INTENT(INOUT) :: DA
TYPE(DIAG_NP_t), INTENT(INOUT) :: NDA
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG_ISBA_N:MAKE_AVERAGE',0,ZHOOK_HANDLE)
!
DA%XRN   (:) = 0.
DA%XH    (:) = 0.
DA%XLE   (:) = 0.
DA%XLEI  (:) = 0.
DA%XGFLUX(:) = 0.
!
DA%XSWD(:) = 0.
DA%XSWU(:) = 0.
DA%XLWD(:) = 0.
DA%XLWU(:) = 0.
DA%XFMU(:) = 0.
DA%XFMV(:) = 0.
!
DA%XEVAP (:) = 0.
DA%XSUBL (:) = 0.
!
DO JP=1,KNPATCH
  DO JI = 1,NP%AL(JP)%NSIZE_P 
    IMASK = NP%AL(JP)%NR_P(JI) 
    !
    ! Net radiation
    DA%XRN   (IMASK) = DA%XRN   (IMASK) + NP%AL(JP)%XPATCH(JI) * NDA%AL(JP)%XRN(JI)
    !
    ! Sensible heat flux
    DA%XH    (IMASK) = DA%XH    (IMASK) + NP%AL(JP)%XPATCH(JI) * NDA%AL(JP)%XH(JI)
    !
    ! Total latent heat flux
    DA%XLE   (IMASK) = DA%XLE   (IMASK) + NP%AL(JP)%XPATCH(JI) * NDA%AL(JP)%XLE(JI)
    !
    ! Storage flux
    DA%XGFLUX(IMASK) = DA%XGFLUX(IMASK) + NP%AL(JP)%XPATCH(JI) * NDA%AL(JP)%XGFLUX(JI)
    !
    ! Total surface sublimation
    DA%XLEI  (IMASK) = DA%XLEI  (IMASK) + NP%AL(JP)%XPATCH(JI) * NDA%AL(JP)%XLEI(JI)          
    !
    ! Evapotranspiration
    DA%XEVAP (IMASK) = DA%XEVAP (IMASK) + NP%AL(JP)%XPATCH(JI) * NDA%AL(JP)%XEVAP(JI)
    !
    !  Sublimation
    DA%XSUBL (IMASK) = DA%XSUBL (IMASK) + NP%AL(JP)%XPATCH(JI) * NDA%AL(JP)%XSUBL(JI)
    !
    !  Downwards SW radiation
    DA%XSWD  (IMASK) = DA%XSWD  (IMASK) + NP%AL(JP)%XPATCH(JI) * NDA%AL(JP)%XSWD(JI)
    !
    !    Upwards SW radiation
    DA%XSWU  (IMASK) = DA%XSWU  (IMASK) + NP%AL(JP)%XPATCH(JI) * NDA%AL(JP)%XSWU(JI)
    !
    !    Downwards LW radiation
    DA%XLWD  (IMASK) = DA%XLWD  (IMASK) + NP%AL(JP)%XPATCH(JI) * NDA%AL(JP)%XLWD(JI)
    !
    !    Upwards LW radiation
    DA%XLWU  (IMASK) = DA%XLWU  (IMASK) + NP%AL(JP)%XPATCH(JI) * NDA%AL(JP)%XLWU(JI)
    !
    !    Zonal wind stress
    DA%XFMU  (IMASK) = DA%XFMU  (IMASK) + NP%AL(JP)%XPATCH(JI) * NDA%AL(JP)%XFMU(JI)
    !
    !    Meridian wind stress
    DA%XFMV  (IMASK) = DA%XFMV  (IMASK) + NP%AL(JP)%XPATCH(JI) * NDA%AL(JP)%XFMV(JI)
    !
  ENDDO
END DO
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG_ISBA_N:MAKE_AVERAGE',1,ZHOOK_HANDLE)
!
END SUBROUTINE MAKE_AVERAGE
!
!
END SUBROUTINE AVERAGE_DIAG_ISBA_n
