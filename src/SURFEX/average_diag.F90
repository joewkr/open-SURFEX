!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE AVERAGE_DIAG(PFRAC_TILE, DGO, D, ND, DC, NDC      )                                
!     ######################################################################
!
!
!!****  *AVERAGE_DIAG*  
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
!!    V. Masson * Meteo-France-
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    06/2003
!!      Modified    08/2009 (B. Decharme) : new diag
!     02/2010 - S. Riette - Security for wind average in case of XUNDEF values
!       B. decharme 04/2013 : Add EVAP and SUBL diag
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_DATA_COVER_PAR, ONLY : NTILESFC
!
USE MODD_DIAG_n, ONLY : DIAG_t, DIAG_NP_t, DIAG_OPTIONS_t
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:), INTENT(IN) :: PFRAC_TILE   ! Fraction in a mesh-area of 
!
TYPE(DIAG_OPTIONS_t), INTENt(INOUT) :: DGO
TYPE(DIAG_t), INTENT(INOUT) :: D
TYPE(DIAG_NP_t), INTENT(INOUT) :: ND
TYPE(DIAG_t), INTENT(INOUT) :: DC
TYPE(DIAG_NP_t), INTENT(INOUT) :: NDC
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PFRAC_TILE,1))    :: ZLAND, ZSEA, ZFRL
INTEGER :: JT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!       1.     Grid-Box average fluxes
!              -----------------------
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG',0,ZHOOK_HANDLE)
!
IF (DGO%LSURF_BUDGET) THEN
!
  DO JT = 1,NTILESFC
  !
  ! Net radiation
  !
  CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),ND%AL(JT)%XRN,D%XRN,JT)
!
! Sensible heat flux
!
  CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),ND%AL(JT)%XH,D%XH,JT)
!
! Total latent heat flux
!
  CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),ND%AL(JT)%XLE,D%XLE,JT)
!
! Sublimation latent heat flux
!
  CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),ND%AL(JT)%XLEI,D%XLEI,JT)
!
! Total evapotranspiration
!
  CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),ND%AL(JT)%XEVAP,D%XEVAP,JT)
!
! Sublimation
!
  CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),ND%AL(JT)%XSUBL,D%XSUBL,JT)
!
! Storage flux
!
  CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),ND%AL(JT)%XGFLUX,D%XGFLUX,JT)
!
! Downwards short wave radiation
!
  CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),ND%AL(JT)%XSWD,D%XSWD,JT)
!
! Upwards short wave radiation
!
  CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),ND%AL(JT)%XSWU,D%XSWU,JT)
!
! Downwards long wave radiation
!
  CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),ND%AL(JT)%XLWD,D%XLWD,JT)
!
! Upwards long wave radiation
!
  CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),ND%AL(JT)%XLWU,D%XLWU,JT)
!
! Zonal wind stress
!
  CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),ND%AL(JT)%XFMU,D%XFMU,JT)
!
! Meridian wind stress
!
  CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),ND%AL(JT)%XFMV,D%XFMV,JT)
!
! Downwards short wave radiation for each spectral band
!
  CALL MAKE_AVERAGE_2D(PFRAC_TILE(:,JT),ND%AL(JT)%XSWBD,D%XSWBD,JT)
!
! Upwards short wave radiation for each spectral band
!
  CALL MAKE_AVERAGE_2D(PFRAC_TILE(:,JT),ND%AL(JT)%XSWBU,D%XSWBU,JT)
!
  ENDDO
  !
END IF
!
IF (DGO%LSURF_BUDGETC) THEN
!
  DO JT = 1,NTILESFC
  !
! Net radiation
!
  CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),NDC%AL(JT)%XRN,DC%XRN,JT)
!
! Sensible heat flux
!
  CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),NDC%AL(JT)%XH,DC%XH,JT)
!
! Total latent heat flux
!
  CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),NDC%AL(JT)%XLE,DC%XLE,JT)
!
! Sublimation latent heat flux
!
  CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),NDC%AL(JT)%XLEI,DC%XLEI,JT)
!
! Storage flux
!
  CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),NDC%AL(JT)%XGFLUX,DC%XGFLUX,JT)
!
! Total evapotranspiration
!
  CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),NDC%AL(JT)%XEVAP,DC%XEVAP,JT)
!
! Sublimation
!
  CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),NDC%AL(JT)%XSUBL,DC%XSUBL,JT)
!
! Downwards short wave radiation
!
  CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),NDC%AL(JT)%XSWD,DC%XSWD,JT)
!
! Upwards short wave radiation
!
  CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),NDC%AL(JT)%XSWU,DC%XSWU,JT)
!
! Downwards long wave radiation
!
  CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),NDC%AL(JT)%XLWD,DC%XLWD,JT)
!
! Upwards long wave radiation
!
  CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),NDC%AL(JT)%XLWU,DC%XLWU,JT)
!
! Zonal wind stress
!
  CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),NDC%AL(JT)%XFMU,DC%XFMU,JT)
!
! Meridian wind stress
!
  CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),NDC%AL(JT)%XFMV,DC%XFMV,JT)
!
  ENDDO
  !
END IF
!
!-------------------------------------------------------------------------------
!
!       2.     Richardson number
!              -----------------
!
IF (DGO%N2M>=1) THEN
!
  DO JT = 1,NTILESFC
    !
    CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),ND%AL(JT)%XRI,D%XRI,JT)
    !
  ENDDO
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!       3.     Operational parameters at surface, 2 and 10 meters
!              --------------------------------------------------
!
!
IF (DGO%N2M>=1.OR.DGO%LSURF_BUDGET.OR.DGO%LSURF_BUDGETC) THEN
!
  DO JT = 1,NTILESFC
    !
! Surface temperature
!
    CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),ND%AL(JT)%XTS,D%XTS,JT)
!
  ENDDO
  !
ENDIF
!
IF (DGO%N2M>=1) THEN
!
! Temperature at 2 meters
!
  IF (DGO%LT2MMW) THEN
    DO JT=1,NTILESFC
! Modified weighting giving increased weight to LAND temperature
      CALL  MAKE_AVERAGE_MW(PFRAC_TILE(:,JT),ND%AL(JT)%XT2M,D%XT2M,JT,ZLAND,ZSEA,ZFRL)
    ENDDO
    DO JT=1,NTILESFC
      CALL MAKE_AVERAGE_MW(PFRAC_TILE(:,JT),ND%AL(JT)%XT2M_MIN,D%XT2M_MIN,JT,ZLAND,ZSEA,ZFRL)
    ENDDO
    DO JT=1,NTILESFC      
      CALL MAKE_AVERAGE_MW(PFRAC_TILE(:,JT),ND%AL(JT)%XT2M_MAX,D%XT2M_MAX,JT,ZLAND,ZSEA,ZFRL)
    ENDDO
  ELSE
    DO JT=1,NTILESFC
      CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),ND%AL(JT)%XT2M,D%XT2M,JT)
      CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),ND%AL(JT)%XT2M_MIN,D%XT2M_MIN,JT)
      CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),ND%AL(JT)%XT2M_MAX,D%XT2M_MAX,JT)
    ENDDO
  ENDIF
!
  DO JT=1,NTILESFC
! Relative humidity at 2 meters
!
  CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),ND%AL(JT)%XHU2M,D%XHU2M,JT)
  CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),ND%AL(JT)%XHU2M_MIN,D%XHU2M_MIN,JT)
  CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),ND%AL(JT)%XHU2M_MAX,D%XHU2M_MAX,JT)
!
! Specific humidity at 2 meters
!
  CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),ND%AL(JT)%XQ2M,D%XQ2M,JT)
!
! Wind at 10 meters
!
  CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),ND%AL(JT)%XZON10M,D%XZON10M,JT)
!
  CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),ND%AL(JT)%XMER10M,D%XMER10M,JT)
!
  CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),ND%AL(JT)%XWIND10M,D%XWIND10M,JT)
  CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),ND%AL(JT)%XWIND10M_MAX,D%XWIND10M_MAX,JT)
!
  ENDDO
  !
END IF
!-------------------------------------------------------------------------------
!
!       4.     Transfer coeffients and roughness lengths
!              -----------------------------------------
!
IF (DGO%LCOEF) THEN
!
  DO JT=1,NTILESFC
  !
  CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),ND%AL(JT)%XCD,D%XCD,JT)
!
  CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),ND%AL(JT)%XCH,D%XCH,JT)
!
  CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),ND%AL(JT)%XCE,D%XCE,JT)
!
  CALL MAKE_AVERAGE_Z0(PFRAC_TILE(:,JT),D%XUREF,ND%AL(JT)%XZ0,D%XZ0,JT)
!
  CALL MAKE_AVERAGE_Z0(PFRAC_TILE(:,JT),D%XZREF,ND%AL(JT)%XZ0H,D%XZ0H,JT)
!
  ENDDO
  !
ENDIF
!
IF (DGO%LSURF_VARS) THEN
!
  DO JT=1,NTILESFC
    CALL MAKE_AVERAGE(PFRAC_TILE(:,JT),ND%AL(JT)%XQS,D%XQS,JT)
  ENDDO
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG',1,ZHOOK_HANDLE)
!
CONTAINS
!
SUBROUTINE MAKE_AVERAGE(PFRAC,PFIELD_IN,PFIELD_OUT,KTILE)
!
USE MODD_SURF_PAR, ONLY : XUNDEF
!
IMPLICIT NONE
!
REAL, DIMENSION(:),INTENT(IN)   :: PFRAC
REAL, DIMENSION(:),INTENT(IN)   :: PFIELD_IN
REAL, DIMENSION(:), INTENT(OUT) :: PFIELD_OUT
INTEGER, INTENT(IN) :: KTILE
REAL(KIND=JPRB) :: ZHOOK_HANDLE
INTEGER :: JT
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG:MAKE_AVERAGE',0,ZHOOK_HANDLE)
!
IF (KTILE==1) PFIELD_OUT(:) = 0.
!
WHERE (PFIELD_IN(:)==XUNDEF .AND. PFRAC(:)/=0.) PFIELD_OUT(:) = XUNDEF
!
WHERE (PFIELD_OUT(:)/=XUNDEF) 
  PFIELD_OUT(:) = PFIELD_OUT(:) + PFRAC(:) * PFIELD_IN(:)
END WHERE
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG:MAKE_AVERAGE',1,ZHOOK_HANDLE)
!
END SUBROUTINE MAKE_AVERAGE
!
SUBROUTINE MAKE_AVERAGE_2D(PFRAC,PFIELD_IN,PFIELD_OUT,KTILE)
!
USE MODD_SURF_PAR, ONLY : XUNDEF
!
IMPLICIT NONE
!
REAL, DIMENSION(:),INTENT(IN)   :: PFRAC
REAL, DIMENSION(:,:),INTENT(IN)   :: PFIELD_IN
REAL, DIMENSION(:,:), INTENT(OUT) :: PFIELD_OUT
INTEGER, INTENT(IN) :: KTILE
REAL(KIND=JPRB) :: ZHOOK_HANDLE
INTEGER :: JT, JL
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG:MAKE_AVERAGE_2D',0,ZHOOK_HANDLE)
!
IF (KTILE==1) PFIELD_OUT(:,:) = 0.
!
DO JL=1,SIZE(PFIELD_IN,2)
  WHERE (PFIELD_IN(:,JL)==XUNDEF .AND. PFRAC(:)/=0.) PFIELD_OUT(:,JL) = XUNDEF
  WHERE(PFIELD_OUT(:,JL)/=XUNDEF)
    PFIELD_OUT(:,JL) = PFIELD_OUT(:,JL) + PFRAC(:) * PFIELD_IN(:,JL)
  END WHERE
END DO
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG:MAKE_AVERAGE_2D',1,ZHOOK_HANDLE)
!
END SUBROUTINE MAKE_AVERAGE_2D
!
SUBROUTINE MAKE_AVERAGE_Z0(PFRAC,PREF,PFIELD_IN,PFIELD_OUT,KTILE)
!
USE MODD_SURF_PAR, ONLY : XUNDEF
!
IMPLICIT NONE
!
REAL, DIMENSION(:),INTENT(IN)   :: PFRAC
REAL, DIMENSION(:),INTENT(IN)   :: PFIELD_IN
REAL, DIMENSION(:),INTENT(IN)   :: PREF
REAL, DIMENSION(:), INTENT(OUT) :: PFIELD_OUT
INTEGER, INTENT(IN) :: KTILE
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG:MAKE_AVERAGE_Z0',0,ZHOOK_HANDLE)
!
IF (KTILE==1) PFIELD_OUT(:) = 0.
!
WHERE (PFIELD_IN(:)==XUNDEF .AND. PFRAC(:)/=0.) PFIELD_OUT(:) = XUNDEF
!
WHERE (PFIELD_OUT(:)/=XUNDEF) 
  PFIELD_OUT(:) = PFIELD_OUT(:) + PFRAC(:) * 1./(LOG(PREF(:)/PFIELD_IN(:)))**2
END WHERE
!
IF (KTILE==NTILESFC) THEN
  WHERE (PFIELD_OUT(:) == 0.)
    PFIELD_OUT(:) = XUNDEF
  ELSEWHERE (PFIELD_OUT(:)/=XUNDEF)
    PFIELD_OUT(:) = PREF(:) * EXP( - SQRT(1./PFIELD_OUT(:)) )
  ENDWHERE
ENDIF
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG:MAKE_AVERAGE_Z0',1,ZHOOK_HANDLE)
!
END SUBROUTINE MAKE_AVERAGE_Z0
!
SUBROUTINE MAKE_AVERAGE_MW(PFRAC,PFIELD_IN,PFIELD_OUT,KTILE,PLAND,PSEA,PFRL)
!
USE MODD_SURF_PAR, ONLY : XUNDEF
!
IMPLICIT NONE
!
REAL, DIMENSION(:),INTENT(IN)   :: PFRAC
REAL, DIMENSION(:),INTENT(IN)   :: PFIELD_IN
REAL, DIMENSION(:),  INTENT(OUT)  :: PFIELD_OUT
INTEGER, INTENT(IN) :: KTILE
REAL, DIMENSION(:), INTENT(INOUT) :: PLAND
REAL, DIMENSION(:), INTENT(INOUT) :: PSEA
REAL, DIMENSION(:), INTENT(INOUT) :: PFRL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
INTEGER :: JT
REAL, DIMENSION(SIZE(PFIELD_IN))    :: ZALFA
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG:MAKE_AVERAGE_MW',0,ZHOOK_HANDLE)
!
IF (KTILE==1) THEN
  PFIELD_OUT(:) = 0.
  PSEA     (:)= 0.
  PLAND    (:)= 0.
  PFRL     (:)= 0.
ENDIF
!
WHERE (PFIELD_IN(:)==XUNDEF .AND. PFRAC(:)/=0.) PFIELD_OUT(:) = XUNDEF
!
IF (KTILE==1.OR.KTILE==2) THEN
  PSEA (:) = PSEA(:)  + PFRAC(:) * PFIELD_IN(:)
ENDIF
!
IF (KTILE==3.OR.KTILE==4) THEN
  PLAND    (:) = PLAND(:) + PFRAC(:) * PFIELD_IN(:)
  PFRL     (:) = PFRL (:) + PFRAC(:)
ENDIF
! 
IF (KTILE==4) THEN
  WHERE(ZFRL(:)>0.)
    ZLAND    (:) = ZLAND(:)/ZFRL(:)
  ENDWHERE
  WHERE(ZFRL(:)<1.)
    ZSEA     (:) = ZSEA (:)/(1.-ZFRL(:))
  ENDWHERE
  !
  ZALFA     (:) = 1. - EXP(-10.*ZFRL(:))
  !
  WHERE (PFIELD_OUT(:)/=XUNDEF) 
    PFIELD_OUT(:) = ZALFA(:) * ZLAND(:) + (1. - ZALFA(:)) * ZSEA(:)
  END WHERE
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG:MAKE_AVERAGE_MW',1,ZHOOK_HANDLE)
!
END SUBROUTINE MAKE_AVERAGE_MW
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGE_DIAG
