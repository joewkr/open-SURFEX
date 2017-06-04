!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
       SUBROUTINE DIAG_INLINE_TEB_n (DGO, D, SB, T, &
                                     OCANOPY, PTA, PTS, PQA, PPA, PPS, PRHOA,                &
                                     PZONA, PMERA, PWIND, PHT, PHW,                          &
                                     PCD, PCDN, PRI, PCH, PZ0,                               &
                                     PTRAD, PEMIS, PDIR_ALB, PSCA_ALB,                       &
                                     PLW, PDIR_SW, PSCA_SW,                                  &
                                     PSFTH, PSFTQ, PSFZON, PSFMER, PSFCO2,                   &
                                     PRN, PH, PLE, PGFLUX                                    )  
!     ###############################################################################!
!!****  *DIAG_INLINE_TEB_n * - Computes diagnostics during TEB time-step
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
!!      S. Riette   06/2009 CLS_WIND has one more argument (height of diagnostic)
!!      S. Riette   01/2010 Use of interpol_sbl to compute 10m wind diagnostic
!       B. decharme 04/2013 : Add EVAP and SUBL diag
!!------------------------------------------------------------------
!

!
!
!
USE MODD_DIAG_n, ONLY : DIAG_t, DIAG_OPTIONS_t
USE MODD_CANOPY_n, ONLY : CANOPY_t
USE MODD_TEB_n, ONLY : TEB_t
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE MODI_CLS_WIND
USE MODI_DIAG_SURF_BUDGET_TEB
USE MODI_INTERPOL_SBL
!
USE MODE_THERMOS
USE MODE_COUPLING_CANOPY
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
TYPE(CANOPY_t), INTENT(INOUT) :: SB
TYPE(TEB_t), INTENT(INOUT) :: T
!
LOGICAL,            INTENT(IN)       :: OCANOPY  ! Flag for canopy
REAL, DIMENSION(:), INTENT(IN)       :: PTA      ! atmospheric temperature
REAL, DIMENSION(:), INTENT(IN)       :: PTS      ! surface temperature
REAL, DIMENSION(:), INTENT(IN)       :: PQA      ! atmospheric specific humidity
REAL, DIMENSION(:), INTENT(IN)       :: PPA      ! atmospheric level pressure
REAL, DIMENSION(:), INTENT(IN)       :: PPS      ! surface pressure
REAL, DIMENSION(:), INTENT(IN)       :: PRHOA    ! air density
REAL, DIMENSION(:), INTENT(IN)       :: PZONA    ! zonal wind
REAL, DIMENSION(:), INTENT(IN)       :: PMERA    ! meridian wind
REAL, DIMENSION(:), INTENT(IN)       :: PWIND    ! wind
REAL, DIMENSION(:), INTENT(IN)       :: PHT      ! atmospheric level height
REAL, DIMENSION(:), INTENT(IN)       :: PHW      ! atmospheric level height for wind
REAL, DIMENSION(:), INTENT(IN)       :: PCD      ! drag coefficient for momentum
REAL, DIMENSION(:), INTENT(IN)       :: PCDN     ! neutral drag coefficient
REAL, DIMENSION(:), INTENT(IN)       :: PSFZON   ! zonal friction
REAL, DIMENSION(:), INTENT(IN)       :: PSFMER   ! meridian friction
REAL, DIMENSION(:), INTENT(IN)       :: PSFCO2   ! CO2 flux   (m/s*kg_CO2/kg_air)
REAL, DIMENSION(:), INTENT(IN)       :: PSFTH    ! heat flux  (W/m2)
REAL, DIMENSION(:), INTENT(IN)       :: PSFTQ    ! water flux (kg/m2/s)
REAL, DIMENSION(:), INTENT(IN)       :: PRI      ! Richardson number
REAL, DIMENSION(:), INTENT(IN)       :: PCH      ! drag coefficient for heat
REAL, DIMENSION(:), INTENT(IN)       :: PZ0      ! roughness length for momentum
REAL, DIMENSION(:), INTENT(IN)       :: PRN      ! net radiation
REAL, DIMENSION(:), INTENT(IN)       :: PH       ! sensible heat flux
REAL, DIMENSION(:), INTENT(IN)       :: PLE      ! latent heat flux
REAL, DIMENSION(:), INTENT(IN)       :: PGFLUX   ! storage flux
REAL, DIMENSION(:,:),INTENT(IN)      :: PDIR_SW  ! direct  solar radiation (on horizontal surf.)
!                                                !                                      (W/m2)
REAL, DIMENSION(:,:),INTENT(IN)      :: PSCA_SW  ! diffuse solar radiation (on horizontal surf.)
!                                                !                                      (W/m2)
REAL, DIMENSION(:), INTENT(IN)       :: PLW      ! longwave radiation (on horizontal surf.)
REAL, DIMENSION(:), INTENT(IN)       :: PTRAD    ! radiative temperature                 (K)
REAL, DIMENSION(:,:),INTENT(IN)      :: PDIR_ALB ! direct albedo for each spectral band  (-)
REAL, DIMENSION(:,:),INTENT(IN)      :: PSCA_ALB ! diffuse albedo for each spectral band (-)
REAL, DIMENSION(:), INTENT(IN)       :: PEMIS    ! emissivity                            (-)
!
!*      0.2    declarations of local variables
!
REAL                                 :: ZZ0_O_Z0H
REAL, DIMENSION(SIZE(PTA))           :: ZH  
REAL, DIMENSION(SIZE(PTA))  :: ZU10
REAL, DIMENSION(SIZE(PTA))  :: ZWIND10M_MAX
REAL, DIMENSION(SIZE(PTA))  :: ZT2M_MIN
REAL, DIMENSION(SIZE(PTA))  :: ZT2M_MAX
REAL, DIMENSION(SIZE(PTA))  :: ZHU2M_MIN
REAL, DIMENSION(SIZE(PTA))  :: ZHU2M_MAX
INTEGER                              :: JJ    ! loop counter

REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_INLINE_TEB_N',0,ZHOOK_HANDLE)
!
! * Mean surface temperature need to couple with AGCM
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Here it is the radiative temperature that is wrong !
!It should be the arithmetic mean of the surface temperature
!of each independant energy budget, if there is. See ISBA for more detail.
!
D%XTS(:) = PTS(:)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
ZZ0_O_Z0H = 200.
!
!* 2m and 10m variables interpolated from canopy if used
!
IF (OCANOPY) THEN
  ZT2M_MIN    (:) = XUNDEF
  ZT2M_MAX    (:) = XUNDEF
  ZHU2M_MIN   (:) = XUNDEF
  ZHU2M_MAX   (:) = XUNDEF
  ZWIND10M_MAX(:) = XUNDEF
  IF (DGO%N2M>0) CALL INIT_2M_10M( SB, D, PZONA, PMERA, PWIND, PRHOA )
ELSE
!* 2m and 10m variables using CLS laws
  IF (DGO%N2M==2) THEN
    ZH(:)=10.
    CALL CLS_WIND(PZONA, PMERA, PHW, PCD, PCDN, PRI, ZH, D%XZON10M, D%XMER10M    )  
    D%XT2M  = T%XT_CANYON
    D%XQ2M  = T%XQ_CANYON
    D%XRI   = PRI
    D%XHU2M = MIN(T%XQ_CANYON /QSAT(T%XT_CANYON,PPA),1.)
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
    D%XWIND10M    (:) = SQRT(D%XZON10M**2+D%XMER10M**2)
    D%XWIND10M_MAX(:) = MAX(D%XWIND10M_MAX(:),D%XWIND10M(:))
    !
  END IF
ENDIF
!
IF (DGO%LSURF_BUDGET) THEN
   !
   CALL DIAG_SURF_BUDGET_TEB(D, PDIR_SW, PSCA_SW, PDIR_ALB, PSCA_ALB, PLW, PEMIS, PTRAD   )  
   !                             
   D%XRN    = PRN
   D%XH     = PH
   D%XLE    = PLE
   D%XGFLUX = PGFLUX
   D%XFMU   = PSFZON
   D%XFMV   = PSFMER
   D%XSFCO2 = PSFCO2
   !
END IF
!
IF (DGO%LCOEF) THEN
  D%XCD    = PCD
  D%XCH    = PCH
  D%XCE    = PCH
  D%XZ0    = PZ0
  D%XZ0H   = PZ0 / ZZ0_O_Z0H
END IF
!
IF (DGO%LSURF_VARS) THEN
  D%XQS    = T%XQ_CANYON
END IF
IF (LHOOK) CALL DR_HOOK('DIAG_INLINE_TEB_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_INLINE_TEB_n
