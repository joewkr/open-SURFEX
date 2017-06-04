!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #####################################################
!      SUBROUTINE SOILEMISNO_n(PSW_FORBIO, PUA, PVA, KSV, HSV, PFLUX)
      SUBROUTINE SOILEMISNO_n (GB, S, K, NP, NPE, PUA, PVA)
!     #####################################################
!!
!!****** *SOILEMISNO*
!!
!!
!!    PURPOSE
!!    -------
!
!     Calculates NO emissions from soil
!     plus estimation of Canopy Reduction Factor
!!
!!    METHOD
!!    ------
!     Parameterizes NO fluxes function of temperature and soil moisture and other soil parameters,
!     Development from a neural network algorithm.

!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!    MODD_EMIS_NOX
!!
!!    REFERENCE
!!    ---------
!!
!!    Parameterization from neural network
!!
!!    Input data : wind speed, deep soil temperature, surface soil temperature, surface WFPS,
!!    fertilisation rate, pH, sand percentage
!!    Delon et al. (2007) Tellus B
!!
!!    AUTHOR
!!    ------
!!
!!      C. Delon           * LA *
!!
!!    MODIFICATIONS
!!    -------------
!!

!
!--------------------------------------------------------------------------
!
!       0. DECLARATIONS
!          ------------
!
USE MODD_GR_BIOG_n, ONLY : GR_BIOG_t
USE MODD_ISBA_n, ONLY : ISBA_S_t, ISBA_K_t, ISBA_NP_t, ISBA_NPE_t
!
USE MODD_EMIS_NOX
USE MODD_CSTS,       ONLY : XAVOGADRO
!
USE MODD_SURF_PAR, ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!       0.1 Declaration of arguments
!
!
!REAL, DIMENSION(:,:), INTENT(IN)              :: PSW_FORBIO
!
TYPE(GR_BIOG_t), INTENT(INOUT) :: GB
TYPE(ISBA_S_t), INTENT(INOUT) :: S
TYPE(ISBA_K_t), INTENT(INOUT) :: K
TYPE(ISBA_NP_t), INTENT(INOUT) :: NP
TYPE(ISBA_NPE_t), INTENT(INOUT) :: NPE
!
REAL, DIMENSION(:), INTENT(IN)                :: PUA        ! wind module
REAL, DIMENSION(:), INTENT(IN)                :: PVA
INTEGER                                       :: JI         ! index
INTEGER                                       :: JSV
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Local variables:
!
REAL, DIMENSION(SIZE(PUA,1))   :: ZCRF            ! Canopy Reduction Factor
!
REAL, DIMENSION(SIZE(PUA,1))   :: ZTG_D           ! Deep soil temperature in °C
REAL, DIMENSION(SIZE(PUA,1))   :: ZTG_S           ! Surface soil temperature in °C
REAL, DIMENSION(SIZE(PUA,1))   :: ZWFPS_S         ! Water filled pore space at surface
REAL, DIMENSION(SIZE(PUA,1))   :: ZSAND           ! % of sand at surface (0-100)
REAL, DIMENSION(SIZE(PUA,1))   :: ZWIND          ! wind speed
REAL, DIMENSION(SIZE(PUA,1))   :: ZFWORK        
REAL, DIMENSION(SIZE(PUA,1))   :: ZN_WIND          ! Normalized wind speed
REAL, DIMENSION(SIZE(PUA,1))   :: ZN_ZTG_D        ! Normalized deep soil temperature
REAL, DIMENSION(SIZE(PUA,1))   :: ZN_ZTG_S        ! Normalized surface soil temperature
REAL, DIMENSION(SIZE(PUA,1))   :: ZN_ZWFPS_S      ! Normalized WFPS at surface
REAL, DIMENSION(SIZE(PUA,1))   :: ZN_FERT        ! Normalized fertilisation rate (Nitrogen Unity)
REAL, DIMENSION(SIZE(PUA,1))   :: ZN_PH          ! Normalized pH value
REAL, DIMENSION(SIZE(PUA,1))   :: ZN_ZSAND        ! Normalized sand content (%)
REAL, DIMENSION(SIZE(PUA,1))   :: ZN_Y            ! Normalized NO flux
!
REAL, DIMENSION(SIZE(PUA,1),3)   :: ZS            ! normalized sum
!
 CHARACTER(LEN=2)               :: TEST_CRF ! 'OK' if VEG<60% (i.e. soils with sparse vegetation)
!
INTEGER :: J, IMASK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!=============================================================================
IF (LHOOK) CALL DR_HOOK('SOILEMISNO_n',0,ZHOOK_HANDLE)
!
IF (.NOT.ASSOCIATED(GB%XNOFLUX))  ALLOCATE(GB%XNOFLUX(SIZE(PUA,1)))
!
! Calculation of WFPS
! coefficients obtenus a partir des donnees Grignon+Hombori+Escompte(0.536 0.4 0.43)
ZWFPS_S(:) = XUNDEF
ZTG_D  (:) = XUNDEF
ZSAND  (:) = XUNDEF
!
DO JI = 1,NP%AL(1)%NSIZE_P
  !
  IMASK = NP%AL(1)%NR_P(JI)
  !
  ZWFPS_S(IMASK) = (NPE%AL(1)%XWG(JI,1) / 0.45) * 100.      
  ! Change unity of temperatures from Kelvin to Celsius
  ZTG_D  (IMASK) = NPE%AL(1)%XTG(JI,2)  - 273.15
  ZTG_S  (IMASK) = NPE%AL(1)%XTG(JI,1)  - 273.15
  !
ENDDO
!
! Change sand fraction into sand percentage
ZSAND(:) = K%XSAND(:,1) * 100.
!
! Calculate wind module
ZWIND(:) = SQRT( PUA(:)**2 + PVA(:)**2 )
!
! Calculation of NO flux from soil 
!------------------------------------
! 1- Normalized centered entries
!
ZN_ZTG_S  (:) = XCOEF_TG_S  (1) + XCOEF_TG_S  (2) * ZTG_S  (:)
ZN_ZWFPS_S(:) = XCOEF_WFPS_S(1) + XCOEF_WFPS_S(2) * ZWFPS_S(:)
ZN_ZTG_D  (:) = XCOEF_TG_D  (1) + XCOEF_TG_D  (2) * ZTG_D  (:)
ZN_FERT   (:) = XCOEF_FERT  (1) + XCOEF_FERT  (2) * S%XFERT(:)
ZN_ZSAND  (:) = XCOEF_SAND  (1) + XCOEF_SAND  (2) * ZSAND  (:)
ZN_PH     (:) = XCOEF_PH    (1) + XCOEF_PH    (2) * S%XPH  (:)
ZN_WIND   (:) = XCOEF_WIND  (1) + XCOEF_WIND  (2) * ZWIND  (:)
!
! 2- weighted sums
!
DO J=1,3
  ZS(:,J) = XWGT_0(J) + XWGT_TG_S(J) * ZN_ZTG_S(:) &
          + XWGT_WFPS_S(J) * ZN_ZWFPS_S(:) + XWGT_TG_D(J) * ZN_ZTG_D(:) &
          + XWGT_FERT(J) * ZN_FERT(:) + XWGT_SAND(J) * ZN_ZSAND(:) &
          + XWGT_PH(J) * ZN_PH(:) + XWGT_WIND(J) * ZN_WIND(:) 
ENDDO
!
! 3- Hyperbolic tangent calculation    
!
ZN_Y(:) = XWGT_TOT(1) + XWGT_TOT(2)*TANH(ZS(:,1)) + XWGT_TOT(3)*TANH(ZS(:,2)) + XWGT_TOT(4)*TANH(ZS(:,3)) 
!
!  4- Flux calculation
!       If  pH> 6, pulse effect, amplitude coefficient is maximum.
!       If pH < 6, amplitude coefficient is reduced to avoid strong emissions
WHERE (S%XPH(:) .GE. 6.0)
  GB%XNOFLUX(:) = XCOEF_NO0 + XCOEF_NO1_s*ZN_Y(:)
ELSEWHERE
  GB%XNOFLUX(:) = XCOEF_NO0 + XCOEF_NO1_l*ZN_Y(:)
ENDWHERE
!
!PRINT*,'flux de NO en gN/ha/d = ',XNOFLUX(:)
!
!  5- Flag to avoid negative fluxes.
WHERE (GB%XNOFLUX(:).LT. 0.) GB%XNOFLUX(:)=0.
!     PRINT*,'!!!!!! Attention flux de NO negatifs !!!!!',XNOFLUX(JI)
!
!  6- Changing units from gN/ha/d to molecules/m2/s
! 1 ha=10000 m2, 1d=86400s, 1mole(NO)=30g, 1mole=Avogadro molec (6.022E23).
!                           1mole(N) =14g
GB%XNOFLUX(:) = GB%XNOFLUX(:)*XAVOGADRO/(1.0E4*8.64E4*14)
!
!PRINT*,'flux de NO en molec/cm2/s = ',XNOFLUX(JI)
!
!  7- Reduction du flux dans la canopee
!          WHERE (XLAI(:,1)/=XUNDEF) 
!         ZCRF(:) = -0.0917*XLAI(:,1) + 0.9429
ZCRF(:) = 1.
DO JI = 1,NP%AL(1)%NSIZE_P
  IMASK = NP%AL(1)%NR_P(JI)
  IF (NPE%AL(1)%XLAI(JI) > 1.9 .AND. NPE%AL(1)%XLAI(JI) < 5.) THEN
    ZCRF(IMASK) = 0.5
  ELSEIF (NPE%AL(1)%XLAI(JI) > 5.) THEN
    ZCRF(IMASK) = 0.2
  ENDIF
ENDDO
!       PRINT*,'LAI, CRF', XLAI(:), ZCRF(:)
GB%XNOFLUX(:) = GB%XNOFLUX(:)*ZCRF(:)
!       PRINT*,'flux de NO en molec/m2/s apres CRF = ',XNOFLUX(:)
!
!  8- Introduction du Flux de NO final dans la chimie apres reduction par le CRF (avec MesoNH chimie)
!  IF (NBEQ>0) THEN
!      DO JSV=NSV_CHSBEG,NSV_CHSEND
!         IF (CSV(JSV) == "NO") THEN
!          PFLUX(:,JSV) = PFLUX(:,JSV) + XNOFLUX(:)
!         ENDIF
!      END DO
!  ELSE
!      PFLUX(:,1) = PFLUX(:,1) + XNOFLUX(:)
!  ENDIF
!
IF (LHOOK) CALL DR_HOOK('SOILEMISNO_n',1,ZHOOK_HANDLE)
!
END SUBROUTINE SOILEMISNO_n
