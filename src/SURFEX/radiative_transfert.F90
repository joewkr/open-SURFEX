!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
MODULE MODI_RADIATIVE_TRANSFERT
CONTAINS
SUBROUTINE RADIATIVE_TRANSFERT(OAGRI_TO_GRASS, PVEGTYPE,          &
            PALBVIS_VEG, PALBVIS_SOIL, PALBNIR_VEG, PALBNIR_SOIL, &
            PSW_RAD, PLAI, PZENITH, PABC,                         &
            PFAPARC, PFAPIRC, PMUS, PLAI_EFFC, OSHADE, PIACAN,    &
            PIACAN_SUNLIT, PIACAN_SHADE, PFRAC_SUN,               &
            PFAPAR, PFAPIR, PFAPAR_BS, PFAPIR_BS                  )
!   #########################################################################
!
!!****  *RADIATIVE_TRANSFERT*
!!
!!    PURPOSE
!!    -------
!!
!!    Calculates the fraction of absorbed photosynthetic radiation (FAPAR),
!!      the fraction of absorbed near-infrared radiation (FAPIR), based on
!!      the fraction of diffuse and direct radiation (Erbs et al 1982 formulation).
!!    Calculates also the clumping index and the resulting transmittance
!!      distinguishing between the upper part of the canopy (SUP) and the rest (INF),
!!      direct and diffuse radiation, sunlit and shaded leaves.
!!
!!**  METHOD
!!    ------
!!    Carrer et al, 2013 (doi:10.1002/jgrg20070)
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    USE MODD_CSTS
!!    USE MODD_CO2V_PAR
!!    USE MODD_SURF_PAR
!!    USE MODI_FAPAIR
!!
!!    REFERENCE
!!    ---------
!!
!!    Carrer et al, 2013 (doi:10.1002/jgrg20070)
!!
!!    AUTHOR
!!    ------
!!
!!        D. Carrer           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!     Original    04/11
!!     C. Delire   08/13 : moved calculation of diffuse fraction from fapair to here
!!     Commented by C. Delire 07/13
!!
!-------------------------------------------------------------------------------
!!
USE MODD_CSTS,           ONLY : XI0                              ! Solar constant
USE MODD_CO2V_PAR,       ONLY : XPARCF, XLAI_SHADE,            &
                                XXB_SUP, XXB_INF,              & ! sigma parameter in clumping (Table 1, eq4)
                                XSSA_SUP, XSSA_INF,            & ! single scatering albedo (PAR)
                                XSSA_SUP_PIR, XSSA_INF_PIR       ! single scatering albedo (NIR)
!
USE MODD_DATA_COVER_PAR, ONLY : NVT_C3, NVT_C4, &
                                NVT_IRR, NVT_GRAS, &
                                NVT_C3W, NVT_C3S
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODI_FAPAIR
!
!*       0.     DECLARATIONS
!               ------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
LOGICAL,             INTENT(IN)  :: OAGRI_TO_GRASS
!
REAL, DIMENSION(:,:),INTENT(IN)  :: PVEGTYPE     ! PVEGTYPE  = type de vegetation (1 a 9)
!
REAL, DIMENSION(:), INTENT(IN)   :: PALBVIS_VEG  ! visible snow free albedo of vegetation
REAL, DIMENSION(:), INTENT(IN)   :: PALBVIS_SOIL ! visible snow free albedo of soil
REAL, DIMENSION(:), INTENT(IN)   :: PALBNIR_VEG  ! NIR snow free albedo of vegetation
REAL, DIMENSION(:), INTENT(IN)   :: PALBNIR_SOIL ! NIR snow free albedo of soil
!
REAL,DIMENSION(:),   INTENT(IN)  :: PSW_RAD      ! incident broadband solar radiation (PAR+NIR)
REAL,DIMENSION(:),   INTENT(IN)  :: PLAI         ! PLAI  = leaf area index
!
REAL,DIMENSION(:),    INTENT(IN)  :: PZENITH     ! solar zenith angle needed
!                                    for computation of diffusion of solar
!                                    radiation
!
REAL,DIMENSION(:),  INTENT(INOUT) :: PABC        ! normalized canopy height (0=bottom, 1=top)
!
!
REAL, DIMENSION(:),   INTENT(INOUT) :: PFAPARC   !fraction of absorbed photosynthetic active radiation (cumulated over patches)
REAL, DIMENSION(:),   INTENT(INOUT) :: PFAPIRC   !fraction of absorbed NIR (cumulated)
REAL, DIMENSION(:),   INTENT(INOUT) :: PMUS      ! cosine of solar zenith angle (averaged)
REAL, DIMENSION(:),   INTENT(INOUT) :: PLAI_EFFC ! Effective LAI (cumulated)
!
LOGICAL, DIMENSION(:),INTENT(OUT) :: OSHADE      ! OSHADE = if 1 shading activated
REAL, DIMENSION(:,:), INTENT(OUT) :: PIACAN      ! APAR in the canopy at different gauss level
REAL, DIMENSION(:,:), INTENT(OUT) :: PIACAN_SUNLIT, PIACAN_SHADE
!                                                ! absorbed PAR at each level within the
!                                                ! canopy - Split into shaded and SUNLIT
REAL, DIMENSION(:,:), INTENT(OUT) :: PFRAC_SUN   ! fraction of sunlit leaves
!
REAL, DIMENSION(:),   INTENT(OUT) :: PFAPAR, PFAPIR, PFAPAR_BS, PFAPIR_BS
!
!*      0.2    declarations of local variables
!
!
REAL, DIMENSION(SIZE(PLAI)) :: ZIA, ZLAI, ZLAI_EFF, ZXMUS, ZFD_SKY
!                                ZXMUS = cosine of solar zenith angle
!                                ZFD_SKY = fraction of diffuse radiation in sky
REAL,    DIMENSION(SIZE(PLAI)) :: ZB_INF, ZB_SUP
INTEGER, DIMENSION(1)          :: IDMAX
REAL                           :: ZTAU, ZRATIO
!                               ZTAU = exp(-aerosol optical depth taken as 0.1)
!                               ZRATIO = clearness index K_t eq.1 from Carrer et al
INTEGER                        :: JJ, I ! index for loops
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('RADIATIVE_TRANSFERT',0,ZHOOK_HANDLE)
!
ZLAI(:)   = PLAI(:)
ZFD_SKY(:) = 0.
!
WHERE (PLAI(:)==XUNDEF) ZLAI(:) = 0.0
!
! Geometrical configuration and density of leaves induce different
! min value of LAI to start the shading.
OSHADE(:)= .TRUE.
DO JJ = 1, SIZE(PLAI)
! CD value calculated for patch with largest fraction ?
  IDMAX = MAXLOC(PVEGTYPE(JJ,:))
  IF(OAGRI_TO_GRASS.AND. &
      (IDMAX(1)==NVT_C3 .OR. IDMAX(1)==NVT_C3W .OR. IDMAX(1)==NVT_C3S .OR. IDMAX(1)==NVT_C4 .OR. IDMAX(1)==NVT_IRR)) &
        IDMAX(1)=NVT_GRAS
  IF (PLAI(JJ).LT.XLAI_SHADE(IDMAX(1))) OSHADE(JJ) = .FALSE.
  ZB_INF(JJ) = XXB_INF(IDMAX(1))
  ZB_SUP(JJ) = XXB_SUP(IDMAX(1))
ENDDO
!
!to consider all the tickness of the canopy
PABC(1) = 0.
!
! cosine of solar zenith angle
!
ZXMUS(:) = MAX(COS(PZENITH(:)),0.01)
!
! CD Calculation of diffuse fraction done here because depends on solar radiation and not PAR
!
ZTAU = EXP(-0.1) !  Aerosol Optical Depth fixed at low value (Carrer et al, section 2.1.2 eq. 1)
!
! Diffuse fraction based on clearness index (Carrer et la, eq. 1 & 2.)
DO I=1,SIZE(PLAI)
  IF (PSW_RAD(I) > 0.) THEN
    ! estimates fraction of diffuse radiation by Erbs (1982)
    ZRATIO = PSW_RAD(I)/XI0/ZXMUS(I)
    IF (ZRATIO < 0.22) THEN
      ZFD_SKY(I) = (1 - 0.09*ZRATIO)
    ELSE IF (ZRATIO < 0.8) THEN
      ZFD_SKY(I) = (0.9511 + (-0.1604 + (4.388 + (-16.64 + 12.34*ZRATIO)*ZRATIO)*ZRATIO)*ZRATIO)
    ELSE
      !!$ PXFD_SKY(I) = PIA(I)*0.165  ! original Erbs formulation
      !if clear sky, the diffuse fraction depends on aerosol load
      ZFD_SKY(I) = (1. - ZTAU) /(1. - (1.-ZXMUS(I))*ZTAU)
    ENDIF
  ENDIF
END DO
!
! NIR calculations
ZIA(:)     = PSW_RAD(:)*(1.-XPARCF)
 CALL FAPAIR(PABC, ZFD_SKY, ZIA, ZLAI, ZXMUS, XSSA_SUP_PIR, XSSA_INF_PIR,  &
         ZB_SUP, ZB_INF, PALBNIR_VEG, PALBNIR_SOIL, OSHADE,      &
         PFAPIR, PFAPIR_BS                                       )
!
ZIA(:)     = PSW_RAD(:)*XPARCF
 CALL FAPAIR(PABC, ZFD_SKY, ZIA, ZLAI, ZXMUS, XSSA_SUP, XSSA_INF,          &
         ZB_SUP, ZB_INF, PALBVIS_VEG, PALBVIS_SOIL, OSHADE,      &
         PFAPAR, PFAPAR_BS, PLAI_EFF=ZLAI_EFF, PIACAN=PIACAN,    &
         PIACAN_SHADE=PIACAN_SHADE, PIACAN_SUNLIT=PIACAN_SUNLIT, &
         PFRAC_SUN=PFRAC_SUN                                     )
!
DO JJ = 1,SIZE(PLAI)
  IF (ZIA(JJ).NE.0.) THEN
    PFAPIRC(JJ)   = PFAPIRC(JJ)   + PFAPIR(JJ)   * ZXMUS(JJ)
    PFAPARC(JJ)   = PFAPARC(JJ)   + PFAPAR(JJ)   * ZXMUS(JJ)
    PLAI_EFFC(JJ) = PLAI_EFFC(JJ) + ZLAI_EFF(JJ) * ZXMUS(JJ)
    PMUS(JJ)      = PMUS(JJ)      + ZXMUS(JJ)
  ENDIF
ENDDO
!
IF (LHOOK) CALL DR_HOOK('RADIATIVE_TRANSFERT',1,ZHOOK_HANDLE)
!
END SUBROUTINE RADIATIVE_TRANSFERT
END MODULE MODI_RADIATIVE_TRANSFERT
