!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ######
SUBROUTINE FAPAIR(PABC, PFD_SKY, PIA, PLAI, PXMUS, PSSA_SUP, PSSA_INF, &
           PB_SUP, PB_INF, PALB_VEG, PALB_SOIL, OSHADE,            &
           PFAPR, PFAPR_BS, PLAI_EFF, PIACAN,                      &
           PIACAN_SHADE, PIACAN_SUNLIT, PFRAC_SUN                  )
!   #########################################################################
!
!!****  *FAPAIR*  
!!
!!    PURPOSE
!!    -------
!!    Calculates fraction of absorbed photosynthetic active radiation (FAPAR) and 
!!     fraction of absorbed near infrared (FAPIR) of vegetation and bare soil.
!!              
!!**  METHOD
!!    ------
!!    Carrer et al. 
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    USE MODD_SURF_PAR
!!    USE MODD_CSTS
!!    USE MODD_CO2V_PAR
!!    USE MODI_CCETR_PAIR
!!
!!    REFERENCE
!!    ---------
!!     Carrer et al, 2013 (doi:10.1002/jgrg20070)
!!      
!!    AUTHOR
!!    ------
!!      D. Carrer          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/04/2011 
!!      Commented by C. Delire 07/13
!!      C. Delire   08/13 : moved calculation of diffuse fraction from here to radiative_transfert.F90
!!      A. Boone    02/17 : corrected computation of PFAPR_BS
!!
!!-------------------------------------------------------------------------------
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_CO2V_PAR,   ONLY : XK_SUP, XK_INF, XXSI_SUP, XXSI_INF ! clumping index parameters (Carrer et al 2.1.3)
!
USE MODD_SURFEX_MPI, ONLY : NRANK
USE MODI_CCETR_PAIR  
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
!*       0.     DECLARATIONS
!               ------------
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:), INTENT(IN) :: PABC    ! abscissa needed for integration
!                                         ! of net assimilation and stomatal
!                                         ! conductance over canopy depth
REAL, DIMENSION(:), INTENT(IN) :: PFD_SKY ! fraction of diffused radiation in sky
REAL, DIMENSION(:), INTENT(IN) :: PIA     ! incident PAR or NIR
REAL, DIMENSION(:), INTENT(IN) :: PLAI    ! leaf area index
REAL, DIMENSION(:), INTENT(IN) :: PXMUS   ! cosine of solar zenith angle
REAL,               INTENT(IN) :: PSSA_SUP, PSSA_INF  !single scatering albedo (PAR or NIR)
REAL, DIMENSION(:), INTENT(IN) :: PB_SUP, PB_INF      !sigma parameter in clumping (Table 1, eq4)
REAL, DIMENSION(:), INTENT(IN) :: PALB_VEG, PALB_SOIL
LOGICAL, DIMENSION(:), INTENT(IN) :: OSHADE   ! OSHADE = if 1 shading activated
!
REAL, DIMENSION(:), INTENT(OUT) :: PFAPR
REAL, DIMENSION(:), INTENT(OUT) :: PFAPR_BS
REAL, DIMENSION(:), OPTIONAL,   INTENT(OUT) :: PLAI_EFF
!
REAL, DIMENSION(:,:), OPTIONAL, INTENT(OUT) :: PIACAN ! PAR in the canopy at different gauss level
REAL, DIMENSION(:,:), OPTIONAL, INTENT(OUT) :: PIACAN_SHADE  ! PAR in the canopy at different gauss level
REAL, DIMENSION(:,:), OPTIONAL, INTENT(OUT) :: PIACAN_SUNLIT ! PAR in the canopy at different gauss level
REAL, DIMENSION(:,:), OPTIONAL, INTENT(OUT) :: PFRAC_SUN     ! fraction of sunlit leaves
!
!*      0.2    declarations of local variables
!
!
REAL, DIMENSION(SIZE(PLAI))   :: ZXIA, ZXIA_SUP, ZKMUSP_SUP, ZKMUSP_INF
REAL, DIMENSION(SIZE(PLAI))   :: ZB_DR_SUP, ZB_DR_INF, ZOMEGA_DR_SUP, ZOMEGA_DR_INF, &
                                 ZOMEGA_DF_SUP, ZOMEGA_DF_INF
!                                ZXIA  = abs. radiation of vegetation
REAL, DIMENSION(SIZE(PLAI))   :: ZTR, ZFD_VEG, ZFD_SUP, ZLAI_EFF0, ZLAI_EFF
!                                ZTR = transmittance
!                                ZFD_VEG, ZFD_SUP = fraction of radiation diffused by the considered medium (vegetation)     
!REAL, DIMENSION(SIZE(PLAI))  :: ZXIA_SUNLIT, ZXIA_SHADE, ZLAI_SUNLIT, ZLAI_SHADE
!                                ZXIA_SUNLIT = absorbed PAR of sunlit leaves
!                                ZXIA_SHADE = absorbed PAR of shaded leaves
!                                ZLAI_SUNLIT = LAI of sunlit leaves
!                                ZLAI_SHADE = LAI of shaded leaves
!REAL, DIMENSION(SIZE(PLAI))    :: ZRN_SUNLIT, ZRN_SHADE
REAL, DIMENSION(SIZE(PLAI),SIZE(PABC)) :: ZIACAN, ZIACAN_SUNLIT, ZIACAN_SHADE, ZFRAC_SUN
REAL                           :: ZABC, ZWEIGHT, ZCOEF, ZSUP, ZINF, &
                                  ZSSA_SUP, ZSSA_INF, ZB_DF_SUP, ZB_DF_INF
!                                 ZABC    = abscissa needed for integration
!                                            of net assimilation and stomatal 
!                                            conductance over canopy depth (working scalar)
!                                 ZSUP, ZINF = d_sup and d_inf from Table 1. Carrer et al.
!
INTEGER                        :: JINT, I ! index for loops
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('FAPAIR',0,ZHOOK_HANDLE)
!
! initialisation
!
ZTR(:)      = 1.0
!
ZXIA_SUP(:) = 0.
!
ZFD_VEG(:)  = 0.
ZFD_SUP(:)  = 0.
!
!ZXIA_SUNLIT(:) = 0.
!ZXIA_SHADE(:)  = 0.
!ZLAI_SUNLIT(:) = 0.
!ZLAI_SHADE(:)  = 0.
!
ZLAI_EFF(:) = 0.
!
ZIACAN(:,:)        = 0.
ZIACAN_SUNLIT(:,:) = 0.
ZIACAN_SHADE(:,:)  = 0.
ZFRAC_SUN(:,:)     = 0.
!
PFAPR(:)    = 0.
PFAPR_BS(:) = 0.
!PRN_SHADE(:) = 0.
!PRN_SUNLIT(:) = 0.
!
!
IF (PABC(SIZE(PABC)).GT.0.8) ZFD_VEG(:) = MIN(PFD_SKY(:),1.)   
! set param sup / inf
!
ZSSA_SUP = SQRT(1.-PSSA_SUP)
ZSSA_INF = SQRT(1.-PSSA_INF)
!
ZSUP = - 0.461 * XXSI_SUP + 3.8     ! d_sup Carrer et al Table 1
ZINF = - 0.461 * XXSI_INF + 3.8     ! d_inf Carrer et al Table 1
!
DO I=1,SIZE(PIA)
  IF (PIA(I).NE.0.) THEN
    ZKMUSP_SUP(I) = EXP(-XK_SUP*(ACOS(PXMUS(I)))**ZSUP)
    ZKMUSP_INF(I) = EXP(-XK_INF*(ACOS(PXMUS(I)))**ZINF)
    ! direct case
    ! Directional albedo of upper/lower layer
    ZB_DR_SUP(I) = 1.-(1.-ZSSA_SUP)/(1.+2.*PXMUS(I)*ZSSA_SUP) ! Carrer et al. b_dr Table 1
    ZB_DR_INF(I) = 1.-(1.-ZSSA_SUP)/(1.+2.*PXMUS(I)*ZSSA_INF)
    ! CLUMPING INDEX 
    ZOMEGA_DR_SUP(I) = 1. / (1.+ PB_SUP(I)*ZKMUSP_SUP(I))     ! Carrer et al. eq. 4a
    ZOMEGA_DR_INF(I) = 1. / (1.+ PB_INF(I)*ZKMUSP_INF(I))
    ! diffuse case
    ! CLUMPING INDEX
    ZOMEGA_DF_SUP(I) = (1.+PB_SUP(I)/2.)/(1.+PB_SUP(I))       ! Carrer et al. eq. 4b
    ZOMEGA_DF_INF(I) = (1.+PB_INF(I)/2.)/(1.+PB_INF(I))
  ENDIF
ENDDO
!
! Non-directional albedo of diffuse 
ZB_DF_SUP = 1.-(1.-ZSSA_SUP)/(1.+ ZSSA_SUP)
ZB_DF_INF = 1.-(1.-ZSSA_INF)/(1.+ ZSSA_INF)
!
! Integration over the canopy: SIZE(PABC) increments
! are used to approximate the integral. And to calculate 
! absorded fluxes within the canopy and in the bare soil  
DO JINT = SIZE(PABC),1,-1
!
  ZABC = 1.                                ! normalized height unit of the layer above
  IF (JINT.LT.SIZE(PABC)) ZABC = PABC(JINT+1)
  ZWEIGHT = ZABC - PABC(JINT)
  !
  IF (PABC(JINT).GT.0.8) THEN
    !  Compute transmittance of each level  
    CALL CCETR_PAIR (JINT, PABC(JINT), ZABC, PIA, PXMUS, ZB_DR_SUP, &
                     ZOMEGA_DR_SUP, ZOMEGA_DF_SUP, ZB_DF_SUP, PLAI, &
                     PALB_VEG, PALB_SOIL, PFD_SKY, ZFD_VEG, ZTR,    &
                     ZXIA, ZLAI_EFF0              )
  ELSE
    CALL CCETR_PAIR (JINT, PABC(JINT), ZABC, PIA, PXMUS, ZB_DR_INF, &
                     ZOMEGA_DR_INF, ZOMEGA_DF_INF, ZB_DF_INF, PLAI, &
                     PALB_VEG, PALB_SOIL, PFD_SKY, ZFD_VEG, ZTR,    &
                     ZXIA, ZLAI_EFF0              )
  ENDIF          
  ! 
  DO I=1,SIZE(PIA)
    !
    ZXIA(I)        = MAX(0.,ZXIA(I))
    ZIACAN(I,JINT) = MAX(0.,ZXIA(I)-ZXIA_SUP(I))
    ZXIA_SUP(I)    = ZXIA(I)
    !
    ZLAI_EFF0(I) = MAX(0.,ZLAI_EFF0(I))
    ZLAI_EFF(I)  = ZLAI_EFF(I) + ZLAI_EFF0(I)
    !
    !calculate a FAPAR/FAPIR of the entire canopy
    PFAPR(I)  = PFAPR(I) + ZIACAN(I,JINT)
    !
    !------------------------------------------------------
    ! If LSHADE=0 no shading, only sunlit leaves
    ! If LSHADE=1 shading
    ! PIACAN is used to calculate An of each level within the canopy in cotwores
    ! ZIACAN_SUNLIT used for net assimilation of a sunlit leave in COTWO
    ! ZIACAN_SHADE used  in A-gs for net assimilation of a shaded leave in COTWO
    IF (OSHADE(I)) THEN
      !
      !sunlit leaves
      !absorbed PAR of an equivalent canopy representative of the layer of leaves  eq. (8)
      ZCOEF = (1.0-ZFD_SUP(I))/ZTR(I)+ ZFD_SUP(I)    
      ZIACAN_SUNLIT(I,JINT) =             ZCOEF/(ZWEIGHT*MAX(0.0001,PLAI(I)))*ZIACAN(I,JINT)    
      !not sunlit leaves
      ZIACAN_SHADE(I,JINT)  = MAX(0.,ZFD_SUP(I)/(ZWEIGHT*MAX(0.0001,PLAI(I)))*ZIACAN(I,JINT))
      !
      !ZXIA_SUNLIT(I) = ZXIA_SUNLIT(I) + ZWEIGHT*ZTR(I)      *ZIACAN_SUNLIT(I,JINT)
      !ZLAI_SUNLIT(I) = ZLAI_SUNLIT(I) + ZWEIGHT*ZTR(I)*ZCOEF*PLAI(I)
      !
      !ZXIA_SHADE(I)  = ZXIA_SHADE(I)  + ZWEIGHT*(1-ZTR(I))           *ZIACAN_SHADE(I,JINT)
      !ZLAI_SHADE(I)  = ZLAI_SHADE(I)  + ZWEIGHT*(1-ZTR(I))*ZFD_SUP(I)*PLAI(I)
      !
      ZFRAC_SUN(I,JINT) = ZTR(I)  !fraction of sunlit leaves
      !      
    ELSE
      !
      ZIACAN_SUNLIT(I,JINT) = MAX(0.,ZIACAN(I,JINT)/(ZWEIGHT*MAX(0.0001,PLAI(I))))
      !ZLAI_SUNLIT(I) = ZLAI_SUNLIT(I) + ZWEIGHT*PLAI(I)
      !
    ENDIF
    !
    ZFD_SUP(I) = ZFD_VEG(I)
    !
    ENDDO
  !
END DO
!
!
WHERE (PIA(:).NE.0.)
  PFAPR(:) = PFAPR(:) / PIA(:)
  PFAPR_BS(:)= ZTR(:)*(1.-PALB_SOIL(:)*(1. - PALB_VEG(:)*(1.-ZTR(:))))
END WHERE
!
!WHERE (ZLAI_SHADE(:) .NE.0.) ZRN_SHADE(:)  = ZXIA_SHADE(:) / ZLAI_SHADE(:)
!WHERE (ZLAI_SUNLIT(:).NE.0.) ZRN_SUNLIT(:) = ZXIA_SUNLIT(:)/ ZLAI_SUNLIT(:)
!
IF (PRESENT(PLAI_EFF))      PLAI_EFF      = ZLAI_EFF
IF (PRESENT(PIACAN))        PIACAN        = ZIACAN
IF (PRESENT(PIACAN_SUNLIT)) PIACAN_SUNLIT = ZIACAN_SUNLIT
IF (PRESENT(PIACAN_SHADE))  PIACAN_SHADE  = ZIACAN_SHADE
IF (PRESENT(PFRAC_SUN))     PFRAC_SUN     = ZFRAC_SUN
!
IF (LHOOK) CALL DR_HOOK('FAPAIR',1,ZHOOK_HANDLE)
!
END SUBROUTINE FAPAIR
