!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ######
!
   SUBROUTINE CCETR_PAIR(KNIV, PABC, PABC_SUP, PIA, PXMUS, PB_DR, POMEGA_DR,&
                        POMEGA_DF, PB_DF, PLAI, PALB_VEG, PALB_SOIL,        &
                        PFD_SKY, PFD_VEG, PTR, PXIA, PLAI_EFF               )
   
!
!!***   *CCETR_PAIR* ***
!!
!!    PURPOSE
!!    -------
!!    Calculates radiative transfer within the canopy
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
!!    USE MODD_CO2V_PAR
!!
!!    REFERENCE
!!    ---------
!!    Carrer et al. 2013
!!      
!!    AUTHOR
!!    ------
!!      D. Carrer           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/04/11 
!!      LAI_EFF corrected 07/2013 
!-------------------------------------------------------------------------------
!
USE MODD_CSTS,       ONLY : XI0
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
INTEGER,            INTENT(IN)    :: KNIV              ! layer number from 10 (top of canopy) to 1 (bottom)
REAL,               INTENT(IN)    :: PABC              ! normalized height units of the considered layer and the one above
REAL,               INTENT(IN)    :: PABC_SUP          ! cumulated canopy height (0 at the bottom, 1 at the top)
REAL,               INTENT(IN)    :: PB_DF             ! single_scattering albedo of considered leaf layer for diffuse rad.
REAL, DIMENSION(:), INTENT(IN)    :: PIA               ! incident PAR or NIR
REAL, DIMENSION(:), INTENT(IN)    :: PXMUS             ! cosine of solar zenith angle
REAL, DIMENSION(:), INTENT(IN)    :: PB_DR             ! single_scattering albedo of considered leaf layer for direct rad.
REAL, DIMENSION(:), INTENT(IN)    :: POMEGA_DR         !
REAL, DIMENSION(:), INTENT(IN)    :: POMEGA_DF         !
REAL, DIMENSION(:), INTENT(IN)    :: PLAI              ! leaf area index
REAL, DIMENSION(:), INTENT(IN)    :: PALB_VEG          ! vegetation albedo in PAR or NIR
REAL, DIMENSION(:), INTENT(IN)    :: PALB_SOIL         ! soil albedo in PAR or NIR
REAL, DIMENSION(:), INTENT(IN)    :: PFD_SKY           ! fraction of incident diffuse radiation at top of canopy
!
REAL, DIMENSION(:), INTENT(INOUT) :: PFD_VEG           ! fraction of incident diffuse radiation at top of considered canopy layer
REAL, DIMENSION(:), INTENT(INOUT) :: PTR               ! fraction of transmited radiation
!
REAL, DIMENSION(:), INTENT(OUT)   :: PXIA              ! fraction of abs. radiation of veg
REAL, DIMENSION(:), INTENT(OUT)   :: PLAI_EFF          ! LAI effective
!
!*      0.2    declarations of local variables

!
REAL, DIMENSION(SIZE(PLAI)) :: ZSLAI_TRU, ZFD_VEG, ZTDF, ZIDR, &
                               ZIDF, ZABC, ZABC_SUP, ZB_DF, ZGT
!                              ZIDF  = interception of diffusion
!                              ZIDR  = direct interception
!                              XB_DR = DH albedo of upper/lower layers
REAL    :: ZGT_SUP, ZGT_INF
INTEGER :: I
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-----------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('CCETR_PAIR',0,ZHOOK_HANDLE)
!
PLAI_EFF(:) = 0.
!
!Angular projection of the leaves 
!  0.5                    : spherical distribution 
!  (2./!PI)*sin(zs*!Dtor) : vertical distribution
!  cos(zs*!Dtor)          : horizontal distribution
ZGT_SUP = 0.5 
ZGT_INF = 0.5
!
IF (PABC.GT.0.8) THEN
  ZGT(:) = ZGT_SUP
ELSE
  ZGT(:) = ZGT_INF
ENDIF
!
ZABC    (:) = PABC
ZABC_SUP(:) = PABC_SUP
ZB_DF   (:) = PB_DF
!
IF (PABC.GT.0.8) THEN
  DO I=1,SIZE(PIA)
    IF (PIA(I)>0.) THEN
      ! diffuse fraction due to vegetation
      ZFD_VEG(I) = EXP(-(1.-ZABC(I))*POMEGA_DR(I)*PLAI(I))
      ZFD_VEG(I) = (1. - ZFD_VEG(I)) / (1. - (1.-PXMUS(I))*ZFD_VEG(I))
      PFD_VEG(I) = MIN(ZFD_VEG(I) + PFD_SKY(I),1.)
    ENDIF
  ENDDO
ENDIF
!
DO I=1,SIZE(PIA)
  IF (PIA(I)>0.) THEN
    ZSLAI_TRU(I) = (ZABC_SUP(I)-ZABC(I))*PLAI(I)
    !PLAI_EFF(I) = POMEGA_DR(I)*ZSLAI_TRU(I)
    ! transmittance of direct beam
    ZIDR(I) = EXP(-ZGT(I)*PB_DR(I)*POMEGA_DR(I)*ZSLAI_TRU(I)/PXMUS(I))
    ! transmittance of diffuse beam
    ZIDF(I) = EXP(-ZB_DF(I)*POMEGA_DF(I)*ZSLAI_TRU(I))
    PLAI_EFF(I) = ((1.-PFD_VEG(I))*POMEGA_DR(I)+PFD_VEG(I)*POMEGA_DF(I))*ZSLAI_TRU(I)
    !
    PTR(I) = ((1.-PFD_VEG(I))*ZIDR(I) + PFD_VEG(I)*ZIDF(I))*PTR(I)
  ENDIF
ENDDO
!
!
! transmissivity of upper layers
!
PXIA(:) = 0.
WHERE (PIA(:)>0.) PXIA(:) = (1-PALB_VEG(:))*(1.-PTR(:))*PIA(:)
!
IF (KNIV .EQ. 1) THEN
  DO I=1,SIZE(PIA)
    IF (PIA(I)>0.) THEN  
      ! -- reflection of surface ---   
      ! transmittance diffuse up - all layer
      ZTDF(I) = EXP(-ZB_DF(I)*POMEGA_DF(I)*(1.-ZABC(I))*PLAI(I))
      PXIA(I)= PXIA(I) + (1.-PALB_VEG(I))*(1.-PALB_VEG(I))*PALB_SOIL(I)*(1.-ZTDF(I))*PTR(I)*PIA(I)
    ENDIF
  ENDDO
ENDIF
!
IF (LHOOK) CALL DR_HOOK('CCETR_PAIR',1,ZHOOK_HANDLE)
!
END SUBROUTINE CCETR_PAIR
 
