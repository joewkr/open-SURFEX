!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     ##########################################################################
      SUBROUTINE ISBA_FLUXES_MEB(KK, PK, PEK, DK, DEK, DMK, PRHOA, PLTT, PSIGMA_F,PSIGMA_FN, &
                                 PRN_V, PRN_G, PLWNET_V_DTV, PLWNET_V_DTG, PLWNET_V_DTN, &
                                 PLWNET_G_DTV, PLWNET_G_DTG, PLWNET_G_DTN, PLWNET_N_DTV, &
                                 PLWNET_N_DTG, PLWNET_N_DTN, PTHRMA_TA, PTHRMB_TA,       &
                                 PTHRMA_TC, PTHRMB_TC, PTHRMA_TG, PTHRMB_TG, PTHRMA_TV,  &
                                 PTHRMB_TV, PTHRMA_TN, PTHRMB_TN, PQSAT_G, PQSAT_V,      &
                                 PQSATI_N, PPSNA, PPSNCV, PFROZEN1,PLEG_DELTA,           &
                                 PLEGI_DELTA, PHUGI, PHVG, PHVN, PFLXC_CA, PFLXC_GV,     &
                                 PFLXC_VG_C, PFLXC_VN_C, PFLXC_GN, PFLXC_N_A, PFLXC_MOM, &
                                 PFLXC_CV, PHVGS, PHVNS, PTG, PDQSAT_G, PDQSAT_V,        &
                                 PDQSATI_N, PTA_IC, PQA_IC, PDELTA_V, PDELTAT_G,         &
                                 PDELTAT_V, PDELTAT_N, PSW_UP, PSW_RAD, PLW_RAD, PLW_UP, &
                                 PH_N_A, PEVAP_C_A, PEVAP_N_A, PLEG, PLEGI, PLES, PLEL,  &
                                 PEVAPN, PEMIS                             )
!     ##########################################################################
!
!!****  *ISBA_FLXUES_MEB*
!!
!!    PURPOSE
!!    -------
!
!     Calculates the implicit fluxes for implicit or explicit atmospheric
!     coupling and fluxes needed by hydrology, soil and snow routines.
!     finally, compute soil phase changes.
!
!
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    Noilhan and Planton (1989)
!!    Belair (1995)
!!    * to be done * (2011)
!!
!!    AUTHOR
!!    ------
!!
!!      A. Boone           * Meteo-France *
!!      P. Samuelsson      * SMHI *
!!      S. Gollvik         * SMHI *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    22/01/11
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_n, ONLY : ISBA_K_t, ISBA_PE_t, ISBA_P_t
USE MODD_DIAG_n, ONLY : DIAG_t
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_t
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t
!
USE MODD_ISBA_PAR,       ONLY : XEMISSOIL, XEMISVEG
USE MODD_CSTS,           ONLY : XLVTT, XLSTT, XSTEFAN
!
USE MODI_ISBA_EMIS_MEB
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
TYPE(ISBA_K_t), INTENT(INOUT) :: KK
TYPE(ISBA_P_t), INTENT(INOUT) :: PK
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
TYPE(DIAG_t), INTENT(INOUT) :: DK
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: DEK
TYPE(DIAG_MISC_ISBA_t), INTENT(INOUT) :: DMK
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:),   INTENT(IN)   :: PRHOA, PLTT
!                                     PRHOA = reference level air density (kg m-3)
!                                     PLTT  = latent heat normalization factor (J kg-1)
!
REAL, DIMENSION(:),   INTENT(IN)   :: PSIGMA_F, PSIGMA_FN
!
REAL, DIMENSION(:),   INTENT(IN)   :: PLWNET_V_DTV, PLWNET_V_DTG, PLWNET_V_DTN
!                                     PLWNET_V_DTV, PLWNET_V_DTG, PLWNET_V_DTN = Vegetation canopy net LW radiation
!                                     derivatives w/r/t surface temperature(s) (W m-2 K-1)
!
REAL, DIMENSION(:),   INTENT(IN)   :: PLWNET_G_DTV, PLWNET_G_DTG, PLWNET_G_DTN
!                                     PLWNET_G_DTV, PLWNET_G_DTG, PLWNET_G_DTN = Understory-ground net LW radiation
!                                          derivatives w/r/t surface temperature(s) (W m-2 K-1)
!
REAL, DIMENSION(:),   INTENT(IN)   :: PLWNET_N_DTV, PLWNET_N_DTG, PLWNET_N_DTN
!                                     PLWNET_N_DTV, PLWNET_N_DTG, PLWNET_N_DTN = Ground-based snow net LW radiation
!                                          derivatives w/r/t surface temperature(s) (W m-2 K-1)
!
REAL, DIMENSION(:),   INTENT(IN)   :: PTHRMA_TA, PTHRMB_TA, PTHRMA_TC, PTHRMB_TC,                     &
                                      PTHRMA_TG, PTHRMB_TG, PTHRMA_TV, PTHRMB_TV, PTHRMA_TN, PTHRMB_TN
!                                     PTHRMA_TA                                                    (J kg-1 K-1)
!                                     PTHRMB_TA = linear transform coefficinets for atmospheric
!                                                 thermal variable for lowest atmospheric level.   (J kg-1)
!                                                 Transform T to dry static energy or enthalpy.
!                                     PTHRMA_TC                                                    (J kg-1 K-1)
!                                     PTHRMB_TC = linear transform coefficinets for atmospheric
!                                                 thermal variable for canopy air                  (J kg-1)
!                                                 Transform T to dry static energy or enthalpy.
!                                     PTHRMA_TG,V,N                                                (J kg-1 K-1)
!                                     PTHRMB_TG,V,N = linear transform coefficinets for atmospheric
!                                                 thermal variable for surfaces (G, V, and N)      (J kg-1)
!                                                 Transform T to dry static energy or enthalpy.
!
REAL, DIMENSION(:),   INTENT(IN)   :: PQSAT_G, PQSAT_V, PQSATI_N
!                                     PQSAT_G  = saturation specific humidity for understory surface    (kg kg-1)
!                                     PQSAT_V  = saturation specific humidity for the vegetation canopy (kg kg-1)
!                                     PQSATI_N = saturation specific humidity over ice for the snowpack (kg kg-1)
!
REAL, DIMENSION(:),   INTENT(IN)   :: PPSNA, PPSNCV, PFROZEN1
!                                     PPSNA    = fraction of vegetation canopy buried by ground-based snowpack (-)
!                                     PPSNCV   = fraction of vegetation canopy covered by intercepted snow     (-)
!                                     PFROZEN1 = frozen fraction of surface ground layer                       (-)
!
!
REAL, DIMENSION(:),   INTENT(IN)   :: PLEG_DELTA, PLEGI_DELTA, PHUGI, PHVG, PHVN
!
!                                     PHVG = Halstead coefficient of non-buried (snow) canopy vegetation       (-)
!                                     PHVN = Halstead coefficient of paritally-buried (snow) canopy vegetation (-)
!
REAL, DIMENSION(:),   INTENT(IN)   :: PFLXC_CA, PFLXC_GV, PFLXC_VG_C, PFLXC_VN_C, PFLXC_GN, PFLXC_N_A,   &
                                      PFLXC_CV, PFLXC_MOM
!                                     PFLXC_CA  = Flux form heat transfer coefficient: canopy air to atmosphere (kg m-2 s-1)
!                                     PFLXC_GV  = As above, but for : ground-understory to canopy air           (kg m-2 s-1)
!                                     PFLXC_VG_C = As above, but for : non-snow buried canopy to canopy air      (kg m-2 s-1)
!                                     PFLXC_VN_C = As above, but for : partially snow-buried canopy air to canopy
!                                                  air                                                           (kg m-2 s-1)
!                                     PFLXC_CV  = As above, but for : bulk vegetation canopy to canopy air      (kg m-2 s-1)
!                                     PFLXC_GN  = As above, but for : ground-based snow to atmosphere           (kg m-2 s-1)
!                                     PFLXC_N_A  = As above, but for : ground-based snow to canopy air           (kg m-2 s-1)
!                                     PFLXC_MOM  = flux form drag transfer coefficient: canopy air to atmosphere (kg m-2 s-1)
!
REAL, DIMENSION(:,:), INTENT(IN)   :: PTG
!                                     PTG    = Soil temperature profile (K)
!
REAL, DIMENSION(:),   INTENT(IN)   :: PDQSAT_G, PDQSAT_V, PDQSATI_N
!                                     PQSAT_G  = saturation specific humidity derivative for understory
!                                                surface               (kg kg-1 K-1)
!                                     PQSAT_V  = saturation specific humidity derivative for the vegetation
!                                                canopy                (kg kg-1 K-1)
!                                     PQSATI_N = saturation specific humidity derivative over ice for the
!                                                ground-based snowpack (kg kg-1 K-1)
!
REAL, DIMENSION(:),   INTENT(IN)   :: PHVGS, PHVNS
!                                     PHVGS     = Dimensionless pseudo humidity factor for computing vapor
!                                                 fluxes from the non-buried part of the canopy to the canopy air    (-)
!                                     PHVNS     = Dimensionless pseudo humidity factor for computing vapor
!                                                 fluxes from the partly-buried part of the canopy to the canopy air (-)
!
REAL, DIMENSION(:),   INTENT(IN)   :: PTA_IC, PQA_IC
!                                     PTA_IC    = Near-ground air temperature        (K)
!                                     PQA_IC    = Near-ground air specific humidity  (kg kg-1)
!
REAL, DIMENSION(:),   INTENT(IN)   :: PSW_UP, PSW_RAD, PLW_RAD
!                                     PSW_UP  = total upwelling shortwave radiation from the surface at the atmosphere (W m-2)
!                                     PSW_RAD = downwelling shortwave radiation from the atmosphere above the canopy  (W m-2)
!                                     PLW_RAD = downwelling longwave radiation from the atmosphere above the canopy  (W m-2)
!
REAL, DIMENSION(:),   INTENT(IN)   :: PDELTA_V
!                                     PDELTA_V = Explicit canopy interception fraction                 (-)
!
REAL, DIMENSION(:),   INTENT(IN)   :: PDELTAT_V, PDELTAT_N, PDELTAT_G
!                                     PDELTAT_V = Time change in vegetation canopy temperature (K)
!                                     PDELTAT_N = Time change in snowpack surface temperature  (K)
!                                     PDELTAT_G = Time change in soil surface temperature      (K)
!
REAL, DIMENSION(:),   INTENT(OUT)  :: PRN_V, PRN_G
!                                     PRN_G = Understory-ground net radiation (W m-2)
!                                     PRN_V = Vegetation canopy net radiation (W m-2)
!
REAL, DIMENSION(:),   INTENT(OUT)  :: PLW_UP
!                                     PLW_UP  = total net longwave upwelling radiation to the atmosphere  (W m-2)
!
REAL, DIMENSION(:),   INTENT(OUT)  :: PH_N_A
!                                     PH_N_A  = Sensible heat flux: ground based snowpack to overlying atmosphere (W m-2)
!
REAL, DIMENSION(:),   INTENT(OUT)  :: PEVAP_C_A,  PEVAP_N_A, PEVAPN
!                                     PEVAP_C_A = Water flux: canopy air space to overlying atmosphere (kg m-2 s-1)
!                                     PEVAP_N_A = Water flux: ground based snowpack to overlying atmosphere (kg m-2 s-1)
!                                     PEVAPN    = Water flux: ground based snowpack to both canopy air space and overlying atmosphere (kg m-2 s-1)
!
REAL, DIMENSION(:),   INTENT(OUT)  :: PLEG, PLEGI, PLES, PLEL
!                                     PLEG        = Latent heat flux: baresoil evaporation (W m-2)
!                                     PLEGI       = Latent heat flux: baresoil sublimation (W m-2)
!                                     PLES        = Latent heat flux: net sublimation from ground-based snowpack to canopy air and overlying atmosphere (W m-2)
!                                     PLEL        = Latent heat flux: net evaporation from ground-based snowpack to canopy air and overlying atmosphere (W m-2)
!
REAL, DIMENSION(:),   INTENT(OUT)  :: PEMIS
!                                     PEMIS       = effective (aggregated) net surface emissivity (-)
!
!
!*      0.2    declarations of local variables
!
!
REAL, DIMENSION(SIZE(PEK%XTV(:)))         :: ZWORK
!                                     ZWORK = working array
!
REAL, DIMENSION(SIZE(PEK%XTV(:)))         :: ZSAIR, ZSAIRC
!                                     ZSAIR   = atmospheric value of the therodynamic variable
!                                     ZSAIRC  = canopy air value of the therodynamic variable
!
REAL, DIMENSION(SIZE(PEK%XTV(:)))         :: ZEVAP_CV
!                                     ZEVAP_CV = Water flux: Evapotranspiration vapor flux from the vegetation canopy (kg m-2 s-1)
!
REAL, DIMENSION(SIZE(PEK%XTV(:)))         :: ZQSATN_V, ZQSATIN_N, ZQSATN_G
!                                     ZQSATN_V  = saturation specific humidity (over water) for the vegetation canopy (kg kg-1)
!                                     ZQSATIN_N = saturation specific humidity (over ice) for the snow (kg kg-1)
!                                                 NOTE that liquid water can only exist when the snowpack T=XTT in the model,
!                                                 and at the freezing point, the value is the same over ice and water, therefore
!                                                 over snow, we do not need to explicitly consider a "ZQSATN_N"
!                                     ZQSATN_G  = saturation specific humidity (over water) for the understory (kg kg-1)
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*       0.     Initialization:
!               ---------------
!
IF (LHOOK) CALL DR_HOOK('ISBA_FLUXES_MEB',0,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
!*       1.     Radiative Fluxes
!               ----------------
!
! LWnet: transform from explicit to implicit (i.e. at time t+dt)
!
DEK%XLWNET_V(:)  = DEK%XLWNET_V(:) + PLWNET_V_DTV(:)*PDELTAT_V(:)   &
                                   + PLWNET_V_DTG(:)*PDELTAT_G(:)   &
                                   + PLWNET_V_DTN(:)*PDELTAT_N(:)

DEK%XLWNET_G(:)  = DEK%XLWNET_G(:) + PLWNET_G_DTV(:)*PDELTAT_V(:)   &
                                   + PLWNET_G_DTG(:)*PDELTAT_G(:)   &
                                   + PLWNET_G_DTN(:)*PDELTAT_N(:)

DEK%XLWNET_N(:)  = DEK%XLWNET_N(:) + PLWNET_N_DTV(:)*PDELTAT_V(:)   &
                                   + PLWNET_N_DTG(:)*PDELTAT_G(:)   &
                                   + PLWNET_N_DTN(:)*PDELTAT_N(:)
!
! LWup at t+dt
!
PLW_UP(:)   = PLW_RAD(:) - (DEK%XLWNET_V(:) + DEK%XLWNET_G(:) + DEK%XLWNET_N(:))
!
!
! Effective emissivity:
!
 CALL ISBA_EMIS_MEB(PEK%XPSN, PPSNA, PSIGMA_F, PSIGMA_FN, PEK%TSNOW%EMIS, PEMIS   )
!
! Now compute the effective radiative temperature while
! imposing the constraint:
!
!    LW_RAD * (1 - EMIS ) + EMIS * XSTEFAN * TS_RAD**4 = LWUP
!
! Using the effective emissivity ensures that the upwelling radiation from the surface (RHS)
! model will be equal to the upwelling radiation computed in the atmospheric model (LHS)
! (i.e. LWUP is consistent with EMIS & TS_RAD), thereby insuring energy conservation from
! the surface to the atmosphere. Solving the above equation for
! the radiative T gives:
!
DK%XTSRAD(:)    = ((PLW_UP(:) - PLW_RAD(:)*(1.0-PEMIS(:)))/(XSTEFAN*PEMIS(:)))**0.25
!
!
! Rnet (t+dt)
!
PRN_V(:)  = DEK%XSWNET_V(:) + DEK%XLWNET_V(:)
!
PRN_G(:)  = DEK%XSWNET_G(:) + DEK%XLWNET_G(:)
!
DMK%XRNSNOW(:) = DEK%XSWNET_N(:) + DEK%XLWNET_N(:)
!
!
! total Rnet (t+dt):
!
DK%XRN(:)    = PRN_G(:) + PRN_V(:) + DMK%XRNSNOW(:)
!
!
!*       2.a    Implicit (Turbulent) Sensible Heat Fluxes
!               -----------------------------------------

! First get input thermo variable (could be enthalpy (air heat capacity x potential temperature or dry static energy)

ZSAIR(:)  = PTHRMB_TA(:) + PTHRMA_TA(:)*PTA_IC(:)
ZSAIRC(:) = PTHRMB_TC(:) + PTHRMA_TC(:)*PEK%XTC(:)

! Sensible heat fluxes (W m-2):
! - Canopy air to atmosphere, vegetation canopy to canopy air (implicitly includes from canopy intercepted snow),
!   understory-ground to canopy air,
!   ground-based snow to canopy air, ground-based snow to atmosphere:

DEK%XH_CA(:)  = PFLXC_CA (:) *( ZSAIRC(:)                                      - ZSAIR (:))*(1.0 - PEK%XPSN(:)*PPSNA(:))
DEK%XH_CV(:)  = PFLXC_CV (:) *( PTHRMB_TV(:) + PTHRMA_TV(:)*PEK%XTV(:)         - ZSAIRC(:))
DEK%XH_GV(:)  = PFLXC_GV (:) *( PTHRMB_TG(:) + PTHRMA_TG(:)*PTG(:,1)           - ZSAIRC(:))*(1.0 - PEK%XPSN(:))
DEK%XH_GN(:)  = PFLXC_GN (:) *( PTHRMB_TN(:) + PTHRMA_TN(:)*DMK%XSNOWTEMP(:,1) - ZSAIRC(:))*       PEK%XPSN(:) *(1.0-PPSNA(:))
PH_N_A   (:)  = PFLXC_N_A(:) *( PTHRMB_TN(:) + PTHRMA_TN(:)*DMK%XSNOWTEMP(:,1) - ZSAIR (:))*       PEK%XPSN(:) *     PPSNA(:)

! - Net sensible heat flux from ground-based snow (to the canopy and the atmosphere (from
!   the buried-vegetation canopy fraction)) (W m-2)

DMK%XHSNOW(:) = DEK%XH_GN(:) + PH_N_A(:)

! FINAL sensible heat flux to the atmosphere (W m-2):

DK%XH(:)      = DEK%XH_CA(:) + PH_N_A(:)

!
!*       2.b    Implicit (Turbulent) Vapor and Latent Heat Fluxes
!               -------------------------------------------------
! Note, to convert any of the latent heat fluxes back to vapor fluxes,
! simply divide by XLVTT, even sublimation fluxes as XLSTT already accounted for.

! - first get 'new' surface specific humidities, qsatn, at time t+dt:

ZQSATN_G(:)  =        PQSAT_G(:)  + PDQSAT_G(:)  * PDELTAT_G(:)
ZQSATN_V(:)  =        PQSAT_V(:)  + PDQSAT_V(:)  * PDELTAT_V(:)
ZQSATIN_N(:) =        PQSATI_N(:) + PDQSATI_N(:) * PDELTAT_N(:)

! - Evaporation and Sublimation latent heat fluxes from the soil, respectively:
! (W m-2)

ZWORK(:)      = (1.-PEK%XPSN(:)-KK%XFF(:)) * PFLXC_GV(:)
PLEG(:)       = ZWORK(:)*PLEG_DELTA(:) *( DK%XHUG(:) *ZQSATN_G(:) - PEK%XQC(:) )*(1.-PFROZEN1(:))*XLVTT
PLEGI(:)      = ZWORK(:)*PLEGI_DELTA(:)*(   PHUGI(:) *ZQSATN_G(:) - PEK%XQC(:) )*    PFROZEN1(:) *XLSTT

! - Latent heat flux from frozen and unfrozen flooded zones (W m-2)

ZWORK(:)      = KK%XFF(:) * PFLXC_GV(:)*( ZQSATN_G(:) - PEK%XQC(:) )
DEK%XLE_FLOOD(:)  = ZWORK(:) * (1.-KK%XFFROZEN(:))* XLVTT
DEK%XLEI_FLOOD(:) = ZWORK(:) *     KK%XFFROZEN(:) * XLSTT

! - Evapotranspiration vapor flux from the vegetation canopy (kg m-2 s-1)

ZEVAP_CV(:) = (1.-PPSNCV(:)) * PHVGS(:) * PFLXC_CV(:)*( ZQSATN_V(:) - PEK%XQC(:) ) * (XLVTT/PLTT(:))

! - Latent heat flux from the canopy (liquid) water interception reservoir (W m-2)

DEK%XLER_CV(:) = ( (1.-PPSNA(:))*PEK%XPSN(:) * PFLXC_VN_C(:)    +              &
                             (1.-PEK%XPSN(:))* PFLXC_VG_C(:)  ) *              &
                 XLVTT * (1.-PPSNCV(:))* PDELTA_V(:) * ( ZQSATN_V(:) - PEK%XQC(:) )
!
! - Total latent heat flux (evapotranspiration) from the vegetation to the canopy air space (W m-2)
!   *without* sublimation (for TOTAL evapotranspiration and sublimation, add PLESC here)

DEK%XLEV_CV(:)  = PLTT(:) * ZEVAP_CV(:)
!
! - latent heat flux from transpiration from the canopy (W m-2)

DEK%XLETR_CV(:) = DEK%XLEV_CV(:) - DEK%XLER_CV(:)

! Snow sublimation and evaporation latent heat flux from canopy-intercepted snow (W m-2)

DEK%XLES_CV(:)  =  PPSNCV(:) * XLSTT * PHVNS(:) * PFLXC_CV(:)*( ZQSATN_V(:) - PEK%XQC(:) )

! - Total latent heat flux from vegetation canopy overstory to canopy air space
!   (including transpiration, liquid water store, canopy snow sublimation):

DEK%XLE_CV(:)   = DEK%XLEV_CV(:) + DEK%XLES_CV(:)

! - Vapor flux from the ground-based snowpack to the canopy air (kg m-2 s-1):

ZWORK(:)        = PFLXC_GN(:)*(ZQSATIN_N(:) - PEK%XQC(:))*PEK%XPSN(:)*(1.0-PPSNA(:))
DEK%XEVAP_GN(:) = ZWORK(:)*(XLSTT/PLTT(:))
DEK%XLE_GN(:)   = ZWORK(:)* XLSTT                              ! W m-2

! - latent heat flux from transpiration from canopy veg (evapotranspiration)

DEK%XLETR(:)     = DEK%XLETR_CV(:)

! Total latent heat flux from transpiration from understory veg and canopy veg (evapotranspiration and sublimation)
!   and intercepted water on both reservoirs (W m-2)

DEK%XLEV(:)      = DEK%XLETR(:) + DEK%XLER_CV(:)

! Total latent heat flux from intercepted water (canopy and understory vegetation):
! (does not include intercepted snow sublimation): W m-2

DEK%XLER(:)      = DEK%XLER_CV(:)

! - Vapor flux from the ground-based snowpack (part burying the canopy vegetation) to the atmosphere (kg m-2 s-1):

ZWORK(:)     = PFLXC_N_A(:) *( ZQSATIN_N(:) - PQA_IC(:))*       PEK%XPSN(:)*     PPSNA(:)
PEVAP_N_A(:) = ZWORK(:) *(XLSTT/XLVTT)

! - Net Snow (groud-based) sublimation latent heat flux (W m-2) to the canopy air space and the overlying atmosphere:

PLES(:)      = ( PFLXC_GN(:) *( ZQSATIN_N(:) - PEK%XQC(:))*       PEK%XPSN(:)*(1.0-PPSNA(:)) + ZWORK(:) ) * XLSTT

! - Net Snow evaporation (liquid water) latent heat flux (W m-2)

PLEL(:)      = PLTT(:)*(DEK%XEVAP_GN(:) + PEVAP_N_A(:)) - PLES(:)

! - Total mass flux from ground-based snowpack (kg m-2 s-1)

PEVAPN(:)    = (PLEL(:) + PLES(:))/PLTT(:)

! - Total snow-free vapor flux from the understory (flooded areas, baresoil and understory vegetation)
!   to the canopy air space (W m-2 and kg m-2 s-1, respectively):

DEK%XLE_GV(:)   = DEK%XLE_FLOOD(:) + DEK%XLEI_FLOOD(:) + PLEGI(:) + PLEG(:)

DEK%XEVAP_G(:)  = DEK%XLE_GV(:)/XLVTT

! - Net vapor flux from canopy air to the atmosphere (kg m-2 s-1)

PEVAP_C_A(:) = PFLXC_CA(:) *( PEK%XQC(:) - PQA_IC(:))*(1.0 - PEK%XPSN(:)*PPSNA(:))

DEK%XLE_CA(:)   = PLTT(:) * PEVAP_C_A(:) ! W m-2

! FINAL net vapor flux from the surface to the Atmosphere:
! - Net vapor flux from canopy air and exposed ground based snow (from part of snow
!   burying the vegetation canopy) to the atmosphere (kg m-2 s-1)
!
DK%XEVAP(:)     = PEVAP_C_A(:) + PEVAP_N_A(:)
!
! Total latent heat flux of surface/snow/vegetation: W m-2
!
PEK%XLE(:)       = DK%XEVAP(:)*PLTT(:)
!
! Total sublimation from the surface/snow/vegetation: W m-2
!
DK%XLEI(:)      = DEK%XLES_CV(:) + PLEGI(:) + DEK%XLEI_FLOOD(:)
!
! Total sublimation from the surface/snow/vegetation: kg m-2 s-1
!
DK%XSUBL(:)     = DK%XLEI(:)/ PLTT(:)
!
IF (LHOOK) CALL DR_HOOK('ISBA_FLUXES_MEB',1,ZHOOK_HANDLE)
!
END SUBROUTINE ISBA_FLUXES_MEB


