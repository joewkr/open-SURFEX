!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE ISBA_SNOW_AGR(KK, PK, PEK, DMK, DK, DEK, &
                               OMEB, OMEB_LITTER, PEXNS, PEXNA, PTA, PQA,  &
                               PZREF, PUREF, PDIRCOSZW, PVMOD, PRR, PSR,   &
                               PEMIS, PALB, PUSTAR, PLES3L, PLEL3L,        &
                               PEVAP3L, PQS3L, PALB3L, PGSFCSNOW,          &
                               PZGRNDFLUX, PFLSN_COR, PEMIST, PPALPHAN )
!     ##########################################################################
!
!
!!****  *ISBA_SNOW_AGR* aggregates snow free and snow fluxes
!!
!!    PURPOSE
!!    -------
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
!!    Noilhan and Planton (1989)
!!      
!!    AUTHOR
!!    ------
!!      V. Masson           * Meteo-France *
!!      (following A. Boone)
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/03/95 
!!      B. Decharme 01/2009  Floodplains 
!!      B. Decharme 01/2010  Effective surface temperature (for diag)
!!      B. Decharme 09/2012  Bug total sublimation flux: no DEK%XLESL
!!      B. Decharme 04/2013  Bug wrong radiative temperature
!!                           Sublimation diag flux
!!                           Qs for 3l or crocus (needed for coupling with atm)
!!      A. Boone    11/2014  MEB
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
USE MODD_ISBA_n, ONLY : ISBA_K_t, ISBA_P_t, ISBA_PE_t
USE MODD_DIAG_n, ONLY : DIAG_t
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_t
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!              -------------------------
!
!
!* general variables
!  -----------------
!
TYPE(ISBA_K_t), INTENT(INOUT) :: KK
TYPE(ISBA_P_t), INTENT(INOUT) :: PK
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
TYPE(DIAG_t), INTENT(INOUT) :: DK
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: DEK
TYPE(DIAG_MISC_ISBA_t), INTENT(INOUT) :: DMK
!
LOGICAL,              INTENT(IN)  :: OMEB       ! True = patch with multi-energy balance 
!                                               ! False = patch with classical ISBA
LOGICAL, INTENT(IN)               :: OMEB_LITTER !True = litter option activated
!                                                 ! over the ground
!
!* surface and atmospheric parameters
!  ----------------------------------
!
REAL, DIMENSION(:), INTENT(IN)  :: PEXNS     ! Exner function at the surface
REAL, DIMENSION(:), INTENT(IN)  :: PEXNA     ! Exner function
REAL, DIMENSION(:), INTENT(IN)  :: PTA       ! air temperature
REAL, DIMENSION(:), INTENT(IN)  :: PQA       ! air specific humidity
REAL, DIMENSION(:), INTENT(IN)  :: PZREF     ! reference height of the first atmospheric level
REAL, DIMENSION(:), INTENT(IN)  :: PUREF     ! reference height of the wind
REAL, DIMENSION(:), INTENT(IN)  :: PDIRCOSZW ! Cosinus of the angle between the normal to the surface and the vertical
REAL, DIMENSION(:), INTENT(IN)  :: PVMOD     ! module of the horizontal wind
REAL, DIMENSION(:), INTENT(IN)  :: PRR       ! Rain rate (in kg/m2/s)
REAL, DIMENSION(:), INTENT(IN)  :: PSR       ! Snow rate (in kg/m2/s)
!
!* surface parameters
!  ------------------
!
REAL, DIMENSION(:), INTENT(IN)  :: PALB       ! albedo 
REAL, DIMENSION(:), INTENT(IN)  :: PEMIS      ! emissivity
!  'D95'     : represents aggregated (snow + flood + snow-flood-free) albedo and emissivity
!  '3-L'     : represents                    flood + snow-flood-free  albedo and emissivity
!  'MEB+3-L' : represents aggregated (snow + flood + snow-flood-free) albedo and emissivity
!
!
!* snow fractions
!  --------------
!
REAL, DIMENSION(:), INTENT(IN)  :: PPALPHAN   ! fraction of the the explicit veg.
!                                             ! canopy buried by snow
!
!* ISBA-SNOW3L variables/parameters:
!  ---------------------------------
!
! Prognostic variables:
!
REAL, DIMENSION(:),   INTENT(IN) :: PALB3L      ! Snow albedo
REAL, DIMENSION(:),   INTENT(IN) :: PQS3L       ! Surface humidity
! 
! Diagnostics:
!
REAL, DIMENSION(:), INTENT(IN)    :: PZGRNDFLUX ! snow/soil-biomass interface flux (W/m2)
REAL, DIMENSION(:), INTENT(IN)    :: PFLSN_COR  ! snow/soil-biomass correction flux (W/m2)
!
REAL, DIMENSION(:), INTENT(INOUT) :: PGSFCSNOW  ! heat flux from snow sfc to sub sfc layers (W/m2)
REAL, DIMENSION(:), INTENT(IN)    :: PLES3L     ! sublimation from ISBA-ES(3L)
REAL, DIMENSION(:), INTENT(IN)    :: PLEL3L     ! evaporation heat flux of water in the snow (W/m2)
REAL, DIMENSION(:), INTENT(INOUT) :: PEVAP3L    ! evaporation flux over snow from ISBA-ES (kg/m2/s)
!
!* diagnostic variables
!  --------------------
!
REAL, DIMENSION(:), INTENT(INOUT) :: PEMIST   ! total surface emissivity
!
!* surface fluxes
!  --------------
!
REAL, DIMENSION(:), INTENT(INOUT) :: PUSTAR   ! friction velocity
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PTA))       :: ZWORK
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ISBA_SNOW_AGR',0,ZHOOK_HANDLE)
!
ZWORK(:) = 0.
!
IF(OMEB)THEN
  !
  ! Snow free (ground-based snow) diagnostics: canopy and ground blended (W m-2):
  ! NOTE that the effects of snow cover *fraction* are implicitly *included* in these fluxes 
  ! so do NOT multiply by snow fraction.
  !
  DEK%XRN_SN_FR   (:) = DEK%XSWNET_V(:) + DEK%XSWNET_G(:) + DEK%XLWNET_V(:) + DEK%XLWNET_G(:)
  DEK%XH_SN_FR    (:) = DEK%XH_CV(:) + DEK%XH_GN(:)
  IF (OMEB_LITTER) THEN
    DEK%XLEG_SN_FR (:) = DEK%XLELITTER (:)
    DEK%XLEGI_SN_FR(:) = DEK%XLELITTERI(:)
    DEK%XLEG (:) = DEK%XLELITTER (:)
    DEK%XLEGI(:) = DEK%XLELITTERI(:)
  ELSE
    DEK%XLEG_SN_FR (:) = DEK%XLEG (:)
    DEK%XLEGI_SN_FR(:) = DEK%XLEGI(:)
  ENDIF

  DEK%XLEV_SN_FR (:) = DEK%XLEV_CV (:)
  DEK%XLETR_SN_FR(:) = DEK%XLETR_CV(:) 
  ! NOTE for now, this is same as total Ustar (includes snow)   
  DEK%XUSTAR_SN_FR(:) = PUSTAR       (:)        
  ! LER does not include intercepted snow sublimation
  DEK%XLER_SN_FR  (:) = DEK%XLEV_CV(:) - DEK%XLETR_CV(:) 

  DEK%XLEI_SN_FR  (:) = DEK%XLEGI(:) + DEK%XLEI_FLOOD(:) + DEK%XLES(:) + DEK%XLES_CV(:)
  ! LE includes intercepted snow sublimation
  DEK%XLE_SN_FR   (:) = DEK%XLEG_SN_FR(:) + DEK%XLEGI_SN_FR(:) + DEK%XLEV_SN_FR(:) + &
                 DEK%XLES_CV(:) + DEK%XLE_FLOOD(:) + DEK%XLEI_FLOOD(:)
  DEK%XGFLUX_SN_FR(:) = DEK%XRN_SN_FR(:) - DEK%XH_SN_FR(:) - DEK%XLE_SN_FR(:)
  !
  PEMIST(:) = PEMIS(:)
  !
  ! Effective surface temperature (for diag): for MEB:

  ZWORK   (:) =  PPALPHAN(:)*PEK%XPSN(:)
  DK%XTS(:) = (1.0 - ZWORK(:))*PEK%XTC(:) + ZWORK(:)*DMK%XSNOWTEMP(:,1)
  !
  ! Total heat FLUX into snow/soil/vegetation surface:
  !
  DK%XGFLUX(:) = DK%XRN(:) - DK%XH(:) - PEK%XLE(:) + DMK%XHPSNOW(:) 
  !
ELSE
!
! * 2. Using an explicit snow scheme option with composite soil/veg ISBA:
!      ------------------------------------------------------------------
!
   IF(PEK%TSNOW%SCHEME == '3-L' .OR. PEK%TSNOW%SCHEME == 'CRO')THEN
!
!     Save fluxes from Force-Restore snow/explicit snow-free
!     portion of grid box (vegetation/soil):
!
      DEK%XLEG_SN_FR  (:) = DEK%XLEG (:)
      DEK%XLEGI_SN_FR (:) = DEK%XLEGI(:)
      DEK%XLEV_SN_FR  (:) = DEK%XLEV (:)
      DEK%XLETR_SN_FR (:) = DEK%XLETR(:)
      DEK%XLER_SN_FR  (:) = DEK%XLER (:)
      DEK%XRN_SN_FR   (:) = DK%XRN   (:)
      DEK%XH_SN_FR    (:) = DK%XH    (:)
      DEK%XUSTAR_SN_FR(:) = PUSTAR   (:)      

      DEK%XLE_SN_FR   (:) = PEK%XLE(:)
      DEK%XGFLUX_SN_FR(:) = DK%XGFLUX(:)
!  
      DEK%XLEI_SN_FR  (:)= DEK%XLEGI(:) + DEK%XLEI_FLOOD(:) + DEK%XLES(:)
!
!     Effective surface temperature (for diag):
!
      DK%XTS(:) = (1.-PEK%XPSN(:))*PEK%XTG(:,1)+PEK%XPSN(:)*DMK%XSNOWTEMP(:,1)
!
!     Effective surface radiating temperature:
!
      DK%XALBT (:) = PALB (:)*(1.-PEK%XPSN(:)) + PEK%XPSN(:)*PALB3L(:)
      PEMIST     (:) = PEMIS(:)*(1.-PEK%XPSN(:)) + PEK%XPSN(:)*PEK%TSNOW%EMIS(:)
!  
      DK%XTSRAD(:) = ( ((1.-PEK%XPSN(:))*PEMIS(:)*PEK%XTG(:,1)**4 +   &
                               PEK%XPSN(:) *PEK%TSNOW%EMIS(:)*DMK%XSNOWTEMP(:,1)**4     &
                            )/PEMIST(:) )**(0.25)  
!
!     Calculate actual fluxes from snow-free natural
!     portion of surface: NET flux from surface is the sum of
!     fluxes from snow free and snow covered portions
!     of natural portion of grid box when *ISBA-ES* in force.
!     when NOT in use, then these fluxes equal those above.
!
      DK%XRN   (:) = (1.-PEK%XPSN(:)) * DK%XRN(:) + PEK%XPSN(:) * DMK%XRNSNOW(:)
      DK%XH    (:) = (1.-PEK%XPSN(:)) * DK%XH (:) + PEK%XPSN(:) * DMK%XHSNOW(:)
!  
      DEK%XLEG (:) = (1.-PEK%XPSNG(:)-KK%XFFG(:)) * DEK%XLEG(:)  
      DEK%XLEGI(:) = (1.-PEK%XPSNG(:)-KK%XFFG(:)) * DEK%XLEGI(:)  
      DEK%XLEV (:) = (1.-PEK%XPSNV(:)-KK%XFFV(:)) * DEK%XLEV(:)   
      DEK%XLETR(:) = (1.-PEK%XPSNV(:)-KK%XFFV(:)) * DEK%XLETR(:)  
      DEK%XLER (:) = (1.-PEK%XPSNV(:)-KK%XFFV(:)) * DEK%XLER(:)  
!
!     Total evapotranspiration flux (kg/m2/s):
!
      DK%XEVAP(:) = (DEK%XLEV(:) + DEK%XLEG(:))/PK%XLVTT(:) + DEK%XLEGI(:)/PK%XLSTT(:) + &
                                   DEK%XLE_FLOOD(:)/PK%XLVTT(:) + DEK%XLEI_FLOOD(:)/PK%XLSTT(:) + &
                                   PEK%XPSN(:) * PEVAP3L(:)
!
!     ISBA-ES/SNOW3L fluxes:
!
      DEK%XLES     (:) = PEK%XPSN(:) * PLES3L(:)
      DEK%XLESL    (:) = PEK%XPSN(:) * PLEL3L(:)
      DMK%XRNSNOW   (:) = PEK%XPSN(:) * DMK%XRNSNOW   (:)
      DMK%XHSNOW    (:) = PEK%XPSN(:) * DMK%XHSNOW    (:)
      DMK%XGFLUXSNOW(:) = PEK%XPSN(:) * DMK%XGFLUXSNOW(:)
      DMK%XSNOWHMASS(:) = PEK%XPSN(:) * DMK%XSNOWHMASS(:)  ! (J m-2)
      DMK%XHPSNOW   (:) = PEK%XPSN(:) * DMK%XHPSNOW   (:)
      PGSFCSNOW      (:) = PEK%XPSN(:) * PGSFCSNOW(:)
      PEVAP3L        (:) = PEK%XPSN(:) * PEVAP3L  (:)
!
!     Total heat flux between snow and soil
!
      DMK%XGRNDFLUX(:) = PEK%XPSN(:) * (PZGRNDFLUX(:)+PFLSN_COR(:))
      DEK%XMELTADV(:) = PEK%XPSN(:) * DEK%XMELTADV(:)
!
!     Total evaporative flux (W/m2) :
!
      PEK%XLE(:) = DEK%XLEG(:) + DEK%XLEV(:) + DEK%XLES(:) + DEK%XLESL(:) + &
                   DEK%XLEGI(:) + DEK%XLE_FLOOD(:) + DEK%XLEI_FLOOD(:)
!
!     Total sublimation flux (W/m2) :
!
      DK%XLEI  (:) = DEK%XLES(:) + DEK%XLEGI(:) + DEK%XLEI_FLOOD(:)
!
!     Total sublimation flux (kg/m2/s):
!
      DK%XSUBL (:) = DK%XLEI(:)/PK%XLSTT(:)
!
!     Total FLUX into snow/soil/vegetation surface:
!
      DK%XGFLUX(:) = DK%XRN(:) - DK%XH(:) - PEK%XLE(:) + DMK%XHPSNOW(:)  
!
!     surface humidity:
!
      DK%XQS   (:) = (1.-PEK%XPSN(:)) * DK%XQS(:) + PEK%XPSN(:) * PQS3L(:)
!
!     near-surface humidity :
!  
      DK%XHU   (:) = (1.-PEK%XPSN(:)) * DK%XHU(:) + PEK%XPSN(:)
!
!     Momentum fluxes:
!
      PUSTAR     (:) = SQRT( (1.-PEK%XPSN(:))  * PUSTAR(:)**2  + &
                                 PEK%XPSN(:) * DMK%XUSTARSNOW(:)**2 )
!
!     Richardson number and Drag coeff:
!
      CALL COMPUT_RI_DRAG
!
   ELSE
!
      DK%XTS    (:)  = PEK%XTG(:,1)
      DK%XTSRAD (:)  = PEK%XTG(:,1)
      DK%XALBT  (:)  = PALB (:)
      PEMIST    (:)  = PEMIS(:)
!  
!     Total sublimation flux (W/m2) :
      DK%XLEI   (:)  = DEK%XLES(:) + DEK%XLEGI(:) + DEK%XLEI_FLOOD(:)
!
!     Total sublimation flux (kg/m2/s):
      DK%XSUBL  (:)  = DK%XLEI(:)/PK%XLSTT(:)
!
   ENDIF
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('ISBA_SNOW_AGR',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
CONTAINS
!-------------------------------------------------------------------------------
!
SUBROUTINE COMPUT_RI_DRAG
!
USE MODD_SURF_ATM, ONLY : LDRAG_COEF_ARP, LRRGUST_ARP,   &
                          XRRSCALE, XRRGAMMA, XUTILGUST  
!
USE MODI_SURFACE_RI
USE MODI_SURFACE_AERO_COND
USE MODI_SURFACE_CD
USE MODI_SURFACE_CDCH_1DARP
USE MODI_WIND_THRESHOLD
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PTA)) :: ZFP, ZRRCOR, ZVMOD, ZAC, ZRA
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ISBA_SNOW_AGR:COMPUT_RI_DRAG',0,ZHOOK_HANDLE)
!
! * Richardson number
!
CALL SURFACE_RI(DK%XTS, DK%XQS, PEXNS, PEXNA, PTA, PQA,  &
                PZREF, PUREF, PDIRCOSZW, PVMOD, DK%XRI)  
!
! * Wind check
!
ZVMOD = WIND_THRESHOLD(PVMOD,PUREF)
!
! * Drag coefficient for heat and momentum
!
IF (LDRAG_COEF_ARP) THEN
   CALL SURFACE_CDCH_1DARP(PZREF, DK%XZ0EFF, DK%XZ0H, ZVMOD, PTA, PEK%XTG(:,1), &
                             PQA, DK%XQS, DK%XCD, DK%XCDN, DK%XCH              )
ELSE
   CALL SURFACE_AERO_COND(DK%XRI, PZREF, PUREF, ZVMOD, DK%XZ0, DK%XZ0H, ZAC, ZRA, DK%XCH)
   CALL SURFACE_CD(DK%XRI, PZREF, PUREF, DK%XZ0EFF, DK%XZ0H, DK%XCD, DK%XCDN)
ENDIF
!
IF (LRRGUST_ARP) THEN
   ZFP(:)=MAX(0.0,PRR(:)+PSR(:))
   ZRRCOR(:)=SQRT(1.0+((((ZFP(:)/(ZFP(:)+XRRSCALE))**XRRGAMMA)*XUTILGUST)**2) &
       /(DK%XCD(:)*ZVMOD(:)**2))  
   DK%XCD  = DK%XCD  * ZRRCOR
   DK%XCH  = DK%XCH  * ZRRCOR
   DK%XCDN = DK%XCDN * ZRRCOR
ENDIF
!
IF (LHOOK) CALL DR_HOOK('ISBA_SNOW_AGR:COMPUT_RI_DRAG',1,ZHOOK_HANDLE)
!
END SUBROUTINE COMPUT_RI_DRAG
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE ISBA_SNOW_AGR
