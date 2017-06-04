!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ######spl
      SUBROUTINE ISBA_FLUXES(IO, KK, PK, PEK, DMK, PTSTEP, &
                             PSW_RAD, PLW_RAD, PTA, PQA, PRHOA, PEXNS, PEXNA, &
                             PHUG, PHUI, PLEG_DELTA, PLEGI_DELTA, PDELTA, PF5, PCS, PTSM, PT2M, &
                             PFROZEN1, PALBT, PEMIST, PQSAT, PDQSAT, PSNOW_THRUFAL, &
                             PRN, PH, PLE, PLEG, PLEGI, PLEV,  PLES, PLER, PLETR, PEVAP, &
                             PGFLUX, PMELTADV, PMELT, PSOILCONDZ, PLE_FLOOD, PLEI_FLOOD)
!     ##########################################################################
!
!!****  *ISBA_FLUXES*  
!!
!!    PURPOSE
!!    -------
!
!     Calculates the simple snowpack schemes melt and the surface fluxes.
!         
!     
!!**  METHOD
!!    ------
!
!     1- snow melt latent heat, liquid rate (DEF option)
!     2- derive the surface fluxes.
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
!!    REFERENCE
!!    ---------
!!
!!    Noilhan and Planton (1989)
!!    Belair (1995)
!!    Douville et al. (1995)
!!    Boone et al. (2000)
!!      
!!    AUTHOR
!!    ------
!!
!!      S. Belair           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    14/03/95 
!!      (J.Stein)   15/11/95  use the wind components in the flux computation
!!      (J.Noilhan) 15/03/96  use the potential temperature instead of the
!!                            temperature for the heat flux computation 
!!      (J.Stein)   27/03/96  use only H and LE in the soil scheme
!!      (A.Boone V.Masson) 05/10/98 splits e_budget in two for CO2
!!      (A.Boone)   03/10/99  explicit latent heat of sublimation calculated 
!!      (A.Boone)   08/11/00  snow melt changes herein
!!      (A.Boone)   06/05/02  Updates, ordering. 
!!                            Introduction of snow melt timescale to 'DEF' snow option
!!      (P.LeMoigne) 01/07/05 expression of latent heat flux as a function of
!!                            w'theta' instead of w'T' (divison by surface exner)
!!      (P.LeMoigne) 28/07/05 dependence on qs for cp
!!      (A. Dziedzic and PLM) 10/2006 EBA snow option
!!      (B. Decharme)01/2009  Floodplains
!!      (R. Hamdi)   01/09    Cp and L are not constants (As in ALADIN)
!!      (B. Decharme)09/2009  Close the energy budget with the D95 snow scheme
!!      (A.Boone)    03/2010  Add delta fnctions to force LEG ans LEGI=0
!!                            when hug(i)Qsat < Qa and Qsat > Qa
!!      (A.Boone)    11/2011  Add RS_max limit to Etv
!!      (B. Decharme)07/2012  Error in restore flux calculation (only for diag)
!!      (B. Decharme)10/2012  Melt rate with D95 computed using max(XTAU,PTSTEP)
!!      (A.Boone)    02/2013  Split soil phase changes into seperate routine
!!      (B. Decharme)04/2013  Pass soil phase changes routines in hydro.F90
!!      (B. Decharme)04/2013  Delete PTS_RAD because wrong diagnostic
!!      (B. Decharme)10/14    "Restore" flux computed in e_budget
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_K_t, ISBA_P_t, ISBA_PE_t
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t
!
USE MODD_CSTS,       ONLY : XSTEFAN, XCPD, XLSTT, XLVTT, XCL, XTT, XPI, XDAY, &
                            XCI, XRHOLI, XLMTT, XRHOLW, XG, XCL, XCONDI
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_ISBA_PAR,   ONLY : XWGMIN, XSPHSOIL, XDRYWGHT, XRS_MAX
USE MODD_SNOW_PAR,   ONLY : XTAU_SMELT
!
USE MODE_THERMOS
!
USE MODE_SURF_SNOW_FRAC
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_K_t), INTENT(INOUT) :: KK
TYPE(ISBA_P_t), INTENT(INOUT) :: PK
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
TYPE(DIAG_MISC_ISBA_t), INTENT(INOUT) :: DMK
!
REAL, INTENT (IN)                   :: PTSTEP     ! model time step (s)
!
REAL, DIMENSION(:), INTENT (IN)     :: PSW_RAD, PLW_RAD, PTA, PQA, PRHOA
!                                      PSW_RAD = incoming solar radiation
!                                      PLW_RAD = atmospheric infrared radiation
!                                      PTA = near-ground air temperature
!                                      PQA = near-ground air specific humidity
!                                      PRHOA = near-ground air density
!
REAL, DIMENSION(:), INTENT(IN)      :: PEXNS, PEXNA
REAL, DIMENSION(:), INTENT(IN)      :: PHUG, PHUI, PDELTA, PF5
REAL, DIMENSION(:), INTENT(IN)      :: PFROZEN1
REAL, DIMENSION(:), INTENT(IN)      :: PALBT, PEMIST
REAL, DIMENSION(:), INTENT(IN)      :: PQSAT, PDQSAT
REAL, DIMENSION(:), INTENT(IN)      :: PLEG_DELTA, PLEGI_DELTA
!                                      PHUG = relative humidity of the soil
!                                      PF5 = water stress numerical correction factor (based on F2)
!                                      PDELTA = fraction of the foliage covered
!                                               by intercepted water
!                                      PFROZEN1 = fraction of ice in near-surface
!                                                 ground
!                                      PALBT = area averaged albedo
!                                      PEMIST = area averaged emissivity
!                                      PQSAT = stauration vapor humidity at 't'
!                                      PDQSAT= stauration vapor humidity derivative at 't'
!                                      PLEG_DELTA = soil evaporation delta fn
!                                      PLEGI_DELTA = soil evaporation delta fn
!
REAL, DIMENSION(:), INTENT (IN)     :: PCS, PT2M, PTSM
!                                      PCS    = heat capacity of the snow (K m2 J-1)
!                                      PT2M   = mean surface (or restore) temperature at start 
!                                               of time step (K)
!                                      PTSM   = surface temperature at start 
!                                               of time step (K)
REAL, DIMENSION(:), INTENT(IN)      :: PSNOW_THRUFAL
!                                      PSNOW_THRUFAL = rate that liquid water leaves snow pack: 
!                                                     ISBA-ES [kg/(m2 s)]
REAL, DIMENSION(:,:), INTENT(IN)    :: PSOILCONDZ
!                                      PSOILCONDZ= ISBA-DF Soil conductivity profile  [W/(m K)]
!
REAL, DIMENSION(:), INTENT(OUT)     :: PLE_FLOOD, PLEI_FLOOD !Floodplains latent heat flux [W/m²]
!
REAL, DIMENSION(:), INTENT(OUT)     :: PRN, PH, PLE, PLEG, PLEV, PLES
REAL, DIMENSION(:), INTENT(OUT)     :: PLER, PLETR, PEVAP, PGFLUX, PMELTADV, PMELT
!                                     PRN = net radiation at the surface
!                                     PH = sensible heat flux
!                                     PLE = latent heat flux
!                                     PLEG = latent heat flux from the soil surface
!                                     PLEV = latent heat flux from the vegetation
!                                     PLES = latent heat flux from the snow
!                                     PLER = direct evaporation from the fraction
!                                            delta of the foliage
!                                     PLETR = transpiration of the remaining
!                                             part of the leaves
!                                     PEVAP = total evaporative flux (kg/m2/s)
!                                     PGFLUX = ground flux
!                                     PMELTADV = heat advection by melting snow
!                                                (acts to restore temperature to
!                                                 melting point) (W/m2)
!                                     PMELT = melting rate of snow (kg m-2 s-1)
!
REAL, DIMENSION(:), INTENT(OUT)     :: PLEGI
!                                      PLEGI   = sublimation component of the 
!                                                latent heat flux from the soil surface
!
!*      0.2    declarations of local variables
!
REAL                        :: ZKSOIL     ! coefficient for soil freeze/thaw
!
REAL, DIMENSION(SIZE(PTA))  :: ZZHV, ZTN, ZDT
!                                      ZZHV = for the calculation of the latent
!                                             heat of evapotranspiration
!!                                     ZTN  = average temperature used in 
!                                             the calculation of the 
!                                             melting effect
!                                      ZDT  = temperature change (K)
!
REAL, DIMENSION(SIZE(PTA))  ::  ZPSN, ZPSNV, ZPSNG, ZFRAC
!                               ZPSN, ZPSNV, ZPSNG = snow fractions corresponding to
!                                                    dummy arguments PEK%XPSN(:), PEK%XPSNG(:), PEK%XPSNV(:)
!                                                    if PEK%TSNOW%SCHEME = 'DEF' (composite
!                                                    or Force-Restore snow scheme), else
!                                                    they are zero for explicit snow case
!                                                    as snow fluxes calculated outside of
!                                                    this routine using the 
!                                                    PEK%TSNOW%SCHEME = '3-L' option.
!
REAL, DIMENSION(SIZE(PTA))  ::  ZNEXTSNOW
!                               ZNEXTSNOW = Future snow reservoir to close the
!                                           energy budget (see hydro_snow.f90)
!
REAL, DIMENSION(SIZE(PTA))  ::  ZCONDAVG
!                               ZCONDAVG   = average thermal conductivity of surface
!                                            and sub-surface layers (W m-1 K-1)
!
!
!*      0.2    local arrays for EBA scheme
!
REAL                            :: ZEPS1
!
!*      0.3    declarations of local parameters
!
INTEGER         :: JJ
!
REAL, DIMENSION(SIZE(PTA))      :: ZWORK1, ZWORK2, ZWORK3
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
!
!*       0.     Initialization
!               --------------
IF (LHOOK) CALL DR_HOOK('ISBA_FLUXES',0,ZHOOK_HANDLE)
!
IF (PEK%TSNOW%SCHEME == 'EBA') ZEPS1=1.0E-8
!
PMELT(:)        = 0.0
PLER(:)         = 0.0 
!
ZTN(:)          = 0.0
ZDT(:)          = 0.0
!
! If ISBA-ES option in use, then snow covered surface
! fluxes calculated outside of this routine, so set
! the local snow fractions here to zero:
! 
IF(PEK%TSNOW%SCHEME == '3-L' .OR. PEK%TSNOW%SCHEME == 'CRO' .OR. IO%CISBA == 'DIF')THEN
   ZPSN(:)      = 0.0
   ZPSNG(:)     = 0.0
   ZPSNV(:)     = 0.0
ELSE
   ZPSN(:)      = PEK%XPSN(:)
   ZPSNG(:)     = PEK%XPSNG(:)+KK%XFFG(:)
   ZPSNV(:)     = PEK%XPSNV(:)+KK%XFFV(:)
   ZFRAC(:)     = PEK%XPSNG(:)
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       1.     FLUX CALCULATIONS
!               -----------------
!
DO JJ=1,SIZE(PEK%XTG,1)
!                                            temperature change
  ZDT(JJ) = PEK%XTG(JJ,1) - PTSM(JJ)
!
!                                            net radiation
!
  PRN(JJ) = (1. - PALBT(JJ)) * PSW_RAD(JJ) + PEMIST(JJ) *      &
           (PLW_RAD(JJ) - XSTEFAN * (PTSM(JJ)** 3)*(4.*PEK%XTG(JJ,1) - 3.*PTSM(JJ)))
!
!                                            sensible heat flux
!
  PH(JJ) = PRHOA(JJ) * PK%XCPS(JJ) * (PEK%XTG(JJ,1) - PTA(JJ)*PEXNS(JJ)/PEXNA(JJ)) &
           / PEK%XRESA(JJ) / PEXNS(JJ)
!
  ZWORK1(JJ) = PRHOA(JJ) * (1.-PEK%XVEG(JJ))*(1.-ZPSNG(JJ)) / PEK%XRESA(JJ)
  ZWORK2(JJ) = PQSAT(JJ)+PDQSAT(JJ)*ZDT(JJ) 
!                                            latent heat of sublimation from
!                                            the ground
!
  PLEGI(JJ) = ZWORK1(JJ) * PK%XLSTT(JJ) * ( PHUI(JJ) * ZWORK2(JJ) - PQA(JJ)) * PFROZEN1(JJ) * PLEGI_DELTA(JJ)
!
!                                            total latent heat of evaporation from
!                                            the ground
!
  PLEG(JJ) = ZWORK1(JJ) * PK%XLVTT(JJ) * ( PHUG(JJ) * ZWORK2(JJ) - PQA(JJ)) * (1.-PFROZEN1(JJ)) * PLEG_DELTA(JJ)
!
  ZWORK2(JJ) = PRHOA(JJ) * (ZWORK2(JJ) - PQA(JJ))
  ZWORK3(JJ) = ZWORK2(JJ) / PEK%XRESA(JJ)
!                                            latent heat of evaporation from 
!                                            the snow canopy
!
  PLES(JJ)     = PK%XLSTT(JJ) * ZPSN(JJ) * ZWORK3(JJ)
!
!                                            latent heat of evaporation from
!                                            evaporation
!
  PLEV(JJ)     = PK%XLVTT(JJ) * PEK%XVEG(JJ)*(1.-ZPSNV(JJ)) * DMK%XHV(JJ) * ZWORK3(JJ)
!
!                                            latent heat of evapotranspiration
!                                            
  ZZHV (JJ) = MAX(0., SIGN(1.,PQSAT(JJ) - PQA(JJ)))
  PLETR(JJ) = ZZHV(JJ) * (1. - PDELTA(JJ)) * PK%XLVTT(JJ) * PEK%XVEG(JJ)*(1-ZPSNV(JJ))          &
              * ZWORK2(JJ) *( (1/(PEK%XRESA(JJ) + DMK%XRS(JJ))) - ((1.-PF5(JJ))/(PEK%XRESA(JJ) + XRS_MAX)) )
!               
!
  PLER(JJ)     = PLEV(JJ) - PLETR(JJ)
!
!                                            latent heat of free water (floodplains)
!
  PLE_FLOOD(JJ)  = PK%XLVTT(JJ) * (1.-KK%XFFROZEN(JJ)) * KK%XFF(JJ) * ZWORK3(JJ) 
!
  PLEI_FLOOD(JJ) = PK%XLSTT(JJ) * KK%XFFROZEN(JJ) * KK%XFF(JJ) * ZWORK3(JJ) 
!
!                                            total latent heat of evaporation
!                                            without flood
!
  PLE(JJ)      = PLEG(JJ) + PLEV(JJ) + PLES(JJ) + PLEGI(JJ)
!
!                                            heat flux into the ground
!                                            without flood
!
  PGFLUX(JJ)   = PRN(JJ) - PH(JJ) - PLE(JJ)
!
!                                            heat flux due to snow melt
!                                            (ISBA-ES/SNOW3L)
!
  PMELTADV(JJ) = PSNOW_THRUFAL(JJ)*XCL*(XTT - PEK%XTG(JJ,1))
!
!                                            restore heat flux in FR mode,
!                                            or surface to sub-surface heat
!                                            flux using the DIF mode.
!
!
  PEVAP(JJ)    = ((PLEV(JJ) + PLEG(JJ))/PK%XLVTT(JJ)) + ((PLEGI(JJ) + PLES(JJ))/PK%XLSTT(JJ))
!                                            total evaporative flux (kg/m2/s)
!                                            without flood
!
ENDDO
!
!-------------------------------------------------------------------------------
!
IF(PEK%TSNOW%SCHEME == 'D95')THEN
  DO JJ=1,SIZE(PEK%XTG,1)
    PLE    (JJ)  = PLE    (JJ) + PLE_FLOOD(JJ) + PLEI_FLOOD(JJ)
    PGFLUX (JJ)  = PGFLUX (JJ) - PLE_FLOOD(JJ) - PLEI_FLOOD(JJ)
    PEVAP  (JJ)  = PEVAP  (JJ) + PLE_FLOOD(JJ)/PK%XLVTT(JJ) + PLEI_FLOOD(JJ)/PK%XLSTT(JJ)
  ENDDO
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       3.     SNOWMELT LATENT HEATING EFFECTS ('DEF' option)
!               ----------------------------------------------
!
IF( (PEK%TSNOW%SCHEME == 'D95' .OR. PEK%TSNOW%SCHEME == 'EBA') .AND. IO%CISBA /= 'DIF' )THEN
!                                            temperature tn
!
    IF (PEK%TSNOW%SCHEME == 'D95') THEN
!           
      ZTN(:) = (1.-PEK%XVEG(:))*PEK%XTG(:,1) + PEK%XVEG(:)*PT2M(:)
!
!     Only diag
      DMK%XSNOWTEMP(:,1) = ZTN (:)
!
!
!                                            melting rate
!                                            there is melting only if T > T0 and
!                                            of course when SNOWSWE > 0.
!
      WHERE ( ZTN(:) > XTT .AND. PEK%TSNOW%WSNOW(:,1) > 0.0 )
        PMELT(:) = ZPSN(:)*(ZTN(:)-XTT) / (PCS(:)*XLMTT*MAX(XTAU_SMELT,PTSTEP))
      END WHERE
!
!                                            close the energy budget: cannot melt 
!                                            more than the futur available snow
!      
      ZNEXTSNOW(:) = PEK%TSNOW%WSNOW(:,1) + PTSTEP * (DMK%XSRSFC(:) - PLES(:) / PK%XLSTT(:))
!
      WHERE ( PMELT(:) > 0.0 )
!              
              PMELT(:)=MIN(PMELT(:),ZNEXTSNOW(:)/PTSTEP)      
              ZNEXTSNOW(:) = ZNEXTSNOW(:) - PTSTEP * PMELT
!              
!             removes very small fraction
              ZFRAC(:) = SNOW_FRAC_GROUND(ZNEXTSNOW(:))
              WHERE(ZFRAC(:)<1.0E-4)
                    PMELT(:)     = PMELT(:) + ZNEXTSNOW(:) / PTSTEP       
              ENDWHERE   
!       
      ENDWHERE   
!    
    ELSEIF (PEK%TSNOW%SCHEME == 'EBA') THEN
!    
      PMELT(:)=MIN( PEK%TSNOW%WSNOW(:,1)/PTSTEP + DMK%XSRSFC(:) - PLES(:)/ PK%XLSTT(:) , &
                  MAX(0.0,(PEK%XTG(:,1)-XTT))  / MAX(ZEPS1,DMK%XCT*PTSTEP) / XLMTT )
!
    ENDIF
!
!                                            new temperature Ts(t) after melting
!                                            (cooling due to the melting of the
!                                            snow)
!
  PEK%XTG(:,1) = PEK%XTG(:,1) - DMK%XCT(:)*XLMTT*PMELT(:)*PTSTEP
!
ENDIF
!
!
IF (LHOOK) CALL DR_HOOK('ISBA_FLUXES',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
END SUBROUTINE ISBA_FLUXES











