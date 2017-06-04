!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE HYDRO_SOIL(IO, KK, PK, PEK, DMK, PTSTEP,  &
                            PLETR, PLEG, PPG, PEVAPCOR, PD_G3, &
                            PWSAT, PWFC, PDWGI1, PDWGI2, PLEGI,&
                            PWG3, PRUNOFF, PDRAIN, PWWILT )  
!     #####################################################################
!
!!****  *HYDRO_SOIL*  
!!
!!    PURPOSE
!!    -------
!
!     Calculates the evolution of the water variables, i.e., the superficial
!     and deep-soil volumetric water content (wg and w2), the equivalent
!     liquid water retained in the vegetation canopy (Wr), the equivalent
!     water of the snow canopy (Ws), and also of the albedo and density of
!     the snow (i.e., ALBS and RHOS).  Also determine the runoff and drainage
!     into the soil.
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
!!      
!!    AUTHOR
!!    ------
!!
!!      S. Belair           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original    14/03/95 
!!                  31/08/98 (V. Masson and F. Habets) add Dumenil et Todini
!!                           runoff scheme
!!                  31/08/98 (V. Masson and A. Boone) add the third soil-water
!!                           reservoir (WG3,D3)
!!                  15/03/99 A. Boone soil ice modification: advance
!!                           soil ice and liquid water, do budget checks
!!                  25/01/00 A. Boone : fully implicit method for WG2, WG3
!!                  05/03/07 A. Boone : changed drainage diagnostic computation for
!!                                      single bulk-soil option...i.e. for
!!                                      cases when IO%CISBA=2-L or d2>=d3 (IO%CISBA=3-L)
!!                                      for tighter water budget closure 
!!                  07/08/12 B. Decharme : Soil ice energy conservation
!!                     04/13 B. Decharme : Apply physical limits on wg here instead of in hydro.F90
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_K_t, ISBA_P_t, ISBA_PE_t
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t
!
USE MODD_CSTS,     ONLY : XLVTT, XRHOLW, XLMTT, XLSTT, XDAY
USE MODD_ISBA_PAR, ONLY : XWGMIN
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
REAL, INTENT(IN)                  :: PTSTEP
!                                      timestep of the integration (s)
!
REAL, DIMENSION(:), INTENT(IN)    :: PLETR, PLEG, PPG, PEVAPCOR
!                                      PLETR    = evapotranspiration of the vegetation (W m-2)
!                                      PLEG     = latent heat of evaporation over the ground (W m-2)
!                                      PPG      = total water reaching the ground (kg m-2 s-1)
!                                      PEVAPCOR = correction for any excess evaporation 
!                                                from snow as it completely ablates (kg m-2 s-1)
!
REAL, DIMENSION(:), INTENT(IN)    :: PD_G3, PWSAT, PWFC
!                                      PD_G3 = depth of the soil column (m)
!                                      PWSAT = saturation volumetric water content
!                                              of the soil (m3 m-3)
!                                      PWFC  = field capacity volumetric water
!                                              content (m3 m-3)
!
REAL, DIMENSION(:), INTENT(IN)    :: PDWGI1, PDWGI2, PLEGI
!                                      PDWGI1 = surface layer liquid water equivalent 
!                                               volumetric ice content time tendency (m3 m-3)
!                                      PDWGI2 = deep-soil layer liquid water equivalent  
!                                               volumetric ice content time tendency  (m3 m-3)
!                                      PLEGI  = surface soil ice sublimation (W m-2)
!
REAL, DIMENSION(:), INTENT(INOUT) :: PWG3
!                                      PWG3   = bulk deep-soil moisture at 't+dt' (m3 m-3)
!
REAL, DIMENSION(:), INTENT(OUT)   :: PRUNOFF, PDRAIN
!                                      PRUNOFF = runoff (kg m-2 s-1)
!                                      PDRAIN  = drainage (kg m-2 s-1)
!
REAL, DIMENSION(:), INTENT(IN)    :: PWWILT
!                                    PWWILT = wilting point volumetric water
!                                             content (m3 m-3)
!*      0.2    declarations of local variables
!
!
REAL, DIMENSION(SIZE(PEK%XTG,1))   :: ZWGI1M, ZWG2M, ZWG3M, ZWGI2M
!                                      Prognostic variables of ISBA at 't-dt'
!                                      ZWG2M = root-soil volumetric water content
!                                      ZWG3M = deep-soil volumetric water content
!                                      ZWGI1M = surface-soil volumetric ice content
!                                      ZWGI2M = deep-soil volumetric ice content
!
REAL, DIMENSION(SIZE(PEK%XTG,1))   :: ZETR, ZEG
!                                             ZETR = evapotranspiration rate
!                                             ZEG = evaporation rate from the ground
!
! 
REAL, DIMENSION(SIZE(PEK%XTG,1))   :: ZWSAT, ZWFC
!                                             ZWSAT = Wsat  when ice is present
!                                             ZWFC  = Wfc   when ice is present
!
REAL, DIMENSION(SIZE(PWG3))  :: ZC4
REAL, DIMENSION(SIZE(PWG3))  :: ZWAVG, ZSINK2,                                    &
                                  ZFACTOR, ZDRAINCF2, ZDRAINCF3, ZDRAIN2,           &
                                  ZDELTA2, ZDELTA3, ZDELTA22, ZDELTA33,             &
                                  ZWDRAIN2, ZWDRAIN3  
!
REAL, DIMENSION(SIZE(PWG3))  :: ZEXCESSF, ZA2, ZB2, ZC2, ZA3, ZB3, ZC3, ZWDRAIN, ZEXCESSFC
!
REAL, DIMENSION(SIZE(PWG3))  :: ZWLIM2, ZWLIM3, ZWWILT
!
INTEGER                 :: JJ
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*       0.     Initialization
!               --------------
!
!
IF (LHOOK) CALL DR_HOOK('HYDRO_SOIL',0,ZHOOK_HANDLE)
ZWSAT(:)     = 0.
ZWFC(:)      = 0.
ZWWILT(:)    = 0.
!
PDRAIN (:)   = 0.
PRUNOFF(:)   = 0.
!
ZDRAIN2(:)   = 0.
ZDRAINCF2(:) = 0.
ZDRAINCF3(:) = 0.
ZDELTA2(:)   = 0.0
ZDELTA3(:)   = 0.0
ZDELTA22(:)  = 0.0
ZDELTA33(:)  = 0.0
ZSINK2(:)    = 0.
ZWDRAIN(:)   = 0.0
ZWDRAIN2(:)  = 0.0
ZWDRAIN3(:)  = 0.0
ZA2(:)       = 0.0
ZB2(:)       = 0.0
ZC2(:)       = 0.0
ZA3(:)       = 0.0
ZB3(:)       = 0.0
ZC3(:)       = 0.0
!
ZEXCESSF(:)  = 0.0
!
! Fields at time t-dt
!
ZWG2M(:)     = PEK%XWG(:,2)
ZWG3M(:)     = PWG3(:)
ZWGI1M(:)    = PEK%XWGI(:,1)
ZWGI2M(:)    = PEK%XWGI(:,2)

!-------------------------------------------------------------------------------
!
DO JJ=1,SIZE(PEK%XTG,1)
!
!*       1.     New Wsat
!               --------
!
  ZWSAT (JJ) = PWSAT(JJ) - ZWGI2M(JJ)      ! per definition ZWGI2M<PWSAT-0.001
!                                             ! then ZWSAT>0.001
!
  ZWFC(JJ)   = PWFC(JJ)   * ZWSAT(JJ) / PWSAT(JJ)
!
  ZWWILT(JJ) = PWWILT(JJ) * ZWSAT(JJ) / PWSAT(JJ)
!
!
!                                           evaporation rates
!
  ZETR(JJ) = PLETR(JJ) / XLVTT
!
! Remove sublimation from total soil evaporation: this is done
! because ZEG is a liquid water sink term. Sublimation
! is removed from the surface ice tendency equation (below):
! PLEG represents the total of sublimation and evaporation.
! Also, add an additional possible correction which is used if
! evaporation from the snow surface exceeds the last amount of
! snow on the surface as the snow completely ablates (this
! is a very small term and is generally negligible HOWEVER
! it is retained here for a high order water budget):
!
  ZEG(JJ)  = PLEG(JJ) / XLVTT + PEVAPCOR(JJ)
!
!-------------------------------------------------------------------------------
!
!*       4.     EVOLUTION OF THE SUPERFICIAL WATER CONTENT WG1
!               ----------------------------------------------
!
!                                           updated values for wg 
!
  PEK%XWG(JJ,1) = (PEK%XWG(JJ,1) - PTSTEP * &
            (DMK%XC1(JJ)*(ZEG(JJ)-PPG(JJ))/XRHOLW - DMK%XC2(JJ)*DMK%XWGEQ(JJ)/XDAY)) &
              / (1. + PTSTEP * DMK%XC2(JJ) / XDAY)  
!
!
ENDDO
!
IF(IO%CKSAT=='SGH' .OR. IO%CKSAT=='EXP') THEN
  ZWLIM2(:)=ZWWILT(:)
  ZWLIM3(:)=PWWILT(:)
ELSE
  ZWLIM2(:)=XWGMIN
  ZWLIM3(:)=XWGMIN
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       5.     EVOLUTION OF THE DEEP WATER CONTENT WG2 and WG3
!               -----------------------------------------------
!
!*       5.1    2-L ISBA version
!               ----------------
!
IF (IO%CISBA=='2-L') THEN
! 
  DO JJ=1,SIZE(PEK%XTG,1)
!
    PEK%XWG(JJ,2) = ZWG2M(JJ) - PTSTEP*(ZEG(JJ) + ZETR(JJ) - PPG(JJ)) / (PK%XDG(JJ,2) * XRHOLW)  
!
!*       6.     DRAINAGE FROM THE DEEP SOIL
!               ------------------
!
    ZWDRAIN(JJ)   = KK%XWDRAIN(JJ) * &
            MAX(0.0, MIN(ZWFC(JJ),PEK%XWG(JJ,2))-ZWLIM2(JJ))/(ZWFC(JJ)-ZWLIM2(JJ))

    ZDRAIN2(JJ)   =  MAX( MIN(ZWDRAIN(JJ),PEK%XWG(JJ,2)) , &
               PEK%XWG(JJ,2)-ZWFC(JJ) )*PK%XC3(JJ,1) / (PK%XDG(JJ,2)*XDAY) * PTSTEP  
!
!                                      the deep-soil volumetric water content w2
!                                      is modified consequently
!
    PEK%XWG(JJ,2)    = PEK%XWG(JJ,2) - ZDRAIN2(JJ)

    PDRAIN(JJ)  = ZDRAIN2(JJ)*PK%XDG(JJ,2)*XRHOLW/PTSTEP  ! Final output units: kg m-2 s-1
!
  ENDDO
!
ELSE
!
  DO JJ=1,SIZE(PEK%XTG,1)

!*       5.2    3-L ISBA version (with only 2 active layers)
!               ----------------
!
    IF (PK%XDG(JJ,2) >= PD_G3(JJ)) THEN

      PEK%XWG(JJ,2) = ZWG2M(JJ) - PTSTEP*(ZEG(JJ) + ZETR(JJ) - PPG(JJ))   &
                    / (PK%XDG(JJ,2) * XRHOLW)  

!*       6.     DRAINAGE FROM THE DEEP SOIL
!               ------------------
!                                      when w2 > wfc, there is drainage
!
      ZWDRAIN(JJ) = KK%XWDRAIN(JJ) * &
              MAX(0.0, MIN(ZWFC(JJ),PEK%XWG(JJ,2))-ZWLIM2(JJ))/(ZWFC(JJ)-ZWLIM2(JJ))

      ZDRAIN2(JJ) = MAX( MIN(ZWDRAIN(JJ),PEK%XWG(JJ,2)) , &
              PEK%XWG(JJ,2)-ZWFC(JJ) )*PK%XC3(JJ,1) / (PK%XDG(JJ,2)*XDAY) * PTSTEP  

      PEK%XWG(JJ,2)    = PEK%XWG(JJ,2) -  ZDRAIN2(JJ)
      PWG3(JJ)    = PEK%XWG(JJ,2)

      PDRAIN(JJ)  = ZDRAIN2(JJ)*PK%XDG(JJ,2)*XRHOLW/PTSTEP  ! Final output units: kg m-2 s-1
!
    ELSE
!
!*       5.3    3-L ISBA version (with 3 active layers)
!               ----------------
!
! Linear drainage term check (m3 m-3):
! Use a linear scaling to prevent it from completely drying out the soil. This
! term will, under most conditions, not result in such a drying. Until a more
! rhobust (but more complicated) method is developed for maintaining a minimum river
! flow under dry conditions, this method will be used.
!
      ZWDRAIN2(JJ) = KK%XWDRAIN(JJ)* MAX(0.0, MIN(ZWFC(JJ),ZWG2M(JJ))-ZWLIM2(JJ))/(ZWFC(JJ)-ZWLIM2(JJ))
      ZWDRAIN3(JJ) = KK%XWDRAIN(JJ)* MAX(0.0, MIN(PWFC(JJ),ZWG3M(JJ))-ZWLIM3(JJ))/(PWFC(JJ)-ZWLIM3(JJ))
!
! Delta functions:
!
      ZDELTA2(JJ)    = 0.0
      IF ( ZWG2M(JJ) - ZWFC(JJ) > ZWDRAIN2(JJ) ) ZDELTA2(JJ) = 1.0
  !
      ZDELTA3(JJ)    = 0.0
      IF ( ZWG3M(JJ) - PWFC(JJ) > ZWDRAIN3(JJ) ) ZDELTA3(JJ) = 1.0
  !
  !

! evaluate inter-facial water content, grid factor, and diffusion coefficient:

      ZWAVG(JJ)     = ( ( (ZWG2M(JJ)**6)* PK%XDG(JJ,2)          +                       &
                           (ZWG3M(JJ)**6)*(PD_G3(JJ)-PK%XDG(JJ,2)) )/PD_G3(JJ) )**(1./6.)  

      ZFACTOR(JJ)   = PK%XDG(JJ,2)/(PD_G3(JJ)-PK%XDG(JJ,2))

      ZC4    (JJ)   = PK%XC4REF(JJ)*(ZWAVG(JJ)**KK%XC4B(JJ))                &
                     *(10.**(-KK%XC4B(JJ)*PEK%XWGI(JJ,2)/(PWSAT(JJ)-XWGMIN)))  
!
! calculate sources/sinks
!
      ZSINK2 (JJ)   = -(ZEG(JJ) + ZETR(JJ) - PPG(JJ) )/(PK%XDG(JJ,2)*XRHOLW)

! Compute evolution of water content using linearized equations
! (see Boone 2000, Appendix F.2 for details)
!
! sink terms are treated explicitly, other terms are implicit
!
      ZDRAINCF2(JJ) = PK%XC3(JJ,1) / (PK%XDG(JJ,2) * XDAY)
      ZDELTA22(JJ)  = ZDELTA2(JJ)*ZWFC(JJ) - (1.0-ZDELTA2(JJ))*ZWDRAIN2(JJ)
      ZC2(JJ)       = 1.0 + PTSTEP*(ZDELTA2(JJ)*ZDRAINCF2(JJ) + (ZC4(JJ)/XDAY)) 
      ZB2(JJ)       = PTSTEP*ZC4(JJ)/(XDAY*ZC2(JJ))
      ZA2(JJ)       = ( ZWG2M(JJ) + PTSTEP*(ZSINK2(JJ) + ZDRAINCF2(JJ)*ZDELTA22(JJ)) )/ZC2(JJ)
!
      ZDRAINCF3(JJ) = PK%XC3(JJ,2) / ( (PD_G3(JJ)-PK%XDG(JJ,2)) * XDAY)
      ZDELTA33(JJ)  = ZDELTA3(JJ)*PWFC(JJ) - (1.0-ZDELTA3(JJ))*ZWDRAIN3(JJ)
      ZC3(JJ)       = 1.0 + PTSTEP*(ZDELTA3(JJ)*ZDRAINCF3(JJ) + ZFACTOR(JJ)*(ZC4(JJ)/XDAY)) 
      ZB3(JJ)       = PTSTEP*ZFACTOR(JJ)*(ZDELTA2(JJ)*ZDRAINCF2(JJ) + (ZC4(JJ)/XDAY) )/ZC3(JJ)
      ZA3(JJ)       = ( ZWG3M(JJ) + PTSTEP*(                                                 &
                         - ZFACTOR(JJ)*ZDRAINCF2(JJ)*ZDELTA22(JJ)                            &
                          +            ZDRAINCF3(JJ)*ZDELTA33(JJ)) )/ZC3(JJ)  
!
! Advance volumetric water content values in time:
! system of 2 linear equations:
!
      PEK%XWG(JJ,2) = ( ZA2(JJ)+ZB2(JJ)*ZA3(JJ) )/(1.0 - ZB2(JJ)*ZB3(JJ))
      PWG3(JJ)       = ZA3(JJ) + ZB3(JJ)*PEK%XWG(JJ,2)
!
! Drainage (kg m-2 s-1): this term is implicit and is extracted directly from
!                        the drainage computation in the above equations.
!
      ZWDRAIN(JJ)   = (XRHOLW*PK%XC3(JJ,2)/XDAY)*                                                 &
                       ( ZDELTA3(JJ)*(PWG3(JJ)-PWFC(JJ)) + (1.0-ZDELTA3(JJ))*ZWDRAIN3(JJ) )  
!
! As drainage is implicit, perform a check to prevent any negative drainage
! (can arise rarely and is generally negligible, but to ensure a high order conservation):
!
      PDRAIN(JJ)    = MAX(0.0, ZWDRAIN(JJ))
      PWG3(JJ)      = PWG3(JJ) + (PDRAIN(JJ) - ZWDRAIN(JJ))*PTSTEP/((PD_G3(JJ)-PK%XDG(JJ,2))*XRHOLW)
!
    ENDIF
  ENDDO
END IF
!
!-------------------------------------------------------------------------------
!
DO JJ=1,SIZE(PEK%XTG,1)
!*       7.     EFFECT OF THE MELTING/FREEZING ON THE SOIL WATER CONTENT
!               --------------------------------------------------------
!
!*       7.1    Effect on surface water liquid and ice reservoirs
!               -------------------------------------------------
!
! First, advance the surface layer ice content using the time tendency
! and sublimation (*heat effect* of sublimation already accounted
! for in latent heat flux calculation):
!           
  PEK%XWGI(JJ,1) = ZWGI1M(JJ) + PDWGI1(JJ) - PLEGI(JJ)*PTSTEP/(XLSTT*PK%XDG(JJ,1)*XRHOLW)
!
! Next, update the liquid water content:
!
  PEK%XWG(JJ,1)  = PEK%XWG(JJ,1)  - PDWGI1(JJ) 
!
! Make sure that ice has not dropped below
! zero due to sublimation (the ONLY way
! it can drop below 0). If it has, then
! freeze the needed liquid water (thus warming the
! layer) to ensure sublimation is accomodated,
! then extract this from liquid water store.
! This is a correction used ONLY when last traces
! of soil ice sublimate within a model time step.
!
  ZEXCESSFC(JJ)= 0.0
!
  ZEXCESSF(JJ) = MAX(0.0, - PEK%XWGI(JJ,1))
  PEK%XWG(JJ,1)     = PEK%XWG(JJ,1)  - ZEXCESSF(JJ)
  PEK%XWGI(JJ,1)    = PEK%XWGI(JJ,1) + ZEXCESSF(JJ)
  ZEXCESSFC(JJ)= ZEXCESSFC(JJ) - ZEXCESSF(JJ)
!
! Modif H.Douville 26/08/03 (global scale)
! Make sure that ice has not raised above 
! saturation (minus XWGMIN) due to sublimation.
! If it has, then melt the needed frozen water 
! (thus cooling the layer) to ensure sublimation
! is accomodated, then extract this from frozen water store.
!
  ZEXCESSF(JJ) = MIN(0.0, PWSAT(JJ) - XWGMIN - PEK%XWGI(JJ,1))
  PEK%XWG(JJ,1)     = PEK%XWG(JJ,1)  - ZEXCESSF(JJ)
  PEK%XWGI(JJ,1)    = PEK%XWGI(JJ,1) + ZEXCESSF(JJ)
  ZEXCESSFC(JJ)= ZEXCESSFC(JJ) - ZEXCESSF(JJ)
!
! Make sure that liquid has not dropped below
! minimum threshold: this could arise due to
! evaporation and ice formation. Melt the needed water.
! Normally simply a budget check, i.e. usually small but accounted
! for none-the-less to assure high accuracy.
!
  ZEXCESSF(JJ) = MAX(0.0, XWGMIN - PEK%XWG(JJ,1))
  PEK%XWGI(JJ,1)    = PEK%XWGI(JJ,1)  - ZEXCESSF(JJ)
  PEK%XWG(JJ,1)     = PEK%XWG(JJ,1)   + ZEXCESSF(JJ)
  ZEXCESSFC(JJ)= ZEXCESSFC(JJ) + ZEXCESSF(JJ)
!
! removes very small values due to computation precision
!
  IF(PEK%XWGI(JJ,1) < 1.0E-10) THEN
    ZEXCESSF(JJ)    = PEK%XWGI(JJ,1)
    PEK%XWG(JJ,1)  = PEK%XWG(JJ,1) + ZEXCESSF(JJ)
    PEK%XWGI(JJ,1) = 0.0
    ZEXCESSFC(JJ)   = ZEXCESSFC(JJ) + ZEXCESSF(JJ)
  ENDIF
!
! Cummulative phase change for the ice/liquid budget corrections:
!
  PEK%XTG(JJ,1) = PEK%XTG(JJ,1) - ZEXCESSFC(JJ)*XLMTT*DMK%XCT(JJ)*XRHOLW*PK%XDG(JJ,1)
!
!
!*       7.2    Effect on deep-soil liquid and ice reservoirs
!               ---------------------------------------------
!
! Update the ice content using the ice tendency:
! Since this reservoir includes surface reservoir, add
! any changes in ice content due to sublimation:
!
  PEK%XWGI(JJ,2) = ZWGI2M(JJ) + PDWGI2(JJ) - PLEGI(JJ)*PTSTEP/(XLSTT*PK%XDG(JJ,2)*XRHOLW)
!
! Update the liquid water content:
!
  PEK%XWG(JJ,2)   = PEK%XWG(JJ,2)   - PDWGI2(JJ)
!
! Make sure that ice has not dropped below
! zero due to sublimation (as above).
!
  ZEXCESSFC(JJ)= 0.0
!
  ZEXCESSF(JJ) = MAX(0.0, -PEK%XWGI(JJ,2))
  PEK%XWG(JJ,2)     = PEK%XWG(JJ,2)  - ZEXCESSF(JJ)
  PEK%XWGI(JJ,2)    = PEK%XWGI(JJ,2) + ZEXCESSF(JJ)
  ZEXCESSFC(JJ)= ZEXCESSFC(JJ) - ZEXCESSF(JJ)
!
! Budget check of minimum threshold for liquid
! water as for surface: MUCH LESS likely
! to be utilized, but retained for accuracy
! in energy and water balance (as above).
!
  ZEXCESSF(JJ) = MAX(0.0, XWGMIN - PEK%XWG(JJ,2))
  PEK%XWGI(JJ,2)    = PEK%XWGI(JJ,2)  - ZEXCESSF(JJ)
  PEK%XWG(JJ,2)     = PEK%XWG(JJ,2)   + ZEXCESSF(JJ)
  ZEXCESSFC(JJ)= ZEXCESSFC(JJ) + ZEXCESSF(JJ)
!
! removes very small values due to computation precision
!
  IF (PEK%XWGI(JJ,2) < 1.0E-10 * PTSTEP) THEN
      ZEXCESSF(JJ) = PEK%XWGI(JJ,2)
      PEK%XWG(JJ,2)    = PEK%XWG(JJ,2) + ZEXCESSF(JJ)
      PEK%XWGI(JJ,2)    = 0.
      ZEXCESSFC(JJ)= ZEXCESSFC(JJ) + ZEXCESSF(JJ)
  ENDIF
!
! Cummulative phase change for the ice/liquid budget corrections:
!
  PEK%XTG(JJ,2) = PEK%XTG(JJ,2) - ZEXCESSFC(JJ)*XLMTT*DMK%XCG(JJ)*XRHOLW*PK%XDG(JJ,2)
!
!
ENDDO
!
!-------------------------------------------------------------------------------
!
!*       8.     PHYSICAL LIMITS AND RUNOFF
!               --------------------------
!
! runoff of second layer
!
PRUNOFF(:) = MAX( 0., PEK%XWG(:,2)+PEK%XWGI(:,2)-PWSAT(:) )*PK%XDG(:,2) * XRHOLW / PTSTEP
!
! now apply limits:
!
PEK%XWG(:,1) = MIN( PEK%XWG(:,1), PWSAT(:) - PEK%XWGI(:,1) )
PEK%XWG(:,1) = MAX( PEK%XWG(:,1), XWGMIN              )
!
PEK%XWG(:,2) = MIN( PEK%XWG(:,2), PWSAT(:) - PEK%XWGI(:,2) )
PEK%XWG(:,2) = MAX( PEK%XWG(:,2), XWGMIN              )
!
!runoff of third layer added to drainage
!
IF (IO%CISBA=='3-L') THEN
   PDRAIN(:) = PDRAIN(:) + MAX( 0., PWG3(:)-PWSAT(:) )* (PD_G3(:)-PK%XDG(:,2)) * XRHOLW / PTSTEP  
   PWG3(:) = MIN( PWG3(:), PWSAT(:)         )
   PWG3(:) = MAX( PWG3(:), XWGMIN           )
END IF
!
IF (LHOOK) CALL DR_HOOK('HYDRO_SOIL',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE HYDRO_SOIL
