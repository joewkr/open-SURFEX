!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE E_BUDGET(IO, KK, PK, PEK, DK, DMK, HIMPLICIT_WIND,  &
                          PTSTEP, PUREF, PPEW_A_COEF, PPEW_B_COEF, PPET_A_COEF, &
                          PPEQ_A_COEF, PPET_B_COEF, PPEQ_B_COEF, PVMOD, PTSM, PT2M, &
                          PSW_RAD, PLW_RAD, PTA, PQA, PPS, PRHOA, PEXNS, PEXNA, &
                          PHUI, PLEG_DELTA, PLEGI_DELTA, PGRNDFLUX, PFLUX_COR, &
                          PSOILCONDZ, PSOILHCAPZ, PALBT, PEMIST, PQSAT, PDQSAT, &
                          PFROZEN1, PTDEEP_A,PTA_IC, PQA_IC, PUSTAR2_IC, PDEEP_FLUX, &
                          PRESTORE                                             )  
!     ##########################################################################
!
!!****  *E_BUDGET*  
!!
!!    PURPOSE
!!    -------
!
!     Calculates the evolution of the surface and deep-soil temperature
!     (i.e., Ts and T2), as well as all the surface fluxes.
!         
!     
!!**  METHOD
!!    ------
!
!     1- find the grid-averaged albedo, emissivity, and roughness length
!     2- compute the za, zb, and zc terms involved in the numerical
!        resolution of the equations for Ts and T2.
!     3- find Ts(t) and T2(t).
!     4- derive the surface fluxes.
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
!!    Belair (199:)
!!      
!!    AUTHOR
!!    ------
!!
!!      S. Belair           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    14/03/95 
!!      (J.Stein)   15/11/95 use the wind components in the flux computation
!!      (J.Noilhan) 15/03/96 use the potential temperature instead of the
!!                           temperature for the heat flux computation 
!!      (J.Stein)   27/03/96 use only H and LE in the soil scheme
!!      (A.Boone, V.Masson)  28/08/98 splits the routine in two for C02 computations
!!      (A.Boone)   15/03/99 Soil ice tendencies calculated here: heating/cooling
!!                           affects surface and deep soil temperatures.
!!      (A. Boone, V. Masson) 01/2003 Externalization
!!      (E. Martin)          07/05 implicit coupling (coeff ZA,ZB,ZC)
!!      (P. Le Moigne)       07/05 dependence on qs for cp
!!      (B. Decharme)        05/08 Add floodplains dependencies
!!      (B. Decharme)        01/09 optional deep soil temperature as in Arpege
!!      (R. Hamdi)           01/09 Cp and L are not constants (As in ALADIN)
!!      (B. Decharme)        09/09 When LCPL_ARP, do not calculate x2 each coef
!!      (A.Boone)            03/10 Add delta fnctions to force LEG ans LEGI=0
!!                                 when hug(i)Qsat < Qa and Qsat > Qa
!!      (B. Decharme)        09/12 new wind implicitation
!!      (V. Masson)          01/13 Deep soil flux implicitation
!!      (B. Decharme)        10/14 Bug in DIF composite budget
!!                                 Use harmonic mean to compute interfacial thermal conductivities
!!                                 "Restore" flux computed here
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_K_t, ISBA_P_t, ISBA_PE_t
USE MODD_DIAG_n, ONLY : DIAG_t
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t
!
USE MODD_CSTS,       ONLY : XLVTT, XLSTT, XSTEFAN, XCPD, XPI, XDAY, &
                              XTT, XCL, XCPV, XCI  
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_SNOW_PAR,   ONLY : XEMISSN, XEMCRIN
!
USE MODD_SURF_ATM,   ONLY : LCPL_ARP, LQVNPLUS
!
USE MODE_THERMOS
!
USE MODI_SOIL_HEATDIF 
USE MODI_SOIL_TEMP_ARP
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_K_t), INTENT(INOUT) :: KK
TYPE(ISBA_P_t), INTENT(INOUT) :: PK
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
TYPE(DIAG_t), INTENT(INOUT) :: DK
TYPE(DIAG_MISC_ISBA_t), INTENT(INOUT) :: DMK
!
 CHARACTER(LEN=*),     INTENT(IN)  :: HIMPLICIT_WIND   ! wind implicitation option
!                                                     ! 'OLD' = direct
!                                                     ! 'NEW' = Taylor serie, order 1
REAL, INTENT(IN)                 :: PTSTEP
!                                   timestep of the integration
REAL, DIMENSION(:), INTENT(IN)   :: PUREF       ! reference height of the wind
REAL, DIMENSION(:), INTENT (IN)  :: PSW_RAD, PLW_RAD, PPS, PRHOA, PTA, PQA, PVMOD
!                                     PSW_RAD = incoming solar radiation
!                                     PLW_RAD = atmospheric infrared radiation
!                                     PRHOA = near-ground air density
!                                     PPS = surface pressure
!                                     PTA = near-ground air temperature
!                                     PQA = near-ground air specific humidity
!                                     PVMOD = wind speed
!
! implicit atmospheric coupling coefficients:
!
REAL, DIMENSION(:), INTENT(IN)  :: PPEW_A_COEF, PPEW_B_COEF,                   &
                                   PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF,      &
                                   PPEQ_B_COEF  
!                                  PPEW_A_COEF = A-wind coefficient (m2s/kg)
!                                  PPEW_B_COEF = B-wind coefficient (m/s)
!                                  PPET_A_COEF = A-air temperature coefficient
!                                  PPET_B_COEF = B-air temperature coefficient
!                                  PPEQ_A_COEF = A-air specific humidity coefficient
!                                  PPEQ_B_COEF = B-air specific humidity coefficient
!
REAL, DIMENSION(:), INTENT(IN)   :: PEXNS, PEXNA
REAL, DIMENSION(:), INTENT(IN)   :: PHUI
!
REAL, DIMENSION(:), INTENT(IN)     :: PFROZEN1
!                                     PFROZEN1 = ice fraction in supurficial soil
REAL, DIMENSION(:), INTENT(IN)     :: PTDEEP_A
!                                      PTDEEP_A = Deep soil temperature
!                                                 coefficient depending on flux
REAL, DIMENSION(:), INTENT(IN)      :: PGRNDFLUX 
!                                      PGRNDFLUX = soil/snow interface flux (W/m2) using
!                                                  ISBA-SNOW3L option
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PFLUX_COR
!                                      PFLUX_COR = correction flux to conserve energy (W/m2)
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PSOILCONDZ, PSOILHCAPZ
!                                      PSOILCONDZ= ISBA-DF Soil conductivity profile  [W/(m K)]
!                                      PSOILHCAPZ=ISBA-DF Soil heat capacity profile [J/(m3 K)]
!
REAL, DIMENSION(:), INTENT(INOUT)   :: PLEG_DELTA, PLEGI_DELTA
!                                      PLEG_DELTA = soil evaporation delta fn
!                                      PLEGI_DELTA = soil evaporation delta fn
!
REAL, DIMENSION(:), INTENT (OUT)   :: PQA_IC, PTA_IC, PUSTAR2_IC
!                                     PTA_IC = near-ground air temperature
!                                     PQA_IC = near-ground air specific humidity
!                                     PUSTAR2_IC = near-ground wind friction (m2/s2)
!                                           (modified if implicit coupling with
!                                            atmosphere used)
!
REAL, DIMENSION(:), INTENT (IN)     :: PTSM, PT2M
!                                      PTSM   = surface temperature at start 
!                                               of time step (K)
!                                      PT2M   = mean surface (or restore) temperature at start 
!                                               of time step (K)
!
REAL, DIMENSION(:), INTENT(OUT)  :: PALBT, PEMIST, PDQSAT
!                                     PALBT  = averaged albedo
!                                     PEMIST = averaged emissivity
!                                     PDQSAT = saturation vapor humidity derivative
REAL, DIMENSION(:), INTENT(IN)   :: PQSAT
!                                     PQSAT  = saturation vapor humidity
!
REAL, DIMENSION(:), INTENT(OUT)    :: PDEEP_FLUX ! Heat flux at bottom of ISBA (W/m2)
!
REAL, DIMENSION(:), INTENT(OUT)    :: PRESTORE
!                                     PRESTORE = surface restore flux (W m-2)
!
!*      0.2    declarations of local variables
!
!
REAL, DIMENSION(SIZE(PEK%XSNOWFREE_ALB)) :: ZRORA,                        &
!                                             rhoa / ra
!
                                               ZA,ZB,ZC  
!                                             terms for the calculation of Ts(t)
!
! ISBA-DF:
!
REAL, DIMENSION(SIZE(PEK%XSNOWFREE_ALB)) ::  ZCONDAVG, ZCOND1, ZCOND2, ZTERM2, ZTERM1
!
! implicit atmospheric coupling coefficients: (modified-form)
!
REAL, DIMENSION(SIZE(PEK%XSNOWFREE_ALB)) :: ZPET_A_COEF, ZPEQ_A_COEF, ZPET_B_COEF,      &
                               ZPEQ_B_COEF, Z_CCOEF, ZHUMS, ZHUMA, ZLAVG,  &
                               ZHUMSD, ZHUMAD 
!                              ZPET_A_COEF = A-air temperature coefficient
!                              ZPET_B_COEF = B-air temperature coefficient
!                              ZPEQ_A_COEF = A-air specific humidity coefficient
!                              ZPEQ_B_COEF = B-air specific humidity coefficient
!                              Z_CCOEF     = C-working variable
!
REAL, DIMENSION(SIZE(PEK%XSNOWFREE_ALB)) :: ZUSTAR2, ZVMOD
!                              ZUSTAR2 = friction     (m2/s2)
!                              ZVMOD   = wind modulus (m/s)
REAL, DIMENSION(SIZE(PEK%XSNOWFREE_ALB)) :: ZXCPV_XCL_AVG
REAL, DIMENSION(SIZE(PEK%XSNOWFREE_ALB)) :: ZCNHUMA, ZPEQA2, ZDKQB, ZCDQSAT, ZINCR, ZTRAD, &
                                ZCHUMS, ZCHUMA, ZPETA2, ZPETB2,ZTEMP, ZFGNFRZ, &
                                ZFGFRZ, ZFV, ZFG, ZFNFRZ, ZFFRZ, ZFNSNOW, ZCPS,&
                                ZLVTT, ZLSTT
REAL                        :: ZSNOW
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*       0.     Initialization:
!               ---------------
!
IF (LHOOK) CALL DR_HOOK('E_BUDGET',0,ZHOOK_HANDLE)
!
ZCONDAVG(:)  = 0.0
ZTERM2(:)    = 0.0
ZTERM1(:)    = 0.0
ZHUMSD(:)    = 0.0
ZHUMAD(:)    = 0.0
!
!-------------------------------------------------------------------------------
!
!*       1.     COEFFICIENTS FOR THE TIME INTEGRATION OF  TS 
!               --------------------------------------------
!
!
!                                              function dqsat(Ts,ps)
!
PDQSAT(:) = DQSAT(PEK%XTG(:,1),PPS(:),PQSAT(:))
!                                              function zrsra
!
! Modify flux-form implicit coupling coefficients:
! - wind components:
!
ZTEMP  (:) = DK%XCD(:)*PVMOD(:)
!
IF(HIMPLICIT_WIND=='OLD')THEN
! old implicitation (m2/s2)
  ZUSTAR2(:) = ZTEMP(:) * PPEW_B_COEF(:) / (1.0- ZTEMP(:)*PRHOA(:)*PPEW_A_COEF(:)) 
ELSE
! new implicitation (m2/s2)
  ZUSTAR2(:) = ZTEMP(:) * (2.*PPEW_B_COEF(:)-PVMOD(:)) / (1.0-2.0*ZTEMP(:)*PRHOA(:)*PPEW_A_COEF(:)) 
ENDIF
!
!wind modulus at t+1 (m/s)
ZVMOD(:) = PRHOA(:)*PPEW_A_COEF(:)*ZUSTAR2(:) + PPEW_B_COEF(:)
ZVMOD(:) = MAX(ZVMOD(:),0.)
!
WHERE(PPEW_A_COEF(:)/= 0.)
      ZUSTAR2(:) = MAX( ( ZVMOD(:) - PPEW_B_COEF(:) ) / (PRHOA(:)*PPEW_A_COEF(:)), 0.)
ENDWHERE
!
ZUSTAR2(:) = MAX(ZUSTAR2(:),0.)
!
ZRORA(:)    = PRHOA(:) / PEK%XRESA(:)
!
!                                              terms za, zb, and zc for the
!                                              calculation of ts(t)
!
! Modify flux-form implicit coupling coefficients:
! - air temperature:
!
ZTEMP(:) = PPET_A_COEF(:)*ZRORA(:)
Z_CCOEF(:)     = (1.0 - ZTEMP(:))/PEXNA(:)
!
ZPET_A_COEF(:) = - ZTEMP(:)/PEXNS(:)/Z_CCOEF(:)
!
ZPET_B_COEF(:) = PPET_B_COEF(:)/Z_CCOEF(:) 
!
!-------------------------------------------------------------------------------
!
!*       2.     AIR AND SOIL SPECIFIC HUMIDITIES 
!               --------------------------------
!
! - air specific humidity:
!
ZFV(:) = PEK%XVEG(:)     *(1-PEK%XPSNV(:)-KK%XFFV(:))
ZFG(:) = (1.-PEK%XVEG(:))*(1.-PEK%XPSNG(:)-KK%XFFG(:))
ZFNFRZ(:) = (1.-KK%XFFROZEN(:))*KK%XFF(:) + ZFV + ZFG(:)*(1.-PFROZEN1(:))
ZFFRZ(:)  = KK%XFFROZEN(:)     *KK%XFF(:) +       ZFG(:)*PFROZEN1(:)      + PEK%XPSN(:)
!
ZSNOW      = 1.
ZFNSNOW(:) = 1.
ZCPS(:)    = PK%XCPS(:)
!
IF (LCPL_ARP) THEN

  ! currently this correction not applied for this option, but can be
  ! added later after testing...so delta fns set to 1 (turns OFF this correction)

  PLEG_DELTA(:)  = 1.0
  PLEGI_DELTA(:) = 1.0

  ZLAVG(:)        = PK%XLVTT(:)*ZFNFRZ(:) + PK%XLSTT(:)*ZFFRZ(:)
  ZXCPV_XCL_AVG(:)=   (XCPV-XCL)*ZFNFRZ(:) + (XCPV-XCI)  *ZFFRZ(:) 

  ZLVTT(:) = ZLAVG(:)
  ZLSTT(:) = ZLAVG(:)

ELSE

  IF(PEK%TSNOW%SCHEME == '3-L' .OR. PEK%TSNOW%SCHEME == 'CRO' .OR. IO%CISBA == 'DIF')THEN
    ZSNOW = 0.
    ZFNSNOW(:) = 1. - PEK%XPSN(:)
    ZCPS(:)=XCPD
  ENDIF

  ZLAVG(:)     = XLVTT*ZFNFRZ(:) + XLSTT*ZFFRZ(:)

  ZLVTT(:) = XLVTT
  ZLSTT(:) = XLSTT

ENDIF
!
ZFGNFRZ(:) = ZFG(:) * (1.-PFROZEN1(:)) * PLEG_DELTA(:)
ZFGFRZ (:) = ZFG(:) * PFROZEN1(:)      * PLEGI_DELTA(:)
!
ZHUMA(:) = ZLVTT(:)/ZLAVG(:) * ((1.-KK%XFFROZEN(:))*KK%XFF(:) + ZFV(:)*DMK%XHV(:) + ZFGNFRZ(:))   +  &
           ZLSTT(:)/ZLAVG(:) * (KK%XFFROZEN(:)     *KK%XFF(:) + ZFGFRZ(:) + ZSNOW*PEK%XPSN(:) ) 
!
ZHUMS(:) = ZLVTT(:)/ZLAVG(:) * ((1.-KK%XFFROZEN(:))*KK%XFF(:) + ZFV(:)*DMK%XHV(:) + ZFGNFRZ(:)*DK%XHUG(:)) +   &
           ZLSTT(:)/ZLAVG(:) * (KK%XFFROZEN(:)     *KK%XFF(:) + ZFGFRZ(:)*PHUI(:)  + ZSNOW*PEK%XPSN(:) )  
!
IF(PEK%TSNOW%SCHEME == '3-L' .OR. PEK%TSNOW%SCHEME == 'CRO' .OR. IO%CISBA == 'DIF')THEN
!
! humidity considering no snow (done elsewhere) and flooded zones:
!
  ZHUMAD(:) = KK%XFF(:) + ZFV(:)*DMK%XHV(:) + ZFGNFRZ(:) + ZFGFRZ(:)  
  ZHUMSD(:) = KK%XFF(:) + ZFV(:)*DMK%XHV(:) + ZFGNFRZ(:)*DK%XHUG(:) + ZFGFRZ(:)*PHUI(:)          
ELSE
  ZHUMAD(:) = ZHUMA(:)
  ZHUMSD(:) = ZHUMS(:)
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       3.     COEFFICIENTS FOR THE TIME INTEGRATION OF Q 
!               -------------------------------------------
!
! implicit q coefficients:
!
ZTEMP(:) = PPEQ_A_COEF(:)*ZRORA(:)
Z_CCOEF(:)     = 1.0 - ZTEMP(:)*ZHUMAD(:)
!
ZPEQ_A_COEF(:) = - ZTEMP(:)*PDQSAT(:)*ZHUMSD(:)/Z_CCOEF(:)
!
ZPEQ_B_COEF(:) = ( PPEQ_B_COEF(:) - ZTEMP(:)*ZHUMSD(:)* (PQSAT(:) - PDQSAT(:)*PEK%XTG(:,1)) )/Z_CCOEF(:)  
!
!-------------------------------------------------------------------------------
!
!*       4.     TOTAL ALBEDO AND EMISSIVITY 
!               ---------------------------
!
!
IF(PEK%TSNOW%SCHEME == '3-L' .OR. PEK%TSNOW%SCHEME == 'CRO' .OR. IO%CISBA == 'DIF')THEN
!
! NON-SNOW covered Grid averaged albedo and emissivity for explicit
! snow scheme 
!
  IF(.NOT.IO%LFLOOD)THEN
!          
     PALBT (:) = PEK%XSNOWFREE_ALB(:)
     PEMIST(:) = PEK%XEMIS(:)
!     
  ELSE
!
! Taking into account the floodplains with snow grid fractions :
!     PFF    1.-PFF-PEK%XPSN(:)   PEK%XPSN(:)
! |------------|----|---------------|
!
  WHERE(PEK%XPSN(:)<1.0)          
     PALBT (:) = ((1.-KK%XFF(:)-PEK%XPSN(:))*PEK%XSNOWFREE_ALB(:) + KK%XFF(:)*KK%XALBF(:) )/(1.-PEK%XPSN(:))
     PEMIST(:) = ((1.-KK%XFF(:)-PEK%XPSN(:))*PEK%XEMIS(:)         + KK%XFF(:)*KK%XEMISF(:))/(1.-PEK%XPSN(:))
  ELSEWHERE
     PALBT (:) = PEK%XSNOWFREE_ALB(:)
     PEMIST(:) = PEK%XEMIS(:)
  ENDWHERE
!
  ENDIF
!
!
ELSE
!
! Grid averaged albedo and emissivity for composite snow scheme:
!
   IF(PEK%TSNOW%SCHEME=='EBA') THEN
!
      PALBT(:)  = (1-PEK%XVEG(:)) * (PEK%XSNOWFREE_ALB_SOIL(:)*(1-PEK%XPSNG(:)) + &
                                                 PEK%TSNOW%ALB(:)*PEK%XPSNG(:)) + &
                     PEK%XVEG(:)  * (PEK%XSNOWFREE_ALB_VEG (:)*(1-PEK%XPSNV_A(:)) +      &
                                                PEK%TSNOW%ALB(:)*PEK%XPSNV_A(:))  
!
      PEMIST(:) = PEK%XEMIS(:)-PEK%XPSN(:)*(PEK%XEMIS(:)-XEMCRIN)
!      
   ELSE
!
      PALBT (:) = ( 1.-PEK%XPSN(:)-KK%XFF(:))* PEK%XSNOWFREE_ALB(:) + &
                       PEK%XPSN(:)           * PEK%TSNOW%ALB(:)     + KK%XFF(:)*KK%XALBF(:)     
!
      PEMIST(:) = ( 1.-PEK%XPSN(:)-KK%XFF(:))* PEK%XEMIS(:) + &
                       PEK%XPSN(:)           * XEMISSN      + KK%XFF(:)*KK%XEMISF(:)
!
   ENDIF

ENDIF
!
!-------------------------------------------------------------------------------
!
!*       5. CALCULATION OF ZA, ZB, ZC
!       -----------------------------
!
!       5.1. Default
!       ------------
!
ZTRAD(:) =  PEMIST(:) * XSTEFAN * (PEK%XTG(:,1)**3)
ZCHUMS(:) = ZRORA(:)*ZLAVG(:)*ZHUMS(:)
ZCHUMA(:) = ZRORA(:)*ZLAVG(:)*ZHUMA(:)
!
ZPETA2(:) = 1./PEXNS(:) - ZPET_A_COEF(:)/PEXNA(:)
ZPETB2(:) = ZPET_B_COEF(:)/PEXNA(:)
!
! Surface Energy Budget linearization coefficients for an explicit
! soil-flood-vegetation energy budget with an insulating fractional overlying
! layer of snow: fluxes partitioned between surface "felt" by atmosphere
! and surface in contact with base of snowpack (flux exchange between
! atmosphere and snow surface calculated in explicit snow routine)
! (Boone and Etchevers, 2001, J Hydromet.)
! NOTE for now, the meltwater advection term (heat source/sink)
! is OFF because the corresponding energy should be compensated for
! (but code is retained for possible future activation).
!
ZA(:) = 1. / PTSTEP + DMK%XCT(:) *                         &
         ((ZFNSNOW(:) *                                    &
           ( 4.*ZTRAD(:) + ZRORA(:)*ZCPS(:)*ZPETA2(:) ))   &
         + ZCHUMS(:)*PDQSAT(:) - ZCHUMA(:)*ZPEQ_A_COEF(:)) &
         + 2. * XPI / XDAY        
!
ZB(:) = 1. / PTSTEP + DMK%XCT(:) * ( ZFNSNOW(:)* 3.*ZTRAD(:) + ZCHUMS(:)*PDQSAT(:) ) 
!
ZC(:) = 2. * XPI * PEK%XTG(:,2) / XDAY + DMK%XCT(:) *       &
       ( ZFNSNOW(:) *                                       &
       ( ZRORA(:)*ZCPS(:)*ZPETB2(:)                         &
       + PSW_RAD(:)*(1.-PALBT(:)) + PLW_RAD(:)*PEMIST(:))   &
       - (ZCHUMS(:)*PQSAT(:) - ZCHUMA(:)*ZPEQ_B_COEF(:)))           
!
IF(PEK%TSNOW%SCHEME == '3-L' .OR. PEK%TSNOW%SCHEME == 'CRO' .OR. IO%CISBA == 'DIF')THEN                                 
!
!       5.2. With CSNOW=SNOW3L or CSNOW=CRO or IO%CISBA=DIF
!       -------------------------------------------------
!
   ZC(:) = ZC(:) + DMK%XCT(:)*(PEK%XPSN(:)*PGRNDFLUX(:)+PFLUX_COR(:,1))
!
ELSEIF (LCPL_ARP) THEN
!
!       5.3. With Arpege
!       ----------------
!
ZCDQSAT(:) = (XCPV-XCPD)*ZHUMS(:)*PDQSAT(:)
ZINCR(:)= DMK%XCT(:) * ZRORA(:) * &
          (ZCDQSAT(:) * ( ZPETA2(:)*PEK%XTG(:,1) - ZPETB2(:)) + &
          ZXCPV_XCL_AVG(:) * &
          (ZHUMS(:)*PQSAT(:) - ZHUMA(:) * (ZPEQ_B_COEF(:) + ZPEQ_A_COEF(:) * PEK%XTG(:,1)))) 

! Surface Energy Budget linearization coefficients for a composite 
! (soil-vegetation-flood-snow) energy budget: composite fluxes "felt" by
! atmosphere from a mixed soil,snow and vegetation surface:
! (Douville et al. 1995, J. Clim. Dyn.)
!

  ZA(:) = ZA(:) + ZINCR(:)

  ZB(:) = ZB(:) + ZINCR(:)            
           
  IF (LQVNPLUS) THEN
!
!       5.4. With  LQVNPLUS=TRUE
!       ------------------------
!
    ZCNHUMA(:)=(XCPV-XCPD)*(1.-ZHUMA(:))
    ZPEQA2(:)=ZCNHUMA(:)*ZPEQ_A_COEF(:)*ZPETA2(:)*PEK%XTG(:,1)
    ZDKQB(:)=ZPEQ_B_COEF(:)-PQA(:)

    ZA(:) = ZA(:) + DMK%XCT(:) * ZRORA(:) * &
             (2.* ZPEQA2(:) + &
             ZCNHUMA(:) * (ZDKQB(:)*ZPETA2(:) - ZPEQ_A_COEF(:)*ZPETB2(:) )) 

    ZB(:) = ZB(:) + DMK%XCT(:) * ZRORA(:) * ZPEQA2(:)         
  
    ZC(:) = ZC(:) + DMK%XCT(:)*ZRORA(:)*ZCNHUMA(:) *ZDKQB(:)*ZPETB2(:) 
           
  ENDIF
ENDIF
!

!-------------------------------------------------------------------------------
!
!*       6.     T AT TIME 'T+DT' (before snowmelt or soil ice evolution)
!               -----------------
!
IF(IO%CISBA == 'DIF')THEN                                                          
!
! First determine terms needed for implicit linearization of surface:
!
!  We use harmonic mean to compute the thermal conductivity at the layers interface
!
   ZCOND1(:) = PK%XDZG(:,1)/((PK%XDZG(:,1)+PK%XDZG(:,2))*PSOILCONDZ(:,1))
   ZCOND2(:) = PK%XDZG(:,2)/((PK%XDZG(:,1)+PK%XDZG(:,2))*PSOILCONDZ(:,2))
!
   ZCONDAVG(:) = 1.0/(ZCOND1(:)+ZCOND2(:))
!   
   ZA(:)       = ZA(:) - (2. * XPI / XDAY) + ZCONDAVG(:)*DMK%XCG(:)/PK%XDZDIF(:,1)
   ZTERM2(:)   = ZCONDAVG(:)*DMK%XCG(:)/(ZA(:)*PK%XDZDIF(:,1))
   ZTERM1(:)   = (PEK%XTG(:,1)*ZB(:) + (ZC(:) - (2. * XPI * PEK%XTG(:,2) / XDAY)) )/ZA(:)  
!
! Determine the soil temperatures:
!
   CALL SOIL_HEATDIF(PTSTEP,PK%XDZG(:,:),PK%XDZDIF(:,:),PSOILCONDZ,  &
                     PSOILHCAPZ,DMK%XCG,ZTERM1,ZTERM2, PTDEEP_A,     &
                     KK%XTDEEP, PEK%XTG(:,:), PDEEP_FLUX, PFLUX_COR   )
!
!
! "Restore" flux here is actually the heat flux between the surface
! and sub-surface layers (W m-2):
!
   PRESTORE(:) = ZCONDAVG(:)*(PEK%XTG(:,1)-PEK%XTG(:,2))/PK%XDZDIF(:,1)
!
ELSE
!
  IF(IO%LTEMP_ARP)THEN
!
    CALL SOIL_TEMP_ARP(PTSTEP,ZA,ZB,ZC,KK%XGAMMAT,KK%XTDEEP,IO%XSODELX,PEK%XTG(:,:))
!
!     "Restore" flux between surface and deep layer(W m-2):
    PRESTORE(:) = 2.0 * XPI * (PEK%XTG(:,1)-PEK%XTG(:,2)) / &
                         ( DMK%XCT(:)*XDAY*IO%XSODELX(1)*(IO%XSODELX(1)+IO%XSODELX(2)) )
!      
  ELSE
!
    PEK%XTG(:,1) = ( PEK%XTG(:,1)*ZB(:) + ZC(:) ) / ZA(:)
!
    WHERE(KK%XTDEEP(:) /= XUNDEF .AND. KK%XGAMMAT(:) /= XUNDEF)
      PEK%XTG(:,2) = (PEK%XTG(:,2) + (PTSTEP/XDAY)*(PEK%XTG(:,1) + KK%XGAMMAT(:)*KK%XTDEEP(:))) / &
                         (1.+(PTSTEP/XDAY)*(1.0+KK%XGAMMAT(:)))  
    ELSEWHERE
      PEK%XTG(:,2) = (PEK%XTG(:,2) + (PTSTEP/XDAY)*PEK%XTG(:,1)) /        &
                         (1.+(PTSTEP/XDAY) )  
    END WHERE
!
!     "Restore" flux between surface and deep layer(W m-2):
    PRESTORE(:) = 2.0*XPI*(PEK%XTG(:,1)-PT2M(:))/(DMK%XCT(:)*XDAY)  
!
  ENDIF
!
ENDIF
!
!-------------------------------------------------------------------------------
!*       7.     TA and QA AT TIME 'T+DT' 
!               ------------------------
!               (QA and TA are only modified by these expressions
!                if the implicit atmospheric coupling is used)
!
PQA_IC(:) =  ZPEQ_A_COEF(:)*PEK%XTG(:,1)   + ZPEQ_B_COEF(:)
!
PTA_IC(:) =  ZPET_A_COEF(:)*PEK%XTG(:,1)   + ZPET_B_COEF(:)
!
PUSTAR2_IC(:) =  ZUSTAR2(:)
!
!--------------------------------------------------------------------------------------
!*       8.     Update of LSTT and LVTT for Arpege
!               ----------------------------------
!
IF (LCPL_ARP) THEN

  IF (.NOT.LQVNPLUS) THEN
    PK%XCPS(:) =  PK%XCPS(:) + (XCPV-XCPD) *ZHUMS(:)*PDQSAT(:)*(PEK%XTG(:,1)-PTSM(:))
  ENDIF


  IF (LQVNPLUS) THEN
    PK%XCPS(:) =  PK%XCPS(:) + (XCPV-XCPD) *ZHUMS(:)*PDQSAT(:)*(PEK%XTG(:,1)-PTSM(:))  &
                       + (XCPV-XCPD) *(1-ZHUMA(:))*(PQA_IC(:)-PQA(:))  
  ENDIF

  PK%XLSTT(:) = PK%XLSTT(:) + (XCPV-XCI)*(PEK%XTG(:,1)-PTSM(:))

  PK%XLVTT(:) = PK%XLVTT(:) + (XCPV-XCL)*(PEK%XTG(:,1)-PTSM(:))


ENDIF
!
IF (LHOOK) CALL DR_HOOK('E_BUDGET',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE E_BUDGET
