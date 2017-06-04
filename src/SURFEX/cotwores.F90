!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE COTWORES(PTSTEP, IO, OSHADE, PK, PEK, PDMAX, PPOI, PCSP, &
                    PTG, PF2, PSW_RAD, PQA, PQSAT, PPSNV, PDELTA, PRHOA, &
                    PZENITH, PFFV, PIACAN_SUNLIT, PIACAN_SHADE, PFRAC_SUN, &
                    PIACAN, PABC, PRS, PGPP, PRESP_LEAF     ) 
!   #########################################################################
!
!!****  *COTWORES*  
!!
!!    PURPOSE
!!    -------
!!
!!    Calculates net assimilation of CO2 and leaf conductance.
!!              
!!**  METHOD
!!    ------
!!    Calvet et al. 1998 Forr. Agri. Met. [from model of Jacobs(1994)]
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    USE MODD_CST
!!    USE MODD_CO2V_PAR
!!    USE MODI_COTWO
!!    USE MODI_CCETR
!!    USE MODE_THERMOS
!!
!!    REFERENCE
!!    ---------
!!
!!    Calvet et al. 1998 Forr. Agri. Met. 
!!      
!!    AUTHOR
!!    ------
!!
!!      A. Boone           * Meteo-France *
!!      (following Belair)
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    27/10/97 
!!      V. Masson and V. Rivailland 12/2003 modificatino of ISBA routines order
!!      L. Jarlan   27/10/04 : add of T2 to calculate soil respiration and use
!!                              of CRESPSL key to manage the calculation of soil
!!                              respiration
!!                             PEK%XAN et PPST in kgCO2 m-2 s-1 to be fully
!!                              compatible with vegetation growth module (lailoss.f90)
!!      P Le Moigne 09/2005 AGS modifs of L. Jarlan
!!      S. Lafont      03/09 : change units of EPSO GAMM ANDAY
!!      A.L. Gibelin   06/09 : suppress evolution of [CO2] in canopy
!!      A.L. Gibelin   06/09 : move calculations of some CO2 fluxes
!!      A.L. Gibelin   06/09 : add RESP_LEAF
!!      A.L. Gibelin   07/09 : ensure coherence between cotwores and cotworestress
!!      A.L. Gibelin   07/09 : Suppress PPST and PPSTF as outputs, and diagnose GPP
!!        S. Lafont    03/11 : Correct a bug fopr grassland below wilting point
!!      D. Carrer      04/11 : new radiative transfert 
!!      A. Boone       11/11 : add rsmax to MODD_VEG_PAR
!!      B. Decharme    05/12 : Bug : flood fraction in COTWORES
!!                                   Optimization
!!      R. Alkama      04/12 : add 6 new tree vegtype (9 instead 3)
!!      C. Delire      01/14 : vertical profile of dark respiration for tropical forest 
!!                             (GTROP)   with Carrer radiative transfer (IO%LTR_ML = T)               
!!Seferian & Delire  06/2015 : generalization of (i) linear water-stress reponse
!                              and (ii) exponential decrease of autothrophic respiration to all woody PFTs
!!      B. Decharme    07/15 : Suppress some numerical adjustement for F2 
!!
!-------------------------------------------------------------------------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_P_t, ISBA_PE_t
!
USE MODD_CSTS,           ONLY : XMD, XTT, XLVTT
USE MODD_ISBA_PAR,       ONLY : XRS_MAX, XDENOM_MIN
USE MODD_CO2V_PAR,       ONLY : XPARCF, XMCO2,       &
                                XDMAXX, XDMAXN, XAW, XBW, XASW                              
USE MODD_DATA_COVER_PAR, ONLY : NVT_TEBD, NVT_TRBE, NVT_BONE,   &
                                NVT_TRBD, NVT_TEBE, NVT_TENE,   &
                                NVT_BOBD, NVT_BOND, NVT_SHRB
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODI_CCETR
USE MODI_COTWO
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
!
REAL,                INTENT(IN)  :: PTSTEP      ! time step
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_P_t), INTENT(INOUT) :: PK
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
!
LOGICAL, DIMENSION(:),INTENT(IN) :: OSHADE
!
REAL, DIMENSION(:),  INTENT(IN)  :: PPOI     ! Gaussian weights (as above)
!
REAL,DIMENSION(:),   INTENT(IN)  :: PDMAX  
!                                    PDMAX     = maximum saturation deficit of 
!                                                  atmosphere tolerate by vegetation
!
REAL,DIMENSION(:),   INTENT(IN)  :: PCSP, PTG, PF2, PSW_RAD 
!                                    PCSP  = atmospheric concentration of CO2
!                                    PTG   = updated leaf temperature
!                                    PF2   = normalized soil water stress factor
!                                    PSW_RAD = incident solar radiation
!
REAL,DIMENSION(:),   INTENT(IN)  :: PQA, PQSAT, PPSNV, PDELTA, PRHOA
!                                    PQA   = atmospheric mixing ratio
!                                    PQSAT = surface saturation mixing ratio
!                                    PPSNV = snow cover fraction
!                                    PDELTA= fraction of the foliage covered
!                                        by intercepted water
!                                    PEK%XRESA = air density
!
REAL,DIMENSION(:),    INTENT(IN)  :: PZENITH
!                                    PZENITH = solar zenith angle needed 
!                                    for computation of diffusuion of solar
!                                    radiation: for CO2 model.
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PIACAN_SUNLIT, PIACAN_SHADE, PFRAC_SUN
!
REAL, DIMENSION(:), INTENT(IN)      :: PFFV ! Floodplain fraction over vegetation
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PIACAN ! PAR in the canopy at different gauss level
!
REAL,DIMENSION(:),  INTENT(INOUT) :: PABC, PRS, PGPP
!                                    PABC  = Carrer radiative transfer: normalized heigh of considered layer (bottom=0, top=1)
!                                            Calvet radiative transfer: abcissa of the 3-points Gaussian quadrature 
!                                                (Goudriaan, Agric&For.Meteor, 38,1986)    
!                                    PRS   = stomatal resistance
!                                    PGPP  = Gross Primary Production (kg_CO2/kg_air * m/s)
!
REAL,DIMENSION(:),    INTENT(OUT) :: PRESP_LEAF
!                                    PRESP_LEAF = dark respiration over canopy
!
!*      0.2    declarations of local variables
!
REAL, PARAMETER                :: ZRS_MIN     = 1.E-4  ! minimum canopy resistance (s m-1)
!
INTEGER                     :: JINT, JJ ! index for loops
!
REAL, DIMENSION(SIZE(PEK%XLAI,1)) :: ZANF ! ZANF  = total assimilation over canopy
REAL, DIMENSION(SIZE(PEK%XLAI,1)) :: ZCONVE1, ZTSPC, ZIA
!                                 ZTSPC = temperature conversion (K to C) 
!                                 ZIA   = absorbed PAR
REAL, DIMENSION(SIZE(PEK%XLAI,1)) :: ZLAI, ZGMEST, ZFZERO, ZDMAX
!                                 ZLAI = LAI 
!                                 ZFZERO  = ideal value of F, no photorespiration or 
!                                            saturation deficit
!                                 ZDMAX   = maximum saturation deficit of atmosphere
!                                           tolerate by vegetation
!
REAL, DIMENSION(SIZE(PEK%XLAI,1)) :: ZGAMMT, ZDSP, ZANMAX
!                                 ZGAMMT  = compensation point 
!                                 ZDSP    = saturation deficit of atmosphere 
!                                           verses the leaf surface (with correction)
!
REAL, DIMENSION(SIZE(PEK%XLAI,1)) :: ZXMUS, ZTAN, ZTGS, ZXIA, ZAN0, ZGS0, ZXTGS, ZRDK,ZLAITOP,ZTRDK,ZZLAI  
!                                           ZXMUS = cosine of solar zenith angle
!                                           ZTAN  = canopy integrated net assimilation 
!                                           ZTGS  = canopy integrated  leaf conductance
!                                           ZXIA  = incident radiation after diffusion
!                                           ZAN0  = net asimilation at each interval
!                                                   in the canopy
!                                           ZGS0  = leaf conductance at each interval
!                                                   in the canopy        
!                                           ZXTGS = total canopy conductance
!                                           ZRDK  = dark respiration
!                                           ZLAITOP = LAI (thickness of canopy) above considered layer 
!                                           ZTRDK = canopy integrated dark respiration
!                                           ZZLAI = LAI, used for dark respiration profile
!
REAL, DIMENSION(SIZE(PEK%XLAI,1)) :: ZAN0_,ZGS0_,ZRDK_ ! parameters for shaded leaves
!
REAL, DIMENSION(SIZE(PEK%XLAI,1)) :: ZEPSO
!                                           ZEPSO conversion of PEPSO in kgCO2/kgair m/s
!
REAL, DIMENSION(SIZE(PEK%XLAI,1)) :: ZDMAXSTAR, ZFZEROSTAR, ZFZERON, ZGMESTN  
!                                 ZDMAXSTAR  = maximum saturation deficit of atmosphere
!                                              tolerate by vegetation without soil water stress
!                                 ZFZEROSTAR = initial optimal ratio Ci/Cs for woody vegetation
!                                 ZFZERON    = minimum value for "fzero" in defensive woody strategy
!                                 ZGMESTN    = gmest value at zf2=zf2i in offensive woody strategy
!
!
REAL :: ZABC, ZWEIGHT
!                                           ZABC    = abscissa needed for integration
!                                                     of net assimilation and stomatal 
!                                                     conductance over canopy depth 
!                                                     (working scalar)
!
REAL, DIMENSION(SIZE(PEK%XLAI,1))    :: ZWORK !Work array
!
LOGICAL, DIMENSION(SIZE(PEK%XLAI,1)) :: GHERB, GWOOD, GF2_INF_F2I, GTROP
!
INTEGER, DIMENSION(1)          :: IDMAX
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
! STOMATAL RESISTANCE: ENTRY VARIABLES TO CO2 ROUTINE:
!   CS        = CO2 concentration (kgCO2 kgair-1) cs
!   DSP       = specific humidity deficit (kgH2O kgair-1) ds
!   TSM       = surface temperature (C) ts
!   RG        = global radiation (W m-2) rg
!
! initialisation: convert from ppm to mg/m-3
!
IF (LHOOK) CALL DR_HOOK('COTWORES',0,ZHOOK_HANDLE)
!
ZCONVE1(:) = XMCO2*PRHOA/XMD
!
! initialisation: convert from K to C
!
ZTSPC(:)  = PTG(:) - XTT               
!
ZLAI(:)   = PEK%XLAI(:)
ZGMEST(:) = PEK%XGMES(:)
ZFZERO(:) = PK%XFZERO(:)
!
!GTROP = linear stress in case of tropical evergreen forest 
!        (with fixed f0=0.74 for Carrer rad. transf., f0=0.7 with Calvet rad. transf.)
GTROP (:) = (PK%XVEGTYPE_PATCH(:,NVT_TRBE) > 0.8) 
!
GHERB(:) = (PK%XVEGTYPE_PATCH(:,NVT_TEBD) + PK%XVEGTYPE_PATCH(:,NVT_TRBE) + PK%XVEGTYPE_PATCH(:,NVT_BONE)   &
           +PK%XVEGTYPE_PATCH(:,NVT_TRBD) + PK%XVEGTYPE_PATCH(:,NVT_TEBE) + PK%XVEGTYPE_PATCH(:,NVT_TENE)   & 
           +PK%XVEGTYPE_PATCH(:,NVT_BOBD) + PK%XVEGTYPE_PATCH(:,NVT_BOND) + PK%XVEGTYPE_PATCH(:,NVT_SHRB)<0.5)
GWOOD      (:) = (.NOT.GHERB (:))
!
!
WHERE (PEK%XLAI(:)==XUNDEF) ZLAI(:)=0.0
!
!    See (Varlet-Granchet C., M. Chartier, G. Gosse,  and R. Bonhomme, 1981: 
!    Rayonnement utilise pour la photosynthese des vegetaux en
!    conditions naturelles: caracterisation et variations. 
!    Oecol. Plant. 2(16), 189-202.)
!
!-------------------------------------
! Add soil moisture stress effect to leaf conductance:
! OFFENSIVE and DEFENSIVE water stress response 
!  
ZDMAX(:)  = PDMAX(:)
!
GF2_INF_F2I(:) = (PF2(:)<PEK%XF2I(:))
!
! -HERBACEOUS-
!
WHERE(GHERB(:).AND.PEK%LSTRESS(:))
  ZDMAX(:) = XDMAXN
ENDWHERE
WHERE(GHERB(:).AND..NOT.PEK%LSTRESS(:))
  ZDMAX(:) = XDMAXX
ENDWHERE
!
! PAH and PBH are original coefficients of Calvet 2000
WHERE(GHERB(:).AND.(.NOT.GF2_INF_F2I(:)))
  ZDMAXSTAR(:) = EXP((LOG(ZGMEST(:)*1000.)-PK%XAH(:))/PK%XBH(:))/1000.
  ZDMAX(:) = ZDMAXSTAR(:) - (ZDMAXSTAR(:)-ZDMAX(:))*(1.-PF2(:))/(1.-PEK%XF2I(:))
ENDWHERE
!
WHERE(GHERB(:))
  ZGMEST(:) = EXP(PK%XAH(:)+PK%XBH(:)*LOG(ZDMAX(:)*1000.))/1000.
ENDWHERE
!
WHERE (GHERB(:).AND.GF2_INF_F2I(:).AND.PEK%LSTRESS(:))
  ZGMEST(:) = ZGMEST(:) * PF2(:)/PEK%XF2I(:)
ENDWHERE
WHERE(GHERB(:).AND.GF2_INF_F2I(:).AND.(.NOT.PEK%LSTRESS(:)))
  ZDMAX(:) = ZDMAX(:) * PF2(:)/PEK%XF2I(:)
ENDWHERE
!
! to limit photosynthesis under wilting point
WHERE (GHERB(:).AND.(.NOT.PEK%LSTRESS(:)).AND.ZDMAX(:)<=XDMAXN)
  ZDMAX(:)  = XDMAXN
  ZGMEST(:) = (EXP(PK%XAH(:)+PK%XBH(:)*LOG(XDMAXN*1000.))/1000.)*PF2(:)/PEK%XF2I(:)
ENDWHERE
!
! -WOODY but not tropical forest-
!
WHERE(GWOOD(:))
  ZFZEROSTAR(:) = ( XAW  - LOG(ZGMEST(:)*1000.) )/XBW
ENDWHERE
!
WHERE (GWOOD(:).AND.PEK%LSTRESS(:))
  ZGMESTN(:) = ZGMEST(:)
ENDWHERE
WHERE(GWOOD(:).AND.(.NOT.PEK%LSTRESS(:)))
  ZGMESTN(:) = EXP(XASW - XBW*ZFZEROSTAR(:))/1000.
ENDWHERE
!
WHERE (GWOOD(:).AND.GF2_INF_F2I(:)) 
  ZGMESTN(:) = ZGMESTN(:)*PF2(:)/PEK%XF2I(:)
ENDWHERE
!
WHERE(GWOOD(:))
  ZWORK  (:) = MAX( XDENOM_MIN, ZGMESTN(:) )
  ZFZERON(:) = (XASW - LOG(ZWORK(:)*1000.))/XBW
ENDWHERE
!
WHERE(GWOOD(:).AND.(.NOT.GF2_INF_F2I(:)).AND.PEK%LSTRESS(:))
  ZFZERO(:) = ZFZEROSTAR(:)
  ZFZERO(:) = ZFZERO(:) - (ZFZERO(:)-ZFZERON(:))*(1.-PF2(:))/(1.-PEK%XF2I(:))  
ENDWHERE    
WHERE(GWOOD(:).AND.(.NOT.GF2_INF_F2I(:)).AND.(.NOT.PEK%LSTRESS(:)))
  ZFZERO(:) = ZFZEROSTAR(:)
  ZGMEST(:) = ZGMEST(:) - (ZGMEST(:)-ZGMESTN(:))*(1.-PF2(:))/(1.-PEK%XF2I(:))  
ENDWHERE    
!
WHERE(GWOOD(:).AND.GF2_INF_F2I(:))
  ZFZERO(:) = MIN(.95, ZFZERON(:))
  ZGMEST(:) = ZGMESTN(:)
ENDWHERE  
!
! -Tropical Forest-
!
WHERE(GTROP(:))
 ZFZERO(:) = PK%XFZERO(:)
 ZGMEST(:) = PEK%XGMES(:)*PF2(:)
ENDWHERE
!
!-------------------------
!
! compensation point (ppm): temperature response
!
!before optimization (with non log PQDGAMM) : 
!ZGAMMT(:) = PGAMM(:)*PQDGAMM(:)**(0.1*(ZTSPC(:)-25.0))
ZWORK (:) = (0.1*(ZTSPC(:)-25.0)) * PK%XQDGAMM(:)
ZGAMMT(:) = PK%XGAMM(:) * EXP(ZWORK(:))
!
! specific humidity deficit (kg kg-1)
!
ZDSP(:)   = MAX( 0.0, PQSAT(:) - PQA(:) - PEK%XLE(:)*PEK%XRESA(:)/(PRHOA*XLVTT) )
!
! cosine of solar zenith angle 
!
ZXMUS(:) = MAX(COS(PZENITH(:)),0.01)
!
!
! Compute temperature response functions:
!
! kg/m2/s
!before optimization (with non log PQDAMAX) : 
!ZANMAX(:) = ( PAMAX(:)*PQDAMAX(:)**(0.1*(ZTSPC(:)-25.0)) ) / ...
ZWORK (:) = (0.1*(ZTSPC(:)-25.0)) * PK%XQDAMAX(:)
ZANMAX(:) = ( PK%XAMAX(:) * EXP(ZWORK(:))  ) &
          / ( (1.0+EXP(0.3*(PK%XT1AMAX(:)-ZTSPC(:))))* (1.0+EXP(0.3*(ZTSPC(:)-PK%XT2AMAX(:)))) )
!
! m/s
!before optimization (with non log PQDGMES) : 
!ZGMEST(:) = ( ZGMEST(:)*PQDGMES(:)**(0.1*(ZTSPC(:)-25.0)) ) / ...
ZWORK (:) = (0.1*(ZTSPC(:)-25.0)) * PK%XQDGMES(:)
ZGMEST(:) = ( ZGMEST(:) * EXP(ZWORK(:)) ) &
          / ( (1.0+EXP(0.3*(PK%XT1GMES(:)-ZTSPC(:))))*  (1.0+EXP(0.3*(ZTSPC(:)-PK%XT2GMES(:)))) )  
!
!
! Integration over the canopy: SIZE(PABC) increments
! are used to approximate the integral.
!
ZTAN(:) = 0.0
ZTGS(:) = 0.0
ZTRDK(:)= 0.0
!
! Unit conversion
! ZANMAX and ZEPSO from kgCO2/m2/s to kgCO2/kgair m/s by dividing by RHOA (kgair/m3)
! ZGAMMT from ppm to kgCO2/kgair
ZGAMMT(:)  = ZGAMMT(:) * XMCO2 / XMD * 1e-6
ZANMAX(:) = ZANMAX(:) / PRHOA
ZEPSO(:)  = PK%XEPSO(:)  / PRHOA
!
ZIA(:)     = PSW_RAD(:)*XPARCF
!
DO JINT = 1, SIZE(PABC)
  !
  !  Diffusion of incident radiation:
  !
  IF (IO%LTR_ML) THEN
    !
    ZABC = 1.
    IF (JINT.LT.SIZE(PABC)) ZABC = PABC(JINT+1)
    ZWEIGHT = ZABC - PABC(JINT)
    ZXIA(:) = PIACAN_SUNLIT(:,JINT)
    !
  ELSE
    !
    ZABC = PABC(JINT)
    ZWEIGHT = PPOI(JINT)
    !
    CALL CCETR(ZXIA,ZIA,ZXMUS,ZABC,ZLAI)
    !
    ! PAR at different Gauss  level in micmolphot/m2/s
    !
    PIACAN(:,JINT)= ZXIA(:)
    !
  ENDIF
  !
  ! Compute conductance and assimilation of CO2: 
  !
  !Extinction of respiration depends on LAI above only for tropical evergreen forest
  ZLAITOP(:) = 0.
  ZZLAI  (:) = 1.
  IF (IO%LTR_ML) THEN         
    WHERE(GWOOD(:))  
      ZLAITOP(:) = (1.-(PABC(JINT)+ZABC)/2.)*ZLAI(:)
      ZZLAI(:) = ZLAI(:)
    ENDWHERE
  ENDIF
  CALL COTWO(PCSP, PF2, ZXIA, ZDSP, ZGAMMT,             &
             ZFZERO, ZEPSO, ZANMAX, ZGMEST, PEK%XGC(:), ZDMAX, &  
             ZAN0, ZGS0, ZRDK, ZLAITOP, ZZLAI           )
  !
  IF (IO%LTR_ML) THEN
    !
    ZXIA(:) = PIACAN_SHADE(:,JINT)
    CALL COTWO(PCSP, PF2, ZXIA, ZDSP, ZGAMMT,             &
               ZFZERO, ZEPSO, ZANMAX, ZGMEST, PEK%XGC(:), ZDMAX, &  
               ZAN0_, ZGS0_, ZRDK_, ZLAITOP, ZZLAI        )
    !
    WHERE (OSHADE(:))
      !ponderate sum.
      ZAN0(:)=PFRAC_SUN(:,JINT)*ZAN0(:)+(1.-PFRAC_SUN(:,JINT))*ZAN0_(:)
      ZRDK(:)=PFRAC_SUN(:,JINT)*ZRDK(:)+(1.-PFRAC_SUN(:,JINT))*ZRDK_(:)
      ZGS0(:)=PFRAC_SUN(:,JINT)*ZGS0(:)+(1.-PFRAC_SUN(:,JINT))*ZGS0_(:)
    ENDWHERE
    !
  ENDIF
  !
  ! kgCO2/kgair m/s
  ZTAN (:) = ZTAN (:) + ZAN0(:)*ZWEIGHT
  ZTGS (:) = ZTGS (:) + ZGS0(:)*ZWEIGHT
  ZTRDK(:) = ZTRDK(:) + ZRDK(:)*ZWEIGHT
  !
END DO
!
!
! Total assimilation
!
ZANF(:)= ZTAN(:)
!
! Net assimilation over canopy
!
PEK%XAN(:) = (1.0-PDELTA(:))*(1.0-PPSNV(:)-PFFV(:))*ZANF(:)*ZLAI(:)
!
! Dark respiration over canopy (does not depend on radiation, 
! no need to integrate over vertical dimension)
!
PRESP_LEAF(:) = (1.0-PDELTA(:))*(1.0-PPSNV(:)-PFFV(:))*ZTRDK(:)*ZLAI(:)
!
! Gross primary production over canopy
!
PGPP(:) = PEK%XAN(:) + PRESP_LEAF(:)
!
! Cumulated daily net assimilation over canopy (kgCO2/m2/day)
!
PEK%XANDAY(:) = PEK%XANDAY(:) + PEK%XAN(:) * PTSTEP * PRHOA
!
! Adjust maximum leaf assimilation:
!
PEK%XANFM(:) = MAX( ZANF(:), PEK%XANFM(:) )
!
! Total conductance over canopy 
!
ZXTGS(:) = ZTGS(:)*ZLAI(:)
!
! Canopy resistance from Ags:
!
PRS(:) = MIN( 1.0/(ZXTGS(:)+XDENOM_MIN), XRS_MAX)
!
PRS(:) = MAX( PRS(:), ZRS_MIN)
!
IF (LHOOK) CALL DR_HOOK('COTWORES',1,ZHOOK_HANDLE)
!
END SUBROUTINE COTWORES
