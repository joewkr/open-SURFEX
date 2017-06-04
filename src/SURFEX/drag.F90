!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE DRAG(HISBA, HSNOW_ISBA, HCPSURF, PTSTEP, PTG, PWG, PWGI,  &
                    PEXNS, PEXNA, PTA, PVMOD, PQA, PRR, PSR, PPS, PRS,   &
                    PVEG, PZ0, PZ0EFF, PZ0H, PWFC, PWSAT, PPSNG, PPSNV,  &
                    PZREF, PUREF, PDIRCOSZW, PDELTA, PF5, PRA, PCH, PCD, &
                    PCDN, PRI, PHUG, PHUGI, PHV, PHU, PCPS, PQS, PFFG,   &
                    PFFV, PFF, PFFG_NOSNOW, PFFV_NOSNOW, PLEG_DELTA,     &
                    PLEGI_DELTA, PWR, PRHOA, PLVTT, PQSAT  )  
!   ############################################################################
!
!!****  *DRAG*  
!!
!!    PURPOSE
!!    -------
!
!     Calculates the drag coefficients for heat and momentum transfers
!     over ground (i.e., Ch and Cd).
!         
!     
!!**  METHOD
!!    ------
!
!     1) computes hu, hv, and delta
!
!     2) use this to find qsoil, the grid-averaged relative humidity
!        of the soil
!
!     3) calculates the Richardson's number
!
!     4) find the transfer and resistance coefficients Ch, Cd, and Ra
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    USE MODD_CST 
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!    Mascart et al. (1995)
!!    Belair (1995)
!!      
!!    AUTHOR
!!    ------
!!
!!      S. Belair           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    13/03/95 
!!      (J.Stein)   15/11/95  use the potential temperature to compute Ri
!!                            and PVMOD instead of ZVMOD
!!      (P.Lacarrere)15/03/96 replace * PEXNS by / PEXNS
!!      (V.Masson)   22/12/97 computation of z0eff after snow treatment
!!      (V.Masson)   05/10/98 clear routine
!!      (A.Boone)    11/26/98 Option for PDELTA: forested vs default surface
!!      (V.Masson)   01/03/03 Puts PDELTA in another routine
!!      (P.LeMoigne) 28/07/05 dependance on qs for cp
!!      (P.LeMoigne) 09/02/06 z0h and snow  
!!      (P.LeMoigne) 09/01/08 limitation of Ri
!!      (B.Decharme) 01/05/08 optional limitation of Ri
!!      (B.Decharme) 03/06/08 flooding scheme
!!      (A.Boone)    03/15/10 Add delta fnctions to force LEG ans LEGI=0
!!                            when hug(i)Qsat < Qa and Qsat > Qa
!!      (A.Boone)    21/11/11 Add Rs_max limit for dry conditions with Etv
!!      (B. Decharme)   09/12 limitation of Ri in surface_ri.F90
!!      (C. Ardilouze)  09/13 Halstead coef set to 0 for very low values
!!      (B. Decharme)   03/16 Bug in limitation of Er for Interception reservoir
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS,     ONLY : XPI, XCPD, XCPV
USE MODD_ISBA_PAR, ONLY : XWGMIN, XRS_MAX
USE MODD_SURF_ATM, ONLY : LDRAG_COEF_ARP, LRRGUST_ARP, XRRSCALE,   &
                            XRRGAMMA, XUTILGUST, LCPL_ARP  
!
USE MODI_SURFACE_RI
USE MODI_SURFACE_AERO_COND
USE MODI_SURFACE_CD
USE MODI_SURFACE_CDCH_1DARP
USE MODI_WIND_THRESHOLD
!
USE MODE_THERMOS
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
CHARACTER(LEN=*),     INTENT(IN)  :: HISBA      ! type of ISBA version:
!                                               ! '2-L' (default)
!                                               ! '3-L'
!                                               ! 'DIF'
CHARACTER(LEN=*),     INTENT(IN)  :: HSNOW_ISBA ! 'DEF' = Default F-R snow scheme
!                                               !         (Douville et al. 1995)
!                                               ! '3-L' = 3-L snow scheme (option)
!                                               !         (Boone and Etchevers 2000)
CHARACTER(LEN=*),     INTENT(IN)  :: HCPSURF    ! option for specific heat Cp:
!                                               ! 'DRY' = dry Cp
!                                               ! 'HUM' = Cp as a function of qs
!
REAL,                 INTENT(IN) :: PTSTEP      ! timestep of ISBA (not split time step)
!
REAL, DIMENSION(:), INTENT(IN)   :: PTG, PWG, PWGI, PEXNS
!                                     PTG     = surface temperature
!                                     PWG     = near-surface volumetric water content
!                                     PWGI    = near-surface frozen volumetric water content
!                                     PEXNS   = Exner function at the surface
!
REAL, DIMENSION(:), INTENT(IN)   :: PEXNA, PTA, PVMOD, PQA, PRR, PSR, PPS
!                                     PEXNA= Exner function
!                                     near surface atmospheric variables
!                                     PTA = 2m temperature
!                                     PVMOD = module of the horizontal wind
!                                             NOTE it should be input as >= 1. (m)
!                                     PQA = specific humidity
!                                     PPS = surface pressure
!                                     PRR = rain rate    
!                                     PSR = snow rate             
!
REAL, DIMENSION(:), INTENT(IN)   :: PRS, PVEG, PZ0, PZ0H, PZ0EFF
REAL, DIMENSION(:), INTENT(IN)   :: PWFC, PWSAT, PPSNG, PPSNV, PZREF, PUREF
!                                     PRS = stomatal resistance
!                                     PVEG = vegetation fraction
!                                     PZ0  = roughness length for momentum
!                                     PZ0H = roughness length for heat
!                                     PZ0EFF= effective roughness length
!                                     PWFC = field capacity volumetric water content
!                                     PWSAT = volumetric water content at saturation
!                                     PPSNG = fraction of the bare ground covered
!                                             by snow
!                                     PPSNV = fraction of the vegetation covered
!                                             by snow
!                                     PZREF = reference height of the first
!                                             atmospheric level 
!                                     PUREF = reference height of the wind
!                                             NOTE this is different from ZZREF
!                                             ONLY in stand-alone/forced mode,
!                                             NOT when coupled to a model (MesoNH)
!
REAL, DIMENSION(:), INTENT(INOUT) :: PDELTA
!                                     PDELTA = fraction of the foliage covered
!                                              by intercepted water
REAL, DIMENSION(:), INTENT(IN)    :: PF5
!                                     PF5 = water stress function for Hv
REAL, DIMENSION(:), INTENT(IN)    :: PDIRCOSZW 
! Cosinus of the angle between the normal to the surface and the vertical
REAL, DIMENSION(:), INTENT(INOUT) :: PRA
!                                      aerodynamic surface resistance
REAL, DIMENSION(:), INTENT(INOUT) :: PCPS
!                                      specific heat at surface
!
REAL, DIMENSION(:), INTENT(OUT)  :: PCH, PCD, PCDN, PRI
!                                     PCH = drag coefficient for heat
!                                     PCD = drag coefficient for momentum
!                                     PCDN= neutral drag coefficient for momentum
!                                     PRI = Richardson number
!
REAL, DIMENSION(:), INTENT(OUT)  :: PHUG, PHUGI, PHV, PHU, PQS, PLEG_DELTA, PLEGI_DELTA
!                                     PHUG  = ground relative humidity
!                                     PHUGI = ground (ice) relative humidity
!                                     PHV = Halstead coefficient
!                                     PHU = relative humidity at the surface
!                                     PQS = humidity at surface
!                                     PLEG_DELTA  = soil evaporation delta fn
!                                     PLEGI_DELTA = soil sublimation delta fn
!
REAL, DIMENSION(:), INTENT(IN)   :: PFFV, PFF, PFFG, PFFG_NOSNOW, PFFV_NOSNOW
!                                   PFFG = Floodplain fraction over ground
!                                   PFFV = Floodplain fraction over vegetation
!                                   PFF  = Floodplain fraction at the surface
!
REAL, DIMENSION(:), INTENT(IN)   :: PWR, PRHOA, PLVTT
!                                   PWR = liquid water retained on the foliage
!                                   PRHOA = near-ground air density
!                                   PLVTT = Vaporization heat
!
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PQSAT
!                                            PQSAT = specific humidity at saturation
!
!*      0.2    declarations of local variables
!
REAL, PARAMETER            :: ZHVLIM = 1.E-12 ! set halstead coef to 0 at this limit
!
REAL, DIMENSION(SIZE(PTG)) :: ZQSAT,           &
!                                              ZQSAT = specific humidity at saturation
                                 ZAC,            &
!                                              ZAC = aerodynamical conductance
                                 ZWFC,           &
!                                              ZWFC = field capacity in presence of ice
                                 ZVMOD,          &
!                                              ZVMOD = wind modulus with minimum threshold
                                  ZFP,           &
!                                              ZFP = working variable                               
                                 ZZHV,           &
!                                              ZZHV = condensation delta fn for Hv
                                 ZRRCOR
!                                              ZRRCOR = correction of CD, CH, CDN due to moist-gustiness
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*       0.     Initialization:
!               ---------------
!
IF (LHOOK) CALL DR_HOOK('DRAG',0,ZHOOK_HANDLE)
PCH(:)   =0.
PCD(:)   =0.
PHUG(:)  =0.
PHUGI(:) =0.
PHV(:)   =0.
PHU(:)   =0.
!
ZQSAT(:)    =0.
ZWFC(:)     =0.
PRI(:)      =0.
!
ZVMOD = WIND_THRESHOLD(PVMOD,PUREF)
!
ZQSAT(:) = QSAT(PTG(:),PPS(:))
!
IF(PRESENT(PQSAT))PQSAT(:)=ZQSAT(:)
!
!-------------------------------------------------------------------------------
!
!*       1.     RELATIVE HUMIDITY OF THE GROUND HU
!               ----------------------------------
!
!                                         this relative humidity is related to
!                                         the superficial soil moisture and the
!                                         field capacity of the ground
!
!
ZWFC(:)  = PWFC(:)*(PWSAT(:)-PWGI(:))/PWSAT(:)
!
PHUG(:)  = 0.5 * ( 1.-COS(XPI*MIN((PWG(:)-XWGMIN) /ZWFC(:),1.)) )

ZWFC(:)  = PWFC(:)*MAX(XWGMIN, PWSAT(:)-PWG(:))/PWSAT(:)
!
PHUGI(:) = 0.5 * ( 1.-COS(XPI*MIN(PWGI(:)/ZWFC(:),1.)) )
!
!
!                                         there is a specific treatment for dew
!                                         (see Mahfouf and Noilhan, jam, 1991)
!                                         when hu*qsat < qa, there are two
!                                         possibilities
!
!                                         a) low-level air is dry, i.e., 
!                                            qa < qsat
!
!                                         NOTE the additional delta fn's
!                                         here are needed owing to linearization 
!                                         of Qsat in the surface energy budget.
!
PLEG_DELTA(:)    = 1.0
PLEGI_DELTA(:)   = 1.0
WHERE ( PHUG*ZQSAT < PQA .AND. ZQSAT > PQA )
  PHUG(:)  = PQA(:) / ZQSAT(:)
  PLEG_DELTA(:)  = 0.0
END WHERE
WHERE ( PHUGI*ZQSAT < PQA .AND. ZQSAT > PQA )
  PHUGI(:) = PQA(:) / ZQSAT(:)
  PLEGI_DELTA(:) = 0.0
END WHERE
!
!                                         b) low-level air is humid, i.e.,
!                                            qa >= qsat
!
WHERE ( PHUG*ZQSAT  < PQA .AND. ZQSAT <= PQA )
  PHUG(:)  = 1.0
END WHERE
WHERE ( PHUGI*ZQSAT < PQA .AND. ZQSAT <= PQA )
  PHUGI(:) = 1.0
END WHERE
!
!-------------------------------------------------------------------------------
!
!*       3.     HALSTEAD COEFFICIENT (RELATIVE HUMIDITY OF THE VEGETATION)
!               ----------------------------------------------------------
!
ZZHV(:)       = MAX(0.,SIGN(1.,ZQSAT(:)-PQA(:))) ! condensation on foilage if = 1
!
PHV(:)        = PDELTA(:) + (1.- PDELTA(:))*                                    &
                ( PRA(:) + PRS(:)*(1.0 - ZZHV(:)) )*                            &
                ( (1/(PRA(:)+PRS(:))) - (ZZHV(:)*(1.-PF5(:))/(PRA(:)+XRS_MAX)) )
!
WHERE(PHV(:)<ZHVLIM)
      PHV(:)=0.0
ENDWHERE
!
!-------------------------------------------------------------------------------
!
!*       4.     GRID-AVERAGED HUMIDITY OF THE SURFACE
!               -------------------------------------
!
IF(HSNOW_ISBA == '3-L' .OR. HSNOW_ISBA == 'CRO' .OR. HISBA == 'DIF')THEN
!
! For explicit snow, humidity HERE only over snow-free region:
!
   PQS(:) =( (1.-PVEG(:))*(1.-PFFG_NOSNOW(:))*(PWG(:) /(PWG(:)+PWGI(:)))*PHUG (:)   &
             + (1.-PVEG(:))*(1.-PFFG_NOSNOW(:))*(PWGI(:)/(PWG(:)+PWGI(:)))*PHUGI(:)   &
             +     PVEG(:) *(1.-PFFV_NOSNOW(:))*    PHV(:)                            &
             + ((1.-PVEG(:))*PFFG_NOSNOW(:)+PVEG(:)*PFFV_NOSNOW(:)))*ZQSAT(:)         &
             +     PVEG(:) *(1.-PFFV_NOSNOW(:))*(1.-PHV(:))*PQA(:)  
!
ELSE
!
   PQS(:) =( (1.-PVEG(:))*(1.-PPSNG(:)-PFFG(:))*(PWG(:) /(PWG(:)+PWGI(:)))*PHUG (:)  &
             + (1.-PVEG(:))*(1.-PPSNG(:)-PFFG(:))*(PWGI(:)/(PWG(:)+PWGI(:)))*PHUGI(:)  &
             + (1.-PVEG(:))*    PPSNG(:)                                               &
             +     PVEG(:) *(1.-PPSNV(:)-PFFV(:))*PHV(:)                               &
             +     PVEG(:) *    PPSNV(:)                                               &
             +     PFF(:)                            )*ZQSAT(:)                        &
             +     PVEG(:) *(1.-PPSNV(:)-PFFV(:))*(1.-PHV(:))*PQA(:)  
!
ENDIF
!
PHU(:) = PQS(:)/ZQSAT(:)
!
!-------------------------------------------------------------------------------
!
!*       5.     SPECIFIC HEAT OF THE SURFACE
!               ----------------------------
!
IF (HCPSURF=='DRY') THEN
    PCPS(:) = XCPD
ELSEIF(.NOT.LCPL_ARP)THEN
    PCPS(:) = XCPD + ( XCPV - XCPD ) * PQA(:)   
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       6.     COMPUTATION OF RESISTANCE AND DRAG COEFFICIENT
!               ----------------------------------------------
!
CALL SURFACE_RI(PTG, PQS, PEXNS, PEXNA, PTA, PQA,                    &
                PZREF, PUREF, PDIRCOSZW, PVMOD, PRI                  )  
!
!-------------------------------------------------------------------------------
!
!*       6.5    LIMITATION OF RICHARDSON NUMBER
!               -------------------------------
!
!Now done directly in SURFACE_RI subroutine
!
!-------------------------------------------------------------------------------
!*       7.0     DRAG COEFFICIENT FOR HEAT AND MOMENTUM TRANSFERS
!               -------------------------------------------------
!
IF (LDRAG_COEF_ARP) THEN

   CALL SURFACE_CDCH_1DARP(PZREF, PZ0EFF, PZ0H, ZVMOD, PTA, PTG, &
                             PQA, PQS, PCD, PCDN, PCH              )  
   PRA(:) = 1. / ( PCH(:) * ZVMOD(:) )

ELSE

!
!*       7.1    SURFACE AERODYNAMIC RESISTANCE FOR HEAT TRANSFERS
!               -------------------------------------------------
!
   CALL SURFACE_AERO_COND(PRI, PZREF, PUREF, ZVMOD, PZ0, PZ0H, ZAC, PRA, PCH)
!
!-------------------------------------------------------------------------------
!
!*       7.2    DRAG COEFFICIENT FOR MOMENTUM TRANSFERS
!               ---------------------------------------
!
!
   CALL SURFACE_CD(PRI, PZREF, PUREF, PZ0EFF, PZ0H, PCD, PCDN)
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       8.     CORRECTION DUE TO MOIST GUSTINESS
!               ---------------------------------
!
IF (LRRGUST_ARP) THEN

   ZFP(:)=MAX(0.0,PRR(:)+PSR(:))
   ZRRCOR(:)=SQRT(1.0+((((ZFP(:)/(ZFP(:)+XRRSCALE))**XRRGAMMA)*XUTILGUST)**2) &
       /(PCD(:)*ZVMOD(:)**2))  

   PCD  = PCD  * ZRRCOR
   PCH  = PCH  * ZRRCOR
   PCDN = PCDN * ZRRCOR

ENDIF
!
!-------------------------------------------------------------------------------
!
!
!*       9.     HALSTEAD COEFFICIENT (WITH THE NEW VALUES OF PRA)
!               ----------------------------------------------------------
!
!
PHV(:)        = PDELTA(:) + (1.- PDELTA(:))*                                       &
                ( PRA(:) + PRS(:)*(1.0 - ZZHV(:)) )*                               &
                ( (1/(PRA(:)+PRS(:))) - (ZZHV(:)*(1.-PF5(:))/(PRA(:)+XRS_MAX)) )
!
CALL LIMIT_LER(PDELTA)
!
WHERE(PHV(:)<ZHVLIM)
      PHV(:)=0.0
ENDWHERE
!                
IF (LHOOK) CALL DR_HOOK('DRAG',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
CONTAINS
!
!-------------------------------------------------------------------------------
!
SUBROUTINE LIMIT_LER(PDELTA)
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:), INTENT(INOUT) :: PDELTA
!
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PDELTA)) ::  ZPSNV
REAL, DIMENSION(SIZE(PDELTA)) ::  ZLEV
REAL, DIMENSION(SIZE(PDELTA)) ::  ZLETR
REAL, DIMENSION(SIZE(PDELTA)) ::  ZLECOEF
REAL, DIMENSION(SIZE(PDELTA)) ::  ZER
REAL, DIMENSION(SIZE(PDELTA)) ::  ZRRVEG
REAL, DIMENSION(SIZE(PDELTA)) ::  ZWR_DELTA
!
REAL(KIND=JPRB)   :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('DRAG:LIMIT_LER',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!*       1.     Initialization:
!               ---------------
!
IF(HSNOW_ISBA == '3-L' .OR. HSNOW_ISBA == 'CRO' .OR. HISBA == 'DIF')THEN
   ZLECOEF(:) = (1.0-PPSNV(:)-PFFV(:))
   ZPSNV  (:) = 0.0
ELSE
   ZLECOEF(:) = 1.0
   ZPSNV  (:) = PPSNV(:)+PFFV(:)
ENDIF
!
!
!*       2.     Interception reservoir consistency:
!               -----------------------------------
!
!  In DRAG, we use the timestep of ISBA (PTSTEP) and not the split time step (ZTSTEP)
!  because diagnostic canopy evaporation (Er) must be consistent with PWR to
!  limit negative dripping in hydro_veg
!
ZLEV(:)  = PRHOA(:) * PLVTT(:) * PVEG(:) * (1-ZPSNV(:)) * PHV(:) * (ZQSAT(:) - PQA(:)) / PRA(:)
!
ZLETR(:) = ZZHV(:) * PRHOA(:) * (1. - PDELTA(:)) * PLVTT(:) * PVEG(:) *(1-ZPSNV(:))         &
         * (ZQSAT(:) - PQA(:)) * ( (1/(PRA(:) + PRS(:))) - ((1.-PF5(:))/(PRA(:) + XRS_MAX)) )
!
ZER(:)=PTSTEP*(ZLEV(:)-ZLETR(:))*ZLECOEF(:)/PLVTT(:)
!
ZRRVEG(:) = PTSTEP*PVEG(:)*(1.-PPSNV(:))*PRR(:)
!
ZWR_DELTA(:)=1.0
!
WHERE( ZZHV(:)>0.0 .AND. ZER(:)/=0.0 .AND. (PWR(:)+ZRRVEG(:))<ZER(:) )
!
       ZWR_DELTA(:) = MAX(0.01,MIN(1.0,(PWR(:)+ZRRVEG(:))/ZER(:)))
!       
       PDELTA(:) = PDELTA(:) * ZWR_DELTA(:)
!
       PHV(:)    = PDELTA(:) + (1.- PDELTA(:))*( PRA(:) + PRS(:)*(1.0 - ZZHV(:)) )* &
                  ( (1/(PRA(:)+PRS(:))) - (ZZHV(:)*(1.-PF5(:))/(PRA(:)+XRS_MAX)) )
!
ENDWHERE
!
IF (LHOOK) CALL DR_HOOK('DRAG:LIMIT_LER',1,ZHOOK_HANDLE)
!
END SUBROUTINE LIMIT_LER
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DRAG
