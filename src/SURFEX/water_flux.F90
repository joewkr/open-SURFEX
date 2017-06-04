!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WATER_FLUX(PZ0SEA,                                         &
                              PTA, PEXNA, PRHOA, PSST, PEXNS, PQA, PRR, PRS,  &
                              PTT, PVMOD, PZREF, PUREF,                       &
                              PPS, OHANDLE_SIC, PQSAT,                        &
                              PSFTH, PSFTQ, PUSTAR,                           &
                              PCD, PCDN, PCH, PRI, PRESA, PZ0HSEA             )  
!     #######################################################################
!
!
!!****  *WATER_FLUX*  
!!
!!    PURPOSE
!!    -------
!      Calculate the surface fluxes of heat, moisture, and momentum over
!      water surfaces.  
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
!!    AUTHOR
!!    ------
!!      S. Belair           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original      01/09/95 
!!      (J.Stein)     16/11/95  use PUSLOPE and Theta to compute Ri
!!      (P.Lacarrere) 19/03/96  bug in the ZTHVI and ZTHVIS computations
!!      (J.Stein)     27/03/96  use only H and LE in the soil scheme
!!      (P.Jabouille) 12/11/96  bug in the Z0 computation
!!      (V.Masson)    01/02/00  detection of sea ice
!!      (P. Tulet)    01/10/03  aerodynamical resistance output
!!      (P. LeMoigne) 29/03/04  bug in the heat flux computation
!!      (P. LeMoigne) 29/03/04  use z0h for diagnostics (ice)
!!      (P. LeMoigne) 20/06/07  minimum wind speed and/or shear
!!      B. Decharme    06/2009 limitation of Ri
!!      B. Decharme    09/2012 limitation of Ri in surface_ri.F90
!!      B. Decharme    06/2013 Charnock number according to coare3.0
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS,       ONLY : XG, XCPD, XLSTT
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_SNOW_PAR,   ONLY : XZ0SN, XZ0HSN
!
USE MODI_SURFACE_RI
USE MODI_SURFACE_AERO_COND
USE MODI_SURFACE_CD
USE MODI_SURFACE_CDCH_1DARP
USE MODI_WIND_THRESHOLD
!
USE MODE_THERMOS
!
USE MODD_SURF_ATM,    ONLY : LDRAG_COEF_ARP, XVCHRNK, XVZ0CM, LVZIUSTAR0_ARP, XVZIUSTAR0,  &
                               LRRGUST_ARP, XRRSCALE, XRRGAMMA, XUTILGUST, XRZHZ0M
!
USE MODD_REPROD_OPER,  ONLY : CCHARNOCK
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
REAL, DIMENSION(:), INTENT(IN)       :: PTA   ! air temperature at atm. level
REAL, DIMENSION(:), INTENT(IN)       :: PQA   ! air humidity at atm. level (kg/kg)
REAL, DIMENSION(:), INTENT(IN)       :: PEXNA ! Exner function at atm. level
REAL, DIMENSION(:), INTENT(IN)       :: PRHOA ! air density at atm. level
REAL, DIMENSION(:), INTENT(IN)       :: PVMOD ! module of wind at atm. wind level
REAL, DIMENSION(:), INTENT(IN)       :: PZREF ! atm. level for temp. and humidity
REAL, DIMENSION(:), INTENT(IN)       :: PUREF ! atm. level for wind
REAL, DIMENSION(:), INTENT(IN)       :: PSST  ! Sea Surface Temperature
REAL, DIMENSION(:), INTENT(IN)       :: PEXNS ! Exner function at sea surface
REAL, DIMENSION(:), INTENT(IN)       :: PPS   ! air pressure at sea surface
LOGICAL,            INTENT(IN)       :: OHANDLE_SIC ! if TRUE, do not care to detect seaice
REAL, DIMENSION(:), INTENT(IN)       :: PRR   ! rain rate
REAL, DIMENSION(:), INTENT(IN)       :: PRS   ! snow rate
REAL,               INTENT(IN)       :: PTT   ! temperature of freezing point
!
REAL, DIMENSION(:), INTENT(INOUT)    :: PZ0SEA! roughness length over the ocean
!                                         
!                                         
!  surface fluxes : latent heat, sensible heat, friction fluxes
REAL, DIMENSION(:), INTENT(OUT)      :: PSFTH ! heat flux  (W/m2)
REAL, DIMENSION(:), INTENT(OUT)      :: PSFTQ ! water flux (kg/m2/s)
REAL, DIMENSION(:), INTENT(OUT)      :: PUSTAR! friction velocity (m/s)
!
! diagnostics
REAL, DIMENSION(:), INTENT(OUT)      :: PQSAT ! humidity at saturation
REAL, DIMENSION(:), INTENT(OUT)      :: PCD   ! heat drag coefficient
REAL, DIMENSION(:), INTENT(OUT)      :: PCDN  ! momentum drag coefficient
REAL, DIMENSION(:), INTENT(OUT)      :: PCH   ! neutral momentum drag coefficient
REAL, DIMENSION(:), INTENT(OUT)      :: PRI   ! Richardson number
REAL, DIMENSION(:), INTENT(OUT)      :: PRESA ! aerodynamical resistance
REAL, DIMENSION(:), INTENT(OUT)      :: PZ0HSEA ! heat roughness length over the ocean
!
!
!*      0.2    declarations of local variables
!
!
REAL, DIMENSION(SIZE(PTA)) :: ZVMOD     ! wind modulus
REAL, DIMENSION(SIZE(PTA)) :: ZUSTAR2   ! square of friction velocity
REAL, DIMENSION(SIZE(PTA)) :: ZAC       ! Aerodynamical conductance
REAL, DIMENSION(SIZE(PTA)) :: ZRA       ! Aerodynamical resistance
REAL, DIMENSION(SIZE(PTA)) :: ZDIRCOSZW ! orography slope cosine (=1 on water!)
REAL, DIMENSION(SIZE(PTA)) :: ZFP       ! working variable
REAL, DIMENSION(SIZE(PTA)) :: ZRRCOR    ! correction of CD, CH, CDN due to moist-gustiness
REAL, DIMENSION(SIZE(PTA)) :: ZCHARN    ! Charnock number
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!       1.     Initializations
!              ---------------
!
IF (LHOOK) CALL DR_HOOK('WATER_FLUX',0,ZHOOK_HANDLE)
ZDIRCOSZW=1.
!
PRI(:) = XUNDEF
PCH(:) = XUNDEF
PCD(:) = XUNDEF
PCDN(:) = XUNDEF
!
PSFTH (:)=XUNDEF
PSFTQ (:)=XUNDEF
PUSTAR(:)=XUNDEF
PRESA(:)=XUNDEF
!
!
!       1.1    Saturated specified humidity near the water surface
!              ---------------------------------------------------
!
PQSAT(:) = QSAT(PSST(:),PPS(:))
!
!
!       1.2    Wind speed threshold
!              --------------------
!
ZVMOD(:)=WIND_THRESHOLD(PVMOD(:),PUREF(:))
!
!       1.3    Charnock number
!              ---------------
!
IF(CCHARNOCK=='OLD')THEN
  ZCHARN(:) = XVCHRNK
ELSE
! vary between 0.011 et 0.018 according to Chris Fairall's data as in coare3.0        
  ZCHARN(:) = MAX(0.011,MIN(0.018,0.011+0.007*(ZVMOD(:)-10.)/8.))
ENDIF
!
!-------------------------------------------------------------------------------
!
!       2.     Calculate the drag coefficient for momentum (PCD)
!              -------------------------------------------------
!
!       2.1    Richardson number
!              -----------------
!
CALL SURFACE_RI(PSST,PQSAT,PEXNS,PEXNA,PTA,PQA,  &
                PZREF, PUREF, ZDIRCOSZW,PVMOD,PRI)
!
!       2.2    Detection of sea ice
!              --------------------
!
IF (LVZIUSTAR0_ARP) THEN
   PZ0HSEA(:)=MIN(PZ0SEA(:),PZ0SEA(:)*XRZHZ0M)
ELSE
   PZ0HSEA(:)=PZ0SEA(:)
ENDIF
!
IF (.NOT.OHANDLE_SIC ) THEN 
   WHERE (PSST(:) < PTT)
      PZ0HSEA(:) = XZ0HSN
   END WHERE
ENDIF
!
!       2.3    Drag coefficient
!              ----------------
!
IF (LDRAG_COEF_ARP) THEN
 
  CALL SURFACE_CDCH_1DARP(PZREF, PZ0SEA, PZ0HSEA, ZVMOD, PTA, PSST, &
                          PQA, PQSAT, PCD, PCDN, PCH                )  

  ZRA(:) = 1. / ( PCH(:) * ZVMOD(:) )
!
ELSE
!
  CALL SURFACE_CD(PRI, PZREF, PUREF, PZ0SEA, PZ0HSEA, PCD, PCDN)
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!       3.     Calculate u* and the roughness length over the ocean
!              ----------------------------------------------------
!
!                              According to Charnock's expression...
!
ZUSTAR2(:) = PCD(:)*ZVMOD(:)*ZVMOD(:)
!
PZ0SEA (:) = ZCHARN(:) * ZUSTAR2(:) / XG + XVZ0CM * PCD(:) / PCDN(:)
!
IF (LVZIUSTAR0_ARP .AND. XVZIUSTAR0>0.) THEN
    PZ0HSEA(:)=PZ0SEA(:)*EXP(-SQRT(ZUSTAR2(:))*XVZIUSTAR0)
ELSE
    PZ0HSEA(:)=PZ0SEA(:)
ENDIF
!
IF (.NOT.OHANDLE_SIC ) THEN 
   WHERE (PSST(:) < PTT)
          PZ0SEA(:) = XZ0SN
   END WHERE
ENDIF
!
!-------------------------------------------------------------------------------
!
!       4.     Drag coefficient for heat and aerodynamical resistance
!              -------------------------------------------------------
!
IF (.NOT.LDRAG_COEF_ARP) THEN
   CALL SURFACE_AERO_COND(PRI, PZREF, PUREF, ZVMOD, PZ0SEA, PZ0HSEA, ZAC, ZRA, PCH)
ENDIF
!
IF (LRRGUST_ARP) THEN
  ZFP(:)=MAX(0.0,PRR(:)+PRS(:))
  ZRRCOR(:)=SQRT(1.0+((((ZFP(:)/(ZFP(:)+XRRSCALE))**XRRGAMMA)*XUTILGUST)**2) &
      /(PCD(:)*ZVMOD(:)**2))  

  PCD  = PCD*ZRRCOR
  PCH  = PCH*ZRRCOR
  PCDN = PCDN*ZRRCOR
ENDIF
!
PRESA(:) = ZRA(:)
!
!-------------------------------------------------------------------------------
!
!       5.     The fluxes
!              ----------
!
PSFTH (:) =  XCPD * PRHOA(:) * PCH(:) * ZVMOD(:) * ( PSST(:) -PTA(:) * PEXNS(:) / PEXNA(:) ) / PEXNS(:)
PSFTQ (:) =  PRHOA(:) * PCH(:) * ZVMOD(:) * ( PQSAT(:)-PQA(:) )
PUSTAR(:) = SQRT(ZUSTAR2(:))
!
IF (LHOOK) CALL DR_HOOK('WATER_FLUX',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WATER_FLUX
