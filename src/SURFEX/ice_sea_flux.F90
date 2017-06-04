!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE ICE_SEA_FLUX(PZ0ICE,                                       &
                              PTA, PEXNA, PRHOA, PTICE, PEXNS, PQA, PRR, PRS, &
                              PVMOD, PZREF, PUREF,                            &
                              PPS, PQSAT,                                     &
                              PSFTH, PSFTQ, PUSTAR,                           &
                              PCD, PCDN, PCH, PRI, PRESA, PZ0HICE             )  
!     #######################################################################
!
!
!!****  *ICE_SEA_FLUX*  
!!
!!    PURPOSE
!!    -------
!      Calculate the surface fluxes of heat, moisture, and momentum over
!       sea ice. adapted from WATER_FLUX  
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!    XCD_ICE_CST, from MODD_SEAFLUX
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
!!      (P. LeMoigne) 09/02/06  Z0H as output
!!      B. Decharme    06/2009  limitation of Ri
!!      Modified       09/2009  B. Decharme: limitation of Ri in surface_ri.F90
!!      S.Senesi       01/2014  use XCD_ICE_CST (if /= 0) as value for for Cd, Cdn and Ch
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS,       ONLY : XG, XCPD
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_SURF_ATM,   ONLY : LDRAG_COEF_ARP, LRRGUST_ARP, XRRSCALE, &
                            XRRGAMMA, XUTILGUST     
USE MODD_SNOW_PAR,   ONLY : XZ0SN, XZ0HSN
USE MODN_SEAFLUX_n,  ONLY : XCD_ICE_CST
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
REAL, DIMENSION(:), INTENT(IN)       :: PTICE ! Sea ice Surface Temperature
REAL, DIMENSION(:), INTENT(IN)       :: PEXNS ! Exner function at sea surface
REAL, DIMENSION(:), INTENT(IN)       :: PPS   ! air pressure at sea surface
REAL, DIMENSION(:), INTENT(IN)       :: PRR   ! rain rate
REAL, DIMENSION(:), INTENT(IN)       :: PRS   ! snow rate
!
REAL, DIMENSION(:), INTENT(INOUT)    :: PZ0ICE! roughness length over the sea ice
!                                         
!                                         
!  surface fluxes : latent heat, sensible heat, friction fluxes
REAL, DIMENSION(:), INTENT(OUT)      :: PSFTH ! heat flux  (W/m2)
REAL, DIMENSION(:), INTENT(OUT)      :: PSFTQ ! water flux (kg/m2/s)
REAL, DIMENSION(:), INTENT(OUT)      :: PUSTAR! friction velocity (m/s)
!
! diagnostics
REAL, DIMENSION(:), INTENT(OUT)      :: PQSAT ! humidity at saturation
REAL, DIMENSION(:), INTENT(OUT)      :: PCD   ! momentum drag coefficient
REAL, DIMENSION(:), INTENT(OUT)      :: PCDN  ! neutral momentum drag coefficient
REAL, DIMENSION(:), INTENT(OUT)      :: PCH   ! heat drag coefficient
REAL, DIMENSION(:), INTENT(OUT)      :: PRI   ! Richardson number
REAL, DIMENSION(:), INTENT(OUT)      :: PRESA     ! aerodynamical resistance
REAL, DIMENSION(:), INTENT(OUT)      :: PZ0HICE    ! heat roughness length
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
REAL, DIMENSION(SIZE(PTA)) :: ZRRCOR    ! correction od CD, CH, CDN due to moist-gustiness
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!
!-------------------------------------------------------------------------------
!
!       1.     Initializations
!              ---------------
!
IF (LHOOK) CALL DR_HOOK('ICE_SEA_FLUX',0,ZHOOK_HANDLE)
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
PQSAT(:) = QSAT(PTICE(:),PPS(:))
!
!-------------------------------------------------------------------------------
!
!       2.     Calculate the drag coefficient for momentum (PCD)
!              -------------------------------------------------
!
!       2.1    Richardson number
!              -----------------
!

 CALL SURFACE_RI(PTICE,PQSAT,PEXNS,PEXNA,PTA,PQA, &
                  PZREF, PUREF, ZDIRCOSZW,PVMOD,PRI)
!                  
!
!       2.2    Z0 for  sea ice
!              --------------------
!
PZ0HICE(:) = XZ0HSN
!
PZ0ICE (:) = XZ0SN
!
!-------------------------------------------------------------------------------
!
!       3.     Drag coefficient for heat and aerodynamical resistance
!              ----------------
!
ZVMOD(:)=WIND_THRESHOLD(PVMOD(:),PUREF(:))
!
IF ( XCD_ICE_CST == 0.0 ) THEN 
!
  IF (LDRAG_COEF_ARP) THEN
!
     CALL SURFACE_CDCH_1DARP(PZREF, PZ0ICE, PZ0HICE , ZVMOD, PTA, PTICE, &
                             PQA, PQSAT, PCD, PCDN, PCH                 )  
!
     ZRA(:) = 1. / ( PCH(:) * ZVMOD(:) )
!
  ELSE

     CALL SURFACE_CD(PRI, PZREF, PUREF, PZ0ICE, PZ0HICE, PCD, PCDN)
!
     CALL SURFACE_AERO_COND(PRI, PZREF, PUREF, ZVMOD, PZ0ICE, PZ0HICE, ZAC, ZRA, PCH)
!
  ENDIF
!
ELSE
!
! Using variable transfer coefficients is not appropriate on seaice 
! with simple bulk functions.
! A constant value (e.g. 1.5.e-3 ) is preferable, and used except if the  
! user request backward compatibility by setting XCD_ICE_CST to 0 (DEFAULT). 
!
   PCD (:)=XCD_ICE_CST
   PCDN(:)=XCD_ICE_CST
   PCH (:)=XCD_ICE_CST
   ZRA (:)=1./(PCH(:)*ZVMOD(:))
!
ENDIF
!
ZUSTAR2(:) = PCD(:)*ZVMOD(:)*ZVMOD(:)
!
PRESA(:) = ZRA(:)
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
!-------------------------------------------------------------------------------
!
!       4.     The fluxes
!              ----------
!
PSFTH (:) =  XCPD * PRHOA(:) * PCH(:) * ZVMOD(:) * ( PTICE(:) -PTA(:) * PEXNS(:) / PEXNA(:) ) / PEXNS(:)
! Using Heat transfer coefficient CH for vapor transfer coefficient CE !
PSFTQ (:) =  PRHOA(:) * PCH(:) * ZVMOD(:) * ( PQSAT(:)-PQA(:) )
PUSTAR(:) = SQRT(ZUSTAR2(:))
!
IF (LHOOK) CALL DR_HOOK('ICE_SEA_FLUX',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE ICE_SEA_FLUX
