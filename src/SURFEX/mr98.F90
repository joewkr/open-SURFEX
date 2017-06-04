!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE MR98      (PZ0SEA,                                         &
                              PTA, PEXNA, PRHOA, PSST, PEXNS, PQA,            &
                              PTT,                                            &
                              PVMOD, PZREF, PUREF,                            &
                              PPS, PQSAT,                                     &
                              PSFTH, PSFTQ, PUSTAR,                           &
                              PCD, PCDN, PCH, PRI, PRESA, PZ0HSEA             )  
!     #######################################################################
!
!
!!****  *MR98*  
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
!!      V. Masson           * Meteo-France *
!!      from part of water_flux.f90 written by M. tomasini
!!
!!    MODIFICATIONS
!!    -------------
!!      Original      11/2004
!!      (M.Tomasini)  25/07/00  Add an iterative method for the fluxes calculation
!!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS,     ONLY : XPI, XCPD, XG, XKARMAN
USE MODD_SURF_PAR, ONLY : XUNDEF
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
REAL, DIMENSION(:), INTENT(IN)       :: PSST  ! Sea Surface Temperature
REAL, DIMENSION(:), INTENT(IN)       :: PEXNS ! Exner function at sea surface
REAL, DIMENSION(:), INTENT(IN)       :: PPS   ! air pressure at sea surface
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
REAL, DIMENSION(:), INTENT(OUT)      :: PRESA     ! aerodynamical resistance
REAL, DIMENSION(:), INTENT(OUT)      :: PZ0HSEA   ! roughness length for heat
!                                         
!
!
!*      0.2    declarations of local variables
!
!
REAL, DIMENSION(SIZE(PTA)) :: ZZ0SEAH,        &!roughness length heat
                                ZZ0SEAQ,        &!roughness length moisture
                                ZTHVI  
!
REAL, DIMENSION(SIZE(PTA)) :: ZQSA, ZTHA, ZDU, ZDT, ZDQ, ZNU, ZWG
!                             ZQSA = specific humidity at the
!                                     sea surface
!                             ZTHA = potential temperature at the
!                                     the first level
!                             ZDU, ZDT, ZDQ = wind, potential
!                                     temperature and specific 
!                                     humidity difference between
!                                     the first level and the sea
!                             ZNU = air kinematic visosity
!                             ZWG = wind subgrid "gustiness" effects
REAL, DIMENSION(SIZE(PTA)) :: ZTSTAR, ZQSTAR, ZTVSTAR,          &
!                             T*, Q*, Tv* usefull in fluxes computation 
                                ZBUFLX  
!                             ZBUFLX = surface buoyancy flux
!
REAL, DIMENSION(SIZE(PTA)) :: ZLMON, ZZETA, ZPSIM, ZPSIH, ZPSIQ,  &
                                ZKHI, ZKHIC, ZPSIC, ZF,             &
                                ZPSIHSTAND,ZPSIMSTAND,ZZETAST  
!                             ZLMON = Monin Obukhov length
!                             ZZETA = Monin Obukhov Stability
!                                     parameter : Zeta=Zref/Lmon 
!                             ZPSIM, ZPSIH, ZPSIQ = Stability
!                                     functions for Momentum,
!                                     Heat and Moisture
!                             ZKHI = variable for Stability functions
!                             ZPSIC = Stability function correction for
!                                     very unstable conditions
!                             ZKHIC = variable for ZPSIC
!                             ZF = weight parameter in psi function
!                                     computation
!
!  constants for wind gustiness effect (ZWG) calculation
!                                            
REAL                      ::  ZBETA
!                             ZBETA = coef.  for the gustiness
!                                      effect on the wind 

!                                     Wg=(ZBETA).W*
REAL                      ::  ZZBL
!                             ZZBL = atmospheric boundary 
!                                    layer depth (m)
!
REAL                      ::  ZSTAND        
!
!  loop parameters for iterative case
!
INTEGER                   ::  JITER
!                             JITER = loop parameter for
!                                     iterations
!
INTEGER, PARAMETER        ::  IITERMAX = 10
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!                             IITERMAX = number of iterations
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MR98',0,ZHOOK_HANDLE)
PRI(:) = XUNDEF
PCH(:) = XUNDEF
PCD(:) = XUNDEF
PCDN(:) = XUNDEF
PRESA(:) = XUNDEF
!
PSFTH(:) =XUNDEF
PSFTQ(:)=XUNDEF
PUSTAR(:)=XUNDEF
!
!                                    Although it is not necessary, fluxes over
!                                    water surfaces are calculated at every
!                                    grid-points, even for those over the
!                                    continent.  When the "WHERE" function of
!                                    the CRAY F90 will be vectorized, it can
!                                    be used to discriminate points over the
!                                    ocean and the continent.  But for now, 
!                                    the current approach is believed the
!                                    simplest, without being too penalizing.
!                                    It mainly consists in averaging the
!                                    surface fluxes over water and land using
!                                    the cover type variables.
!
!
!       1.     Saturated specific humidity near the water surface
!       ---------------------------------------------------------
!
PQSAT(:) = QSAT(PSST(:),PPS(:))
!
!-------------------------------------------------------------------------------
!
!         Begining of the fluxes calculation
!            by an iterative method coming from the one
!                                  proposed by Fairall et al (1996) for
!                                  the TOGA-COARE Bulk Flux Algorithm
!
!
!
!       3.1            initialisations
!       ----------------------------------------------------
!
ZZBL=650.       
ZSTAND=15.         
ZBETA = 0.6
!
  ZQSA(:)  = 0.98 * PQSAT(:)
  ZTHA(:)  = PTA(:) / PEXNA(:)
  ZTHVI(:) = ( 1 + 0.61 * PQA(:) ) * ZTHA(:)
!
  ZDU(:) = PVMOD(:)
  ZDT(:) = ZTHA(:) - PSST(:)  /PEXNS(:)
  ZDQ(:) = PQA(:)  - ZQSA(:)
!
  PUSTAR(:) = MAX( 0.04 * ZDU(:) , 5E-3)
  ZTSTAR(:)  = 0.04 * ZDT(:)
  ZQSTAR(:)  = 0.04 * ZDQ(:)
  ZTVSTAR(:) = ZTSTAR(:)*(1+0.61*PQA(:)) + 0.61*ZQSTAR(:)*ZTHA(:)
!
  ZNU(:) =  1.318E-5 + 9.282E-8 * (PTA(:) - PTT)
!
!
!
!           3.2    begining of iterations 
!           -------------------------------------------------
!
  DO JITER = 1 , IITERMAX
!
!           3.2.1   compute stability functions
!           -------------------------------------------------
!
    WHERE (ABS(ZTVSTAR(:)) < 1E-6)
      ZZETA(:)   = 0.0
      ZZETAST(:) = 0.0
    ELSEWHERE
      ZLMON(:)   =  ZTHVI(:)*PUSTAR(:)*PUSTAR(:)/(ZTVSTAR(:)*XG*XKARMAN)
      ZZETA(:)   =  MAX( PZREF(:) / ZLMON(:) , -20000.)
      ZZETAST(:) =  MAX( ZSTAND / ZLMON(:) , -20000.)
    END WHERE
!
!
    WHERE(ZZETA(:) >= 0.0)
      ZPSIM(:) = -4.7*ZZETA(:)
      ZPSIH(:) = ZPSIM(:)
      ZPSIQ(:) = ZPSIH(:)
      ZPSIHSTAND(:) = -4.7*ZZETAST(:)
      ZPSIMSTAND(:) = ZPSIHSTAND(:)
    ELSEWHERE
      ZKHI(:) = (1 - 16 * ZZETA(:))**0.25
      ZPSIM(:) =  2*LOG((1+ZKHI(:))/2)                            &
                    + LOG((1+ZKHI(:)*ZKHI(:))/2)                    &
                    - 2*ATAN(ZKHI(:))                               &
                    + XPI/2  
      ZPSIH(:) = 2 * LOG((1+ZKHI(:)*ZKHI(:))/2) 
!
      ZKHI(:)  = (1 - 16 * ZZETAST(:))**0.25
      ZPSIHSTAND(:) =  2 * LOG((1+ZKHI(:)*ZKHI(:))/2) 
      ZPSIMSTAND(:) =   2*LOG((1+ZKHI(:))/2)                                   &
                        + LOG((1+ZKHI(:)*ZKHI(:))/2)                             &
                        - 2*ATAN(ZKHI(:))                                        &
                        + XPI/2  
!
! to match very unstable conditions
!
      ZKHIC(:) = (1 - 12.87 * ZZETA(:))**0.33
      ZPSIC(:) =  1.5 * LOG ((1+ZKHIC(:)+ZKHIC(:)*ZKHIC(:))/3)                 &
                    - (3**0.5)*ATAN((2*ZKHIC(:)+1)/(3**0.5))                     &
                    + XPI/(3**0.5)  
!
      ZF(:) = 1 / (1+ ZZETA(:)*ZZETA(:)) 
      ZPSIM(:) =  ZPSIM(:)*ZF(:) + ZPSIC(:)*(1-ZF(:))
      ZPSIH(:) =  ZPSIH(:)*ZF(:) + ZPSIC(:)*(1-ZF(:))
      ZPSIQ(:) =  ZPSIH(:) 
!
      ZKHIC(:) = (1 - 12.87 * ZZETAST(:))**0.33
      ZPSIC(:) =  1.5 * LOG ((1+ZKHIC(:)+ZKHIC(:)*ZKHIC(:))/3)                 &
                    - (3**0.5)*ATAN((2*ZKHIC(:)+1)/(3**0.5))                     &
                    + XPI/(3**0.5)  
!
      ZF(:) = 1 / (1+ ZZETAST(:)*ZZETAST(:)) 
      ZPSIMSTAND(:) =  ZPSIMSTAND(:)*ZF(:) + ZPSIC(:)*(1-ZF(:))
      ZPSIHSTAND(:) =  ZPSIHSTAND(:)*ZF(:) + ZPSIC(:)*(1-ZF(:))
!
    END WHERE
!
!           3.2.2 compute roughness length Zo, Zoh and Zoq
!           ----------------------------------------------
!
    PZ0SEA(:) = 0.011*PUSTAR(:)*PUSTAR(:)/XG                                 &
                   +0.11*ZNU(:)/(PUSTAR(:))  
!
!
    WHERE(PUSTAR(:) > 0.23)
      ZZ0SEAH(:) = 0.14*ZNU(:)/(PUSTAR(:)-0.2) + 7E-6
      ZZ0SEAQ(:) = 0.20*ZNU(:)/(PUSTAR(:)-0.2) + 9E-6
    ELSEWHERE
      ZZ0SEAH(:) = 0.015*PUSTAR(:)*PUSTAR(:)/XG                             &
                      + 0.18*ZNU(:)/(PUSTAR(:))  
      ZZ0SEAQ(:) = 0.0205*PUSTAR(:)*PUSTAR(:)/XG                            &
                      + 0.294*ZNU(:)/(PUSTAR(:))  
    END WHERE
!
!
!          3.2.3  compute friction velocity u*, T*, Q* and Tv*
!          ---------------------------------------------------
!
    PUSTAR(:) = MAX(                                                         &
                    ZDU(:)*XKARMAN / (LOG(PZREF(:)/PZ0SEA(:)) - ZPSIM(:))       &
                    , 5E-3 )  
!
    ZTSTAR(:) = ZDT(:)*XKARMAN / (LOG(PZREF(:)/ZZ0SEAH(:)) - ZPSIH(:))
!
    ZQSTAR(:) = ZDQ(:)*XKARMAN / (LOG(PZREF(:)/ZZ0SEAQ(:)) - ZPSIQ(:))
!
    ZTVSTAR(:) = ZTSTAR(:)*(1+0.61*PQA(:))+0.61*ZQSTAR(:)*ZTHA(:)
!
!
!         3.2.4  compute subgrid correction for wind due to convective motions
!         --------------------------------------------------------------------
!
    ZBUFLX(:) = -XG*ZTVSTAR(:)*PUSTAR(:)/ZTHVI(:)
!
    WHERE(ZBUFLX(:) > 0.)
      ZWG(:) = ZBETA*(ZBUFLX(:)*ZZBL)**0.333
    ELSEWHERE
      ZWG(:) = 0.
    END WHERE
!
    ZDU(:) = (PVMOD(:)*PVMOD(:)+ZWG(:)*ZWG(:))**0.5
!
  END DO
!
PZ0HSEA = ZZ0SEAH
!-------------------------------------------------------------------------------
!
!        3.3  here are the fluxes of iterative case
!        ------------------------------------------
!
  PSFTH(:)  = - PRHOA(:)  * XCPD  * PUSTAR(:) * ZTSTAR(:)
  PSFTQ(:)  = - PRHOA(:)  *         PUSTAR(:) * ZQSTAR(:)
IF (LHOOK) CALL DR_HOOK('MR98',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE MR98
