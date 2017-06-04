!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!####################################################################
    SUBROUTINE SURFACE_AIR_MEB(PZ0, PZ0H, PZ0G, PH_VEG, PLAI,          &
                               PTG, PTC, PTV, PVELC, PLW,              &
                               PDISPH,                                 &
                               PRAGNC, PGVNC,                          &
                               PUSTAR2, PCD, PCH, PRI                  )
!
! typical values for nordic forest:
!     PZ0 = 0.8 m
!     PZ0G = 0.007m
!     PH_VEG =   15 m
!     PLAI =  5.
!     PCHIL =   0.12
!     PLW   =   0.02 m
!
!
!!****  *SURFACE_AIR_MEB*
!!
!!    PURPOSE
!!    -------
!
!     Calculates the aerodynamic resistance (PRAGNC) between the soil and eventually
!     understory vegetation and canopy air, based on Choudhury and Monteith (1998)
!     and Monteith (1975). 
!     Modification for free convection based on Sellers et.al (1986), leaf drag 
!     coefficient according to Sellers et.al. (1996).
!     Also calculates the aerodynamic conductance (PGVNC) between the canopy itself
!     and canopy air, also based on Choudhury and Monteith (1998), and modified for
!     free conveection by Sellers et.al. (1986)
!     Only used for double energy balance
!         
!     
!!**  METHOD
!!    ------
!!      
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!      P. Samuelsson/S.Gollvik           * SMHI *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       11/2010
!!     (A. Boone)   25/06/2014  Use stability fn from ISBA for stable conditions
!!                              (since none existed before). Added to handle extremely 
!!                              stable conditions, such as encountered over a snowpack
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS,     ONLY : XG, XKARMAN
USE MODD_SURF_ATM, ONLY : XRIMAX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:), INTENT(IN)   :: PZ0, PZ0H, PZ0G, PH_VEG, PLAI
!                                     PZ0  = roughness length for momentum
!                                     PZ0H = roughness length for heat
!                                     PZ0G = roughness length for soil(understory veg?)/snow
!                                     PH_VEG = height of the vegetation
!                                     PLAI = leaf area index
!
REAL, DIMENSION(:), INTENT(IN)   :: PTG, PTC, PTV, PVELC
!                                     PTG     = surface temperature
!                                     PTC     = canopy air temperature
!                                     PTV     = canopy temperature
!                                     PVELC  =  wind speed at top of vegetation
!
REAL, DIMENSION(:), INTENT(IN)   :: PLW, PDISPH
!                                     PLW = leaf width
!                                     PDISPH = displacement height
!
REAL, DIMENSION(:), INTENT(OUT)  :: PRAGNC, PGVNC
!                                     PRAGNC = aerodynamic resistance between
!                                              soil/snow and canopy air
!                                     PGVNC = aerodynamic conductance between
!                                              the canopy and canopy air
!
REAL, DIMENSION(:), INTENT(OUT)  :: PUSTAR2, PCD, PCH, PRI
!                                   PUSTAR2 = canopy top friction velocity squared (m2/s2)
!                                   PCD     = drag coefficient (-)
!                                   PCH     = heat transfor coefficient (ground to canopy air) (-)
!                                   PRI     = Richardson number (ground to canopy air) (-)
!
!
!*      0.2    declarations of local variables
!
!
REAL, DIMENSION(SIZE(PTG)) :: ZDIFFH, ZK, ZPIH, ZDIFFT, ZRIF, ZZ0HG, ZUSTAR
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      0.3    declarations of local parameters
!
REAL, PARAMETER             :: ZNY          = 0.15e-04 !kinematic viscosity for air
REAL, PARAMETER             :: ZALPHA       = 4.       !attenuation coef. for mom.
                                                       ! ~2 for Wheat, ~4 for coniferous forest
                                                       ! NOTE Eventually should possibly be made a fn of veg type
REAL, PARAMETER             :: ZALPHAPRIM   = 3.       !attenuation coef. for wind
REAL, PARAMETER             :: ZA           = 0.01
REAL, PARAMETER             :: ZRAFA        = 9.       !resistance factor for stability correction
                                                       !for unstable conditions

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
!*       0.     Initialization:
!               ---------------
!
IF (LHOOK) CALL DR_HOOK('SURFACE_AIR_MEB',0,ZHOOK_HANDLE)
!
PRAGNC(:)  = 0.
PGVNC(:)   = 0.
!
ZDIFFH(:)  = MAX(PH_VEG(:)-PDISPH(:),0.1)
!
ZK(:)      = (XKARMAN*XKARMAN)*PVELC(:)*ZDIFFH(:)/LOG(ZDIFFH(:)/PZ0(:))
!
! Just a diagnostic: Ustar and the equivalent drag coef at the top of the canopy (m/s):
!
ZUSTAR(:)  = ZK(:)/(XKARMAN*ZDIFFH(:))
PCD(:)     = ZUSTAR(:)/MAX(1.,PVELC(:))
PUSTAR2(:) = ZUSTAR(:)**2
!
! Aerodynamic resistance, Eq. 25 Choudhury and Monteith, 1988:
!
PRAGNC(:)=PH_VEG(:)*EXP(ZALPHA)/(ZALPHA*ZK(:))*(EXP(-ZALPHA*PZ0G(:)/PH_VEG(:)) &
          -EXP(-ZALPHA*(PDISPH(:)+PZ0(:))/PH_VEG(:)))
!
! Modify the aerodynamic resistance, with an unstable transfer correction 
! Eq. A15, Sellers et.al. 1986 (RI < 0)
!
! For stable conditions (RI > 0), use a form which asymptotically approaches 0
! as Ri==>infinty. This is a fairly generic form typical of such curves...
! As we have no predefined stable correction, we use ISBAs
! (see Noilhan and Mahfouf, 1996). This stable correction
! is most critical for snow...BOTH sublimation from ground-based snowpack and
! melt conditions which can be characterized by very
! strong stable conditions, thus a limited, constant value of the turbulent
! exchange can permit very large negative heat fluxes (towards the snow)
! and excessive melt rates ...also excessive sublimation can occur in winter.
! Since the ISBA stability fn has a roughness length factor, we use
! a simple Ri-based weight factor to ensure a continuous transition
! between stability regimes. In the limit as Ri==>Ricrit, the 
! transfer function collapses into the ISBA relationship with the full roughness factor.
! Finally, note that we assume the ratio z0/z0h is the same for vegetation and 
! underlying surface (as done for composite ISBA).
!
PRI(:)   = -XG*PH_VEG(:)*(PTG(:)-PTC(:))/(PTG(:)*PVELC(:)*PVELC(:))
!
ZRIF(:)  = 0.
ZZ0HG(:) = PZ0G(:)
!
WHERE(PRI(:) <= 0.)                                        ! Unstable - Sellers 
   ZPIH(:) = SQRT(1. - ZRAFA*PRI(:))                       
ELSEWHERE                                                  ! Stable   - Noilhan and Mahfouf
   ZZ0HG(:)= PZ0G(:)*PZ0H(:)/PZ0(:)                        
   ZRIF(:) = MIN(1., PRI(:)/XRIMAX)
   ZPIH(:) = (1/(1. + 15*PRI(:)*SQRT(1.+5*PRI(:))))* &  
             ((1.-ZRIF(:)) + ZRIF(:)*(LOG(ZDIFFH(:)/PZ0G(:))/LOG(ZDIFFH(:)/ZZ0HG(:))) )            ! continuous
END WHERE
!
PRAGNC(:)  = PRAGNC(:)/ZPIH(:)
!
! Diagnose CH (transfer coefficient: -) just for diagnostic purposes:
!
PCH(:)     = 1/(PRAGNC(:)*MAX(1., PVELC(:)))
!
! Aerodynamic resistance within the canopy layer, i.e. between canopy and canopy air
! Eq. 29 and 30, Choudhury and Monteith, 1988:
!
PGVNC(:)  = (2.*ZA)*PLAI(:)/ZALPHAPRIM*SQRT(PVELC(:)/PLW(:))*(1.-EXP(-0.5*ZALPHAPRIM)) 
!
! Limit the aerodynamic conductance to a small value
!
PGVNC(:)  = MAX(1.E-06,PGVNC(:))
!
! Free convection correction Eq. A9 Sellers et.al., 1986: 
!
ZDIFFT(:) = MAX(1.E-06,PTV(:)-PTC(:))
ZDIFFT(:) = SQRT(SQRT(ZDIFFT(:)/PLW(:)))
PGVNC(:)  = PGVNC(:)+ZDIFFT(:)*PLAI(:)/890.
!
IF (LHOOK) CALL DR_HOOK('SURFACE_AIR_MEB',1,ZHOOK_HANDLE)
!
END SUBROUTINE SURFACE_AIR_MEB
