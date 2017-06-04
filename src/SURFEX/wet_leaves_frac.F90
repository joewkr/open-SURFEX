!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE WET_LEAVES_FRAC(PWRM, PVEG, PWRMAX_CF, PZ0, PLAI, PWRMAX, PDELTA)
!   ############################################################################
!
!!****  *WET_LEAVES_FRAC*  
!!
!!    PURPOSE
!!    -------
!
!     
!!**  METHOD
!!    ------
!
!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!      S. Belair           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    13/03/95 
!!      (A.Boone)    11/26/98 Option for PDELTA: forested vs default surface
!!      B. Decharme    2008  Add optional maximum value for the fraction of the foliage covered by intercepted water
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURF_ATM, ONLY : XDELTA_MAX
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
REAL, DIMENSION(:), INTENT(IN)   :: PWRM
!                                     PWRM    = liquid water retained on the foliage
!                                               of the vegetation
!
REAL, DIMENSION(:), INTENT(IN)   :: PVEG, PWRMAX_CF, PLAI, PZ0
!                                     PVEG = vegetation fraction
!                                     PLAI = leaf area index
!                                     PWRMAX_CF = coefficient for maximum water interception 
!                                                 storage capacity on the vegetation (kg/m2)
!                                     PZ0  = roughness length
!
!
REAL, DIMENSION(:), INTENT(OUT)  :: PWRMAX
!                                     PWRMAX = maximum equivalent water content
!                                              in the vegetation canopy
!
REAL, DIMENSION(:), INTENT(OUT)  :: PDELTA
!                                     PDELTA = fraction of the foliage covered
!                                              by intercepted water
!
!
!
!*      0.2    declarations of local variables
!
!
!
REAL, DIMENSION(SIZE(PVEG)) :: ZCOEF,          &
!                                              ZCOEF = work array
                                 ZWR,            &
!                                              Interception reservoir limited by WRMAX
                                 ZDELTA_LOW,     &
!                                              ZDELTA_LOW = fraction of the foliage covered
!                                              by intercepted water for low vegetation
                                 ZDELTA_HIGH  
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!                                              ZDELTA_HIGH = fraction of the foliage covered
!                                              by intercepted water for high vegetation

!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WET_LEAVES_FRAC',0,ZHOOK_HANDLE)
PDELTA(:) = 0.
!
!*       2.     FRACTION OF THE FOLIAGE COVERED BY INTERCEPTED WATER (DELTA)
!               ------------------------------------------------------------
!
!                                         first calculate the maximum value of
!                                         equivalent water content in the 
!                                         vegetation canopy
!
PWRMAX(:) = PWRMAX_CF(:) * PVEG(:) * PLAI(:) 
!
ZWR(:) = MIN(PWRM(:),PWRMAX(:))
!
WHERE (PVEG(:)>0. .AND. PWRMAX>0.)
!*                                        calculate 'DELTA'
!
!*       2.1    Low vegetation, Deardorff (1978) formulmation:
!               ---------------------------------------------
!       
  ZDELTA_LOW(:) = ( ZWR(:)/PWRMAX(:) )**(2./3.)
!
!*       2.2    High vegetation, Manzi (1993) formulmation:
!               ------------------------------------------
!
!                                           Manzi (1993) [see also Delire et al. JGR 1997]
!                                           The dynamic vegetation roughness length
!                                           is used to determine which formulation
!                                           for 'DELTA' is used. This formulation
!                                           was calibrated for ARME (tropical forrest)
!                                           and so is used for forest canopies. It
!                                           results in 'smeared' (time and amplitude)
!                                           evaporation from interception relative to
!                                           that using Deardorff (above).
!
  ZCOEF(:)  = 1. + 2.*PLAI(:)
!
  ZDELTA_HIGH(:) =   ZWR(:)/( (1.-ZCOEF(:))*ZWR(:) + ZCOEF(:)*PWRMAX(:) )
!
!
!*       2.3    Ponderation between low and high vegetation (min and max thresholds: z0 of 0.5m and 1m)
!               ------------------------------------------
!
  ZCOEF(:) = MAX(MIN(2.*PZ0(:)-1. ,1.),0.)
!
  PDELTA(:) = (1.-ZCOEF(:)) * ZDELTA_LOW(:) + ZCOEF(:) * ZDELTA_HIGH(:)
!
END WHERE
!
PDELTA(:) = MIN(XDELTA_MAX,PDELTA(:)) 
IF (LHOOK) CALL DR_HOOK('WET_LEAVES_FRAC',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WET_LEAVES_FRAC
