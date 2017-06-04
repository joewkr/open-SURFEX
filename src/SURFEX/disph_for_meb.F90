!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!####################################################################
    SUBROUTINE DISPH_FOR_MEB(PCHIL,PLAIV,PLW,PH_VEG,PZREF,PZ0_MEBV,PDISPH)
!
! typical values for nordic forest:
!     PH_VEG =   15 m
!     PCHIL =   0.12
!     PLW   =   0.02 m
!
!!****  *DISPH_FOR_MEB*
!!
!!    PURPOSE
!!    -------
!
!     Calculates the displacement height, (PDISPH) 
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
!!      Original    12/2010
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS,     ONLY : XPI
USE MODD_ISBA_PAR, ONLY : XLIMH
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:), INTENT(IN)   :: PCHIL, PLAIV, PLW, PH_VEG, PZREF,PZ0_MEBV  
!                                     PCHIL   = Ross-Goudriaan leaf angle distr
!                                     PLAIV    = leaf area index
!                                     PLW     = leaf width
!                                     PH_VEG = height of the vegetation
!                                     PZREF   = height of the lowest model layer
!                                     PZ0_MEBV = momentum roughness for canopy
!
REAL, DIMENSION(:), INTENT(OUT)   :: PDISPH
!                                      PDISPH = displacement height
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PLAIV)) :: ZREVEG, ZCD
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      0.3    declarations of local parameters
!
REAL, PARAMETER             :: ZUL    = 1.        ! typical windspeed within the foliage
REAL, PARAMETER             :: ZNY    = 0.15e-04  ! kinematic viscosity for air
REAL, PARAMETER             :: ZFRTOP  = 0.95     ! maximumi displacement heightfraction
!                                                   of vegetation top
!
!---------------------------------------------------------------------------------
!
!*       0.     Initialization:
!               ---------------
!
IF (LHOOK) CALL DR_HOOK('DISPH_FOR_MEB',0,ZHOOK_HANDLE)
!
PDISPH(:) = 0.
!
! Reynolds number:
!
ZREVEG(:) = ZUL*PLW(:)/ZNY
!
!
! Eq. B7, Sellers et.al. 1996:
!
ZCD(:)    = 0.
!
WHERE(ZREVEG>0.)
   ZCD(:) =1.328*2./sqrt(ZREVEG(:)) + 0.45*((1.-PCHIL(:))/XPI)**1.6
END WHERE
!
! Dispacement height, Eq. 20, Choudhury and Monteith, 1988:
!
PDISPH(:) = 1.1*PH_VEG(:)*ALOG(1.+(ZCD(:)*PLAIV(:))**0.25)
!
! SAFE! Displacement height + PZREF + XLIMH >= PH_VEG
!
PDISPH(:) = MAX(PDISPH(:),PH_VEG(:)+XLIMH-PZREF(:))
PDISPH(:) = MIN(PDISPH(:),PH_VEG(:)*ZFRTOP)
!
! SAFE assure that PH_VEG-DISPH > PZ0_MEBV+0.01 (see surface_air_meb)
PDISPH(:)=MIN(PDISPH(:),PH_VEG(:)-PZ0_MEBV(:)-0.01)
!
IF (LHOOK) CALL DR_HOOK('DISPH_FOR_MEB',1,ZHOOK_HANDLE)
!
END SUBROUTINE DISPH_FOR_MEB
