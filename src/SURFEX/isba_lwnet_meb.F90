!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##########################################################################             
      SUBROUTINE ISBA_LWNET_MEB(PLAI,PPSN,PPSNA,PEMIS_N,PEMIS_F,PFF,                          &
           PTV,PTG,PTN,PLW_RAD,PLWNET_N,PLWNET_V,PLWNET_G,                                    &
           PLWNET_V_DTV,PLWNET_V_DTG,PLWNET_V_DTN,                                            &
           PLWNET_G_DTV,PLWNET_G_DTG,PLWNET_G_DTN,                                            &
           PLWNET_N_DTV,PLWNET_N_DTG,PLWNET_N_DTN,                                            &
           PSIGMA_F,PSIGMA_FN,                                                                &
           PLWDOWN_GN                                                                         )

!
!!****  *ISBA_LWNET_MEB*  
!!
!!    PURPOSE
!!    -------
!
!     Calculates the net longwave radiation budget terms for fully
!     coupled snow, soil-understory vegetation and canopy vegetation.
!     Flux derrivatives also herein.
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
!!    REFERENCE
!!    ---------
!!
!!    Noilhan and Planton (1989)
!!    Belair (1995)
!!    * to be done * (2011)
!!      
!!    AUTHOR
!!    ------
!!
!!      A. Boone           * Meteo-France *
!!      P. Samuelsson      * SMHI *
!!      S. Gollvik         * SMHI * 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    22/01/11 
!!      A. Boone    04/03/17  add factor to dLWnet_n/dT_n term to cause it to vanish
!!                            under some extreme circumstances. 
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_PAR,       ONLY : XEMISSOIL, XEMISVEG
!
USE MODE_MEB,  ONLY : MEB_SHIELD_FACTOR
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:),   INTENT(IN)  :: PLAI, PPSN, PPSNA, PLW_RAD, PSIGMA_F
!
REAL, DIMENSION(:),   INTENT(IN)  :: PTV, PTG, PTN
!
REAL, DIMENSION(:),   INTENT(IN)  :: PEMIS_N, PEMIS_F, PFF
!
REAL, DIMENSION(:),   INTENT(OUT) :: PLWNET_N, PLWNET_V, PLWNET_G
!
REAL, DIMENSION(:),   INTENT(OUT) :: PLWDOWN_GN
!
REAL, DIMENSION(:),   INTENT(OUT) :: PLWNET_V_DTV, PLWNET_V_DTG, PLWNET_V_DTN
!                                     PLWNET_V_DTV, PLWNET_V_DTG, PLWNET_V_DTN = Vegetation canopy net radiation 
!                                     derivatives w/r/t surface temperature(s) (W m-2 K-1)
!
REAL, DIMENSION(:),   INTENT(OUT) :: PLWNET_G_DTV, PLWNET_G_DTG, PLWNET_G_DTN
!                                     PLWNET_G_DTV, PLWNET_G_DTG, PLWNET_G_DTN = Understory-ground net radiation 
!                                          derivatives w/r/t surface temperature(s) (W m-2 K-1)
!
REAL, DIMENSION(:),   INTENT(OUT) :: PLWNET_N_DTV, PLWNET_N_DTG, PLWNET_N_DTN
!                                     PLWNET_N_DTV, PLWNET_N_DTG, PLWNET_N_DTN = Ground-based snow net radiation 
!                                          derivatives w/r/t surface temperature(s) (W m-2 K-1)
!
REAL, DIMENSION(:),   INTENT(OUT) :: PSIGMA_FN
!
!*      0.2    declarations of local variables
!
!
REAL, DIMENSION(SIZE(PLAI)) :: ZLWUP
!
REAL, DIMENSION(SIZE(PLAI)) :: ZSIGMA_FA, ZPN, ZFRAC, ZEMIS
!
REAL, DIMENSION(SIZE(PLAI)) :: ZLW_G_A, ZLW_G_B, ZLW_G_C,            &
                               ZLW_G_D, ZLW_G_E, ZLW_G_F, ZLW_G_G,   &
                               ZLW_G_H, ZLW_G_I, ZLW_G_J, ZLW_G_K,   &
                               ZLW_G_L
!
REAL, DIMENSION(SIZE(PLAI)) :: ZLW_N_A, ZLW_N_B, ZLW_N_C,            &
                               ZLW_N_D, ZLW_N_E, ZLW_N_F, ZLW_N_G,   &
                               ZLW_N_H, ZLW_N_I, ZLW_N_J, ZLW_N_K,   &
                               ZLW_N_L
!
REAL, DIMENSION(SIZE(PEMIS_N)) :: ZEMIS_G
!
!*      0.3    declarations of local parameters
!
REAL, PARAMETER             :: ZTGRAD_MAX = 60. ! K When Tn-Tv approaches this difference, the derrivative term 
                                                !   dLWnet_n/dTn vanishes. See below for details.
REAL, PARAMETER             :: ZTGRAD_DIF = 10. ! K This is the range in Tn-Tv over which dLWnet_n/dTn 
                                                !   linearly vanishes. See below for details.
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*       0.     Initialization:
!               ---------------
!
IF (LHOOK) CALL DR_HOOK('ISBA_LWNET_MEB',0,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
! Soil with flooded part:
!
ZEMIS_G(:)    = XEMISSOIL*(1.-PFF(:)) + PFF(:)*PEMIS_F(:)
!
!*       1.     View factors: transmission
!               --------------------------
!
PSIGMA_FN(:)  = 1.0 - MEB_SHIELD_FACTOR(PLAI,PPSNA)          ! NOTE: Effects of intercepted snow on the 
!                                                            !       canopy is neglected.
!
!*       2.     Longwave radiation terms
!               ------------------------

! - over snow-free fraction:

ZFRAC(:)      = 1.-PPSN(:)
ZPN(:)        = PPSN(:)*(1.-PPSNA(:))
ZSIGMA_FA(: ) = (1.-ZPN(:))*PSIGMA_F(:) + ZPN(:)*PSIGMA_FN(:)

CALL LW_FLUX_COMP(ZPN,PLW_RAD,ZFRAC,PSIGMA_F,ZSIGMA_FA,       &
     ZEMIS_G,PTV,PTG,                                         & 
     ZLW_G_A,ZLW_G_B,ZLW_G_C,ZLW_G_D,ZLW_G_E,ZLW_G_F,         &
     ZLW_G_G,ZLW_G_H,ZLW_G_I,ZLW_G_J,ZLW_G_K,ZLW_G_L          )

! - over snow-covered fraction:

ZFRAC(:)      = PPSN(:)
ZPN(:)        = PPSN(:) + PPSNA(:)*(1.-PPSN(:))
ZSIGMA_FA(: ) = (1.-ZPN(:))*PSIGMA_F(:) + ZPN(:)*PSIGMA_FN(:)

CALL LW_FLUX_COMP(ZPN,PLW_RAD,ZFRAC,PSIGMA_FN,ZSIGMA_FA,      &
     PEMIS_N,PTV,PTN,                                         & 
     ZLW_N_A,ZLW_N_B,ZLW_N_C,ZLW_N_D,ZLW_N_E,ZLW_N_F,         &
     ZLW_N_G,ZLW_N_H,ZLW_N_I,ZLW_N_J,ZLW_N_K,ZLW_N_L          )

!------------------------------------------------------------------
! Diagnostics
!------------------------------------------------------------------

! Total LW energy flux reaching ground/snow surface: (W m-2)
! (explicit part: the implicit flux needs to have the derrivative terms added)

PLWDOWN_GN(:) = ZLW_G_C(:) + ZLW_G_F(:) + ZLW_N_C(:) + ZLW_N_F(:) + &
                ZLW_G_J(:) + ZLW_G_K(:) + ZLW_N_J(:) + ZLW_N_K(:)

!------------------------------------------------------------------
! - compute derivatives: W m-2 K-1
!------------------------------------------------------------------

PLWNET_V_DTV(:) = ( ZLW_G_G(:) - ZLW_G_H(:) - 2*ZLW_G_F(:)             &
                  + ZLW_N_G(:) - ZLW_N_H(:) - 2*ZLW_N_F(:)  )*4/PTV(:)
PLWNET_V_DTG(:) = ( ZLW_G_I(:) - ZLW_G_J(:) -   ZLW_G_K(:)             &
                  - ZLW_G_L(:)                              )*4/PTG(:)
PLWNET_V_DTN(:) = ( ZLW_N_I(:) - ZLW_N_J(:) -   ZLW_N_K(:)             &
                  - ZLW_N_L(:)                              )*4/PTN(:)

PLWNET_G_DTV(:) = ( ZLW_G_F(:) - ZLW_G_G(:)                 )*4/PTV(:)
PLWNET_G_DTG(:) = ( ZLW_G_J(:) - ZLW_G_I(:)                 )*4/PTG(:)
PLWNET_G_DTN(:) =   ZLW_N_J(:)                               *4/PTN(:)

PLWNET_N_DTV(:) = ( ZLW_N_F(:) - ZLW_N_G(:)                 )*4/PTV(:)
PLWNET_N_DTG(:) =   ZLW_G_K(:)                               *4/PTG(:)
PLWNET_N_DTN(:) = ( ZLW_N_K(:) - ZLW_N_I(:)                 )*4/PTN(:)
!
! Note that for very thin snow combined with extremely cold 
! conditions, very weak LWdown forcing and strong Tn-Tc differences,
! the derrivative of Lwnet_n/dT_n can pose problems (i.e. the assumption of
! a linear change over the given time step is a poor approximation), 
! thus in these rare instances, this derrivative is forced
! to vanish (over some small temperature difference range). This has no impact
! on numerical stability (since the LW linerization has no effect on this) and has
! no impact on results in the general sense (since it is temporary and the assumed gradieints
! are so large that they are rarely, if ever during a run, encountered).
! Use PTV as a proxy for air T (under these conditions, it should be close to PTC for example)

PLWNET_N_DTN(:) = PLWNET_N_DTN(:)*MIN(1., MAX(0., ZTGRAD_MAX-MAX(0.,PTN(:)-PTV(:)))/ZTGRAD_DIF)

!------------------------------------------------------------------
! - Compute *explicit* net budgets (at time t): W m-2
!   NOTE: fully implicit budgets (at time t+dt) are computed 
!   *after* energy budget
!------------------------------------------------------------------

ZLWUP(:)     =   ZLW_G_B(:) + ZLW_G_E(:) + ZLW_G_F(:) + ZLW_G_H(:)     &
               + ZLW_G_L(:)                                            &
               + ZLW_N_B(:) + ZLW_N_E(:) + ZLW_N_F(:) + ZLW_N_H(:)     &
               + ZLW_N_L(:)

PLWNET_G(:)  =   ZLW_G_C(:) + ZLW_G_F(:) + ZLW_G_J(:)                  &
               - ZLW_G_I(:) - ZLW_G_D(:) - ZLW_G_G(:)                  &
               + ZLW_N_J(:)

PLWNET_N(:)  =   ZLW_N_C(:) + ZLW_N_F(:) + ZLW_N_K(:)                  &
               - ZLW_N_I(:) - ZLW_N_D(:) - ZLW_N_G(:)                  &
               + ZLW_G_K(:)

PLWNET_V(:)  = PLW_RAD(:) - ZLWUP(:) - PLWNET_G(:) - PLWNET_N(:) 

IF (LHOOK) CALL DR_HOOK('ISBA_LWNET_MEB',1,ZHOOK_HANDLE)

CONTAINS
!=========================================================
SUBROUTINE LW_FLUX_COMP(PPN,PLW_RAD,PFRAC,PSIGMA_F,PSIGMA_FA,      &
     PEMIS_S,PTV,PTEMP_S,                                          & 
     PLW_A,PLW_B,PLW_C,PLW_D,PLW_E,PLW_F,PLW_G,PLW_H,              &
     PLW_I,PLW_J,PLW_K,PLW_L                                       )

USE MODD_CSTS,       ONLY : XSTEFAN

IMPLICIT NONE

REAL, DIMENSION(:),   INTENT(IN)  :: PPN, PLW_RAD, PSIGMA_F, PSIGMA_FA, PFRAC
REAL, DIMENSION(:),   INTENT(IN)  :: PTEMP_S, PTV
REAL, DIMENSION(:),   INTENT(IN)  :: PEMIS_S
REAL, DIMENSION(:),   INTENT(OUT) :: PLW_A, PLW_B, PLW_C, PLW_D, PLW_E, PLW_F, &
                                     PLW_G, PLW_H, PLW_I, PLW_J, PLW_K, PLW_L

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

REAL, DIMENSION(SIZE(PLW_RAD))    :: ZWORK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!--------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('LW_FLUX_COMP',0,ZHOOK_HANDLE)

PLW_A(:)      = PLW_RAD(:)*PFRAC(:)
PLW_B(:)      = PLW_A(:)*    PSIGMA_F(:)    *(1.-XEMISVEG)
PLW_C(:)      = PLW_A(:)*(1.-PSIGMA_F(:))
PLW_D(:)      = PLW_C(:)                    *(1.-PEMIS_S(:))
PLW_E(:)      = PLW_D(:)*(1.-PSIGMA_FA(:))

PLW_F(:)      =                PSIGMA_FA(:) *    XEMISVEG * PFRAC(:) *XSTEFAN*(PTV(:)**4)
PLW_G(:)      = PLW_F(:)                    *(1.-PEMIS_S(:))
PLW_H(:)      = PLW_G(:)*(1.-PSIGMA_FA(:))

ZWORK(:)      = (1.-XEMISVEG)*PSIGMA_FA(:)
PLW_I(:)      =                                  PEMIS_S(:) * PFRAC(:) *XSTEFAN*(PTEMP_S(:)**4)
PLW_J(:)      = PLW_I(:)*ZWORK(:)                           *(1.-PPN(:))
PLW_K(:)      = PLW_I(:)*ZWORK(:)*                               PPN(:)
PLW_L(:)      = PLW_I(:)*(1.-PSIGMA_FA(:))

IF (LHOOK) CALL DR_HOOK('LW_FLUX_COMP',1,ZHOOK_HANDLE)

END SUBROUTINE LW_FLUX_COMP
!=========================================================

END SUBROUTINE ISBA_LWNET_MEB
