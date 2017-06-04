!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #####################
      MODULE MODD_GR_BIOG_GREENROOF_n
!     ######################
!
!!
!!!!****  *MODD_GR_BIOG_GREENROOF_n* - Declaration of variables for biogenic emissions 
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!    AUTHOR
!!    ------
!!      F. Solmon  *LA*
!!
!!    MODIFICATIONS
!!    -------------
!!    P. Tulet  30/07/03 externalisation of biogenics fluxes
!!     
!*       0.   DECLARATIONS
!             ----------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

TYPE GR_BIOG_GREENROOF_t
!
!
!
!* Emission potential for isoprene and monoterpenes 
  REAL, DIMENSION(:), POINTER :: XISOPOT 
  REAL, DIMENSION(:), POINTER :: XMONOPOT 
!
!* Radiation at different level(cf Gauss) in the canopy
  REAL, DIMENSION(:), POINTER :: XP_IACAN !pack radiation
  REAL, DIMENSION(:,:,:),POINTER ::XIACAN ! PAR at 3 gauss level for each patch
!
!* XFISO  = isoprene emission flux (ppp.m.s-1)
!  XFMONO = monoterpenes emission flux (ppp m s-1)
  REAL, DIMENSION(:), POINTER :: XFISO, XFMONO
!
!
END TYPE GR_BIOG_GREENROOF_t



CONTAINS

!




SUBROUTINE GR_BIOG_GREENROOF_INIT(YGR_BIOG_GREENROOF)
TYPE(GR_BIOG_GREENROOF_t), INTENT(INOUT) :: YGR_BIOG_GREENROOF
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_GR_BIOG_GREENROOF_N:GR_BIOG_GREENROOF_INIT",0,ZHOOK_HANDLE)
  NULLIFY(YGR_BIOG_GREENROOF%XISOPOT)
  NULLIFY(YGR_BIOG_GREENROOF%XMONOPOT)
  NULLIFY(YGR_BIOG_GREENROOF%XP_IACAN)
  NULLIFY(YGR_BIOG_GREENROOF%XIACAN)
  NULLIFY(YGR_BIOG_GREENROOF%XFISO)
  NULLIFY(YGR_BIOG_GREENROOF%XFMONO)
IF (LHOOK) CALL DR_HOOK("MODD_GR_BIOG_GREENROOF_N:GR_BIOG_GREENROOF_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE GR_BIOG_GREENROOF_INIT


END MODULE MODD_GR_BIOG_GREENROOF_n
