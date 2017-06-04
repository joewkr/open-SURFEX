!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ################
      MODULE MODE_MEB
!     ################
!
!!****  *MODE_MEB * - contains multi-energy balance characteristics functions
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!    
!!    EXTERNAL
!!    --------
!!       
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      P. Samuelsson            * SMHI *
!!      A. Boone                 * Meteo France *       
!!      S. Gollvik               * SMHI *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original        18/01/11
!-----------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
!
INTERFACE MEBPALPHAN
  MODULE PROCEDURE MEBPALPHAN_3D
  MODULE PROCEDURE MEBPALPHAN_2D
  MODULE PROCEDURE MEBPALPHAN_1D
  MODULE PROCEDURE MEBPALPHAN_0D
END INTERFACE
!
INTERFACE SFC_HEATCAP_VEG
   MODULE PROCEDURE SFC_HEATCAP_VEG_2D
   MODULE PROCEDURE SFC_HEATCAP_VEG_1D
   MODULE PROCEDURE SFC_HEATCAP_VEG_0D
END INTERFACE
!
INTERFACE SWDOWN_DIFF
   MODULE PROCEDURE SWDOWN_DIFF_2D
   MODULE PROCEDURE SWDOWN_DIFF_1D
   MODULE PROCEDURE SWDOWN_DIFF_0D
END INTERFACE
!
INTERFACE SNOW_INTERCEPT_EFF
   MODULE PROCEDURE SNOW_INTERCEPT_EFF_2D
   MODULE PROCEDURE SNOW_INTERCEPT_EFF_1D
   MODULE PROCEDURE SNOW_INTERCEPT_EFF_0D
END INTERFACE
!
INTERFACE MEB_SHIELD_FACTOR
   MODULE PROCEDURE MEB_SHIELD_FACTOR_2D
   MODULE PROCEDURE MEB_SHIELD_FACTOR_1D
   MODULE PROCEDURE MEB_SHIELD_FACTOR_0D
END INTERFACE
!
!-------------------------------------------------------------------------------
CONTAINS
!
!####################################################################
!####################################################################
!####################################################################
      FUNCTION MEBPALPHAN_3D(PSNOWDEPTH,PH_VEG) RESULT(PPALPHAN)
!
!!    PURPOSE
!!    -------
!     Calculation of p_alphan, the snow/canopy transition coefficient
!     0 for snow at canopy base
!     1 for snow at canopy top
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:,:), INTENT(IN)     :: PSNOWDEPTH,PH_VEG
!
REAL, DIMENSION(SIZE(PSNOWDEPTH,1),SIZE(PSNOWDEPTH,2),SIZE(PSNOWDEPTH,3)) :: PPALPHAN
!
!*      0.2    declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
REAL, DIMENSION(SIZE(PSNOWDEPTH,1),SIZE(PSNOWDEPTH,2),SIZE(PSNOWDEPTH,3))   :: ZH_BASEVEG  ! height of the base of the canopy
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_MEB:MEBPALPHAN_3D',0,ZHOOK_HANDLE)
!
! multi-energy balance: Maybe the calculation of ZH_BASEVEG should be modified.
!WHERE(PH_VEG>1.)
!  ZH_BASEVEG(:,:,:)=0.3*PH_VEG(:,:,:)
!ELSEWHERE
!  ZH_BASEVEG(:,:,:)=0.
!END WHERE
ZH_BASEVEG(:,:,:)=MAX(0.2*(PH_VEG(:,:,:)-2.0),0.0);
!
PPALPHAN(:,:,:)=MIN(1.,MAX(0., (PSNOWDEPTH(:,:,:)-ZH_BASEVEG(:,:,:))/(PH_VEG(:,:,:)-ZH_BASEVEG(:,:,:)) ))
!
IF (LHOOK) CALL DR_HOOK('MODE_MEB:MEBPALPHAN_3D',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END FUNCTION MEBPALPHAN_3D
!####################################################################
!####################################################################
!####################################################################
      FUNCTION MEBPALPHAN_2D(PSNOWDEPTH,PH_VEG) RESULT(PPALPHAN)
!
!!    PURPOSE
!!    -------
!     Calculation of p_alphan, the snow/canopy transition coefficient
!     0 for snow at canopy base
!     1 for snow at canopy top
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:), INTENT(IN)     :: PSNOWDEPTH,PH_VEG
!
REAL, DIMENSION(SIZE(PSNOWDEPTH,1),SIZE(PSNOWDEPTH,2)) :: PPALPHAN
!
!*      0.2    declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
REAL, DIMENSION(SIZE(PSNOWDEPTH,1),SIZE(PSNOWDEPTH,2))   :: ZH_BASEVEG  ! height of the base of the canopy
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_MEB:MEBPALPHAN_2D',0,ZHOOK_HANDLE)
!
!WHERE(PH_VEG>1.)
!  ZH_BASEVEG(:,:)=0.3*PH_VEG(:,:)
!ELSEWHERE
!  ZH_BASEVEG(:,:)=0.
!END WHERE
ZH_BASEVEG(:,:)=MAX(0.2*(PH_VEG(:,:)-2.0),0.0);
!
PPALPHAN(:,:)=MIN(1.,MAX(0., (PSNOWDEPTH(:,:)-ZH_BASEVEG(:,:))/(PH_VEG(:,:)-ZH_BASEVEG(:,:)) ))
!
IF (LHOOK) CALL DR_HOOK('MODE_MEB:MEBPALPHAN_2D',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END FUNCTION MEBPALPHAN_2D
!####################################################################
!####################################################################
!####################################################################
      FUNCTION MEBPALPHAN_1D(PSNOWDEPTH,PH_VEG) RESULT(PPALPHAN)
!
!!    PURPOSE
!!    -------
!     Calculation of p_alphan, the snow/canopy transition coefficient
!     0 for snow at canopy base
!     1 for snow at canopy top
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:), INTENT(IN)     :: PSNOWDEPTH,PH_VEG
!
REAL, DIMENSION(SIZE(PSNOWDEPTH,1)) :: PPALPHAN
!
!*      0.2    declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
REAL, DIMENSION(SIZE(PSNOWDEPTH,1))   :: ZH_BASEVEG  ! height of the base of the canopy
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_MEB:MEBPALPHAN_1D',0,ZHOOK_HANDLE)
!
!WHERE(PH_VEG>1.)
!  ZH_BASEVEG(:)=0.3*PH_VEG(:)
!ELSEWHERE
!  ZH_BASEVEG(:)=0.
!END WHERE
ZH_BASEVEG(:)=MAX(0.2*(PH_VEG(:)-2.0),0.0);
!
!
PPALPHAN(:)=MIN(1.,MAX(0., (PSNOWDEPTH(:)-ZH_BASEVEG(:))/(PH_VEG(:)-ZH_BASEVEG(:)) ))
!
IF (LHOOK) CALL DR_HOOK('MODE_MEB:MEBPALPHAN_1D',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END FUNCTION MEBPALPHAN_1D
!####################################################################
!####################################################################
!####################################################################
      FUNCTION MEBPALPHAN_0D(PSNOWDEPTH,PH_VEG) RESULT(PPALPHAN)
!
!!    PURPOSE
!!    -------
!     Calculation of p_alphan, the snow/canopy transition coefficient
!     0 for snow at canopy base
!     1 for snow at canopy top
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)     :: PSNOWDEPTH,PH_VEG
!
REAL :: PPALPHAN
!
!*      0.2    declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
REAL :: ZH_BASEVEG  ! height of the base of the canopy
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_MEB:MEBPALPHAN_0D',0,ZHOOK_HANDLE)
!
!IF(PH_VEG>1.)THEN
!  ZH_BASEVEG=0.3*PH_VEG
!ELSE
!  ZH_BASEVEG=0.
!ENDIF
ZH_BASEVEG=MAX(0.2*(PH_VEG-2.0),0.0);
!
PPALPHAN=MIN(1.,MAX(0., (PSNOWDEPTH-ZH_BASEVEG)/(PH_VEG-ZH_BASEVEG) ))
!
IF (LHOOK) CALL DR_HOOK('MODE_MEB:MEBPALPHAN_0D',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END FUNCTION MEBPALPHAN_0D
!####################################################################
!####################################################################
!####################################################################
FUNCTION SFC_HEATCAP_VEG_0D(PWRN,PWR,PCV) RESULT(ZCHEATV)

! Compute the bulk heat capacity of the vegetation canopy

USE MODD_CSTS,     ONLY : XCL, XCI
USE MODD_ISBA_PAR, ONLY : XCVHEATF

USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

!*      0.1    declarations of arguments
!
REAL,               INTENT(IN)  :: PWRN, PWR, PCV
!                                  PWRN      = Liquid water equivalent mass of intercepted snow (kg m-2)
!                                  PCV       = Thermal inertia of the vegetation (m2 K J-1)
!                                  PWR       = Liquid water mass intercepted (kg m-2)
!
!*      0.2    declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
REAL                            :: ZCHEATV
!                                  ZCHEATV = Total bulk Vegetation canopy heat capacity (J m-2 K-1)
!
!*      0.3    declarations of local parameters
!
REAL, PARAMETER                 :: ZCHEATVMIN = 1.E+4 ! minimum limit (J m-2 K-1)
!
!------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_MEB:SFC_HEATCAP_VEG_0D',0,ZHOOK_HANDLE)
!
! Total bulk canopy heat capacity
! Method: we use the ratio of biomass to LAI to get a total biomass estimate,
! next assume that the dry biomass heat capacity is small compared to that
! of the water contained in the vegetation to arrive at the vegetation part, then
! we add the heat capacities of intercepted liquid and frozen water.
! Finally, use a minimum value to avoid numerical jumps, while still ensuring that
! the heat capacity of the vegetation is generally < a typical daily restore for the soil

ZCHEATV   = MAX(ZCHEATVMIN,XCVHEATF/PCV)           +   & ! stems, branches, trunk...
               XCI*PWRN                            +   & ! intercepted snow
               XCL*PWR                                   ! intercepted water

IF (LHOOK) CALL DR_HOOK('MODE_MEB:SFC_HEATCAP_VEG_0D',1,ZHOOK_HANDLE)

END FUNCTION SFC_HEATCAP_VEG_0D
!####################################################################
!####################################################################
!####################################################################
FUNCTION SFC_HEATCAP_VEG_1D(PWRN,PWR,PCV) RESULT(ZCHEATV)

! Compute the bulk heat capacity of the vegetation canopy

USE MODD_CSTS,     ONLY : XCL, XCI
USE MODD_ISBA_PAR, ONLY : XCVHEATF

USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

!*      0.1    declarations of arguments
!
REAL, DIMENSION(:), INTENT(IN)  :: PWRN, PWR, PCV
!                                  PWRN      = Liquid water equivalent mass of intercepted snow (kg m-2)
!                                  PCV       = Thermal inertia of the vegetation (m2 K J-1)
!                                  PWR       = Liquid water mass intercepted (kg m-2)
!
!*      0.2    declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
REAL, DIMENSION(SIZE(PCV))     :: ZCHEATV
!                                  ZCHEATV = Total bulk Vegetation canopy heat capacity (J m-2 K-1)
!
!*      0.3    declarations of local parameters
!
REAL, PARAMETER                 :: ZCHEATVMIN = 1.E+4 ! minimum limit (J m-2 K-1)
!
!------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_MEB:SFC_HEATCAP_VEG_1D',0,ZHOOK_HANDLE)
!
! Total bulk canopy heat capacity
! Method: we use the ratio of biomass to LAI to get a total biomass estimate,
! next assume that the dry biomass heat capacity is small compared to that
! of the water contained in the vegetation to arrive at the vegetation part, then
! we add the heat capacities of intercepted liquid and frozen water.
! Finally, use a minimum value to avoid numerical jumps, while still ensuring that
! the heat capacity of the vegetation is generally < a typical daily restore for the soil

ZCHEATV(:)   = MAX(ZCHEATVMIN,XCVHEATF/PCV(:))        +   & ! stems, branches, trunk...
               XCI*PWRN(:)                            +   & ! intercepted snow
               XCL*PWR(:)                                   ! intercepted water

IF (LHOOK) CALL DR_HOOK('MODE_MEB:SFC_HEATCAP_VEG_1D',1,ZHOOK_HANDLE)

END FUNCTION SFC_HEATCAP_VEG_1D
!####################################################################
!####################################################################
!####################################################################
FUNCTION SFC_HEATCAP_VEG_2D(PWRN,PWR,PCV) RESULT(ZCHEATV)

! Compute the bulk heat capacity of the vegetation canopy

USE MODD_CSTS,     ONLY : XCL, XCI
USE MODD_ISBA_PAR, ONLY : XCVHEATF

USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:), INTENT(IN) :: PWRN, PWR, PCV
!                                   PWRN      = Liquid water equivalent mass of intercepted snow (kg m-2)
!                                   PCV       = Thermal inertia of the vegetation (m2 K J-1)
!                                   PWR       = Liquid water mass intercepted (kg m-2)
!
!*      0.2    declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
REAL, DIMENSION(SIZE(PCV),SIZE(PCV,2)) :: ZCHEATV
!                                  ZCHEATV = Total bulk Vegetation canopy heat capacity (J m-2 K-1)
!
!*      0.3    declarations of local parameters
!
REAL, PARAMETER                 :: ZCHEATVMIN = 1.E+4 ! minimum limit (J m-2 K-1)
!
!------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_MEB:SFC_HEATCAP_VEG_2D',0,ZHOOK_HANDLE)
!
! Total bulk canopy heat capacity
! Method: we use the ratio of biomass to LAI to get a total biomass estimate,
! next assume that the dry biomass heat capacity is small compared to that
! of the water contained in the vegetation to arrive at the vegetation part, then
! we add the heat capacities of intercepted liquid and frozen water.
! Finally, use a minimum value to avoid numerical jumps, while still ensuring that
! the heat capacity of the vegetation is generally < a typical daily restore for the soil

ZCHEATV(:,:) = MAX(ZCHEATVMIN,XCVHEATF/PCV(:,:))      +   & ! stems, branches, trunk...
               XCI*PWRN(:,:)                          +   & ! intercepted snow
               XCL*PWR(:,:)                                 ! intercepted water

IF (LHOOK) CALL DR_HOOK('MODE_MEB:SFC_HEATCAP_VEG_2D',1,ZHOOK_HANDLE)

END FUNCTION SFC_HEATCAP_VEG_2D
!####################################################################
!####################################################################
!####################################################################
!####################################################################
!####################################################################
!####################################################################
FUNCTION SWDOWN_DIFF_2D(PSW_RAD,PCOSZENITH) RESULT(ZSWDOWN_DIFF)

! Based on incoming total rad (direct and diffuse) and cosine of the
! solar zenith angle, compute the fraction of that rad which is diffuse.
! D.G. Erbs, S.A. Klein and J.A. Duffie, 1982:
! Estimation of the diffuse radiation fraction for hourly, daily and monthly-average global radiation.
! Solar Energy, 28(4), 293-302.
!
! Author: A. Boone CNRM-GAME

USE MODD_CSTS, ONLY : XI0

USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

!*      0.1    declarations of arguments
!
REAL,    DIMENSION(:,:), INTENT(IN)                 :: PSW_RAD 
                                                             
REAL,    DIMENSION(:,:), INTENT(IN)                 :: PCOSZENITH 

!
!*      0.2    declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
REAL,    DIMENSION(SIZE(PSW_RAD,1),SIZE(PSW_RAD,2)) :: ZSWDOWN_DIFF, ZRATIO

!*      0.3    declarations of local parameters

REAL, PARAMETER                                     :: ZSWCNT = 1.0 ! 1.000 if all wavelength SWdown 
                                                                    ! 2.083 (=1/0.48) if just visible
!--------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_MEB:SWDOWN_DIFF_2D',0,ZHOOK_HANDLE)

ZRATIO(:,:)       = PSW_RAD(:,:)*(ZSWCNT/XI0)/MAX(0.01,PCOSZENITH(:,:))

! Numerical Check:

ZRATIO(:,:)       = MIN(1.0, ZRATIO(:,:))                                 

ZSWDOWN_DIFF(:,:) = 0.165 ! RATIO >= 0.8

WHERE(ZRATIO(:,:) <  0.22 )                                        &
     ZSWDOWN_DIFF(:,:) = 1.0 - 0.09*ZRATIO(:,:)

WHERE(ZRATIO(:,:) >= 0.22 .AND. ZRATIO(:,:) < 0.80)                &
     ZSWDOWN_DIFF(:,:) = 0.9511 + (-0.1604 + (4.388 + (-16.64 +    &
     12.34*ZRATIO(:,:))*ZRATIO(:,:))*ZRATIO(:,:))*ZRATIO(:,:)

IF (LHOOK) CALL DR_HOOK('MODE_MEB:SWDOWN_DIFF_2D',1,ZHOOK_HANDLE)

END FUNCTION SWDOWN_DIFF_2D
!####################################################################
!####################################################################
!####################################################################
FUNCTION SWDOWN_DIFF_1D(PSW_RAD,PCOSZENITH) RESULT(ZSWDOWN_DIFF)

! Based on incoming total rad (direct and diffuse) and cosine of the
! solar zenith angle, compute the fraction of that rad which is diffuse.
! D.G. Erbs, S.A. Klein and J.A. Duffie, 1982:
! Estimation of the diffuse radiation fraction for hourly, daily and monthly-average global radiation.
! Solar Energy, 28(4), 293-302.
!
! Author: A. Boone CNRM-GAME

USE MODD_CSTS, ONLY : XI0

USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

!*      0.1    declarations of arguments
!
REAL,    DIMENSION(:), INTENT(IN)                 :: PSW_RAD 
                                                             
REAL,    DIMENSION(:), INTENT(IN)                 :: PCOSZENITH 

!
!*      0.2    declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
REAL,    DIMENSION(SIZE(PSW_RAD))                 :: ZSWDOWN_DIFF, ZRATIO

!*      0.3    declarations of local parameters

REAL, PARAMETER                                   :: ZSWCNT = 1.0 ! 1.000 if all wavelength SWdown 
                                                                  ! 2.083 (=1/0.48) if just visible
!
!--------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_MEB:SWDOWN_DIFF_1D',0,ZHOOK_HANDLE)

ZRATIO(:) = PSW_RAD(:)*(ZSWCNT/XI0)/MAX(0.01,PCOSZENITH(:))

! Numerical Check:

ZRATIO(:) = MIN(1.0, ZRATIO(:))                                 

ZSWDOWN_DIFF(:) = 0.165 ! RATIO >= 0.8

WHERE(ZRATIO(:) <  0.22 )                                      &
     ZSWDOWN_DIFF(:) = 1.0 - 0.09*ZRATIO(:)

WHERE(ZRATIO(:) >= 0.22 .AND. ZRATIO(:) < 0.80)                &
     ZSWDOWN_DIFF(:) = 0.9511 + (-0.1604 + (4.388 + (-16.64 +  &
     12.34*ZRATIO(:))*ZRATIO(:))*ZRATIO(:))*ZRATIO(:)

IF (LHOOK) CALL DR_HOOK('MODE_MEB:SWDOWN_DIFF_1D',1,ZHOOK_HANDLE)

END FUNCTION SWDOWN_DIFF_1D
!####################################################################
!####################################################################
!####################################################################
FUNCTION SWDOWN_DIFF_0D(PSW_RAD,PCOSZENITH) RESULT(ZSWDOWN_DIFF)

! Based on incoming total rad (direct and diffuse) and cosine of the
! solar zenith angle, compute the fraction of that rad which is diffuse.
! D.G. Erbs, S.A. Klein and J.A. Duffie, 1982:
! Estimation of the diffuse radiation fraction for hourly, daily and monthly-average global radiation.
! Solar Energy, 28(4), 293-302.
!
! Author: A. Boone CNRM-GAME

USE MODD_CSTS, ONLY : XI0

USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

!*      0.1    declarations of arguments
!
REAL, INTENT(IN)                 :: PSW_RAD 
                                                             
REAL, INTENT(IN)                 :: PCOSZENITH 

!
!*      0.2    declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
REAL                             :: ZSWDOWN_DIFF, ZRATIO
!
!*      0.3    declarations of local parameters
!
REAL, PARAMETER                  :: ZSWCNT = 1.0 ! 1.000 if all wavelength SWdown 
                                                                  ! 2.083 (=1/0.48) if just visible
!--------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_MEB:SWDOWN_DIFF_0D',0,ZHOOK_HANDLE)

ZRATIO       = PSW_RAD*(ZSWCNT/XI0)/MAX(0.01,PCOSZENITH)

! Numerical Check:

ZRATIO       = MIN(1.0, ZRATIO)                                 

ZSWDOWN_DIFF = 0.165 ! RATIO >= 0.8

IF    (ZRATIO <  0.22 )THEN
     ZSWDOWN_DIFF = 1.0 - 0.09*ZRATIO
ELSEIF(ZRATIO >= 0.22 .AND. ZRATIO < 0.80)THEN
     ZSWDOWN_DIFF = 0.9511 + (-0.1604 + (4.388 + (-16.64 +  &
                    12.34*ZRATIO)*ZRATIO)*ZRATIO)*ZRATIO
ENDIF

IF (LHOOK) CALL DR_HOOK('MODE_MEB:SWDOWN_DIFF_0D',1,ZHOOK_HANDLE)

END FUNCTION SWDOWN_DIFF_0D
!####################################################################
!####################################################################
!####################################################################
FUNCTION SNOW_INTERCEPT_EFF_0D(PCHIP,PVELC,PWRVNMAX) RESULT(ZKVN)
!!
!! Calculate snow interception efficiency.
!!
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!      P. Samuelsson           * SMHI *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       02/2011
!!      (A. Boone)  08/08/2011 Transform from subroutine to function
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,               INTENT(IN)   :: PCHIP, PVELC
!                                     PCHIP      = view factor (for LW) 
!                                     PVELC      = wind speed at top of vegetation
!
REAL,               INTENT(IN)   :: PWRVNMAX
!                                     PWRVNMAX   = maximum equivalent snow content
!                                                  in the canopy vegetation
!
!*      0.2    declarations of local variables
!
!
REAL                             :: ZKVN
!                                     ZKVN       = Snow interception efficiency
!                                                  coefficient.
!                                                  Note: if this is set=0 it means
!                                                  that snow interception is shut
!                                                  off.
!
REAL                             :: ZFCP
!                                     ZFCP       = snow interception factor
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      0.3    declarations of local parameters
!
REAL, PARAMETER             :: ZWSNOW        = 0.8        ! Snow fall velocity (m/s)
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_MEB:SNOW_INTERCEPT_EFF_0D',0,ZHOOK_HANDLE)
!
! Initialization:
!
ZFCP        = 0.0 
ZKVN        = 0.0
!
! Snow interception efficiency
!
IF(PWRVNMAX > 0.0)THEN
   ZFCP     = MIN(1.,MAX(0., PVELC/((2*ZWSNOW)*PCHIP) ) )
   ZKVN     = (1.-PCHIP+ZFCP*PCHIP)/PWRVNMAX
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_MEB:SNOW_INTERCEPT_EFF_0D',1,ZHOOK_HANDLE)
END FUNCTION SNOW_INTERCEPT_EFF_0D
!####################################################################
!####################################################################
!####################################################################
FUNCTION SNOW_INTERCEPT_EFF_1D(PCHIP,PVELC,PWRVNMAX) RESULT(ZKVN)
!!
!! Calculate snow interception efficiency.
!!
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!      P. Samuelsson           * SMHI *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       02/2011
!!      (A. Boone)  08/08/2011 Transform from subroutine to function
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:), INTENT(IN)   :: PCHIP, PVELC
!                                     PCHIP      = view factor (for LW) 
!                                     PVELC      = wind speed at top of vegetation
!
REAL, DIMENSION(:), INTENT(IN)   :: PWRVNMAX
!                                     PWRVNMAX   = maximum equivalent snow content
!                                                  in the canopy vegetation
!
!*      0.2    declarations of local variables
!
!
REAL, DIMENSION(SIZE(PVELC))     :: ZKVN
!                                     ZKVN       = Snow interception efficiency
!                                                  coefficient.
!                                                  Note: if this is set=0 it means
!                                                  that snow interception is shut
!                                                  off.
!
REAL, DIMENSION(SIZE(PVELC))     :: ZFCP
!                                     ZFCP       = snow interception factor
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      0.3    declarations of local parameters
!
REAL, PARAMETER                  :: ZWSNOW       = 0.8        ! Snow fall velocity (m/s)
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_MEB:SNOW_INTERCEPT_EFF_1D',0,ZHOOK_HANDLE)
!
! Initialization:
!
ZFCP(:)        = 0.0 
ZKVN(:)        = 0.0
!
! Snow interception efficiency
!
WHERE(PWRVNMAX(:) > 0.0)
   ZFCP(:)     = MIN(1.,MAX(0., PVELC(:)/((2*ZWSNOW)*PCHIP(:)) ) )
   ZKVN(:)     = (1.-PCHIP(:)+ZFCP(:)*PCHIP(:))/PWRVNMAX(:)
END WHERE
!
IF (LHOOK) CALL DR_HOOK('MODE_MEB:SNOW_INTERCEPT_EFF_1D',1,ZHOOK_HANDLE)
END FUNCTION SNOW_INTERCEPT_EFF_1D
!####################################################################
!####################################################################
!####################################################################
FUNCTION SNOW_INTERCEPT_EFF_2D(PCHIP,PVELC,PWRVNMAX) RESULT(ZKVN)
!!
!! Calculate snow interception efficiency.
!!
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!      P. Samuelsson           * SMHI *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       02/2011
!!      (A. Boone)  08/08/2011 Transform from subroutine to function
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:), INTENT(IN)   :: PCHIP, PVELC
!                                     PCHIP      = view factor (for LW) 
!                                     PVELC      = wind speed at top of vegetation
!
REAL, DIMENSION(:,:), INTENT(IN)   :: PWRVNMAX
!                                     PWRVNMAX   = maximum equivalent snow content
!                                                  in the canopy vegetation
!
!*      0.2    declarations of local variables
!
!
REAL, DIMENSION(SIZE(PVELC,1),SIZE(PVELC,2)) :: ZKVN
!                                     ZKVN       = Snow interception efficiency
!                                                  coefficient.
!                                                  Note: if this is set=0 it means
!                                                  that snow interception is shut
!                                                  off.
!
REAL, DIMENSION(SIZE(PVELC,1),SIZE(PVELC,2)) :: ZFCP
!                                     ZFCP       = snow interception factor
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      0.3    declarations of local parameters
!
REAL, PARAMETER                  :: ZWSNOW       = 0.8        ! Snow fall velocity (m/s)
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_MEB:SNOW_INTERCEPT_EFF_2D',0,ZHOOK_HANDLE)
!
! Initialization:
!
ZFCP(:,:)     = 0.0 
ZKVN(:,:)     = 0.0
!
! Snow interception efficiency
!
WHERE(PWRVNMAX(:,:) > 0.0)
   ZFCP(:,:)  = MIN(1.,MAX(0., PVELC(:,:)/((2*ZWSNOW)*PCHIP(:,:)) ) )
   ZKVN(:,:)  = (1.-PCHIP(:,:)+ZFCP(:,:)*PCHIP(:,:))/PWRVNMAX(:,:)
END WHERE
!
IF (LHOOK) CALL DR_HOOK('MODE_MEB:SNOW_INTERCEPT_EFF_2D',1,ZHOOK_HANDLE)
END FUNCTION SNOW_INTERCEPT_EFF_2D
!####################################################################
!####################################################################
!####################################################################
FUNCTION MEB_SHIELD_FACTOR_0D(PLAI,PPALPHAN) RESULT(PCHIP)
!!
!! Calculate MEB shielding factor
!!
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!  A. Boone                * Meteo France *       
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       02/2011
!!      P. Samuelsson  07/2014 Transform from subroutine to function
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_MEB_PAR,   ONLY : XTAU_LW
!
USE YOMHOOK,        ONLY : LHOOK,   DR_HOOK
USE PARKIND1,       ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,               INTENT(IN)   :: PLAI, PPALPHAN
!                                   PWRVNMAX   = canopy vegetation leaf area index
!                                   PPALPHAN   = snow/canopy transition coefficient
!
REAL                             :: PCHIP
!                                   PCHIP      = shielding factor (for LW) 
!
!*      0.2    declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_MEB:MEB_SHIELD_FACTOR_0D',0,ZHOOK_HANDLE)
!
PCHIP = EXP(-XTAU_LW*PLAI*(1.-PPALPHAN))
!
IF (LHOOK) CALL DR_HOOK('MODE_MEB:MEB_SHIELD_FACTOR_0D',1,ZHOOK_HANDLE)
END FUNCTION MEB_SHIELD_FACTOR_0D
!####################################################################
!####################################################################
!####################################################################
FUNCTION MEB_SHIELD_FACTOR_1D(PLAI,PPALPHAN) RESULT(PCHIP)
!!
!! Calculate MEB shielding factor
!!
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!  A. Boone                * Meteo France *       
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       02/2011
!!      P. Samuelsson  07/2014 Transform from subroutine to function
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_MEB_PAR,   ONLY : XTAU_LW
!
USE YOMHOOK,        ONLY : LHOOK,   DR_HOOK
USE PARKIND1,       ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:), INTENT(IN)   :: PLAI, PPALPHAN
!                                   PWRVNMAX   = canopy vegetation leaf area index
!                                   PPALPHAN   = snow/canopy transition coefficient
!
REAL, DIMENSION(SIZE(PLAI,1))    :: PCHIP
!                                   PCHIP      = shielding factor (for LW) 
!
!*      0.2    declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_MEB:MEB_SHIELD_FACTOR_1D',0,ZHOOK_HANDLE)
!
PCHIP(:) = EXP(-XTAU_LW*PLAI(:)*(1.-PPALPHAN(:)))
!
IF (LHOOK) CALL DR_HOOK('MODE_MEB:MEB_SHIELD_FACTOR_1D',1,ZHOOK_HANDLE)
END FUNCTION MEB_SHIELD_FACTOR_1D
!####################################################################
!####################################################################
!####################################################################
FUNCTION MEB_SHIELD_FACTOR_2D(PLAI,PPALPHAN) RESULT(PCHIP)
!!
!! Calculate MEB shielding factor
!!
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!  A. Boone                * Meteo France *       
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       02/2011
!!      P. Samuelsson  07/2014 Transform from subroutine to function
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_MEB_PAR,   ONLY : XTAU_LW
!
USE YOMHOOK,        ONLY : LHOOK,   DR_HOOK
USE PARKIND1,       ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:), INTENT(IN)   :: PLAI, PPALPHAN
!                                     PWRVNMAX   = canopy vegetation leaf area index
!                                     PPALPHAN   = snow/canopy transition coefficient
!
REAL, DIMENSION(SIZE(PLAI,1),SIZE(PLAI,2))    :: PCHIP
!                                                PCHIP      = shielding factor (for LW) 
!
!*      0.2    declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_MEB:MEB_SHIELD_FACTOR_2D',0,ZHOOK_HANDLE)
!
PCHIP(:,:) = EXP(-XTAU_LW*PLAI(:,:)*(1.-PPALPHAN(:,:)))
!
IF (LHOOK) CALL DR_HOOK('MODE_MEB:MEB_SHIELD_FACTOR_2D',1,ZHOOK_HANDLE)
END FUNCTION MEB_SHIELD_FACTOR_2D
!####################################################################
!####################################################################
!####################################################################
END MODULE MODE_MEB
