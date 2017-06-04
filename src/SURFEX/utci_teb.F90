!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!   ##########################################################################
SUBROUTINE UTCI_TEB(T, DUT, PTI_BLD, PQI_BLD, PU10, PPS, PREF_SW_GRND, PREF_SW_FAC, &
                    PSCA_SW, PDIR_SW, PZENITH, PEMIT_LW_FAC, PEMIT_LW_GRND, PLW_RAD,&
                    PTRAD_IN )
!   ##########################################################################
!
!!****  *UTCI_TEB*  
!!
!!    PURPOSE
!!    -------
!
!     Computes the Universal Thermal and Climate Index Equivalent temperature
!     for 3 persons in the urban environment
!         
!     
!!**  METHOD
!     ------
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!! a supplement
!!    MODD_CST
!!
!!    REFERENCE
!!    ---------
!!   www.utci.org
!!      
!!    AUTHOR
!!    ------
!!
!!      G. Pigeon           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original  03/2011
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_DIAG_UTCI_TEB_n, ONLY : DIAG_UTCI_TEB_t
USE MODD_TEB_n, ONLY : TEB_t
!
USE MODD_CSTS, ONLY : XTT
USE MODI_UTCI_APPROX
USE MODI_TRAD_BODY
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
TYPE(TEB_t), INTENT(INOUT) :: T
TYPE(DIAG_UTCI_TEB_t), INTENT(INOUT) :: DUT
!
REAL, DIMENSION(:), INTENT(IN)  :: PTI_BLD !Indoor air temperature (K) 
REAL, DIMENSION(:), INTENT(IN)  :: PQI_BLD !Indoor specific humidity (kg/kg) 
REAL, DIMENSION(:), INTENT(IN)  :: PU10 !Canyon wind speed at 10m (m/s)
REAL, DIMENSION(:), INTENT(IN)  :: PPS !Atmospheric Pressure (Pa)
REAL, DIMENSION(:), INTENT(IN)  :: PREF_SW_GRND !Solar radiation reflected by ground [road + garden] (W/m2)
REAL, DIMENSION(:), INTENT(IN)  :: PREF_SW_FAC !Solar radiation reflected by facade [wall + glazing] (W/m2)
REAL, DIMENSION(:), INTENT(IN)  :: PSCA_SW !Diffuse solar radiation (W/m2)
REAL, DIMENSION(:), INTENT(IN)  :: PDIR_SW !Direct solar radiation (W/m2)
REAL, DIMENSION(:), INTENT(IN)  :: PZENITH !solar zenithal angle (rad from vert.)
REAL, DIMENSION(:), INTENT(IN)  :: PEMIT_LW_FAC !Longwave radiation emitted by the facade [wall + glazing] (W/m2)
REAL, DIMENSION(:), INTENT(IN)  :: PEMIT_LW_GRND !Longwave radiation emitted by the ground [road + garden] (W/m2)
REAL, DIMENSION(:), INTENT(IN)  :: PLW_RAD !Atmospheric longwave radiation (W/m2)
REAL, DIMENSION(:), INTENT(IN)  :: PTRAD_IN !Indoor radiant temperature (K)
!
!*      0.2    declarations of local variables
REAL, DIMENSION(SIZE(PTI_BLD)) :: ZEHPA !water vapour pressure (hPa)
REAL, DIMENSION(SIZE(PTI_BLD)) :: ZUIN !indoor air wind speed (m/s)
REAL, DIMENSION(SIZE(PTI_BLD)) :: ZDIR_SW !direct solar radiation
REAL, DIMENSION(SIZE(PTI_BLD)) :: ZZENITH !zenithal angle
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('UTCI_TEB',0,ZHOOK_HANDLE)
! 1-calculation of UTCI_IN
ZEHPA = PQI_BLD * PPS /(0.622 + 0.378 * PQI_BLD) / 100.
ZUIN = 0.5
DUT%XUTCI_IN = UTCI_APPROX(PTI_BLD - XTT, ZEHPA, PTRAD_IN - XTT, ZUIN)
!
! 2-calculation of UTCI_OUTSUN
ZEHPA = T%XQ_CANYON * PPS / (0.622 + 0.378 * T%XQ_CANYON) /100.
DUT%XTRAD_SUN = TRAD_BODY(PSCA_SW, PREF_SW_FAC, PREF_SW_GRND, PEMIT_LW_FAC, PEMIT_LW_GRND, PLW_RAD, &
                          T%XBLD, T%XBLD_HEIGHT, T%XWALL_O_HOR, PDIR_SW, PZENITH )
DUT%XUTCI_OUTSUN = UTCI_APPROX(T%XT_CANYON - XTT, ZEHPA, DUT%XTRAD_SUN - XTT, PU10)
!
! 3-calculation of UTCI_OUTSHADE
ZDIR_SW=0.
ZZENITH=0.
!!DUT%XTRAD_SHADE = TRAD_BODY(PSCA_SW,PREF_SW_FAC, PREF_SW_GRND, &
!!                      PEMIT_LW_FAC, PEMIT_LW_GRND, PLW_RAD,&
!!                      T%XBLD, T%XBLD_HEIGHT, T%XWALL_O_HOR, ZDIR_SW, ZZENITH)
DUT%XTRAD_SHADE = TRAD_BODY(PSCA_SW,PREF_SW_FAC, PREF_SW_GRND, PEMIT_LW_FAC, PEMIT_LW_GRND, PLW_RAD,&
                            T%XBLD, T%XBLD_HEIGHT, T%XWALL_O_HOR)
DUT%XUTCI_OUTSHADE = UTCI_APPROX(T%XT_CANYON - XTT, ZEHPA, DUT%XTRAD_SHADE - XTT, PU10)
IF (LHOOK) CALL DR_HOOK('UTCI_TEB',1,ZHOOK_HANDLE)
END SUBROUTINE UTCI_TEB
