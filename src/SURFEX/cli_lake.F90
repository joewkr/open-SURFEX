!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #######
SUBROUTINE CLI_LAKE (G, F)
!     ###############
!
!!****  *CLI_LAKE* - prepares input for lake variables from climate data
!!
!!    PURPOSE
!!    -------
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
!!     E. Kourzeneva
!!
!!    MODIFICATIONS
!!    -------------
!!------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODD_FLAKE_n, ONLY : FLAKE_t
!
USE MODI_START_LAKE_OF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!*      0.2    declarations of local variables
!
!
TYPE(GRID_t), INTENT(INOUT) :: G
TYPE(FLAKE_t), INTENT(INOUT) :: F
!
INTEGER :: JI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('CLI_LAKE',0,ZHOOK_HANDLE)

DO JI=1,SIZE(G%XLAT)
  CALL START_LAKE_OF(F%TTIME%TDATE%DAY,F%TTIME%TDATE%MONTH,G%XLON(JI),G%XLAT(JI),&
        F%XWATER_DEPTH(JI), F%XT_SNOW(JI),F%XT_ICE(JI),F%XT_MNW(JI),F%XT_WML(JI), &
        F%XT_BOT(JI),F%XT_B1(JI),F%XCT(JI), &
        F%XH_SNOW(JI),F%XH_ICE(JI),F%XH_ML(JI),F%XH_B1(JI),F%XTS(JI))
 
END DO

IF (LHOOK) CALL DR_HOOK('CLI_LAKE',1,ZHOOK_HANDLE)

END SUBROUTINE CLI_LAKE
