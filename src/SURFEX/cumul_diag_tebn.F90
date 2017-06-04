!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!##################################
SUBROUTINE CUMUL_DIAG_TEB_n (DMTC, DMT, GDDEC, GDDE, GRDEC, GRDE, TOP, PTSTEP)
!##################################
!
!
!!****  *CUMUL_DIAG_TEB_n*  
!!
!!    PURPOSE
!!    -------
!      Cumulates some diagnostics for TEB
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
!!      C. de Munck       * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    02/2013
!!                  08/2013 (V. Masson) adds solar panels
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_DIAG_MISC_TEB_n, ONLY : DIAG_MISC_TEB_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
!
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_t
!
USE MODD_SURF_PAR,        ONLY :  XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(DIAG_MISC_TEB_t), INTENT(INOUT) :: DMTC
TYPE(DIAG_MISC_TEB_t), INTENT(INOUT) :: DMT
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
!
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: GDDE
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: GDDEC
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: GRDE
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: GRDEC
!
REAL,               INTENT(IN) :: PTSTEP            ! time step
!
!*      0.2    declarations of local variables
!
INTEGER :: JI 

REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!       0.     Initialization
!              --------------
IF (LHOOK) CALL DR_HOOK('CUMUL_DIAG_TEB_N',0,ZHOOK_HANDLE)
!
!       1.     Time-cumulated diagnostics for TEB
!              ----------------------------------
!
DO JI=1,SIZE(DMT%XRUNOFF_ROOF,1)
!
 IF (TOP%LSOLAR_PANEL) THEN
    IF (DMT%XTHER_PROD_BLD(JI) .NE. XUNDEF) THEN
     DMTC%XTHER_PROD_BLD(JI)     =  DMTC%XTHER_PROD_BLD(JI)     + DMT%XTHER_PROD_BLD(JI)  * PTSTEP
    ENDIF
    !
    IF (DMT%XPHOT_PROD_BLD(JI) .NE. XUNDEF) THEN
     DMTC%XPHOT_PROD_BLD(JI)     =  DMTC%XPHOT_PROD_BLD(JI)     + DMT%XPHOT_PROD_BLD(JI)  * PTSTEP
    ENDIF
 END IF

 IF (TOP%CBEM == 'BEM') THEN
    IF (DMT%XHVAC_COOL(JI) .NE. XUNDEF) THEN
     DMTC%XHVAC_COOL(JI)     =  DMTC%XHVAC_COOL(JI)       + DMT%XHVAC_COOL(JI)        * PTSTEP
    ENDIF
    !
    IF (DMT%XHVAC_HEAT(JI) .NE. XUNDEF) THEN
     DMTC%XHVAC_HEAT(JI)     =  DMTC%XHVAC_HEAT(JI)       + DMT%XHVAC_HEAT(JI)        * PTSTEP
    ENDIF
 ENDIF
 !
 IF (DMT%XRUNOFF_TOWN(JI) .NE. XUNDEF) THEN
  DMTC%XRUNOFF_TOWN(JI)      =  DMTC%XRUNOFF_TOWN(JI)     + DMT%XRUNOFF_TOWN(JI)      * PTSTEP
 ENDIF
 !
 IF (DMT%XRUNOFF_ROAD(JI) .NE. XUNDEF) THEN
  DMTC%XRUNOFF_ROAD(JI)      =  DMTC%XRUNOFF_ROAD(JI)     + DMT%XRUNOFF_ROAD(JI)      * PTSTEP
 ENDIF
 !
 IF (DMT%XRUNOFF_ROOF(JI) .NE. XUNDEF) THEN 
  DMTC%XRUNOFF_ROOF(JI)      =  DMTC%XRUNOFF_ROOF(JI)     + DMT%XRUNOFF_ROOF(JI)      * PTSTEP
 ENDIF
 !
 IF (DMT%XRUNOFF_STRLROOF(JI) .NE. XUNDEF) THEN
  DMTC%XRUNOFF_STRLROOF(JI)  =  DMTC%XRUNOFF_STRLROOF(JI) + DMT%XRUNOFF_STRLROOF(JI)  * PTSTEP
 ENDIF
 !
 IF (DMT%XIRRIG_ROAD(JI) .NE. XUNDEF) THEN
   DMTC%XIRRIG_ROAD(JI)      =  DMTC%XIRRIG_ROAD(JI)      + DMT%XIRRIG_ROAD(JI)       * PTSTEP
 ENDIF
 !
 IF (TOP%LGARDEN) THEN
   !
   IF (GDDE%XRUNOFF(JI) .NE. XUNDEF) THEN
     GDDEC%XRUNOFF(JI)    =  GDDEC%XRUNOFF(JI)   + GDDE%XRUNOFF(JI)    * PTSTEP
   ENDIF 
   !
   IF (GDDE%XDRAIN(JI) .NE. XUNDEF) THEN
     GDDEC%XDRAIN(JI)    =  GDDEC%XDRAIN(JI)    + GDDE%XDRAIN(JI)     * PTSTEP
   ENDIF
   !
   IF (DMT%XIRRIG_GARDEN(JI) .NE. XUNDEF) THEN
     DMTC%XIRRIG_GARDEN(JI)    =  DMTC%XIRRIG_GARDEN(JI)    + DMT%XIRRIG_GARDEN(JI)     * PTSTEP
   ENDIF
   !
 ENDIF
 ! 
 IF (TOP%LGREENROOF) THEN 

    IF (GRDE%XRUNOFF(JI) .NE. XUNDEF) THEN
     GRDEC%XRUNOFF(JI) =  GRDEC%XRUNOFF(JI)+ GRDE%XRUNOFF(JI) * PTSTEP
    ENDIF
    !
    IF (GRDE%XDRAIN(JI) .NE. XUNDEF) THEN
     GRDEC%XDRAIN(JI)  =  GRDEC%XDRAIN(JI) + GRDE%XDRAIN(JI)  * PTSTEP
    ENDIF
    !
    IF (DMT%XIRRIG_GREENROOF(JI) .NE. XUNDEF) THEN
     DMTC%XIRRIG_GREENROOF(JI)  =  DMTC%XIRRIG_GREENROOF(JI) + DMT%XIRRIG_GREENROOF(JI)  * PTSTEP
    ENDIF
 ENDIF
 !
ENDDO
!
IF (LHOOK) CALL DR_HOOK('CUMUL_DIAG_TEB_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE CUMUL_DIAG_TEB_n
