!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE INTERPOL_TS_WATER_MTH (W)
!     #######################################################
!
!!****  *INTERPOL_TS_WATER_MTH* - Interpolation of monthly TS water
!!
!!    PURPOSE
!!    -------
!!
!!
!!**  METHOD
!!    ------
!!
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
!!      
!     B.Decharme  Meteo-France
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    28/01/10
!!      Modified    07/2015   B. Decharme : new linear interpolation
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
USE MODD_WATFLUX_n, ONLY : WATFLUX_t
!
USE MODI_INTERPOL_QUADRA
USE MODI_INTERPOL_LINEAR
!
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declaration of arguments
!------------------------
! 
!
TYPE(WATFLUX_t), INTENT(INOUT) :: W
!
!*       0.2   Declaration of local variables
!              ------------------------------
!
REAL            :: ZDAT   ! current day in the current month
REAL            :: ZNDAT  ! number of days in the current month
INTEGER         :: IMTH0  ! previous month
INTEGER         :: IMTH1  ! current month 
INTEGER         :: IMTH2  ! next month
INTEGER         :: INDAYS ! number of days in KMONTH
!
INTEGER         :: IDELTA
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*       1.    Number of days in a month
!              -------------------------
!
IF (LHOOK) CALL DR_HOOK('INTERPOL_TS_WATER_MTH',0,ZHOOK_HANDLE)
SELECT CASE (W%TTIME%TDATE%MONTH)
    CASE(4,6,9,11)
      INDAYS=30
    CASE(1,3,5,7:8,10,12)
      INDAYS=31
    CASE(2)
      IF( ((MOD(W%TTIME%TDATE%YEAR,4)==0).AND.(MOD(W%TTIME%TDATE%YEAR,100)/=0)) .OR. (MOD(W%TTIME%TDATE%YEAR,400)==0))THEN
        INDAYS=29
      ELSE
        INDAYS=28
      ENDIF
END SELECT
!
!
!-------------------------------------------------------------------------------
!
!*       2.    TS water Interpolation using previous, current and next month
!              -------------------------------------------------------------
!
ZDAT = REAL(W%TTIME%TDATE%DAY)
ZNDAT= REAL(INDAYS)
!
! The current month correspond to the indice 2 (or 3 if next month))
!
IF (W%TTIME%TDATE%MONTH==W%TZTIME%TDATE%MONTH) THEN 
   IDELTA=0
ELSE
   IDELTA=1
END IF
!
IMTH0=1+IDELTA
IMTH1=2+IDELTA
IMTH2=3+IDELTA
!
IF(W%CINTERPOL_TS=='QUADRA')THEN
  CALL INTERPOL_QUADRA(ZDAT,ZNDAT,W%XTS_MTH(:,IMTH0),W%XTS_MTH(:,IMTH1),W%XTS_MTH(:,IMTH2),W%XTS)
ELSEIF(W%CINTERPOL_TS=='LINEAR')THEN
  CALL INTERPOL_LINEAR(ZDAT,ZNDAT,W%XTS_MTH(:,IMTH0),W%XTS_MTH(:,IMTH1),W%XTS_MTH(:,IMTH2),W%XTS)
ELSEIF(W%CINTERPOL_TS=='UNIF')THEN
  W%XTS(:) = W%XTS_MTH(:,IMTH1)
ELSE
  CALL ABOR1_SFX('INTERPOL_TS_WATER_MTH: interpolation method not supported')
ENDIF
!
IF (LHOOK) CALL DR_HOOK('INTERPOL_TS_WATER_MTH',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INTERPOL_TS_WATER_MTH
