!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!      ########################
MODULE MODI_SNOW_HEAT_TO_T_WLIQ
!      ########################
!
INTERFACE SNOW_HEAT_TO_T_WLIQ
!
SUBROUTINE SNOW_HEAT_TO_T_WLIQ_1D(PHEAT,PRHO,PT,PWLIQ)
REAL, DIMENSION(:), INTENT(IN)           :: PHEAT  ! snow heat density        (J/m3)
REAL, DIMENSION(:), INTENT(IN)           :: PRHO   ! snow density             (kg/m3)
REAL, DIMENSION(:), INTENT(OUT)          :: PT     ! snow temperature profile (K)
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL:: PWLIQ  ! liquid water profile     (kg/m3)
END SUBROUTINE SNOW_HEAT_TO_T_WLIQ_1D
!
SUBROUTINE SNOW_HEAT_TO_T_WLIQ_2D(PHEAT,PRHO,PT,PWLIQ)
REAL, DIMENSION(:,:), INTENT(IN)           :: PHEAT  ! snow heat density        (J/m3)
REAL, DIMENSION(:,:), INTENT(IN)           :: PRHO   ! snow density             (kg/m3)
REAL, DIMENSION(:,:), INTENT(OUT)          :: PT     ! snow temperature profile (K)
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL:: PWLIQ  ! liquid water profile     (kg/m3)
END SUBROUTINE SNOW_HEAT_TO_T_WLIQ_2D
!
SUBROUTINE SNOW_HEAT_TO_T_WLIQ_3D(PHEAT,PRHO,PT,PWLIQ)
REAL, DIMENSION(:,:,:), INTENT(IN)           :: PHEAT  ! snow heat density        (J/m3)
REAL, DIMENSION(:,:,:), INTENT(IN)           :: PRHO   ! snow density             (kg/m3)
REAL, DIMENSION(:,:,:), INTENT(OUT)          :: PT     ! snow temperature profile (K)
REAL, DIMENSION(:,:,:), INTENT(OUT), OPTIONAL:: PWLIQ  ! liquid water profile     (kg/m3)
END SUBROUTINE SNOW_HEAT_TO_T_WLIQ_3D
!
END INTERFACE
!
END MODULE MODI_SNOW_HEAT_TO_T_WLIQ
!
!          ###########################################
SUBROUTINE SNOW_HEAT_TO_T_WLIQ_1D(PHEAT,PRHO,PT,PWLIQ)
!          ###########################################
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_CSTS,       ONLY : XTT, XLMTT
USE MODE_SNOW3L
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:), INTENT(IN)           :: PHEAT  ! snow heat density        (J/m3)
REAL, DIMENSION(:), INTENT(IN)           :: PRHO   ! snow density             (kg/m3)
REAL, DIMENSION(:), INTENT(OUT)          :: PT     ! snow temperature profile (K)
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL:: PWLIQ  ! liquid water profile     (kg/m3)
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*      0.2    declarations of local variables
!
!---------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_SNOW_HEAT_TO_T_WLIQ:SNOW_HEAT_TO_T_WLIQ_1D',0,ZHOOK_HANDLE)
PT = XUNDEF
IF (PRESENT(PWLIQ)) PWLIQ = XUNDEF
!
!* temperature from heat
!
WHERE(PRHO/=XUNDEF) PT = XTT + (PHEAT + XLMTT*PRHO) / SNOW3LSCAP(PRHO)
!
!* effect of liquid water
!
IF (PRESENT(PWLIQ)) THEN
  WHERE(PRHO/=XUNDEF) PWLIQ = (PT-XTT) * SNOW3LSCAP(PRHO) / XLMTT
END IF
!
!* physical limits
!
IF (PRESENT(PWLIQ)) THEN
  PT = MIN(XTT,PT)
  PWLIQ = MAX(0.,PWLIQ)
  PWLIQ = MIN(SNOW3LWLIQMAX(PRHO),PWLIQ)
END IF
IF (LHOOK) CALL DR_HOOK('MODI_SNOW_HEAT_TO_T_WLIQ:SNOW_HEAT_TO_T_WLIQ_1D',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------------
!
END SUBROUTINE SNOW_HEAT_TO_T_WLIQ_1D
!
!          ###########################################
SUBROUTINE SNOW_HEAT_TO_T_WLIQ_2D(PHEAT,PRHO,PT,PWLIQ)
!          ###########################################
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_CSTS,       ONLY : XTT, XLMTT
USE MODE_SNOW3L
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:), INTENT(IN)           :: PHEAT  ! snow heat density        (J/m3)
REAL, DIMENSION(:,:), INTENT(IN)           :: PRHO   ! snow density             (kg/m3)
REAL, DIMENSION(:,:), INTENT(OUT)          :: PT     ! snow temperature profile (K)
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL:: PWLIQ  ! liquid water profile     (kg/m3)
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*      0.2    declarations of local variables
!
!---------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_SNOW_HEAT_TO_T_WLIQ:SNOW_HEAT_TO_T_WLIQ_2D',0,ZHOOK_HANDLE)
PT = XUNDEF
IF (PRESENT(PWLIQ)) PWLIQ = XUNDEF
!
!* temperature from heat
!
WHERE(PRHO/=XUNDEF) PT = XTT + (PHEAT + XLMTT*PRHO) / SNOW3LSCAP(PRHO)
!
!* effect of liquid water
!
IF (PRESENT(PWLIQ)) THEN
  WHERE(PRHO/=XUNDEF) PWLIQ = (PT-XTT) * SNOW3LSCAP(PRHO) / XLMTT
END IF
!
!* physical limits
!
IF (PRESENT(PWLIQ)) THEN
  PT = MIN(XTT,PT)
  PWLIQ = MAX(0.,PWLIQ)
  PWLIQ = MIN(SNOW3LWLIQMAX(PRHO),PWLIQ)
END IF
IF (LHOOK) CALL DR_HOOK('MODI_SNOW_HEAT_TO_T_WLIQ:SNOW_HEAT_TO_T_WLIQ_2D',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------------
!
END SUBROUTINE SNOW_HEAT_TO_T_WLIQ_2D
!
!          ###########################################
SUBROUTINE SNOW_HEAT_TO_T_WLIQ_3D(PHEAT,PRHO,PT,PWLIQ)
!          ###########################################
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_CSTS,       ONLY : XTT, XLMTT
USE MODE_SNOW3L
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:,:), INTENT(IN)           :: PHEAT  ! snow heat density        (J/m3)
REAL, DIMENSION(:,:,:), INTENT(IN)           :: PRHO   ! snow density             (kg/m3)
REAL, DIMENSION(:,:,:), INTENT(OUT)          :: PT     ! snow temperature profile (K)
REAL, DIMENSION(:,:,:), INTENT(OUT), OPTIONAL:: PWLIQ  ! liquid water profile     (kg/m3)
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*      0.2    declarations of local variables
!
!---------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_SNOW_HEAT_TO_T_WLIQ:SNOW_HEAT_TO_T_WLIQ_3D',0,ZHOOK_HANDLE)
PT = XUNDEF
IF (PRESENT(PWLIQ)) PWLIQ = XUNDEF
!
!* temperature from heat
!
WHERE(PRHO/=XUNDEF) PT = XTT + (PHEAT + XLMTT*PRHO) / SNOW3LSCAP(PRHO)
!
!* effect of liquid water
!
IF (PRESENT(PWLIQ)) THEN
  WHERE(PRHO/=XUNDEF) PWLIQ = (PT-XTT) * SNOW3LSCAP(PRHO) / XLMTT
END IF
!
!* physical limits
!
IF (PRESENT(PWLIQ)) THEN
  PT = MIN(XTT,PT)
  PWLIQ = MAX(0.,PWLIQ)
  PWLIQ = MIN(SNOW3LWLIQMAX(PRHO),PWLIQ)
END IF
IF (LHOOK) CALL DR_HOOK('MODI_SNOW_HEAT_TO_T_WLIQ:SNOW_HEAT_TO_T_WLIQ_3D',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------------
!
END SUBROUTINE SNOW_HEAT_TO_T_WLIQ_3D
