!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!      ########################
MODULE MODI_SNOW_T_WLIQ_TO_HEAT
!      ########################
!
INTERFACE SNOW_T_WLIQ_TO_HEAT
!
SUBROUTINE SNOW_T_WLIQ_TO_HEAT_1D(PHEAT,PRHO,PT,PWLIQ)
REAL, DIMENSION(:), INTENT(OUT)          :: PHEAT  ! snow heat density        (J/m3)
REAL, DIMENSION(:), INTENT(IN)           :: PRHO   ! snow density             (kg/m3)
REAL, DIMENSION(:), INTENT(IN)           :: PT     ! snow temperature profile (K)
REAL, DIMENSION(:), INTENT(IN), OPTIONAL :: PWLIQ  ! liquid water profile     (kg/m3)
END SUBROUTINE SNOW_T_WLIQ_TO_HEAT_1D
!
SUBROUTINE SNOW_T_WLIQ_TO_HEAT_2D(PHEAT,PRHO,PT,PWLIQ)
REAL, DIMENSION(:,:), INTENT(OUT)          :: PHEAT  ! snow heat density        (J/m3)
REAL, DIMENSION(:,:), INTENT(IN)           :: PRHO   ! snow density             (kg/m3)
REAL, DIMENSION(:,:), INTENT(IN)           :: PT     ! snow temperature profile (K)
REAL, DIMENSION(:,:), INTENT(IN), OPTIONAL :: PWLIQ  ! liquid water profile     (kg/m3)
END SUBROUTINE SNOW_T_WLIQ_TO_HEAT_2D
!
SUBROUTINE SNOW_T_WLIQ_TO_HEAT_3D(PHEAT,PRHO,PT,PWLIQ)
REAL, DIMENSION(:,:,:), INTENT(OUT)          :: PHEAT  ! snow heat density        (J/m3)
REAL, DIMENSION(:,:,:), INTENT(IN)           :: PRHO   ! snow density             (kg/m3)
REAL, DIMENSION(:,:,:), INTENT(IN)           :: PT     ! snow temperature profile (K)
REAL, DIMENSION(:,:,:), INTENT(IN), OPTIONAL :: PWLIQ  ! liquid water profile     (kg/m3)
END SUBROUTINE SNOW_T_WLIQ_TO_HEAT_3D
!
END INTERFACE SNOW_T_WLIQ_TO_HEAT
!
END MODULE MODI_SNOW_T_WLIQ_TO_HEAT
!
!          ###########################################
SUBROUTINE SNOW_T_WLIQ_TO_HEAT_1D(PHEAT,PRHO,PT,PWLIQ)
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
REAL, DIMENSION(:), INTENT(OUT)          :: PHEAT  ! snow heat density        (J/m3)
REAL, DIMENSION(:), INTENT(IN)           :: PRHO   ! snow density             (kg/m3)
REAL, DIMENSION(:), INTENT(IN)           :: PT     ! snow temperature profile (K)
REAL, DIMENSION(:), INTENT(IN), OPTIONAL :: PWLIQ  ! liquid water profile     (kg/m3)
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*      0.2    declarations of local variables
!
!---------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_SNOW_T_WLIQ_TO_HEAT:SNOW_T_WLIQ_TO_HEAT_1D',0,ZHOOK_HANDLE)
PHEAT = XUNDEF
!
!* effect of temperature on heat
!
WHERE(PRHO/=XUNDEF) PHEAT = (PT-XTT)*SNOW3LSCAP(PRHO) - XLMTT * PRHO
!
!* effect of liquid water
!
IF (PRESENT(PWLIQ)) THEN
  WHERE(PRHO/=XUNDEF) PHEAT = PHEAT + XLMTT * PWLIQ
END IF
IF (LHOOK) CALL DR_HOOK('MODI_SNOW_T_WLIQ_TO_HEAT:SNOW_T_WLIQ_TO_HEAT_1D',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------------
!
END SUBROUTINE SNOW_T_WLIQ_TO_HEAT_1D
!
!          ###########################################
SUBROUTINE SNOW_T_WLIQ_TO_HEAT_2D(PHEAT,PRHO,PT,PWLIQ)
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
REAL, DIMENSION(:,:), INTENT(OUT)          :: PHEAT  ! snow heat density        (J/m3)
REAL, DIMENSION(:,:), INTENT(IN)           :: PRHO   ! snow density             (kg/m3)
REAL, DIMENSION(:,:), INTENT(IN)           :: PT     ! snow temperature profile (K)
REAL, DIMENSION(:,:), INTENT(IN), OPTIONAL :: PWLIQ  ! liquid water profile     (kg/m3)
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*      0.2    declarations of local variables
!
!---------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_SNOW_T_WLIQ_TO_HEAT:SNOW_T_WLIQ_TO_HEAT_2D',0,ZHOOK_HANDLE)
PHEAT = XUNDEF
!
!* effect of temperature on heat
!
WHERE(PRHO/=XUNDEF) PHEAT = (PT-XTT)*SNOW3LSCAP(PRHO) - XLMTT * PRHO
!
!* effect of liquid water
!
IF (PRESENT(PWLIQ)) THEN
  WHERE(PRHO/=XUNDEF) PHEAT = PHEAT + XLMTT * PWLIQ
END IF
IF (LHOOK) CALL DR_HOOK('MODI_SNOW_T_WLIQ_TO_HEAT:SNOW_T_WLIQ_TO_HEAT_2D',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------------
!
END SUBROUTINE SNOW_T_WLIQ_TO_HEAT_2D
!
!          ###########################################
SUBROUTINE SNOW_T_WLIQ_TO_HEAT_3D(PHEAT,PRHO,PT,PWLIQ)
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
REAL, DIMENSION(:,:,:), INTENT(OUT)          :: PHEAT  ! snow heat density        (J/m3)
REAL, DIMENSION(:,:,:), INTENT(IN)           :: PRHO   ! snow density             (kg/m3)
REAL, DIMENSION(:,:,:), INTENT(IN)           :: PT     ! snow temperature profile (K)
REAL, DIMENSION(:,:,:), INTENT(IN), OPTIONAL :: PWLIQ  ! liquid water profile     (kg/m3)
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*      0.2    declarations of local variables
!
!---------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_SNOW_T_WLIQ_TO_HEAT:SNOW_T_WLIQ_TO_HEAT_3D',0,ZHOOK_HANDLE)
PHEAT = XUNDEF
!
!* effect of temperature on heat
!
WHERE(PRHO/=XUNDEF) PHEAT = (PT-XTT)*SNOW3LSCAP(PRHO) - XLMTT * PRHO
!
!* effect of liquid water
!
IF (PRESENT(PWLIQ)) THEN
  WHERE(PRHO/=XUNDEF) PHEAT = PHEAT + XLMTT * PWLIQ
END IF
IF (LHOOK) CALL DR_HOOK('MODI_SNOW_T_WLIQ_TO_HEAT:SNOW_T_WLIQ_TO_HEAT_3D',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------------
!
END SUBROUTINE SNOW_T_WLIQ_TO_HEAT_3D
