!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #############################################################
SUBROUTINE SW_DAYCYCLE(KI, PZENITH, PTOT_SW)
!     #############################################################
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
!http://www.apesimulator.it/help/models/solarradiation/Calculating_extra-terrestrial_solar_radiation.html
!
INTEGER,             INTENT(IN)  :: KI        ! number of points
REAL, DIMENSION(KI), INTENT(IN)  :: PZENITH     ! Zenith angle (rad)
REAL, DIMENSION(KI), INTENT(OUT) :: PTOT_SW     ! Extraterrestrial solar radiation (W m-2)
!
!RJ: missing declaration
INTEGER :: JJ
!
REAL      :: ZC_SOL  ! Solar constant [W/m2]
REAL      :: ZD_CORR ! Correction to acutal solar distance at any specific day of the year
INTEGER   :: ZJULIAN ! Julian day of the year
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('SW_DAYCYCLE',0,ZHOOK_HANDLE)
!
ZJULIAN = 197
ZC_SOL  = 4.921*1E6/3600                ! W m-2
ZD_CORR = 1 + 0.0334 * COS(0.01721 * ZJULIAN - 0.0552)
!
DO JJ=1,KI
  PTOT_SW(JJ) = ZC_SOL * ZD_CORR * COS(PZENITH(JJ))
END DO
!
IF (LHOOK) CALL DR_HOOK('SW_DAYCYCLE',1,ZHOOK_HANDLE)
!
END SUBROUTINE SW_DAYCYCLE

