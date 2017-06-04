!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE OI_KALMAN_GAIN(PDWG_DWG,PDWG_DW2,PD2,PK1,PK2)
!
!****-------------------------------------------------------------------
!
USE MODD_CSTS,  ONLY : XRHOLW 
USE MODD_ASSIM, ONLY : XRD1, XSIGWGO, XSIGWGB, XSIGW2B
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
REAL ,INTENT(IN)  :: PDWG_DWG, PDWG_DW2, PD2
REAL ,INTENT(OUT) :: PK1, PK2
!
REAL :: ZSIG_WG,ZSIG_W2,ZSIG_WO,ZA,ZB,ZH1,ZH2,ZDENOM
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! Standard deviation of background and observation errors
!
IF (LHOOK) CALL DR_HOOK('OI_KALMAN_GAIN',0,ZHOOK_HANDLE)
ZSIG_WG = XSIGWGB
ZSIG_W2 = XSIGW2B
ZSIG_WO = XSIGWGO
!
! Jacobians elements
!
ZH1 = PDWG_DWG
ZH2 = PDWG_DW2
!
! Elements of background covariance matrix
!
ZA = (ZSIG_WG*ZH1)**2 + (ZSIG_W2*ZH2)**2
ZB =  ZSIG_W2**2
!
ZDENOM = (ZA + ZSIG_WO*ZSIG_WO)
!
! Kalman gain elements (m3/m3)
!
PK1 =  ZA    /ZDENOM !*RD1*XRHOLW
PK2 =  ZB*ZH2/ZDENOM !*PD2*XRHOLW
IF (LHOOK) CALL DR_HOOK('OI_KALMAN_GAIN',1,ZHOOK_HANDLE)
!
!**---------------------------------------------------------------------
END SUBROUTINE OI_KALMAN_GAIN

