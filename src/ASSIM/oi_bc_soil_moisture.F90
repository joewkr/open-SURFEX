!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE OI_BC_SOIL_MOISTURE (KNBPT,&
!---------------------------------------------------------------------------------
! - INPUT  1D
   PSM_O,PSAB,&
! - OUTPUT 1D .
   PWS_O)    
!
!**** * BC_SOIL_MOISTURE * - BIAS CORRECTION OF ASCAT SUPERFICIAL SOIL MOISTURE

!     Purpose.
!     --------

!     - Use of CDF matching technique (Koster and Reichle, 2004)
!       
    
!**   Interface.
!     ----------
!        *CALL* *BC_SOIL_MOISTURE*

!----------------------------------------------------------------------------------

!     Externals.
!     ---------

!     Method.  5th other polynomial correction
!     -------

!     Author.
!     -------
!        09-06, J.-F. Mahfouf

!-----------------------------------------------------------------------
!
!  
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!  
INTEGER, INTENT(IN)   :: KNBPT
!
REAL    ,INTENT(IN)    :: PSM_O(KNBPT) 
REAL    ,INTENT(IN)    :: PSAB(KNBPT)
!
REAL    ,INTENT(OUT)   :: PWS_O(KNBPT) 
!
REAL    :: ZWSAT, ZA0, ZA1, ZA2, ZA3, ZA4, ZA5
INTEGER    :: JROF
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! Coefficient for 5th order polynomial fit
!        
IF (LHOOK) CALL DR_HOOK('OI_BC_SOIL_MOISTURE',0,ZHOOK_HANDLE)
!
ZA0 =  8.80461E-08
ZA1 = -2.21598E-05
ZA2 =  0.00188043
ZA3 = -0.0575883
ZA4 =  0.0249301
ZA5 =  15.7502
!
! Perform the bias correction when observation available
!
DO JROF = 1,KNBPT
  !
  IF (PSM_O(JROF) /= 999.0) THEN
    !
    ZWSAT = -0.108*PSAB(JROF) + 0.494305
    !
    PWS_O(JROF) = ZA0*PSM_O(JROF)**5 + ZA1*PSM_O(JROF)**4 + ZA2*PSM_O(JROF)**3 + &
                    ZA3*PSM_O(JROF)**2 + (1.+ZA4)*PSM_O(JROF) + ZA5  
    !
    PWS_O(JROF) = PWS_O(JROF)*ZWSAT*0.01
    !
  ELSE
    PWS_O(JROF) = 999.0 
  ENDIF 
  !
ENDDO
!
IF (LHOOK) CALL DR_HOOK('OI_BC_SOIL_MOISTURE',1,ZHOOK_HANDLE)
!
END SUBROUTINE OI_BC_SOIL_MOISTURE
