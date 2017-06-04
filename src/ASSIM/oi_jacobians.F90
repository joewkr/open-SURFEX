!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE OI_JACOBIANS (KNBPT,&
!---------------------------------------------------------------------------------
! - INPUT  1D
   PWS_O, PSAB, PARG, PD2, PWP, &
! - OUTPUT 1D .
   PDWG_DWG, PDWG_DW2)    
!
!**** * JACOBIANS * - Compute analytical Jacobians for assimilation of WG

!     Purpose.
!     --------

!     - Based on the restore term of the ISBA equation for WG
!       
    
!**   Interface.
!     ----------
!        *CALL* *OI_JACOBIANS*

!----------------------------------------------------------------------------------

!     Externals.
!     ---------

!     Method.  analytical formulation
!     -------

!     Author.
!     -------
!        09-06, J.-F. Mahfouf

!-----------------------------------------------------------------------
!
USE MODD_ASSIM,      ONLY : NECHGU, XRSCAL_JAC                                                                      
USE MODD_CSTS,       ONLY : XRHOLW, XDAY  
!  
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!  
INTEGER, INTENT(IN)    :: KNBPT
REAL    ,INTENT(IN)    :: PWS_O(KNBPT) 
REAL    ,INTENT(IN)    :: PSAB(KNBPT)
REAL    ,INTENT(IN)    :: PARG(KNBPT)
REAL    ,INTENT(IN)    :: PD2(KNBPT)
REAL    ,INTENT(IN)    :: PWP(KNBPT)
!
REAL    ,INTENT(OUT)   :: PDWG_DWG(KNBPT)
REAL    ,INTENT(OUT)   :: PDWG_DW2(KNBPT) 
!
REAL    :: ZWSAT, ZWL, ZDT, ZW2, ZC2, ZC2REF, ZP, ZA, ZWGEQ_DW2
INTEGER    :: JROF
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('OI_JACOBIANS',0,ZHOOK_HANDLE)
!
ZWL = 1.E-5
ZDT = REAL(NECHGU)*3600.0*XRSCAL_JAC
!
! Compute analytical Jacobians for the ISBA 2L scheme
!
DO JROF = 1,KNBPT

  IF (PWS_O(JROF) /= 999.0) THEN

    ZP = 0.134     * PARG(JROF) + 3.4
    ZA = 732.42E-3 * PARG(JROF)**(-0.539)
    ZC2REF = 13.815 * PARG(JROF)**(-0.954)
    ZWSAT = (-1.08*PSAB(JROF) + 494.305)*0.001
    ZW2 = PWP(JROF)/(PD2(JROF)*XRHOLW)
    ZC2 = ZC2REF*ZW2/(ZWSAT -ZW2 + ZWL)
    ZWGEQ_DW2 = 1.0 - ZA*ZP*(ZW2/ZWSAT)**(ZP-1.0) +  &
                  9.0*ZA*ZP*(ZW2/ZWSAT)**(9.0*ZP-1.0)   
    PDWG_DWG(JROF) = EXP(-ZC2/XDAY*ZDT)
    PDWG_DW2(JROF) = ZWGEQ_DW2 * (1.0 - EXP(-ZC2/XDAY*ZDT))

  ELSE

    PDWG_DWG(JROF) = 0.0
    PDWG_DW2(JROF) = 0.0

  ENDIF 

ENDDO
IF (LHOOK) CALL DR_HOOK('OI_JACOBIANS',1,ZHOOK_HANDLE)
!
END SUBROUTINE OI_JACOBIANS
