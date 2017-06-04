!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE INTERPOL_LINEAR(PDAT,PNDAT,PVAL0,PVAL1,PVAL2,POUT)
!     #############################################################
!
!!**** *INTERPOL_LINEAR* Linear interpolation between 3 month.
!!                       Current value is reached evry 16 of each month,
!!                       except in February every 15.
!!
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
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
!!
!!    B. Decharme         Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    07/2015
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
REAL,                    INTENT(IN)    :: PDAT    ! Present date in the current month
REAL,                    INTENT(IN)    :: PNDAT   ! Number of date in the current month
REAL, DIMENSION(:),      INTENT(IN)    :: PVAL0   ! Value of the precedent month
REAL, DIMENSION(:),      INTENT(IN)    :: PVAL1   ! Value of the current month
REAL, DIMENSION(:),      INTENT(IN)    :: PVAL2   ! Value of the next month
REAL, DIMENSION(:),      INTENT(OUT)   :: POUT    ! Interpolated value
!
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
REAL         :: ZFACT0, ZFACT1, ZFACT2
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!----------------------------------------------------------------------------
!
!*    0.     Allocation
!            ----------
!
IF (LHOOK) CALL DR_HOOK('INTERPOL_LINEAR',0,ZHOOK_HANDLE)
!
!*    1.     Initialization
!            --------------
!
!*    2.     Coef calculation
!            ----------------
!
ZFACT0=MAX(1.0-(PDAT*2.0+PNDAT-1.0)/(PNDAT*2.0),0.0)
ZFACT1=(2.0*PNDAT-ABS(2.0*PDAT-PNDAT-1.0))/(PNDAT*2.0)
ZFACT2=MAX(1.0-((PNDAT+1.0-PDAT)*2.0+PNDAT-1.0)/(PNDAT*2.0),0.0)
!
!*    3.     Final calculation
!            -----------------
!
POUT(:) = PVAL0(:)*ZFACT0+PVAL1(:)*ZFACT1+PVAL2(:)*ZFACT2
!
!*    4.     End
!            ---
!
IF (LHOOK) CALL DR_HOOK('INTERPOL_LINEAR',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INTERPOL_LINEAR
