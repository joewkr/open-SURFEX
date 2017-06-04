!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      FUNCTION GAMMAS(PX)  RESULT(PGAMMA)
!     ##################################
!
!
!!****  *GAMMAS * -  Gamma  function
!!
!!
!!    PURPOSE
!!    -------
!       The purpose of this function is to compute the Generalized gamma
!    function of its argument.
!
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!      NONE
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      None
!!
!!    REFERENCE
!!    ---------
!!      Press, Teukolsky, Vetterling and Flannery: Numerical Recipes, 206-207
!!
!!    AUTHOR
!!    ------
!!      Jean-Pierre Pinty *LA/OMP*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original     7/11/95
!
!*       0. DECLARATIONS
!           ------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1 declarations of arguments and result
!
REAL, INTENT(IN)                     :: PX
REAL                                 :: PGAMMA
!
!*       0.2 declarations of local variables
!
INTEGER                              :: JJ ! Loop index
REAL                                 :: ZSER,ZSTP,ZTMP,ZX,ZY,ZCOEF(6)
REAL                                 :: ZPI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('GAMMAS',0,ZHOOK_HANDLE)
ZCOEF(1) = 76.18009172947146
ZCOEF(2) =-86.50532032941677
ZCOEF(3) = 24.01409824083091
ZCOEF(4) = -1.231739572450155
ZCOEF(5) =  0.1208650973866179E-2
ZCOEF(6) = -0.5395239384953E-5
ZSTP     =  2.5066282746310005
!
ZPI = 3.141592654
IF (PX.LT.0.) THEN
  ZX = 1.- PX
ELSE 
  ZX = PX
END IF
ZY = ZX
ZTMP =  ZX + 5.5
ZTMP = (ZX + 0.5)*ALOG(ZTMP) - ZTMP
ZSER = 1.000000000190015
!
DO JJ = 1 , 6
  ZY = ZY + 1.0
  ZSER = ZSER + ZCOEF(JJ)/ZY
END DO
!
IF (PX.LT.0.) THEN
  PGAMMA = ZPI/SIN(ZPI*PX)/EXP( ZTMP + ALOG( ZSTP*ZSER/ZX ) )
ELSE
  PGAMMA = EXP( ZTMP + ALOG( ZSTP*ZSER/ZX ) )
END IF
IF (LHOOK) CALL DR_HOOK('GAMMAS',1,ZHOOK_HANDLE)
!
END FUNCTION GAMMAS
