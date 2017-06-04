!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################
      MODULE MODE_COTWO
!     ##################
!
!!****  *MODE_COTWO * - contains some needed computations for vegetation
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
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
!!      A. Boone                 * Meteo France *       
!!
!!    MODIFICATIONS
!!    -------------
!!      Original        05/03/15
!-----------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
!
INTERFACE GAULEG
   MODULE PROCEDURE GAULEG
END INTERFACE
!
!
!-------------------------------------------------------------------------------
CONTAINS
!
!####################################################################
!####################################################################
!####################################################################
SUBROUTINE GAULEG(PX1,PX2,PX,PW,KN)
!
!
!!****  *GAULEG*  
!!
!!    PURPOSE
!!    -------
!
!     Calculates the Gaussian weights for integration of net assimilation
!     and stomatal conductance over the canopy depth
!         
!     
!!**  METHOD
!!    ------
!
!     1) Calculate the weights and abscissa using Gaussian Quadrature
!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!    MODD_CST
!!      
!!    REFERENCE
!!    ---------
!!
!!    Calvet et al. (1998) For. Agri. Met.
!!      
!!    AUTHOR
!!    ------
!!
!!	A. Boone           * Meteo-France *
!!      (following Belair)
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    27/10/97 
!!
!-------------------------------------------------------------------------------
!
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS, ONLY : XPI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER,             INTENT(IN)   :: KN
!                                    number of points at which Gaussian
!                                    weights are evaluated/needed
!
REAL,                INTENT(IN)   :: PX1, PX2
!                                    mathematical/numerical values needed for 
!                                    weight computation
!
REAL, DIMENSION(KN), INTENT(OUT)  :: PX, PW
!                                    PX = abscissa
!                                    PW = Gaussian weights
!
!
!*      0.2    declarations of local variables
!
REAL, PARAMETER                             :: PPEPS = 3.0E-14
!                                              convergence criteria
!
INTEGER JI,JJ,JK                             ! loop indexes
!
INTEGER IM                                   ! 
!
REAL ZXM, ZXL, ZZ, ZP1, ZP2, ZP3, ZPP, ZZ1   ! dummy variables needed for 
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!                                              computation of the gaussian weights
!
!-------------------------------------------------------------------------------
!
!  calcul des poids et abscisses par la methode de quad de Gauss
!
IF (LHOOK) CALL DR_HOOK('GAULEG',0,ZHOOK_HANDLE)
!
IM  = (KN+1)/2
ZXM = 0.50*(PX2+PX1)
ZXL = 0.50*(PX2-PX1)
!      
IM_POINT_LOOP: DO JI = 1,IM
   ZZ  = COS(XPI*(FLOAT(JI)-.250)/(FLOAT(KN)+.50))
!
!  begin iteration:
!
   ITERATION_LOOP: DO JK = 1,100
      ZP1 = 1.
      ZP2 = 0.
      DO JJ = 1,KN
         ZP3 = ZP2
         ZP2 = ZP1
         ZP1 = ((2.*(JJ)-1.)*ZZ*ZP2-(FLOAT(JJ)-1.)*ZP3)/JJ
      END DO
      ZPP = FLOAT(KN)*(ZZ*ZP1-ZP2)/(ZZ*ZZ-1.)
      ZZ1 = ZZ
      ZZ  = ZZ1-ZP1/ZPP
      IF(ABS(ZZ-ZZ1).LE.PPEPS)EXIT
   END DO ITERATION_LOOP
!
!  end iteration.
!
!  compute abscissa
!
   PX(JI)      = ZXM - ZXL*ZZ
   PX(KN+1-JI) = ZXM + ZXL*ZZ
!
!  compute weights
!
   PW(JI)      = 2.0*ZXL/((1.0-ZZ*ZZ)*ZPP*ZPP)
   PW(KN+1-JI) = PW(JI)
END DO IM_POINT_LOOP
!
IF (LHOOK) CALL DR_HOOK('GAULEG',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE GAULEG
!####################################################################
!####################################################################
!####################################################################
!
END MODULE MODE_COTWO
