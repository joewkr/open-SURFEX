!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
MODULE MODE_COARE30_PSI
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
 INTERFACE PSIFCTU
  MODULE PROCEDURE PSIFUNCTU
 END INTERFACE
 INTERFACE PSIFCTT
  MODULE PROCEDURE PSIFUNCTT
 END INTERFACE
!
CONTAINS
!
!---------------------------------------------------------------------------------------
!
!#######################################################################################
FUNCTION PSIFUNCTU(PZL) RESULT(PSIFCTU)
!#######################################################################################
!
!****  *PSIFUNCTU*
!
!       PURPOSE
!       -------
!       To evaluate the stability function psi for wind speed (if KID=1) or
!       for temperature or humidity profiles (if KID.ne.1) from stability parameter
!       z/L. 
!
!       EXTERNAL
!       --------
!
!       IMPLICIT ARGUMENTS
!       ------------------
!
!       REFERENCE
!       ---------
!       Lik79 : Liu, W. T., K. B. Katsaros, and J. A. Businger, 1979: 
!       Bulk parameterization of air-sea exchanges of heat and water vapor including 
!       the molecular constraints at the interface. J. Atm. Sci., 36, 1722--1735.
!       DyH70 : Dyer, A. J., and B. B. Hicks, 1970: Flux-gradient relationship 
!       in the constant flux layer. Quart. J. Roy. Meteor. Soc., 96, 715--721.
!
!       AUTHOR
!       ------
!
!       MODIFICATIONS
!       -------------
!-------------------------------------------------------------------------------
IMPLICIT NONE
!
!        0.  Declaration
!
!        0.1 declaration of arguments
!
REAL, DIMENSION(:), INTENT(IN)    :: PZL  !Obukhovs stability parameter
REAL, DIMENSION(SIZE(PZL))        :: PSIFCTU !function psi value
!        0.2 declaration of local variables
REAL, DIMENSION(SIZE(PZL)) :: ZY,ZX,ZC,ZPSIC,ZPSIK,ZF
INTEGER :: JJ
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_COARE30_PSI:PSIFUNCTU',0,ZHOOK_HANDLE)
DO JJ=1,SIZE(PZL)
  IF(PZL(JJ)<0.) THEN
    ZX(JJ)   = (1.0 - 15. * PZL(JJ))**0.25         ! Kansas unstable
    ZPSIK(JJ)= 2.0 * LOG((1.0+ZX(JJ)       )/2.0) &
             +       LOG((1.0+ZX(JJ)*ZX(JJ))/2.0) &
             - 2.0 * atan(ZX(JJ)) &
             + 2.0 * atan(1.0)  
    !
    ZY(JJ)   = (1.0 - 10.15 * PZL(JJ))**0.3333     ! Convective
    ZPSIC(JJ)= 1.5 * LOG((ZY(JJ)*ZY(JJ)+ZY(JJ)+1.)/3.) &
             - (3.0**0.5) * atan((2.0*ZY(JJ)+1.0)/(3.0**0.5)) &
             + 4.0        * atan(1.0)/(3.0**0.5) 
    !
    ZF(JJ)   =PZL(JJ) * PZL(JJ) / (1.0+PZL(JJ)*PZL(JJ)) 
    !
    PSIFCTU(JJ)=(1.-ZF(JJ)) * ZPSIK(JJ) + ZF(JJ) * ZPSIC(JJ)
  ELSE
    ZC(JJ)=MIN(50.,0.35*PZL(JJ))           ! Stable
    PSIFCTU(JJ)=-((1.+1.*PZL(JJ))**1. + 0.6667*(PZL(JJ)-14.28)/EXP(ZC(JJ)) + 8.525)
  ENDIF
ENDDO
IF (LHOOK) CALL DR_HOOK('MODE_COARE30_PSI:PSIFUNCTU',1,ZHOOK_HANDLE)

END FUNCTION PSIFUNCTU
!---------------------------------------------------------------------------------------
!
!#######################################################################################
FUNCTION PSIFUNCTT(PZL) RESULT(PSIFCTT)
!#######################################################################################
!
!****  *PSIFUNCTU*
!
!       PURPOSE
!       -------
!       To evaluate the stability function psi for wind speed (if KID=1) or
!       for temperature or humidity profiles (if KID.ne.1) from stability parameter
!       z/L. 
!
!       EXTERNAL
!       --------
!
!       IMPLICIT ARGUMENTS
!       ------------------
!
!       REFERENCE
!       ---------
!       Lik79 : Liu, W. T., K. B. Katsaros, and J. A. Businger, 1979: 
!       Bulk parameterization of air-sea exchanges of heat and water vapor including 
!       the molecular constraints at the interface. J. Atm. Sci., 36, 1722--1735.
!       DyH70 : Dyer, A. J., and B. B. Hicks, 1970: Flux-gradient relationship 
!       in the constant flux layer. Quart. J. Roy. Meteor. Soc., 96, 715--721.
!
!       AUTHOR
!       ------
!
!       MODIFICATIONS
!       -------------
!-------------------------------------------------------------------------------
IMPLICIT NONE
!
!        0.  Declaration
!
!        0.1 declaration of arguments
!
REAL, DIMENSION(:), INTENT(IN)    :: PZL  !Obukhovs stability parameter
REAL, DIMENSION(SIZE(PZL))        :: PSIFCTT !function psi value
!        0.2 declaration of local variables
REAL, DIMENSION(SIZE(PZL)) :: ZX,ZY,ZC,ZPSIC,ZPSIK,ZF
INTEGER :: JJ
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_COARE30_PSI:PSIFUNCTT',0,ZHOOK_HANDLE)
DO JJ=1,SIZE(PZL)
  IF(PZL(JJ)<0.) THEN
    ZX(JJ)   = (1. - 15. * PZL(JJ))**.5         ! Kansas unstable
    ZPSIK(JJ)= 2.0 * LOG((1.0+ZX(JJ)       )/2.0)
    !
    ZY(JJ)   = (1.0 - 34.15 * PZL(JJ))**0.3333  ! Convective
    ZPSIC(JJ)= 1.5 * LOG((ZY(JJ)*ZY(JJ)+ZY(JJ)+1.0)/3.) &
             - (3.0**0.5) * atan((2.0*ZY(JJ)+1.0)/(3.0**0.5)) &
             + 4.0        * atan(1.0)/(3.0**0.5)  
    !
    ZF(JJ)   = PZL(JJ) * PZL(JJ) / (1.0+PZL(JJ)*PZL(JJ)) 
    !
    PSIFCTT(JJ)= (1.-ZF(JJ)) * ZPSIK(JJ) + ZF(JJ) * ZPSIC(JJ)
  ELSE
    ZC(JJ)=MIN(50.,0.35*PZL(JJ))           ! Stable
    PSIFCTT(JJ)=-((1.+2.*PZL(JJ)/3.)**1.5 + 0.6667*(PZL(JJ)-14.28)/EXP(ZC(JJ)) + 8.525)
  ENDIF
ENDDO
IF (LHOOK) CALL DR_HOOK('MODE_COARE30_PSI:PSIFUNCTT',1,ZHOOK_HANDLE)

END FUNCTION PSIFUNCTT
!
END MODULE MODE_COARE30_PSI
