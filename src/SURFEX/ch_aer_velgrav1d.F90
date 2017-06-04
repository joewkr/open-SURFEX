!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE CH_AER_VELGRAV1D(PSIG, PRG, PTA, PRHODREF, PRHOP, PMU, PVGK,PDPK, PVGG,PDPG)
!!   #######################################
!!
!!   PURPOSE
!!   -------
!!
!!   REFERENCE
!!   ---------
!!   none
!!
!!   AUTHOR
!!    ------
!!   P. Tulet (meteo france)
!!
!!   MODIFICATIONS
!!    -------------
!!
! Entry variables:
!
! PM(IN)       -Array of moments
!
!*************************************************************
! Exit variables:
!
! PFSED(IN)  -Array of moment variation due to dry deposition
!
!*************************************************************
! Variables used during the deposition velocity calculation
! 
! PDPK       -Polydisperse diffusivity (m2/s)
! PVGK       -Polydisperse settling velocity of the kth moment (m/s)
!************************************************************
!!
!!   IMPLICIT ARGUMENTS
USE MODD_CHS_AEROSOL
!!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

!! Declarations d'arguments
!
REAL, DIMENSION(:,:), INTENT(OUT) :: PVGK,PDPK
REAL, DIMENSION(:),   INTENT(OUT) :: PMU
REAL, DIMENSION(:,:), INTENT(OUT) :: PVGG
REAL, DIMENSION(:,:), INTENT(OUT) :: PDPG
REAL, DIMENSION(:,:), INTENT(IN)    :: PRHOP
REAL, DIMENSION(:,:), INTENT(IN)    :: PSIG, PRG
REAL, DIMENSION(:),   INTENT(IN)    :: PTA, PRHODREF
!
!!!! Declarations de variables internes
!
REAL, DIMENSION(size(PSIG,1)) :: ZLAMBDA

REAL, DIMENSION(size(PSIG,1)) :: ZRG,ZLN2S

REAL, DIMENSION(size(PSIG,1)) :: ZKNG

REAL, PARAMETER :: gasmw=28.9644d0
REAL :: ZK, ZRD, ZAVOGADRO, ZBOLTZ, ZMD, ZPI, ZG

INTEGER :: II,IJ
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('CH_AER_VELGRAV1D',0,ZHOOK_HANDLE)
ZPI = 2.*ASIN(1.)
ZAVOGADRO =  6.0221367E+23
ZBOLTZ = 1.380658E-23 
ZMD   = 28.9644E-3
ZRD  = ZAVOGADRO * ZBOLTZ / ZMD
ZG  = 9.80665


! Sutherland's equation for viscosity
PMU(:)=1.8325d-5*416.16/(PTA(:)+120)*(PTA(:)/296.16)*SQRT(PTA(:)/296.16)

! Mean free path (Seinfeld and Pandis p455)
ZLAMBDA(:)=PMU(:)/PRHODREF(:)*sqrt(1.89d-4*gasmw/PTA(:))*1.e6

do II=1,JPMODE

  ZRG(:)=PRG(:,II) * 1E-6 
  ZLN2S(:)=LOG(PSIG(:,II))**2 
  !
  ZKNG(:)=ZLAMBDA(:) / PRG(:,II) 
  !
  PVGG(:,II)= 2.*ZG*PRHOP(:,II)*ZRG(:)**2 /(9.*PMU(:))
  PDPG(:,II)=ZBOLTZ*PTA(:)/ (6.*ZPI* ZRG(:)*PMU(:))


 
  do IJ=0,2
!
    ZK=real(3*IJ)
    PDPK(:,3*II+IJ-2)=PDPG(:,II)*(exp((-2.*ZK+1.)/2.*ZLN2S(:))+1.246*ZKNG(:)*&
                exp((-4.*ZK+4)/2.*ZLN2S(:)))  

    PVGK(:,3*II+IJ-2)=PVGG(:,II)*&
      (exp((4.*ZK+4.)/2.*ZLN2S(:)) + 1.246*ZKNG(:)* exp((2.*ZK+1.)/2.*ZLN2S(:)))  

  enddo
 
enddo
IF (LHOOK) CALL DR_HOOK('CH_AER_VELGRAV1D',1,ZHOOK_HANDLE)

END SUBROUTINE CH_AER_VELGRAV1D
