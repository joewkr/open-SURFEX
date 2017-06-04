!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE DSLT_VELGRAV1D(PSIG, PRG, PTA, PRHODREF, PRHOP, &
                          PMU, PVGK, PDPK, PVGG, PDPG      )
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
!
USE MODD_CSTS ,ONLY : XPI, XG, XBOLTZ
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

!! Declarations d'arguments
!
REAL, DIMENSION(:,:), INTENT(IN)  :: PSIG, PRG
REAL, DIMENSION(:),   INTENT(IN)  :: PTA, PRHODREF
REAL,                 INTENT(IN)  :: PRHOP
REAL, DIMENSION(:),   INTENT(OUT) :: PMU
REAL, DIMENSION(:,:), INTENT(OUT) :: PVGK, PDPK
REAL, DIMENSION(:,:), INTENT(OUT) :: PVGG, PDPG
!
!!!! Declarations de variables internes
!
REAL, DIMENSION(SIZE(PTA)) :: ZLAMBDA
REAL, DIMENSION(SIZE(PTA)) :: ZRG, ZLN2S
REAL, DIMENSION(SIZE(PTA)) :: ZKNG
!
REAL, PARAMETER :: ZGASMW = 28.9644d0
REAL :: ZK
INTEGER :: IJ, II
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('DSLT_VELGRAV1D',0,ZHOOK_HANDLE)

! Sutherland's equation for viscosity
PMU(:)=1.8325d-5 * 416.16/(PTA(:)+120.) * (PTA(:)/296.16) * SQRT(PTA(:)/296.16)

! Mean free path (Seinfeld and Pandis p455)
ZLAMBDA(:)=PMU(:)/PRHODREF(:) * SQRT(1.89d-4*ZGASMW/PTA(:))*1.e6

DO II = 1,SIZE(PRG,2)

  ZRG  (:) = PRG(:,II) * 1E-6 
  ZLN2S(:) = LOG(PSIG(:,II))**2 
  !
  ZKNG (:) = ZLAMBDA(:) / PRG(:,II)
  !
  PVGG(:,II) = 2. * XG * PRHOP * ZRG(:)**2 / (9.*PMU(:))
  PDPG(:,II) = XBOLTZ * PTA(:)/(6.*XPI*ZRG(:)*PMU(:))

  DO IJ = 0,2
    !
    ZK = REAL(3*IJ)
   
    PDPK(:,3*II+IJ-2) = PDPG(:,II) * &
                        (EXP((-2.*ZK+1.)/2.*ZLN2S(:)) + &
                        1.246*ZKNG(:) * EXP((-4.*ZK+4)/2.*ZLN2S(:)))  

    PVGK(:,3*II+IJ-2) = PVGG(:,II) * &
                        (EXP((4.*ZK+4.)/2.*ZLN2S(:)) + &
                        1.246*ZKNG(:)* EXP((2.*ZK+1.)/2.*ZLN2S(:)))  
  ENDDO

ENDDO
IF (LHOOK) CALL DR_HOOK('DSLT_VELGRAV1D',1,ZHOOK_HANDLE)
 

END SUBROUTINE DSLT_VELGRAV1D
