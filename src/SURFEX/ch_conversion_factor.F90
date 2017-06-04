!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE CH_CONVERSION_FACTOR (PCONVERSION, HCONVERSION,PRHOA)
!     #######################################
!
!!****  *CH_CONVERSION_FACTOR
!!
!!    PURPOSE
!!    -------
!     Determines the correct conversion factor to produce emissions in
!     Molec/m2/s
!
!!**  METHOD
!!    ------
!!    
!!    
!!    AUTHOR
!!    ------
!!      S.QUEGUINER 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original        11/2011
!!      A. Alias        07/2013 add MODI_ABOR1_SFX
!!      M. Leriche      04/2014 correct conversion factor
!!-----------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
!
USE MODD_CSTS,       ONLY : XAVOGADRO, XMD
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
!
IMPLICIT NONE
!
!*       0.1   declarations of arguments
!
!
REAL, DIMENSION(:), INTENT(INOUT) :: PCONVERSION
!
 CHARACTER(LEN=3),  INTENT(IN)  :: HCONVERSION ! Unit conversion code
REAL, DIMENSION(:),INTENT(IN)  :: PRHOA       ! air density
!
!*       0.2   declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('CH_CONVERSION_FACTOR',0,ZHOOK_HANDLE)
!
! determine the conversion factor
PCONVERSION(:) = 1.
SELECT CASE (HCONVERSION)
  CASE ('MIX') ! flux given ppp*m/s,  conversion to molec/m2/s
    PCONVERSION(:) = XAVOGADRO * PRHOA(:) / XMD
  CASE ('CON') ! flux given in molecules/cm2/s, conversion to molec/m2/s 
    PCONVERSION(:) =  1E4
  CASE ('MOL') ! flux given in microMol/m2/day, conversion to molec/m2/s  
    PCONVERSION(:) = 1E-6 * XAVOGADRO / 86400.
  CASE DEFAULT
    CALL ABOR1_SFX('CH_BUILDEMISSN: UNKNOWN CONVERSION FACTOR')
END SELECT
!
IF (LHOOK) CALL DR_HOOK('CH_CONVERSION_FACTOR',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE CH_CONVERSION_FACTOR
