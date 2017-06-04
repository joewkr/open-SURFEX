!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
   SUBROUTINE CCETR(PXIA,PIA,PXMUS,PABC,PLAI)
!
!!***   *CCETR* ***
!!
!!    PURPOSE
!!    -------
!!    Calculates radiative transfer within the canopy
!!
!!**  METHOD
!!    ------
!!    Calvet et al. 1998 Forr. Agri. Met. 
!!    [from model of Jacobs(1994) and Roujean(1996)]
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------      
!!    USE MODD_CO2V_PAR
!!
!!    REFERENCE
!!    ---------
!!    Calvet et al. 1998 Forr. Agri. Met. 
!!      
!!    AUTHOR
!!    ------
!!      A. Boone           * Meteo-France *
!!      (following Belair)
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    27/10/97 
!!
!-------------------------------------------------------------------------------
!
USE MODD_CO2V_PAR, ONLY : XDIFRACF, XXGT, XXBOMEGA
!
!*       0.     DECLARATIONS
!               ------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)               :: PABC
!                                 PABC = abscissa needed for integration
!                                        of net assimilation and stomatal 
!                                        conductance over canopy depth
!
REAL, DIMENSION(:), INTENT(IN) :: PIA,PXMUS,PLAI
!                                 PIA   = absorbed PAR
!                                 PXMUS = cosine of solar zenith angle
!                                 PLAI  = leaf area index
!
REAL, DIMENSION(:), INTENT(OUT):: PXIA
!                                 PXIA  = incident radiation after diffusion
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PIA,1)) :: ZXFD,ZXSLAI,ZXIDF,ZXIDR
!                               ZXFD   = fraction of diffusion
!                               ZXSLAI = LAI of upper layer 
!                                ZXIDF  = interception of diffusion
!                              ZXIDR  = direct interception
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!----------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('CCETR',0,ZHOOK_HANDLE)
!
PXIA(:) = 0.
!
WHERE (PIA(:)>0.)
!
! diffusion fraction
!
! fraction of diffusion
!
  ZXFD(:)   = XDIFRACF/(XDIFRACF + PXMUS(:))                 
!
! LAI of upper layer
!
  ZXSLAI(:) = PLAI(:)*(1.0-PABC)                             
!
! interception of diffusion
!
  ZXIDF(:)  = ZXFD(:)*(1.0-EXP(-0.8*ZXSLAI(:)*XXBOMEGA)) 
  !ZXIDF(:)  = 0.8*XXBOMEGA*EXP(-0.8*ZXSLAI(:)*XXBOMEGA) 
!
! direct interception
!
  ZXIDR(:)  = (1.0-ZXFD(:))*(1.0-EXP(-XXGT*ZXSLAI(:)*XXBOMEGA/PXMUS(:)))     
  !ZXIDR(:) = XXGT*XXBOMEGA/PXMUS(:)*EXP(-XXGT*ZXSLAI(:)*XXBOMEGA/PXMUS(:))
!
! Adjusted radiation:
!
  PXIA(:)   = PIA(:)*(1.0-ZXIDF(:)-ZXIDR(:))
  !PXIA(:) = PIA(:)*(ZXFD(:)*ZXIDF(:) + (1 - ZXFD(:))*ZXIDR(:))
!
END WHERE
!
IF (LHOOK) CALL DR_HOOK('CCETR',1,ZHOOK_HANDLE)
!
END SUBROUTINE CCETR
