!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##########################################################################
      SUBROUTINE ISBA_EMIS_MEB(PPSN, PPSNA, PSIGMA_F, PSIGMA_FN,  &
                               PEMIS_N, PEMIS                     )
!     ##########################################################################
!
!!****  *ISBA_EMIS_MEB*  
!!
!!    PURPOSE
!!    -------
!
!     Calculates the total emissivity for a MEB patch
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!      A. Boone           * Meteo-France *
!!      P. Samuelsson      * SMHI *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2014 
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_PAR,       ONLY : XEMISSOIL, XEMISVEG
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:),   INTENT(IN)   :: PPSN, PPSNA
!                                     PPSN     = fraction of snow on ground and understory vegetation          (-)
!                                     PPSNA    = fraction of vegetation canopy buried by ground-based snowpack (-)
!
REAL, DIMENSION(:),   INTENT(IN)   :: PSIGMA_F, PSIGMA_FN, PEMIS_N
!
REAL, DIMENSION(:),   INTENT(OUT)  :: PEMIS
!                                     PEMIS    = effective (aggregated) net surface emissivity (-)
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PPSN))        :: ZPN, ZEMISG, ZEMISN, ZSIGMA_FA
!                                     ZPN       = working snow fraction (-)
!                                     ZEMISG    = understory surface emissivity for computing effective value (-)
!                                     ZEMISN    = snowpack surface emissivity for computing effective value (-)
!                                     ZSIGMA_FA = zworking shading factor (-)
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*       0.     Initialization:
!               ---------------
!
IF (LHOOK) CALL DR_HOOK('ISBA_EMIS_MEB',0,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
!*       1.     Compute effective emissivity
!               ----------------------------
! (this is consistent with the LW flux computations in isba_lwnet_meb.f90):
!
ZPN(:)        = PPSN(:)*(1.-PPSNA(:))
ZSIGMA_FA(: ) = (1.-ZPN(:))*PSIGMA_F(:) + ZPN(:)*PSIGMA_FN(:)
ZEMISG(:)     = ZSIGMA_FA(:)*XEMISVEG + (1.0-ZSIGMA_FA(:))*XEMISSOIL
!
ZPN(:)        = PPSN(:) + PPSNA(:)*(1.0-PPSN(:))
ZSIGMA_FA(: ) = (1.-ZPN(:))*PSIGMA_F(:) + ZPN(:)*PSIGMA_FN(:)
ZEMISN(:)     = ZSIGMA_FA(:)*XEMISVEG + (1.0-ZSIGMA_FA(:))*PEMIS_N(:)
!
PEMIS(:)      = (1.0-PPSN(:))*ZEMISG(:) + PPSN(:)*ZEMISN(:)
!
!
IF (LHOOK) CALL DR_HOOK('ISBA_EMIS_MEB',1,ZHOOK_HANDLE)
!
END SUBROUTINE ISBA_EMIS_MEB

