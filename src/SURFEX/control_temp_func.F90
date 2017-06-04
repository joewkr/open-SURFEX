!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!   ################
FUNCTION CONTROL_TEMP_FUNC (PTEMP_IN) RESULT (PTEMPFUNC_RESULT)

!
!   ###############################################################
!!**   CONTROL_TEMP_FUNC
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!     Temperature control factor of decomposition.
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    none
!!
!!    REFERENCE
!!    ---------
!!
!!      Krinner et al., Global Biochemical Cycles, 2005
!!      Gibelin et al. 2008, AFM
!!      
!!    AUTHOR
!!    ------
!!
!!      A.-L. Gibelin           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    23/06/09
!!      B. Decharme 05/2012 : Optimization and ISBA-DIF coupling
!!      
!-------------------------------------------------------------------------------

!
!*       0.     DECLARATIONS
!               ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:), INTENT(IN)  :: PTEMP_IN ! temperature (K)
!
!*      0.2    declarations of local variables
!
REAL                            :: ZCOEF1
REAL, PARAMETER                 :: ZCOEF2 = 10.0
REAL, PARAMETER                 :: ZCOEF3 = 30.0
!
REAL, DIMENSION(SIZE(PTEMP_IN)) :: PTEMPFUNC_RESULT ! temperature control factor
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*       1 Calculates temperature control factor
!
IF (LHOOK) CALL DR_HOOK('CONTROL_TEMP_FUNC',0,ZHOOK_HANDLE)
!
ZCOEF1 = LOG(2.0)
!
PTEMPFUNC_RESULT(:) = EXP( (ZCOEF1/ZCOEF2) * (PTEMP_IN(:)-ZCOEF3) )
!
PTEMPFUNC_RESULT(:) = MIN( 1., PTEMPFUNC_RESULT(:) )
!
IF (LHOOK) CALL DR_HOOK('CONTROL_TEMP_FUNC',1,ZHOOK_HANDLE)

END FUNCTION CONTROL_TEMP_FUNC


