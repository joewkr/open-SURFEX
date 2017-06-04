!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!   #############
FUNCTION CONTROL_MOIST_FUNC (PMOIST,PSAT) RESULT (PMOISTFUNC)

!   ###############################################################
!!**   CONTROL_MOIST_FUNC
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!     Moisture control factor for decomposition.
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
!!      Modified for Wfc < W < Wsat following Probert et al., Agricultural Systems, 1998
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
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:), INTENT(IN)   :: PMOIST ! soil moisture index (-)
REAL, DIMENSION(:), INTENT(IN)   :: PSAT   ! soil saturated index (-)
!
!*      0.2    declarations of local variables
!
REAL, PARAMETER               :: ZMOIST_LIM = 0.05
REAL, PARAMETER               :: ZSAT_LIM   = 0.5
!
REAL, PARAMETER               :: ZCOEF1 = 2.40
REAL, PARAMETER               :: ZCOEF2 = 1.10
REAL, PARAMETER               :: ZCOEF3 = 0.29
!
REAL, DIMENSION(SIZE(PMOIST)) :: PMOISTFUNC    ! moisture control factor
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      1.0    Calculates moisture control factor
!
IF (LHOOK) CALL DR_HOOK('CONTROL_MOIST_FUNC',0,ZHOOK_HANDLE)
!
WHERE(PMOIST(:)<=1.0)
      PMOISTFUNC(:)=MIN(1.0,ZCOEF1*PMOIST(:)-ZCOEF2*PMOIST(:)*PMOIST(:)-ZCOEF3)
      PMOISTFUNC(:)=MAX(ZMOIST_LIM,PMOISTFUNC(:))
ELSEWHERE
      PMOISTFUNC(:)=MAX(ZSAT_LIM,1.0-0.5*PSAT(:))
ENDWHERE
!
IF (LHOOK) CALL DR_HOOK('CONTROL_MOIST_FUNC',1,ZHOOK_HANDLE)

END FUNCTION CONTROL_MOIST_FUNC
