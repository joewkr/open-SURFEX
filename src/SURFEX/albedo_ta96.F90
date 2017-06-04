!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      FUNCTION ALBEDO_TA96(PZENITH) RESULT(PDIR_ALB)
!     ##################################################################
!
!!****  *ALBEDO_TA96*  
!!
!!    PURPOSE
!!    -------
!       computes the direct albedo over open water
!
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!      V. Masson           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    19/09/03
!!                  20/08/14 R. Séférian correction of the zenith solar angle
!       
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_WATER_PAR,  ONLY : XALBCOEF_TA96
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!              -------------------------
!
REAL, DIMENSION(:), INTENT(IN)  :: PZENITH     ! zenithal angle (radian)
!
REAL, DIMENSION(SIZE(PZENITH))  :: PDIR_ALB    ! direct albedo on water
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ALBEDO_TA96',0,ZHOOK_HANDLE)
PDIR_ALB(:) = XALBCOEF_TA96/(1.1*(MAX(COS(PZENITH(:)),0.))**1.4+0.15)
IF (LHOOK) CALL DR_HOOK('ALBEDO_TA96',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END FUNCTION ALBEDO_TA96
