!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ######spl
      FUNCTION ALBEDO_MK10(PZENITH) RESULT(PDIR_ALB)
!     ##################################################################
!
!!****  *ALBEDO_MK10*  
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
!!    Modified version taken from SAM6.8 (Oct 2010) Marat Khairoutdinov 
!!    from CCM3). For the EUCLIPSE COMPOSITE CASE.
!!
!!      
!!    AUTHOR
!!    ------
!!      E. Bazile           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    28/03/11
!       
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
!              -------------------------
!
REAL, DIMENSION(:), INTENT(IN)  :: PZENITH     ! zenithal angle (radian)
!
REAL, DIMENSION(SIZE(PZENITH))  :: PDIR_ALB    ! direct albedo on water
REAL, DIMENSION(SIZE(PZENITH))  :: ZCOSZRS     ! Cosine of solar zenith angle
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ALBEDO_MK10',0,ZHOOK_HANDLE)
ZCOSZRS(:)  = MAX(COS(PZENITH(:)),0.)
PDIR_ALB(:) = ( 0.026 / (ZCOSZRS**1.7 + .065)) + &
              (.15*(ZCOSZRS - 0.10) * (ZCOSZRS - 0.50) * (ZCOSZRS - 1.00) )
IF (LHOOK) CALL DR_HOOK('ALBEDO_MK10',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END FUNCTION ALBEDO_MK10
