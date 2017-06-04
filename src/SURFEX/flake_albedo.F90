!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE FLAKE_ALBEDO( PDIR_SW   , PSCA_SW , KSW,      &
                               PDIR_ALB  , PSCA_ALB,           &
                               PGLOBAL_SW, PALB                )
!     ##########################################################################
!
!!****  *FLAKE_ALBEDO*  
!!
!!    PURPOSE
!!    -------
!
!     Calculates  albedo and emissivity 
!         
!!    EXTERNAL
!!    --------
!!
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!      
!!    AUTHOR
!!    ------
!!
!!      P. Le Moigne           * Meteo-France *
!!
!!      Modified by P. Le Moigne - 10/2013 : bug in ZSW_UP declaration
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
USE MODD_SURF_PAR,     ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:), INTENT(IN)   :: PDIR_SW            ! direct incoming solar radiation
REAL, DIMENSION(:,:), INTENT(IN)   :: PSCA_SW            ! diffuse incoming solar radiation
REAL, DIMENSION(:,:), INTENT(IN)   :: PDIR_ALB           ! direct  albedo
REAL, DIMENSION(:,:), INTENT(IN)   :: PSCA_ALB           ! diffuse albedo
INTEGER,              INTENT(IN)   :: KSW                ! number of short-wave spectral bands
!
REAL, DIMENSION(:)  , INTENT(OUT)  :: PGLOBAL_SW         ! global incoming SW rad.
REAL, DIMENSION(:)  , INTENT(OUT)  :: PALB               ! albedo 
!
!-------------------------------------------------------------------------------
!
!*      0.     Local variables
!              ---------------
!
INTEGER                          :: JSWB
REAL, DIMENSION(SIZE(PDIR_SW,1)) :: ZSW_UP
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*      1.     surface albedo for each wavelength
!              ----------------------------------
!
IF (LHOOK) CALL DR_HOOK('FLAKE_ALBEDO',0,ZHOOK_HANDLE)
!
!* total shortwave incoming radiation
!
  PGLOBAL_SW(:) = 0.
  DO JSWB=1,KSW
    PGLOBAL_SW(:) = PGLOBAL_SW(:) + (PDIR_SW(:,JSWB) + PSCA_SW(:,JSWB))
  END DO
!
!* total shortwave upcoming radiation
!
  ZSW_UP(:) = 0. 
  DO JSWB=1,KSW
    ZSW_UP(:) =  ZSW_UP(:)                            &
                 + PDIR_ALB(:,JSWB) * PDIR_SW(:,JSWB) &
                 + PSCA_ALB(:,JSWB) * PSCA_SW(:,JSWB)  
  END DO
!
!* global albedo
!
  WHERE(PGLOBAL_SW(:)>0.)  
       PALB(:) = ZSW_UP(:) / PGLOBAL_SW(:)
  ELSEWHERE
       PALB(:) = PDIR_ALB(:,1)
  END WHERE
!
IF (LHOOK) CALL DR_HOOK('FLAKE_ALBEDO',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE FLAKE_ALBEDO
