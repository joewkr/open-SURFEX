!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!##########################################################################
SUBROUTINE SEAFLUX_ALBEDO(PDIR_SW,PSCA_SW,PDIR_ALB,PSCA_ALB,PALB)
!##########################################################################
!
!!****  *SEAFLUX_ALBEDO*  
!!
!!    PURPOSE
!!    -------
!
!     Calculates  total sea albedo 
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
!!      B. Decharme           * Meteo-France *
!!
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
!
REAL, DIMENSION(:)  , INTENT(OUT)  :: PALB               ! albedo 
!
!-------------------------------------------------------------------------------
!
!*      0.     Local variables
!              ---------------
!
REAL, DIMENSION(SIZE(PDIR_SW,1)) :: ZSW_UP, ZGLOBAL_SW
!
INTEGER              :: INI, JI, ISW, JSWB
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*      1.     surface albedo for each wavelength
!              ----------------------------------
!
IF (LHOOK) CALL DR_HOOK('SEAFLUX_ALBEDO',0,ZHOOK_HANDLE)
!
INI=SIZE(PDIR_SW,1)
ISW=SIZE(PDIR_SW,2)
!
!* total shortwave incoming and upcoming radiation
!
ZGLOBAL_SW(:) = 0.
ZSW_UP    (:) = 0.
!
DO JI=1,INI
   DO JSWB=1,ISW
     ZGLOBAL_SW(JI) = ZGLOBAL_SW(JI) + PDIR_SW (JI,JSWB) + PSCA_SW(JI,JSWB)
     ZSW_UP    (JI) = ZSW_UP    (JI) + PDIR_ALB(JI,JSWB) * PDIR_SW(JI,JSWB) &
                                     + PSCA_ALB(JI,JSWB) * PSCA_SW(JI,JSWB)  
  END DO
END DO
!
!* global albedo
!
WHERE(ZGLOBAL_SW(:)>0.)  
     PALB(:) = ZSW_UP(:) / ZGLOBAL_SW(:)
ELSEWHERE
     PALB(:) = PDIR_ALB(:,1)
END WHERE
!
IF (LHOOK) CALL DR_HOOK('SEAFLUX_ALBEDO',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SEAFLUX_ALBEDO
