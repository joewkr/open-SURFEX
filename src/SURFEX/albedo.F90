!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ####################################################################
      SUBROUTINE ALBEDO(HALBEDO, PEK, PSNOW, OMASK    )  
!     ####################################################################
!
!!****  *ALBEDO*  
!!
!!    PURPOSE
!!    -------
!  computes the albedo of for different types (patches) 
! of natural continental parts, from
! vegetation albedo and soil albedo.
! Soil albedo is estimated from sand fraction.
! A correction due to the soil humidity is used.
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
!!      F.Solmon  /  V. Masson          
!!
!!    MODIFICATIONS
!!    -------------
!!      Original     
!!                  01/2004  Externalization (V. Masson)
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_n, ONLY : ISBA_PE_t
!
USE MODD_DATA_COVER_PAR, ONLY : NVT_SNOW
USE MODD_SNOW_PAR,       ONLY : XANSMAX
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=*),       INTENT(IN)   :: HALBEDO
! Albedo dependance wxith surface soil water content
!   "EVOL" = albedo evolves with soil wetness
!   "DRY " = constant albedo value for dry soil
!   "WET " = constant albedo value for wet soil
!   "MEAN" = constant albedo value for medium soil wetness
!
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
!
REAL,    DIMENSION(:), INTENT(IN), OPTIONAL  :: PSNOW ! fraction of permanent snow and ice
LOGICAL, DIMENSION(:),   INTENT(IN), OPTIONAL :: OMASK    ! mask where computations are done
!
!*      0.2    declarations of local variables
!              -------------------------------
!
LOGICAL, DIMENSION(SIZE(PEK%XVEG)) :: GMASK
!
REAL, DIMENSION(SIZE(PEK%XVEG)) :: ZSNOW
INTEGER :: JP     !loop index for patches
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ALBEDO',0,ZHOOK_HANDLE)
IF (HALBEDO=='USER' .AND. LHOOK) CALL DR_HOOK('ALBEDO',1,ZHOOK_HANDLE)
IF (HALBEDO=='USER') RETURN
!
GMASK(:) = .TRUE.
IF (PRESENT(OMASK)) GMASK(:) = OMASK(:)
!
WHERE (GMASK(:))
  PEK%XALBVIS (:) = XUNDEF
  PEK%XALBNIR (:) = XUNDEF
  PEK%XALBUV  (:) = XUNDEF
END WHERE
!
ZSNOW(:) = 0.
IF (PRESENT(PSNOW)) ZSNOW(:) = PSNOW(:)
!
WHERE (GMASK(:) .AND. PEK%XVEG(:)/=XUNDEF)

  PEK%XALBVIS(:) = ( (1.-PEK%XVEG(:)) * PEK%XALBVIS_SOIL(:) + PEK%XVEG(:)  * PEK%XALBVIS_VEG (:)) &
         * (1-ZSNOW(:)) + XANSMAX  * ZSNOW(:)   
  !
  PEK%XALBNIR(:) = ( (1.-PEK%XVEG(:)) * PEK%XALBNIR_SOIL(:) + PEK%XVEG(:)  * PEK%XALBNIR_VEG (:)) &
         * (1-ZSNOW(:)) + XANSMAX  * ZSNOW(:)   
  !
  PEK%XALBUV (:) = ( (1.-PEK%XVEG(:)) * PEK%XALBUV_SOIL (:) + PEK%XVEG(:)  * PEK%XALBUV_VEG  (:)) &
         * (1-ZSNOW(:)) + XANSMAX  * ZSNOW(:)   
END WHERE
!
IF (LHOOK) CALL DR_HOOK('ALBEDO',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE ALBEDO
