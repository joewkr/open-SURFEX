!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ####################################################################
      SUBROUTINE SOIL_ALBEDO(HALBEDO, PWSAT, PWG1, KK, PEK, HBAND)  
!     ####################################################################
!
!!****  *SOIL_ALBEDO*  
!!
!!    PURPOSE
!!    -------
!  computes the SOIL_ALBEDO of for different types (patches) 
! of natural continental parts.
!
! Soil SOIL_ALBEDO is estimated from sand fraction.
! A correction due to the soil humidity can be used.
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
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_n, ONLY : ISBA_K_t, ISBA_PE_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
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
! SOIL_ALBEDO dependance wxith surface soil water content
!   "EVOL" = SOIL_ALBEDO evolves with soil wetness
!   "DRY " = constant SOIL_ALBEDO value for dry soil
!   "WET " = constant SOIL_ALBEDO value for wet soil
!   "MEAN" = constant SOIL_ALBEDO value for medium soil wetness
!
REAL, DIMENSION(:), INTENT(IN)    :: PWSAT       ! saturation water content
REAL, DIMENSION(:), INTENT(IN)  :: PWG1        ! surface water content
!
TYPE(ISBA_K_t), INTENT(INOUT) :: KK
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
!
 CHARACTER(LEN=*), INTENT(IN) :: HBAND
!
!*      0.2    declarations of local variables
!              -------------------------------
!
REAL,    DIMENSION(SIZE(PWSAT)) :: ZX
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('SOIL_ALBEDO',0,ZHOOK_HANDLE)
IF (HALBEDO=='USER' .AND. LHOOK) CALL DR_HOOK('SOIL_ALBEDO',1,ZHOOK_HANDLE)
IF (HALBEDO=='USER') RETURN
!
IF (TRIM(HBAND)=="VIS".OR.TRIM(HBAND)=="ALL") PEK%XALBVIS_SOIL = XUNDEF
IF (TRIM(HBAND)=="NIR".OR.TRIM(HBAND)=="ALL") PEK%XALBNIR_SOIL = XUNDEF
IF (TRIM(HBAND)=="UV" .OR.TRIM(HBAND)=="ALL") PEK%XALBUV_SOIL  = XUNDEF
!
SELECT CASE ( HALBEDO )
CASE ('EVOL')

    ZX = MIN( PWG1(:)/PWSAT(:) , 1. )

    IF (TRIM(HBAND)=="VIS".OR.TRIM(HBAND)=="ALL") &
      WHERE (PWG1(:)/=XUNDEF) &
        PEK%XALBVIS_SOIL(:) = KK%XALBVIS_WET(:) + &
            (0.25*KK%XALBVIS_DRY(:)-KK%XALBVIS_WET(:)) * (1. - ZX(:)) * &
       ( ZX(:) + (KK%XALBVIS_DRY(:)-KK%XALBVIS_WET(:)) / (0.25*KK%XALBVIS_DRY(:)-KK%XALBVIS_WET(:)) )  
    IF (TRIM(HBAND)=="NIR".OR.TRIM(HBAND)=="ALL") &
      WHERE (PWG1(:)/=XUNDEF) &      
        PEK%XALBNIR_SOIL(:) = KK%XALBNIR_WET(:) + &
           (0.25*KK%XALBNIR_DRY(:)-KK%XALBNIR_WET(:)) * (1. - ZX(:)) * &
      ( ZX(:) + (KK%XALBNIR_DRY(:)-KK%XALBNIR_WET(:)) / (0.25*KK%XALBNIR_DRY(:)-KK%XALBNIR_WET(:)) )  
    IF (TRIM(HBAND)=="UV".OR.TRIM(HBAND)=="ALL") &
      WHERE (PWG1(:)/=XUNDEF) &      
        PEK%XALBUV_SOIL (:) = KK%XALBUV_WET (:) + &
           (0.25*KK%XALBUV_DRY (:)-KK%XALBUV_WET (:)) * (1. - ZX(:)) * &
      ( ZX(:) + (KK%XALBUV_DRY (:)-KK%XALBUV_WET (:)) / (0.25*KK%XALBUV_DRY (:)-KK%XALBUV_WET (:)) )  

    !END WHERE

CASE ('DRY ')
  IF (TRIM(HBAND)=="VIS".OR.TRIM(HBAND)=="ALL") &
    WHERE (PWG1(:)/=XUNDEF) PEK%XALBVIS_SOIL(:) = KK%XALBVIS_DRY(:)
  IF (TRIM(HBAND)=="NIR".OR.TRIM(HBAND)=="ALL") &
    WHERE (PWG1(:)/=XUNDEF) PEK%XALBNIR_SOIL(:) = KK%XALBNIR_DRY(:)
  IF (TRIM(HBAND)=="UV".OR.TRIM(HBAND)=="ALL") &
    WHERE (PWG1(:)/=XUNDEF) PEK%XALBUV_SOIL (:) = KK%XALBUV_DRY (:)

CASE ('WET ')
  IF (TRIM(HBAND)=="VIS".OR.TRIM(HBAND)=="ALL") &
    WHERE (PWG1(:)/=XUNDEF) PEK%XALBVIS_SOIL(:) = KK%XALBVIS_WET(:)
  IF (TRIM(HBAND)=="NIR".OR.TRIM(HBAND)=="ALL") &
    WHERE (PWG1(:)/=XUNDEF) PEK%XALBNIR_SOIL(:) = KK%XALBNIR_WET(:)
  IF (TRIM(HBAND)=="UV".OR.TRIM(HBAND)=="ALL") &
    WHERE (PWG1(:)/=XUNDEF) PEK%XALBUV_SOIL (:) = KK%XALBUV_WET (:)

CASE ('MEAN')
  IF (TRIM(HBAND)=="VIS".OR.TRIM(HBAND)=="ALL") &
    WHERE (PWG1(:)/=XUNDEF) PEK%XALBVIS_SOIL(:) = 0.5 * ( KK%XALBVIS_DRY(:) + KK%XALBVIS_WET(:) )
  IF (TRIM(HBAND)=="NIR".OR.TRIM(HBAND)=="ALL") &
    WHERE (PWG1(:)/=XUNDEF) PEK%XALBNIR_SOIL(:) = 0.5 * ( KK%XALBNIR_DRY(:) + KK%XALBNIR_WET(:) )
  IF (TRIM(HBAND)=="UV".OR.TRIM(HBAND)=="ALL") &
    WHERE (PWG1(:)/=XUNDEF) PEK%XALBUV_SOIL (:) = 0.5 * ( KK%XALBUV_DRY (:) + KK%XALBUV_WET (:) )

END SELECT
IF (LHOOK) CALL DR_HOOK('SOIL_ALBEDO',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SOIL_ALBEDO
