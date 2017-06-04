!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_SFX_LAKE (F, U, &
                               PLAKE_EVAP,PLAKE_RAIN,PLAKE_SNOW,PLAKE_WATF)  
!     ############################################################################
!
!!****  *GET_SFX_LAKE* - routine to get some variables from surfex to
!                        a oceanic general circulation model
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      B. Decharme      *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2013
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_FLAKE_n, ONLY : FLAKE_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
!
!
USE MODI_UNPACK_SAME_RANK
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(FLAKE_t), INTENT(INOUT) :: F
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
REAL, DIMENSION(:), INTENT(OUT) :: PLAKE_EVAP  ! Cumulated Evaporation             (kg/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PLAKE_RAIN  ! Cumulated Rainfall rate           (kg/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PLAKE_SNOW  ! Cumulated Snowfall rate           (kg/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PLAKE_WATF  ! Cumulated Net water flux          (kg/m2)
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('GET_SFX_LAKE',0,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
!*       1.0   Initialization
!              --------------
!
PLAKE_EVAP (:) = XUNDEF
PLAKE_RAIN (:) = XUNDEF
PLAKE_SNOW (:) = XUNDEF
PLAKE_WATF (:) = XUNDEF
!
!*       2.0   Get variable over lake
!              ----------------------
!
IF(U%NSIZE_WATER>0)THEN
!
  CALL UNPACK_SAME_RANK(U%NR_WATER(:),F%XCPL_FLAKE_EVAP(:),PLAKE_EVAP(:),XUNDEF)
  CALL UNPACK_SAME_RANK(U%NR_WATER(:),F%XCPL_FLAKE_RAIN(:),PLAKE_RAIN(:),XUNDEF)
  CALL UNPACK_SAME_RANK(U%NR_WATER(:),F%XCPL_FLAKE_SNOW(:),PLAKE_SNOW(:),XUNDEF)
  F%XCPL_FLAKE_EVAP(:) = 0.0
  F%XCPL_FLAKE_RAIN(:) = 0.0
  F%XCPL_FLAKE_SNOW(:) = 0.0
!
  PLAKE_WATF(:) =  PLAKE_RAIN(:) + PLAKE_SNOW(:) - PLAKE_EVAP(:)
!
ENDIF
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('GET_SFX_LAKE',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_SFX_LAKE
