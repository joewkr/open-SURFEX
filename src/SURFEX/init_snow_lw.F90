!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE INIT_SNOW_LW(PEMISSN,TPSNOW)
!     #######################################
!
!!****  *INIT_SNOW_LW* - routine to initialize snow surf. temp and emissivity
!!
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
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2003 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_TYPE_SNOW
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_CSTS,       ONLY : XTT, XLMTT
!
USE MODE_SNOW3L
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
REAL,                 INTENT(IN)    :: PEMISSN ! snow emissivity
TYPE(SURF_SNOW),      INTENT(INOUT) :: TPSNOW  ! snow characteristics
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INIT_SNOW_LW',0,ZHOOK_HANDLE)
!
!*       1.    Emissivity
!              ----------
!
IF (TPSNOW%SCHEME=='1-L' .OR. TPSNOW%SCHEME=='3-L' .OR. TPSNOW%SCHEME=='CRO') THEN
  WHERE(TPSNOW%WSNOW(:,1)==0. .OR. TPSNOW%WSNOW(:,1)==XUNDEF )
    TPSNOW%EMIS (:)= XUNDEF
  ELSEWHERE
    TPSNOW%EMIS (:)= PEMISSN
  END WHERE
END IF
!
!*      2.     Surface temperature
!              -------------------
!
IF (TPSNOW%SCHEME=='1-L') THEN
  WHERE(TPSNOW%WSNOW(:,1)==0. .OR. TPSNOW%WSNOW(:,1)==XUNDEF )
    TPSNOW%TS (:)= XUNDEF
  ELSEWHERE
    TPSNOW%TS(:) = TPSNOW%T(:, TPSNOW%NLAYER)
  END WHERE
END IF
!
IF (TPSNOW%SCHEME=='3-L' .OR. TPSNOW%SCHEME=='CRO') THEN
  WHERE(TPSNOW%WSNOW(:,1)==0. .OR. TPSNOW%WSNOW(:,1)==XUNDEF )
    TPSNOW%TS (:)= XUNDEF
  ELSEWHERE
    TPSNOW%TS(:) = XTT + (TPSNOW%HEAT(:,1) + XLMTT * TPSNOW%RHO(:,1)) / SNOW3LSCAP(TPSNOW%RHO(:,1))  
    TPSNOW%TS(:) = MIN(XTT, TPSNOW%TS(:))
  END WHERE
END IF
!
IF (LHOOK) CALL DR_HOOK('INIT_SNOW_LW',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INIT_SNOW_LW
