!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
       SUBROUTINE DIAG_MISC_FLAKE_n (DMF, F)
!     ###############################################################################
!
!!****  *DIAG_MISC-FLAKE_n * - additional diagnostics for FLake
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     P. Le Moigne 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2005
!!------------------------------------------------------------------
!
!
USE MODD_FLAKE_n, ONLY : FLAKE_t
USE MODD_DIAG_MISC_FLAKE_n, ONLY : DIAG_MISC_FLAKE_t
!
USE MODD_SURF_PAR,           ONLY : XUNDEF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(FLAKE_t), INTENT(INOUT) :: F
TYPE(DIAG_MISC_FLAKE_t), INTENT(INOUT) :: DMF
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(DMF%XZW_PROFILE),SIZE(F%XT_WML)) :: ZCSI      ! Vertical normalized coordinate
REAL, DIMENSION(SIZE(DMF%XZW_PROFILE),SIZE(F%XT_WML)) :: ZSHAPE    ! Shape function
!
INTEGER         :: IZW
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_MISC_FLAKE_N',0,ZHOOK_HANDLE)
!
!* Flake temperature profile
!
DMF%XTW_PROFILE(:,:) = XUNDEF
!
IF (DMF%LWATER_PROFILE) THEN
!
   DO IZW=1,SIZE(DMF%XZW_PROFILE)
      WHERE (F%XWATER_DEPTH(:)==F%XH_ML(:))
         ZCSI(IZW,:) = 0.
      ELSEWHERE
         ZCSI(IZW,:) = (DMF%XZW_PROFILE(IZW) - F%XH_ML(:))/(F%XWATER_DEPTH(:) - F%XH_ML(:))
      END WHERE
      ZSHAPE(IZW,:) = (40./3.*F%XCT-20./3.)*ZCSI(IZW,:)   +     (18.-30.*F%XCT)*ZCSI(IZW,:)**2 &
                       + (20.*F%XCT-12.)   *ZCSI(IZW,:)**3+(5./3.-10./3.*F%XCT)*ZCSI(IZW,:)**4  
   END DO
!
   DO IZW=1,SIZE(DMF%XZW_PROFILE)
      WHERE (F%XH_ML(:) >= DMF%XZW_PROFILE(IZW))
         DMF%XTW_PROFILE(IZW,:) =  F%XT_WML(:) 
      ELSEWHERE (F%XWATER_DEPTH(:) >= DMF%XZW_PROFILE(IZW)) 
         DMF%XTW_PROFILE(IZW,:) = F%XT_WML(:) - (F%XT_WML(:) - F%XT_BOT(:)) * ZSHAPE(IZW,:)
      END WHERE
   END DO
!
END IF
!
IF (LHOOK) CALL DR_HOOK('DIAG_MISC_FLAKE_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_MISC_FLAKE_n
