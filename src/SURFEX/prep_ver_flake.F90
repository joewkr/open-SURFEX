!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_VER_FLAKE (F)
!     #################################################################################
!
!!****  *PREP_VER_FLAKE* - change in FLAKE var. due to altitude change
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
!!     S. Malardel
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      09.2010, E. Kourzeneva: Make not possible to shift the lake profile
!!                              in vertical, just to shift the lake surface 
!!                              temperature and then to set the default lake profile
!!------------------------------------------------------------------
!

!
!
!
USE MODD_FLAKE_n, ONLY : FLAKE_t
!
USE MODD_PREP,       ONLY : XZS_LS, XT_CLIM_GRAD
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!*      0.2    declarations of local variables
!
!
TYPE(FLAKE_t), INTENT(INOUT) :: F
!
REAL, DIMENSION(:), ALLOCATABLE :: ZTS_LS ! large-scale water temperature
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('PREP_VER_FLAKE',0,ZHOOK_HANDLE)

!       1. Check if the shift is needed at all
IF((ABS(MAXVAL(F%XZS)) < 0.001).AND.(ABS(MINVAL(F%XZS))< 0.001)) &
        CALL DR_HOOK('PREP_VER_FLAKE',1,ZHOOK_HANDLE)
IF((ABS(MAXVAL(F%XZS)) < 0.001).AND.(ABS(MINVAL(F%XZS))< 0.001)) RETURN
!
!*      2.  Shift surface temperature of water
!
ALLOCATE(ZTS_LS(SIZE(F%XTS)))
!
ZTS_LS = F%XTS
!
F%XTS = ZTS_LS  + XT_CLIM_GRAD  * (F%XZS - XZS_LS)
!
DEALLOCATE(ZTS_LS)
!
IF (LHOOK) CALL DR_HOOK('PREP_VER_FLAKE',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_VER_FLAKE
