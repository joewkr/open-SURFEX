!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_VER_WATFLUX (W)
!     #################################################################################
!
!!****  *PREP_VER_WATFLUX* - change in WATFLUX var. due to altitude change
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
!!------------------------------------------------------------------
!

!
!
USE MODD_WATFLUX_n, ONLY : WATFLUX_t
!
USE MODD_PREP,       ONLY : XZS_LS, XT_CLIM_GRAD
USE MODD_CSTS,       ONLY : XTT
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
!*      0.2    declarations of local variables
!
!
TYPE(WATFLUX_t), INTENT(INOUT) :: W
!
REAL, DIMENSION(:), ALLOCATABLE :: ZTS_LS ! large-scale water temperature
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
!*      1.     Surface temperature of water
!
IF (LHOOK) CALL DR_HOOK('PREP_VER_WATFLUX',0,ZHOOK_HANDLE)
ALLOCATE(ZTS_LS(SIZE(W%XTS)))
ZTS_LS = W%XTS
!
W%XTS = ZTS_LS  + XT_CLIM_GRAD  * (W%XZS - XZS_LS)
!
!-------------------------------------------------------------------------------------
!
!*      2.     Limit on freezing
!
!* if water was already frozen, it can be considered liquid only for
!  temperatures that are larger than 4 C
!
WHERE (ZTS_LS < XTT .AND. W%XTS < XTT + 4.) W%XTS = MIN(W%XTS,XTT)
!
!* if water was liquid, it can be considered frozen only for
!  very cold temperatures, colder than 20 C. It should obviously 
!  depend on the size of the lake. This cold limit is taken for
!  large lakes, as US great lakes, that take a long time to freeze.
!
!  4 C is used here for the water of lowest density.
!
WHERE (ZTS_LS > XTT .AND. W%XTS > XTT - 20.) W%XTS = MAX(W%XTS,XTT+4.)
!
!-------------------------------------------------------------------------------------
!
!*      3.     Deallocation of large-scale orography
!
DEALLOCATE(ZTS_LS)
IF (LHOOK) CALL DR_HOOK('PREP_VER_WATFLUX',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_VER_WATFLUX
