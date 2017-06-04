!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
       SUBROUTINE DIAG_INLINE_SURF_ATM_n (DGO, D, PHW, PHT, PPS, PRHOA, PTRAD, PEMIS, PSFU, PSFV, PSFCO2)
!     ###############################################################################!
!!****  *DIAG_INLINE_SURF_ATM_n * - Computes diagnostics during SURF_ATM time-step
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
!!     P. LeMoigne
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    02/2006
!!------------------------------------------------------------------
!

!
!
!
!
USE MODD_DIAG_n, ONLY : DIAG_t, DIAG_OPTIONS_t
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DGO
TYPE(DIAG_t), INTENT(INOUT) :: D
!
REAL, DIMENSION(:), INTENT(IN)       :: PHW    ! atmospheric level height for wind
REAL, DIMENSION(:), INTENT(IN)       :: PHT    ! atmospheric level height
REAL, DIMENSION(:), INTENT(IN)       :: PPS    ! surface pressure
REAL, DIMENSION(:), INTENT(IN)       :: PRHOA  ! air density
REAL, DIMENSION(:), INTENT(IN)       :: PTRAD  ! radiative temperature at t (K)
REAL, DIMENSION(:), INTENT(IN)       :: PEMIS  ! emissivity at t (-)
REAL, DIMENSION(:), INTENT(IN)       :: PSFU   ! zonal momentum flux                   (Pa)
REAL, DIMENSION(:), INTENT(IN)       :: PSFV   ! meridian momentum flux                (Pa)
REAL, DIMENSION(:), INTENT(IN)       :: PSFCO2 ! CO2 flux                              (kg/m2/s)
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      0.2    declarations of local variables
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_INLINE_SURF_ATM_N',0,ZHOOK_HANDLE)
IF (DGO%LCOEF) THEN
  D%XUREF = PHW
  D%XZREF = PHT
END IF
!
D%XRHOA = PRHOA
D%XPS   = PPS
D%XTRAD = PTRAD
D%XEMIS = PEMIS
!
D%XSSO_FMU   = PSFU
D%XSSO_FMV   = PSFV
!
D%XSFCO2 = PSFCO2
!
IF (LHOOK) CALL DR_HOOK('DIAG_INLINE_SURF_ATM_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_INLINE_SURF_ATM_n
