!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     ######################
      MODULE MODD_DIAG_IDEAL_n
!     ######################
!
!!****  *MODD_DIAG_IDEAL - declaration of diagnostics for IDEAL scheme
!!
!!    PURPOSE
!!    -------
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      P. Le Moigne   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       04/2009
!!      P. Le Moigne 03/2015: add diagnostics IDEAL case
!
!*       0.   DECLARATIONS
!             ------------
!
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

TYPE DIAG_IDEAL_t
!------------------------------------------------------------------------------
!
  REAL    :: XDIAG_TSTEP  ! time step for diagnostics writing
!
  INTEGER :: N2M          ! flag for 2 meters (and 10 meters) quantities
  LOGICAL :: L2M_MIN_ZS   ! flag for 2 meters quantities evaluated on
!                         ! the minimum orographyy of the grid
  LOGICAL :: LSURF_BUDGET ! flag for surface energy budget
  LOGICAL :: LRAD_BUDGET  ! flag for radiative energy budget
  LOGICAL :: LCOEF        ! flag for transfer coefficients
  LOGICAL :: LSURF_VARS   ! flag for surface variables
  LOGICAL :: LSURF_BUDGETC       ! flag for surface cumulated energy budget
  LOGICAL :: LRESET_BUDGETC      ! flag for surface cumulated energy budget

!* averaged variables
!
  REAL, POINTER, DIMENSION(:)   :: XRI      ! Bulk-Richardson number           (-)
  REAL, POINTER, DIMENSION(:)   :: XCD      ! drag coefficient for wind        (W/s2)
  REAL, POINTER, DIMENSION(:)   :: XCH      ! drag coefficient for heat        (W/s)
  REAL, POINTER, DIMENSION(:)   :: XCE      ! drag coefficient for vapor       (W/s/K)
  REAL, POINTER, DIMENSION(:)   :: XZ0      ! roughness length for momentum    (m)
  REAL, POINTER, DIMENSION(:)   :: XZ0H     ! roughness length for heat        (m)
  REAL, POINTER, DIMENSION(:)   :: XRN      ! net radiation at surface         (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XH       ! sensible heat flux               (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XLE      ! latent heat flux                 (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XLEI     ! sublimation latent heat flux     (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XGFLUX   ! net soil-vegetation flux         (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XEVAP    ! total evaporation                (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XSUBL    ! sublimation                      (kg/m2/s)
  REAL, POINTER, DIMENSION(:)   :: XT2M     ! air temperature at 2 meters      (K)
  REAL, POINTER, DIMENSION(:)   :: XT2M_MIN ! Minimum air temperature at 2 meters      (K)
  REAL, POINTER, DIMENSION(:)   :: XT2M_MAX ! Maximum air temperature at 2 meters      (K)
  REAL, POINTER, DIMENSION(:)   :: XQ2M     ! air humidity at 2 meters         (kg/kg)
  REAL, POINTER, DIMENSION(:)   :: XHU2M    ! relative humidity at 2 meters    (-)
  REAL, POINTER, DIMENSION(:)   :: XHU2M_MIN! Minimum relative humidity at 2 meters    (-)
  REAL, POINTER, DIMENSION(:)   :: XHU2M_MAX! Maximum relative humidity at 2 meters    (-)
  REAL, POINTER, DIMENSION(:)   :: XQS      ! humidity at surface              (kg/kg)
  REAL, POINTER, DIMENSION(:)   :: XZON10M  ! zonal wind at 10 meters          (m/s)
  REAL, POINTER, DIMENSION(:)   :: XMER10M  ! meridian wind at 10 meters       (m/s)
  REAL, POINTER, DIMENSION(:)   :: XWIND10M ! wind at 10 meters                (m/s)
  REAL, POINTER, DIMENSION(:)   :: XWIND10M_MAX! Maximum wind at 10 meters     (m/s)
  REAL, POINTER, DIMENSION(:)   :: XLWD     ! downward long wave radiation     (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XLWU     ! upward long wave radiation       (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XSWD     ! downward short wave radiation    (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XSWU     ! upward short wave radiation      (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XSWBD    ! downward short wave radiation by spectral band   (W/m2)
  REAL, POINTER, DIMENSION(:,:) :: XSWBU    ! upward short wave radiation by spectral band (W/m2)
  REAL, POINTER, DIMENSION(:)   :: XFMU     ! horizontal momentum flux zonal   (m2/s2)
  REAL, POINTER, DIMENSION(:)   :: XFMV     ! horizontal momentum flux meridian (m2/s2)
  REAL, POINTER, DIMENSION(:)   :: XDIAG_TS ! water surface temperature (K)
  REAL, POINTER, DIMENSION(:)   :: XALBT    ! Total Albedo
  REAL, POINTER, DIMENSION(:)   :: XSWE     ! snow water equivalent (kg/m2)
!
!* cumulated averaged variables
!
  REAL, POINTER, DIMENSION(:)   :: XRNC     ! net radiation at surface         (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XHC      ! sensible heat flux               (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XLEC     ! total latent heat flux           (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XLEIC    ! sublimation latent heat flux     (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XGFLUXC  ! net soil-vegetation flux         (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XEVAPC   ! total evaporation                (kg/m2)
  REAL, POINTER, DIMENSION(:)   :: XSUBLC   ! sublimation                      (kg/m2)
  REAL, POINTER, DIMENSION(:)   :: XLWDC    ! downward long wave radiation     (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XLWUC    ! upward long wave radiation       (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XSWDC    ! downward short wave radiation    (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XSWUC    ! upward short wave radiation      (J/m2)
  REAL, POINTER, DIMENSION(:)   :: XFMUC    ! horizontal momentum flux zonal    (kg/ms)
  REAL, POINTER, DIMENSION(:)   :: XFMVC    ! horizontal momentum flux meridian (kg/ms)
!
!------------------------------------------------------------------------------
!

END TYPE DIAG_IDEAL_t



CONTAINS

!




SUBROUTINE DIAG_IDEAL_INIT(YDIAG_IDEAL)
TYPE(DIAG_IDEAL_t), INTENT(INOUT) :: YDIAG_IDEAL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_DIAG_IDEAL_N:DIAG_IDEAL_INIT",0,ZHOOK_HANDLE)
  NULLIFY(YDIAG_IDEAL%XRI)
  NULLIFY(YDIAG_IDEAL%XCD)
  NULLIFY(YDIAG_IDEAL%XCH)
  NULLIFY(YDIAG_IDEAL%XCE)
  NULLIFY(YDIAG_IDEAL%XZ0)
  NULLIFY(YDIAG_IDEAL%XZ0H)
  NULLIFY(YDIAG_IDEAL%XRN)
  NULLIFY(YDIAG_IDEAL%XH)
  NULLIFY(YDIAG_IDEAL%XLE)
  NULLIFY(YDIAG_IDEAL%XLEI)
  NULLIFY(YDIAG_IDEAL%XGFLUX)
  NULLIFY(YDIAG_IDEAL%XEVAP)
  NULLIFY(YDIAG_IDEAL%XSUBL)
  NULLIFY(YDIAG_IDEAL%XT2M)
  NULLIFY(YDIAG_IDEAL%XT2M_MIN)
  NULLIFY(YDIAG_IDEAL%XT2M_MAX)
  NULLIFY(YDIAG_IDEAL%XQ2M)
  NULLIFY(YDIAG_IDEAL%XHU2M)
  NULLIFY(YDIAG_IDEAL%XHU2M_MIN)
  NULLIFY(YDIAG_IDEAL%XHU2M_MAX)
  NULLIFY(YDIAG_IDEAL%XQS)
  NULLIFY(YDIAG_IDEAL%XZON10M)
  NULLIFY(YDIAG_IDEAL%XMER10M)
  NULLIFY(YDIAG_IDEAL%XWIND10M)
  NULLIFY(YDIAG_IDEAL%XWIND10M_MAX)
  NULLIFY(YDIAG_IDEAL%XLWD)
  NULLIFY(YDIAG_IDEAL%XLWU)
  NULLIFY(YDIAG_IDEAL%XSWD)
  NULLIFY(YDIAG_IDEAL%XSWU)
  NULLIFY(YDIAG_IDEAL%XSWBD)
  NULLIFY(YDIAG_IDEAL%XSWBU)
  NULLIFY(YDIAG_IDEAL%XFMU)
  NULLIFY(YDIAG_IDEAL%XFMV)
  NULLIFY(YDIAG_IDEAL%XDIAG_TS)
  NULLIFY(YDIAG_IDEAL%XALBT)
  NULLIFY(YDIAG_IDEAL%XSWE)
  NULLIFY(YDIAG_IDEAL%XRNC)
  NULLIFY(YDIAG_IDEAL%XHC)
  NULLIFY(YDIAG_IDEAL%XLEC)
  NULLIFY(YDIAG_IDEAL%XLEIC)
  NULLIFY(YDIAG_IDEAL%XGFLUXC)
  NULLIFY(YDIAG_IDEAL%XEVAPC)
  NULLIFY(YDIAG_IDEAL%XSUBLC)
  NULLIFY(YDIAG_IDEAL%XLWDC)
  NULLIFY(YDIAG_IDEAL%XLWUC)
  NULLIFY(YDIAG_IDEAL%XSWDC)
  NULLIFY(YDIAG_IDEAL%XSWUC)
  NULLIFY(YDIAG_IDEAL%XFMUC)
  NULLIFY(YDIAG_IDEAL%XFMVC)
YDIAG_IDEAL%XDIAG_TSTEP=0.
YDIAG_IDEAL%N2M=0
YDIAG_IDEAL%L2M_MIN_ZS=.FALSE.
YDIAG_IDEAL%LSURF_BUDGET=.FALSE.
YDIAG_IDEAL%LRAD_BUDGET=.FALSE.
YDIAG_IDEAL%LCOEF=.FALSE.
YDIAG_IDEAL%LSURF_VARS=.FALSE.
YDIAG_IDEAL%LSURF_BUDGETC=.FALSE.
YDIAG_IDEAL%LRESET_BUDGETC=.FALSE.
IF (LHOOK) CALL DR_HOOK("MODD_DIAG_IDEAL_N:DIAG_IDEAL_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE DIAG_IDEAL_INIT


END MODULE MODD_DIAG_IDEAL_n
