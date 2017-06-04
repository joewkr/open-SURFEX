!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE DEFAULT_IDEAL_FLUX(KFORCF, KFORCT, PTIMEF, PTIMET,   &
                                    PSFTH, PSFTQ, PSFCO2,             &
                                    HUSTARTYPE, PUSTAR, PZ0M, PALB,   &
                                    PEMIS, PTSRAD)
!     ########################################################################
!
!!****  *DEFAULT_IDEAL_FLUX* - routine to set default values for the configuration for ISBA FLUX assimilation scheme
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
!!      L. Jarlan  *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    02/2005
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_CSTS,       ONLY : XTT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
INTEGER, INTENT(OUT) :: KFORCF
INTEGER, INTENT(OUT) :: KFORCT
REAL, DIMENSION(:), INTENT(OUT)   :: PTIMEF
REAL, DIMENSION(:), INTENT(OUT)   :: PTIMET
REAL, DIMENSION(:), INTENT(OUT)   :: PSFTH      ! hourly data of heat surface flux        (W/m2)
REAL, DIMENSION(:), INTENT(OUT)   :: PSFTQ      ! hourly data of water vapor surface flux (kg/m2/s) or (W/m2)
REAL, DIMENSION(:), INTENT(OUT)   :: PSFCO2     ! hourly data of CO2 surface flux         (kg/m2/s)
 CHARACTER(LEN=5), INTENT(OUT)     :: HUSTARTYPE ! type of computation for friction
                                                ! 'USTAR'
                                                ! 'Z0   '
REAL, DIMENSION(:), INTENT(OUT)   :: PUSTAR     ! hourly data of friction                 (m2/s2)
REAL, INTENT(OUT)                 :: PZ0M       ! roughness length (m)
REAL, INTENT(OUT)                 :: PALB       ! albedo (-)
REAL, INTENT(OUT)                 :: PEMIS      ! emissivity (-)
REAL, DIMENSION(:),INTENT(OUT)    :: PTSRAD     ! radiative temperature (K)
!
!*       0.2   declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('DEFAULT_IDEAL_FLUX',0,ZHOOK_HANDLE)
!
KFORCF = 2.
KFORCT = 2.
!
PTIMEF(1) = 0.
PTIMET(1) = 0.
PTIMEF(2) = XUNDEF
PTIMET(2) = XUNDEF

!
!----------------------------------------------------------------------------------
!
!*       1.    HOURLY surface theta flux (NFORC+1 values from 00UTC to 24UTC)
!               -------------------------
!
!* unit: W/m2
!
PSFTH(:) = 0.
!
!----------------------------------------------------------------------------------
!
!*       2.    HOURLY surface vapor mixing ratio flux (NFORC+1 values from 00UTC to 24UTC)
!              --------------------------------------
!
!* unit: kg/m2/s
!
PSFTQ(:) = 0.
!
!----------------------------------------------------------------------------------
!
!*       4.    HOURLY surface CO2 flux (NFORC+1 values from 00UTC to 24UTC)
!              -----------------------
!
!* unit: kg/m2/s
!
PSFCO2(:) = 0.
!
!----------------------------------------------------------------------------------
!
!*       5.    Type of definition for friction fluxes
!              --------------------------------------
!
!* HUSTARTYPE = 'Z0   '  ! friction is defined using a roughness length formulation
!*            = 'USTAR'  ! friction is prescribed via the friction velocity u*
!
HUSTARTYPE = 'Z0   '
!
!----------------------------------------------------------------------------------
!
!*       6.    Roughness length (used if XUSTARTYPE = 'Z0   ')
!              ----------------
!
PZ0M = 0.01              ! unit in meters
!      
!----------------------------------------------------------------------------------
!
!*       6.    Friction (used if XUSTARTYPE = 'USTAR')
!              --------
!
!* unit: m2/s2
!
PUSTAR = 0.
!
!
!----------------------------------------------------------------------------------
!
!*       7.    HOURLY surface radiative temperature (NFORC+1 values from 00UTC to 24UTC)
!               -------------------------
!

PTSRAD(:) = XTT            ! radiative surface temperature, (unit is K)
!
!----------------------------------------------------------------------------------
!
!*       8.    Radiative fields constant and uniform values
!              --------------------------------------------
!
PALB      = 0.             ! albedo, no unit
PEMIS     = 1.             ! emissivity, no unit
!      
IF (LHOOK) CALL DR_HOOK('DEFAULT_IDEAL_FLUX',1,ZHOOK_HANDLE)
!
END SUBROUTINE DEFAULT_IDEAL_FLUX
