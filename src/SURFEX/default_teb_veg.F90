!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE DEFAULT_TEB_VEG(HRUNOFF, HALBEDO, HSCOND,               &
                                 HC1DRY, HSOILFRZ, HDIFSFCOND, HSNOWRES, &
                                 HCPSURF, PCGMAX, HKSAT,                 &
                                 HRAIN, HHORT,                           &
                                 OGLACIER, OCANOPY_DRAG, OVEGUPD,        &
                                 ONITRO_DILU                             )
!     ########################################################################
!
!!****  *DEFAULT_TEB_VEG* - routine to set default values for the configuration for TEB scheme
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
!!      Original                 01/2004
!!      C. de Munck & A. Lemonsu  07/2011 ! urban vegetation options
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
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
!
 CHARACTER(LEN=4),  INTENT(OUT) :: HALBEDO    ! albedo type ('DRY','EVOL4,'WET','USER')
 CHARACTER(LEN=4),  INTENT(OUT) :: HSCOND     ! Thermal conductivity ('DEF','PL98')
 CHARACTER(LEN=4),  INTENT(OUT) :: HC1DRY     ! C1 formulation for dry soils ('DEF','GB93')
 CHARACTER(LEN=3),  INTENT(OUT) :: HSOILFRZ   ! soil freezing-physics option ('DEF','LWT')
 CHARACTER(LEN=4),  INTENT(OUT) :: HDIFSFCOND ! Mulch effects ('MLCH','DEF ')
 CHARACTER(LEN=3),  INTENT(OUT) :: HSNOWRES   ! Turbulent exchanges over snow ('DEF','RIL')
 CHARACTER(LEN=3),  INTENT(OUT) :: HCPSURF    ! specific heat ('DRY','HUM')
 CHARACTER(LEN=4),  INTENT(OUT) :: HRUNOFF    ! surface runoff formulation ('WSAT','DT92','SGH ')
 CHARACTER(LEN=3),  INTENT(OUT) :: HKSAT      ! soil hydraulic profile option ('DEF','SGH')
 CHARACTER(LEN=3),  INTENT(OUT) :: HRAIN      ! Rainfall spatial distribution ('DEF','SGH')
 CHARACTER(LEN=3),  INTENT(OUT) :: HHORT      ! Horton runoff ('DEF','SGH')

LOGICAL, INTENT(OUT)           :: OGLACIER   ! T= Over permanent snow and ice, initialise WGI=WSAT,
!                                                 Hsnow>=3.3m and allow 0.8<SNOALB<0.85
!                                            ! F= No specific treatment
LOGICAL, INTENT(OUT)           :: OCANOPY_DRAG ! T: drag activated in SBL scheme within the canopy
LOGICAL, INTENT(OUT)           :: OVEGUPD      ! T: update vegetation parameters every decade
!                                              ! F: keep vegetation parameters constant in time
REAL,    INTENT(OUT)           :: PCGMAX     ! maximum soil heat capacity
LOGICAL, INTENT(OUT)           :: ONITRO_DILU ! nitrogen dilution fct of CO2 (Calvet et al. 2008)
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_TEB_VEG',0,ZHOOK_HANDLE)
!
HSCOND     = 'PL98'
HALBEDO    = 'DRY '
!
HC1DRY     = 'DEF '
HSOILFRZ   = 'DEF'
HDIFSFCOND = 'DEF '
HSNOWRES   = 'DEF'
HCPSURF    = 'DRY'
!
HRUNOFF    = "WSAT"
HKSAT      = 'DEF'
HRAIN      = 'DEF'
HHORT      = 'DEF'
!
PCGMAX     = 2.0E-5
!
OCANOPY_DRAG = .FALSE.
!
OVEGUPD     = .TRUE.
ONITRO_DILU = .FALSE.
!
OGLACIER  = .FALSE.
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_TEB_VEG',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DEFAULT_TEB_VEG
