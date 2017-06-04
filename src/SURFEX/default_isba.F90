!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ######### 
      SUBROUTINE DEFAULT_ISBA(PTSTEP, POUT_TSTEP,                        &
                              HRUNOFF, HSCOND,                           &
                              HC1DRY, HSOILFRZ, HDIFSFCOND, HSNOWRES,    &
                              HCPSURF, PCGMAX, PCDRAG, HKSAT, OSOC,      &
                              HRAIN, HHORT, OGLACIER, OCANOPY_DRAG,      &
                              OVEGUPD, OSPINUPCARBS, OSPINUPCARBW,       &
                              PSPINMAXS, PSPINMAXW, PCO2_START, PCO2_END,&
                              KNBYEARSPINS, KNBYEARSPINW,                &
                              ONITRO_DILU                                )
!     ########################################################################
!
!!****  *DEFAULT_ISBA* - routine to set default values for the configuration for ISBA
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
!!      Original    01/2004 
!!      B.Decharme  04/2013 delete HTOPREG (never used)
!!                          water table / surface coupling 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
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
REAL,              INTENT(OUT) :: PTSTEP     ! time-step for run
REAL,              INTENT(OUT) :: POUT_TSTEP ! time-step for writing
CHARACTER(LEN=4),  INTENT(OUT) :: HSCOND   ! Thermal conductivity
!                                          ! 'DEF ' = DEFault: NP89 implicit method
!                                          ! 'PL98' = Peters-Lidard et al. 1998 used
!                                          ! for explicit computation of CG
CHARACTER(LEN=4),  INTENT(OUT) :: HC1DRY   ! C1 formulation for dry soils
!                                          ! 'DEF ' = DEFault: Giard-Bazile formulation
!                                          ! 'GB93' = Giordani 1993, Braud 1993 
!                                          !discontinuous at WILT
CHARACTER(LEN=3),  INTENT(OUT) :: HSOILFRZ ! soil freezing-physics option
!                                          ! 'DEF' = Default (Boone et al. 2000; 
!                                          !        Giard and Bazile 2000)
!                                          ! 'LWT' = Phase changes as above,
!                                          !         but relation between unfrozen 
!                                          !         water and temperature considered
!                            NOTE that when using the YISBA='DIF' multi-layer soil option,
!                            the 'LWT' method is used. It is only an option
!                            when using the force-restore soil method ('2-L' or '3-L')
!
CHARACTER(LEN=4),  INTENT(OUT) :: HDIFSFCOND ! Mulch effects
!                                          ! 'MLCH' = include the insulating effect of
!                                          ! leaf litter/mulch on the surf. thermal cond.
!                                          ! 'DEF ' = no mulch effect
!                           NOTE: Only used when YISBA = DIF
!
CHARACTER(LEN=3), INTENT(OUT) :: HSNOWRES  ! Turbulent exchanges over snow
!                                          ! 'DEF' = Default: Louis (ISBA)
!                                          ! 'RIL' = Maximum Richardson number limit
!                                          !         for stable conditions ISBA-SNOW3L
!                                          !         turbulent exchange option
CHARACTER(LEN=3), INTENT(OUT) :: HCPSURF   ! SPECIFIC HEAT
!                                          ! 'DRY' = dry Cp
!                                          ! 'HUM' = Cp fct of qs
REAL,              INTENT(OUT) :: PCGMAX   ! maximum soil heat capacity
!
REAL,              INTENT(OUT) :: PCDRAG   ! drag coefficient in canopy
!
CHARACTER(LEN=4),  INTENT(OUT) :: HRUNOFF  ! surface runoff formulation
!                                          ! 'WSAT'
!                                          ! 'DT92'
!                                          ! 'SGH ' Topmodel
!
CHARACTER(LEN=3), INTENT(OUT) :: HKSAT     ! SOIL HYDRAULIC CONDUCTIVITY PROFILE OPTION
!                                          ! 'DEF'  = ISBA homogenous soil
!                                          ! 'SGH'  = ksat exponential decay
!
LOGICAL, INTENT(OUT) ::          OSOC      ! SOIL ORGANIC CARBON PROFILE OPTION
!                                          ! False  = ISBA homogenous soil
!                                          ! True   = SOC profile effect
!
CHARACTER(LEN=3), INTENT(OUT) :: HRAIN     ! Rainfall spatial distribution
                                           ! 'DEF' = No rainfall spatial distribution
                                           ! 'SGH' = Rainfall exponential spatial distribution
                                           ! 
! 
CHARACTER(LEN=3), INTENT(OUT) :: HHORT     ! Horton runoff
                                           ! 'DEF' = no Horton runoff
                                           ! 'SGH' = Horton runoff
!                                         
LOGICAL, INTENT(OUT)          :: OGLACIER  ! True = Over permanent snow and ice, 
!                                                   initialise WGI=WSAT, 
!                                                   Hsnow>=3.3m and allow 0.8<SNOALB<0.85
                                           ! False = No specific treatment
LOGICAL, INTENT(OUT)          :: OCANOPY_DRAG ! T: drag activated in SBL scheme within the canopy
!
LOGICAL, INTENT(OUT)          :: OVEGUPD   ! T: update vegetation parameters 
                                           !    every decade
                                           ! F: keep vegetation parameters
                                           !    constant in time
!
LOGICAL, INTENT(OUT)          :: OSPINUPCARBS ! T: carbon spinup soil
LOGICAL, INTENT(OUT)          :: OSPINUPCARBW ! T: carbon spinup wood
REAL,    INTENT(OUT)          :: PSPINMAXS    ! max number of times CARBON_SOIL subroutine is called
REAL,    INTENT(OUT)          :: PSPINMAXW    ! max number of times the wood is accelerated 
REAL,    INTENT(OUT)          :: PCO2_START   ! Pre-industrial CO2 concentration
REAL,    INTENT(OUT)          :: PCO2_END     ! Begin-transient CO2 concentration
INTEGER, INTENT(OUT)          :: KNBYEARSPINS ! nbr years needed to reaches soil equilibrium
INTEGER, INTENT(OUT)          :: KNBYEARSPINW ! nbr years needed to reaches wood equilibrium
!
LOGICAL, INTENT(OUT)          :: ONITRO_DILU ! nitrogen dilution fct of CO2 (Calvet et al. 2008)
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_ISBA',0,ZHOOK_HANDLE)
!
PTSTEP     = XUNDEF
POUT_TSTEP = XUNDEF
HSCOND  = "PL98"
!
HC1DRY     = 'DEF '
HSOILFRZ   = 'DEF'
HDIFSFCOND = 'DEF '
HSNOWRES   = 'DEF'
HCPSURF    = 'DRY'
!
HRUNOFF    = "WSAT"
HKSAT      = 'DEF'
OSOC       = .FALSE.
HRAIN      = 'DEF'
HHORT      = 'DEF'
!
PCGMAX   = 2.0E-5
!
PCDRAG   = 0.15
!
OGLACIER = .FALSE.
!
OCANOPY_DRAG = .FALSE.
!
OVEGUPD = .TRUE.
!
OSPINUPCARBS = .FALSE.
OSPINUPCARBW = .FALSE.
!
PSPINMAXS = 0.
PSPINMAXW = 0.
PCO2_START= XUNDEF
PCO2_END  = XUNDEF
KNBYEARSPINS = 0
KNBYEARSPINW = 0
!
ONITRO_DILU = .FALSE.
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_ISBA',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DEFAULT_ISBA
