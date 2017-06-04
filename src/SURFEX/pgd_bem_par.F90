!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##########################################
      SUBROUTINE PGD_BEM_PAR (DTCO, UG, U, USS, DTB, KDIM, &
                              HPROGRAM,OAUTOSIZE)
!     ##########################################
!
!!**** *PGD_BEM_PAR* monitor for averaging and interpolations of BEM input data
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    G. Pigeon        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    08/2011
!!    G. Pigeon   09/2012, NPAR_FLOOR_LAYER default to 1
!!
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
USE MODD_DATA_BEM_n, ONLY : DATA_BEM_t
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_TEST_NAM_VAR_SURF
USE MODI_INI_VAR_FROM_DATA_0D
USE MODI_INI_VAR_FROM_DATA
USE MODI_ABOR1_SFX
!
USE MODE_POS_SURF
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
TYPE(DATA_BEM_t), INTENT(INOUT) :: DTB
!
INTEGER, INTENT(IN) :: KDIM
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM     ! Type of program
LOGICAL,          INTENT(IN) :: OAUTOSIZE    ! T for automatic determination
!                                            ! of HVAC systems charcateristics
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER               :: ILUOUT    ! output listing logical unit
INTEGER               :: ILUNAM    ! namelist file  logical unit
LOGICAL               :: GFOUND    ! true if namelist is found
INTEGER               :: JLAYER    ! loop counter on layers
!
!
!*    0.3    Declaration of namelists
!            ------------------------
!
! bem options
!
INTEGER, PARAMETER                      :: NFLOOR_MAX  = 9
INTEGER                                 :: NPAR_FLOOR_LAYER ! number of floor layers
!
REAL                                    :: XUNIF_SHADE    ! 
 CHARACTER(LEN=28)                       :: CFNAM_SHADE    ! 
 CHARACTER(LEN=6)                        :: CFTYP_SHADE    ! 
REAL                                    :: XUNIF_NATVENT    ! 
 CHARACTER(LEN=28)                       :: CFNAM_NATVENT    ! 
 CHARACTER(LEN=6)                        :: CFTYP_NATVENT    ! 
!
! Floor parameters
!
REAL, DIMENSION(NFLOOR_MAX)             :: XUNIF_HC_FLOOR     ! floor layers heat capacity (J/K/m3)
REAL, DIMENSION(NFLOOR_MAX)             :: XUNIF_TC_FLOOR     ! floor layers thermal conduc (W/K/m)
REAL, DIMENSION(NFLOOR_MAX)             :: XUNIF_D_FLOOR      ! depth of floor layers      (m)
REAL                                    :: XUNIF_FLOOR_HEIGHT ! building floor height [m]
 CHARACTER(LEN=28), DIMENSION(NFLOOR_MAX):: CFNAM_HC_FLOOR     ! file name for HC_FLOOR   
 CHARACTER(LEN=28), DIMENSION(NFLOOR_MAX):: CFNAM_TC_FLOOR     ! file name for TC_FLOOR
 CHARACTER(LEN=28), DIMENSION(NFLOOR_MAX):: CFNAM_D_FLOOR      ! file name for D_FLOOR
 CHARACTER(LEN=28)                       :: CFNAM_FLOOR_HEIGHT ! file name for FLOOR_HEIGHT
 CHARACTER(LEN=6),  DIMENSION(NFLOOR_MAX):: CFTYP_HC_FLOOR     ! file type for HC_FLOOR   
 CHARACTER(LEN=6),  DIMENSION(NFLOOR_MAX):: CFTYP_TC_FLOOR     ! file type for TC_FLOOR
 CHARACTER(LEN=6),  DIMENSION(NFLOOR_MAX):: CFTYP_D_FLOOR      ! file type for D_FLOOR
 CHARACTER(LEN=6)                        :: CFTYP_FLOOR_HEIGHT ! file type for FLOOR_HEIGHT
!
! AC systems parameters
!
REAL                                    :: XUNIF_TCOOL_TARGET  !cooling setpoint
REAL                                    :: XUNIF_THEAT_TARGET  !heating setpoint
REAL                                    :: XUNIF_F_WASTE_CAN  ! fraction of waste heat into the canyon
REAL                                    :: XUNIF_EFF_HEAT     ! efficiency of the heating system
REAL                                    :: XUNIF_HR_TARGET    ! Relative humidity setpoint
REAL                                    :: XUNIF_CAP_SYS_HEAT ! Capacity of the heating system 
REAL                                    :: XUNIF_CAP_SYS_RAT  ! Rated capacity of the cooling system
REAL                                    :: XUNIF_T_ADP        ! Apparatus dewpoint temperature of the
REAL                                    :: XUNIF_M_SYS_RAT    ! Rated HVAC mass flow rate 
REAL                                    :: XUNIF_COP_RAT      ! Rated COP of the cooling system
REAL                                    :: XUNIF_F_WATER_COND ! fraction of evaporation of condensers
 CHARACTER(LEN=28)                       :: CFNAM_TCOOL_TARGET ! file name for TCOOL_TARGET
 CHARACTER(LEN=28)                       :: CFNAM_THEAT_TARGET ! file name for THEAT_TARGET
 CHARACTER(LEN=28)                       :: CFNAM_F_WASTE_CAN  ! file name for F_WASTE_CAN
 CHARACTER(LEN=28)                       :: CFNAM_EFF_HEAT     ! file name for EFF_HEAT
 CHARACTER(LEN=28)                       :: CFNAM_HR_TARGET    ! Relative humidity setpoint
 CHARACTER(LEN=28)                       :: CFNAM_CAP_SYS_HEAT ! Capacity of the heating system 
 CHARACTER(LEN=28)                       :: CFNAM_CAP_SYS_RAT  ! Rated capacity of the cooling system
 CHARACTER(LEN=28)                       :: CFNAM_T_ADP        ! Apparatus dewpoint temperature of the
 CHARACTER(LEN=28)                       :: CFNAM_M_SYS_RAT    ! Rated HVAC mass flow rate 
 CHARACTER(LEN=28)                       :: CFNAM_COP_RAT      ! Rated COP of the cooling system
 CHARACTER(LEN=28)                       :: CFNAM_F_WATER_COND ! fraction of evaporation of condensers
 CHARACTER(LEN=6)                        :: CFTYP_TCOOL_TARGET ! file type for TCOOL_TARGET
 CHARACTER(LEN=6)                        :: CFTYP_THEAT_TARGET ! file type for THEAT_TARGET
 CHARACTER(LEN=6)                        :: CFTYP_F_WASTE_CAN  ! file type for F_WASTE_CAN
 CHARACTER(LEN=6)                        :: CFTYP_EFF_HEAT     ! file type for EFF_HEAT
 CHARACTER(LEN=6)                        :: CFTYP_HR_TARGET    ! Relative humidity setpoint
 CHARACTER(LEN=6)                        :: CFTYP_CAP_SYS_HEAT ! Capacity of the heating system 
 CHARACTER(LEN=6)                        :: CFTYP_CAP_SYS_RAT  ! Rated capacity of the cooling system
 CHARACTER(LEN=6)                        :: CFTYP_T_ADP        ! Apparatus dewpoint temperature of the
 CHARACTER(LEN=6)                        :: CFTYP_M_SYS_RAT    ! Rated HVAC mass flow rate 
 CHARACTER(LEN=6)                        :: CFTYP_COP_RAT      ! Rated COP of the cooling system
 CHARACTER(LEN=6)                        :: CFTYP_F_WATER_COND ! fraction of evaporation of condensers
!
! Internal heat gains
REAL                                    :: XUNIF_QIN          ! internal heat gains [W m-2(floor)]
REAL                                    :: XUNIF_QIN_FRAD     ! radiant fraction of int heat gains
REAL                                    :: XUNIF_QIN_FLAT     ! Latent franction of internal heat gains
 CHARACTER(LEN=28)                       :: CFNAM_QIN          ! file name for QIN
 CHARACTER(LEN=28)                       :: CFNAM_QIN_FRAD     ! file name for QIN_FRAD
 CHARACTER(LEN=28)                       :: CFNAM_QIN_FLAT     ! Latent franction of internal heat gains
 CHARACTER(LEN=6)                        :: CFTYP_QIN          ! file type for QIN
 CHARACTER(LEN=6)                        :: CFTYP_QIN_FRAD     ! file type for QIN_FRAD
 CHARACTER(LEN=6)                        :: CFTYP_QIN_FLAT     ! Latent franction of internal heat gains
!
! window parameters
REAL                                    :: XUNIF_GR           ! glazing ratio
REAL                                    :: XUNIF_SHGC         ! solar transmitance of windows
REAL                                    :: XUNIF_SHGC_SH      ! solar transmitance of windows + shading
REAL                                    :: XUNIF_U_WIN        ! glazing thermal resistance[K m W-2]
 CHARACTER(LEN=28)                       :: CFNAM_GR           ! file name for GR
 CHARACTER(LEN=28)                       :: CFNAM_SHGC         ! file name for SHGC
 CHARACTER(LEN=28)                       :: CFNAM_SHGC_SH      ! file name for SHGC_SH
 CHARACTER(LEN=28)                       :: CFNAM_U_WIN        ! file name for U_WIN
 CHARACTER(LEN=6)                        :: CFTYP_GR           ! file type for GR
 CHARACTER(LEN=6)                        :: CFTYP_SHGC         ! file type for SHGC
 CHARACTER(LEN=6)                        :: CFTYP_SHGC_SH      ! file type for SHGC
 CHARACTER(LEN=6)                        :: CFTYP_U_WIN        ! file type for U_WIN
!
! air renewal
REAL                                    :: XUNIF_INF          ! infiltration/ventilation flow rate [AC/H]
REAL                                    :: XUNIF_V_VENT       ! Ventilation flow rate [AC/H]
 CHARACTER(LEN=28)                       :: CFNAM_INF          ! file name for INF
 CHARACTER(LEN=28)                       :: CFNAM_V_VENT       ! Ventilation flow rate [AC/H]
 CHARACTER(LEN=6)                        :: CFTYP_INF          ! file type for INF
 CHARACTER(LEN=6)                        :: CFTYP_V_VENT       ! Ventilation flow rate [AC/H]
!
! parameters for autosize calculation of the AC systems
REAL                                    :: XUNIF_T_SIZE_MAX   !
REAL                                    :: XUNIF_T_SIZE_MIN    !
 CHARACTER(LEN=28)                       :: CFNAM_T_SIZE_MAX    ! 
 CHARACTER(LEN=28)                       :: CFNAM_T_SIZE_MIN    ! 
 CHARACTER(LEN=6)                        :: CFTYP_T_SIZE_MAX    !
 CHARACTER(LEN=6)                        :: CFTYP_T_SIZE_MIN    !
!
REAL, DIMENSION(KDIM) :: ZWORK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
NAMELIST/NAM_DATA_BEM/ NPAR_FLOOR_LAYER,                          &
                  XUNIF_HC_FLOOR, XUNIF_TC_FLOOR, XUNIF_D_FLOOR,  &
                  XUNIF_FLOOR_HEIGHT,                             &
                  XUNIF_TCOOL_TARGET, XUNIF_THEAT_TARGET,         &
                  XUNIF_F_WASTE_CAN, XUNIF_EFF_HEAT,              &
                  XUNIF_F_WATER_COND, XUNIF_HR_TARGET,            &
                  XUNIF_QIN, XUNIF_QIN_FRAD, XUNIF_QIN_FLAT,      &   
                  XUNIF_SHGC, XUNIF_U_WIN, XUNIF_GR,XUNIF_SHGC_SH,&
                  XUNIF_INF, XUNIF_V_VENT,                        &
                  XUNIF_CAP_SYS_HEAT,                             &
                  XUNIF_CAP_SYS_RAT, XUNIF_T_ADP, XUNIF_M_SYS_RAT,&
                  XUNIF_COP_RAT, XUNIF_T_SIZE_MAX,                &
                  XUNIF_T_SIZE_MIN,                               &
                  XUNIF_SHADE, CFNAM_SHADE, CFTYP_SHADE,          &
                  XUNIF_NATVENT, CFNAM_NATVENT, CFTYP_NATVENT,    &
                  CFNAM_HC_FLOOR, CFNAM_TC_FLOOR, CFNAM_D_FLOOR,  &
                  CFNAM_FLOOR_HEIGHT,                             &
                  CFNAM_TCOOL_TARGET, CFNAM_THEAT_TARGET,         &
                  CFNAM_F_WASTE_CAN, CFNAM_EFF_HEAT,              &
                  CFNAM_F_WATER_COND, CFNAM_HR_TARGET,            &
                  CFNAM_QIN, CFNAM_QIN_FRAD, CFNAM_QIN_FLAT,      &   
                  CFNAM_SHGC, CFNAM_U_WIN, CFNAM_GR,              &
                  CFNAM_SHGC_SH, CFNAM_INF, CFNAM_V_VENT,         &
                  CFNAM_CAP_SYS_HEAT,                             &
                  CFNAM_CAP_SYS_RAT, CFNAM_T_ADP, CFNAM_M_SYS_RAT,&
                  CFNAM_COP_RAT, CFNAM_T_SIZE_MAX,                &
                  CFNAM_T_SIZE_MIN,                               &
                  CFTYP_HC_FLOOR, CFTYP_TC_FLOOR, CFTYP_D_FLOOR,  &
                  CFTYP_FLOOR_HEIGHT,                             &
                  CFTYP_TCOOL_TARGET, CFTYP_THEAT_TARGET,         &
                  CFTYP_F_WASTE_CAN, CFTYP_EFF_HEAT,              &
                  CFTYP_F_WATER_COND, CFTYP_HR_TARGET,            &
                  CFTYP_QIN, CFTYP_QIN_FRAD, CFTYP_QIN_FLAT,      &   
                  CFTYP_SHGC, CFTYP_U_WIN, CFTYP_GR,              &
                  CFTYP_SHGC_SH, CFTYP_INF, CFTYP_V_VENT,         &
                  CFTYP_CAP_SYS_HEAT,                             &
                  CFTYP_CAP_SYS_RAT, CFTYP_T_ADP, CFTYP_M_SYS_RAT,&
                  CFTYP_COP_RAT, CFTYP_T_SIZE_MAX,                &
                  CFTYP_T_SIZE_MIN
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('PGD_BEM_PAR',0,ZHOOK_HANDLE)
!
!*    1.      Initializations
!             ---------------
!
NPAR_FLOOR_LAYER = 1
XUNIF_SHADE   = XUNDEF
XUNIF_NATVENT = XUNDEF
XUNIF_HC_FLOOR     = XUNDEF
XUNIF_TC_FLOOR     = XUNDEF
XUNIF_D_FLOOR      = XUNDEF
XUNIF_TCOOL_TARGET = XUNDEF
XUNIF_THEAT_TARGET = XUNDEF
XUNIF_F_WASTE_CAN  = XUNDEF
XUNIF_EFF_HEAT     = XUNDEF
XUNIF_QIN          = XUNDEF
XUNIF_QIN_FRAD     = XUNDEF
XUNIF_SHGC         = XUNDEF
XUNIF_U_WIN        = XUNDEF
XUNIF_GR           = XUNDEF
XUNIF_SHGC_SH      = XUNDEF
XUNIF_FLOOR_HEIGHT = XUNDEF
XUNIF_INF          = XUNDEF
XUNIF_F_WATER_COND = XUNDEF
XUNIF_QIN_FLAT     = XUNDEF
XUNIF_HR_TARGET    = XUNDEF
XUNIF_V_VENT       = XUNDEF
XUNIF_CAP_SYS_HEAT = XUNDEF
XUNIF_CAP_SYS_RAT  = XUNDEF
XUNIF_T_ADP        = XUNDEF
XUNIF_M_SYS_RAT    = XUNDEF
XUNIF_COP_RAT      = XUNDEF
XUNIF_T_SIZE_MAX   = XUNDEF
XUNIF_T_SIZE_MIN   = XUNDEF
!
CFNAM_SHADE        = '                            '
CFNAM_NATVENT      = '                            '
CFNAM_HC_FLOOR (:) = '                            '
CFNAM_TC_FLOOR (:) = '                            '
CFNAM_D_FLOOR  (:) = '                            '
CFNAM_TCOOL_TARGET = '                            '
CFNAM_THEAT_TARGET = '                            '
CFNAM_F_WASTE_CAN  = '                            '
CFNAM_EFF_HEAT     = '                            '
CFNAM_QIN          = '                            '
CFNAM_QIN_FRAD     = '                            '
CFNAM_SHGC         = '                            '
CFNAM_U_WIN        = '                            '
CFNAM_GR           = '                            '
CFNAM_SHGC_SH      = '                            '
CFNAM_FLOOR_HEIGHT = '                            '
CFNAM_INF          = '                            '
CFNAM_F_WATER_COND = '                            '
CFNAM_QIN_FLAT     = '                            '
CFNAM_HR_TARGET    = '                            '
CFNAM_V_VENT       = '                            '
CFNAM_CAP_SYS_HEAT = '                            '
CFNAM_CAP_SYS_RAT  = '                            '
CFNAM_T_ADP        = '                            '
CFNAM_M_SYS_RAT    = '                            '
CFNAM_COP_RAT      = '                            '
CFNAM_T_SIZE_MAX   = '                            '
CFNAM_T_SIZE_MIN   = '                            '
!
CFTYP_SHADE        = '      '
CFTYP_NATVENT      = '      '
CFTYP_HC_FLOOR(:)  = '      '
CFTYP_TC_FLOOR(:)  = '      '
CFTYP_D_FLOOR(:)   = '      '
CFTYP_TCOOL_TARGET = '      '
CFTYP_THEAT_TARGET = '      '
CFTYP_F_WASTE_CAN  = '      '
CFTYP_EFF_HEAT     = '      '
CFTYP_QIN          = '      '
CFTYP_QIN_FRAD     = '      '
CFTYP_SHGC         = '      '
CFTYP_U_WIN        = '      '
CFTYP_GR           = '      '
CFTYP_SHGC_SH      = '      '
CFTYP_FLOOR_HEIGHT = '      '
CFTYP_INF          = '      '
CFTYP_F_WATER_COND = '      '
CFTYP_QIN_FLAT     = '      '
CFTYP_HR_TARGET    = '      '
CFTYP_V_VENT       = '      '
CFTYP_CAP_SYS_HEAT = '      '
CFTYP_CAP_SYS_RAT  = '      '
CFTYP_T_ADP        = '      '
CFTYP_M_SYS_RAT    = '      '
CFTYP_COP_RAT      = '      '
CFTYP_T_SIZE_MAX   = '      '
CFTYP_T_SIZE_MIN   = '      '
!
!-------------------------------------------------------------------------------
!
!*    2.      Input file for cover types
!             --------------------------
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
 CALL POSNAM(ILUNAM,'NAM_DATA_BEM',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_DATA_BEM)
!
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
!-------------------------------------------------------------------------------
!
DTB%NPAR_FLOOR_LAYER = NPAR_FLOOR_LAYER
!
!-------------------------------------------------------------------------------
!
!* coherence check
!
IF ((     ANY(XUNIF_HC_FLOOR/=XUNDEF) .OR. ANY(LEN_TRIM(CFNAM_HC_FLOOR)>0) &
     .OR. ANY(XUNIF_TC_FLOOR/=XUNDEF) .OR. ANY(LEN_TRIM(CFNAM_TC_FLOOR)>0) &
     .OR. ANY(XUNIF_D_FLOOR /=XUNDEF) .OR. ANY(LEN_TRIM(CFNAM_D_FLOOR )>0) &
    ) .AND. NPAR_FLOOR_LAYER<1                                  ) THEN
  CALL ABOR1_SFX('In order to initialize FLOOR thermal quantities, please specify NPAR_FLOOR_LAYER in namelist NAM_DATA_BEM')
END IF
!
!-------------------------------------------------------------------------------
IF (NFLOOR_MAX < NPAR_FLOOR_LAYER) THEN
  WRITE(ILUOUT,*) '---------------------------------------------'
  WRITE(ILUOUT,*) 'Please update pgd_bem_par.F90 routine :      '
  WRITE(ILUOUT,*) 'The maximum number of FLOOR LAYER             '
  WRITE(ILUOUT,*) 'in the declaration of the namelist variables '
  WRITE(ILUOUT,*) 'must be increased to : ', NPAR_FLOOR_LAYER
  WRITE(ILUOUT,*) '---------------------------------------------'
  CALL ABOR1_SFX('PGD_BEM_PAR: MAXIMUM NUMBER OF NPAR_FLOOR_LAYER MUST BE INCREASED')
END IF
!-------------------------------------------------------------------------------
!
!*    3.      user defined fields are prescribed
!             ----------------------------------
!
!
ALLOCATE(DTB%XPAR_SHADE        (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'MAJ','SHADE      ','TWN', CFNAM_SHADE, CFTYP_SHADE, XUNIF_SHADE, &
        DTB%XPAR_SHADE, DTB%LDATA_SHADE )
IF (.NOT.DTB%LDATA_SHADE) DEALLOCATE(DTB%XPAR_SHADE)
!
ALLOCATE(DTB%XPAR_NATVENT      (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'MAJ','NATVENT   ','TWN', CFNAM_NATVENT, CFTYP_NATVENT, XUNIF_NATVENT, &
        DTB%XPAR_NATVENT, DTB%LDATA_NATVENT )
IF (.NOT.DTB%LDATA_NATVENT) DEALLOCATE(DTB%XPAR_NATVENT)
!
ALLOCATE(DTB%XPAR_HC_FLOOR    (KDIM,NPAR_FLOOR_LAYER))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS,  &
                        HPROGRAM,'INV','HC_FLOOR  ','TWN',CFNAM_HC_FLOOR,CFTYP_HC_FLOOR, &
        XUNIF_HC_FLOOR,DTB%XPAR_HC_FLOOR,DTB%LDATA_HC_FLOOR )
IF (.NOT.DTB%LDATA_HC_FLOOR) DEALLOCATE(DTB%XPAR_HC_FLOOR)
!
ALLOCATE(DTB%XPAR_TC_FLOOR    (KDIM,NPAR_FLOOR_LAYER))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS,  &
                        HPROGRAM,'ARI','TC_FLOOR  ','TWN',CFNAM_TC_FLOOR,CFTYP_TC_FLOOR, &
                 XUNIF_TC_FLOOR ,DTB%XPAR_TC_FLOOR, DTB%LDATA_TC_FLOOR )
IF (.NOT.DTB%LDATA_TC_FLOOR) DEALLOCATE(DTB%XPAR_TC_FLOOR)
!
ALLOCATE(DTB%XPAR_D_FLOOR     (KDIM,NPAR_FLOOR_LAYER))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS,  &
                        HPROGRAM,'ARI','D_FLOOR   ','TWN',CFNAM_D_FLOOR,CFTYP_D_FLOOR, &
                 XUNIF_D_FLOOR  ,DTB%XPAR_D_FLOOR , DTB%LDATA_D_FLOOR )
IF (.NOT.DTB%LDATA_D_FLOOR) DEALLOCATE(DTB%XPAR_D_FLOOR)
!
!-------------------------------------------------------------------------------
!
ALLOCATE(DTB%XPAR_TCOOL_TARGET  (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','TCOOL_TARGET','TWN',CFNAM_TCOOL_TARGET, CFTYP_TCOOL_TARGET, XUNIF_TCOOL_TARGET, &
        DTB%XPAR_TCOOL_TARGET, DTB%LDATA_TCOOL_TARGET)
IF (.NOT.DTB%LDATA_TCOOL_TARGET) DEALLOCATE(DTB%XPAR_TCOOL_TARGET)
!
ALLOCATE(DTB%XPAR_THEAT_TARGET  (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','THEAT_TARGET','TWN',CFNAM_THEAT_TARGET, CFTYP_THEAT_TARGET, XUNIF_THEAT_TARGET, &
        DTB%XPAR_THEAT_TARGET, DTB%LDATA_THEAT_TARGET)
IF (.NOT.DTB%LDATA_THEAT_TARGET) DEALLOCATE(DTB%XPAR_THEAT_TARGET)
!
ALLOCATE(DTB%XPAR_F_WASTE_CAN   (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','F_WASTE_CAN','TWN',CFNAM_F_WASTE_CAN, CFTYP_F_WASTE_CAN, XUNIF_F_WASTE_CAN, &
        DTB%XPAR_F_WASTE_CAN, DTB%LDATA_F_WASTE_CAN)
IF (.NOT.DTB%LDATA_F_WASTE_CAN) DEALLOCATE(DTB%XPAR_F_WASTE_CAN)
!
ALLOCATE(DTB%XPAR_EFF_HEAT      (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','EFF_HEAT','TWN',CFNAM_EFF_HEAT, CFTYP_EFF_HEAT, XUNIF_EFF_HEAT, &
        DTB%XPAR_EFF_HEAT, DTB%LDATA_EFF_HEAT)
IF (.NOT.DTB%LDATA_EFF_HEAT) DEALLOCATE(DTB%XPAR_EFF_HEAT)
!
ALLOCATE(DTB%XPAR_QIN           (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','QIN','TWN',CFNAM_QIN, CFTYP_QIN, XUNIF_QIN, DTB%XPAR_QIN, DTB%LDATA_QIN)
IF (.NOT.DTB%LDATA_QIN) DEALLOCATE(DTB%XPAR_QIN)
!
ALLOCATE(DTB%XPAR_QIN_FRAD      (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','QIN_FRAD','TWN',CFNAM_QIN_FRAD, CFTYP_QIN_FRAD, XUNIF_QIN_FRAD, &
        DTB%XPAR_QIN_FRAD, DTB%LDATA_QIN_FRAD)
IF (.NOT.DTB%LDATA_QIN_FRAD) DEALLOCATE(DTB%XPAR_QIN_FRAD)
!
ALLOCATE(DTB%XPAR_SHGC          (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','SHGC','TWN',CFNAM_SHGC, CFTYP_SHGC, XUNIF_SHGC, DTB%XPAR_SHGC, DTB%LDATA_SHGC)
IF (.NOT.DTB%LDATA_SHGC) DEALLOCATE(DTB%XPAR_SHGC)
!
ALLOCATE(DTB%XPAR_U_WIN         (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','U_WIN','TWN',CFNAM_U_WIN, CFTYP_U_WIN, XUNIF_U_WIN, DTB%XPAR_U_WIN, DTB%LDATA_U_WIN)
IF (.NOT.DTB%LDATA_U_WIN) DEALLOCATE(DTB%XPAR_U_WIN)
!
ALLOCATE(DTB%XPAR_GR            (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','GR','TWN',CFNAM_GR, CFTYP_GR, XUNIF_GR, DTB%XPAR_GR, DTB%LDATA_GR)
IF (.NOT.DTB%LDATA_GR) DEALLOCATE(DTB%XPAR_GR)
!
ALLOCATE(DTB%XPAR_SHGC_SH       (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','SHGC_SH','TWN',CFNAM_SHGC_SH, CFTYP_SHGC_SH, XUNIF_SHGC_SH, &
        DTB%XPAR_SHGC_SH, DTB%LDATA_SHGC_SH)
IF (.NOT.DTB%LDATA_SHGC_SH) DEALLOCATE(DTB%XPAR_SHGC_SH)
!
ALLOCATE(DTB%XPAR_FLOOR_HEIGHT  (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','FLOOR_HEIGHT','TWN',CFNAM_FLOOR_HEIGHT, CFTYP_FLOOR_HEIGHT, XUNIF_FLOOR_HEIGHT, &
        DTB%XPAR_FLOOR_HEIGHT, DTB%LDATA_FLOOR_HEIGHT)
IF (.NOT.DTB%LDATA_FLOOR_HEIGHT) DEALLOCATE(DTB%XPAR_FLOOR_HEIGHT)
!
ALLOCATE(DTB%XPAR_INF           (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','INF','TWN',CFNAM_INF, CFTYP_INF, XUNIF_INF, DTB%XPAR_INF, DTB%LDATA_INF)
IF (.NOT.DTB%LDATA_INF) DEALLOCATE(DTB%XPAR_INF)
!
ALLOCATE(DTB%XPAR_F_WATER_COND (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','F_WATER_COND','TWN',CFNAM_F_WATER_COND, CFTYP_F_WATER_COND, XUNIF_F_WATER_COND, &
        DTB%XPAR_F_WATER_COND, DTB%LDATA_F_WATER_COND)
IF (.NOT.DTB%LDATA_F_WATER_COND) DEALLOCATE(DTB%XPAR_F_WATER_COND)
!
ALLOCATE(DTB%XPAR_QIN_FLAT     (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','QIN_FLAT','TWN',CFNAM_QIN_FLAT, CFTYP_QIN_FLAT, XUNIF_QIN_FLAT, &
        DTB%XPAR_QIN_FLAT, DTB%LDATA_QIN_FLAT)
IF (.NOT.DTB%LDATA_QIN_FLAT) DEALLOCATE(DTB%XPAR_QIN_FLAT)
!
ALLOCATE(DTB%XPAR_HR_TARGET    (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','HR_TARGET','TWN',CFNAM_HR_TARGET, CFTYP_HR_TARGET, XUNIF_HR_TARGET, &
        DTB%XPAR_HR_TARGET, DTB%LDATA_HR_TARGET)
IF (.NOT.DTB%LDATA_HR_TARGET) DEALLOCATE(DTB%XPAR_HR_TARGET)
!
ALLOCATE(DTB%XPAR_V_VENT       (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','V_VENT','TWN',CFNAM_V_VENT, CFTYP_V_VENT, &
                           XUNIF_V_VENT, DTB%XPAR_V_VENT, DTB%LDATA_V_VENT)
IF (.NOT.DTB%LDATA_V_VENT) DEALLOCATE(DTB%XPAR_V_VENT)
!
ALLOCATE(DTB%XPAR_T_SIZE_MAX   (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','T_SIZE_MAX','TWN',CFNAM_T_SIZE_MAX, CFTYP_T_SIZE_MAX, XUNIF_T_SIZE_MAX, &
        DTB%XPAR_T_SIZE_MAX, DTB%LDATA_T_SIZE_MAX)
IF (.NOT.DTB%LDATA_T_SIZE_MAX) DEALLOCATE(DTB%XPAR_T_SIZE_MAX)
!
ALLOCATE(DTB%XPAR_T_SIZE_MIN   (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','T_SIZE_MIN','TWN',CFNAM_T_SIZE_MIN, CFTYP_T_SIZE_MIN, XUNIF_T_SIZE_MIN, &
        DTB%XPAR_T_SIZE_MIN, DTB%LDATA_T_SIZE_MIN)
IF (.NOT.DTB%LDATA_T_SIZE_MIN) DEALLOCATE(DTB%XPAR_T_SIZE_MIN)
!
ALLOCATE(DTB%XPAR_CAP_SYS_HEAT (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','CAP_SYS_HEAT','TWN',CFNAM_CAP_SYS_HEAT, CFTYP_CAP_SYS_HEAT, XUNIF_CAP_SYS_HEAT, &
        DTB%XPAR_CAP_SYS_HEAT, DTB%LDATA_CAP_SYS_HEAT)
IF (.NOT.DTB%LDATA_CAP_SYS_HEAT) DEALLOCATE(DTB%XPAR_CAP_SYS_HEAT)
!
ALLOCATE(DTB%XPAR_CAP_SYS_RAT  (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','CAP_SYS_RAT','TWN',CFNAM_CAP_SYS_RAT, CFTYP_CAP_SYS_RAT, XUNIF_CAP_SYS_RAT, &
        DTB%XPAR_CAP_SYS_RAT, DTB%LDATA_CAP_SYS_RAT)
IF (.NOT.DTB%LDATA_CAP_SYS_RAT) DEALLOCATE(DTB%XPAR_CAP_SYS_RAT)
!
ALLOCATE(DTB%XPAR_M_SYS_RAT    (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','M_SYS_RAT','TWN',CFNAM_M_SYS_RAT, CFTYP_M_SYS_RAT, XUNIF_M_SYS_RAT, &
        DTB%XPAR_M_SYS_RAT, DTB%LDATA_M_SYS_RAT)
IF (.NOT.DTB%LDATA_M_SYS_RAT) DEALLOCATE(DTB%XPAR_M_SYS_RAT)
  !
IF (OAUTOSIZE) THEN
  IF (DTB%LDATA_CAP_SYS_HEAT .OR. DTB%LDATA_CAP_SYS_RAT .OR. DTB%LDATA_M_SYS_RAT) THEN
    WRITE(ILUOUT,*) '==> You choose LAUTOSIZE=T <=='
    WRITE(ILUOUT,*) 'Therefore HVAC systems characteristics will be computed automatically'
    IF (DTB%LDATA_CAP_SYS_HEAT) THEN
      WRITE(ILUOUT,*) 'Data you provided for CAP_SYS_HEAT are then discarded.'
      DEALLOCATE(DTB%XPAR_CAP_SYS_HEAT)
    END IF
    IF (DTB%LDATA_CAP_SYS_RAT ) THEN
      WRITE(ILUOUT,*) 'Data you provided for CAP_SYS_RAT  are then discarded.'
      DEALLOCATE(DTB%XPAR_CAP_SYS_RAT)
    END IF
    IF (DTB%LDATA_M_SYS_RAT   ) THEN
      WRITE(ILUOUT,*) 'Data you provided for M_SYS_RAT    are then discarded.'
      DEALLOCATE(DTB%XPAR_M_SYS_RAT)
    END IF
  END IF
  DTB%LDATA_CAP_SYS_HEAT = .FALSE.
  DTB%LDATA_CAP_SYS_RAT  = .FALSE.
  DTB%LDATA_M_SYS_RAT    = .FALSE.
ELSE
  IF (DTB%LDATA_T_SIZE_MAX .OR. DTB%LDATA_T_SIZE_MAX) THEN
    WRITE(ILUOUT,*) '==> You choose LAUTOSIZE=F <=='
    WRITE(ILUOUT,*) 'Therefore HVAC systems characteristics are specified'
    WRITE(ILUOUT,*) 'and you do not need the minimal and maximum temperatures'
    WRITE(ILUOUT,*) 'that would be used if you have chosen an automatic calibration.'
    IF (DTB%LDATA_T_SIZE_MAX) THEN
      WRITE(ILUOUT,*) 'Data you provided for T_SIZE_MAX are then discarded.'
      DEALLOCATE(DTB%XPAR_T_SIZE_MAX)
    END IF
    IF (DTB%LDATA_T_SIZE_MIN) THEN
      WRITE(ILUOUT,*) 'Data you provided for T_SIZE_MIN are then discarded.'
      DEALLOCATE(DTB%XPAR_T_SIZE_MIN)
    END IF
  END IF
  DTB%LDATA_T_SIZE_MAX = .FALSE.
  DTB%LDATA_T_SIZE_MIN = .FALSE.
END IF
!
ALLOCATE(DTB%XPAR_T_ADP        (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','T_ADP','TWN',CFNAM_T_ADP, CFTYP_T_ADP, XUNIF_T_ADP, DTB%XPAR_T_ADP, DTB%LDATA_T_ADP)
IF (.NOT.DTB%LDATA_T_ADP) DEALLOCATE(DTB%XPAR_T_ADP)
!
ALLOCATE(DTB%XPAR_COP_RAT      (KDIM))
CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                           HPROGRAM,'ARI','COP_RAT','TWN',CFNAM_COP_RAT, CFTYP_COP_RAT, XUNIF_COP_RAT, &
        DTB%XPAR_COP_RAT, DTB%LDATA_COP_RAT)
IF (.NOT.DTB%LDATA_COP_RAT) DEALLOCATE(DTB%XPAR_COP_RAT)

!-------------------------------------------------------------------------------
!
!* coherence checks
!
 CALL COHERENCE_THERMAL_DATA_FL('FLOOR',DTB%LDATA_HC_FLOOR,DTB%LDATA_TC_FLOOR,DTB%LDATA_D_FLOOR)
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('PGD_BEM_PAR',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
CONTAINS
SUBROUTINE COHERENCE_THERMAL_DATA_FL(HTYPE,ODATA_HC,ODATA_TC,ODATA_D)
 CHARACTER(LEN=5), INTENT(IN) :: HTYPE
LOGICAL,          INTENT(IN) :: ODATA_HC
LOGICAL,          INTENT(IN) :: ODATA_TC
LOGICAL,          INTENT(IN) :: ODATA_D
!
IF (ODATA_HC .OR. ODATA_TC .OR. ODATA_D) THEN
  IF (.NOT. (ODATA_HC .AND. ODATA_TC .AND. ODATA_D)) THEN
    WRITE(ILUOUT,*) '*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*'
    WRITE(ILUOUT,*) 'When specifying data for thermal ',TRIM(HTYPE),' characteristics,'
    WRITE(ILUOUT,*) 'All three parameters MUST be defined:'
    WRITE(ILUOUT,*) 'Heat capacity, Thermal conductivity and depths of layers'
    WRITE(ILUOUT,*) ' '
    WRITE(ILUOUT,*) 'In your case :'
    IF (ODATA_HC) THEN
      WRITE(ILUOUT,*) 'Heat capacity is defined'
    ELSE
      WRITE(ILUOUT,*) 'Heat capacity is NOT defined'
    END IF
    IF (ODATA_TC) THEN
      WRITE(ILUOUT,*) 'Thermal conductivity is defined'
    ELSE
      WRITE(ILUOUT,*) 'Thermal conductivity is NOT defined'
    END IF
    IF (ODATA_D) THEN
      WRITE(ILUOUT,*) 'Depths of layers are defined'
    ELSE
      WRITE(ILUOUT,*) 'Depths of layers are NOT defined'
    END IF
    WRITE(ILUOUT,*) '*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*'
    CALL ABOR1_SFX('Heat capacity, Thermal conductivity and depths of layers MUST all be defined for '//HTYPE)
  END IF
END IF
END SUBROUTINE COHERENCE_THERMAL_DATA_FL
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_BEM_PAR
