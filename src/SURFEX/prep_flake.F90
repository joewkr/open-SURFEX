!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_FLAKE (DTCO, USS, FG, F, SB, UG, U, GCP, &
                       HPROGRAM,HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,YDCTL)
!     #################################################################################
!
!!****  *PREP_FLAKE* - prepares FLAKE fields
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
!!      S. Riette   06/2009 PREP_FLAKE_SBL has no more argument
!!      E. Kourzeneva 09/2010 (i)  Change the default initialisation,
!!                            (ii) Include the possibility to use 
!!                                 lake climate data
!!      P. Marguinaud10/2014, Support for a 2-part PREP
!!------------------------------------------------------------------
!
!
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODD_FLAKE_n, ONLY : FLAKE_t
USE MODD_CANOPY_n, ONLY : CANOPY_t
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
!
USE MODI_PREP_HOR_FLAKE_FIELD
USE MODI_PREP_VER_FLAKE
USE MODI_PREP_SBL
USE MODI_PREP_OUTPUT_GRID
USE MODI_GET_LUOUT
USE MODI_CLI_LAKE
!
USE MODN_PREP_FLAKE
!
USE MODD_READ_NAMELIST,ONLY : LNAM_READ
USE MODD_SURF_ATM,     ONLY : LVERTSHIFT
USE MODD_PREP,         ONLY : XZS_LS
USE MODD_PREP_FLAKE,   ONLY : LCLIM_LAKE
USE MODD_SURF_PAR,     ONLY : XUNDEF
!
USE MODD_CSTS,       ONLY : XTT
USE MODE_PREP_CTL, ONLY : PREP_CTL, PREP_CTL_CAN
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_CLEAN_PREP_OUTPUT_GRID
!
USE MODI_ABOR1_SFX
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SSO_t), INTENT(INOUT) :: USS
!
TYPE(GRID_t), INTENT(INOUT) :: FG
TYPE(FLAKE_t), INTENT(INOUT) :: F
TYPE(CANOPY_t), INTENT(INOUT) :: SB
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
TYPE (PREP_CTL),    INTENT(INOUT) :: YDCTL
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=28),  INTENT(IN)  :: HATMFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6),   INTENT(IN)  :: HATMFILETYPE! type of the Atmospheric file
 CHARACTER(LEN=28),  INTENT(IN)  :: HPGDFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6),   INTENT(IN)  :: HPGDFILETYPE! type of the Atmospheric file
!
!*      0.2    declarations of local variables
!
INTEGER :: ISIZE
INTEGER :: ILUOUT
LOGICAL :: GNOVALUE       ! if the variable is not defined
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
!*      1.     Default of configuration
!
!
IF (LHOOK) CALL DR_HOOK('PREP_FLAKE',0,ZHOOK_HANDLE)
!
IF (.NOT. PREP_CTL_CAN (YDCTL)) THEN
  CALL ABOR1_SFX('PREP_FLAKE: TWO STEP PREP NOT IMPLEMENTED')
ENDIF

 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
 CALL PREP_OUTPUT_GRID(UG%G, FG, U%NSIZE_FULL, ILUOUT)
!
ISIZE = SIZE(FG%XLAT)
!
!-------------------------------------------------------------------------------------
!
!*      2.     Reading and horizontal interpolations
!
!
!*      2.0    Large scale orography
!
 CALL PREP_HOR_FLAKE_FIELD(DTCO, UG, U, USS, GCP, ISIZE, F, &
                           HPROGRAM,'ZS     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!
!*      2.1    FLake variables
!
GNOVALUE = .FALSE.
!
IF (.NOT.LCLIM_LAKE) THEN
  !
  CALL PREP_HOR_FLAKE_FIELD(DTCO, UG, U, USS, GCP, ISIZE, F, &
                           HPROGRAM,'TS     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,GNOVALUE)
  IF (GNOVALUE) CALL ABOR1_SFX('PREP_FLAKE: AT LEAST TS SHOULD BE GIVEN!')
  !
  CALL PREP_HOR_FLAKE_FIELD(DTCO, UG, U, USS, GCP, ISIZE, F, &
                           HPROGRAM,'T_SNOW ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,GNOVALUE)
  !
  IF (.NOT.GNOVALUE) CALL PREP_HOR_FLAKE_FIELD(DTCO, UG, U, USS, GCP, ISIZE, F, &
                           HPROGRAM,'T_ICE  ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,GNOVALUE)
  !
  IF (.NOT.GNOVALUE) CALL PREP_HOR_FLAKE_FIELD(DTCO, UG, U, USS, GCP, ISIZE, F, &
                           HPROGRAM,'T_WML  ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,GNOVALUE)
  !
  ALLOCATE(F%XT_MNW(ISIZE))
  !
  IF (.NOT.GNOVALUE) CALL PREP_HOR_FLAKE_FIELD(DTCO, UG, U, USS, GCP, ISIZE, F, &
                           HPROGRAM,'T_BOT  ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,GNOVALUE)
  !
  IF (.NOT.GNOVALUE) CALL PREP_HOR_FLAKE_FIELD(DTCO, UG, U, USS, GCP, ISIZE, F, &
                           HPROGRAM,'T_B1   ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,GNOVALUE)
  !
  IF (.NOT.GNOVALUE) CALL PREP_HOR_FLAKE_FIELD(DTCO, UG, U, USS, GCP, ISIZE, F, &
                           HPROGRAM,'CT     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,GNOVALUE)
  !
  IF (.NOT.GNOVALUE) CALL PREP_HOR_FLAKE_FIELD(DTCO, UG, U, USS, GCP, ISIZE, F, &
                           HPROGRAM,'H_SNOW ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,GNOVALUE)
  !
  IF (.NOT.GNOVALUE) CALL PREP_HOR_FLAKE_FIELD(DTCO, UG, U, USS, GCP, ISIZE, F, &
                           HPROGRAM,'H_ICE  ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,GNOVALUE)
  !
  IF (.NOT.GNOVALUE) CALL PREP_HOR_FLAKE_FIELD(DTCO, UG, U, USS, GCP, ISIZE, F, &
                           HPROGRAM,'H_ML   ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,GNOVALUE)
  !
  IF (.NOT.GNOVALUE) CALL PREP_HOR_FLAKE_FIELD(DTCO, UG, U, USS, GCP, ISIZE, F, &
                           HPROGRAM,'H_B1   ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,GNOVALUE)
  !
ENDIF
!
IF (LCLIM_LAKE .OR. GNOVALUE) THEN
  IF (LCLIM_LAKE) THEN
    ALLOCATE(F%XTS(ISIZE))
    F%XTS(:)=XUNDEF
  ENDIF
  ALLOCATE(F%XT_SNOW(ISIZE)) 
  ALLOCATE(F%XT_ICE(ISIZE))  
  ALLOCATE(F%XT_WML(ISIZE))
  ALLOCATE(F%XT_MNW(ISIZE)) 
  ALLOCATE(F%XT_BOT(ISIZE))  
  ALLOCATE(F%XT_B1(ISIZE))
  ALLOCATE(F%XCT(ISIZE))  
  ALLOCATE(F%XH_SNOW(ISIZE))  
  ALLOCATE(F%XH_ICE(ISIZE))
  ALLOCATE(F%XH_ML(ISIZE))
  ALLOCATE(F%XH_B1(ISIZE))  
  F%XT_SNOW(:)=XUNDEF
  F%XT_ICE(:)=XUNDEF
  F%XT_WML(:)=XUNDEF
  F%XT_MNW(:)=XUNDEF
  F%XT_BOT(:)=XUNDEF
  F%XT_B1(:)=XUNDEF
  F%XCT(:)=XUNDEF
  F%XH_SNOW(:)=XUNDEF
  F%XH_ICE(:)=XUNDEF
  F%XH_ML(:)=XUNDEF
  F%XH_B1(:)=XUNDEF
ENDIF
!
!-------------------------------------------------------------------------------------
!
 CALL CLEAN_PREP_OUTPUT_GRID
!
!*      2.2    Roughness
!
ALLOCATE(F%XZ0(SIZE(F%XTS)))
F%XZ0 = 0.001
!
!*      2.2    Friction velocity
!
ALLOCATE(F%XUSTAR(SIZE(F%XTS)))
F%XUSTAR = 0.
!
!-------------------------------------------------------------------------------------

!
!*      3.     Vertical interpolations of all variables
!
IF(.NOT.LCLIM_LAKE) THEN
  IF (LVERTSHIFT)THEN    
    CALL PREP_VER_FLAKE(F)
    WRITE(ILUOUT,*) "WARNING: You want the vertical shift for lakes?"
    WRITE(ILUOUT,*) "WARNING: Vertical shift for the lake temperature profile is impossible!"
    WRITE(ILUOUT,*) "WARNING: So, set the default vertical profiles from the shifted surface temperature."    !
    GNOVALUE=.TRUE.
  ENDIF
END IF
!
DEALLOCATE(XZS_LS)
!-------------------------------------------------------------------------------------
!
!*      4.    Compute T_MNW and give the default profile if needed 
!              or read data from climate files 
!
IF (LCLIM_LAKE) THEN
 CALL CLI_LAKE(FG, F)
ELSEIF (.NOT.GNOVALUE) THEN
  F%XT_MNW(:)=F%XT_WML(:)-(F%XT_WML(:)-F%XT_BOT(:))*(1.-F%XH_ML(:)/F%XWATER_DEPTH(:))*F%XCT(:)
ELSE
  WRITE(ILUOUT,*) "WARNING! One of the lake profile variales was not indicated, so set the default profile!"
  F%XT_WML=MAX(F%XTS(:),XTT)  
  F%XT_SNOW=MIN(F%XTS(:),XTT)
  F%XT_ICE=MIN(F%XTS(:),XTT)
  F%XH_B1=0.0 
  F%XCT=0.5   
  F%XH_SNOW=0.0   
  WHERE (F%XTS <= XTT)
   F%XT_BOT=XTT+4.
   F%XT_B1=XTT+3.9
   F%XH_ICE=0.01
   F%XH_ML=F%XWATER_DEPTH/2.
   F%XT_MNW=F%XT_WML-(F%XT_WML-F%XT_BOT)*(1.-F%XH_ML/F%XWATER_DEPTH)*F%XCT
  ELSEWHERE
   F%XT_BOT=F%XTS
   F%XT_B1=F%XTS-0.1
   F%XH_ICE=0.0
   F%XH_ML=F%XWATER_DEPTH
   F%XT_MNW=F%XTS 
  END WHERE
END IF
!
!-------------------------------------------------------------------------------------
!
!*      6.     Preparation of SBL air variables
!
F%LSBL = LWAT_SBL
IF (F%LSBL) CALL PREP_SBL(ISIZE, SB)
!
IF (LHOOK) CALL DR_HOOK('PREP_FLAKE',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_FLAKE
