!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PUT_SFXCPL_n (F, IM, S, U, W, &
                         HPROGRAM,KI,KSW,PSW_BANDS,PZENITH, &
                        PLAND_WTD,PLAND_FWTD,PLAND_FFLOOD, &
                        PLAND_PIFLOOD,PSEA_SST,PSEA_UCU,   &
                        PSEA_VCU,PSEAICE_SIT,PSEAICE_CVR,  &
                        PSEAICE_ALB,PTSRAD,                &
                        PDIR_ALB,PSCA_ALB,PEMIS,PTSURF     )  
!     #################################################################################################
!
!!****  *PUT_SFXCPL_n* - routine to modify some variables in surfex from information coming
!                        from an ocean and/or a river routing model (but already on Surfex grid)
!
!
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
!!      B. Decharme      *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    08/2009
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_FLAKE_n, ONLY : FLAKE_t
USE MODD_SURFEX_n, ONLY : ISBA_MODEL_t
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_WATFLUX_n, ONLY : WATFLUX_t
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE MODN_SFX_OASIS,  ONLY : LWATER
USE MODD_SFX_OASIS,  ONLY : LCPL_SEA, LCPL_SEAICE, &
                            LCPL_LAND, LCPL_GW,    &
                            LCPL_FLOOD
!                          
USE MODI_GET_LUOUT
!
USE MODI_ABOR1_SFX
USE MODI_PUT_SFX_LAND
USE MODI_PUT_SFX_SEA
USE MODI_UPDATE_ESM_SURF_ATM_n
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
TYPE(FLAKE_t), INTENT(INOUT) :: F
TYPE(ISBA_MODEL_t), INTENT(INOUT) :: IM
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(WATFLUX_t), INTENT(INOUT) :: W
!
CHARACTER(LEN=6),        INTENT(IN)  :: HPROGRAM
INTEGER,                 INTENT(IN)  :: KI      ! number of points
INTEGER,                 INTENT(IN)  :: KSW     ! number of bands
!
REAL, DIMENSION(KI),      INTENT(IN) :: PZENITH
REAL, DIMENSION(KSW),     INTENT(IN) :: PSW_BANDS ! mean wavelength of each shortwave band (m)
!
REAL, DIMENSION(KI),      INTENT(IN) :: PLAND_WTD     ! Land water table depth (m)
REAL, DIMENSION(KI),      INTENT(IN) :: PLAND_FWTD    ! Land grid-cell fraction of water table rise (-)
REAL, DIMENSION(KI),      INTENT(IN) :: PLAND_FFLOOD  ! Land Floodplains fraction (-)
REAL, DIMENSION(KI),      INTENT(IN) :: PLAND_PIFLOOD ! Land Potential flood infiltration (kg/m2)
!
REAL, DIMENSION(KI),      INTENT(IN) :: PSEA_SST ! Sea surface temperature (K)
REAL, DIMENSION(KI),      INTENT(IN) :: PSEA_UCU ! Sea u-current stress (Pa)
REAL, DIMENSION(KI),      INTENT(IN) :: PSEA_VCU ! Sea v-current stress (Pa)
!
REAL, DIMENSION(KI),      INTENT(IN) :: PSEAICE_SIT ! Sea-ice Temperature (K)
REAL, DIMENSION(KI),      INTENT(IN) :: PSEAICE_CVR ! Sea-ice cover (-)
REAL, DIMENSION(KI),      INTENT(IN) :: PSEAICE_ALB ! Sea-ice albedo (-)
!
REAL, DIMENSION(KI),     INTENT(OUT) :: PTSRAD   ! Total radiative temperature see by the atmosphere
REAL, DIMENSION(KI),     INTENT(OUT) :: PTSURF   ! Total surface temperature see by the atmosphere
REAL, DIMENSION(KI),     INTENT(OUT) :: PEMIS    ! Total emissivity see by the atmosphere
REAL, DIMENSION(KI,KSW), INTENT(OUT) :: PDIR_ALB ! Total direct albedo see by the atmosphere
REAL, DIMENSION(KI,KSW), INTENT(OUT) :: PSCA_ALB ! Total diffus albedo see by the atmosphere
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!
INTEGER :: ILU, ILUOUT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('PUT_SFXCL_N',0,ZHOOK_HANDLE)
!
CALL GET_LUOUT(HPROGRAM,ILUOUT)
!-------------------------------------------------------------------------------
!
! Global argument
!
IF(KI/=U%NSIZE_FULL)THEN
  WRITE(ILUOUT,*) 'size of field from the coupler :', KI
  WRITE(ILUOUT,*) 'size of field in SURFEX        :', U%NSIZE_FULL
  CALL ABOR1_SFX('PUT_SFXCPL_N: VECTOR SIZE NOT CORRECT FOR COUPLING')
ENDIF
!
!-------------------------------------------------------------------------------
! Put variable over land tile
!-------------------------------------------------------------------------------
!
IF(LCPL_LAND)THEN
  CALL PUT_SFX_LAND(IM%O, IM%S, IM%K, IM%NK, IM%NP, U, ILUOUT, LCPL_GW, LCPL_FLOOD, &
                    PLAND_WTD(:), PLAND_FWTD(:),PLAND_FFLOOD(:),PLAND_PIFLOOD(:))        
ENDIF
!
!-------------------------------------------------------------------------------
! Put variable over sea and/or water tile
!-------------------------------------------------------------------------------
!
IF(LCPL_SEA)THEN
!
  CALL PUT_SFX_SEA(S, U, W, ILUOUT,LCPL_SEAICE,LWATER,PSEA_SST(:),PSEA_UCU(:), &
                   PSEA_VCU(:),PSEAICE_SIT(:),PSEAICE_CVR(:),PSEAICE_ALB(:) )
!
ENDIF
!
!-------------------------------------------------------------------------------
! Update radiative properties at time t+1 for radiative scheme
!-------------------------------------------------------------------------------
!
IF(LCPL_SEA.OR.LCPL_FLOOD)THEN
  CALL UPDATE_ESM_SURF_ATM_n(F, IM, S, U, W, HPROGRAM, KI, KSW, PZENITH, PSW_BANDS,  &
                             PTSRAD, PDIR_ALB, PSCA_ALB, PEMIS, PTSURF )
ENDIF
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PUT_SFXCL_N',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE PUT_SFXCPL_n
