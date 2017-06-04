!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#########
SUBROUTINE SFX_OASIS_RECV_OL (F, IM, S, U, W, &
                              HPROGRAM,KI,KSW,PTIMEC,PTSTEP_SURF,   &
                              PZENITH,PSW_BANDS,          &
                              PTSRAD,PDIR_ALB,PSCA_ALB,PEMIS,PTSURF )
!#############################################
!
!!****  *SFX_OASIS_RECV_OL* - Offline driver that receive coupling fields from oasis
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
!!      B. Decharme   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2013
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODN_SFX_OASIS,  ONLY : XTSTEP_CPL_LAND, &
                            XTSTEP_CPL_SEA,  &
                            LWATER
!
USE MODD_SFX_OASIS,  ONLY : LCPL_LAND,         &
                            LCPL_GW,LCPL_FLOOD,&
                            LCPL_SEA,          &
                            LCPL_SEAICE
!
USE MODD_OFF_SURFEX_n, ONLY : GOTO_MODEL
!
USE MODD_FLAKE_n, ONLY : FLAKE_t
USE MODD_SURFEX_n, ONLY : ISBA_MODEL_t
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_WATFLUX_n, ONLY : WATFLUX_t
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE MODI_GET_LUOUT
USE MODI_SFX_OASIS_RECV
USE MODI_PUT_SFX_LAND
USE MODI_PUT_SFX_SEA
USE MODI_UPDATE_ESM_SURF_ATM_n
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
#ifdef AIX64
!$ USE OMP_LIB
#endif
!
IMPLICIT NONE
!
#ifndef AIX64
!$ INCLUDE 'omp_lib.h'
#endif
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
CHARACTER(LEN=6),       INTENT(IN)  :: HPROGRAM    ! program calling surf. schemes
!
INTEGER,                INTENT(IN)  :: KI          ! number of points on this proc
INTEGER,                INTENT(IN)  :: KSW         ! number of short-wave spectral bands
REAL,                   INTENT(IN)  :: PTIMEC      ! Cumulated run time step (s)
REAL,                   INTENT(IN)  :: PTSTEP_SURF ! Surfex time step
!
REAL, DIMENSION(KI),    INTENT(IN)  :: PZENITH   ! zenithal angle       (radian from the vertical)
REAL, DIMENSION(KSW),   INTENT(IN)  :: PSW_BANDS ! mean wavelength of each shortwave band (m)
!
REAL, DIMENSION(KI),    INTENT(OUT) :: PTSRAD    ! radiative temperature                 (K)
REAL, DIMENSION(KI,KSW),INTENT(OUT) :: PDIR_ALB  ! direct albedo for each spectral band  (-)
REAL, DIMENSION(KI,KSW),INTENT(OUT) :: PSCA_ALB  ! diffuse albedo for each spectral band (-)
REAL, DIMENSION(KI),    INTENT(OUT) :: PEMIS     ! emissivity                            (-)
REAL, DIMENSION(KI),    INTENT(OUT) :: PTSURF    ! surface effective temperature         (K)
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(KI) :: ZLAND_WTD     ! Land water table depth (m)
REAL, DIMENSION(KI) :: ZLAND_FWTD    ! Land grid-cell fraction of water table rise (-)
REAL, DIMENSION(KI) :: ZLAND_FFLOOD  ! Land Floodplains fraction (-)
REAL, DIMENSION(KI) :: ZLAND_PIFLOOD ! Land Potential flood infiltration (kg/m2/s)
REAL, DIMENSION(KI) :: ZSEA_SST      ! Sea surface temperature (K)
REAL, DIMENSION(KI) :: ZSEA_UCU      ! Sea u-current stress (Pa)
REAL, DIMENSION(KI) :: ZSEA_VCU      ! Sea v-current stress (Pa)
REAL, DIMENSION(KI) :: ZSEAICE_SIT   ! Sea-ice Temperature (K)
REAL, DIMENSION(KI) :: ZSEAICE_CVR   ! Sea-ice cover (-)
REAL, DIMENSION(KI) :: ZSEAICE_ALB   ! Sea-ice albedo (-)
!
REAL                :: ZTIME_CPL
!
LOGICAL             :: GRECV_LAND
LOGICAL             :: GRECV_FLOOD
LOGICAL             :: GRECV_SEA
!
INTEGER             :: ILUOUT
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('SFX_OASIS_RECV_OL',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!*       1.     init coupling fields:
!               ----------------------------------
!
ZTIME_CPL = PTIMEC-PTSTEP_SURF
!
GRECV_LAND=(LCPL_LAND.AND.MOD(ZTIME_CPL,XTSTEP_CPL_LAND)==0.0)
GRECV_SEA =(LCPL_SEA .AND.MOD(ZTIME_CPL,XTSTEP_CPL_SEA )==0.0)
!
IF(.NOT.(GRECV_LAND.OR.GRECV_SEA))THEN
  IF (LHOOK) CALL DR_HOOK('SFX_OASIS_RECV_OL',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
IF(GRECV_LAND)THEN
  ZLAND_WTD    (:) = XUNDEF
  ZLAND_FWTD   (:) = XUNDEF
  ZLAND_FFLOOD (:) = XUNDEF
  ZLAND_PIFLOOD(:) = XUNDEF
ENDIF
!
IF(GRECV_SEA)THEN
  ZSEA_SST   (:) = XUNDEF
  ZSEA_UCU   (:) = XUNDEF
  ZSEA_VCU   (:) = XUNDEF
  ZSEAICE_SIT(:) = XUNDEF
  ZSEAICE_CVR(:) = XUNDEF
  ZSEAICE_ALB(:) = XUNDEF
ENDIF
!
!*       2.     Receive fields to other models proc by proc:
!               --------------------------------------------
!
CALL SFX_OASIS_RECV(HPROGRAM,KI,KSW,ZTIME_CPL,         &
                    GRECV_LAND, GRECV_SEA,             &
                    ZLAND_WTD    (:),ZLAND_FWTD   (:), &
                    ZLAND_FFLOOD (:),ZLAND_PIFLOOD(:), &
                    ZSEA_SST     (:),ZSEA_UCU     (:), &
                    ZSEA_VCU     (:),ZSEAICE_SIT  (:), &
                    ZSEAICE_CVR  (:),ZSEAICE_ALB  (:)  )
!
!-------------------------------------------------------------------------------
! Put variable over land tile
!-------------------------------------------------------------------------------
!
IF(GRECV_LAND)THEN
  CALL PUT_SFX_LAND(IM%O, IM%S, IM%K, IM%NK, IM%NP, U, ILUOUT,LCPL_GW,LCPL_FLOOD, ZLAND_WTD(:),&
                    ZLAND_FWTD(:), ZLAND_FFLOOD(:),ZLAND_PIFLOOD(:)  )        
ENDIF
!
!-------------------------------------------------------------------------------
! Put variable over sea and/or water tile
!-------------------------------------------------------------------------------
!
IF(GRECV_SEA)THEN
  CALL PUT_SFX_SEA(S, U, W, ILUOUT,LCPL_SEAICE,LWATER, ZSEA_SST(:),ZSEA_UCU(:), &
                   ZSEA_VCU(:),ZSEAICE_SIT(:), ZSEAICE_CVR(:),ZSEAICE_ALB(:)  )
ENDIF
!
!-------------------------------------------------------------------------------
! Update radiative properties at time t+1 for radiative scheme
!-------------------------------------------------------------------------------
!
GRECV_FLOOD=(GRECV_LAND.AND.LCPL_FLOOD)
!
IF(GRECV_SEA.OR.GRECV_FLOOD)THEN     
  CALL UPDATE_ESM_SURF_ATM_n(F, IM, S, U, W, HPROGRAM, KI, KSW, PZENITH, PSW_BANDS, &
                             PTSRAD, PDIR_ALB, PSCA_ALB, PEMIS, PTSURF )                    
ENDIF
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('SFX_OASIS_RECV_OL',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SFX_OASIS_RECV_OL
