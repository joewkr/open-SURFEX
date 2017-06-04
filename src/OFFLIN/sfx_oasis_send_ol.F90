!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#########
SUBROUTINE SFX_OASIS_SEND_OL (F, IM, S, U, W, &
                              HPROGRAM,KI,PTIMEC,PSTEP_SURF)
!###########################################
!
!!****  *SFX_OASIS_SEND_OL* - Offline driver to send coupling fields
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
!!      B. Decharme 10/2016  bug surface/groundwater coupling
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
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
USE MODN_SFX_OASIS,  ONLY : XTSTEP_CPL_LAND, &
                            XTSTEP_CPL_LAKE, &
                            XTSTEP_CPL_SEA , &
                            LWATER
!
USE MODD_SFX_OASIS,  ONLY : LCPL_LAND,LCPL_GW,       &
                            LCPL_FLOOD,LCPL_CALVING, &
                            LCPL_LAKE,               &
                            LCPL_SEA,LCPL_SEAICE
!
USE MODI_GET_SFX_LAND
USE MODI_GET_SFX_LAKE
USE MODI_GET_SFX_SEA
!
USE MODI_GET_LUOUT
USE MODI_SFX_OASIS_SEND
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
CHARACTER(LEN=*),      INTENT(IN) :: HPROGRAM
INTEGER,               INTENT(IN) :: KI            ! number of points
REAL,                  INTENT(IN) :: PTIMEC        ! Cumulated run time step (s)
REAL,                  INTENT(IN) :: PSTEP_SURF    ! Model time step (s)
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(KI)   :: ZLAND_RUNOFF    ! Cumulated Surface runoff             (kg/m2)
REAL, DIMENSION(KI)   :: ZLAND_DRAIN     ! Cumulated Deep drainage              (kg/m2)
REAL, DIMENSION(KI)   :: ZLAND_CALVING   ! Cumulated Calving flux               (kg/m2)
REAL, DIMENSION(KI)   :: ZLAND_WATFLD    ! Cumulated net freshwater rate        (kg/m2)
!
REAL, DIMENSION(KI)   :: ZLAKE_EVAP  ! Cumulated Evaporation             (kg/m2)
REAL, DIMENSION(KI)   :: ZLAKE_RAIN  ! Cumulated Rainfall rate           (kg/m2)
REAL, DIMENSION(KI)   :: ZLAKE_SNOW  ! Cumulated Snowfall rate           (kg/m2)
REAL, DIMENSION(KI)   :: ZLAKE_WATF  ! Cumulated net freshwater rate     (kg/m2)
!
REAL, DIMENSION(KI)   :: ZSEA_FWSU  ! Cumulated zonal wind stress       (Pa.s)
REAL, DIMENSION(KI)   :: ZSEA_FWSV  ! Cumulated meridian wind stress    (Pa.s)
REAL, DIMENSION(KI)   :: ZSEA_HEAT  ! Cumulated Non solar net heat flux (J/m2)
REAL, DIMENSION(KI)   :: ZSEA_SNET  ! Cumulated Solar net heat flux     (J/m2)
REAL, DIMENSION(KI)   :: ZSEA_WIND  ! Cumulated 10m wind speed          (m)
REAL, DIMENSION(KI)   :: ZSEA_FWSM  ! Cumulated wind stress             (Pa.s)
REAL, DIMENSION(KI)   :: ZSEA_EVAP  ! Cumulated Evaporation             (kg/m2)
REAL, DIMENSION(KI)   :: ZSEA_RAIN  ! Cumulated Rainfall rate           (kg/m2)
REAL, DIMENSION(KI)   :: ZSEA_SNOW  ! Cumulated Snowfall rate           (kg/m2)
REAL, DIMENSION(KI)   :: ZSEA_WATF  ! Cumulated net freshwater rate     (kg/m2)
!
REAL, DIMENSION(KI)   :: ZSEAICE_HEAT ! Cumulated Sea-ice non solar net heat flux (J/m2)
REAL, DIMENSION(KI)   :: ZSEAICE_SNET ! Cumulated Sea-ice solar net heat flux     (J/m2)
REAL, DIMENSION(KI)   :: ZSEAICE_EVAP ! Cumulated Sea-ice sublimation             (kg/m2)
!
INTEGER               :: IDATE  ! current coupling time step (s)
INTEGER               :: ILUOUT
INTEGER               :: INKPROMA
!
LOGICAL               :: GSEND_LAND
LOGICAL               :: GSEND_LAKE
LOGICAL               :: GSEND_SEA
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('SFX_OASIS_SEND_OL',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!*       1.     Initialize proc by proc :
!               -------------------------
!
CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
IDATE = INT(PTIMEC-PSTEP_SURF)
!
GSEND_LAND=(LCPL_LAND.AND.MOD(PTIMEC,XTSTEP_CPL_LAND)==0.0)
GSEND_LAKE=(LCPL_LAKE.AND.MOD(PTIMEC,XTSTEP_CPL_LAKE)==0.0)
GSEND_SEA =(LCPL_SEA .AND.MOD(PTIMEC,XTSTEP_CPL_SEA )==0.0)
!
!-------------------------------------------------------------------------------
!
IF(.NOT.(GSEND_LAND.OR.GSEND_LAKE.OR.GSEND_SEA))THEN
  IF (LHOOK) CALL DR_HOOK('SFX_OASIS_SEND_OL',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
!-------------------------------------------------------------------------------
!
IF(GSEND_LAND)THEN
  ZLAND_RUNOFF  (:) = XUNDEF
  ZLAND_DRAIN   (:) = XUNDEF
  ZLAND_CALVING (:) = XUNDEF
  ZLAND_WATFLD  (:) = XUNDEF  
ENDIF
!
IF(GSEND_LAKE)THEN
  ZLAKE_EVAP (:) = XUNDEF
  ZLAKE_RAIN (:) = XUNDEF
  ZLAKE_SNOW (:) = XUNDEF
  ZSEA_WATF  (:) = XUNDEF  
ENDIF
!
IF(GSEND_SEA)THEN
  ZSEA_FWSU (:) = XUNDEF
  ZSEA_FWSV (:) = XUNDEF
  ZSEA_HEAT (:) = XUNDEF
  ZSEA_SNET (:) = XUNDEF
  ZSEA_WIND (:) = XUNDEF
  ZSEA_FWSM (:) = XUNDEF
  ZSEA_EVAP (:) = XUNDEF
  ZSEA_RAIN (:) = XUNDEF
  ZSEA_SNOW (:) = XUNDEF
  ZSEA_WATF (:) = XUNDEF
  !
  ZSEAICE_HEAT (:) = XUNDEF
  ZSEAICE_SNET (:) = XUNDEF
  ZSEAICE_EVAP (:) = XUNDEF
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       2.     get local fields :
!               ------------------
!
IF(GSEND_LAND)THEN
!
! * Get river output fields
!
  CALL GET_SFX_LAND(IM%O, IM%S, U, &
                    LCPL_GW,LCPL_FLOOD,LCPL_CALVING,  &
                    ZLAND_RUNOFF (:),ZLAND_DRAIN   (:),&
                    ZLAND_CALVING(:),ZLAND_WATFLD (:))
!
ENDIF
!
IF(GSEND_LAKE)THEN
!
! * Get output fields
!
  CALL GET_SFX_LAKE(F, U, &
                    ZLAKE_EVAP(:),ZLAKE_RAIN(:), &
                    ZLAKE_SNOW(:),ZLAKE_WATF(:) )
!
ENDIF
!
IF(GSEND_SEA)THEN
!
! * Get sea output fields
!
  CALL GET_SFX_SEA(S, U, W, &
                   LCPL_SEAICE,LWATER,                                                                                    &
                   ZSEA_FWSU   (:),ZSEA_FWSV   (:),ZSEA_HEAT   (:),&
                   ZSEA_SNET   (:),ZSEA_WIND   (:),ZSEA_FWSM   (:),&
                   ZSEA_EVAP   (:),ZSEA_RAIN   (:),ZSEA_SNOW   (:),&
                   ZSEA_WATF   (:),                                &
                   ZSEAICE_HEAT(:),ZSEAICE_SNET(:),ZSEAICE_EVAP(:) )
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       3.     Send fields to OASIS proc by proc:
!               ----------------------------------
!
  CALL SFX_OASIS_SEND(ILUOUT,KI,IDATE,GSEND_LAND,GSEND_LAKE,GSEND_SEA,      &
                      ZLAND_RUNOFF,ZLAND_DRAIN,ZLAND_CALVING,ZLAND_WATFLD,  &
                      ZLAKE_EVAP,ZLAKE_RAIN,ZLAKE_SNOW,ZLAKE_WATF,          &
                      ZSEA_FWSU,ZSEA_FWSV,ZSEA_HEAT,ZSEA_SNET,ZSEA_WIND,    &
                      ZSEA_FWSM,ZSEA_EVAP,ZSEA_RAIN,ZSEA_SNOW,ZSEA_WATF,    &
                      ZSEAICE_HEAT,ZSEAICE_SNET,ZSEAICE_EVAP                )                   
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('SFX_OASIS_SEND_OL',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SFX_OASIS_SEND_OL
