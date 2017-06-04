!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE INIT_WATER_SBL(SB, PPA, PPS, PTA, PQA, PRHOA, PU, PV, PRAIN, PSNOW,     &
                              PSFTH, PSFTQ, PZREF, PUREF, PTS, PZ0 )
!     #################################################################################
!
!!****  *INIT_WATER_SBL* - inits water SBL profiles
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
!!     S. Riette
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    03/2010
!!------------------------------------------------------------------
!
USE MODD_CANOPY_n, ONLY : CANOPY_t
!
USE MODD_CSTS,             ONLY : XCPD, XRD, XP00, XTT, XG
USE MODD_CANOPY_TURB,      ONLY : XALPSBL
!
USE MODI_CLS_WIND
USE MODI_CLS_TQ
USE MODI_WATER_FLUX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(CANOPY_t), INTENT(INOUT) :: SB
!
REAL, DIMENSION(:), INTENT(IN)  :: PPA       ! pressure at forcing level             (Pa)
REAL, DIMENSION(:), INTENT(IN)  :: PPS       ! pressure at atmospheric model surface (Pa)
REAL, DIMENSION(:), INTENT(IN)  :: PTA       ! air temperature forcing               (K)
REAL, DIMENSION(:), INTENT(IN)  :: PQA       ! air humidity forcing                  (kg/m3)
REAL, DIMENSION(:), INTENT(IN)  :: PRHOA     ! air density                           (kg/m3)
REAL, DIMENSION(:), INTENT(IN)  :: PU        ! zonal wind                            (m/s)
REAL, DIMENSION(:), INTENT(IN)  :: PV        ! meridian wind                         (m/s)
REAL, DIMENSION(:), INTENT(IN)  :: PSNOW     ! snow precipitation                    (kg/m2/s)
REAL, DIMENSION(:), INTENT(IN)  :: PRAIN     ! liquid precipitation                  (kg/m2/s)
REAL, DIMENSION(:), INTENT(IN)  :: PZREF     ! height of T,q forcing                 (m)
REAL, DIMENSION(:), INTENT(IN)  :: PUREF     ! height of wind forcing                (m)
REAL, DIMENSION(:), INTENT(IN)  :: PTS       ! surface temperature
!
REAL, DIMENSION(:), INTENT(INOUT) :: PZ0       ! roughness length
REAL, DIMENSION(:), INTENT(OUT) :: PSFTH     ! flux of heat                          (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PSFTQ     ! flux of water vapor                   (kg/m2/s)
!
!*      0.2    declarations of local variables
!
!* forcing variables
!
REAL, DIMENSION(SIZE(PTA))   :: ZWIND    ! lowest atmospheric level wind speed           (m/s)
REAL, DIMENSION(SIZE(PTA))   :: ZEXNA    ! Exner function at lowest SBL scheme level     (-)
REAL, DIMENSION(SIZE(PTA))   :: ZQA      ! specific humidity                             (kg/m3)
!
! SBL turbulence scheme
!
REAL, DIMENSION(SIZE(PTA))   :: ZUSTAR       ! friction velocity (m/s)
!
REAL, DIMENSION(SIZE(PTA))   :: ZEXNS
REAL, DIMENSION(SIZE(PTA))   :: ZQSAT
REAL, DIMENSION(SIZE(PTA))   :: ZCD
REAL, DIMENSION(SIZE(PTA))   :: ZCDN
REAL, DIMENSION(SIZE(PTA))   :: ZCH
REAL, DIMENSION(SIZE(PTA))   :: ZRI
REAL, DIMENSION(SIZE(PTA))   :: ZRESA_SEA
REAL, DIMENSION(SIZE(PTA))   :: ZZ0H
REAL, DIMENSION(SIZE(PTA))   :: ZCLS_WIND_ZON
REAL, DIMENSION(SIZE(PTA))   :: ZCLS_WIND_MER
REAL, DIMENSION(SIZE(PTA))   :: ZTNM
REAL, DIMENSION(SIZE(PTA))   :: ZQNM
REAL, DIMENSION(SIZE(PTA))   :: ZHUNM
REAL, DIMENSION(SIZE(PTA))   :: ZHU
INTEGER                      :: J
INTEGER                      :: JLAYER
LOGICAL                      :: GHANDLE_SIC = .FALSE. ! no sea-ice model
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
!*     1.2     Initialisation at first time step
!              ---------------------------------
!
IF (LHOOK) CALL DR_HOOK('INIT_WATER_SBL',0,ZHOOK_HANDLE)
!
ZEXNA(:) = (PPA(:)/XP00)**(XRD/XCPD)
ZEXNS(:) = (PPS(:)/XP00)**(XRD/XCPD)
ZQA  (:) = PQA(:) / PRHOA(:)
ZWIND(:) = SQRT(PU**2+PV**2)
!
!We choose the case CSEA_FLUX=DIRECT to compute CD, CDN, CH, RI and ZZ0H
!Iterative computation of PZ0 / CD, CDN, CH, ZH0
DO J=1,5
  CALL WATER_FLUX(PZ0, PTA, ZEXNA, PRHOA, PTS, ZEXNS, ZQA, PRAIN, &
                  PSNOW, XTT, ZWIND, PZREF, PUREF,                &
                  PPS, GHANDLE_SIC, ZQSAT,  PSFTH, PSFTQ, ZUSTAR, &
                  ZCD, ZCDN, ZCH, ZRI, ZRESA_SEA, ZZ0H            )
ENDDO
!
!Initialisation of T, Q, Wind and TKE on all canopy levels
ZHU(:)=1.
DO JLAYER=1,SB%NLVL
  !
  CALL CLS_TQ(PTA, ZQA, PPA, PPS, PZREF, ZCD, ZCH, ZRI, PTS, ZHU, ZZ0H, &
              SB%XZ(:,JLAYER), ZTNM, ZQNM, ZHUNM                        )
  !
  SB%XT(:,JLAYER)=ZTNM
  SB%XQ(:,JLAYER)=ZQNM
  !
  CALL CLS_WIND(PU, PV, PUREF, ZCD, ZCDN, ZRI, SB%XZ(:,JLAYER), &
                ZCLS_WIND_ZON, ZCLS_WIND_MER                 )
  !
  SB%XU   (:,JLAYER) = SQRT( ZCLS_WIND_ZON(:)**2 + ZCLS_WIND_MER(:)**2 )
  SB%XTKE (:,JLAYER) = XALPSBL * ZCD(:) * ( PU(:)**2 + PV(:)**2 )
  SB%XP   (:,JLAYER) = PPA(:) + XG * PRHOA(:) * ( SB%XZ(:,SB%NLVL) - SB%XZ(:,JLAYER) )
  !
ENDDO
!
IF (LHOOK) CALL DR_HOOK('INIT_WATER_SBL',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_WATER_SBL
