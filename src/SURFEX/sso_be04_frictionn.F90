!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###############################################################################
SUBROUTINE SSO_BE04_FRICTION_n (SB, USS, PTSTEP,PSEA,PUREF,PRHOA,PU,PV,PSFU,PSFV)
!     ###############################################################################
!
!!****  *SSO_BE04_FRICTION_n * - Computes subgrid-scale orography friction
!                                  according to several options:
!                                CROUGH='Z01D' : orographic roughness length
!                                CROUGH='Z04D' : orographic roughness length
!                                                variable with wind direction
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
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    05/2010
!----------------------------------------------------------------
!
!
!
USE MODD_CANOPY_n, ONLY : CANOPY_t
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_CANOPY_TURB,    ONLY : XALPSBL
USE MODD_CSTS,           ONLY : XKARMAN
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_CANOPY_EVOL
USE MODI_CANOPY_GRID_UPDATE
USE MODI_SSO_BELJAARS04
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(CANOPY_t), INTENT(INOUT) :: SB
TYPE(SSO_t), INTENT(INOUT) :: USS
!
REAL,               INTENT(IN)    :: PTSTEP    ! time step
REAL, DIMENSION(:), INTENT(IN)    :: PSEA      ! Sea fraction                          (-)
REAL, DIMENSION(:), INTENT(IN)    :: PUREF     ! Wind forcing height                   (m)
REAL, DIMENSION(:), INTENT(IN)    :: PRHOA     ! air density                           (kg/m3)
REAL, DIMENSION(:), INTENT(IN)    :: PU        ! zonal wind                            (m/s)
REAL, DIMENSION(:), INTENT(IN)    :: PV        ! meridian wind                         (m/s)
REAL, DIMENSION(:), INTENT(INOUT) :: PSFU      ! zonal momentum flux                   (Pa)
REAL, DIMENSION(:), INTENT(INOUT) :: PSFV      ! meridian momentum flux                (Pa)
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PU))    :: ZWIND   ! wind strength (m/s)
REAL, DIMENSION(SIZE(PU))    :: ZSSO_STDEV! SSO standard deviation (m)
REAL, DIMENSION(SIZE(PU))    :: ZUSTAR  ! friction velocity
!
!* canopy variables
!
REAL, DIMENSION(SIZE(PU))     :: ZTA      ! temperature                                   (K)
REAL, DIMENSION(SIZE(PU))     :: ZQA      ! specific humidity                             (kg/m3)
REAL, DIMENSION(SIZE(PU))     :: ZPA      ! pressure                                      (Pa)
REAL, DIMENSION(SIZE(PU),SB%NLVL) :: ZLM
REAL, DIMENSION(SIZE(PU),SB%NLVL) :: ZLEPS
REAL, DIMENSION(SIZE(PU),SB%NLVL) :: ZP
REAL, DIMENSION(SIZE(PU))     :: ZSFLUX_T
REAL, DIMENSION(SIZE(PU))     :: ZSFLUX_Q
REAL, DIMENSION(SIZE(PU),SB%NLVL) :: ZFORC_T
REAL, DIMENSION(SIZE(PU),SB%NLVL) :: ZDFORC_TDT
REAL, DIMENSION(SIZE(PU),SB%NLVL) :: ZFORC_Q
REAL, DIMENSION(SIZE(PU),SB%NLVL) :: ZDFORC_QDQ
REAL, DIMENSION(SIZE(PU)) :: ZALFATH
REAL, DIMENSION(SIZE(PU)) :: ZBETATH
REAL, DIMENSION(SIZE(PU)) :: ZALFAQ
REAL, DIMENSION(SIZE(PU)) :: ZBETAQ
!
REAL,    DIMENSION(SIZE(PU), SB%NLVL) :: ZFORC_U      ! tendency due to drag force for wind
REAL,    DIMENSION(SIZE(PU), SB%NLVL) :: ZDFORC_UDU   ! formal derivative of
!                                                  ! tendency due to drag force for wind
REAL,    DIMENSION(SIZE(PU), SB%NLVL) :: ZFORC_E      ! tendency due to drag force for TKE
REAL,    DIMENSION(SIZE(PU), SB%NLVL) :: ZDFORC_EDE   ! formal derivative of
!                                                  ! tendency due to drag force for TKE
INTEGER                            :: INI          ! number of points
INTEGER                            :: JI           ! number of points loop counter
INTEGER                            :: JLAYER       ! vertical loop counter
REAL,    DIMENSION(SIZE(PU))       :: ZH        ! Canopy height     (m)
REAL,    DIMENSION(SIZE(PU))       :: ZSFLUX_U  ! Surface flux u'w' (m2/s2)
REAL,    DIMENSION(SIZE(PU))       :: ZALFAU   ! V+(1) = alfa u'w'(1) + beta ! not used
REAL,    DIMENSION(SIZE(PU))       :: ZBETAU   ! V+(1) = alfa u'w'(1) + beta ! not used
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
!
!*      1.     Initializations
!              ---------------
!
!*      1.1    Grid definition
!              ---------------
IF (LHOOK) CALL DR_HOOK('SSO_BE04_FRICTION_N',0,ZHOOK_HANDLE)
INI = SIZE(PU)
!
ZH = 0.
 CALL CANOPY_GRID_UPDATE(INI,ZH,PUREF,SB)
!
!*      1.2    Wind
!              ----
!
ZWIND    = SQRT(PU**2+PV**2)
!
ZSFLUX_U = - SQRT(PSFU**2+PSFV**2)
!
!
!*      1.3    Canopy profiles at first time step (neutral case)
!              ----------------------------------
!
IF (ANY(SB%XU(:,SB%NLVL)==XUNDEF)) THEN
  DO JLAYER=1,SB%NLVL
     DO JI=1,INI
        SB%XU  (JI,JLAYER) = MAX ( ZWIND(JI) + SQRT(-ZSFLUX_U(JI)) / XKARMAN      &
                                  * LOG(SB%XZ(JI,JLAYER)/SB%XZ(JI,SB%NLVL))   , 0.)
        SB%XTKE(JI,JLAYER) = - XALPSBL * ZSFLUX_U(JI)
     ENDDO
  ENDDO
ENDIF
!
!
!-------------------------------------------------------------------------------------
!
!*      2.    Subgrid-scale orographic drag (Beljaars et al 2004)
!             -----------------------------
!
ZSSO_STDEV = USS%XSSO_STDEV
WHERE (ZSSO_STDEV==XUNDEF) ZSSO_STDEV=0.
!
ZFORC_U   (:,:)= 0.
ZDFORC_UDU(:,:)= 0.
ZFORC_E   (:,:) = 0.
ZDFORC_EDE(:,:) = 0.
!
!* computes tendencies on wind and Tke due to subgridscale orography
 CALL SSO_BELJAARS04(USS, SB, INI, ZSSO_STDEV, ZFORC_U, ZDFORC_UDU )
!
DO JLAYER=1,SB%NLVL
   DO JI=1,INI
      ZFORC_U   (JI,SB%NLVL) = ZFORC_U   (JI,SB%NLVL) * (1.0-PSEA(JI))
      ZDFORC_UDU(JI,SB%NLVL) = ZDFORC_UDU(JI,SB%NLVL) * (1.0-PSEA(JI))
   ENDDO
ENDDO
!
!-------------------------------------------------------------------------------------
!
!*      3.    Computes coefficients for implicitation
!             ---------------------------------------
!
ZTA     (:) = XUNDEF
ZQA     (:) = XUNDEF
ZPA     (:) = XUNDEF
ZSFLUX_T(:) = XUNDEF
ZSFLUX_Q(:) = XUNDEF
ZP        (:,:) = XUNDEF
ZFORC_T   (:,:) = XUNDEF
ZDFORC_TDT(:,:) = XUNDEF 
ZFORC_Q   (:,:) = XUNDEF
ZDFORC_QDQ(:,:) = XUNDEF
!
 CALL CANOPY_EVOL(SB, INI, PTSTEP, 2, SB%XZ, ZWIND, ZTA, ZQA, ZPA, PRHOA, &
                  ZSFLUX_U, ZSFLUX_T, ZSFLUX_Q, ZFORC_U, ZDFORC_UDU, &
                  ZFORC_E, ZDFORC_EDE, ZFORC_T, ZDFORC_TDT, &
                  ZFORC_Q, ZDFORC_QDQ, ZLM, ZLEPS, ZUSTAR,  &
                  ZALFAU, ZBETAU, ZALFATH, ZBETATH, ZALFAQ, ZBETAQ, &
                  ONEUTRAL=.TRUE.                 )
!
!-------------------------------------------------------------------------------------
!
!
! Momentum fluxes if canopy is used
!
WHERE (ZWIND>0.)
  PSFU (:) = - PU(:)/ZWIND(:) * ZUSTAR(:)**2 * PRHOA(:)
  PSFV (:) = - PV(:)/ZWIND(:) * ZUSTAR(:)**2 * PRHOA(:)
END WHERE
!
IF (LHOOK) CALL DR_HOOK('SSO_BE04_FRICTION_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE SSO_BE04_FRICTION_n
