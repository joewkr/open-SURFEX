!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE CANOPY_EVOL_WIND(SB, KI, PTSTEP, KIMPL, PWIND, PK, PDKDDVDZ,   &
                                  PSFLUX_U, PFORC_U, PDFORC_UDU, PUW, PALFA, PBETA ) 
!     #########################################
!
!!****  *CANOPY_EVOL_WIND* - evolution of wind in canopy 
!!                        
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
!!      Original    07/2006 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CANOPY_n, ONLY : CANOPY_t
!
USE MODI_CANOPY_EVOL_FIELD
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(CANOPY_t), INTENT(INOUT) :: SB
!
INTEGER,                  INTENT(IN)    :: KI        ! number of horizontal points
REAL,                     INTENT(IN)    :: PTSTEP    ! time-step                             (s)
INTEGER,                  INTENT(IN)    :: KIMPL     ! implicitation code: 
!                                                    ! 1 : computes only alfa and beta coupling
!                                                    !     coefficients for all variables
!                                                    ! 2 : computes temporal evolution of the
!                                                    !     variables
REAL, DIMENSION(KI),      INTENT(IN)    :: PWIND     ! wind at forcing level                 (m/s)
REAL, DIMENSION(KI,SB%NLVL), INTENT(IN)    :: PK        ! mixing exchange coefficient           (m2/s)
REAL, DIMENSION(KI,SB%NLVL), INTENT(IN)    :: PDKDDVDZ  ! derivative of mixing coefficient as a
!                                                    ! function of vertical gradient of wind
!                                                    ! (at mid levels)                       (m2)
REAL, DIMENSION(KI),      INTENT(IN)    :: PSFLUX_U  ! Surface flux u'w'                     (m2/s2)
REAL, DIMENSION(KI,SB%NLVL), INTENT(IN)    :: PFORC_U   ! tendency of wind due to canopy drag   (m/s2)
REAL, DIMENSION(KI,SB%NLVL), INTENT(IN)    :: PDFORC_UDU! formal derivative of the tendency of
!                                                    ! wind due to canopy drag              (1/s)
REAL, DIMENSION(KI,SB%NLVL), INTENT(OUT)   :: PUW       ! turbulent flux (at half levels)       (m2/s2)
REAL, DIMENSION(KI),      INTENT(OUT)   :: PALFA     !  V+(1) = alfa u'w' + beta
REAL, DIMENSION(KI),      INTENT(OUT)   :: PBETA     !  V+(1) = alfa u'w' + beta
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER                     :: JLAYER   ! loop counter on layers
!
REAL, DIMENSION(KI,SB%NLVL)   :: ZDUDZ    ! dU/dz at mid levels
REAL, DIMENSION(KI,SB%NLVL)   :: ZF       ! turbulent flux at mid levels
REAL, DIMENSION(KI,SB%NLVL)   :: ZDFDDVDZ ! derivative of turbulent flux as a
!                                       ! function of vertical gradient of wind variable
!                                       ! (at mid levels)
REAL, DIMENSION(KI,SB%NLVL)   :: ZEXT     ! external forcing at full levels
REAL, DIMENSION(KI,SB%NLVL)   :: ZDEXTDV  ! derivative of external forcing as a
!                                       ! function of vertical variable
!                                       ! (at full levels)
REAL, DIMENSION(KI,SB%NLVL)   :: ZU       ! work variable : wind at futur instant 
!                                       ! (or past at the end of the routine) 
REAL, DIMENSION(KI)         :: ZDUADT   ! dUa/dt   at forcing level
LOGICAL                     :: LIMPL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!
!*    1. initializations
!        ---------------
!
!* external forces
!
IF (LHOOK) CALL DR_HOOK('CANOPY_EVOL_WIND',0,ZHOOK_HANDLE)
ZEXT = 0.
ZDEXTDV = 0.
!
!* coupling coefficient with the surface
!
PALFA(:) = 0.
PBETA(:) = SB%XU(:,1)
!
!-------------------------------------------------------------------------------
!
!*    5. Forcing due to drag (at full levels)
!        -------------------
!
!* drag force by canopy
!
ZEXT = ZEXT + PFORC_U(:,:)
!
ZDEXTDV = ZDEXTDV + PDFORC_UDU(:,:)
!
!-------------------------------------------------------------------------------
!
!*    6. External forcing due to large-scale forces (at full levels)
!        ------------------------------------------
!
!
!* forces due to large-scale forcing
!
! These are computed from wind evolution equation at forcing level :
!
! dUa/dt = Large_Scale_Forcing - d(u'w')/dz|z=forcing_level
!
! because vertical derivative of turbulent flux is not available at forcing
! level, one must make the assumption that the turbulent flux is uniform
! between the forcing level and the level just below.
! This means that one assume that the forcing layer is in the inertail sublayer
! (where turbulent fluxes are constant).
!
ZDUADT(:) = ( PWIND(:) - SB%XU(:,SB%NLVL) ) /PTSTEP
!
!* for smoother evolution at large time steps, does not include explicitely
!  forcing term in lower layers
DO JLAYER=1,SB%NLVL-1
  ZEXT(:,JLAYER) = ZEXT(:,JLAYER)
END DO

!* evolution of forcing layer forced by forcing...
ZEXT(:,SB%NLVL) = ZDUADT(:)
!
 CALL  CANOPY_EVOL_FIELD(KI, SB%NLVL, PTSTEP, KIMPL, PK, PDKDDVDZ,       &
                         PSFLUX_U, PFORC_U, PDFORC_UDU, SB%XDZ, SB%XDZF, &
                         ZEXT, ZDEXTDV, SB%XU, PUW, PALFA, PBETA          ) 
!
!-------------------------------------------------------------------------------
!
!*   10. Security at all levels : positivity of wind speed
!        ----------------------
!
SB%XU(:,:) = MAX(SB%XU,0.)
!
IF (LHOOK) CALL DR_HOOK('CANOPY_EVOL_WIND',1,ZHOOK_HANDLE)
!
!----------------------------------------------------------------
!
END SUBROUTINE CANOPY_EVOL_WIND
