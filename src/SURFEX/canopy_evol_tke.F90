!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE CANOPY_EVOL_TKE(SB, KI, PTSTEP, PRHOA, PFORC_E, PDFORC_EDE, &
                                 PTH, PUW, PWTH, PWQ, PLEPS )  
!     #########################################
!
!!****  *CANOPY_EVOL_TKE* - evolution of wind in canopy 
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
USE MODI_TRIDIAG_SURF
USE MODD_CANOPY_TURB, ONLY : XCED, XTKEMIN
USE MODD_CSTS,        ONLY : XG, XRD, XRV
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
TYPE(CANOPY_t), INTENT(INOUT) :: SB
!
INTEGER,                  INTENT(IN)    :: KI        ! number of horizontal points
REAL,                     INTENT(IN)    :: PTSTEP    ! time-step                             (s)
REAL, DIMENSION(KI),      INTENT(IN)    :: PRHOA     ! Air density                           (kg/m3)
REAL, DIMENSION(KI,SB%NLVL), INTENT(IN)    :: PFORC_E   ! tendency of wind due to canopy drag   (m2/s3)
REAL, DIMENSION(KI,SB%NLVL), INTENT(IN)    :: PDFORC_EDE! formal derivative of the tendency of
!                                                    ! wind due to canopy drag               (1/s)
REAL, DIMENSION(KI,SB%NLVL), INTENT(IN)    :: PTH       ! pot. temp. at canopy levels           (K)
REAL, DIMENSION(KI,SB%NLVL), INTENT(IN)    :: PUW       ! turbulent flux (at half levels)       (m2/s2)
REAL, DIMENSION(KI,SB%NLVL), INTENT(IN)    :: PWTH      ! w'Th' flux (at half levels)           (mK/s)
REAL, DIMENSION(KI,SB%NLVL), INTENT(IN)    :: PWQ       ! w'q'  flux (at half levels)           (kg/m2/s)
REAL, DIMENSION(KI,SB%NLVL), INTENT(IN)    :: PLEPS     ! dissipative length (full levels)      (m)
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER                    :: JLAYER   ! loop counter on layers
!
REAL, DIMENSION(KI,SB%NLVL)   :: ZDUDZ    ! dU/dz at mid levels
REAL, DIMENSION(KI,SB%NLVL)   :: ZDP      ! dynamical production at full levels
!                                      ! (at full levels)
REAL, DIMENSION(KI,SB%NLVL)   :: ZTP      ! thermal   production at full levels
!                                      ! (at full levels)
REAL, DIMENSION(KI,SB%NLVL)   :: ZDISS_O_TKE ! dissipation/TKE ratio at full levels
REAL, DIMENSION(KI,SB%NLVL)   :: ZF       ! turbulent flux at mid levels
REAL, DIMENSION(KI,SB%NLVL)   :: ZDFDDVDZ ! derivative of turbulent flux as a
!                                      ! function of vertical gradient of wind variable
!                                      ! (at mid levels)
REAL, DIMENSION(KI,SB%NLVL)   :: ZEXT     ! external forcing at full levels
REAL, DIMENSION(KI,SB%NLVL)   :: ZDEXTDV  ! derivative of external forcing as a
!                                      ! function of vertical variable
!                                      ! (at full levels)
REAL, DIMENSION(KI,SB%NLVL)   :: ZTKE     ! TKE     at canopy levels (work var.)
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
IF (LHOOK) CALL DR_HOOK('CANOPY_EVOL_TKE',0,ZHOOK_HANDLE)
ZEXT = 0.
ZDEXTDV = 0.
!
!* turbulent flux
!
ZF       = 0.
ZDFDDVDZ = 0.
!
!-------------------------------------------------------------------------------
!
!*    2. wind vertical derivative mid layers (at half levels below full levels)
!        ------------------------
!
ZDUDZ(:,:) = -999.
DO JLAYER=2,SB%NLVL
  ZDUDZ(:,JLAYER) = (SB%XU(:,JLAYER) - SB%XU(:,JLAYER-1)) / SB%XDZF(:,JLAYER)
END DO
!-------------------------------------------------------------------------------
!
!*    3. Dynamical production of TKE (at full levels)
!        ---------------------------
!
ZDP(:,:) = -999.
!
!* first level using an extrapolation using a 1/z law
ZDP(:,1) = - PUW(:,2) * ZDUDZ(:,2) * (SB%XZF(:,2)/SB%XZ(:,1))

! other levels
DO JLAYER=2,SB%NLVL-1
  ZDP(:,JLAYER) = - 0.5 * (PUW(:,JLAYER)   * ZDUDZ(:,JLAYER)  ) &
                    - 0.5 * (PUW(:,JLAYER+1) * ZDUDZ(:,JLAYER+1))  
END DO
!
!* upper level using an extrapolation using a 1/z law
ZDP(:,SB%NLVL) = - PUW(:,SB%NLVL) * ZDUDZ(:,SB%NLVL) * (SB%XZF(:,SB%NLVL)/SB%XZ(:,SB%NLVL))
!
!* adds dynamical production in non-transport forces
ZEXT    = ZEXT    + ZDP
ZDEXTDV = ZDEXTDV + 0.
!
!-------------------------------------------------------------------------------
!
!*    4. Thermal production of TKE (at full levels)
!        -------------------------
!
ZTP(:,:) = -999.
!
! other levels
DO JLAYER=1,SB%NLVL-1
  ZTP(:,JLAYER) = XG/PTH(:,JLAYER) * 0.5 * ( (PWTH(:,JLAYER) + PWTH(:,JLAYER+1) )          &
                             + (XRV/XRD-1) * (PWQ (:,JLAYER) + PWQ (:,JLAYER)   )/PRHOA(:) )  
END DO
!
!* upper level
ZTP(:,SB%NLVL) = XG/PTH(:,SB%NLVL) * PWTH(:,SB%NLVL)
!
!* adds dynamical production in non-transport forces
ZEXT    = ZEXT    + ZTP
ZDEXTDV = ZDEXTDV + 0.
!
!-------------------------------------------------------------------------------
!
!*    4. Dissipation/TKE ratio (to prepare implicitation of dissipation)
!        ---------------------
!
ZDISS_O_TKE = - XCED * SQRT(SB%XTKE(:,:)) / PLEPS(:,:)
!
!* adds dissipation in non-transport forces
ZEXT    = ZEXT    +       ZDISS_O_TKE * SB%XTKE(:,:)
ZDEXTDV = ZDEXTDV + 1.5 * ZDISS_O_TKE
!
!-------------------------------------------------------------------------------
!
!*    6. Adds Creation force due to canopy
!        ---------------------------------
!
!
ZEXT    = ZEXT    + PFORC_E(:,:)
ZDEXTDV = ZDEXTDV + PDFORC_EDE(:,:)


!-------------------------------------------------------------------------------
!
!*    7. Evolution of TKE due to Dyn prod, Dissipation and Drag production
!        -----------------------------------------------------------------
!
!* note that dissipation is implicited
!
 CALL TRIDIAG_SURF(SB%XTKE,ZF,ZDFDDVDZ,ZEXT,ZDEXTDV,PTSTEP,SB%XDZF,SB%XDZ,ZTKE)  
!
!-------------------------------------------------------------------------------
!
!*    7. New value of TKE (at full levels)
!        ----------------
!
SB%XTKE(:,:) = ZTKE(:,:)
!
!-------------------------------------------------------------------------------
!
!*    8. Security at all levels : set minimum threshold
!        ----------------------
!
SB%XTKE(:,:) = MAX(SB%XTKE,XTKEMIN)
!
IF (LHOOK) CALL DR_HOOK('CANOPY_EVOL_TKE',1,ZHOOK_HANDLE)
!
!----------------------------------------------------------------
!
END SUBROUTINE CANOPY_EVOL_TKE
