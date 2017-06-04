!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE CANOPY(KI, SB, PHEIGHT, PDENSITY, PCDRAG, PAIRVOL, PSV, &
                  PFORC, PFORC_U, PDFORC_UDU, PFORC_E, PDFORC_EDE   )  
!     ###############################################################################
!
!!****  *ISBA_CANOPY_n * - prepares forcing for canopy air model
!!
!!    SB%XURPOSE
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
!!      Original    07/2006
!!---------------------------------------------------------------
!
USE MODD_CANOPY_n, ONLY : CANOPY_t
!
USE MODD_CSTS,         ONLY : XRD, XCPD, XP00, XG
USE MODD_SURF_PAR,     ONLY : XUNDEF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER,                  INTENT(IN)    :: KI        ! number of points
TYPE(CANOPY_t), INTENT(INOUT) :: SB
REAL, DIMENSION(KI), INTENT(IN)    :: PHEIGHT   ! canopy height                       (m)
REAL, DIMENSION(KI,SB%NLVL), INTENT(IN)    :: PDENSITY  ! canopy density                  (-)
REAL, DIMENSION(KI,SB%NLVL), INTENT(IN)    :: PCDRAG
!
REAL, DIMENSION(KI,SB%NLVL), INTENT(IN)    :: PAIRVOL   ! Fraction of air for each canopy level total volume
!
REAL, DIMENSION(KI,SB%NLVL), INTENT(OUT)   :: PSV       ! vertical surface of building
                                                     ! (walls) for each canopy level
REAL, DIMENSION(KI,SB%NLVL), INTENT(OUT)   :: PFORC     !
!
REAL, DIMENSION(KI,SB%NLVL), INTENT(OUT)   :: PFORC_U   ! tendency of wind due to canopy drag   (m/s2)
REAL, DIMENSION(KI,SB%NLVL), INTENT(OUT)   :: PDFORC_UDU! formal derivative of the tendency of
!                                                    ! wind due to canopy drag               (1/s)
REAL, DIMENSION(KI,SB%NLVL), INTENT(OUT)   :: PFORC_E   ! tendency of TKE  due to canopy drag   (m2/s3)
REAL, DIMENSION(KI,SB%NLVL), INTENT(OUT)   :: PDFORC_EDE! formal derivative of the tendency of
!                                                    ! TKE  due to canopy drag               (1/s)
!
!*      0.2    declarations of local variables
!
INTEGER                  :: JLAYER    ! loop counter on canopy heights      
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('CANOPY',0,ZHOOK_HANDLE)
!
!*      1.     Computations of canopy grid characteristics
!              -------------------------------------------
!
!
!*      1.2    Discretization on each canopy level
!
PSV(:,:) = 0.
DO JLAYER = 1,SB%NLVL-1
  !
  WHERE ( SB%XZF(:,JLAYER) < PHEIGHT(:) )
    PSV(:,JLAYER) = PDENSITY(:,JLAYER) / PHEIGHT(:)
    WHERE ( SB%XZF(:,JLAYER+1) > PHEIGHT(:) )
      PSV(:,JLAYER) = PSV(:,JLAYER) * ( PHEIGHT(:) - SB%XZF(:,JLAYER) )
    ELSEWHERE
      PSV(:,JLAYER) = PSV(:,JLAYER) *  SB%XDZ(:,JLAYER)
    END WHERE
  END WHERE
  !
END DO
!
PFORC(:,:) = PCDRAG(:,:) * SB%XU(:,:) * PSV(:,:)/PAIRVOL(:,:)/SB%XDZ(:,:)
!
!-------------------------------------------------------------------------------------
!
!*      2.     Computations of wind tendency due to canopy drag
!              ------------------------------------------------
!
PFORC_U    = 0.
PDFORC_UDU = 0.
!
! Ext = - Cdrag  * u- * u- * Sv       tree canopy drag
!       - u'w'(ground)     * Sh       horizontal surfaces (ground)
!
!
!*      2.2    Drag force by wall surfaces
!              ---------------------------
!
!* drag force by vertical surfaces
!
PFORC_U   (:,:) = PFORC_U    -      PFORC(:,:) * SB%XU(:,:)
PDFORC_UDU(:,:) = PDFORC_UDU - 2. * PFORC(:,:)
!
!-------------------------------------------------------------------------------------
!
!*      3.     Computations of TKE  tendency due to canopy drag
!              ------------------------------------------------
!
!* Tendency due to drag for TKE
!
PFORC_E   (:,:) = 0.
PDFORC_EDE(:,:) = 0.
!
!*      3.1    Creation of TKE by wake
!              -----------------------
!
! from Kanda and Hino (1994)
!
! Ext = + Cd * u+^3  * Sv/Vair        vertical surfaces or trees
!
! with Vair = Vair/Vtot * Vtot = (Vair/Vtot) * Stot * Dz
! and  Sv/Vair = (Sv/Stot) * Stot/Vair = (Sv/Stot) / (Vair/Vtot) / Dz
!
PFORC_E    = PFORC_E    + PFORC(:,:) * SB%XU(:,:)**2
PDFORC_EDE = PDFORC_EDE + 0.
!
!
IF (LHOOK) CALL DR_HOOK('CANOPY',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE CANOPY
