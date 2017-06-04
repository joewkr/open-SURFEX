!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE TEB_CANOPY(KI, SB, PBLD, PBLD_HEIGHT, PWALL_O_HOR, PPA, PRHOA, PDUWDU_ROAD, PUW_ROOF, &
                      PDUWDU_ROOF, PH_WALL, PH_ROOF, PE_ROOF, PAC_ROAD, PAC_ROAD_WAT, PFORC_U,    &
                      PDFORC_UDU, PFORC_E, PDFORC_EDE, PFORC_T, PDFORC_TDT, PFORC_Q, PDFORC_QDQ)
!     ###############################################################################
!
!!****  *TEB_CANOPY_n * - prepares forcing for canopy air model
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
!!      Original    07/2006
!!---------------------------------------------------------------
!
USE MODD_CANOPY_n, ONLY : CANOPY_t
!
USE MODD_CSTS,         ONLY : XRD, XCPD, XP00, XG, XPI
USE MODD_SURF_PAR,     ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_CANOPY
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER,                  INTENT(IN)    :: KI        ! number of points
TYPE(CANOPY_t), INTENT(INOUT) :: SB
!
REAL, DIMENSION(KI),      INTENT(IN)    :: PBLD        ! building density                    (-)
REAL, DIMENSION(KI),      INTENT(IN)    :: PBLD_HEIGHT ! building height                     (m)
REAL, DIMENSION(KI),      INTENT(IN)    :: PWALL_O_HOR ! wall surf. / hor. surf.             (-)
!
REAL, DIMENSION(KI),      INTENT(IN)    :: PPA       ! air pressure                          (Pa)
REAL, DIMENSION(KI),      INTENT(IN)    :: PRHOA     ! air density                           (kg/m3)
!
REAL, DIMENSION(KI),      INTENT(IN)    :: PDUWDU_ROAD  ! derivative of road friction flux   (m/s)
REAL, DIMENSION(KI),      INTENT(IN)    :: PUW_ROOF  ! friction flux for roof surfaces       (m2/s2)
REAL, DIMENSION(KI),      INTENT(IN)    :: PDUWDU_ROOF  ! derivative of roof friction flux   (m/s)
REAL, DIMENSION(KI),      INTENT(IN)    :: PH_WALL   ! flux of heat for wall surfaces        (W/m2)
REAL, DIMENSION(KI),      INTENT(IN)    :: PH_ROOF   ! flux of heat for roof surfaces        (W/m2)
REAL, DIMENSION(KI),      INTENT(IN)    :: PE_ROOF   ! flux of vapor for roof surfaces       (kg/m2/s)
REAL, DIMENSION(KI),      INTENT(IN)    :: PAC_ROAD  ! road aerodynamical conductance        ()
REAL, DIMENSION(KI),      INTENT(IN)    :: PAC_ROAD_WAT ! road water aerodynamical conductance        ()
!
REAL, DIMENSION(KI,SB%NLVL), INTENT(OUT)   :: PFORC_U   ! tendency of wind due to canopy drag   (m/s2)
REAL, DIMENSION(KI,SB%NLVL), INTENT(OUT)   :: PDFORC_UDU! formal derivative of the tendency of
!                                                    ! wind due to canopy drag               (1/s)
REAL, DIMENSION(KI,SB%NLVL), INTENT(OUT)   :: PFORC_E   ! tendency of TKE  due to canopy drag   (m2/s3)
REAL, DIMENSION(KI,SB%NLVL), INTENT(OUT)   :: PDFORC_EDE! formal derivative of the tendency of
!                                                    ! TKE  due to canopy drag               (1/s)
REAL, DIMENSION(KI,SB%NLVL), INTENT(OUT)   :: PFORC_T   ! tendency of Temp due to canopy drag   (T/s)
REAL, DIMENSION(KI,SB%NLVL), INTENT(OUT)   :: PDFORC_TDT! formal derivative of the tendency of
!                                                    ! Temp due to canopy drag               (1/s)
REAL, DIMENSION(KI,SB%NLVL), INTENT(OUT)   :: PFORC_Q   ! tendency of Temp due to canopy drag   (kg/m3/s)
REAL, DIMENSION(KI,SB%NLVL), INTENT(OUT)   :: PDFORC_QDQ! formal derivative of the tendency of
!                                                    ! Temp due to canopy drag               (1/s)
!
!*      0.2    declarations of local variables
!
INTEGER                  :: JLAYER    ! loop counter on canopy heights
!         
REAL, DIMENSION(KI,SB%NLVL) :: ZCDRAG    ! drag coefficient in canopy
REAL, DIMENSION(KI,SB%NLVL) :: ZSH       ! horizontal surface of building
                                      ! (road&roof) for each canopy level
REAL, DIMENSION(KI,SB%NLVL) :: ZSV       ! vertical surface of building
                                      ! (walls) for each canopy level
REAL, DIMENSION(KI,SB%NLVL) :: ZFORC
REAL, DIMENSION(KI,SB%NLVL) :: ZDENSITY
REAL, DIMENSION(KI,SB%NLVL) :: ZAIRVOL   ! Fraction of air for each canopy level total volume
REAL, DIMENSION(KI,SB%NLVL) :: ZP        ! pressure              at full levels
REAL, DIMENSION(KI,SB%NLVL) :: ZEXN      ! Exner function        at full levels
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
!*      1.     Computations of canopy grid characteristics
!              -------------------------------------------
!
!
!*      1.2    Proportion of horizontal surfaces for each canopy level
!
IF (LHOOK) CALL DR_HOOK('TEB_CANOPY',0,ZHOOK_HANDLE)
ZSH(:,:) = 0.
ZSH(:,1) = (1.-PBLD(:))
!
WHERE( SB%XZF(:,2)>=PBLD_HEIGHT(:) ) ZSH(:,2) = PBLD(:) ! the roofs cannot be at the same level as roads
DO JLAYER = 2,SB%NLVL-1
  WHERE( SB%XZF(:,JLAYER)<PBLD_HEIGHT(:) .AND. &
         SB%XZF(:,JLAYER+1)>=PBLD_HEIGHT(:) ) ZSH(:,JLAYER) = PBLD(:)
END DO
WHERE( SB%XZF(:,SB%NLVL)<PBLD_HEIGHT(:) ) ZSH(:,SB%NLVL) = PBLD(:)
!
!*      2.1    Drag coefficient by walls
!              -------------------------
!
ZCDRAG(:,:) = 0.40
!* integration for all canyon directions (for facing wind walls)
ZCDRAG(:,:) = ZCDRAG(:,:) / XPI
!
!*      1.4    No building volume
!
! * in order to take into account building volume, further developments must be
!   done in the atmospheric model.
!   If these changes are not done, to take into account building volume in the
!   present routine alone would not be energetically coeherent (there would be
!   too much energy release for heat and vapor or consumed for wind).
!
ZAIRVOL = 1.
!
!*      1.2    Discretization on each canopy level
!
DO JLAYER=1,SB%NLVL
  ZDENSITY(:,JLAYER) = PWALL_O_HOR(:)
ENDDO  
!
 CALL CANOPY(KI, SB, PBLD_HEIGHT, ZDENSITY, ZCDRAG, ZAIRVOL, &
            ZSV, ZFORC, PFORC_U, PDFORC_UDU, PFORC_E, PDFORC_EDE      )
!
!-------------------------------------------------------------------------------------
!
!*      2.     Computations of wind tendency due to canopy drag
!              ------------------------------------------------
!
! Ext = - Cdrag  * u- * u- * Sv/Vair        vertical surfaces or trees
!       - u'w'(roof)       * Sh/Vair        horizontal surfaces (except road)
!       - u'w'(road)       * Sh/Vair        horizontal surfaces (road)
!
! with Vair = Vair/Vtot * Vtot = (Vair/Vtot) * Stot * Dz
! and  Sv/Vair = (Sv/Stot) * Stot/Vair = (Sv/Stot) / (Vair/Vtot) / Dz
! and  Sh/Vair = (Sh/Stot) * Stot/Vair = (Sv/Stot) / (Vair/Vtot) / Dz
!
!* Note that for the time being, air is assumed to occupy all the space of the grid
! (buildings have no volume), so that Vair = Vtot
!
ZFORC(:,:) = ZSH(:,:)/ZAIRVOL(:,:)/SB%XDZ(:,:)
!
!*      2.3    Drag force by roof surfaces
!              ---------------------------
!
!* drag force by horizontal surfaces
!
DO JLAYER=2,SB%NLVL
  PFORC_U   (:,JLAYER) = PFORC_U   (:,JLAYER) + PUW_ROOF   (:) * ZFORC(:,JLAYER)
  PDFORC_UDU(:,JLAYER) = PDFORC_UDU(:,JLAYER) + PDUWDU_ROOF(:) * ZFORC(:,JLAYER)
END DO
!
!*      2.4    Drag force by road surfaces
!              ---------------------------
!
!PFORC_U(:,1)    = PUW_ROAD(:) / SB%XDZ(:,1) * ZSH(:,1)
PFORC_U   (:,1) = PFORC_U   (:,1)    
PDFORC_UDU(:,1) = PDFORC_UDU(:,1) + PDUWDU_ROAD(:) * ZSH(:,1)/SB%XDZ(:,1)
!
!-------------------------------------------------------------------------------------
!
!*      4.     Computations of potential temperature tendency due to canopy drag
!              -----------------------------------------------------------------
!
PFORC_T   (:,:) = 0.
PDFORC_TDT(:,:) = 0.
!
!*      4.1    Heating from the road surface flux
!              ----------------------------------
!
!* surface flux
!
PDFORC_TDT(:,1) = PDFORC_TDT(:,1) - PAC_ROAD(:)
!!* Warning, the "  - PAC_ROAD(:)  " term in the equation above is used
!   to improve stability for the first layer of canopy. But the TEB equations
!   for road temperature are not implicited, so this implies that the energy
!   coming from the road as seen by canopy is not equal to the one emitted by
!   TEB.
!   This has no consequence on the conservation of the energy between the
!   surface and the atmosphere, because the heat flux goes directly from TEB to
!   SURF_ATM and then to the atmosphere. Only the canopy air profiles are
!   affected.
!
!
!*      4.2    Heating from the walls surface flux
!              -----------------------------------
!
DO JLAYER=1,SB%NLVL
  ZFORC(:,JLAYER) = 1. / ZAIRVOL(:,JLAYER) / SB%XDZ(:,JLAYER) / PRHOA(:) / XCPD 
  PFORC_T   (:,JLAYER) = PFORC_T(:,JLAYER) + PH_WALL * ZSV(:,JLAYER) * ZFORC(:,JLAYER)
  PDFORC_TDT(:,JLAYER) = PDFORC_TDT(:,JLAYER) + 0.
END DO
!
!*      4.3    Heating from the roof surface flux
!              ----------------------------------
!
DO JLAYER=2,SB%NLVL
  PFORC_T   (:,JLAYER) = PFORC_T(:,JLAYER) + PH_ROOF * ZSH(:,JLAYER) * ZFORC(:,JLAYER)
  PDFORC_TDT(:,JLAYER) = PDFORC_TDT(:,JLAYER) + 0.
END DO
!
!-------------------------------------------------------------------------------------
!
!*      5.     Conversion into temperature tendency
!              ------------------------------------
!
DO JLAYER=1,SB%NLVL
  ZP(:,JLAYER) = PPA(:) + XG * PRHOA(:) * (SB%XZ(:,SB%NLVL) - SB%XZ(:,JLAYER))
END DO
ZEXN = (ZP/XP00)**(XRD/XCPD)
!
PFORC_T    = PFORC_T    * ZEXN
PDFORC_TDT = PDFORC_TDT * ZEXN
!
!-------------------------------------------------------------------------------------
!
!*      6.     Computations of humidity tendency due to canopy
!              -----------------------------------------------
!
!
PFORC_Q(:,:) = 0.
PDFORC_QDQ(:,:) = 0.
!
!
!*      4.1    Evaporation from the road surface flux
!              --------------------------------------
!
!* surface flux
!
!
PDFORC_QDQ(:,1) = PDFORC_QDQ(:,1) - PAC_ROAD_WAT(:)
!!* Warning, the "  - PAC_ROAD_WAT(:)  " term in the equation above is used
!   to improve stability for the first layer of canopy. But the TEB equations
!   for road latent heat flux are not implicited, so this implies that the energy
!   coming from the road as seen by canopy is not equal to the one emitted by
!   TEB.
!   This has no consequence on the conservation of the energy between the
!   surface and the atmosphere, because the latent heat flux goes directly from TEB to
!   SURF_ATM and then to the atmosphere. Only the canopy air profiles are
!   affected.
!
!*      4.2    Evaporation from the roof surface flux
!              --------------------------------------
!
DO JLAYER=2,SB%NLVL
  PFORC_Q   (:,JLAYER) = PFORC_Q   (:,JLAYER) + PE_ROOF * ZSH(:,JLAYER)/ZAIRVOL(:,JLAYER)/SB%XDZ(:,JLAYER)
  PDFORC_QDQ(:,JLAYER) = PDFORC_QDQ(:,JLAYER) + 0.
END DO
IF (LHOOK) CALL DR_HOOK('TEB_CANOPY',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE TEB_CANOPY
