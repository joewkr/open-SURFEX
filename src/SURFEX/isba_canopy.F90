!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE ISBA_CANOPY (PCDRAG, KI, SB, PHEIGHT, PCANOPY_DENSITY, PUW_GROUND, PDUWDU_GROUND, &
                        PFORC_U, PDFORC_UDU, PFORC_E, PDFORC_EDE)  
!     ###############################################################################
!
!!****  *ISBA_CANOPY_n * - prepares forcing for canopy air model
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
USE MODD_CSTS,         ONLY : XRD, XCPD, XP00, XG
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
!
REAL, INTENT(IN) :: PCDRAG
!
INTEGER,                  INTENT(IN)    :: KI        ! number of points
TYPE(CANOPY_t), INTENT(INOUT) :: SB
REAL, DIMENSION(KI),      INTENT(IN)    :: PHEIGHT     ! canopy height                       (m)
REAL, DIMENSION(KI),      INTENT(IN)    :: PCANOPY_DENSITY ! canopy density                  (-)
!
REAL, DIMENSION(KI),      INTENT(IN)    :: PUW_GROUND  ! friction flux for ground surface       (m2/s2)
REAL, DIMENSION(KI),      INTENT(IN)    :: PDUWDU_GROUND  ! derivative of ground friction flux   (m/s)
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
INTEGER                  :: JLAYER, JJ    ! loop counter on canopy heights
!         
REAL, DIMENSION(KI,SB%NLVL) :: ZCDRAG    ! drag coefficient in canopy
REAL, DIMENSION(KI,SB%NLVL) :: ZDENSITY  ! vegetation density for each canopy level
REAL, DIMENSION(KI,SB%NLVL) :: ZSV       ! vertical surface for each canopy level
REAL, DIMENSION(KI,SB%NLVL) :: ZFORC
REAL, DIMENSION(KI,SB%NLVL) :: ZAIRVOL   ! Fraction of air for each canopy level total volume
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
!*      1.     Computations of canopy grid characteristics
!              -------------------------------------------
!
!
!*      1.1    Proportion of leaves for each canopy level 
!             (parabolic shape, maximum at mid canopy height, with the same
!             total LAI on the canopy)
!
IF (LHOOK) CALL DR_HOOK('ISBA_CANOPY',0,ZHOOK_HANDLE)
ZDENSITY(:,:) = 0.
DO JLAYER = 1,SB%NLVL
  DO JJ = 1,KI
    IF (PHEIGHT(JJ)>0.) THEN
      ZDENSITY(JJ,JLAYER) = 1.5 * &
       MAX(PCANOPY_DENSITY(JJ)*4.*SB%XZ(JJ,JLAYER)*(PHEIGHT(JJ)-SB%XZ(JJ,JLAYER))/PHEIGHT(JJ)**2, 0.)
    ENDIF
  ENDDO
END DO
!
!*      2.1    Drag coefficient by vegetation (Patton et al 2001)
!              ------------------------------
!
ZCDRAG(:,:) = PCDRAG
!
!*      1.4    No building volume
!
! * in order to take into account building volume, further developments must be
!   done in the atmospheric model.
!   If these changes are not done, to take into account building volume in the
!   present routine alone would not be energetically coeherent (there would be
!   too much energy release for heat and vapor or consumed for wind).
!
ZAIRVOL(:,:) = 1.
!
!*      1.2    Discretization on each canopy level
!
 CALL CANOPY(KI, SB, PHEIGHT, ZDENSITY, ZCDRAG, ZAIRVOL, ZSV, &
             ZFORC, PFORC_U, PDFORC_UDU, PFORC_E, PDFORC_EDE )
!
!
!*      2.4    Drag force by ground surface
!              ----------------------------
!
PFORC_U   (:,1) = PUW_GROUND(:) / SB%XDZ(:,1)
PDFORC_UDU(:,1) = PDFORC_UDU(:,1) + PDUWDU_GROUND(:) / SB%XDZ(:,1)

!-------------------------------------------------------------------------------------
!
!*      3.2    Destruction of TKE due to small-scale motions forced by leaves
!              --------------------------------------------------------------
!
! from Kanda and Hino (1994)
!
! Ext = - Cd * e * u  * Sv        trees
!
PFORC_E   (:,:) = PFORC_E    - 2.*SB%XTKE(:,:)*ZFORC(:,:)
PDFORC_EDE(:,:) = PDFORC_EDE - 2.*ZFORC(:,:)
!
IF (LHOOK) CALL DR_HOOK('ISBA_CANOPY',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE ISBA_CANOPY
