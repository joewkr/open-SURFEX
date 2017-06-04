!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
! ######spl
MODULE MODE_DSTMBLUTL ! [mdl] Mobilization utilities
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
CONTAINS
!
!----------------------------------------------------------------------------------------
SUBROUTINE WND_FRC_THR_SLT_GET(PDNS_MDP, PDP, PWND_FRC_THR_SLT)  
! Purpose: Compute dry threshold friction velocity for saltation
!++grini use dstgrd ! [mdl] Dust grid sizes
!++grini use pmgrid ! [mdl] Spatial resolution parameters
!++grini use dstcst ! [mdl] Physical constants for dust routines
USE MODD_DSTMBL,    ONLY : XDNS_SLT
USE MODD_CSTS,      ONLY : XG
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
! Input
!++grini real,intent(in)::dns_aer(dst_nbr) ! [kg m-3] Particle density
!++grini real,intent(in)::dmt_aer(dst_nbr) ! [m] Particle diameter
REAL, DIMENSION(:), INTENT(IN)   :: PDNS_MDP        ! [kg m-3] Midlayer density
REAL, INTENT(IN)  :: PDP   
! Output
REAL, DIMENSION(:), INTENT(OUT) :: PWND_FRC_THR_SLT ! [m s-1] Threshold friction velocity for saltation
!
! Local
REAL :: ZRYN, ZRYN1 ! [frc] Threshold friction Reynolds number approximation for optimal size
REAL :: ZDNS_FCT    ! Density ratio factor for saltation calculation
REAL :: ZICF_FCT    ! Interparticle cohesive forces factor for saltation calculation
REAL :: ZTMP        ! Factor in saltation computation
INTEGER :: I        ! [idx] Counting index
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE   
!
IF (LHOOK) CALL DR_HOOK('MODE_DSTMBLUTL:WND_FRC_THR_SLT_GET',0,ZHOOK_HANDLE)
!
! Initialize some variables
! MaB95 pzn. for Re*t(D_opt) circumvents iterative solution
ZRYN = 0.38d0 + 1331.0d0*(100.0d0*PDP)**1.56d0                        ! [frc] "B" MaB95 p. 16417 (5)
! Given Re*t(D_opt), compute time independent factors contributing to u*t
ZDNS_FCT                 = XDNS_SLT * XG * PDP                        ! IvW82 p. 115 (6) MaB95 p. 16417 (4)
ZICF_FCT                 = 1.0d0 + 6.0d-07/(XDNS_SLT*XG*(PDP**2.5d0)) ! [frc] IvW82 p. 115 (6) MaB95 p. 16417 (4) Interparticle cohesive forces
!
IF (ZRYN < 0.03d0) THEN
  CALL ABOR1_SFX('MODE_DSTMBLUTL:WND_FRC_THR_SLT_GET: RYN < 0.03')
ELSEIF (ZRYN < 10.0d0) THEN
  ZRYN1 = -1.0d0 + 1.928d0 * (ZRYN**0.0922d0) ! [frc] IvW82 p. 114 (3), MaB95 p. 16417 (6)
  ZRYN1 = 0.1291d0 * 0.1291d0 / ZRYN1          ! [frc] 
ELSE
  ZRYN1 = 1.0d0 - 0.0858d0 * EXP(-0.0617d0*(ZRYN-10.0d0)) ! [frc] IvW82 p. 114 (3), MaB95 p. 16417 (7)
  ZRYN1 = 0.120d0 **2 * ZRYN1**2               ! [frc]
  !ryn1=0.129*0.129*ryn1*ryn1 dans le cas mm, à vérifier
ENDIF 
!
! This method minimizes the number of square root computations performed
ZTMP = SQRT (ZICF_FCT*ZDNS_FCT*ZRYN1)
DO I = 1, SIZE(PDNS_MDP)
  PWND_FRC_THR_SLT(I) = ZTMP / SQRT(PDNS_MDP(I))
ENDDO
!
IF (LHOOK) CALL DR_HOOK('MODE_DSTMBLUTL:WND_FRC_THR_SLT_GET',1,ZHOOK_HANDLE)
!
END SUBROUTINE WND_FRC_THR_SLT_GET
!
!----------------------------------------------------------------------------------------
SUBROUTINE VWC2GWC (OFLG_MBL, PVWC_SAT, PVWC_SFC, PGWC_SFC)
! Purpose: Convert volumetric water content to gravimetric water content
!++alfgr use pmgrid ! [mdl] Spatial resolution parameters
use MODD_CSTS, only : XRHOLI  ! [kg/m3] density of liquid water
USE MODD_DSTMBL, ONLY : XDNS_SLT
!
!++alfgr use dstblm,only:dns_H2O_lqd_std ! [mdl] Boundary layer meteorology for non-vegetated land surfaces
IMPLICIT NONE
! Input
LOGICAL, DIMENSION(:), INTENT(IN) :: OFLG_MBL ! [flg] Mobilization candidate flag
REAL,    DIMENSION(:), INTENT(IN) :: PVWC_SAT ! [m3 m-3] Saturated volumetric water content (sand-dependent)
REAL,    DIMENSION(:), INTENT(IN) :: PVWC_SFC ! [m3 m-3] Volumetric water content
REAL,    DIMENSION(:), INTENT(OUT):: PGWC_SFC ! [kg kg-1] Gravimetric water content
! Local
REAL, DIMENSION(SIZE(PVWC_SAT)) :: ZDNS_BLK_DRY ! [kg m-3] Bulk density of dry surface soil
INTEGER :: I
REAL(KIND=JPRB) :: ZHOOK_HANDLE 
! Main Code
IF (LHOOK) CALL DR_HOOK('MODE_DSTMBLUTL:VWC2GWC',0,ZHOOK_HANDLE)
!
! Initialize output
DO I=1,SIZE(PVWC_SAT)
  IF (OFLG_MBL(I)) THEN
    ! Assume volume of air pores when dry equals saturated VWC
    ! This implies air pores are completely filled by water in saturated soil
    ZDNS_BLK_DRY(I) = XDNS_SLT * (1.0 - PVWC_SAT(I)) ! [kg m-3] Bulk density of dry surface soil
    PGWC_SFC    (I) = PVWC_SFC(I) * XRHOLI / ZDNS_BLK_DRY(I)
  ENDIF
ENDDO
!
IF (LHOOK) CALL DR_HOOK('MODE_DSTMBLUTL:VWC2GWC',1,ZHOOK_HANDLE)
END SUBROUTINE VWC2GWC
!
!----------------------------------------------------------------------------------------  
SUBROUTINE FRC_THR_NCR_WTR_GET(OFLG_MBL, PGWC_THR, PGWC_SFC, PFRC_THR_NCR_WTR)   
! Purpose: Compute factor by which soil moisture increases threshold friction velocity 
! This parameterization is based on FMB99
!++alfgr use pmgrid ! [mdl] Spatial resolution parameters
IMPLICIT NONE
! dans le calcul de l'humidit? du sol(a posteriori)
! Input
LOGICAL, DIMENSION(:), INTENT(IN) :: OFLG_MBL         ! I [flg] Mobilization candidate flag
REAL,    DIMENSION(:), INTENT(IN) :: PGWC_THR         ! [kg kg-1] Threshold gravimetric water content
REAL,    DIMENSION(:), INTENT(IN) :: PGWC_SFC         ! [kg kg-1] Gravimetric water content
REAL,    DIMENSION(:), INTENT(OUT):: PFRC_THR_NCR_WTR ! [frc] Factor by which moisture increases threshold friction velocity
! Local
INTEGER :: I ! [idx] Counting index
REAL(KIND=JPRB) :: ZHOOK_HANDLE 

IF (LHOOK) CALL DR_HOOK('MODE_DSTMBLUTL:FRC_THR_NCR_WTR_GET',0,ZHOOK_HANDLE)
! Main Code
! Initialize output
PFRC_THR_NCR_WTR(:) = 1.0d0 ! [frc] Factor by which moisture increases threshold friction velocity
!
DO I = 1, SIZE(PGWC_SFC)
  IF (OFLG_MBL(I)) THEN
    ! Adjust threshold velocity for inhibition by moisture
    ! frc_thr_ncr_wtr(lon_idx)=exp(22.7d0*vwc_sfc(lon_idx)) ! [frc] SRL96
    ! Compute threshold soil moisture based on clay content
    IF (PGWC_SFC(I) > PGWC_THR(I)) &
      PFRC_THR_NCR_WTR(I) = SQRT(1.0d0 + 1.21d0 * (100.0d0*(PGWC_SFC(I)-PGWC_THR(I)))**0.68d0) ! [frc] FMB99 p. 155 (15)
  ENDIF
ENDDO
! Uncomment following line to remove all dependence on gwc_sfc
! frc_thr_ncr_wtr(lon_idx)=1.0d0 ! [frc]
IF (LHOOK) CALL DR_HOOK('MODE_DSTMBLUTL:FRC_THR_NCR_WTR_GET',1,ZHOOK_HANDLE)
END SUBROUTINE FRC_THR_NCR_WTR_GET
!  
!----------------------------------------------------------------------------------------  
!++alfgr fxm: Fix this so that we can actually use rgh_mmn_mbl different for grid cells
SUBROUTINE FRC_THR_NCR_DRG_GET(PRGH_MMN_MBL, PRGH_MMN_SMT, PFRC_THR_NCR_DRG)
! Purpose: Compute factor by which surface roughness increases threshold friction velocity
! This parameterization is based on MaB95 and GMB98
!++alfgr use pmgrid ! [mdl] Spatial resolution parameters
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
! Input
REAL, INTENT(IN) :: PRGH_MMN_MBL     ! [m] Roughness length momentum for erodible surfaces
REAL, INTENT(IN) :: PRGH_MMN_SMT                   ! [m] Smooth roughness length
! Output
REAL, INTENT(OUT):: PFRC_THR_NCR_DRG ! [frc] Factor by which roughness increases threshold friction velocity
! Local
REAL :: ZWND_FRC_FSH_FRC     ! [frc] Efficient fraction of wind friction
REAL(KIND=JPRB) :: ZHOOK_HANDLE 
!
IF (LHOOK) CALL DR_HOOK('MODE_DSTMBLUTL:FRC_THR_NCR_DRG_GET',0,ZHOOK_HANDLE)
! Main Code
!
! Adjust threshold velocity for inhibition by roughness elements
ZWND_FRC_FSH_FRC = & ! [frc] MaB95 p. 16420, GMB98 p. 6207
  1.0d0 - LOG(PRGH_MMN_MBL/PRGH_MMN_SMT) / LOG(0.35d0 * ((0.1d0/PRGH_MMN_SMT)**0.8d0))
ZWND_FRC_FSH_FRC = MAX(1.e-6, MIN(1., ZWND_FRC_FSH_FRC))
IF (ZWND_FRC_FSH_FRC <= 0.0d0 .OR. ZWND_FRC_FSH_FRC > 1.0d0) &
  CALL ABOR1_SFX("MODE_DSTMBLUTL:FRC_THR_NCR_DRG_GET0: WND_FRC_FSH_FRC OUT OF RANGE")
PFRC_THR_NCR_DRG = 1.0d0 / ZWND_FRC_FSH_FRC! [frc]
! fxm: 19991012 Set frc_thr_ncr_drg=1.0, equivalent to assuming mobilization takes place at smooth roughness length
!++alfgr removed this frc_thr_ncr_drg=1.0d0
!
IF (LHOOK) CALL DR_HOOK('MODE_DSTMBLUTL:FRC_THR_NCR_DRG_GET',1,ZHOOK_HANDLE)
END SUBROUTINE FRC_THR_NCR_DRG_GET
!
!----------------------------------------------------------------------------------------
SUBROUTINE WND_FRC_SLT_GET(OFLG_MBL, PWND_FRC, PWND_RFR, PWND_RFR_THR_SLT, PWND_FRC_SLT)
! Purpose: Compute the saltating friction velocity
! Saltation increases friction speed by roughening surface, AKA "Owen's effect"
! This acts as a positive feedback to the friction speed
! GMB98 parameterized this feedback in terms of 10 m windspeeds
!++alfgr use pmgrid ! [mdl] Spatial resolution parameters
IMPLICIT NONE
! Input
LOGICAL, DIMENSION(:), INTENT(IN) :: OFLG_MBL         ! I [flg] Mobilization candidate flag
REAL,    DIMENSION(:), INTENT(IN) :: PWND_FRC         ! I [m s-1] Surface friction velocity
REAL,    DIMENSION(:), INTENT(IN) :: PWND_RFR         ! I [m s-1] Wind speed at reference height
REAL,    DIMENSION(:), INTENT(IN) :: PWND_RFR_THR_SLT ! I [m s-1] Threshold 10 m wind speed for saltation
REAL,    DIMENSION(:), INTENT(OUT):: PWND_FRC_SLT     ! O [m s-1] Saltating friction velocity
! Local
REAL :: ZWND_RFR_DLT     ! [m s-1] Reference windspeed excess over threshold
REAL :: ZWND_FRC_SLT_DLT ! [m s-1] Friction velocity increase from saltation
INTEGER :: I             ! [idx] Counting index
REAL(KIND=JPRB) :: ZHOOK_HANDLE 
!
IF (LHOOK) CALL DR_HOOK('MODE_DSTMBLUTL:WND_FRC_SLT_GET',0,ZHOOK_HANDLE)   
! Main Code
! Compute saltating friction velocity, accounting for "Owen's effect"
PWND_FRC_SLT(:) = PWND_FRC(:) ! [m s-1] Saltating friction velocity
!
DO I = 1, SIZE(PWND_FRC)
  IF (OFLG_MBL(I) .AND. PWND_RFR(I) >= PWND_RFR_THR_SLT(I)) THEN
    ! Saltation roughens the boundary layer, AKA "Owen's effect"
    ! GMB98 p. 6206 Fig. 1 shows observed/computed u* dependence on observed U(1 m)
    ! GMB98 p. 6209 (12) has u* in cm s-1 and U, Ut in m s-1, personal communication, D. Gillette, 19990529
    ! With everything in MKS, the 0.3 coefficient in GMB98 (12) becomes 0.003 
    ! Increase in friction velocity due to saltation varies as square of 
    ! difference between reference wind speed and reference threshold speed 
    ZWND_RFR_DLT     = PWND_RFR(I) - PWND_RFR_THR_SLT(I)
    ZWND_FRC_SLT_DLT = 0.003d0 * ZWND_RFR_DLT**2          ! [m s-1] Friction velocity increase from saltation GMB98 p. 6209
    PWND_FRC_SLT(I)  = PWND_FRC_SLT(I) + ZWND_FRC_SLT_DLT ! [m s-1] Saltating friction velocity
  ENDIF
ENDDO
IF (LHOOK) CALL DR_HOOK('MODE_DSTMBLUTL:WND_FRC_SLT_GET',1,ZHOOK_HANDLE)
END SUBROUTINE WND_FRC_SLT_GET
!
!----------------------------------------------------------------------------------------
SUBROUTINE FLX_MSS_HRZ_SLT_TTL_WHI79_GET(PCOEFF, OFLG_MBL, PDNS_MDP, PWND_FRC, &
                        PWND_FRC_THR_SLT, PFLX_MSS_HRZ_SLT_TTL)
! Purpose: Compute vertically integrated streamwise mass flux of particles
! Theory: Uses method proposed by White (1979)
! fxm: use surface air density not midlayer density
!++alfgr use pmgrid ! [mdl] Spatial resolution parameters
!++alfgr use dstcst ! [mdl] Physical constants for dust routines
use MODD_CSTS,      ONLY : XG  ! Gravitation constant
!
IMPLICIT NONE
! Input
REAL,    DIMENSION(:), INTENT(IN) :: PCOEFF
LOGICAL, DIMENSION(:), INTENT(IN) :: OFLG_MBL             ! I [flg] Mobilization candidate flag
REAL,    DIMENSION(:), INTENT(IN) :: PDNS_MDP             ! I [kg m-3] Midlayer density
REAL,    DIMENSION(:), INTENT(IN) :: PWND_FRC             ! I [m s-1] Surface friction velocity
REAL,    DIMENSION(:), INTENT(IN) :: PWND_FRC_THR_SLT     ! I [m s-1] Threshold friction speed for saltation
REAL,    DIMENSION(:), INTENT(OUT):: PFLX_MSS_HRZ_SLT_TTL ! O [kg m-1 s-1] Vertically integrated streamwise mass flux
! Local
REAL :: ZWND_FRC_RAT ! [frc] Ratio of wind friction threshold to wind friction
INTEGER :: I    ! [idx] Counting index for lon
REAL(KIND=JPRB) :: ZHOOK_HANDLE 
!
IF (LHOOK) CALL DR_HOOK('MODE_DSTMBLUTL:FLX_MSS_HRZ_SLT_TTL_WHI79_GET',0,ZHOOK_HANDLE)   
! Main Code
! Initialize output
PFLX_MSS_HRZ_SLT_TTL(:) = 0.0d0 ! [kg m-1 s-1]
!
DO I = 1, SIZE(PDNS_MDP)
  IF (OFLG_MBL(I) .AND. PWND_FRC(I) > PWND_FRC_THR_SLT(I)) THEN
    ZWND_FRC_RAT = PWND_FRC_THR_SLT(I) / PWND_FRC(I)            ! [frc]
    PFLX_MSS_HRZ_SLT_TTL(I) =                                 & ! [kg m-1 s-1]
      PCOEFF(I) * PDNS_MDP(I) * (PWND_FRC(I)**3.0d0) *      &
      (1.0d0 - ZWND_FRC_RAT) * (1.0d0 + ZWND_FRC_RAT)**2 / XG   ! Whi79 p. 4648 (19), MaB97 p. 16422 (28)
  ENDIF
ENDDO

IF (LHOOK) CALL DR_HOOK('MODE_DSTMBLUTL:FLX_MSS_HRZ_SLT_TTL_WHI79_GET',1,ZHOOK_HANDLE) 
END SUBROUTINE FLX_MSS_HRZ_SLT_TTL_WHI79_GET
! 
!----------------------------------------------------------------------------------------
SUBROUTINE FLX_MSS_VRT_DST_TTL_MAB95_GET(OFLG_MBL, PMSS_FRC_CLY, PFLX_MSS_HRZ_SLT_TTL, & 
                        PDST_SLT_FLX_RAT_TTL, PFLX_MSS_VRT_DST_TTL)
! Purpose: Diagnose total vertical mass flux of dust from vertically integrated streamwise mass flux
! Theory: Uses clay-based method proposed by Marticorena & Bergametti (1995)
! Their parameterization is based only on data for mss_frc_cly < 0.20
! For clayier soils, dst_slt_flx_rat_ttl may behave dramatically differently
! Whether this behavior changes when mss_frc_cly > 0.20 is unknown
! Anecdotal evidence suggests vertical flux decreases for mss_frc_cly > 0.20
! Thus we use min[mss_frc_cly,0.20] in MaB95 parameterization
IMPLICIT NONE
! Input
LOGICAL, DIMENSION(:), INTENT(IN) :: OFLG_MBL             ! I [flg] Mobilization candidate flag
REAL,    DIMENSION(:), INTENT(IN) :: PMSS_FRC_CLY         ! I [frc] Mass fraction clay
REAL,    DIMENSION(:), INTENT(IN) :: PFLX_MSS_HRZ_SLT_TTL ! I [kg m-1 s-1] Vertically integrated streamwise mass flux
REAL,    DIMENSION(:), INTENT(OUT):: PDST_SLT_FLX_RAT_TTL ! O [m-1] Ratio of vertical dust flux to streamwise mass flux
REAL,    DIMENSION(:), INTENT(OUT):: PFLX_MSS_VRT_DST_TTL ! O [kg m-2 s-1] Total vertical mass flux of dust
! Local
REAL :: ZMSS_FRC_CLY_VLD ! [frc] Mass fraction clay limited to 0.20
INTEGER :: I             ! [idx] Counting index for lon
REAL(KIND=JPRB) :: ZHOOK_HANDLE 
!
IF (LHOOK) CALL DR_HOOK('MODE_DSTMBLUTL:FLX_MSS_VRT_DST_TTL_MAB95_GET',0,ZHOOK_HANDLE)  
!
DO I = 1, SIZE(PMSS_FRC_CLY)
  IF (OFLG_MBL(I)) THEN
    ! 19990603: fxm: Dust production is EXTREMELY sensitive to this parameter, which changes flux by 3 orders of magnitude in 0.0 < mss_frc_cly < 0.20
    ZMSS_FRC_CLY_VLD = MIN(PMSS_FRC_CLY(I), 0.2) ! [frc]
    PDST_SLT_FLX_RAT_TTL(I) = & ! [m-1]
      100.0d0 * EXP(LOG(10.0d0)*(13.4d0*ZMSS_FRC_CLY_VLD - 6.0d0))              ! MaB95 p. 16423 (47)
    PFLX_MSS_VRT_DST_TTL(I) = PFLX_MSS_HRZ_SLT_TTL(I) * PDST_SLT_FLX_RAT_TTL(I) ! [kg m-1 s-1]
  ENDIF
ENDDO
!
IF (LHOOK) CALL DR_HOOK('MODE_DSTMBLUTL:FLX_MSS_VRT_DST_TTL_MAB95_GET',1,ZHOOK_HANDLE) 
END SUBROUTINE FLX_MSS_VRT_DST_TTL_MAB95_GET
!
!----------------------------------------------------------------------------------------
SUBROUTINE FLX_MSS_VRT_DST_AUST_GET(OFLG_MBL, PDNS_MDP, PWND_FRC_THR_SLT, &
        PFLX_MSS_HRZ_SLT_TTL, PDST_SLT_FLX_RAT_TTL, PFLX_MSS_VRT_DST_TTL)
! Purpose: Diagnose total vertical mass flux of dust from vertically integrated streamwise mass flux
use MODD_CSTS,      ONLY : XG  ! Gravitation constant
USE MODD_DSTMBL, ONLY : XDMT_SLT_OPT, XDMT_ERO_OPT, XDNS_SLT, XGAMA
!
IMPLICIT NONE
! Input
LOGICAL, DIMENSION(:), INTENT(IN) :: OFLG_MBL             ! I [flg] Mobilization candidate flag
REAL,    DIMENSION(:), INTENT(IN) :: PDNS_MDP             ! I [kg m-3] Midlayer density
REAL,    DIMENSION(:), INTENT(IN) :: PWND_FRC_THR_SLT     ! I [m s-1] Threshold friction speed for saltation
REAL,    DIMENSION(:), INTENT(IN) :: PFLX_MSS_HRZ_SLT_TTL ! I [kg m-1 s-1] Vertically integrated streamwise mass flux
REAL,    DIMENSION(:), INTENT(OUT):: PDST_SLT_FLX_RAT_TTL ! O [m-1] Ratio of vertical dust flux to streamwise mass flux
REAL,    DIMENSION(:), INTENT(OUT):: PFLX_MSS_VRT_DST_TTL ! O [kg m-2 s-1] Total vertical mass flux of dust
! Local
REAL, DIMENSION(SIZE(PDNS_MDP)) :: ZROP_ROA  ! [frc] ratio of particle densite to Midlayer density
REAL :: ZEXPDD, ZLNDS, ZBETA, ZBGXG
INTEGER :: I
REAL(KIND=JPRB) :: ZHOOK_HANDLE 
!
IF (LHOOK) CALL DR_HOOK('MODE_DSTMBLUTL:FLX_MSS_VRT_DST_AUST_GET',0,ZHOOK_HANDLE)  
!
! Initialize some variables
ZEXPDD = EXP(-140.7d0 * XDMT_ERO_OPT + 0.37d0)
ZLNDS  = (0.328*1.0d-4) + (0.125d0*1.0d-4*LOG(XDMT_SLT_OPT*1.0E+3))
ZBETA  = ZLNDS * ZEXPDD ! [mm]
ZBGXG  = ZBETA * XGAMA * XG
!
DO I = 1, SIZE(PDNS_MDP)
  IF (OFLG_MBL(I)) THEN
    ZROP_ROA(I) = XDNS_SLT / PDNS_MDP(I) 
    PDST_SLT_FLX_RAT_TTL(I) =          & ! [mm/m]
      (2.0d0/3.0d0) * ZBGXG * ZROP_ROA(I) / (PWND_FRC_THR_SLT(I)**2.0d0)
    PFLX_MSS_VRT_DST_TTL(I) = 1.0d-3 * & ! [kg m-1 s-1]
      PFLX_MSS_HRZ_SLT_TTL(I) * PDST_SLT_FLX_RAT_TTL(I)
  ENDIF
ENDDO
!
IF (LHOOK) CALL DR_HOOK('MODE_DSTMBLUTL:FLX_MSS_VRT_DST_AUST_GET',1,ZHOOK_HANDLE)
END SUBROUTINE FLX_MSS_VRT_DST_AUST_GET
!
!----------------------------------------------------------------------------------------
SUBROUTINE DISTRIBUTION(KMODE, PDLNDP, KTEXT, PDP, PDSRLV, PZS0)
!
USE MODD_CSTS,      ONLY : XPI
USE MODD_DSTMBL, ONLY : XDNS_SLT
!
IMPLICIT NONE
!
!INPUT, set their dimensions to their passed lengths or to KSIZE ?
INTEGER, INTENT(IN) :: KMODE  ![nbr] loop over mode
REAL,    INTENT(IN) :: PDLNDP ! delta Dp [?m]
INTEGER, DIMENSION(:), INTENT(IN) :: KTEXT  ! texture
REAL, DIMENSION(:),   INTENT(OUT) :: PDP
REAL, DIMENSION(:,:), INTENT(OUT) :: PDSRLV ! [--] Output surface relative
REAL, DIMENSION(:),   INTENT(OUT) :: PZS0   ! [m] Output rugosit? de la surface lisse (Dp/30)
!Define local variables:
REAL, DIMENSION(KMODE,12) :: ZSIGMA ! déviation géométrique standard du mode
REAL, DIMENSION(KMODE,12) :: ZMFRAC ! fraction massique du mode
REAL, DIMENSION(KMODE,12) :: ZDMED  ! diametre median du mode
REAL, DIMENSION(SIZE(PDSRLV,1),SIZE(PDSRLV,2)) :: ZDS
REAL, DIMENSION(SIZE(PDSRLV,2)) :: ZDPLN, ZSP, ZDMLN
REAL :: ZDM1, ZDM2
INTEGER :: IMOD  ! Counter for number of mode
INTEGER :: IDP   ! Counter for number of particle
INTEGER :: ITEX  ! Counter for number of texture
REAL(KIND=JPRB) :: ZHOOK_HANDLE 
!
IF (LHOOK) CALL DR_HOOK('MODE_DSTMBLUTL:DISTRIBUTION',0,ZHOOK_HANDLE)  
!
!1 Sand
!2 Loamy Sand
!3 Sandy Loam
!4 Silt Loam
!5 Loam
!6 Sandy Clay Loam
!7 Silty Clay Loam
!8 Clay Loam
!9 Sandy Clay
!10 Silty Clay
!11 Clay
!12 Silt
ZMFRAC(1,:) = (/0.0, 0.1, 0.1, 0.15, 0.15, 0.2, 0.2, 0.3, 0.35, 0.4, 0.5, 0.15/)
ZMFRAC(2,:) = (/0.1, 0.3, 0.3, 0.35, 0.50, 0.5, 0.5, 0.5, 0.00, 0.0, 0.0, 0.40/)
ZMFRAC(3,:) = (/0.9, 0.6, 0.6, 0.50, 0.35, 0.3, 0.3, 0.2, 0.65, 0.6, 0.5, 0.45/)
!
ZSIGMA(1,:) = (/1.8, 1.8, 1.8, 1.8, 1.8, 1.8, 1.8, 1.8, 1.8, 1.8, 1.8, 1.8/)
ZSIGMA(2,:) = (/1.7, 1.7, 1.7, 1.7, 1.7, 1.7, 1.7, 1.7, 1.8, 1.8, 1.8, 1.7/)
ZSIGMA(3,:) = (/1.6, 1.6, 1.6, 1.6, 1.6, 1.7, 1.7, 1.7, 1.8, 1.8, 1.8, 1.6/)
!
ZDMED (1,:) = (/  10., 10.,  5.,  5., 2.5, 2.5, 2.5,  1.,  1., 0.5, 0.5, 2.5/)
ZDMED (2,:) = (/ 100.,100.,100.,100., 75., 75., 50., 50., 10., 10., 10., 75./)
ZDMED (3,:) = (/1000.,690.,520.,520.,520.,210.,210.,125.,100.,100.,100.,520./)
!
DO ITEX = 1, SIZE(PZS0) 
  PZS0(ITEX) =1E-6*ZDMED(3,ITEX)/30.0
ENDDO
!
! Initialisation pour IDP = 1
PDP  (1) = 0.1          ! ?m
ZDPLN(1) = LOG(PDP(1))  ! ?m
! 
DO IDP = 2, SIZE(PDSRLV,2)
  ZDPLN(IDP) = ZDPLN(IDP-1) + PDLNDP
  PDP  (IDP) = EXP(ZDPLN(IDP))
END DO
!
! calcul pour chaque particule 
! Boucle NDP 
! Initialisation
! surface basale totale
ZSP(:) = 0.d0
! Calcul de la distribution massique des particules (dm(Dp)/dln(Dp)) pour chaque particule de diametre Dp
DO ITEX = 1, SIZE(PDSRLV,1)
  DO IDP = 1, SIZE(PDSRLV,2)
    ZDMLN(IDP) = 0.0d0
    DO IMOD = 1, KMODE
      ZDM1 = ZMFRAC(IMOD,ITEX) / (SQRT(2.d0*XPI) * LOG(ZSIGMA(IMOD,ITEX)))
      ZDM2 = EXP((LOG(PDP(IDP)) - LOG(ZDMED(IMOD,ITEX)))**2. / (-2.d0 * (LOG(ZSIGMA(IMOD,ITEX)))**2.d0))
      ZDMLN(IDP) = ZDMLN(IDP) + ZDM1*ZDM2
    END DO 
    ! Calcul de la surface basale occup?e par chaque particule de diam?tre Dp  
    ZDS(ITEX,IDP) = 3.d0 * ZDMLN(IDP) * PDLNDP / (2.* XDNS_SLT * PDP(IDP))
    ! Calcul de la surface basale totale Sp= (somme (ds(Dp)):
    ZSP(ITEX) = ZSP(ITEX) + ZDS(ITEX,IDP)
  ENDDO
ENDDO 
!
! Calcul de la distribution de surface relative des particules
DO ITEX = 1, SIZE(PZS0)
  DO IDP = 1, SIZE(PDSRLV,2)
    PDSRLV(ITEX,IDP) = ZDS(ITEX,IDP) / ZSP(ITEX)
    ENDDO 
END DO 
! 
IF (LHOOK) CALL DR_HOOK('MODE_DSTMBLUTL:DISTRIBUTION',1,ZHOOK_HANDLE)  
END SUBROUTINE DISTRIBUTION
!
END MODULE MODE_DSTMBLUTL
