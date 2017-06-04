!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE DUSTFLUX_GET(            &
         PUSTAR,                    &
         PRHOA,                     & 
         PWG,                       & 
         PZ0,                       &
         PWSAT,                     &
         PCLAY,                     &
         PSAND,                     &
         PWIND10M,                  &
         PSFDST,                    &
         KSIZE                      &
         )  
!
!PURPOSE: 
!Take input from ISBA land surface model and
!calculate a dust flux which is consistent with the input.

!THEORY:
!Based on Marticorena/Bergametti, 1995 and Zender et al 2003 (JGR)

!CODE HISTORY
!Code is a modified version of dstmbl.F90 in the DEAD model
!Original version was downloaded from the DEAD homepage
!http://dust.ess.uci.edu/dead/ on January 10th 2005

!AUTHOR (or rather "code modifyer")
!Alf Grini <alf.grini@cnrm.meteo.fr>
!
USE MODD_DST_SURF, ONLY :  XFLX_MSS_FDG_FCT
USE MODD_DSTMBL,   ONLY : XRGH_MMN_SMT, XCST_SLT, XDMT_SLT_OPT
USE MODE_DSTMBLUTL               !Dust mobilization subroutines
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!INPUT, set their dimensions to their passed lengths or to KSIZE ?
INTEGER, INTENT(IN)                  :: KSIZE    ![nbr] length of passed arrays
REAL, INTENT(IN), DIMENSION(KSIZE)   :: PUSTAR   ![m/s] wind friction speed
REAL, INTENT(IN), DIMENSION(KSIZE)   :: PRHOA    ![kg/m3] air density at 2m height
REAL, INTENT(IN), DIMENSION(KSIZE)   :: PCLAY    ![frc] mass fraction clay
REAL, INTENT(IN), DIMENSION(KSIZE)   :: PSAND    ![frc] mass fraction sand
REAL, INTENT(IN), DIMENSION(KSIZE)   :: PWG      ![m3 m-3] volumetric water fraction
REAL, INTENT(IN), DIMENSION(KSIZE)   :: PWSAT    ![m3 m-3] saturation water content
REAL, INTENT(IN), DIMENSION(KSIZE)   :: PZ0      ![m] surface roughness length
REAL, INTENT(IN), DIMENSION(KSIZE)   :: PWIND10M ![m/s] wind at 10m altitude
!OUTPUT the flux of dust
REAL, INTENT(OUT), DIMENSION(KSIZE)  :: PSFDST   ! [kg m-2 s-1] Output flux of atmospheric dust

!!!!!!!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&!!!!!!

!#ifdef AlG01
!REAL,PARAMETER::XFLX_MSS_FDG_FCT=28. ! [frc] Global mass flux tuning factor (a posteriori)
!#else
!REAL,PARAMETER::XFLX_MSS_FDG_FCT=7.0e-4 ! [frc] Global mass flux tuning factor (a posteriori)
!REAL,PARAMETER::XFLX_MSS_FDG_FCT=21.0e-4 ! [frc] Global mass flux tuning factor (a posteriori)
    !real,PARAMETER::XFLX_MSS_FDG_FCT=12.0e-4 ! [frc] values used in Masdev47
!REAL,PARAMETER::flx_mss_fdg_fctm=4.0e-4 ! [frc] Global mass flux tuning factor (a posteriori) (proposez by Pierre)
!
!Define local variables:
LOGICAL, DIMENSION(KSIZE) :: GFLG_MBL                  ! [frc] Mobilization candidate flag
REAL,    DIMENSION(KSIZE) :: ZMBL_BSN_FCT              ! [frc] enhancement factor for grid cells with higher erodibility
!REAL,    DIMENSION(KSIZE) :: ZWND_RFR                  ! [m s-1] wind speed at reference level 
REAL,    DIMENSION(KSIZE) :: ZWND_FRC_THR_SLT          ! [m/s] Threshold wind friction speed when all effects taken into account
REAL,    DIMENSION(KSIZE) :: ZGWC_SFC                  ! [kg/kg] Gravimetric water content
REAL,    DIMENSION(KSIZE) :: ZGWC_THR                  ! [kg kg-1] Threshold gravimetric water content
REAL,    DIMENSION(KSIZE) :: ZFRC_THR_NCR_WTR          ! [frc] Fraction by which soil wetness increases threshold wind
REAL,    DIMENSION(KSIZE) :: ZFRC_THR_NCR_DRG          ! [frc] fraction by which drag partitioning increases threshold wind
REAL,    DIMENSION(KSIZE) :: ZWND_FRC_SLT              ! [m/s] wind friction speed after modified for saltation feedbacks
!REAL,    DIMENSION(KSIZE) :: ZWND_RFR_THR_SLT          ! [m s-1] Threshold wind speed at reference level
REAL,    DIMENSION(KSIZE) :: ZCOEF
REAL,    DIMENSION(KSIZE) :: ZFLX_MSS_HRZ_SLT_TTL_WBN  ! [kg m-1 s-1] Vertically integrated horizontal saltation soil flux for a wind bin 
REAL,    DIMENSION(KSIZE) :: ZFLX_MSS_VRT_DST_TTL_WBN  ! [kg m-2 s-1]
REAL,    DIMENSION(KSIZE) :: ZDST_SLT_FLX_RAT_TTL      ! [m-1] ratio of vertical to horizontal flux (alpha in several papers)
!
real  :: ZCLAY(KSIZE)
INTEGER         :: I                    !Counter for number of points (used in loops)
REAL(KIND=JPRB) :: ZHOOK_HANDLE
    
!Allocate the local variables
IF (LHOOK) CALL DR_HOOK('DUSTFLUX_GET',0,ZHOOK_HANDLE)
!
ZCLAY(:)=0.2
!Initialize mobilization candidate flag
GFLG_MBL(:) = .TRUE.
!fxm: Get erodibility limitation factor, use something connected to amount of sand
!Discuss with Valery Masson
ZMBL_BSN_FCT(:) = PSAND(:)
! utilisé dans le calcul de l'effet Owen 
!ZWND_RFR(:) = PWIND10M(:)   
!
!Initialize vertical dust flux
ZFLX_MSS_VRT_DST_TTL_WBN(:) = 0.d0
!
! Old Alf Grini code: bug on DEAD detected ? 
! Modification proposed by M. Mokhtari.. Accepted for all cases
!  if (CVERMOD=='CMDVER') then
ZGWC_THR(:) = MIN(0.14,MAX(0.02,3. * PCLAY(:) * (0.17 + 0.14 * PCLAY(:))))
!ZGWC_THR(:) = PCLAY(:) * (0.17d0 + 0.14d0 * PCLAY(:))
!  else 
! gwc_thr=mss_frc_cly*(0.17d0+0.14d0*mss_frc_cly) ! [m3 m-3] FMB99 p. 155 (14)
! fxm: 19991105 remove factor of mss_frc_cly from gwc_thr to improve large scale behavior
! Begin Old Alf code 
!   gwc_thr(lon_idx)=0.17d0+0.14d0*mss_frc_cly(lon_idx) ! [m3 m-3] 
!  endif
!
! Factor by which surface roughness increases threshold friction velocity 
!++grini: fxm: USE WHOLE ARRAY OF Z0 INSTEAD OF ONLY RGH_MMN_MBL AS IN OLD CODE
DO I = 1,SIZE(PZ0)
  CALL FRC_THR_NCR_DRG_GET(PZ0(I), XRGH_MMN_SMT, ZFRC_THR_NCR_DRG(I))
ENDDO
!
! Convert volumetric water content to gravimetric water content
 CALL VWC2GWC(GFLG_MBL, PWSAT, PWG, ZGWC_SFC)
! Factor by which soil moisture increases threshold friction velocity
 CALL FRC_THR_NCR_WTR_GET (GFLG_MBL, ZGWC_THR, ZGWC_SFC, ZFRC_THR_NCR_WTR)
!
! fxm: Use surface density not midlayer density
 CALL WND_FRC_THR_SLT_GET(PRHOA, XDMT_SLT_OPT, ZWND_FRC_THR_SLT)
!
DO I = 1, KSIZE
  ZWND_FRC_THR_SLT(I) =   & ! [m s-1] Threshold friction velocity for saltation
    ZWND_FRC_THR_SLT(I) * & ! [m s-1] Threshold for dry, flat ground
    ZFRC_THR_NCR_WTR(I)     ! [frc] Adjustment for moisture
ENDDO
!
! Threshold saltation wind speed
!Needed for the saltation feedback roughening effect
!do i=1,KSIZE
!   if (flg_mbl(i)) then
!      wnd_rfr_thr_slt(i)= & ! [m s-1] Threshold 10 m wind speed for saltation
!           wnd_rfr(i)*wnd_frc_thr_slt(i)/PUSTAR(i) !++alfgr 
!   endif                  ! endif flg_mbl
!end do                    ! end loop over lon

!CHECK IF THIS CAN BE USED EASILY
!NEEDS 10M WIND SPEED WHICH IS MAYBE KNOWN MAYBE NOT !
! Saltation increases friction speed by roughening surface
!call wnd_frc_slt_get( &
!    flg_mbl, & ! I [flg] Mobilization candidate flag
!     PUSTAR, & ! I [m s-1] Surface friction velocity
!     wnd_frc_slt, & ! O [m s-1] Saltating friction velocity
!     wnd_rfr, & ! I [m s-1] Wind speed at reference height
!     wnd_rfr_thr_slt) ! I [m s-1] Threshold 10 m wind speed for saltation
!
!
!Skip the roughening of surface effect for now, and 
!just use the wind friction speed as it is modified
!by drag partitioning
ZWND_FRC_SLT(:) = PUSTAR(:) / ZFRC_THR_NCR_DRG(:) 
! 
! Horizontal streamwise mass flux for old "bulk" formulation
ZCOEF(:) = XCST_SLT
 CALL FLX_MSS_HRZ_SLT_TTL_WHI79_GET(ZCOEF, GFLG_MBL, PRHOA, ZWND_FRC_SLT, &
        ZWND_FRC_THR_SLT, ZFLX_MSS_HRZ_SLT_TTL_WBN)
!
! Apply land surface and vegetation limitations and global tuning factor
DO I = 1, KSIZE
  ZFLX_MSS_HRZ_SLT_TTL_WBN(I) = ZFLX_MSS_HRZ_SLT_TTL_WBN(I) & ! [kg m-2 s-1]
            !*lnd_frc_mbl(i) & ! [frc] Bare ground fraction
                                * ZMBL_BSN_FCT(I)           & ! [frc] Erodibility factor
                                * XFLX_MSS_FDG_FCT            ! [frc] Global mass flux tuning factor (empirical) 
ENDDO
!    
! Vertical dust mass flux
 CALL FLX_MSS_VRT_DST_TTL_MAB95_GET(GFLG_MBL, ZCLAY, ZFLX_MSS_HRZ_SLT_TTL_WBN, & 
        ZDST_SLT_FLX_RAT_TTL, ZFLX_MSS_VRT_DST_TTL_WBN)
!
!Assign the output vertical dust flux to the value calculated
!PSFDST(:) = flx_mss_vrt_dst_ttl_wbn(:)
PSFDST(:) = ZDST_SLT_FLX_RAT_TTL(:) * ZFLX_MSS_HRZ_SLT_TTL_WBN(:)
!
IF (LHOOK) CALL DR_HOOK('DUSTFLUX_GET',1,ZHOOK_HANDLE)
!
END SUBROUTINE DUSTFLUX_GET
