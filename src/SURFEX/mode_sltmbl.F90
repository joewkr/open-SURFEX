!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
MODULE MODE_SLTMBL

  !PURPOSE: 
  !Take input from ISBA land surface model and
  !calculate a salt flux which is consistent with the input.

  !THEORY:
  !Based on Marticorena/Bergametti, 1995 and Zender et al 2003 (JGR)

  !CODE HISTORY
  !Code is a modified version of dstmbl.F90 in the DEAD model
  !Original version was downloaded from the DEAD homepage
  !http://salt.ess.uci.edu/dead/ on January 10th 2005

  !AUTHOR (or rather "code modifyer")
  !Alf Grini <alf.grini@cnrm.meteo.fr>

!
  USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
  USE PARKIND1  ,ONLY : JPRB
!
  implicit none
  public

contains

  subroutine saltflux_get(          &
         PUSTAR,                     &!I [m/s] Wind friction speed 
         PRHOA,                     &!I [kg/m3] air density at 2m height 
         PWG,                       &!I [m3/m3] volumetric water content 
         PZ0,                       &!I [m] roughness length of surface
         PWSAT,                     &!I [m3 m-3] saturation liquid water content
         PCLAY,                     &!I [frc] mass fraction clay
         PSAND,                     &!I [frc] mass fraction sand
         PWIND10M,                  &!I [m/s] wind at 10m altitude
         PSFSLT,                    &!O [kg/m2/sec] Vertical salt flux
         KSIZE                     &!I [nbr] number of points for calculation
         )  


    implicit none
    
    !INPUT, set their dimensions to their passed lengths or to KSIZE ?
    integer, intent(in)                  :: KSIZE    ![nbr] length of passed arrays
    real, intent(in), dimension(KSIZE)   :: PUSTAR   ![m/s] wind friction speed
    real, intent(in), dimension(KSIZE)   :: PRHOA    ![kg/m3] air density
    real, intent(in), dimension(KSIZE)   :: PCLAY    ![frc] mass fraction clay
    real, intent(in), dimension(KSIZE)   :: PSAND    ![frc] mass fraction sand
    real, intent(in), dimension(KSIZE)   :: PWG      ![m3 m-3] volumetric water fraction
    real, intent(in), dimension(KSIZE)   :: PWSAT    ![m3 m-3] saturation water content
    real, intent(in), dimension(KSIZE)   :: PZ0      ![m] surface roughness length
    real, intent(in), dimension(KSIZE)   :: PWIND10M ![m/s] wind at 10m altitude

    !OUTPUT the flux of salt
    real, intent(out), dimension(KSIZE)  :: PSFSLT   ! [kg m-2 s-1] Output flux of atmospheric salt

!!!!!!!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&!!!!!!

!#ifdef AlG01
!    real,parameter::flx_mss_fdg_fct=28. ! [frc] Global mass flux tuning factor (a posteriori)
!#else
!    real,parameter::flx_mss_fdg_fct=7.0e-4 ! [frc] Global mass flux tuning factor (a posteriori)
!    real,parameter::flx_mss_fdg_fct=21.0e-4 ! [frc] Global mass flux tuning factor (a posteriori)
    real,parameter::flx_mss_fdg_fct=18.0e-4 ! [frc] Global mass flux tuning factor (a posteriori)
!#endif
    real,parameter::hgt_rfr=10.0 ! [m] Reference height for mobilization processes
    real,parameter::hgt_zpd_mbl=0.0 ! [m] Zero plane displacement for erodible surfaces
    real,parameter::rgh_mmn_mbl=100.0e-6 ! [m] Roughness length momentum for erodible surfaces MaB95 p. 16420, GMB98 p. 6205
    ! fxm: rgh_mmn_smt set to 33.3e-6 um, MaB95 p. 16426 recommend 10.0e-6
    real,parameter::rgh_mmn_smt=33.3e-6 ! [m] Smooth roughness length MaB95 p. 16426, MaB97 p. 4392, GMB98 p. 6207
    real,parameter::wnd_min_mbl=1.0 ! [m s-1] Minimum windspeed used for mobilization 
    real,parameter::wnd_frc_rsl=0.95d0 ! [frc] Fraction of wind PDF to resolve 

    !Define local variables:
    logical,allocatable :: flg_mbl(:)          ![frc] Mobilization candidate flag
    real, allocatable   :: frc_thr_ncr_drg(:)  ![frc] fraction by which drag partitioning increases threshold wind
    real, allocatable   :: frc_thr_ncr_wtr(:)  ![frc] Fraction by which soil wetness increases threshold wind
    real, allocatable   :: gwc_sfc(:)          ![kg/kg] Gravimetric water content
    real, allocatable   :: wnd_frc_thr_slt(:)  ![m/s] Threshold wind friction speed when all effects taken into account
    real, allocatable   :: wnd_frc_slt(:)         ![m/s] wind friction speed after modified for saltation feedbacks
    real, allocatable   :: flx_mss_hrz_slt_ttl_wbn(:) ![kg m-1 s-1] Vertically integrated horizontal saltation soil flux for a wind bin 
    real, allocatable   :: flx_mss_vrt_dst_ttl_wbn(:)     ![kg m-2 s-1]
    real, allocatable   :: wnd_rfr_thr_slt(:)             ![m s-1] Threshold wind speed at reference level
    real, allocatable   :: mbl_bsn_fct(:)                 ![frc] enhancement factor for grid cells with higher erodibility
    real, allocatable   :: dst_slt_flx_rat_ttl(:)         ![m-1] ratio of vertical to horizontal flux (alpha in several papers)
    real, allocatable   :: ZCLAY(:)                       ![frc] dummy for fraction of clay

    integer             :: i                   !Counter for number of points (used in loops)
    REAL(KIND=JPRB) :: ZHOOK_HANDLE
    
    !Allocate the local variables
    IF (LHOOK) CALL DR_HOOK('MODE_SLTMBL:SALTFLUX_GET',0,ZHOOK_HANDLE)
    allocate (flg_mbl(KSIZE))
    allocate (frc_thr_ncr_drg(KSIZE))
    allocate (frc_thr_ncr_wtr(KSIZE))
    allocate (gwc_sfc(KSIZE))
    allocate (wnd_frc_thr_slt(KSIZE))
    allocate (wnd_frc_slt(KSIZE))
    allocate (flx_mss_hrz_slt_ttl_wbn(KSIZE))
    allocate (flx_mss_vrt_dst_ttl_wbn(KSIZE))
    allocate (wnd_rfr_thr_slt(KSIZE))
    allocate (mbl_bsn_fct(KSIZE))
    allocate (dst_slt_flx_rat_ttl(KSIZE))
    allocate (ZCLAY(KSIZE))

    !Initialize mobilization candidate flag
    flg_mbl(:)=.TRUE.
    
    !Initialize vertical salt flux
    flx_mss_vrt_dst_ttl_wbn(:)=0.d0

    !fxm: Get erodibility limitation factor, use something connected to amount of sand
    !Discuss with Valery Masson
    mbl_bsn_fct(:)=PSAND(:)
 
    ! Factor by which surface roughness increases threshold friction velocity 
    !++grini: fxm: USE WHOLE ARRAY OF Z0 INSTEAD OF ONLY RGH_MMN_MBL AS IN OLD CODE


    !Free memory for allocated local variables
    deallocate (flg_mbl)
    deallocate (frc_thr_ncr_drg)
    deallocate (frc_thr_ncr_wtr)
    deallocate (gwc_sfc)
    deallocate (wnd_frc_thr_slt)
    deallocate (wnd_frc_slt)
    deallocate (flx_mss_hrz_slt_ttl_wbn)
    deallocate (flx_mss_vrt_dst_ttl_wbn)
    deallocate (wnd_rfr_thr_slt)
    deallocate (mbl_bsn_fct)
    deallocate (dst_slt_flx_rat_ttl)
    deallocate (ZCLAY)
  IF (LHOOK) CALL DR_HOOK('MODE_SLTMBL:SALTFLUX_GET',1,ZHOOK_HANDLE)

  end subroutine saltflux_get

END MODULE MODE_SLTMBL
