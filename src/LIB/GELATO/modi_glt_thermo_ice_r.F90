!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!GLT_LIC The GELATO model is a seaice model used in stand-alone or embedded mode. 
!GLT_LIC  It has been developed by Meteo-France. The holder of GELATO is Meteo-France.
!GLT_LIC  
!GLT_LIC  This software is governed by the CeCILL-C license under French law and biding
!GLT_LIC  by the rules of distribution of free software. See the CeCILL-C_V1-en.txt
!GLT_LIC  (English) and CeCILL-C_V1-fr.txt (French) for details. The CeCILL is a free
!GLT_LIC  software license, explicitly compatible with the GNU GPL
!GLT_LIC  (see http://www.gnu.org/licenses/license-list.en.html#CeCILL)
!GLT_LIC  
!GLT_LIC  The CeCILL-C licence agreement grants users the right to modify and re-use the
!GLT_LIC  software governed by this free software license. The exercising of this right
!GLT_LIC  is conditional upon the obligation to make available to the community the
!GLT_LIC  modifications made to the source code of the software so as to contribute to
!GLT_LIC  its evolution.
!GLT_LIC  
!GLT_LIC  In consideration of access to the source code and the rights to copy, modify
!GLT_LIC  and redistribute granted by the license, users are provided only with a limited
!GLT_LIC  warranty and the software's author, the holder of the economic rights, and the
!GLT_LIC  successive licensors only have limited liability. In this respect, the risks
!GLT_LIC  associated with loading, using, modifying and/or developing or reproducing the
!GLT_LIC  software by the user are brought to the user's attention, given its Free
!GLT_LIC  Software status, which may make it complicated to use, with the result that its
!GLT_LIC  use is reserved for developers and experienced professionals having in-depth
!GLT_LIC  computer knowledge. Users are therefore encouraged to load and test the
!GLT_LIC  suitability of the software as regards their requirements in conditions enabling
!GLT_LIC  the security of their systems and/or data to be ensured and, more generally, to
!GLT_LIC  use and operate it in the same conditions of security. 
!GLT_LIC  
!GLT_LIC  The GELATO sofware is cureently distibuted with the SURFEX software, available at 
!GLT_LIC  http://www.cnrm.meteo.fr/surfex. The fact that you download the software deemed that
!GLT_LIC  you had knowledge of the CeCILL-C license and that you accept its terms.
!GLT_LIC  Attempts to use this software in a way not complying with CeCILL-C license
!GLT_LIC  may lead to prosecution. 
!GLT_LIC 
! =======================================================================
! ====================== MODULE modi_glt_thermo_ice_r =======================
! =======================================================================
!
! Goal:
! -----
!   Do the thermodynamics for the ice covered fraction of the grid cell
! (considering several ice types_glt, defined according to their 
! thicknesses).
!
! Method:
! -------
!   The main involved processes in this part are :
!       - the impact of precipitations on sea ice / snow build up,
!       - effect of ocean and atmospheric heat fluxes on sea ice 
!       thickness changes (heat conduction in the ice / snow slab). 
!   N.B. : Heat fluxes are considered at the top and at the bottom of 
! the ice (W / m^2). Positive q-fluxes denote melting. 
!
! Created : 1996/04 (D. Salas y Melia)
!           Also includes bulk fluxes (based on Simonsen, 1996), leads
!           physics. 
!           The thermodynamics is only computed at one point
! Modified: 1997/12 (D. Salas y Melia) 
!           Regional model: Arctic or Antarctic. Specific for computing
!           sea ice thermodynamics.
! Modified: 2001/08 (D. Salas y Melia)  
!           Increased modularity. Most processes are now described in
!           a separate routine.
! Modified: 2009/06 (D. Salas y Melia)
!           reduced grid
! Modified: 2010/02 (D. Salas y Melia)
!           interactiver salinity 
! Modified: 2011/12 (A. Voldoire)
!           improved computation of snow and ice mass balance
! Modified: 2012/01 (M. Chevallier)
!           invoke glt_updasn_r (i.e. upgrade the state of melt ponds)
!           after the last potential water input to the ponds (after 
!           surface melting processes)
!
! -------------------- BEGIN MODULE modi_glt_thermo_ice_r -------------------
!
!THXS_SFX!MODULE modi_glt_thermo_ice_r
!THXS_SFX!INTERFACE
!THXS_SFX!!
!THXS_SFX!SUBROUTINE glt_thermo_ice_r  &
!THXS_SFX!  ( tpdom,tpmxl,tpatm,tpblki,tpbud,tpdia,tptfl,tpsit,tpsil )
!THXS_SFX!!
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  USE modd_glt_param
!THXS_SFX!!
!THXS_SFX!  TYPE(t_dom), DIMENSION(np), INTENT(in) ::  &
!THXS_SFX!        tpdom
!THXS_SFX!  TYPE(t_mxl), DIMENSION(np), INTENT(in) ::  &
!THXS_SFX!        tpmxl
!THXS_SFX!  TYPE(t_atm), DIMENSION(np), INTENT(in) ::  &
!THXS_SFX!        tpatm
!THXS_SFX!  TYPE(t_blk), DIMENSION(nt,np), INTENT(in) ::  &
!THXS_SFX!        tpblki
!THXS_SFX!  TYPE(t_bud), DIMENSION(np), INTENT(inout) ::  &
!THXS_SFX!        tpbud
!THXS_SFX!  TYPE(t_dia), DIMENSION(np), INTENT(inout) ::  &
!THXS_SFX!        tpdia
!THXS_SFX!  TYPE(t_tfl), DIMENSION(np), INTENT(inout) ::  &
!THXS_SFX!        tptfl
!THXS_SFX!  TYPE(t_sit), DIMENSION(nt,np), INTENT(inout) ::  &
!THXS_SFX!        tpsit
!THXS_SFX!  TYPE(t_vtp), DIMENSION(nl,nt,np), INTENT(inout) ::  &
!THXS_SFX!        tpsil
!THXS_SFX!!
!THXS_SFX!END SUBROUTINE glt_thermo_ice_r
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_glt_thermo_ice_r
!
! -------------------- END MODULE modi_glt_thermo_ice_r ---------------------
!
!
!
! -----------------------------------------------------------------------
! --------------------- SUBROUTINE glt_thermo_ice_r -------------------------
!
SUBROUTINE glt_thermo_ice_r  &
  ( tpdom,tpmxl,tpatm,tpblki,tpbud,tpdia,tptfl,tpsit,tpsil )
!
!
!
! 1. DECLARATIONS AND INITIALIZATIONS
! ====================================
!
! 1.1. Module declarations
! ------------------------
!
  USE modd_types_glt
  USE modd_glt_param
  USE modd_glt_const_thm
  USE modi_glt_vhdiff_r
!  USE modi_glt_swabs_r
  USE mode_glt_info_r
  USE mode_glt_stats_r
  USE modi_glt_updasn_r 
  USE modi_glt_icetrans_r
  USE modi_glt_sublim_r
  USE modi_glt_precip_r
  USE modi_glt_snowice_r
  USE modi_glt_updhsn_r
  USE modi_glt_updhsi_r
  USE modi_glt_lmltsi_r
  USE modi_glt_updbud_r
  USE modi_glt_updice_r
  USE modi_glt_updsnow_r
  USE modi_glt_icevsp_r
  USE modi_gltools_chkglo_r
  USE mode_gltools_enthalpy
  USE modi_glt_updsal_r
!
  IMPLICIT NONE
!
!
! 1.2. Dummy arguments declarations
! ---------------------------------
!
! --- INTENT(in) arguments.
!
  TYPE(t_dom), DIMENSION(np), INTENT(in) ::  &
        tpdom
  TYPE(t_mxl), DIMENSION(np), INTENT(in) ::  &
        tpmxl
  TYPE(t_atm), DIMENSION(np), INTENT(in) ::  &
        tpatm
  TYPE(t_blk), DIMENSION(nt,np), INTENT(in) ::  &
        tpblki
!
! --- INTENT(inout) arguments.

  TYPE(t_bud), DIMENSION(np), INTENT(inout) ::  &
        tpbud
  TYPE(t_dia), DIMENSION(np), INTENT(inout) ::  &
        tpdia
  TYPE(t_tfl), DIMENSION(np), INTENT(inout) ::  &
        tptfl
  TYPE(t_sit), DIMENSION(nt,np), INTENT(inout) ::  &
        tpsit
  TYPE(t_vtp), DIMENSION(nl,nt,np), INTENT(inout) ::  &
        tpsil 
!
!
! 1.3. Local variables declarations
! --------------------------------- 
!
  LOGICAL, DIMENSION(np) ::  &
        grain,gsnow
  LOGICAL, DIMENSION(nt,np) ::  &
        osmelt
  INTEGER ::  &
        jl
  REAL ::  &
        zwork0,zicondt,zicondb,zidhi,zidhs,zinrg,zsnow_a,zemp_a,&
        zice_a,zemps_a,zsalt_a,zsalf_a
  real,dimension(np) :: zei1,zes1,zei2,zes2
  REAL, DIMENSION(np) ::  &
        zwork2,zemps
  REAL, DIMENSION(nt,np) ::  &
        zcondb,zqtopmelt,znsftop,zdcondt,zqmelt,zmlf3
  REAL, DIMENSION(nl,nt,np) ::  &
        zswtra,zdhmelt,zvsp,zent
  TYPE(t_blk), DIMENSION(np) ::  &
        tzdum
!
!
! 1.4. Welcome message
! --------------------
!
  IF (lp1) THEN
    WRITE(noutlu,*) ' '
    WRITE(noutlu,*) '**** LEVEL 4 - SUBROUTINE THERMO_ICE_R'
    WRITE(noutlu,*) ' '
  ENDIF
!
!
! 1.5. Local variables initializations
! ------------------------------------
!
! .. Two-dimensional logical arrays 
!
  grain(:) = ( tpatm(:)%lip>epsil1 )
  gsnow(:) = ( tpatm(:)%sop>epsil1 )
!
! .. Three-dimensional real arrays
!
  zcondb(:,:) = 0.
  znsftop(:,:) = 0.
  zdcondt(:,:) = 0.
  zqmelt(:,:) = 0.
  zmlf3(:,:) = 0.
!
! .. Vertical salinity profile
!
  CALL glt_icevsp_r( tpsit,zvsp )
!
! .. Type variables
!
  tzdum(:)%swa = 0.
  tzdum(:)%nsf = 0.
  tzdum(:)%dfl = 0.
  tzdum(:)%eva = 0.
!
  zemps(:) = 0.
!
!
! 1.6. Check in
! -------------
!
  CALL gltools_chkglo_r( 'Before THERMO_ICE_R',tpdom,tpsit)
!
  zemps(:) = tptfl(:)%cio
!
  IF ( nupdbud==1 ) THEN
    CALL glt_updsnow_r(0, '   Snow   ', tpdom, tptfl, tpsit, zsnow_a, zemp_a)
    CALL glt_updice_r(0, ' BEGINNING THERMO_ICE ', tpdom, tpsit, zsalt_a, zice_a, tptfl, zemps_a, zsalf_a)
  ENDIF
!
!
!
! 2. Snow surface processes
! ==========================
!
! 2.1. Effect of sublimation of snow/sea ice
! ------------------------------------------
!
  IF ( nicesub==1 ) THEN
      CALL glt_sublim_r( tpmxl,tpblki,tpsit,tpsil,tptfl,tpdia )
  ENDIF
!
  tpdia(:)%subcio = tptfl(:)%cio - zemps(:)
  zemps(:) = tptfl(:)%cio
!
  IF ( nupdbud==1 ) THEN
      CALL glt_updbud_r( 0,'After glt_sublim_r / Before PRECIP_R:',  &
        tpdom,tpmxl,tptfl,tpatm,tzdum,tpblki,tpsit,tpsil,tpbud )
      CALL glt_updsnow_r(1, '   Snow   ', tpdom, tptfl, tpsit, zsnow_a, zemp_a, &
                 -1*tpdia(:)%sus, -1*(tpdia(:)%suw+tpdia(:)%sui))
      CALL glt_updice_r(1, ' AFTER glt_sublim_r ', &
        tpdom, tpsit, zsalt_a, zice_a, tptfl, zemps_a, zsalf_a)
  ENDIF
!
!
! 2.2. Effect of precipitations
! -----------------------------
!
!   Here we consider the effects of precipitations on the marine surface
! layer on their physics. Lead evaporation will be taken into account
! later on (THERMO_LEAD subroutine).
!   Note that this routine generates a trend on surface temperature,
! that should be taken into account like for other processes later on.
!
  CALL glt_precip_r( grain,gsnow,tpmxl,tpatm,tpsit,tpsil,tptfl,tpdia,zqmelt )
!
  IF ( nupdbud==1 ) THEN
      CALL glt_updbud_r( 0,'After glt_precip_r / Before ICETRANS_R:',  &
        tpdom,tpmxl,tptfl,tpatm,tzdum,tpblki,tpsit,tpsil,tpbud )
      CALL glt_updsnow_r(1, '   Snow   ', tpdom, tptfl, tpsit, zsnow_a, zemp_a, &
                 -1*(tpdia(:)%s_pr+tpdia(:)%s_prsn), -1*(tpdia(:)%o_pr+tpdia(:)%o_prsn))
      CALL glt_updice_r(1, ' AFTER glt_precip_r ', tpdom, tpsit, zsalt_a, zice_a, tptfl, zemps_a, zsalf_a)
  ENDIF
!
!
! 2.3. Short wave absorption and transmission
! --------------------------------------------
!
! .. Split the absorbed solar flux into three parts:
!       - One part is retained by the ice surface, 
!       - another is stored by the ice (thermal effect of brine
! pockets), 
!       - the rest crosses the ice slab to reach the mixed layer.
!
  CALL glt_icetrans_r( tpblki,tpmxl,tptfl,tpsit,tpdia,zswtra )
!
!
!
! 3. Update sea ice heat of fusion, temperature and heat reservoirs
! =================================================================
!
! 3.1. Treatment for very thin ice or no ice
! ------------------------------------------
!
! .. Expand 2D tpmxl(:,:)%mlf array to a 3D array.
!
  zmlf3(:,:) = SPREAD( tpmxl(:)%mlf,1,nt )
!
! .. Update gltools_enthalpy profile and surface temperature 
!
! Surface temperature
  WHERE ( tpsit(:,:)%hsi<epsil1 )
    tpsit(:,:)%tsf = zmlf3(:,:) 
  ENDWHERE  
!
! Ice gltools_enthalpy
  DO jl = 1,nilay
    WHERE ( tpsit(:,:)%hsi<epsil1 )
      tpsil(jl,:,:)%ent = -cpsw*mu*zvsp(jl,:,:)
    ENDWHERE
  END DO
!
! Snow gltools_enthalpy
  WHERE( tpsit(:,:)%hsi<epsil1 )
    tpsil(nilay+1,:,:)%ent = -xmhofusn0
  ENDWHERE
!
! .. Checks
!
  IF ( nupdbud==1 ) THEN
      CALL glt_updbud_r( 0,'After glt_icetrans_r / Before VHDIFF_R:',  &
        tpdom,tpmxl,tptfl,tpatm,tzdum,tpblki,tpsit,tpsil,tpbud )
      CALL glt_updsnow_r(1, '   Snow   ', tpdom, tptfl, tpsit, zsnow_a, zemp_a)
      CALL glt_updice_r(1, ' AFTER glt_icetrans_r ', tpdom, tpsit, zsalt_a, zice_a, tptfl, zemps_a, zsalf_a)
  ENDIF
!
!
! 3.2. Treatment of vertical temperature profiles
! ------------------------------------------------
!
! 3.2.1. Update sea ice stored heat and top conduction heat flux
! ...............................................................
!
! .. Initialize and then compute top of the ice/snow slab conduction 
! flux.
!
  znsftop(:,:) = tpblki(:,:)%nsf + zqmelt(:,:)
!      
! .. Derivative of znsftop
!
  zdcondt(:,:) = tpblki(:,:)%dfl
!     
!
! 3.2.2. Run 1D heat diffusion scheme with flux bc at the top
! ............................................................
!
!   This scheme is run with the boundary conditions that have just been
! computed. It updates the vertical temperature profile in the slab,
! and returns the bottom conductive heat flux (ice/ocean interface). 
!
  zwork2 = dtt*SUM( tpsit(:,:)%fsi*znsftop, DIM=1 ) 
!
  zent(:,:,:) = tpsil(:,:,:)%ent
  CALL glt_aventh(tpsit,tpsil,zei1,zes1)
  CALL glt_vhdiff_r  &
    ( tpdom,tpmxl%mlf,zdcondt,tpsit,tpdia,  &
    znsftop,zswtra,zent,zvsp,zcondb,zqtopmelt,zdhmelt,osmelt )
!
    tpsil(:,:,:)%ent = zent(:,:,:)
  CALL glt_aventh(tpsit,tpsil,zei2,zes2)
!
! .. Checks
!
  IF ( nupdbud==1 ) THEN
      CALL glt_updbud_r( 0,'After glt_vhdiff_r / Before UPDHSN_R:',  &
        tpdom,tpmxl,tptfl,tpatm,tzdum,tpblki,tpsit,tpsil,tpbud )
      CALL glt_updsnow_r(1, '   Snow   ', tpdom, tptfl, tpsit, zsnow_a, zemp_a)
      CALL glt_updice_r(1, ' AFTER glt_vhdiff_r ', tpdom, tpsit, zsalt_a, zice_a, tptfl, zemps_a, zsalf_a)
  ENDIF
!
!
!
! 4. Update snow albedo and thickness  
! ===================================
!
! 4.1. Update snow cover thickness
! --------------------------------
!
! .. Update snow thickness. If there is not enough snow to be melted,
! pass the residual melt flux over to the ocean.
!
  CALL glt_updhsn_r( osmelt,zdhmelt,tpmxl,tptfl,tpsit,tpsil,tpdia )
!
  IF ( nupdbud==1 ) THEN
      CALL glt_updbud_r( 0,'After glt_updhsn_r / Before SNOWICE_R:',  &
        tpdom,tpmxl,tptfl,tpatm,tzdum,tpblki,tpsit,tpsil,tpbud )
      CALL glt_updsnow_r(1, '   Snow   ', tpdom, tptfl, tpsit, zsnow_a, zemp_a)
      CALL glt_updice_r(1, ' AFTER glt_updhsn_r ', tpdom, tpsit, zsalt_a, zice_a, tptfl, zemps_a, zsalf_a)
  ENDIF
!
!
! 4.2. Snow ice formation
! ------------------------
!
! .. See the routine itself for more details. 
!
  CALL glt_snowice_r( tpmxl,tpsil,tptfl,tpsit,tpdia )
!
  tpdia(:)%snicio = tptfl(:)%cio - zemps(:)
  zemps(:) = tptfl(:)%cio
!
  IF ( nupdbud==1 ) THEN
      CALL glt_updbud_r( 0,'After glt_snowice_r / Before UPDHSI_R:',  &
        tpdom,tpmxl,tptfl,tpatm,tzdum,tpblki,tpsit,tpsil,tpbud )
      CALL glt_updsnow_r(1, '   Snow   ', tpdom, tptfl, tpsit, zsnow_a, zemp_a)
      CALL glt_updice_r(1, ' AFTER SNOWICE_R', tpdom, tpsit, zsalt_a, zice_a, tptfl, zemps_a, zsalf_a)
  ENDIF
!
!
!
! 5. Sea ice thickness and concentration changes 
! ==============================================
!
! 5.1. Update sea ice thickness
! -----------------------------
!
!
! .. From the contributions of :
!       - conduction bottom heat flux   :       -zcondb
!       - mixed layer heat flux         :       tpmxl%qoc
!       - residual ocean flux           :       tpmxl%qml,
! compute first guess for new sea ice thickness. 
!
  CALL glt_updhsi_r( zcondb,zqtopmelt,zdhmelt,tpmxl,tpdia,tptfl,tpsit,tpsil )
!
  tpdia(:)%hsicio = tptfl(:)%cio - zemps(:)
  zemps(:) = tptfl(:)%cio
!
! .. Checks
!
  IF ( nupdbud==1 ) THEN
      CALL glt_updbud_r( 0,'After glt_updhsi_r / Before UPDASN_R:',  &
        tpdom,tpmxl,tptfl,tpatm,tzdum,tpblki,tpsit,tpsil,tpbud )
      CALL glt_updsnow_r(1, '   Snow   ', tpdom, tptfl, tpsit, zsnow_a, zemp_a)
      CALL glt_updice_r(1, ' AFTER glt_updhsi_r ', &
        tpdom, tpsit, zsalt_a, zice_a, tptfl, zemps_a, zsalf_a)
  ENDIF
!
!
! 4.2. Snow cover albedo change and melting
! ------------------------------------------
!
! .. Update snow cover/bare ice albedo
!
  CALL glt_updasn_r( osmelt,tpatm,tpblki,zvsp,tpsit,tpdia )
!
! .. Checks
!
  IF ( nupdbud==1 ) THEN
      CALL glt_updbud_r( 0,'After glt_updasn_r / before LMLTSI_R:',  &
        tpdom,tpmxl,tptfl,tpatm,tzdum,tpblki,tpsit,tpsil,tpbud )
      CALL glt_updsnow_r(1, '   Snow   ', tpdom, tptfl, tpsit, zsnow_a, zemp_a)
      CALL glt_updice_r(1, ' After glt_updasn_r ', &
        tpdom, tpsit, zsalt_a, zice_a, tptfl, zemps_a, zsalf_a)
  ENDIF
!
!
! 5.2. Lateral melting of sea ice
! -------------------------------
!
! .. As the mixed layer warms up, sea ice ablation not only takes place
! at the underside of the floe, but also laterally (see lmltsi routine
! for more details).
!
  CALL glt_lmltsi_r( tpmxl,tpsil,tpsit,tpdia,tptfl )
!
  tpdia(:)%lmlcio = tptfl(:)%cio - zemps(:)
  zemps(:) = tptfl(:)%cio
!
  IF ( nupdbud==1 ) THEN
      CALL glt_updbud_r( 0,'After glt_lmltsi_r / Before UPDSAL_R:',  &
        tpdom,tpmxl,tptfl,tpatm,tzdum,tpblki,tpsit,tpsil,tpbud )
      CALL glt_updsnow_r(1, '   Snow   ', tpdom, tptfl, tpsit, zsnow_a, zemp_a)
      CALL glt_updice_r(1, ' After glt_lmltsi_r ', &
        tpdom, tpsit, zsalt_a, zice_a, tptfl, zemps_a, zsalf_a)
  ENDIF
!
!
!
! 6. Update salinity
! ===================
!
! .. Desalination processes are taken into account here
!
  CALL glt_updsal_r( osmelt,tpmxl,tpsit,tptfl )
!
  tpdia(:)%salcio = tptfl(:)%cio - zemps(:)
  zemps(:) = tptfl(:)%cio
!
!
! 7. Final operations
! ===================
!
! 7.1. Prints (global quantities)
! -------------------------------
!
! .. General budget checks
!
  IF ( nupdbud==1 ) THEN
      CALL glt_updbud_r( 0,'After glt_updsal_r = End of THERMO_ICE_R:',  &
        tpdom,tpmxl,tptfl,tpatm,tzdum,tpblki,tpsit,tpsil,tpbud )
      CALL glt_updsnow_r(1, '   Snow   ', tpdom, tptfl, tpsit, zsnow_a, zemp_a)
      CALL glt_updice_r(1, ' AFTER glt_updsal_r ', &
        tpdom, tpsit, zsalt_a, zice_a, tptfl, zemps_a, zsalf_a)
  ENDIF
!
! .. Sea ice extent and volume checks
!
  CALL gltools_chkglo_r( 'After THERMO_ICE_R',tpdom,tpsit )
!
!
! 7.2. Farewell message
! ---------------------
!
  IF (lp1) THEN
    WRITE(noutlu,*) ' '
    WRITE(noutlu,*) '**** LEVEL 4 - END SUBROUTINE THERMO_ICE_R'
    WRITE(noutlu,*) ' '
  ENDIF
!
END SUBROUTINE glt_thermo_ice_r
!
! --------------------- END SUBROUTINE glt_thermo_ice_r ---------------------
! -----------------------------------------------------------------------
