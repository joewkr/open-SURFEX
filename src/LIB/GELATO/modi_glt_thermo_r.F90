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
! ======================= MODULE modi_glt_thermo_r ======================
! =======================================================================
!
!
! Goal:
! -----
!   Computation of the thermodynamic forcing over open water, sea ice
! and snow-covered sea ice. Version with thickness within each box
! which is constant, compensated with variable fractional ice
! covers.
!  
! Created : 1996/03 (D. Salas y Melia)
! Modified: 1996/09 (D. Salas y Melia)
!           Include lead scheme, snow scheme, heat diffusion scheme.
! Modified: 1997/12 (D. Salas y Melia)
!           Suppress all loops, split to thermo_init, thermo_ice,
!           thermo_lead, thermo_end routines 
! Modified: 1998/06 (D. Salas y Melia)
!           Introduction of optional arguments so that the routine
!           should be able to accept different sets of arguments,
!           depending on run options 
! Modified: 2001/07 (D. Salas y Melia) 
!           Suppress optional arguments as the possibility that the
!           model should compute its own fluxes is no longer offered. 
! Modified: 2001/09 (D. Salas y Melia)
!           Suppress the CALL to thermo_init routine. Move its two 
!           functionalities to thermo_ice and gelato.
! Modified: 2009/06 (D. Salas y Melia)
!           Reduced grid version 
! Modified: 2015/06 (D. Salas y Melia)
!           The constrain of sea ice with a climatology is now applied 
!           after the thermodynamics (and outside the present routine). 
!           If Gelato is activated in ARPEGE/Surfex, the atmospheric code 
!           will therefore "see" surface conditions closer to the used 
!           climatology.
!
! ------------------- BEGIN MODULE modi_glt_thermo_r ------------------------

!THXS_SFX!MODULE modi_glt_thermo_r
!THXS_SFX!INTERFACE
!THXS_SFX!
!THXS_SFX!SUBROUTINE glt_thermo_r  &
!THXS_SFX!  ( tpdom,pustar,tpmxl,tpatm,  &
!THXS_SFX!    tpblkw,tpblki,tpbud,tpdia,tptfl,tpsit,tpsil,tpsit_d )
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  USE modd_glt_param
!THXS_SFX!  TYPE(t_dom), DIMENSION(np), INTENT(in) ::  &
!THXS_SFX!        tpdom
!THXS_SFX!  REAL, DIMENSION(np), INTENT(in) ::  &
!THXS_SFX!        pustar
!THXS_SFX!  TYPE(t_mxl), DIMENSION(np), INTENT(inout) ::  &
!THXS_SFX!        tpmxl
!THXS_SFX!  TYPE(t_atm), DIMENSION(np), INTENT(in) ::  &
!THXS_SFX!        tpatm
!THXS_SFX!  TYPE(t_blk), DIMENSION(np), INTENT(inout) ::  &
!THXS_SFX!        tpblkw
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
!THXS_SFX!  TYPE(t_sit), DIMENSION(ntd,np), OPTIONAL, INTENT(in) ::  &
!THXS_SFX!        tpsit_d
!THXS_SFX!END SUBROUTINE glt_thermo_r
!THXS_SFX!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_glt_thermo_r

! --------------------- END MODULE modi_glt_thermo_r ------------------------



! -----------------------------------------------------------------------
! ---------------------- SUBROUTINE glt_thermo_r ----------------------------
!
SUBROUTINE glt_thermo_r  &
  ( tpdom,pustar,tpmxl,tpatm,  &
    tpblkw,tpblki,tpbud,tpdia,tptfl,tpsit,tpsil )
!
!
! 1. DECLARATIONS
! ===============
!
! 1.1. Module declarations
! ------------------------
!
  USE modd_glt_const_thm
  USE modd_types_glt
  USE modd_glt_param
  USE mode_glt_info_r 
  USE modi_glt_updbud_r
  USE mode_glt_stats_r
  USE mode_gltools_enthalpy
  USE modi_glt_constrain_r
  USE modi_glt_thermo_ice_r
  USE modi_glt_thermo_lead_r
  USE modi_glt_thermo_end_r
  USE modi_glt_updice_r
!
  IMPLICIT none
!
!
! 1.2. Dummy arguments declarations
! ---------------------------------
!
  TYPE(t_dom), DIMENSION(np), INTENT(in) ::  &
        tpdom
  REAL, DIMENSION(np), INTENT(in) ::  &
        pustar
  TYPE(t_mxl), DIMENSION(np), INTENT(inout) ::  &
        tpmxl
  TYPE(t_atm), DIMENSION(np), INTENT(in) ::  &
        tpatm
  TYPE(t_blk), DIMENSION(np), INTENT(inout) ::  &
        tpblkw
  TYPE(t_blk), DIMENSION(nt,np), INTENT(in) ::  &
        tpblki
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
  TYPE(t_sit), DIMENSION(nt,np) ::  &
        tzldsit
  TYPE(t_vtp), DIMENSION(nl,nt,np) ::  &
        tzldsil
  REAL :: &
        zice_a, zemps_a, zsalt_a, zsalf_a, zsalt_a_0, zsalf_a_0
!
!
! 1.4. Welcome message
! --------------------
!
  IF (lp1) THEN
    WRITE(noutlu,*) ' '
    WRITE(noutlu,*) ' *** LEVEL 3 - SUBROUTINE THERMO_R'
    WRITE(noutlu,*) ' '
  ENDIF
!
! 
! 1.5. Initialize arrays
! ----------------------
!
! .. Types 
!
  tzldsit(:,:)%esi = .FALSE.
  tzldsit(:,:)%asn = albw
  tzldsit(:,:)%fsi = 0.
  tzldsit(:,:)%hsi = 0.
  tzldsit(:,:)%hsn = 0.
  tzldsit(:,:)%rsn = rhosnwmax
  tzldsit(:,:)%tsf = SPREAD(tpmxl(:)%tml,1,nt)
  tzldsit(:,:)%age = 0.
  tzldsit(:,:)%ssi = SPREAD(tpmxl(:)%sml,1,nt)
  tzldsit(:,:)%vmp = 0.
  tzldsil(:,:,:)%ent = 0.
! 
! .. To know how much sea ice was thermodynamically created, record 
! initial average sea ice thickness field 
!
  tpdia(:)%dsi = rhoice*SUM( tpsit(:,:)%fsi*tpsit(:,:)%hsi, DIM=1 )
!
! .. Idem for sea ice fresh water content
!
  tpdia(:)%dwi = rhoice*SUM( tpsit(:,:)%fsi*tpsit(:,:)%hsi*  &
    ( 1.-1.e-3*tpsit(:,:)%ssi ), DIM=1 )
!
! .. Idem for snow mass variations
!
  tpdia(:)%dsn = SUM( tpsit(:,:)%fsi*tpsit(:,:)%rsn*tpsit(:,:)%hsn, DIM=1 )
!
! .. Idem for salt mass variations
!
  tpdia(:)%dsa = SUM( tpsit(:,:)%fsi*tpsit(:,:)%hsi*tpsit(:,:)%ssi, DIM=1 )
!
!
!
! 2. LEADS THERMODYNAMICS
! =======================
!
! * Do the thermodynamics for the lead covered fraction of the grid
! cell.
!   A lead is now physically defined in depth as the depth reached by
! its associated sea ice element on the grid cell. Underneath lies the
! mixed layer, whose velocity is different from that of the ensemble
! ice/leads, making lead further downward spreading virtually
! impossible.
!
  IF ( nupdbud==1 ) THEN
      CALL glt_updbud_r( 1,'Before THERMO_LEAD_R:',  &
        tpdom,tpmxl,tptfl,tpatm,tpblkw,tpblki,tpsit,tpsil,tpbud )
      CALL glt_updice_r(0,' BEFORE glt_thermo_lead_r ', &
        tpdom, tpsit, zsalt_a, zice_a, tptfl, zemps_a, zsalf_a)
      zsalt_a_0 = zsalt_a
      zsalf_a_0 = zsalf_a
  ENDIF
!
  CALL glt_thermo_lead_r  &
    (tpdom,pustar,tpmxl,tpatm,tpblkw,  &
    tpdia,tptfl,tpsit,tpsil,  &
    tzldsit,tzldsil)
!
!
!
! 3. SEA ICE THERMODYNAMICS
! =========================
!
! * Do the thermodynamics for the ice covered fraction of the grid cell
! (considering several ice types_glt, defined according to their
! thicknesses).
!   The main involved processes in this part are :
!       - the impact of precipitations on sea ice / snow build up,
!       - effect of ocean and atmospheric heat fluxes on sea ice
!       thickness changes (heat conduction in the ice / snow slab).
!   N.B. : Heat fluxes are considered at the top and at the bottom of
! the ice (W / m^2). Positive q-fluxes denote melting.
!
!
! 3.1. Check
! ----------
!
! .. Check sea ice model thermodynamics energy balance : compute the
! total enthalpy, latent heat and stored heat of sea ice.
!
  IF ( nupdbud==1 ) THEN
      CALL glt_updbud_r( 0,'After glt_thermo_lead_r / Before THERMO_ICE_R:',  &
        tpdom,tpmxl,tptfl,tpatm,tpblkw,tpblki,tpsit,tpsil,tpbud )
      CALL glt_updice_r(1, ' AFTER THERMO_LEAD_R', &
        tpdom, tpsit, zsalt_a, zice_a, tptfl, zemps_a, zsalf_a)
  ENDIF
!
!
! 3.2. Run sea ice and snow thermodynamics
! ----------------------------------------
!
  CALL glt_thermo_ice_r  &
    ( tpdom,tpmxl,tpatm,tpblki,tpbud,tpdia,tptfl,tpsit,tpsil )
!
! 
!
! 4. FINAL THERMODYNAMIC COMPUTATIONS
! ===================================
!
! * Previous calls to other thermodynamic subroutines may have led to
! disruptions in ice types_glt classification, for several reasons :
!       - an ice type has grown in thickness, moving to the next class.
!       - an ice type has disappeared totally because of lateral or
!         (and) vertical melting.
!       - sea ice may has grown on an open water surface.
!
! * The goal of thermo_end subroutine is to assess which ice type(s) are
! now in the different classes. If necessary, merge them, as well as
! associated leads.
!
!
! 4.1. Check in 
! -------------
!
  IF ( nupdbud==1 ) THEN
      CALL glt_updbud_r( 0,'After glt_thermo_ice_r / Before THERMO_END_R:',  &
        tpdom,tpmxl,tptfl,tpatm,tpblkw,tpblki,tpsit,tpsil,tpbud )
      CALL glt_updice_r(1, ' AFTER glt_thermo_ice_r ', &
        tpdom, tpsit, zsalt_a, zice_a, tptfl, zemps_a, zsalf_a)
  ENDIF
!
!
! 4.2. Final operations
! ---------------------
!
  CALL glt_thermo_end_r( tpdom,tpmxl,tzldsit,tzldsil,tpsit,tpsil )
!
! 
! 4.3. Compute some diagnostics
! ------------------------------
!
! This is done even if updbud flag is off, to allow the computation
! of certain diagnostics if wished by the user. 
!
  CALL glt_updbud_r( 0,'After THERMO_END_R:',  &
    tpdom,tpmxl,tptfl,tpatm,tpblkw,tpblki,tpsit,tpsil,tpbud )
!
! Compute change in stored latent heat and enthalpy in sea ice/snow
! due to thermodynamic processes
!
  tpdia(:)%the = ( tpbud(:)%enn - tpbud(:)%eni ) / dtt
!
! Compute net sea ice production field
!
  tpdia(:)%dsi =  &
    ( rhoice * SUM( tpsit(:,:)%fsi*tpsit(:,:)%hsi, DIM=1 )-  &
    tpdia(:)%dsi ) / dtt
!
! Compute net sea ice fresh water content change field
!
  tpdia(:)%dwi =  &
    ( rhoice * SUM( tpsit(:,:)%fsi*tpsit(:,:)%hsi* &
      ( 1.-1.e-3*tpsit(:,:)%ssi ), DIM=1 )-  &
      tpdia(:)%dwi ) / dtt
!
! Compute net snow mass change field
!
  tpdia(:)%dsn =  &
    ( SUM( tpsit(:,:)%rsn*tpsit(:,:)%fsi*tpsit(:,:)%hsn, DIM=1 )-  &
    tpdia(:)%dsn ) / dtt
!
! Compute net salt mass change field
!
  tpdia(:)%dsa = rhoice*1.e-3*  &
    ( SUM( tpsit(:,:)%fsi*tpsit(:,:)%hsi*tpsit(:,:)%ssi, DIM=1 )-  &
    tpdia(:)%dsa ) / dtt
!
! Compute sea ice and snow heat content
!
  CALL glt_aventh( tpsit,tpsil,tpdia%sie,tpdia%sne )
!
!
!
! 6. FAREWELL MESSAGE
! ====================
!
  IF (lp1) THEN
    WRITE(noutlu,*) ' '
    WRITE(noutlu,*) ' *** LEVEL 3 - END SUBROUTINE THERMO_R'
    WRITE(noutlu,*) ' '
  ENDIF
  IF ( nupdbud==1 ) THEN
     CALL glt_updice_r(1, '   SALT BUDGET OVER ENTIRE glt_thermo_r ', &
        tpdom, tpsit, zsalt_a_0, zice_a, tptfl, zemps_a, zsalf_a_0)
  ENDIF
!
END SUBROUTINE glt_thermo_r

! --------------------- END SUBROUTINE glt_thermo_r ---------------------
! -----------------------------------------------------------------------
