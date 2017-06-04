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
! ========================= MODULE mode_glt_dia_ar5 =========================
! =======================================================================
!
! 
! * Contains a subroutine that writes model glt_output in Gelato format
!
! --------------------- BEGIN MODULE mode_glt_dia_ar5 -----------------------

MODULE mode_glt_dia_ar5
CONTAINS 
!
!
! -----------------------------------------------------------------------
! ------------------------ SUBROUTINE WRIDIA_AR5 ------------------------

! * A subroutine that computes interesting quantities from certain
! icestate variables (statistics) and records them in data files at 
! every time step.

SUBROUTINE wridia_ar5( tpglt )
!
  USE modd_types_glt
  USE modd_glt_param
  USE modd_glt_const_thm
  USE modi_gltools_avevai
  USE mode_gltools_wrivais
  USE modi_gltools_outdia
#if ! defined in_surfex
  USE modi_gltools_dynami
#endif
#if ! defined in_arpege
  USE lib_mpp
#endif
  USE mode_glt_stats
  USE MODI_GLTOOLS_GLTERR
  IMPLICIT none
!
  TYPE(t_glt), INTENT(inout) ::  &
        tpglt
!
  CHARACTER(8) ::  &
        yword
  CHARACTER(80) ::  &
        yfname,ymess,yfdef,ylnam
  INTEGER ::  &
        ji,jt,ii,ij,ii0,ij0
  LOGICAL, DIMENSION(nx,ny) ::  &
        ynhemis,yshemis
  REAL ::  &
        zai,zaj,zcslat,zdilat,zdilon,zdjlat,zdjlon,zw
  REAL, DIMENSION(1,1) ::  &
        zehn,zehs,zshn,zshs,zvhn,zvhs,zwhn,zwhs,  &
        zfram,zbering,zncwest,znceast,znorthb
  REAL, DIMENSION(nx,ny) ::  &
        zfsit,zhsit,zhsnt,zmsnt
  REAL, DIMENSION(nx,ny) ::  &
        zwork2,zsrf
  REAL, DIMENSION(nt,nx,ny) ::  &
        zwork3
  TYPE(t_def) ::  &
        tznam
  TYPE(t_ind) ::  &
        tzind
  TYPE(t_dom), DIMENSION(nx,ny) ::  &
        tzdom
  TYPE(t_mxl), DIMENSION(nx,ny) ::  &
        tzml
  TYPE(t_tfl), DIMENSION(nx,ny) ::  &
        tztfl 
  TYPE(t_blk), DIMENSION(nx,ny) ::  &
        tzblkw
  TYPE(t_blk), DIMENSION(nt,nx,ny) ::  &
        tzblki
  TYPE(t_sit), DIMENSION(nt,nx,ny) ::  &
        tzsit
  TYPE(t_bud), DIMENSION(nx,ny) ::  &
        tzbud
  TYPE(t_dia), DIMENSION(nx,ny) ::  &
        tzdia
  REAL, DIMENSION(ndiamax,1,1) ::  &
        zcumdia0
  REAL, DIMENSION(ndiamax,nx,ny) ::  &
        zcumdia
!
!
!
! 1. Initializations
! ===================
!
! .. Arrays
!
  zwork2(:,:) = 0.
  zwork3(:,:,:) = 0.
  zcumdia0 = tpglt%cdia0
  zcumdia = tpglt%cdia
! 
! .. Structures
!
  tzind = tpglt%ind
  tzdom = tpglt%dom
  tzml = tpglt%tml
  tztfl = tpglt%tfl
  tzblkw = tpglt%blkw
  tzblki = tpglt%blki
  tzsit = tpglt%sit
  tzbud = tpglt%bud
  tzdia = tpglt%dia 
!
! .. Welcome message
!
  IF(lp1) WRITE(noutlu,*) ' '
  IF(lp1) WRITE(noutlu,*) ' *** LEVEL 3 - SUBROUTINE WRIDIA_AR5'
  IF(lp1) WRITE(noutlu,*) ' '
  IF(lp1) WRITE(noutlu,*) '     --> Write diagnostic files'
!
! .. Compute total sea ice concentration with threshold, net total sea
! ice concentration, sea ice average thickness
!
  zfsit(:,:) = glt_iceconcm( tzdom,tzsit )
  zhsit(:,:) = glt_avhicem( tzdom,tzsit )
  zhsnt(:,:) = glt_avhsnwm( tzdom,tzsit )      
  zmsnt(:,:) = glt_avmsnwm( tzdom,tzsit )      
!
! .. Time counter
!
  tzind%nts = tzind%nts + 1
!
! .. Set field counters to zero (field index in cumulated diagnostics array)
! Has to be done before writing first 0d and 2d arrays
!
  tzind%i0d = 0
  tzind%i2d = 0
!
! .. For 'specialised averaging' (e.g. ice age, snow density or salinity), 
! count time steps when there is sea ice
!
  tzdia(:,:)%sic = tzdia(:,:)%sic + zfsit(:,:)
  tzdia(:,:)%sit = tzdia(:,:)%sit + zhsit(:,:)
  tzdia(:,:)%snd = tzdia(:,:)%snd + zhsnt(:,:)
!
!
!
! 2. Write priority 1 diagnostics
! ================================
!
  IF ( ndiap1==1 ) THEN 
!
! 2.1. OImon table requested diagnostics
! ---------------------------------------
!
! A few remarks:
!   - snow mass can be easily re-computed from: sim-rhoice*snd
! OK
      tznam = t_def(  &
        "Sea Ice Area Fraction",  &
        "sea_ice_area_fraction",  &
        "sic", "%", "T", "SCALAR" )
      zwork2(:,:) = 100.*zfsit(:,:)
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
! OK
      tznam = t_def(  &
        "Sea Ice Thickness",  &
        "sea_ice_thickness",  &
        "sit", "m", "T", "SCALAR" )
      zwork2(:,:) = zhsit(:,:)
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
! OK
      tznam = t_def(  &
        "Frozen Water Mass",  &
        "",  &
        "sim", "kg.m-2", "T", "SCALAR" )
      zwork2(:,:) = ( rhoice*zhsit(:,:) + zmsnt(:,:) )
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
! OK (note that sign is negative if sea ice loses water)
! Ce champ est en fait égal à sbl car Arpege ne considère que de la 
! sublimation sur neige / glace
! Noter l'ambiguité avec evapsbl dans les tables Amon
      zwork2(:,:) =  &
        SUM( tzsit(:,:,:)%fsi*tzblki(:,:,:)%eva, DIM=1 )
      tznam = t_def(  &
        "Water Evaporation Flux from Sea Ice",  &
        "water_evaporation_flux",  &
        "evap", "kg.m-2.s-1", "T", "SCALAR" )
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
! OK
      tznam = t_def(  &
        "Snow Depth",  &
        "surface_snow_thickness",  &
        "snd", "m", "T", "SCALAR" )
      zwork2(:,:) = zhsnt(:,:)
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
! OK
!* Note that here, we do not divide zwork2 by zhsnt, since tzdia%snd
! contains the sum of the zhsnt [weights] over all time outcomes
      tznam = t_def(  &
        "Snow Layer Density",  &
        "snow_layer_density",  &
        "rhosn", "kg.m-3", "T", "SCALAR" )
      zwork2(:,:) = zmsnt(:,:)
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia,  &
        pwgt=tzdia%snd )
! OK
!* Note that here, we do not divide zwork2 by zfsit, since tzdia%aiw
! contains the sum of the zfsit [weights] over all time outcomes
! (tzdia%aiw and tzdia%aiw are both computed in updasn_r)
      tznam = t_def(  &
        "Bare Sea Ice Albedo",  &
        "bare_sea_ice_albedo",  &
        "ialb", "1", "T", "SCALAR" )
      zwork2(:,:) = tzdia(:,:)%asi
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia,  &
        pwgt=tzdia%aiw )
! OK
!* Note that here, we do not divide zwork2 by zfsit, since tzdia%aiw
! contains the sum of the zfsit [weights] over all time outcomes
! (tzdia%aiw and tzdia%aiw are both computed in updasn_r)
      tznam = t_def(  &
        "Melt pond Albedo",  &
        "melt_pond_albedo",  &
        "mpalb", "1", "T", "SCALAR" )
      zwork2(:,:) = tzdia(:,:)%amp
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia,  &
        pwgt=tzdia%aiw )
! OK
!* Note that here, we do not divide zwork2 by zfsit, since tzdia%sic 
! contains the sum of the zfsit [weights] over all time outcomes
      tznam = t_def(  &
        "Surface Temperature of Sea Ice",  &
        "surface_temperature_of_sea_ice",  &
        "tsice", "K", "T", "SCALAR" )
      zwork2(:,:) = SUM( tzsit(:,:,:)%fsi*(tzsit(:,:,:)%tsf+t0deg),DIM=1 )
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia,  &
        pwgt=tzdia%sic )
! OK
!* Note that here, we do not divide zwork2 by zfsit, since tzdia%tiw 
! contains the sum of the zfsit [weights] over all time outcomes
! (tzdia%tiw and tzdia%tin are both computed in vhdiff_r)
      tznam = t_def(  &
        "Temperature at Interface Between Sea Ice and Snow",  &
        "temperature_at_interface_between_sea_ice_and_snow",  &
        "tsnint", "K", "T", "SCALAR" )
      zwork2(:,:) = tzdia(:,:)%tin
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia,  &
        pwgt=tzdia%tiw )
! OK
      tznam = t_def(  &
        "Surface Rainfall Rate into the Sea Ice Portion of the Grid Cell",  &
        "surface_rainfall_rate_into_the_sea_ice_portion_of_the_grid_cell",  &
        "pr", "kg.m-2.s-1", "T", "SCALAR" )
      zwork2(:,:) = tzdia(:,:)%lip
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
! OK
      tznam = t_def(  &
        "Surface Snowfall Rate into the Sea Ice Portion of the Grid Cell",  &
        "surface_snowfall_rate_into_the_sea_ice_portion_of_the_grid_cell",  & 
        "prsn", "kg.m-2.s-1", "T", "SCALAR" )
      zwork2(:,:) = tzdia(:,:)%sop
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
      tznam = t_def(  &
        "Frazil Sea Ice Growth (Leads) Rate",  &
        "frazil_sea_ice_growth_(leads)_rate",  & 
        "grFrazil", "kg.m-2.s-1", "T", "SCALAR" )
      zwork2(:,:) = tzdia(:,:)%lsi
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
      tznam = t_def(  &
        "Congelation Sea Ice Growth Rate",  &
        "congelation_sea_ice_growth_rate",  &
        "grCongel", "kg.m-2.s-1", "T", "SCALAR" )
      zwork2(:,:) = tzdia(:,:)%cgl
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
      tznam = t_def(  &
        "Lateral Sea Ice Growth Rate",  &
        "lateral_sea_ice_growth_rate",  &
        "grLateral", "kg.m-2.s-1", "T", "SCALAR" )
      zwork2(:,:) = tzdia(:,:)%mrl
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
      tznam = t_def(  &
        "Ice Formation Rate from snow-ice formation",  &
        "ice_formation_rate_from_Snow-Ice_Formation",  &
        "snoToIce", "kg.m-2.s-1", "T", "SCALAR" )
      zwork2(:,:) = tzdia(:,:)%sni
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
      tznam = t_def(  &
        "Snow Melt Rate",  &
        "snow_melt_rate",  &
        "snomelt", "kg.m-2.s-1", "T", "SCALAR" )
      zwork2(:,:) = tzdia(:,:)%snm
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
      tznam = t_def(  &
        "Snow Melt Rate due to lateral ice erosion",  &
        "snow_melt_rate_Due_to_Lateral_Ice_Erosion",  &
        "latsnomelt", "kg.m-2.s-1", "T", "SCALAR" )
      zwork2(:,:) = tzdia(:,:)%snml
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
      tznam = t_def(  &
        "Rate of Melt at Upper Surface of Sea Ice",  &
        "rate_of_melt_at_upper_surface_of_sea_ice",  & 
        "tmelt", "kg.m-2.s-1", "T", "SCALAR" )
      zwork2(:,:) = tzdia(:,:)%mrt
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
      tznam = t_def(  &
        "Rate of Melt at Sea Ice Base",  &
        "rate_of_melt_at_sea_ice_base",  &
        "bmelt", "kg.m-2.s-1", "T", "SCALAR" )
      zwork2(:,:) = tzdia(:,:)%mrb
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
      tznam = t_def(  &
        "Dilution flux due to ice sublimation",  &
        "Dilution_flux_due_to_Ice_Sublimation",  &
        "vsfsub", "kg.m-2.s-1", "T", "SCALAR" )
      zwork2(:,:) = tzdia(:,:)%subcio
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
      tznam = t_def(  &
        "Dilution flux due to lateral melting of ice",  &
        "Dilution_flux_due_to_Lateral_Melting_of_Ice",  &
        "vsflml", "kg.m-2.s-1", "T", "SCALAR" )
      zwork2(:,:) = tzdia(:,:)%lmlcio
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
      tznam = t_def(  &
        "Dilution flux due to salt exchanges",  &
        "Dilution_flux_due_to_Salt_Exchanges",  &
        "vsfsal", "kg.m-2.s-1", "T", "SCALAR" )
      zwork2(:,:) = tzdia(:,:)%salcio
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
      tznam = t_def(  &
        "Dilution flux due to change of ice depth",  &
        "Dilution_flux_due_to_change_of_ice_depth",  &
        "vsfhsi", "kg.m-2.s-1", "T", "SCALAR" )
      zwork2(:,:) = tzdia(:,:)%hsicio
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
      tznam = t_def(  &
        "Dilution flux due to conversion of snow to ice",  &
        "Dilution_flux_due_to_Conversion_of_Snow_to_Ice",  &
        "vsfsni", "kg.m-2.s-1", "T", "SCALAR" )
      zwork2(:,:) = tzdia(:,:)%snicio
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
!* Not provided (net is provided instead)
!      tznam = t_def(  &
!        "Downward Shortwave over Sea Ice",  &
!        "surface_downwelling_shortwave_flux_in_air",  &
!        "rsdssi", "W.m-2", "T", "SCALAR" )
!      zwork2(:,:) = tzdia(:,:)%swd
!      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
!* Not provided (net is provided instead) 
!      tznam = t_def(  &
!        "Upward Shortwave over Sea Ice",  &
!        "surface_upwelling_shortwave_flux_in_air",  &
!        "rsussi", "W.m-2", "T", "SCALAR" )
! OK
      tznam = t_def(  &
        "X-Component of Sea Ice Mass Transport",  &
        "",  &
        "transix", "kg.s-1", "U", "VECTOR" )
      zwork2(:,:) = tzdia(:,:)%xtr
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
! OK
      tznam = t_def(  &
        "Y-Component of Sea Ice Mass Transport",  &
        "",  &
        "transiy", "kg.s-1", "V", "VECTOR" )
      zwork2(:,:) = tzdia(:,:)%ytr
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
! transifs : Fram Strait glt_transport (à fournir ???)
! 
!
! 2.2. Omon table requested diagnostics 
! ---------------------------------------
!
! .. These diagnostics are not in the sea ice CMIP5 table, but in the 
! ocean table. However they can be computed in the sea ice model
! 
!OK
!* This is reduced to snow layer melt (true only if sea ice is levitating !)
      tznam = t_def(  &
        "Water Flux into Sea Water due to Sea Ice Thermodynamics",  &
        "water_flux_into_sea_water_due_to_sea_ice_thermodynamics",  &
        "fsitherm", "kg.m-2.s-1", "T", "SCALAR" )
      IF ( nleviti==1 ) THEN
          zwork2(:,:) = -tzdia(:,:)%snm
        ELSE      
          zwork2(:,:) = xbig20
      ENDIF
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
! OK
! PEUT-ETRE UN PETIT PB D'UNITES (ne faut-il pas remultiplier par
! la profondeur du niveau le plus superficiel ?) voir Griffies et al ?
      tznam = t_def(  &
        "Virtual Salt Flux into Sea Water due to Sea Ice Thermodynamics",  &
        "virtual_salt_flux_into_sea_water_due_to_sea_ice_thermodynamics",  &
        "vsfsit", "kg.m-2.s-1", "T", "SCALAR" )
      zwork2(:,:) = -1.e-3*tztfl(:,:)%cio*tzml(:,:)%sml
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
! OK
      tznam = t_def(  &
        "Downward Sea Ice Basal Salt Flux",  &
        "downward_sea_ice_basal_salt_flux",  &
        "sfdsi", "kg.m-2.s-1", "T", "SCALAR" )
      zwork2(:,:) = tztfl(:,:)%sio
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
!* Not provided
! This is zero in Gelato (no heat transfer to the mixed layer as sea ice
! forms in leads, see imod_thermo_lead_r.f90)
!      tznam = t_def(  &
!        "Heat Flux into Sea Water due to Frazil Ice Formation",  &
!        "heat_flux_into_sea_water_due_to_frazil_ice_formation",  &
!        "hfsifrazil", "kg.m-2.s-1", "T", "SCALAR" )
!
! OK
!* Ex-IOTFLUIW
      tznam = t_def(  &
        "Heat Flux into Sea Water due to Sea Ice Thermodynamics",  &
        "heat_flux_into_sea_water_due_to_sea_ice_thermodynamics",  &
        "hfsithermds", "kg.m-2.s-1", "T", "SCALAR" )
      zwork2(:,:) = tztfl(:,:)%tio
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
! 
!
! 2.3. Other diagnostics (not in CMIP5 tables) 
! ---------------------------------------------
!
!*Ex-AIHFLUIW
! Net heat flux on the ice surface only - weighted [W.m-2]
! (without the effect of snow)
      tznam = t_def(  &
        "Downward Heat Flux at Sea Ice Surface",  &
        "downward_heat_flux_at_sea_ice_surface",  &
        "hfdssi", "W.m-2", "T", "SCALAR" )
      zwork2(:,:) = tzbud(:,:)%hii-tzbud(:,:)%nii
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
!* Ex-AWHFLUWW
! Net heat flux on the water surface only - weighted [W.m-2]
! (without the effect of snow)
      tznam = t_def(  &
        "Downward Heat Flux at Leads Surface",  &
        "downward_heat_flux_at_leads_surface",  &
        "hfdso", "W.m-2", "T", "SCALAR" )
      zwork2(:,:) = (tzbud(:,:)%hli-tzbud(:,:)%nli)
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
! OK
      tznam = t_def(  &
        "Net Downward Shortwave Radiation at Sea Ice Surface",  &
        "net_downward_shortwave_radiation_at_sea_ice_surface",  &
        "rsntdssi", "W.m-2", "T", "SCALAR" )
      zwork2(:,:) = tzdia(:,:)%swi
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
!* Ex-AWSFLUWW
! Solar energy absorbed by the water surface [W.m-2]
      tznam = t_def(  &
        "Net Downward Shortwave Radiation at Leads Surface",  &
        "net_downward_shortwave_radiation_at_leads_surface",  &
        "rsntdso", "W.m-2", "T", "SCALAR" )
      zwork2(:,:) = tzdia(:,:)%sww
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
! OK
!* Average snow albedo field [0-1]
! Note that here, we do not divide zwork2 by zfsit, since tzdia%sic 
! contains the sum of the zfsit [weights] over all time outcomes
      tznam = t_def(  &
        "Weighted Albedo of Snow Over Sea Ice",  &
        "weighted_albedo_of_snow_over_sea_ice",  &
        "sisnalb", "1", "T", "SCALAR" )
      zwork2(:,:) = tzdia(:,:)%asn
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia,  &
        pwgt=tzdia%aiw )
! OK
!* Sea ice u-velocity
      tznam = t_def(  &
        "Eastward Sea Ice Velocity",  &
        "eastward_sea_ice_velocity",  &
        "usi", "m.s-1", "U", "VECTOR" ) 
      zwork2(:,:) = tzdia(:,:)%uvl
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
! OK
!* Sea ice v-velocity
      tznam = t_def(  &
        "Northward Sea Ice Velocity",  &
        "northward_sea_ice_velocity",  &
        "vsi", "m.s-1", "V", "VECTOR" ) 
      zwork2(:,:) = tzdia(:,:)%vvl
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
! OK
!* Fraction of time during which sea ice is present [0-1]
      tznam = t_def(  &
        "Fraction of Time with Sea Ice ",  &
        "fraction_of_time_with_sea_ice",  &
        "timesi", "%", "T", "SCALAR" ) 
      WHERE( zfsit(:,:)>xfsic )
          zwork2(:,:) = 100.
        ELSEWHERE
          zwork2(:,:) = 0.
      ENDWHERE
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
! OK
!* Sublimation over sea-ice
      tznam = t_def(  &
        "Sublimation over Sea Ice",  &
        "sublimation_over_sea_ice",  &
        "subIce", "kg.m-2.s-1", "T", "SCALAR" ) 
      zwork2(:,:) = tzdia(:,:)%sui
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!* Sublimation tot over sea-ice fraction of the grid
      tznam = t_def(  &
        "Sublimation",  &
        "sublimation",  &
        "subTot", "kg.m-2.s-1", "T", "SCALAR" ) 
      zwork2(:,:) = tzdia(:,:)%sut
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
! OK
!* Sublimation over snow
      tznam = t_def(  &
        "Sublimation over Snow",  &
        "sublimation_over_snow",  &
        "subSnw", "kg.m-2.s-1", "T", "SCALAR" ) 
      zwork2(:,:) = tzdia(:,:)%sus
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
! OK
!* Sublimation over ocean
      tznam = t_def(  &
        "Sublimation over Ocean (residual)",  &
        "sublimation_over_ocean",  &
        "subO", "kg.m-2.s-1", "T", "SCALAR" ) 
      zwork2(:,:) = tzdia(:,:)%suw
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
! OK
!* Sublimation over leads
      tznam = t_def(  &
        "Sublimation over Leads",  &
        "sublimation_over_leads",  &
        "subL", "kg.m-2.s-1", "T", "SCALAR" ) 
      zwork2(:,:) = tzdia(:,:)%sul
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
! OK
!* Liquid precip that actually falls on snow
      tznam = t_def(  &
        "Liquid Precipitation over Snow",  &
        "liquid_precipitation_over_snow",  &
        "s_pr", "kg.m-2.s-1", "T", "SCALAR" ) 
      zwork2(:,:) = tzdia(:,:)%s_pr
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
! OK
!* Liquid precip that actually falls on openwater
      tznam = t_def(  &
        "Liquid Precipitation over Ocean",  &
        "liquid_precipitation_over_ocean",  &
        "o_pr", "kg.m-2.s-1", "T", "SCALAR" ) 
      zwork2(:,:) = tzdia(:,:)%o_pr
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
! OK
!* Liquid precip that actually falls on leads
      tznam = t_def(  &
        "Liquid Precipitation over Leads",  &
        "liquid_precipitation_over_leads",  &
        "l_pr", "kg.m-2.s-1", "T", "SCALAR" ) 
      zwork2(:,:) = tzdia(:,:)%l_pr
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
! OK
!* Solid precip that actually falls on snow
      tznam = t_def(  &
        "Solid Precipitation over Snow",  &
        "solid_precipitation_over_snow",  &
        "s_prsn", "kg.m-2.s-1", "T", "SCALAR" ) 
      zwork2(:,:) = tzdia(:,:)%s_prsn
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
! OK
!* Solid precip that actually falls on open water
      tznam = t_def(  &
        "Solid Precipitation over ocean",  &
        "solid_precipitation_over_ocean",  &
        "o_prsn", "kg.m-2.s-1", "T", "SCALAR" ) 
      zwork2(:,:) = tzdia(:,:)%o_prsn
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
! OK
!* Solid precip that actually falls on leads
      tznam = t_def(  &
        "Solid Precipitation over Leads",  &
        "solid_precipitation_over_leads",  &
        "l_prsn", "kg.m-2.s-1", "T", "SCALAR" ) 
      zwork2(:,:) = tzdia(:,:)%l_prsn
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
!* Average surface albedo field (all marine surface) [0-1]
! Water albedo should not be assumed to be constant to albw (this may change)
!      tznam = t_def(  &
!        "Weighted Marine Surface Albedo",  &
!        "weighted_marine_surface_albedo",  &
!        "wsalb", "1", "T", "SCALAR" )
!      zwork2(:,:) =  &
!        ( SUM( tzsit(:,:,:)%fsi*tzsit(:,:,:)%asn,DIM=1 ) +  &
!        ( 1.-zfsit(:,:) )*albw )
!      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
  ENDIF
!
!
! 
! 3. Write second set of diagnostics
! ===================================
!
! .. Note that the outgoing heat flux affecting ice free areas are 
! exactly equal to the incoming heat flux in the same zones.
!
! NOTES 
! ------
!  * If you want to compute a complete energy balance on sea ice,
! you must compare, on the one hand:
!    . SITDENIW (gltools_enthalpy change)
! And, on the other hand:
!    . OIHFLUIW + AIHFLUIW + AISNWFIW + AWHFLUWW + AWSNWFWW - 
! ( IOLFLUIW + IOTFLUIW + LOLFLUIW + LOTFLUIW )
! i.e. 
! ( ocean heat flux + 
!   atmospheric heat flux on ice + energy flux due to snowfalls on ice + 
!   atmospheric heat flux on water + energy flux due to snowfalls on water )
! minus
! ( outgoing short wave + non-solar through leads +
!   outgoing short wave + non-solar through ice )
!  
!  - outgoing energy (solar+non-solar) at the bottom of sea ice)
!
!  * If you want to compute a complete fresh water balance on sea ice, 
! you must compare, on the one hand:
!    . ALLFWTOT (all precip-evapo) - LOWFLUIW - IOWFLUIW (outgoing water 
! through leads and under sea ice)
! And, on the other hand:
!    . SIDMSIIW (sea ice mass change) - SIDMSAIW (remove salt) + SIDMSNIW
! (snow mass change)
! 
!  * The energetic balance due to the (non perfectly conservative) sea 
! ice advection is also available, see SIDDENIW and SIDDLAIW fields.
!
!  * The change in water budget due to dynamics is not implemented yet.
!
  IF ( ndiap2==1 ) THEN 
!
!OK
      tznam = t_def(  &
        "Surface Snow Area Fraction",  &
        "surface_snow_area_fraction",  &
        "snc", "%", "T", "SCALAR" )
      zwork2(:,:) =  &
        100.*SUM( tzsit(:,:,:)%fsi,DIM=1,MASK=tzsit(:,:,:)%hsn>epsil1 )
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
!* Sea Ice total heat content includes snow heat content
! Note that the average sea ice temperature can be estimated from
! a function given in gltools_temper_r
      tznam = t_def(  &
        "Sea Ice Total Heat Content",  &
        "sea_ice_total_heat_content",  & 
        "hcice", "J.m-2", "T", "SCALAR" )
      zwork2(:,:) = tzdia(:,:)%sie + tzdia(:,:)%sne
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
!* Snow total heat content
      tznam = t_def(  &
        "Snow Total Heat Content",  &
        "snow_total_heat_content",  & 
        "hcsnow", "J.m-2", "T", "SCALAR" )
      zwork2(:,:) = tzdia(:,:)%sne
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
! PERHAPS AVERAGE SEA ICE TEMPERATURE SHOULD BE PROVIDED ??
!(divide %sie by fsit in glt_thermo, then apply a new temper function)
! 
! The 4 next fields cannot be provided. 
! The net non-solar flux on sea ice is provided instead.
!* Not provided
      tznam = t_def(  &
        "Downward Longwave over Sea Ice",  &
        "surface_downwelling_longwave_flux_in_air",  &
        "rldssi", "W.m-2", "T", "SCALAR" )
! 
!* Not provided
      tznam = t_def(  &
        "Upward longwave over Sea Ice",  &
        "surface_upwelling_longwave_flux_in_air",  &
        "rlussi", "W.m-2", "T", "SCALAR" )
! 
!* Not provided
      tznam = t_def(  &
        "Surface Upward Sensible Heat Flux over Sea Ice",  &
        "surface_upward_sensible_heat_flux",  &
        "hfssi","W.m-2", "T", "SCALAR" )
! 
!* Not provided
      tznam = t_def(  &
        "Surface Upward Latent Heat Flux over Sea Ice",  &
        "surface_upward_latent_heat_flux",  &
        "hflssi", "W.m-2", "T", "SCALAR" )
!
!* Not provided (= evap field in case of a coupling with Arpege)
      tznam = t_def(  &
        "Sublimation over Sea Ice",  &
        "surface_snow_and_ice_sublimation_flux",  &
        "sblsi", "kg.m-2.s-1", "T", "SCALAR" )
! OK
! Note that here, we do not divide zwork2 by zfsit, since tzdia%sic 
! contains the sum of the zfsit [weights] over all time outcomes
      tznam = t_def(  &
        "X-Component of Atmospheric Stress On Sea Ice",  &
        "",  &
        "strairx", "N.m-2", "U", "VECTOR" )
      zwork2(:,:) = tzdia(:,:)%atx
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia,  &
        pwgt=tzdia%sic )
! OK
! Note that here, we do not divide zwork2 by zfsit, since tzdia%sic 
! contains the sum of the zfsit [weights] over all time outcomes
      tznam = t_def(  &
        "Y-Component of Atmospheric Stress On Sea Ice",  &
        "",  &
        "strairy", "N.m-2", "V", "VECTOR" )
      zwork2(:,:) = tzdia(:,:)%aty
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia,  &
        pwgt=tzdia%sic )
! OK
! Note that here, we do not divide zwork2 by zfsit, since tzdia%sic 
! contains the sum of the zfsit [weights] over all time outcomes
      tznam = t_def(  &
        "X-Component of Ocean Stress On Sea Ice",  &
        "",  &
        "strocnx", "N.m-2", "U", "VECTOR" )
      zwork2(:,:) = tzdia(:,:)%otx
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia,  &
        pwgt=tzdia%sic )
! OK
! Note that here, we do not divide zwork2 by zfsit, since tzdia%sic 
! contains the sum of the zfsit [weights] over all time outcomes
      tznam = t_def(  &
        "Y-Component of Ocean Stress On Sea Ice",  &
        "",  &
        "strocny", "N.m-2", "V", "VECTOR" )
      zwork2(:,:) = tzdia(:,:)%oty
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia,  &
        pwgt=tzdia%sic )
!
!* Not provided 
! When computing the time-mean here, the time samples, weighted
! by the area of sea ice, are accumulated and then divided by 
! the sum of the weights. Report as missing in regions free of
! sea ice.
      tznam = t_def(  &
        "Compressive Sea Ice Strength",  &
        "",  &
        "streng", "N.m-2", "T", "SCALAR" )
!
!* Not provided 
      tznam = t_def(  &
        "Strain Rate Divergence of Sea Ice",  &
        "",  &
        "divice", "s-1", "T", "SCALAR" )
! 
!* Not provided 
      tznam = t_def(  &
        "Strain Rate Shear of Sea Ice",  &
        "",  &
        "shrice", "s-1", "T", "SCALAR" )
! 
! When computing the time-mean here, the time samples, weighted
! by the area of sea ice, are accumulated and then divided by 
! the sum of the weights. Report as missing in regions free of
! sea ice (pas obligatoire...)
      tznam = t_def(  &
        "Sea Ice Ridging Rate",  &
        "",  &
        "ridgice", "s-1", "T", "SCALAR" )
! 
!* Ex-OIHFLUIW
! Ocean heat flux - weighted [W.m-2]
      tznam = t_def(  &
        "Ocean Heat Flux to Sea Ice",  &
        "ocean_heat_flux_to_sea_ice",  &
        "hfoi", "W.m-2", "T", "SCALAR" )
      zwork2(:,:) = tzdia(:,:)%qoi
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
!* Ex-AWSNWFWW
! Equivalent heat flux due to snow melting in the ocean [W.m-2]
      tznam = t_def(  &
        "Surface Heat Flux due to Snowfalls over Leads",  &
        "surface_snowfalls_heat_flux_over_leads",  &
        "sfhfso", "W.m-2", "T", "SCALAR" )
      zwork2(:,:) = tzbud(:,:)%nli
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
!*Ex-AISNWFIW
! Equivalent heat flux due to snowfalls on sea ice [W.m-2]
      tznam = t_def(  &
        "Surface Heat Flux due to Snowfalls over Sea Ice",  &
        "surface_snowfalls_heat_flux_over_sea_ice",  &
        "sfhfssi", "W.m-2", "T", "SCALAR" )
      zwork2(:,:) = tzbud(:,:)%nii
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
!* Ex-SITDENIW
! Rate of change of sea ice heat content due to thermodynamics [W.m-2]
! Sea Ice total heat content includes snow heat content
      tznam = t_def(  &
        "Rate of Change of Sea Ice Total Heat Content due to Thermodynamics",  &
        "rate of change of sea_ice_total_heat_content due to thermodynamics",  &
        "dhcicet", "W.m-2", "T", "SCALAR" )
      zwork2(:,:) = tzdia(:,:)%the
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
!* Ex-SIDDENIW
! Weighted sea ice gltools_enthalpy variation due to advection [W.m-2]
! Sea Ice total heat content includes snow heat content
      tznam = t_def(  &
        "Rate of Change of Sea Ice Total Heat Content due to Advection",  &
        "rate of change of sea_ice_total_heat_content due to advection",  &
        "dhcicea", "W.m-2", "T", "SCALAR" )
      zwork2(:,:) =  &
        ( tzbud(:,:)%enn-tzbud(:,:)%eni ) / dtt - tzdia(:,:)%the
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
! Weighted sea ice gltools_enthalpy variation due to damping [W.m-2]
      tznam = t_def(  &
        "Rate of Change of Sea Ice Total Heat Content due to Damping",  &
        "rate of change of sea_ice_total_heat_content due to damping",  &
        "dhciced", "W.m-2", "T", "SCALAR" )
      zwork2(:,:) = tzdia(:,:)%dmp
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
! The following two water fluxes are the REAL fluxes of water computed 
! by Gelato - a difference compared to fsitherm, which is the water 
! flux actually seen by the ocean code, since sea ice is levitating.
!
!* Ex-IOWFLUIW
! Write weighted net FW flux sent by sea ice to the ocean [W.m-2]
      tznam = t_def(  &
        "Real Water Flux into Sea Water under Sea Ice",  &
        "real_water_flux_into_sea_water_under_sea_ice",  &
        "wfsitsi", "kg.m-2.s-1", "T", "SCALAR" )
      zwork2(:,:) = tztfl(:,:)%wio 
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
!* Ex-LOWFLUIW
! Weighted net FW flux sent by leads to the ocean [kg.m-2.s-1]
      tznam = t_def(  &
        "Real Water Flux into Sea Water under Leads",  &
        "real_water_flux_into_sea_water_under_leads",  &
        "wfsitso", "kg.m-2.s-1", "T", "SCALAR" )
      zwork2(:,:) = tztfl(:,:)%wlo
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
!* Ex-IOVFLUIW
! Weighted virtual FW flux sent by sea ice to the ocean [kg.m-2.s-1]
      tznam = t_def(  &
        "Virtual Water Flux into Sea Water due to Sea Ice Thermodynamics",  &
        "virtual_water_flux_into_sea_water_due_to_sea_ice_thermodynamics",  &
        "vwfsit", "kg.m-2.s-1", "T", "SCALAR" )
      zwork2(:,:) = tztfl(:,:)%cio
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
!* Ex-IOLFLUIW
! Weighted solar heat flux under sea ice [W.m-2]
      tznam = t_def(  &
        "Net Shortwave Flux under Sea Ice",  &
        "net_shortwave_flux_under_sea_ice",  &
        "rsntdusi", "W.m-2", "T", "SCALAR" )
      zwork2(:,:) = tztfl(:,:)%lio
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
!*Ex-LOLFLUIW
! Weighted solar heat flux sent by leads to the ocean
      tznam = t_def(  &
        "Net Shortwave Flux under Leads",  &
        "net_shortwave_flux_under_leads",  &
        "rsntduso", "W.m-2", "T", "SCALAR" )
      zwork2(:,:) = tztfl(:,:)%llo
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
!*Ex-LOTFLUIW + LOLFLUIW
! Net heat flux under leads only [W.m-2]
      tznam = t_def(  &
        "Downward Heat Flux Under Leads",  &
        "downward_heat_flux_under_leads",  &
        "hfduo", "W.m-2", "T", "SCALAR" )
      zwork2(:,:) = tztfl(:,:)%tlo + tztfl(:,:)%llo
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
!*Ex-IOTFLUIW + IOLFLUIW
! Net heat flux on under the ice surface only - weighted [W.m-2]
! (without the effect of snow)
      tznam = t_def(  &
        "Downward Heat Flux Under Sea Ice",  &
        "downward_heat_flux_under_sea_ice",  &
        "hfdusi", "W.m-2", "T", "SCALAR" )
      zwork2(:,:) = tztfl(:,:)%tio + tztfl(:,:)%lio
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
!*Ex-SIDMSAIW
! Weighted change of salt mass in sea ice - glt_thermo only [ kg.m-2.s-1 ]
      tznam = t_def(  &
        "Salt Mass in Sea Ice Rate",  &
        "salt_mass_in_sea_ice_rate",  & 
        "dmsalt", "kg.m-2.s-1", "T", "SCALAR" )
      zwork2(:,:) = tzdia(:,:)%dsa
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
!*Ex-SIDMSNIW
! Weighted snow mass change field - glt_thermo only [ kg.m-2.s-1 ]
      tznam = t_def(  &
        "Snow Mass over Sea Ice Rate due to Sea Ice Thermodynamics",  &
        "snow_mass_over_sea_ice_rate_due_to_sea_ice_thermodynamics",  & 
        "dmsnow", "kg.m-2.s-1", "T", "SCALAR" )
      zwork2(:,:) = tzdia(:,:)%dsn
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
! Weighted snow mass change field - dynamic only [ kg.m-2.s-1 ]
      tznam = t_def(  &
        "Snow Mass over Sea Ice Rate due to Sea Ice Dynamics",  &
        "snow_mass_over_sea_ice_rate_due_to_sea_ice_dynamics",  & 
        "dmsnowdyn", "kg.m-2.s-1", "T", "SCALAR" )
      zwork2(:,:) = tzdia(:,:)%ddn
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!!*Ex-SIDMSIIW
! Weighted sea ice mass change field - glt_thermo only [ kg.m-2.s-1 ]
      tznam = t_def(  &
        "Sea Ice Mass Rate",  &
        "sea_ice_mass_rate",  & 
        "dmice", "kg.m-2.s-1", "T", "SCALAR" )
      zwork2(:,:) = tzdia(:,:)%dsi
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
! Weighted sea ice mass change - constraint only [ kg.m-2.s-1 ]
      tznam = t_def(  &
        "Rate of Change of Sea Ice Mass due to Constraint",  &
        "rate_of_change_of_sea_ice_mass_due_to_constraint",  & 
        "dmicedmp", "kg.m-2.s-1", "T", "SCALAR" )
      zwork2(:,:) = tzdia(:,:)%dci
!
! Sea ice concentration constraint [% ]
      tznam = t_def(  &
        "Sea Ice Concentration Constraint",  &
        "sea_ice_concentration_traint",  & 
        "siccnst", "%", "T", "SCALAR" )
      zwork2(:,:) = tzdia(:,:)%cst
!
!*Ex-SIDMWIIW
! Weighted ice FW content change field - glt_thermo only [ kg.m-2.s-1 ]
      tznam = t_def(  &
        "Sea Ice Fresh Water Content Rate",  &
        "sea_ice_fresh_water_content_rate",  & 
        "dmwice", "kg.m-2.s-1", "T", "SCALAR" )
      zwork2(:,:) = tzdia(:,:)%dwi
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
!*Ex-ALLFWTOT (maybe not necessary)
! Total input water to the snow-ice leads system [kg.m-2.s-1]
      tznam = t_def(  &
        "Total Fresh Water Input Rate",  &
        "total_fresh_water_input_rate",  & 
        "dfwnt", "kg.m-2.s-1", "T", "SCALAR" )
      zwork2(:,:) = tzdia(:,:)%ifw
      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
! CHECK THAT THE SUM OF bottom, lateral and top mass changes match
! SIDMSIIW
! Already implemented as new diagnostics
!!!
!!! >>> Write weighted ice top mass balance [ kg.m-2.s-1 ]
!!!
!!      zwork2(:,:) = tzdia(:,:)%mrt
!!      yword = 'SIMRTIIW'  ! T SCALAR
!!      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!!!
!!! >>> Write weighted ice lateral ablation [ kg.m-2.s-1 ]
!!!
!!      zwork2(:,:) = tzdia(:,:)%mrl
!!      yword = 'SIMRLIIW' ! T SCALAR
!!      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!!!
!!! >>> Write weighted ice bottom mass balance [ kg.m-2.s-1 ]
!!!
!!      zwork2(:,:) =   &
!!        ( tzdia(:,:)%dsi-tzdia(:,:)%lsi-tzdia(:,:)%mrt-tzdia(:,:)%mrl )
!!      yword = 'SIMRBIIW'  ! T SCALAR
!!      CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
! >>> Write spare fields 
!
!      IF ( ANY( ABS( tzdia(:,:)%sp1 ) > epsil1 ) ) THEN 
!        zwork2(:,:) =   &
!          tzdia(:,:)%sp1
!        yword = 'FIELDSP1'  ! T SCALAR
!        CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!      ENDIF
!
!      IF ( ANY( ABS( tzdia(:,:)%sp2 ) > epsil1 ) ) THEN 
!        zwork2(:,:) =   &
!          tzdia(:,:)%sp2
!        yword = 'FIELDSP2'  !  T SCALAR
!        CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!      ENDIF
!
      DO jt=1,nt
!
!*Ex-SIFRCSIx 
! Sea ice categories concentration fields
        WRITE( yfdef,FMT='("Sea Ice Area Fraction Category ",I1.1)' ) jt 
        WRITE( ylnam,FMT='("sea_ice_area_fraction_category_",I1.1)' ) jt 
        WRITE( yword,FMT='("sic",I1.1)' ) jt
        tznam = t_def(  &
          yfdef, ylnam, yword, "%", "T", "SCALAR" )
        zwork2(:,:) = 100.*tzsit(jt,:,:)%fsi
        CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
! >>> Write pond volume over each ice category
        WRITE( yfdef,FMT='("Melt pond volume Category ",I1.1)' ) jt 
        WRITE( ylnam,FMT='("melt_pond_volume_category_",I1.1)' ) jt 
        WRITE( yword,FMT='("vmp",I1.1)' ) jt
        tznam = t_def(  &
          yfdef, ylnam, yword, "%", "T", "SCALAR" )
        zwork2(:,:) = tzsit(jt,:,:)%vmp
        CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
! >>> Write solar energy absorbed by sea ice categories
!
!        zwork2(:,:) = tzblki(jt,:,:)%swa
!        WRITE( yword,FMT='("AISWASI",I1.1)' ) jt
!        CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
! >>> Write non-solar energy absorbed by sea ice categories
!
!        zwork2(:,:) = tzblki(jt,:,:)%nsf
!        WRITE( yword,FMT='("SINSFSI",I1.1)' ) jt
!        CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia )
!
      END DO
!
  ENDIF
!
  IF ( ndiap3==1 ) THEN
!
! *Note that here, we do not divide zwork2 by zhsit, since tzdia%sit 
! contains the sum of the zhsit [weights] over all time outcomes
      tznam = t_def(  &
        "Sea Ice Salinity",  &
        "sea_ice_salinity",  &
        "ssi", "psu", "T", "SCALAR" )
      IF ( nicesal==1 ) THEN
          zwork2(:,:) =  &
            SUM( tzsit(:,:,:)%fsi*tzsit(:,:,:)%hsi*tzsit(:,:,:)%ssi, DIM=1 )
          CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia,  &
            pwgt=tzdia%sit )
      ENDIF
!
! *Note that here, we do not divide zwork2 by zfsit, since tzdia%sic 
! contains the sum of the zfsit [weights] over all time outcomes
      tznam = t_def(  &
        "Age of Sea Ice",  &
        "age_of_sea_ice",  &
        "ageice", "years", "T", "SCALAR" )
      IF ( niceage==1 ) THEN
          zwork2(:,:) =  &
            SUM( tzsit(:,:,:)%fsi*tzsit(:,:,:)%age, DIM=1 ) /  &
              ( xyear2day*xday2sec )
          CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia,  &
            pwgt=tzdia%sic )
      ENDIF
!
! *Note that here, we do not divide zwork2 by zfsit, since tzdia%sic 
! contains the sum of the zfsit [weights] over all time outcomes
      tznam = t_def(  &
        "Melt pond volume",  &
        "melt_pond_volume",  &
        "mpondvol", "m", "T", "SCALAR" )
      IF ( nmponds==1 ) THEN
          zwork2(:,:) =  &
            SUM( tzsit(:,:,:)%fsi*tzsit(:,:,:)%vmp, DIM=1 ) 
          CALL gltools_outdia( tzind,tznam,tzdom,zwork2,zcumdia,  &
            pwgt=tzdia%sic )
      ENDIF

  ENDIF 
!
! .. Just a final check, if ndiamax (in gltzar) is too small to hold all the
! data
!
  IF ( tzind%cur==tzind%beg ) THEN
      IF ( tzind%i2d>ndiamax ) THEN 
          WRITE( ymess,  &
            FMT='("Number of 2d diagnostic fields=", &
            &  I3,"> ndiamax=",I3,"\n")' ) tzind%i2d,ndiamax
          CALL gltools_glterr( 'imod_results','Check ndiamax in gltzar', 'STOP' )
      ENDIF 
      IF ( tzind%i0d>ndiamax ) THEN 
          WRITE( ymess,  &
            FMT='("Number of 0d diagnostic fields=", &
            &  I3,"> ndiamax=",I3,"\n")' ) tzind%i0d,ndiamax
          CALL gltools_glterr( 'imod_results','Check ndiamax in gltzar', 'STOP' )
      ENDIF 
  ENDIF
!
!
!
! 3. Sea ice totals for Northern and Southern hemispheres
! ========================================================
!
! 3.1. Compute sea ice totals
! ----------------------------
! 
! The following quantities are computed :
!   - sea ice extent (sum of all grid cells with more than X % ice
!     concentration)
!   - sea ice area
!   - sea ice volume
! No need to mask the ghost points, since tzdom%srf = 0 for ghost points
#if ! defined in_arpege
  zsrf(:,:) = tzdom(:,:)%srf * FLOAT(tzdom(:,:)%imk)
!
! .. Sea ice extent, north and south
!
  ynhemis(:,:) = ( tzdom(:,:)%lat>0..AND. tzdom(:,:)%tmk==1 )
  yshemis(:,:) = ( tzdom(:,:)%lat<0..AND. tzdom(:,:)%tmk==1 )
!
  zw = SUM(zsrf(:,:),MASK=(ynhemis.AND.zfsit(:,:)>xfsic)) / 1.E+12
  CALL mpp_sum( zw ) ; zehn = zw
!
  zw = SUM(zsrf(:,:),MASK=(yshemis.AND.zfsit(:,:)>xfsic)) / 1.E+12
  CALL mpp_sum( zw ) ; zehs = zw
!
! .. Sea ice area, north and south
!
  zw = SUM(zsrf(:,:)*zfsit(:,:), MASK=ynhemis) / 1.E+12
  CALL mpp_sum( zw ) ; zshn = zw
  zw = SUM(zsrf(:,:)*zfsit(:,:), MASK=yshemis) / 1.E+12
  CALL mpp_sum( zw ) ; zshs = zw
!
! .. Sea ice volume, north and south
!
  zw = SUM(zsrf(:,:)*zhsit(:,:), MASK=ynhemis) / 1.E+12
  CALL mpp_sum( zw ) ; zvhn = zw
  zw = SUM(zsrf(:,:)*zhsit(:,:), MASK=yshemis) / 1.E+12
  CALL mpp_sum( zw ) ; zvhs = zw
!
! .. Snow volume, north and south
!
  zw = SUM(zsrf(:,:)*zhsnt(:,:), MASK=ynhemis) / 1.E+12
  CALL mpp_sum( zw )
  zwhn = zw
  zw = SUM(zsrf(:,:)*zhsnt(:,:), MASK=yshemis) / 1.E+12
  CALL mpp_sum( zw )
  zwhs = zw

!
! .. Ice glt_transport through straits (depends on mesh geometry) 
! Has been only coded for NEMO1 grid.
! Note that the (i,j) indices are given for the full grid, including ghost
! points rows/ranks. For example, the NEMO1 grid has (362,292) points. 
! (i,j) coordinates must be given in this grid, even if i=1, i=362, 
! j=1, j=292 are ghost points.
! Here we are still in the parallel space. Some straits can be in different
! subdomains. We add up the contribution of the different subdomains 
! (computed separately in iceflx). Note that in iceflx, we do not consider 
! points which are ghost points if i=1 or j=1 (to avoid counting them twice).
!
  zfram = 0.
  zbering = 0.
  zncwest = 0.
  znceast = 0.
  znorthb = 0.
#if ! defined in_surfex
!
  IF ( cn_grdname=='NEMO1' ) THEN
!
! Fram Strait
      ii0 = 269
      ij0 = 272
!
      DO ji=1,10
        ii = ii0+ji-1
        ij = ij0
        zfram = zfram +  &
          iceflx( tzdom,zhsit,tzdia,ii,ij,ii,ij+1 )
      END DO
      zfram = -rhoice*zfram
!
! Bering Strait
      ii0 = 115
      ij0 = 246
!
      DO ji=1,2
        ii = ii0+ji-1
        ij = ij0
        zbering = zbering +  &
          iceflx( tzdom,zhsit,tzdia,ii,ij,ii,ij+1 )
      END DO
      zbering = -rhoice*zbering
!
! North Canadian Archipelago (West)
      ii0 = 232
      ij0 = 289
!
      DO ji=1,2
        ii = ii0
        ij = ij0+ji-1
        zncwest = zncwest +  &
          iceflx( tzdom,zhsit,tzdia,ii,ij,ii+1,ij )  
      END DO
      zncwest = -rhoice*zncwest
!
! Nares Strait (between Ellesmere Land and North Western Greenland)
!  - we compute the ice flux at the northern boundary of this strait (at its 
! Arctic Ocean boundary)
      ii0 = 253
      ij0 = 277
!
      DO ji=1,2
        ii = ii0
        ij = ij0+ji-1
        znceast = znceast +  &
          iceflx( tzdom,zhsit,tzdia,ii,ij,ii+1,ij )  
      END DO
      znceast = -rhoice*znceast
!
! Barrow Strait (between Prince of Wales I. - south and Bathurst I. - north)
!  - we compute the ice flux 
      ii0 = 283
      ij0 = 274
!
      DO ji=1,14
        ii = ii0+ji-1
        ij = ij0+ji-1
        znorthb = znorthb -  &
          iceflx( tzdom,zhsit,tzdia,ii,ij,ii,ij+1 ) +  &
          iceflx( tzdom,zhsit,tzdia,ii-1,ij,ii,ij )  
      END DO
      znorthb = -rhoice*znorthb
!
  ENDIF
!
!
! 3.2. Write totals to diagnostic file
! -------------------------------------
!
  IF ( ndiap1==1 ) THEN 
!
!* Ex-SIEHNSIG
      tznam = t_def(  &
        "North Hemisphere Sea Ice Extent",  &
        "north_hemisphere_sea_ice_extent",  &
        "icextn", "10^6 km2", "T", "SCALAR" )
      CALL gltools_outdia( tzind,tznam,tzdom,zehn,zcumdia0 )
!
!* Ex-SIEHNSIG
      tznam = t_def(  &
        "South Hemisphere Sea Ice Extent",  &
        "south_hemisphere_sea_ice_extent",  &
        "icexts", "10^6 km2", "T", "SCALAR" )
      CALL gltools_outdia( tzind,tznam,tzdom,zehs,zcumdia0 )
!
!* Ex-SISHNSIG
      tznam = t_def(  &
        "North Hemisphere Sea Ice Area",  &
        "north_hemisphere_sea_ice_area",  &
        "icearean", "10^6 km2", "T", "SCALAR" )
      CALL gltools_outdia( tzind,tznam,tzdom,zshn,zcumdia0 )
!
!* Ex-SISHSSIG
      tznam = t_def(  &
        "South Hemisphere Sea Ice Area",  &
        "south_hemisphere_sea_ice_area",  &
        "iceareas", "10^6 km2", "T", "SCALAR" )
      CALL gltools_outdia( tzind,tznam,tzdom,zshs,zcumdia0 )
!
!* Ex-SIVHNSIG
      tznam = t_def(  &
        "North Hemisphere Sea Ice Volume",  &
        "north_hemisphere_sea_ice_volume",  &
        "icevoln", "10^3 km3", "T", "SCALAR" )
      CALL gltools_outdia( tzind,tznam,tzdom,zvhn,zcumdia0 )
!
!* Ex-SIVHSSIG
      tznam = t_def(  &
        "South Hemisphere Sea Ice Volume",  &
        "south_hemisphere_sea_ice_volume",  &
        "icevols", "10^3 km3", "T", "SCALAR" )
      CALL gltools_outdia( tzind,tznam,tzdom,zvhs,zcumdia0 )
!
!* Ex-SIWHNSIG
      tznam = t_def(  &
        "North Hemisphere Snow Volume",  &
        "north_hemisphere_sea_ice_volume",  &
        "snovoln", "10^3 km3", "T", "SCALAR" )
      CALL gltools_outdia( tzind,tznam,tzdom,zwhn,zcumdia0 )
!
!* Ex-SIWHSSIG
      tznam = t_def(  &
        "South Hemisphere Snow Volume",  &
        "south_hemisphere_snow_volume",  &
        "snovols", "10^3 km3", "T", "SCALAR" )
      CALL gltools_outdia( tzind,tznam,tzdom,zwhs,zcumdia0 )
! 
!* Ex-SIFRAMST
      tznam = t_def(  &
        "Sea Ice Mass Transport Through Fram Strait",  &
        "",  &
        "transifs", "kg.s-1", "T", "SCALAR" )
      CALL gltools_outdia( tzind,tznam,tzdom,zfram,zcumdia0 )
! 
!* Ex-SIBERING
      tznam = t_def(  &
        "Sea Ice Mass Transport Through Bering Strait",  &
        "",  &
        "tibering", "kg.s-1", "T", "SCALAR" )
      CALL gltools_outdia( tzind,tznam,tzdom,zbering,zcumdia0 )
! 
!* Ex-SINCWEST (North Canadian Archipelago - West)
      tznam = t_def(  &
        "Sea Ice Mass Transport Through NCA West",  &
        "",  &
        "tincawest", "kg.s-1", "T", "SCALAR" )
      CALL gltools_outdia( tzind,tznam,tzdom,zncwest,zcumdia0 )
! 
!* Ex-SINCEAST (North Canadian Archipelago - East, Nares Strait)
      tznam = t_def(  &
        "Sea Ice Mass Transport Through Nares Strait",  &
        "",  &
        "tinares", "kg.s-1", "T", "SCALAR" )
      CALL gltools_outdia( tzind,tznam,tzdom,znceast,zcumdia0 )
! 
!* Ex-SINORTHB
      tznam = t_def(  &
        "Sea Ice Mass Transport Through Barrow Strait",  &
        "",  &
        "tibarrow", "kg.s-1", "T", "SCALAR" )
      CALL gltools_outdia( tzind,tznam,tzdom,znorthb,zcumdia0 )
!
  ENDIF
#endif
#endif
!
!
! 3.3. Print out some important statistics to outzut file
! --------------------------------------------------------
!
  IF(lp1) WRITE(noutlu,*) '                              North        South'
  IF(lp1) WRITE(noutlu,1000) zshn,zshs
  IF ( ndiap3==1 ) THEN 
    IF(lp1) WRITE(noutlu,1100) zehn,zehs
  ENDIF
  IF(lp1) WRITE(noutlu,1200) zwhn,zwhs
  IF(lp1) WRITE(noutlu,1300) zvhn,zvhs
  IF(lp1) WRITE(noutlu,*) '    Ice flux at Fram : ',zfram
  IF(lp1) WRITE(noutlu,*) ' '
!
!
!
! 4. End
! =======
!
1000 FORMAT(5X,"Ice surface (SISH.SIG)",2(4X,F9.5))
1100 FORMAT(5X,"Ice extent  (SIEH.SIG)",2(4X,F9.5))
1200 FORMAT(5X,"Snow volume (SIWH.SIG)",2(4X,F9.5))
1300 FORMAT(5X,"Ice volume  (SIVH.SIG)",2(4X,F9.5))
!
! .. Farewell message
!
  IF(lp1) WRITE(noutlu,*) ' '
  IF(lp1) WRITE(noutlu,*) ' *** LEVEL 3 - END SUBROUTINE WRIDIA_AR5'
  IF(lp1) WRITE(noutlu,*) ' '
!
END SUBROUTINE wridia_ar5 
!
! --------------------- END SUBROUTINE WRIDIA_AR5 -----------------------
! -----------------------------------------------------------------------
END MODULE mode_glt_dia_ar5
!
! ---------------------- END MODULE mode_glt_dia_ar5 ------------------------
