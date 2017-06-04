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
! ========================= MODULE modd_types_glt ===========================
! =======================================================================
!
! Goal:
! -----
!  This module contains the definitions of all the structured 
! variables used in Gelato.
!
! Created : 1996 (D. Salas y Melia)
! Modified: 2009/08 (D. Salas y Melia)
!           Remove obsolete fields, add new types_glt used by Gelato's new
!           interface (allowing double or single physics), re-classify
! Modified: 2010/06 (D. Salas y Melia)
!           New CMIP5 diagnostics
! Modified: 2011/12 (A. Voldoire)
!           New diagnostics were added to compute water balance 
! Modified: 2012/01 (M. Chevallier)
!           Explicit melt pond parameterisation
! Modified: 2012/11 (D. Salas y Melia)
!           Introduce a 'super-structure'. Add ice-ocean stress 
!           components to t_tfl
! Modified: 2013/07 (S. Senesi) : #ifdef in_surfex for dynamic arrays 
!           in t_glt
! -----------------------------------------------------------------------
!
MODULE modd_types_glt
!
USE modd_glt_param
!
!
! 1. Structure defining the domain of study
! ==========================================
!
TYPE t_dom
! If tmk = 1, the grid T-point is ocean, else it is land.
  INTEGER ::                                                            &
        tmk
! If umk = 1, the grid U-point is ocean, else it is land.
  INTEGER ::                                                            &
        umk
! If vmk = 1, the grid V-point is ocean, else it is land.
  INTEGER ::                                                            &
        vmk
! If imk = 1, the grid T-point is ocean, else it is land. Ghost points masked.
  INTEGER ::                                                            &
        imk
! Values of i and j index in the global grid
  INTEGER ::                                                            &
        indi,indj
! Grid point longitude (radians)
  REAL ::                                                               &
        lon
! Grid point latitude (radians)
  REAL ::                                                               &
        lat
! Grid cell length (m) in the X-direction
  REAL ::                                                               &
        dxc
! Grid cell length (m) in the Y-direction
  REAL ::                                                               &
        dyc
! Grid cell surface (=dxc*dyc), ghost points masked
  REAL ::                                                               &
        srf
END TYPE t_dom
!
!
! 
! 2. Structures defining the forcing data
! ========================================
!
! 2.1. Ocean mixed layer forcing
! -------------------------------
!
! .. This type contains all variables describing the state of the 
! ocean mixed layer, that influences sea ice and leads from the bottom.
!
TYPE t_mxl
!
! Frazil flux from ocean, converted in equivalent W.m-2
    REAL ::                                                             &
        qml
! Ocean heat flux, per m2 of marine surface (W.m-2)
    REAL ::                                                             &
        qoc
! Temperature (K)
    REAL ::                                                             &
        tml
! Freezing temperature (K)
    REAL ::                                                             &
        mlf
! Salinity (psu)
    REAL ::                                                             &
        sml
! SSH (m)
    REAL ::                                                             &
        ssh
! Velocity (m.s-1)
    REAL ::                                                             &
        uml,vml
! Mixed layer heat content wrt ice melting T / kg, i.e. Cp x (Tml-Tice_m)
    REAL                                                                &
        hco
END TYPE t_mxl
!
!
! 2.2. Atmospheric forcing, independent of the nature of the surface
! -------------------------------------------------------------------
!
! .. Liquid and solid precipitation does not depend on the surface on 
! which they fall. We suppose here that the stress is not different on
! a snow, water or bare ice surface, which is not totally the case in
! reality.
!
TYPE t_atm
!
! Liquid precipitations (m/time step)
    REAL ::                                                             &
        lip
! Solid precipitations (m/time step)
    REAL ::                                                             &
        sop
! Zonal wind stress (N.m-2)
    REAL ::                                                             &
        ztx
! Meridional wind stress (N.m-2)
    REAL ::                                                             &
        mty
END TYPE t_atm
!
!
! 2.3. Atmospheric forcing, dependent on the nature of the surface
! -----------------------------------------------------------------
!
! .. This structure may concern sea ice or open water. It contains:
!       - short wave absorbed flux
!       - non solar heat flux (i.e the sum of long wave, latent and 
!       sensible heat fluxes)
!       - the derivative of this flux by surface temperature
!       - the evaporation rate
!
TYPE t_blk
! Short wave flux absorbed by the surface (W.m-2)
  REAL ::                                                               &
        swa
! Non solar flux affecting the surface (W.m-2)
  REAL ::                                                               &
        nsf
! Derivative of non solar flux by the surface temperature (W.m-2.K-1)
  REAL ::                                                               &
        dfl
! Evaporation rate (m.s-1)
  REAL ::                                                               &
        eva
END TYPE t_blk
!
!
!
! 3. Structures describing sea ice dynamic and thermodynamic states
! ==================================================================
!
! 3.1. Sea ice dynamics (EVP, JFNK)
! ----------------------------------
!
TYPE t_evp
! (u,v) velocity component
    REAL ::                                                             &
        uvl,vvl
! Stress tensor components
    REAL ::                                                             &
        oone,oose,oosw,oonw
    REAL ::                                                             &
        otne,otse,otsw,otnw
    REAL ::                                                             &
        ttne,ttse,ttsw,ttnw
END TYPE t_evp
!
TYPE t_jfn
! (u,v) velocity components
    REAL ::  &
      uvl,vvl
END TYPE t_jfn
!
!
! 3.2. Sea ice thermodynamics - 0D variables
! -------------------------------------------
!
TYPE t_sit
!
! Boolean that is TRUE if ice class is empty
    LOGICAL ::                                                          &
        esi
! Ice age
    REAL ::                                                             &
        age
! Snow layer albedo
    REAL ::                                                             &
        asn
! Areal fraction for each ice type
    REAL ::                                                             &
        fsi
! Ice thickness for each ice type
    REAL ::                                                             &
        hsi
! Ice salinity for each ice type
    REAL ::                                                             &
        ssi
! Snow thickness over each ice type
    REAL ::                                                             &
        hsn
! Density of snow over each ice type
    REAL ::                                                             &
        rsn
! Ice or snow surface temperature
    REAL ::                                                             &
        tsf
! Melt pond volume
    REAL ::                                                             &
        vmp
END TYPE t_sit
!
!
! 3.3. Sea ice thermodynamics - 1D variables
! -------------------------------------------
!
TYPE t_vtp
! Vertical gltools_enthalpy profile in the slab (J.kg-1)
    REAL ::  &
        ent         
! Vertical salinity profile (g.kg-1)
    REAL ::  &
        vsp
END TYPE t_vtp
!
!
! 3.4. Leads - 0D variables
! --------------------------
!
! .. May be removed in subsequent versions, unless more physics takes 
! places in leads
!
TYPE t_opw
! Areal fraction covered by the lead
    REAL ::                                                             &
        fld
! Lead temperature (K)
    REAL ::                                                             &
        tld
!
END TYPE t_opw
!
!
! 
! 4. Output structures
! =====================
!
! 4.1. Fluxes transmitted to the mixed layer
! -------------------------------------------
!
! NOTE: This is internal to Gelato (not involved in Gelato's interface
! with the ocean model)
!
! .. This structure contains all heat, non solar and solar short wave heat
! fluxes that cross leads or sea ice to reach the mixed layer. The 
! interest of these fluxes lies mainly in the framework of ocean/sea ice
! coupling, and the classifications of variables is the following 
! inside the type, if type_var%xyz
!       x = (l,t,s,c,w) are respectively short wave, non solar, 
! concentration/dilution equivalent water flux (also called virtual 
! water flux) and fresh water mass fluxes. Heat fluxes are in W.m-2 and 
! water virtual fluxes, water fluxes and salt fluxes are in kg.m-2.s-1. 
!       y or z = (i,l,o) are respectively sea ice, lead and mixed layer.
! y is the medium the flux originates from, and z is the destination
! medium. 
!
! * Example : flx%wio is the fresh water flux from sea ice to the mixed 
! layer.
! 
! .. Note that this structure also contains the ice-ocean stress 
! components along x and y (respectively xio and yio)
!
TYPE t_tfl
  REAL ::  &
        lio,llo
  REAL ::  &
        tio,tlo
  REAL ::   &
        sio     ! Note that slo is always zero
  REAL ::  &
        cio     ! Note that clo would be identical to wlo
  REAL ::  &
        wio,wlo 
  REAL ::  &
        xio,yio
END TYPE t_tfl
!    
!
! 4.2. Fluxes transmitted to the ocean model
! -------------------------------------------
!
! NOTE: In contrast with t_tfl, this type is transmitted to the ocean
! model (not strictly internal to Gelato)
!
TYPE t_2oc
!
! Net non solar heat flux (W.m-2)
  REAL ::  &
        nsf
! Net short wave heat flux (W.m-2)
  REAL ::  &
        swa
! Concentration/dilution flux (kg.m-2.s-1)
  REAL ::  &
        cdf
! Salt flux (kg.m-2.s-1)
  REAL ::  &
        saf
! Water flux (kg.m-2.s-1)
  REAL ::  &
        wfl
! Stress X-component (N.m-2)
  REAL ::  &
        ztx
! Stress Y-component (N.m-2)
  REAL ::  &
        mty
! Ice-Ocean friction velocity U* (m.s-2)
  REAL ::  &
        ust
END TYPE t_2oc
!
!
! 4.3. Surface description, sent to the atmosphere (through the ocean model ?)
! -----------------------------------------------------------------------------
!
! .. This structure includes a fraction, albedo and temperature. Two cases 
! arise:
!       - if single physics is used (only one input flux to be shared between
!       the different kinds of surface): the fraction should be the total 
!       fraction of sea ice. The temperature and albedo should be weighted over 
!       water and sea ice.
!       - if double physics is used, the fraction is stille the total fraction
!       of sea ice, but the temperature and albedo are now weighted over
!       sea ice only.
!
TYPE t_2at
!
! Sea ice fraction (0-1)
  REAL ::  &
    fsi
! Albedo (0-1)
  REAL ::  &
    alb
! Temperature (K)
  REAL ::  &
    tsf
END TYPE t_2at
!
!
! 4.4. Energy and water budget
! -----------------------------
!
TYPE t_bud
!
! Sea ice/snow gltools_enthalpy in the grid cell, beg. of time step (J)
  REAL ::  &
        eni
! Sea ice/snow gltools_enthalpy in the grid cell, current (J)
  REAL ::  &
        enn
! Input heat flux on sea ice in the grid cell (bottom)
  REAL ::  &
        bii
! Input heat flux due to snow falls on sea ice 
  REAL ::  &
        nii
! Input heat flux due to snow falls on leads
  REAL ::  &
        nli
! Input heat flux on sea ice in the grid cell (top)
  REAL ::  &
        hii
! Input heat flux on leads in the grid cell
  REAL ::  &
        hli
! Output heat flux on sea ice in the grid cell
  REAL ::  &
        hio
! Output heat flux on leads in the grid cell
  REAL ::  &
        hlo
! Input water flux on sea ice in the grid cell
  REAL ::  &
        wii
! Input water flux on leads in the grid cell
  REAL ::  &
        wli
! Grid cell water stored in sea ice & snow, beg. of time step (kg.m-2)
  REAL ::  &
        fwi
! Grid cell water stored in sea ice & snow, current (kg.m-2)
  REAL ::  &
        fwn
! Grid cell salt stored in sea ice, beg. of time step (kg.m-2)
  REAL ::  &
        isi
! Grid cell salt stored in sea ice, current (kg.m-2)
  REAL ::  &
        isn
!
END TYPE t_bud
!
!
! 4.5. Diagnostics
! -----------------
!
! .. Note that here the sea ice net bottom melting [kg.m-2.s-1] is deduced 
! from dsi-lsi-mrt-mrl
!
! FIELDS
!
TYPE t_dia
! 
! Sea ice u and v velocity on C-grid (m.s-1)
  REAL ::  &
        uvl,vvl
! Weights for bare sea ice albedo
  REAL ::  &
        aiw
! Bare sea ice albedo, averaged over all categories * total ice fraction [0-1]
  REAL ::  &
        asi
! Melt pond albedo, averaged over all categories * total ice fraction [0-1]
  REAL ::  &
        amp
! Snow albedo, averaged over all categories * total ice fraction [0-1]
  REAL ::  &
        asn
! Atmospheric stress on sea ice (X-component) * total ice fraction [N.m-2]
  REAL ::  &
        atx
! Atmospheric stress on sea ice (Y-component) * total ice fraction [N.m-2]
  REAL ::  &
        aty
! Congelation sea ice growth rate [kg.m-2.s-1]
  REAL ::  &
        cgl
! Salt mass net change for all glt_thermo processes [kg.m-2.s-1]
  REAL ::  &
        dsa
! Salt mass net change due to dynamic processes [kg.m-2.s-1]
  REAL ::  &
        dds
! Snow mass net change for all glt_thermo processes [kg.m-2.s-1]
  REAL ::  &
        dsn
! Snow mass net change due to dynamic processes [kg.m-2.s-1]
  REAL ::  &
        ddn
! Sea ice mass change for all glt_thermo processes [kg.m-2.s-1]
  REAL ::  &
        dsi
! Sea ice mass change due to dynamic processes [kg.m-2.s-1]
  REAL ::  &
        ddi
! Sea ice mass change due to the constraint [kg.m-2.s-1]
  REAL ::  &
        dci
! Sea ice concentration for constraint [%] 
  REAL ::  &
        cst
! Sea ice fresh water content change for all glt_thermo processes [kg.m-2.s-1]
  REAL ::  &
        dwi
! Liquid precipitation falling on the sea ice portion of grid cell [kg.m-2.s-1]
  REAL ::  &
        lip
! Sea ice production in leads [kg.m-2.s-1]
  REAL ::  &
        lsi
! Sea ice net bottom melting [kg.m-2.s-1]
  REAL ::  &
        mrb
! Sea ice net top melting [kg.m-2.s-1]
  REAL ::  &
        mrt
! Sea ice lateral melting [kg.m-2.s-1]
  REAL ::  &
        mrl
! Ocean stress on sea ice (X-component) * total ice fraction [N.m-2]
  REAL ::  &
        otx
! Ocean stress on sea ice (Y-component) * total ice fraction [N.m-2]
  REAL ::  &
        oty
! Enthalpy of the sea-ice part of the ice-snow slab [J.m-2]
  REAL ::  &
        sie
! Enthalpy of the snow part of the ice-snow slab [J.m-2]
  REAL ::  &
        sne
! Snow-Ice formation rate [kg.m-2.s-1]
  REAL ::  &
        sni
! Snow melt rate [kg.m-2.s-1]
  REAL ::  &
        snm
! Snow melt rate due to lateral ice erosion [kg.m-2.s-1]
  REAL ::  &
        snml
! Solid precipitation falling on the sea ice portion of grid cell [kg.m-2.s-1]
  REAL ::  &
        sop
! Net shortwave on the sea ice portion of grid cell [W.m-2]
  REAL ::  &
        swi
! Net shortwave on the leads portion of the grid cell [W.m-2]
  REAL ::  &
        sww
! Sea ice gltools_enthalpy change due to the thermodynamics  [W.m-2]
  REAL ::  &
        the
! Temperature at snow-ice interface * ice fraction [K]
  REAL ::  &
        tin
! Ice concentration weights for temperature at snow-ice interface
  REAL ::  &
        tiw
! Input Fresh Water into the snow-ice-leads system [kg.m-2.s-1]
  REAL ::  &
        ifw
! Accumulated total sea ice concentation over all time steps [0-1]
  REAL ::  &
        sic
! Accumulated average sea ice thickness over all time steps [m]
  REAL ::  &
        sit
! Accumulated average snow thickness over all time steps [m]
  REAL ::  &
        snd
! Ocean-ice heat flux per m2 of marine surface [W.m-2]
  REAL ::  &
        qoi
! Transport X and Y-components [kg.s-1]
  REAL ::  &
        xtr,ytr
! Spare fields 
  REAL ::  &
        sp1,sp2
! Sublimation
  REAL ::  &
        sut
! Sublimation over ice
  REAL ::  &
        sui
! Sublimation over snow
  REAL ::  &
        sus
! Sublimation over ocean
  REAL ::  &
        suw
! Sublimation over leads
  REAL ::  &
        sul
! Liquid precip that actually falls on snow
  REAL ::  &
        s_pr
! Liquid precip that actually falls on ocean
  REAL ::  &
        o_pr
! Liquid precip that actually falls on leads
  REAL ::  &
        l_pr
! Solid precip that actually falls on snow
  REAL ::  &
        s_prsn
! Solid precip that actually falls on ocean
  REAL ::  &
        o_prsn
! Solid precip that actually falls on leads
  REAL ::  &
        l_prsn
! terms of virtual salt flux (sublimation, snow2ice, updhsi, lmltsi, updsal)
  REAL :: &
        subcio, snicio, hsicio, lmlcio, salcio
! Total damping/restoring (in equivalent energy flux)
  REAL ::  &
        dmp
END TYPE t_dia
!
! FIELD INDICES, COUNTERS
!
TYPE t_ind
!
! Current time-step
  INTEGER ::  &
        cur
! First time-step
  INTEGER ::  &
        beg
! Last time-step
  INTEGER ::  &
        end
! Current 0d, 2d field index
  INTEGER ::  &
        i0d,i2d
! Number of time steps in accumulating
  INTEGER ::  &
        nts
END TYPE t_ind
!
!
! 4.6. Definitions of diagnostics
! --------------------------------
!
TYPE t_def
! 
! Field definition
  CHARACTER(80) ::  &
    def 
! Field long name
  CHARACTER(80) ::  &
    lna 
! Field short name
  CHARACTER(80) ::  &
    sna 
! Field units
  CHARACTER(80) ::  &
    uni 
! Field location ('T', 'U' or 'V')
  CHARACTER(80) ::  &
    loc
! Field nature ('SCALAR' or 'VECTOR')
  CHARACTER(80) ::  &
    typ
END TYPE t_def
!
!
!
! 5. Super-structure
! ===================
!
! This structures includes all structures that are needed by the different
! parts of Gelato that are invoked from parts of the host program (e.g. 
! surface-scheme or ocean model)
!
TYPE t_glt
!
!
! 5.1. Time-step information
! ---------------------------
!
  TYPE(t_ind) ::  &
    ind
!
!
! 5.2. Domain definition
! -----------------------
!
! Bathymetry (in meters)
  REAL, DIMENSION(:,:), POINTER ::  &
    bat
! Main domain 
  TYPE(t_dom), DIMENSION(:,:), POINTER ::  &
    dom
#if ! defined in_surfex
! Domain for dynamics (EVP) and advection
  REAL ::  &
    xymin
  REAL, DIMENSION(:,:), POINTER ::     &
    dxtr,dytr,fcor,tarear
  REAL, DIMENSION(:,:), POINTER ::     &
    HTN,HTE,HTS,HTW,dxtr4,dytr4,hm,um,  &
    dxta,dyta,dxt2r,dyt2r,HTNa,HTEa,  &
    cx,cy,cxx,cxy,cyy,cxxx,cxxy,cxyy,cyyy
  REAL, DIMENSION(:,:,:,:), POINTER ::     &
    mne,mnw,msw,mse
! Index mapping, for 1d packed vectors
  INTEGER, DIMENSION(:,:,:), POINTER ::  &
    index2d
! Ice mask, to pack arrays to 1d vectors
  REAL, DIMENSION(:,:,:), POINTER ::  &
    mskice
#endif
!
!
! 5.3. Input/Output of the glt_gelato main routine
! ---------------------------------------------
!
! Ocean forcing
  TYPE(t_mxl), DIMENSION(:,:), POINTER ::  &
    oce_all
! Input atmospheric forcing (independent on the nature of the surface)
  TYPE(t_atm), DIMENSION(:,:), POINTER ::  &
    atm_all
! Atmospheric forcing (potentially dependent on the nature of the surface)
  TYPE(t_blk), DIMENSION(:,:,:), POINTER ::  &
    atm_ice, atm_mix
! Atmospheric forcing on water
  TYPE(t_blk), DIMENSION(:,:), POINTER ::  &
    atm_wat
! Output to the ocean
  TYPE(t_2oc), DIMENSION(:,:), POINTER ::  &
    all_oce
! Output to the atmosphere
  TYPE(t_2at), DIMENSION(:,:,:), POINTER ::  &
    ice_atm, mix_atm
! Constraint on sea ice
  TYPE(t_sit), DIMENSION(:,:,:), POINTER ::  &
    sit_d
!
!
! 5.4. Ice and mixed layer states 
! --------------------------------
!
  TYPE(t_evp), DIMENSION(:,:), POINTER ::  &
    evp
!
  TYPE(t_jfn), DIMENSION(:,:), POINTER ::  &
    jfn
!
  TYPE(t_sit), DIMENSION(:,:,:), POINTER ::  &
    sit
!
  TYPE(t_vtp), DIMENSION(:,:,:,:), POINTER ::  &
    sil
!
  TYPE(t_mxl), DIMENSION(:,:), POINTER ::  &
    tml
!
!
! 5.5. Fluxes, budgets, diagnostics
! ----------------------------------
!
  REAL, DIMENSION(:,:), POINTER ::  &
    ust
!
  REAL, DIMENSION(:,:,:), POINTER ::  &
    cdia0,cdia
!
  TYPE(t_blk), DIMENSION(:,:), POINTER ::  &
    blkw
!
  TYPE(t_blk), DIMENSION(:,:,:), POINTER ::  &
    blki
!
  TYPE(t_tfl), DIMENSION(:,:), POINTER ::  &
    tfl
!
  TYPE(t_bud), DIMENSION(:,:), POINTER ::  &
    bud
!
  TYPE(t_dia), DIMENSION(:,:), POINTER ::  &
    dia
!
END TYPE t_glt
!
END MODULE modd_types_glt
