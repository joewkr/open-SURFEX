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
! ========================= MODULE modd_glt_param ===========================
! =======================================================================
!
! Goal:
! -----
!   This module contains the models parameters that are determined from
! the namelist, and derived quantities.
!
! Created : ? (D. Salas y Melia)
! Modified: 2012/08 (D. Salas y Melia) parallel model
!
! ---------------------- BEGIN MODULE modd_glt_param ------------------------
!
MODULE modd_glt_param
  IMPLICIT NONE
!
!
!
! 1. Model parameters
! ===================
!
! .. These parameters can be (theoretically) freely changed by the user
!
!
! 1.1. Options to run glt_gelato
! --------------------------
!
!  - nmkinit    : create initial conditions file
!       nmkinit=0  --> use a restart file instead
!       nmkinit=1  --> use sea ice analytical initialization
!       nmkinit=2  --> use a sea ice fraction climatology
!  - nrstout    : create an output restart file
!       nrstout=0  --> no output restart
!       nrstout=1  --> output restart will be created
!  - nrstgl4    : about restart format
!       nrstgl4=0  --> use an old format restart (before Gelato 4)
!       nrstgl4=1  --> use a new format restart (Gelato 4 and newer)
!  - nthermo    : disable/enable sea ice thermodynamics
!       nthermo=0  --> no thermodynamics
!       nthermo=1  --> thermodynamics enabled
!  - ndynami    : disable/enable sea ice dynamics
!       ndynami=0  --> no dynamics
!       ndynami=1  --> dynamics enabled
!  - nadvect    : disable/enable ice glt_transport
!       nadvect=0  --> no ice glt_transport
!       nadvect=1  --> ice glt_transport enabled
!  - ntimers    : disable/enable ice gltools_timers
!  - ndyncor    : correct water and salt non-conservation due to advection
!       ndyncor=0  --> no correction
!       ndyncor=1  --> correction
!  - ncdlssh    : take ssh into account when computing concentration/dilution
!       ncdlssh=0  --> ssh not taken into account
!       ncdlssh=1  --> ssh taken into account
!  - niceage    : disable/enable ice age computation
!       niceage=0  --> no ice age computation
!       niceage=1  --> ice age computation enabled
!  - nicesal    : disable/enable ice salinity computation
!       nicesal=0  --> no ice salinity computation
!       nicesal=1  --> ice salinity computation enabled
!  - nmponds    : disable/enable melt pond computation (for ice surface albedo)
!       nmponds=0  --> no melt ponds computation
!       nmponds=1  --> melt ponds computation enabled
!  - nsnwrad    : snowfall radiative effect
!       nsnwrad=0  --> no radiative effect of snow melting into sea water
!                      (recommended in case of coupling, if snow fall in
!                      your atm. model does not cause any heat gain for the
!                      atmosphere)
!       nsnwrad=1  --> generates a negative heat flux sent to the ocean by
!                      Gelato, due to the melting of snow into the ocean
!  - nleviti    : sea ice is levitating over the ocean or not 
!       nleviti=0  --> sea ice is not levitating (a freshwater flux due to the
!                      melting/freezing of ice is sent to the ocean model)
!                  --> sea ice is levitating
!  - nsalflx    : ice-ocean salt flux parameterisation 
!                 (if 2 or 3, check ocean topmost level dz parameter rn_htopoc in
!                 the namelist !)
!       nsalflx=1  --> approximated calculation
!       nsalflx=2  --> exact calculation
!       nsalflx=3  --> exact calculation, but SSS replaced with standard sal.
! - nextqoc    : ocean-ice heat flux
!       nextqoc=1  --> the %qoc given as an input is taken into account
!       nextqoc=0  --> the %qoc is computed by Gelato
! - nicesub    : ice sublimation
!       nicesub=1  --> take ice sublimation into account (non heat conservative)
!       nicesub=0  --> no ice sublimation 
! - cnflxin    : input fluxes
!       cnflxin    --> 'mixed' : only one flux, to share between water/ice
!       cnflxin    --> 'double': one flux for water, one flux for ice
!       cnflxin    --> 'multi' : one flux for water, one flux for each ice cat
!
INTEGER ::  &
  nmkinit, nrstout, nrstgl4, nthermo, ndynami, nadvect,  &
  ntimers, ndyncor, ncdlssh, niceage, nicesal, nmponds,  &
  nsnwrad, nleviti, nsalflx, nextqoc, nicesub
CHARACTER(10) ::  &
  cnflxin
!
!
! 1.2. Damping and restoring
! -------------------------
!
!  - cfsidmp    : sea ice fraction constraint
!       cfsidmp='NONE'       --> no sea ice fraction constraint
!       cfsidmp='DAMP'       --> damp
!       cfsidmp='PRESCRIBE'  --> prescribe
!  - xfsidmpeft : sea ice fraction damping e-folding time (in days)
!  - chsidmp    : sea ice thickness constraint
!       chsidmp='NONE'       --> no sea ice thickness constraint
!       chsidmp='DAMP_ADD'   --> damp (thickness of all ice categories is 
!         modified by the same value: h_i => h_i + add)
!       chsidmp='DAMP_FAC'   --> damp (thickness of all ice categories is 
!         modified by the same factor: h_i => h_i * fac)
!       chsidmp='PRESCRIBE'  --> prescribe
!  - xhsidmpeft : sea ice thickness damping e-folding time (in days)
!
CHARACTER(20) ::  &
  cfsidmp, chsidmp
REAL ::  &
  xfsidmpeft, xhsidmpeft
!
!
! 1.3. Diagnostics glt_output
! -----------------------
!
!  - cdiafmt    : diagnostics format
!       cdiafmt='GELATO'  --> Gelato Vairmer format
!       cdiafmt='VMAR5'   --> IPCC AR5 vairmer format
!       cdiafmt='NCAR5'   --> IPCC AR5 NetCDF format (not active yet)
!       cdiafmt='XIOS'    --> Use XIOS I/O server (NetCDF format and
!         diags namelist shared with Nemo - works only within Nemo yet) 
!  - cdialev    : diagnostics level 
!       . cdialev can include one, two or all of the letters b, d and t
!       . If cdiafmt='GELATO' 
!           - 1: makes the 2D diag. file (2D fields), called
!          '2d.[ave|ins].vairmer'. Contains: ice concentration, thickness,
!          velocity components, thin ice+thick ice, snow thickness).
!       .   - 2:  add more detailed 2D diagnostics to the 2D diag file,
!          like: solar short wave flux, non solar flux, water flux crossing 
!          the leads+sea ice ensemble...
!       .   - 3: makes the 0d.ins.vairmer diag. file (0D fields). 
!          Contains: sea ice area, extent, volume, for both hemispheres + 
!          transports at most Arctic Straits
!           - Example: cdialev=13 or 31 means that you want only the 2d basic
!          diagnostics + 0d diagnostic.
!       . If cdiafmt='VMAR5' or 'NCAR5'
!           - 1: save only the priority 1 fields
!           - 2: save only the priority 2 fields
!           - 3: save only the priority 3 fields
!           - x: save only my personal fields (additional)
!           - note fields e.g. in priority 2 fields can have a space dimension
!          equal to grid size nxglo*nyglo or equal to 1 
!           - VMAR5: glt_output in Vairmer; NCAR5: glt_output in NetCDF.
!           - Example: cdialev=123x means you want all AR5 fields + yours
!       . If cdiafmt='XIOS' : levels are handled in dedicated diag namelist 
!  - dttave     : period for averaged fields (days, optional, default=365)
!  - navedia    : average the glt_output over dttave and the whole run (N/A for XIOS)
!  - ninsdia    : glt_output delivered once per time step (N/A for XIOS) 
!  - ndiamax    : maximum number of diagnostic files
!  - nsavinp    : allows to save glt_gelato routine input in a file (used in 
!                 coupled mode)
!       nsavinp=0 --> glt_gelato routine input is not saved
!       nsavinp=1 --> glt_gelato routine input is saved in a file
!  - nsavout    : allows to save glt_gelato routine glt_output in a file (used in 
!                 coupled mode)
!       nsavout=0 --> glt_gelato routine input is not saved
!       nsavout=1 --> glt_gelato routine input is saved in a file
!  - nupdbud    : compute budgets (for model energy conservation tests)
!       nupdbud=0  --> no budgets computations (for operational runs)
!       nupdbud=1  --> budgets computations (for model validation)
!  - nprinto    : glt_gelato prints glt_output as follows
!       nprinto=0  --> minimum glt_output print
!       nprinto=1  --> print fields statistics : mini, maxi, av.
!       nprinto=2  --> print fields + field statistics
!  - nprlast    : glt_gelato prints glt_output as nprinto levels (last time step only)
!  - cinsfld    : list of fields to be delivered at every time step
!                 (all -> deliver all fields)
!                 Note that one line per requested field should be given, e.g.:
!                     cinsfld = sit
!                     cinsfld = sic
!                     ...
!
CHARACTER(8) ::  &
  cdiafmt,cdialev
CHARACTER(80), DIMENSION(:), ALLOCATABLE ::  &
  cinsfld
REAL :: &
  dttave
INTEGER ::  &
  navedia, ninsdia, ndiamax, nsavinp,  &
  nsavout, nupdbud, nprinto, nprlast
!
!
! 1.4. Run date position and time step
! ------------------------------------
!
!  - nidate     : initial date for running glt_gelato (-)
!  - niter      : number of iterations from reference date (-)
!  - dtt        : time step for dynamics and thermodynamics (s)
!
INTEGER ::  &
  nidate, niter
REAL ::  &
  dtt
!
!
! 1.5. Number of ice categories
! -----------------------------
!
!  - nt         : number of ice thicknesses (-)
!  - thick      : a vector, whose components define ice classes
! thickness boundaries
!
INTEGER ::  &
  nt
REAL, DIMENSION(:), ALLOCATABLE ::  &
  thick
!
!
!
! 1.6. Number of layers in the ice-snow slab
! ------------------------------------------
!
! .. Number of layers when solving the problem of vertical heat
! diffusion through the ice and snow slab. Note that if the
! scheme is explicit, nslay=1 is compulsory.
!
!  - nilay      : number of ice layers in vertical discretisation (-)
!  - nslay      : number of snow layers in vertical discretisation (-)
!  - xh*        : vertical coordinate parameters
!
INTEGER ::  &
  nilay, nslay
REAL ::  &
  xh0,xh1,xh2,xh3,xh4
!
!
! 1.7. Elastic Viscous-Plastic sea ice rheology parameters
! ---------------------------------------------------------
!
!  - ntstp      : number of dynamics time steps during one
!                 thermodynamics time step.
!  - ndte       : number of subcycles for velocity computations
!                 during sea ice EVP dynamics.
!
INTEGER ::  &
  ntstp, ndte
!
!
! 1.8. Limit Values for sea ice
! ------------------------------
!
!  - xfsimax  : maximum allowable fractional area for sea ice
!  - xicethcr : ice thickness that represents the limit between thin
! and thick ice (m)
!  - xhsimin  : minimum allowable ice thickness

REAL ::  &
  xfsimax, xicethcr, xhsimin
!
!
! 1.9. Parameterizations 
! -----------------------
!
! .. If you need a standard parameterization of low clouds (not simulated
! by your atmosphere model), a reasonable value for this parameter should
! be 0.25. If you don't need this parameterization, use alblc=0.
! (it is not recommended to use values other than 0...)
!  - alblc      : albedo of low clouds
!  - xlmelt     : lateral melting parameterization factor
!  - xswhdfr    : fraction of the solar radiation absorbed by snow that 
! is involved in the vertical heat diffusion (the rest contributes to direct
! warming/melting)
!  - albyngi    : parameterisation of young ice albedo (exponential formulation)
!       albyngi=0.  --> albedo of young ice does not depend on thickness
!       albyngi=1.  --> albedo of young ice depends on thickness
!  - albimlt    : albedo of melting ice
!  - albsmlt    : albedo of melting snow
!  - albsmlt    : albedo of dry snow
!
REAL ::  &
  alblc, xlmelt, xswhdfr, albyngi, albimlt, albsmlt, albsdry
!
!
! 1.10.  Logical units
! ---------------------
!
!  - ngrdlu     : unit for reading the grid
!  - nsavlu     : unit for writing input/output fields for Gelato 
!  - nrstlu     : unit for reading/writing Gelato restart
!  - n0vilu     : unit for writing 0D Glt Instantaneous diags
!  - n0valu     : unit for writing 0D Glt Instantaneous diags
!  - n2vilu     : unit for writing 2D Glt or IPCC-AR5 Instantaneous diags
!  - n2valu     : unit for writing 2D Glt or IPCC-AR5 Averaged diags
!  - nxvilu     : unit for writing Instantaneous additional diags (AR5 case)
!  - nxvalu     : unit for writing Averaged additional diags (AR5 case)
!  - nibglu     : unit for iceberg physics input/output
!  - nspalu     : spare unit for personal use !
!  - noutlu     : unit for glt_gelato glt_output
!  - ntimlu     : unit for glt_gelato gltools_timers
!
INTEGER ::  &
  ngrdlu, nsavlu, nrstlu, n0vilu, n0valu, n2vilu, n2valu,  &
  nxvilu, nxvalu, nibglu, nspalu, noutlu, ntimlu
!
!
! 1.11. Path to keep Gelato I/O fields 
! -------------------------------------
!
! .. You must define this path (complete), but without "/" at the end if
! you want to keep Gelato daily input/output variables (for example to 
! "replay" a simulation with input/output data obtained in coupled mode).
! This variable is used only if nsavinp=1 or nsavout=1.
!
!  - ciopath    : path for input/output fields to glt_gelato routine
!
CHARACTER(80) ::  &
  ciopath
!
!
! 1.12. Parameters read in the namelist (not gltpar)
! ---------------------------------------------------
!  - cn_grdname   : grid name
!  - nn_readf     : are we reading inpfld files as an input
!  - nn_fisrt     : first iteration number 
!  - nn_final     : last iteration number 
!  - nn_step      : step between two iterations
!  - nn_iglo      : number of cells along the x-axis
!  - nn_jglo      : number of cells along the y-axis
!  - nn_perio     : grid periodicity
!  - rn_htopoc    : ocean uppermost grid cell thickness
!
CHARACTER(80) ::  &
  cn_grdname
INTEGER ::  &
  nn_readf, nn_first, nn_final, nn_step, nn_iglo, nn_jglo, nn_perio
REAL ::  &
  rn_htopoc
!
!
! 2. Parameters computed from previous set
! ========================================
!
! 2.1. Vertical grid
! -------------------
!
! .. number of grid points in the ice+snow slab along the vertical
!
INTEGER ::  &
  nl
!
! .. thickness of the different levels (from top to bottom)
!
REAL, DIMENSION(:), ALLOCATABLE ::  &
  sf3t,e3w
!
! .. thickness of the different levels at t points (from bottom to top)
!
REAL, DIMENSION(:), ALLOCATABLE ::  &
  sf3tinv
!
! .. depth of the different inter-levels (from top to bottom)
!
REAL, DIMENSION(:), ALLOCATABLE ::  &
  depth
!
! .. height of the different inter-levels (from bottom to top)
!
REAL, DIMENSION(:), ALLOCATABLE ::  &
  height
!
!
! 2.2. Diagnostics options
! -------------------------
!
INTEGER ::  &
  ndiap1, ndiap2, ndiap3, ndiapx
!
!
! 2.3. Parameters for EVP sea ice dynamics model
! -----------------------------------------------
!
! .. Full horizontal grid size (equivalent to NEMO's jpiglo,jpjglo)
!
INTEGER ::  &
  nxglo,nyglo
!
! .. Subdomain dimensions (for advection)
!
INTEGER ::  &
  imt_local,jmt_local,ilo,jlo,ihi,jhi
!
! .. Number of categories (advection)
!
INTEGER ::  &
  ncat
!
! .. Number of ice layers (advection)
!
INTEGER ::  &
  nilyr
!
! .. Number of categories by number of ice layers (advection)
!
INTEGER ::  &
  ntilay
!
! .. Number of advected tracers
!
INTEGER ::  &
  na
!
!
! 
! 3. Parameters deduced from other initializations
! =================================================
!
! .. For use within Surfex
!
INTEGER ::  &
  nsurfex
!
! .. Total size of the reduced grid  ( defined at every time step)
!
INTEGER ::  &
  npt
!
! .. Per process size of the reduced grid (defined at every time step)
!
INTEGER ::  &
  np
!
! .. Number of categories considered in observations towards which damping is
! applied
!
INTEGER ::  &
  ntd
! .. Surface of the ocean sub-domain (never changes, computed in bnddmn)
!
REAL ::  &
  xdomsrf
!
! .. Total surface of the ocean domain (never changes, computed in bnddmn)
!
REAL ::  &
  xdomsrf_g
!
! .. Total surface of the reduced grid ocean domain (redefined at every 
! time step in thermo)
!
REAL ::  &
  xdomsrf_r
!
! .. One input non-solar forcing flux per ice category (nt) or one input
! non-solar flux to be shared between all the categories (1)
!
INTEGER ::  &
  nnflxin
!
!
! 
! 4. Parameters related to multi-processing 
! ==========================================
!
! .. Are we multi-processed (using MPI)
LOGICAL :: &
     lmpp
! .. Do we print output file (gltout)
LOGICAL :: &
     lwg
! .. Print levels in gltout
LOGICAL :: &
     lp1,lp2,lp3,lp4,lp5
! .. What is the mpi multi-processing channel number 
INTEGER ::  &
     gelato_communicator
! .. Which are the leading process number, the current process number 
!    the total number of procs for Gelato and OPA
INTEGER ::  &
     gelato_leadproc, gelato_myrank, gelato_nprocs
! .. 2D subdomains definition
! X size of current 2D subdomain
INTEGER ::  &
     nx
! Y size of current 2D subdomain
INTEGER ::  &
     ny
! Array of all X sizes of 2D subdomains (defined only at first time step)
! This is the equivalent of nlcit(:) in NEMO
INTEGER, DIMENSION(:), ALLOCATABLE ::  &
     nxtab
! Array of all Y sizes of 2D subdomains (defined only at first time step)
! This is the equivalent of nlcjt(:) in NEMO
INTEGER, DIMENSION(:), ALLOCATABLE ::  &
     nytab
! Index number of every grid point in the global grid (i,j)
INTEGER, DIMENSION(:,:), ALLOCATABLE ::  &    
     nindi,nindj
!
!
!
! 5. Parameters related to time
! ==============================
!
INTEGER ::  &
  ntimnum
REAL ::  &
  xtime
CHARACTER(80) ::  &
  clabel 
!
END MODULE modd_glt_param
!
! ------------------------ END MODULE modd_glt_param ------------------------
! -----------------------------------------------------------------------
