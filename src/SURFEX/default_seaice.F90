!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE DEFAULT_SEAICE(HPROGRAM,                                   &
                          HINTERPOL_SIC, HINTERPOL_SIT, PFREEZING_SST,&
                          PSEAICE_TSTEP, PSIC_EFOLDING_TIME,          &
                          PSIT_EFOLDING_TIME, PCD_ICE, PSI_FLX_DRV    )  
!     ########################################################################
!
!!****  *DEFAULT_SEAICE* - routine to set default values for the configuration for SEAICE scheme
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!       For now, only Gelato seaice scheme is handled
!!
!!       We do use MODD_GLT_PARAM, for modifying its values, in order to 
!!       avoid duplicating code with Gelato sources
!!
!!       We set all its parameters to values which are sensible in Surfex context
!!       This is done by inserting a relevant 'gltpar' file as source code, and 
!!       changing a few values (we used a Glt 6.0.36 version, initially)
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      S.Senesi   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2014 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODI_GET_LUOUT
!
USE MODD_GLT_PARAM,  ONLY :  nmkinit, nrstout, nrstgl4, nthermo, ndynami, nadvect, ntimers, &
     ndyncor, ncdlssh, niceage, nicesal, nmponds, nsnwrad, nleviti, nsalflx, nextqoc,       &
     nicesub, cnflxin, cfsidmp, xfsidmpeft, chsidmp, xhsidmpeft,                            &
     cdiafmt, cdialev, dttave , navedia, ninsdia, ndiamax, nsavinp,                         &
     nsavout, nupdbud, nprinto, nprlast, cn_grdname, rn_htopoc, nidate , niter,             &
     dtt, nt, thick, nilay, nslay, xh0 , xh1 , xh2 , xh3 , xh4 , ntstp , ndte  , xfsimax,   &
     xicethcr, xhsimin, alblc , xlmelt , xswhdfr, albyngi, albimlt, albsmlt, albsdry,ngrdlu,&
     nsavlu,  nrstlu , n0vilu , n0valu , n2vilu , n2valu , nxvilu , nxvalu , nibglu ,       &
     nspalu , noutlu , ntimlu , ciopath,                                                    &
     gelato_leadproc, gelato_myrank, lwg, nnflxin, ntd
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO 
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling ISBA
CHARACTER(LEN=6),  INTENT(OUT) :: HINTERPOL_SIC ! Quadratic interpolation of monthly SIC
CHARACTER(LEN=6),  INTENT(OUT) :: HINTERPOL_SIT ! Quadratic interpolation of monthly SIT
REAL,              INTENT(OUT) :: PFREEZING_SST ! Value marking frozen sea in SST data
REAL,              INTENT(OUT) :: PSEAICE_TSTEP ! For damping of SIC (days)
REAL,              INTENT(OUT) :: PSIC_EFOLDING_TIME ! E-folding time on SIC relaxation
REAL,              INTENT(OUT) :: PSIT_EFOLDING_TIME ! E-folding time on SIT relaxation
REAL,              INTENT(OUT) :: PCD_ICE       ! turbulent exchanges transfer coefficient on seaice
REAL,              INTENT(OUT) :: PSI_FLX_DRV   ! turbulent exchanges transfer coefficient on seaice

!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
INTEGER           :: ILUOUT         ! logical unit of output file
integer jl
real zjl
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_SEAICE',0,ZHOOK_HANDLE)
!
CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
HINTERPOL_SIC = "NONE"
HINTERPOL_SIT = "NONE"
PFREEZING_SST = -1.8 ! Celsius degree
PSEAICE_TSTEP = XUNDEF
PSIC_EFOLDING_TIME = 0 ! in days; 0 means no relaxation
PSIT_EFOLDING_TIME = 0 ! in days; 0 means no relaxation
PCD_ICE       = 0.0
PSI_FLX_DRV   = -20.
!
! Even if default case is to avoid using a seaice scheme, we set 
! default Gelato seaice model parameters
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Setting those Gelato parameters which are not usually set by an 
! external file but by the gelato library caller program
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! .. Number of categories considered in observations towards which damping is
! applied
ntd=1
! .. One input non-solar forcing flux per ice category (nt) or one input
! non-solar flux to be shared between all the categories (1)
nnflxin=1
! .. Which is the leading process number (useless now ?)
gelato_leadproc=0
! Adapt proc number and print flags to the Surfex proc numbering scheme
gelato_myrank=nrank
lwg=(nrank == npio)


! Setting those Gelato parameters which are usually set by an 
! external file (gltpar)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  
!  Gelato model parameters
! =========================
!
! .. These parameters can be (theoretically) freely changed by the user
!
!
! 1. Options to run GELATO
! -------------------------
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
!  - nadvect    : disable/enable ice transport
!       nadvect=0  --> no ice transport
!       nadvect=1  --> ice transport enabled
!  - ntimers    : disable/enable timers
!       ntimers=0  --> no timers
!       ntimers=1  --> timers enabled
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
!                 (if 2 or 3, check ocean topmost level dz parameter xhtopoc !)
!       nsalflx=1  --> approximated calculation
!       nsalflx=2  --> exact calculation
!       nsalflx=3  --> exact calculation, but SSS replaced with standard sal.
!       nsalflx=4  --> simplified calculation as in LIM2 (fixed ocean and ice reference salinity)
!  - nextqoc    : ocean-ice heat flux
!       nextqoc=1  --> the %qoc given as an input is taken into account
!       nextqoc=0  --> the %qoc is computed by Gelato
!  - nicesub    : ice sublimation
!       nicesub=1  --> take ice sublimation into account (non heat conservative)
!       nicesub=0  --> no ice sublimation
!  - cnflxin    : input fluxes 
!       cnflxin    --> 'mixed' : only one flux, to share between water/ice
!       cnflxin    --> 'double': one flux for water, one flux for ice
!       cnflxin    --> 'multi' : one flux for water, one flux for each ice cat
!
nmkinit = 0
nrstout = 0
nrstgl4 = 1
nthermo = 1
ndynami = 0
nadvect = 0
ntimers = 0
ndyncor = 0
ncdlssh = 1
niceage = 1
nicesal = 1
nmponds = 1
nsnwrad = 1
nleviti = 1
nsalflx = 2
nextqoc = 0
nicesub = 1
cnflxin = 'double'
!
!
! 2. Damping and restoring
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
cfsidmp='NONE'
xfsidmpeft=0.
chsidmp='NONE'
xhsidmpeft=0.
!
!
! 3. Diagnostics output
! ----------------------
!
!  - cdiafmt    : diagnostics format
!       cdiafmt='GELATO'  --> Gelato Vairmer format
!       cdiafmt='VMAR5'   --> IPCC AR5 vairmer format
!       cdiafmt='NCAR5'   --> IPCC AR5 NetCDF format (not active yet)
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
!           - VMAR5: output in Vairmer; NCAR5: output in NetCDF.
!           - Example: cdialev=123x means you want all AR5 fields + yours
!  - dttave     : period for averaged fields (days, optional, default=365)
!  - navedia    : average the output over dttave and over the whole run
!  - ninsdia    : output delivered once per time step
!  - ndiamax    : maximum number of diagnostic files
!  - nsavinp    : allows to save gelato routine input in a file (used in 
!                 coupled mode)
!       nsavinp=0 --> gelato routine input is not saved
!       nsavinp=1 --> gelato routine input is saved in a file
!  - nsavout    : allows to save gelato routine output in a file (used in 
!                 coupled mode)
!       nsavout=0 --> gelato routine input is not saved
!       nsavout=1 --> gelato routine input is saved in a file
!  - nupdbud    : compute budgets (for model energy conservation tests)
!       nupdbud=0  --> no budgets computations (for operational runs)
!       nupdbud=1  --> budgets computations (for model validation)
!  - nprinto    : GELATO prints output as follows
!       nprinto=0  --> minimum output print
!       nprinto=1  --> print fields statistics : mini, maxi, av.
!       nprinto=2  --> print fields + field statistics
!  - nprlast    : GELATO prints output as nprinto levels (last time step only)
!  - cinsfld    : list of fields to be delivered at every time step
!                 (all -> deliver all fields)
!                 Note that one line per requested field should be given, e.g.:
!                     cinsfld = sit
!                     cinsfld = sic
!                     ...
!
cdiafmt = 'VMAR5'
cdialev = ''
dttave = 30.
navedia = 0
ninsdia = 0
ndiamax = 90
nsavinp = 0
nsavout = 0
nupdbud = 0
nprinto = 0
nprlast = 0
!
!
! 4. Grid definition
! -------------------
!
!  - cn_grdname   : grid name radical. Defined the grid you are running on.  
!         . Available (pre-coded) options are : 
!         'OPAG8', 'NEMO1', 'ORCA2' or 'MICOM'.
!         For these precoded grids, any nbndco, nxglo and nyglo values you 
!         will specify in gltpar will be ignored by the code.
!         . You may specify another cgrdname, but then the nbndco, nxglo
!         and nyglo values you provide in gltpar MUST MAKE SENSE and will 
!         be taken into account. 
!  - rn_htopoc    : reference thickness (in m) of the topmost ocean level 
!          . This is important if Gelato is coupled to an ocean model, to
!          send the right concentration / dilution flux to the ocean.
!
cn_grdname = 'SURFEX'
rn_htopoc = 10.
!
!
! 5. Run date position and time step
! -----------------------------------
!
!  - nidate     : initial date for running GELATO, YYYYMMDD (-)
!  - niter      : number of iterations from reference date (-)
!  - dtt        : time step for dynamics and thermodynamics (s)
!
nidate = 20010101
niter = 100000
dtt = XUNDEF  ! means : same time step as seaflux
!
!
! 6. Number of ice categories
! ----------------------------
!
!  - nt         : number of ice thicknesses (-)
!  - thick      : boundaries for thickness categories (-)
!
nt = 1
IF (ALLOCATED(thick)) DEALLOCATE( thick )
ALLOCATE( thick(nt+1) )
thick(1)= -.01 
thick(2) = 1000.
!
!
! 7. Number of layers in the ice-snow slab
! -----------------------------------------
!
! .. Number of layers when solving the problem of vertical heat
! diffusion through the ice and snow slab. Note that if the
! scheme is explicit, nslay=1 is compulsory.
!
!  - nilay      : number of ice layers in vertical discretisation (-)
!  - nslay      : number of snow layers in vertical discretisation (-)
!  - xh*        : vertical coordinate parameters
!  If you need to run the model with constant vertical levels
!  (not recommended), specify xh1=1. and xh2=0.
!
nilay = 9
nslay = 1
xh0 = 4.392339514718992e-01
xh1 = 1.049607477174487e-01
xh2 = 9.507487632412231e-02
xh3 = 1.
xh4 = 5.208820443636069
!
!
! 8. Elastic Viscous-Plastic sea ice rheology parameters
! -------------------------------------------------------
!
!  - ntstp      : number of dynamics time steps during one
!                 thermodynamics time step.
!  - ndte       : number of subcycles for velocity computations
!                 during sea ice EVP dynamics.
!
ntstp = 1
ndte = 100
!
!
! 9. Limit Values for sea ice
! ----------------------------
!
!  - xfsimax  : maximum allowable fractional area for sea ice
!  - xicethcr : ice thickness that represents the limit between thin
! and thick ice (m)
!  - xhsimin  : minimum allowable ice thickness
!
xfsimax = .995
xicethcr = .8
xhsimin = .2
!
!
! 10.  Parameterizations
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
!  - albsdry    : albedo of dry snow
!
alblc = 0.
xlmelt = 3.e-3
xswhdfr = 1.00
albyngi = 1.
albimlt = 0.56
albsmlt = 0.77
albsdry = 0.84
!
!
! 11.  Logical units
! -------------------
!
!  - ngrdlu     : unit for reading the grid
!  - nsavlu     : unit for writing input/output fields for Gelato 
!  - nrstlu     : unit for reading/writing Gelato restart
!  - n0vilu     : unit for writing 0D Glt Instantaneous diags
!  - n0valu     : unit for writing 0D Glt Averaged diags
!  - n2vilu     : unit for writing 2D Glt or IPCC-AR5 Instantaneous diags
!  - n2valu     : unit for writing 2D Glt or IPCC-AR5 Averaged diags
!  - nxvilu     : unit for writing Instantaneous additional diags (AR5 case)
!  - nxvalu     : unit for writing Averaged additional diags (AR5 case)
!  - nibglu     : unit for iceberg physics input/output
!  - nspalu     : spare unit for personal use !
!  - noutlu     : unit for GELATO output
!  - ntimlu     : unit for GELATO timers
!
ngrdlu = 153
nsavlu = 111
nrstlu = 151
n0vilu = 123
n0valu = 125
n2vilu = 121
n2valu = 122
nxvilu = 133
nxvalu = 131
nibglu = 120
nspalu = 130
noutlu = ILUOUT
ntimlu = 201
!
!
! 12. Path to keep Gelato I/O fields
! -----------------------------------
!
! .. You must define this path (complete), but without "/" at the end if
! you want to keep Gelato daily input/output variables (for example to
! "replay" a simulation with input/output data obtained in coupled mode).
! This variable is used only if nsavinp=1 or nsavout=1.
!
!  - ciopath    : path for input/output fields to gelato routine
!
ciopath = '.'

IF (LHOOK) CALL DR_HOOK('DEFAULT_SEAICE',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DEFAULT_SEAICE
