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
! ======================================================================
!  This module contains all the parameters used by the thermodynamics
!  module of GELATO.
! ======================================================================

MODULE modd_glt_const_thm 
!
IMPLICIT NONE
!
!
! 1. PRESCRIBED PARAMETERS 
! =========================
!
! 1.1. Various constants (time, units, math...)
! ----------------------------------------------
! 
! Pi
    REAL, PARAMETER ::  &
        pi = 3.141592653789     ! -
! First tolerance parameter  
    REAL, PARAMETER ::  &
        epsil1 = 1.E-10         ! -
! Second tolerance parameter
    REAL, PARAMETER ::  &
        epsil2 = 1.E-20         ! -
! Second tolerance parameter
    REAL, PARAMETER ::  &
        epsil5 = 1.E-05         ! -
! Standard large number
    REAL, PARAMETER ::  &
        xbig19 = 1.E19          ! -
! Standard large number
    REAL, PARAMETER ::  &
        xbig20 = 1.E20          ! -
! Number of seconds in one day
    REAL, PARAMETER ::  &
        xday2sec = 86400.       ! -
! Number of days in one year   
    REAL, PARAMETER ::  &
        xyear2day = 365.25      ! -
! Number of seconds in one month
    REAL, PARAMETER ::  &
        xmonth2sec = xyear2day/12.*xday2sec
! Factor to convert mm to m
    REAL, PARAMETER ::  &
        xm2mm = 1000.
!
!
! 1.2. Sea water density function parameters
! -------------------------------------------
!
REAL, PARAMETER ::  &
  c1 = -7.2169e-2,  &
  c2 = 4.9762e-2,  &
  c3 = 8.0560e-1,  &
  c4 = -7.5911e-3,  &
  c5 = -3.0063e-3,  &
  c6 = 3.5187e-5,  &
  c7 = 3.7297e-5
!
!
! 1.3. Thermodynamical constants
! -------------------------------
!
! Albedo of dry, bare ice ( hsi <=hsicr ) 
!   - we use a formula adapted from Flato and Brown, JGR (1996)
! (the threshold is just computed in glt_updasn_r routine)
!      alb = xalf1 * hsi^xpow + albw
    REAL, PARAMETER ::  &
        xalf1 = 0.55, xpow = 0.28
! Albedo of dry, bare ice ( hsi > hsicr )
    REAL, PARAMETER ::   &
        albi = .71              ! -
! Albedo of melting sea ice
!    REAL, PARAMETER ::  & 
!        albimlt = .50           ! -
! Albedo over melting snow (Curry et al., JGR-A, 2001)
!    REAL, PARAMETER ::  &
!        albsmlt = .77           ! -
! Albedo over dry snow (Curry et al., JGR-A, 2001)
!    REAL, PARAMETER ::  &
!        albsdry = .84           ! -
! Albedo over water
    REAL, PARAMETER ::  &
        albw = .065              ! -
! Specific heat of pure ice
    REAL, PARAMETER ::  &
        cpice0 = 2080.          ! J/(K.kg)
! Specific heat of sea water
    REAL, PARAMETER ::  &
        cpsw = 3987.            ! J/(K.kg)
! Massic heat of fusion of pure ice
    REAL, PARAMETER ::  &
        xmhofusn0 = 3.3355E+05  ! J/kg
! Beer Lambert's law attenuation coefficient (Ebert et al., JGR-O, 1995)
    REAL, PARAMETER ::  &
        kappa = 1.4             !
! Lateral melting rate (Maykut, 1987) : Mr = m1 * (tlead - t_f)**m2 
    REAL, PARAMETER ::  &
        xm1 = 1.6E-06           ! m.s^-1.K^-1.36
    REAL, PARAMETER ::  &
        xm2 = 1.36              ! -
! Density of fresh water
    REAL, PARAMETER ::  &
        rhofw = 1000.           ! kg/m^3
! Density of pure ice
    REAL, PARAMETER ::  &
        rhoice0 = 917.           ! kg/m^3
! Average density of sea ice
    REAL, PARAMETER ::  &
        rhoice = 910.           ! kg/m^3
! Maximum density for old snow
    REAL, PARAMETER ::  &
        rhosnwmax = 300.        ! kg/m^3 
! Density of fresh snow
    REAL, PARAMETER ::  &
        rhosnwmin = 100.        ! kg/m^3
! Ice conductivity (if function of T and S)
!   - we use a parameterization by Pringle et al., JGR (2007) 
! Original parameterization:
!   rkice(T,S) = rho_sea_ice / rho_pure_ice * ( xrki1 + xrki2*T + xrki3*S/T )
! (up to T=-1.8)
! In Gelato:
!   - the density of pure ice is rhoice0
!   - the density of sea ice is constant ( rhoice )
!   - for T>Tempc, Tempc=-1.8, rkice(T,S) = rkice(Tempc,S)
    REAL, PARAMETER ::  &
        xrki1 = 2.11, xrki2 = -0.011, xrki3 = 0.09, xtempc = -1.8
! Ice conductivity (if assumed to be a constant)
    REAL, PARAMETER ::  &
        rkice0 = 2.04            ! W/(m.K)
! Salt concentration per mil for sea ice (taken as constant here)
    REAL, PARAMETER ::  &
        sice = 4.               ! -
! Salinity of ice formed from sea water at standard salt concentration
! (in case the salinity scheme is not used)
    REAL, PARAMETER ::  &
        ssinew = 5.             ! -
! Standard salinity of sea water
    REAL, PARAMETER ::  &
        ssw0 = 34.              ! -
! Summer salinity equilibrium
    REAL, PARAMETER ::  &
        ssisummer0 = 2.         ! psu
! Winter salinity equilibrium
    REAL, PARAMETER ::  &
        ssiwinter0 = 5.         ! psu
! Summer salinity decrease timescale
    REAL, PARAMETER ::  &
        ssisummer_ts = 10.      ! days
! Winter salinity decrease timescale
    REAL, PARAMETER ::  &
        ssiwinter_ts = 20.      ! days
! Linear decay coef. for snow albedo
    REAL, PARAMETER ::  &
        taua = 0.008            ! -
! Exponential decay coef. for snow albedo
    REAL, PARAMETER ::  &
        tauf = 0.24             ! -
! Melting point (deg C) over sea ice salinity ratio
    REAL, PARAMETER ::  &
        mu = 0.054              ! deg C/psu
! 0 C in K
    REAL, PARAMETER ::  &
        t0deg = 273.15          ! K
! Standard melting point of ice
    REAL, PARAMETER ::  &
        tice_m = -mu*sice       ! K
! Minimum snowfall that allows to cover a sea ice slab totally
    REAL, PARAMETER ::  &
        wnew = 5.E-2            ! m
! Concentration threshold to define sea ice extension 
    REAL, PARAMETER ::  &
        xfsic = 0.15            ! [0-1]
! Concentration threshold to define sea ice existence
    REAL, PARAMETER ::  &
        xiok = 1.e-4            ! [0-1]
!
! 1.4. Parameters for melt ponds (CICE configuration)
! ---------------------------------------------------
! Albedo of melting bare ice
    REAL, PARAMETER ::  &
        xalbareimlt = 0.65      ! [0-1]
! Fraction of melt water captured by the ponds
    REAL, PARAMETER ::  &
        xr1 = 0.16               ! [0-1]
! Melt pond shrinkage exponential parameter
    REAL, PARAMETER ::  &
        xr2 = 0.03               ! -
! Melt pond reference temperature
    REAL, PARAMETER ::  &
        tp = -2.                ! C
! Ratio melt pond depth / melt pond fraction 
    REAL, PARAMETER ::  &
        dpthfrac = 0.8          ! -
! Parameters for melt ponds depth/fraction relation
!   depth = dptfr1 * fraction + dptfr2
    REAL, PARAMETER ::  &
        dptfr1 = 0.8, dptfr2 = -1.E-4
!        dptfr1 = 1.16, dptfr2 = -0.09
! Maximum ratio melt pond depth / ice thickness 
    REAL, PARAMETER ::  &
        dpthhi = 0.9            ! -
! Drainage rate
    REAL, PARAMETER ::  &
        drainrate = 0. !0.8E-2          ! m/day
! Minimum sea ice thickness allowing melt pond formation
    REAL, PARAMETER ::  &
        hsi_mp = 0.10           ! m
! Maximum snow depth allowing melt pond formation
    REAL, PARAMETER ::  &
        hsn_mp = 0.01 !1E-07 !0.189          ! m
!
! Weights for albedo calculation (following Ebert and Curry, 1993)
    REAL, PARAMETER :: &
        xwmp1=0.519, &
        xwmp2=0.337, &
        xwmp3=0.134, &
        xwmp4=0.010
! Parameters for albedo formulation (alb = amp + exp(-bmp*d-cmp))
     REAL, PARAMETER :: &
        xamp1=0.150, &
        xamp2=0.054,  &
        xamp3=0.033,  &
        xamp4=0.030, &
        xbmp1=8.1, &
        xbmp2=31.8,  &
        xbmp3=2.6, &
        xcmp1=0.47, &
        xcmp2=0.94, &
        xcmp3= 3.82
!
!
! 2. PARAMETERS DEDUCED FROM THE PREVIOUS SET
! ============================================

! Volumic latent heat of fusion of pure ice
    REAL, PARAMETER ::  &
        hofusn0 = xmhofusn0 * rhoice0   ! J.m-3
! The inverse of the latent heat of fusion of ice
    REAL, PARAMETER ::  &
        hofusni0 = 1. / hofusn0         ! m3.J-1
! Product of rhoice and cpice0 (pure ice)
    REAL, PARAMETER ::  &
        rhocpice0 = rhoice0*cpice0      ! J.m-3.K-1
! Standard density of sea water
    REAL, PARAMETER ::  &
        rhosw = rhofw + ssw0            ! kg/m^3
!
END MODULE modd_glt_const_thm 
