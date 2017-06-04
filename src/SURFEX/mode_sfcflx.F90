!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
! File %M% from Library %Q%
! Version %I% from %G% extracted: %H%
!------------------------------------------------------------------------------

MODULE mode_sfcflx

!------------------------------------------------------------------------------
!
! Description:
!
!  The main program unit of 
!  the atmospheric surface-layer parameterization scheme "sfcflx".
!  "sfcflx" is used to compute fluxes 
!  of momentum and of sensible and latent heat over lakes.
!  The surface-layer scheme developed by Mironov (1991) was used as the starting point.
!  It was modified and further developed to incorporate new results as to 
!  the roughness lenghts for scalar quantities,
!  heat and mass transfer in free convection,
!  and the effect of limited fetch on the momentum transfer.
!  Apart from the momentum flux and sensible and latent heat fluxes,
!  the long-wave radiation flux from the water surface and
!  the long-wave radiation flux from the atmosphere can also be computed.
!  The atmospheric long-wave radiation flux is computed with simple empirical formulae,
!  where the atmospheric emissivity is taken to be dependent on 
!  the water vapour pressure and cloud fraction.
!
!  A description of sfcflx is available from the author.
!  Dmitrii Mironov 
!  German Weather Service, Kaiserleistr. 29/35, D-63067 Offenbach am Main, Germany. 
!  dmitrii.mironov@dwd.de 
!
!  Lines embraced with "!_tmp" contain temporary parts of the code.
!  Lines embraced/marked with "!_dev" may be replaced
!  as improved parameterizations are developed and tested.
!  Lines embraced/marked with "!_dm" are DM's comments
!  that may be helpful to a user.
!  Lines embraced/marked with "!_dbg" are used
!  for debugging purposes only.
!
!
! Current Code Owner: DWD, Dmitrii Mironov
!  Phone:  +49-69-8062 2705
!  Fax:    +49-69-8062 3721
!  E-mail: dmitrii.mironov@dwd.de
!
! History:
! Version    Date       Name
! ---------- ---------- ----
! 1.00       2005/11/17 Dmitrii Mironov 
!  Initial release
! !VERSION!  !DATE!     <Your name>
!  <Modification comments>
!
! Code Description:
! Language: Fortran 90.
! Software Standards: "European Standards for Writing and
! Documenting Exchangeable Fortran 90 Code".
!==============================================================================
!
! Declarations:
!
! Modules used:

!USE modd_data_parameters  , ONLY :   &
!    ireals                      ,  &! KIND-type parameter for real variables
!    iintegers                       ! KIND-type parameter for "normal" integer variables  

USE modd_flake_parameters , ONLY :   &
    tpl_grav                    ,  &! Acceleration due to gravity [m s^{-2}]
    tpl_T_f                     ,  &! Fresh water freezing point [K]
    tpl_rho_w_r                 ,  &! Maximum density of fresh water [kg m^{-3}]
    tpl_c_w                     ,  &! Specific heat of water [J kg^{-1} K^{-1}]
    tpl_L_f                     ,  &! Latent heat of fusion [J kg^{-1}]
    h_Ice_min_flk                   ! Minimum ice thickness [m]  

!==============================================================================

!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

!==============================================================================
!
! Declarations

!  Dimensionless constants in the Monin-Obukhov surface-layer 
!  similarity relations and in the expressions for the roughness lengths.
REAL , PARAMETER ::   &
    c_Karman      = 0.40      ,  &! The von Karman constant 
    Pr_neutral    = 1.0       ,  &! Turbulent Prandtl number at neutral static stability
    Sc_neutral    = 1.0       ,  &! Turbulent Schmidt number at neutral static stability
    c_MO_u_stab   = 5.0       ,  &! Constant of the MO theory (wind, stable stratification)
    c_MO_t_stab   = 5.0       ,  &! Constant of the MO theory (temperature, stable stratification)
    c_MO_q_stab   = 5.0       ,  &! Constant of the MO theory (humidity, stable stratification)
    c_MO_u_conv   = 15.0      ,  &! Constant of the MO theory (wind, convection)
    c_MO_t_conv   = 15.0      ,  &! Constant of the MO theory (temperature, convection)
    c_MO_q_conv   = 15.0      ,  &! Constant of the MO theory (humidity, convection)
    c_MO_u_exp    = 0.25      ,  &! Constant of the MO theory (wind, exponent)
    c_MO_t_exp    = 0.5       ,  &! Constant of the MO theory (temperature, exponent)
    c_MO_q_exp    = 0.5       ,  &! Constant of the MO theory (humidity, exponent)
    z0u_ice_rough = 1.0E-03   ,  &! Aerodynamic roughness of the ice surface [m] (rough flow)
    c_z0u_smooth  = 0.1       ,  &! Constant in the expression for z0u (smooth flow) 
    c_z0u_rough   = 1.23E-02  ,  &! The Charnock constant in the expression for z0u (rough flow)
    c_z0u_rough_L = 1.00E-01  ,  &! An increased Charnock constant (used as the upper limit)
    c_z0u_ftch_f  = 0.70      ,  &! Factor in the expression for fetch-dependent Charnock parameter
    c_z0u_ftch_ex = 0.3333333 ,  &! Exponent in the expression for fetch-dependent Charnock parameter
    c_z0t_rough_1 = 4.0       ,  &! Constant in the expression for z0t (factor) 
    c_z0t_rough_2 = 3.2       ,  &! Constant in the expression for z0t (factor)
    c_z0t_rough_3 = 0.5       ,  &! Constant in the expression for z0t (exponent) 
    c_z0q_rough_1 = 4.0       ,  &! Constant in the expression for z0q (factor)
    c_z0q_rough_2 = 4.2       ,  &! Constant in the expression for z0q (factor)
    c_z0q_rough_3 = 0.5       ,  &! Constant in the expression for z0q (exponent)
    c_z0t_ice_b0s = 1.250     ,  &! Constant in the expression for z0t over ice
    c_z0t_ice_b0t = 0.149     ,  &! Constant in the expression for z0t over ice
    c_z0t_ice_b1t = -0.550    ,  &! Constant in the expression for z0t over ice
    c_z0t_ice_b0r = 0.317     ,  &! Constant in the expression for z0t over ice
    c_z0t_ice_b1r = -0.565    ,  &! Constant in the expression for z0t over ice
    c_z0t_ice_b2r = -0.183    ,  &! Constant in the expression for z0t over ice
    c_z0q_ice_b0s = 1.610     ,  &! Constant in the expression for z0q over ice
    c_z0q_ice_b0t = 0.351     ,  &! Constant in the expression for z0q over ice
    c_z0q_ice_b1t = -0.628    ,  &! Constant in the expression for z0q over ice
    c_z0q_ice_b0r = 0.396     ,  &! Constant in the expression for z0q over ice
    c_z0q_ice_b1r = -0.512    ,  &! Constant in the expression for z0q over ice
    c_z0q_ice_b2r = -0.180    ,  &! Constant in the expression for z0q over ice
    Re_z0s_ice_t  = 2.5       ,  &! Threshold value of the surface Reynolds number 
                                       ! used to compute z0t and z0q over ice (Andreas 2002)
    Re_z0u_thresh = 0.1           ! Threshold value of the roughness Reynolds number   
                                       ! [value from Zilitinkevich, Grachev, and Fairall (200),
                                       ! currently not used] 

!  Dimensionless constants 
REAL , PARAMETER ::   &
    c_free_conv   = 0.14          ! Constant in the expressions for fluxes in free convection  

!  Dimensionless constants 
REAL , PARAMETER ::   &
    c_lwrad_emis  = 0.99          ! Surface emissivity with respect to the long-wave radiation  

!  Thermodynamic parameters
REAL , PARAMETER ::        &
    tpsf_C_StefBoltz = 5.67E-08    ,  &! The Stefan-Boltzmann constant [W m^{-2} K^{-4}]
    tpsf_R_dryair    = 2.8705E+02  ,  &! Gas constant for dry air [J kg^{-1} K^{-1}]
    tpsf_R_watvap    = 4.6151E+02  ,  &! Gas constant for water vapour [J kg^{-1} K^{-1}]
    tpsf_c_a_p       = 1.005E+03   ,  &! Specific heat of air at constant pressure [J kg^{-1} K^{-1}]
    tpsf_L_evap      = 2.501E+06   ,  &! Specific heat of evaporation [J kg^{-1}]
    tpsf_nu_u_a      = 1.50E-05    ,  &! Kinematic molecular viscosity of air [m^{2} s^{-1}]
    tpsf_kappa_t_a   = 2.20E-05    ,  &! Molecular temperature conductivity of air [m^{2} s^{-1}]
    tpsf_kappa_q_a   = 2.40E-05        ! Molecular diffusivity of air for water vapour [m^{2} s^{-1}]  

!  Derived thermodynamic parameters
REAL , PARAMETER ::                        &
    tpsf_Rd_o_Rv  = tpsf_R_dryair/tpsf_R_watvap           ,  &! Ratio of gas constants (Rd/Rv)
    tpsf_alpha_q  = (1.-tpsf_Rd_o_Rv)/tpsf_Rd_o_Rv     ! Diemsnionless ratio   

!  Thermodynamic parameters
REAL , PARAMETER ::     &
    P_a_ref             = 1.0E+05   ! Reference pressure [N m^{-2} = kg m^{-1} s^{-2}]  


!  The variables declared below
!  are accessible to all program units of the MODULE "sfcflx"
!  and to the driving routines that use "sfcflx".
!  These are basically the quantities computed by sfcflx.
!  Apart from these quantities, there a few local scalars 
!  used by sfcflx routines mainly for security reasons.
!  All variables declared below have a suffix "sf".

!  sfcflx variables of type REAL

!RJ: provide default unreasonable value to init for 'ifort -fpic -openmp', to avoid ICE
REAL,PARAMETER,PRIVATE :: Z_=-HUGE(0.0)

!  Roughness lengths
REAL  ::    &
    z0u_sf=Z_                 ,  &! Roughness length with respect to wind velocity [m]
    z0t_sf=Z_                 ,  &! Roughness length with respect to potential temperature [m]
    z0q_sf=Z_                     ! Roughness length with respect to specific humidity [m]  
!  Security constants
REAL , PARAMETER ::   &
    u_wind_min_sf  = 1.0E-02  ,  &! Minimum wind speed [m s^{-1}]
    u_star_min_sf  = 1.0E-04  ,  &! Minimum value of friction velocity [m s^{-1}]
    z0t_min_sf     = 1.0E-11  ,  &! Minimum value of thermal roughness length [m s^{-1}]
    c_accur_sf     = 1.0E-07  ,  &! A small number (accuracy)
    c_small_sf     = 1.0E-04      ! A small number (used to compute fluxes)  

!  Useful constants
REAL , PARAMETER ::     &
    num_1o3_sf = 1./3.       ! 1/3  

!==============================================================================
! Procedures 
!==============================================================================

CONTAINS

!==============================================================================
!  The codes of the sfcflx procedures are stored in separate "*.incf" files
!  and are included below.
!------------------------------------------------------------------------------

!==============================================================================
! For SURFEX needs, separate *.incf files are explicitly expanded
!==============================================================================
!SURFEX include 'sfcflx_lwradatm.incf'
! File %M% from Library %Q%
! Version %I% from %G% extracted: %H%
!------------------------------------------------------------------------------

!SURFEX REAL  FUNCTION sfcflx_lwradatm (T_a, e_a, cl_tot, cl_low)
FUNCTION sfcflx_lwradatm (T_a, e_a, cl_tot, cl_low)

!------------------------------------------------------------------------------
!
! Description:
!
!  Computes the long-wave radiation flux from the atmosphere
!  as function of air temperature, water vapour pressure and cloud fraction. 
!
!
! Current Code Owner: DWD, Dmitrii Mironov
!  Phone:  +49-69-8062 2705
!  Fax:    +49-69-8062 3721
!  E-mail: dmitrii.mironov@dwd.de
!
! History:
! Version    Date       Name
! ---------- ---------- ----
! 1.00       2005/11/17 Dmitrii Mironov 
!  Initial release
! !VERSION!  !DATE!     <Your name>
!  <Modification comments>
!
! Code Description:
! Language: Fortran 90.
! Software Standards: "European Standards for Writing and
! Documenting Exchangeable Fortran 90 Code".
!==============================================================================
!
! Declarations:
!
! Modules used:

!_dm Parameters are USEd in module "sfcflx".
!_nu USE modd_data_parameters , ONLY : &
!_nu     ireals,                  & ! KIND-type parameter for real variables
!_nu     iintegers                  ! KIND-type parameter for "normal" integer variables

!==============================================================================

IMPLICIT NONE

!==============================================================================
!
! Declarations
 
!  Input (function argument) 
REAL , INTENT(IN) ::   &
    T_a                               ,  &! Air temperature [K]
    e_a                               ,  &! Water vapour pressure [N m^{-2} = kg m^{-1} s^{-2}]
    cl_tot                            ,  &! Total cloud cover [0,1]
    cl_low                                ! Lowe-level cloud cover [0,1]  
 
!  Output (function result) 
REAL               ::  &
    sfcflx_lwradatm                       ! Long-wave radiation flux [W m^{-2}]  


!  Local parameters  

!  Coefficients in the empirical formulation  
!  developed at the Main Geophysical Observatory (MGO), St. Petersburg, Russia.
REAL , PARAMETER ::   &
    c_lmMGO_1    = 43.057924  ,  &! Empirical coefficient 
    c_lmMGO_2    = 540.795        ! Empirical coefficient   
!  Temperature-dependent cloud-correction coefficients in the MGO formula
INTEGER , PARAMETER :: &
    nband_coef = 6                 ! Number of temperature bands  
REAL , PARAMETER, DIMENSION (nband_coef) ::      &
    corr_cl_tot     = (/0.70, 0.45, 0.32,     &
                        0.23, 0.18, 0.13/) ,  &! Total clouds
    corr_cl_low     = (/0.76, 0.49, 0.35,     &
                        0.26, 0.20, 0.15/) ,  &! Low-level clouds
    corr_cl_midhigh = (/0.46, 0.30, 0.21,     &
                        0.15, 0.12, 0.09/)     ! Mid- and high-level clouds  
REAL , PARAMETER ::   &
    T_low  = 253.15           ,  &! Low-limit temperature in the interpolation formula [K]
    del_T  = 10.0                 ! Temperature step in the interpolation formula [K]  

!  Coefficients in the empirical water-vapour correction function 
!  (see Fung et al. 1984, Zapadka and Wozniak 2000, Zapadka et al. 2001). 
REAL , PARAMETER ::     &
    c_watvap_corr_min = 0.6100  ,  &! Empirical coefficient (minimum value of the correction function)
    c_watvap_corr_max = 0.7320  ,  &! Empirical coefficient (maximum value of the correction function)
    c_watvap_corr_e   = 0.0050      ! Empirical coefficient [(N m^{-2})^{-1/2}]  

!  Local variables of type INTEGER
INTEGER  :: &
    i                             ! Loop index  

!  Local variables of type REAL
REAL  ::    &
    c_cl_tot_corr          ,  &! The MGO cloud correction coefficient, total clouds
    c_cl_low_corr          ,  &! The MGO cloud correction coefficient, low-level clouds
    c_cl_midhigh_corr      ,  &! The MGO cloud correction coefficient, mid- and high-level clouds
    T_corr                 ,  &! Temperature used to compute the MGO cloud correction [K]
    f_wvpres_corr          ,  &! Correction function with respect to water vapour
    f_cloud_corr               ! Correction function with respect to cloudiness  
REAL(KIND=JPRB) :: ZHOOK_HANDLE
 
!==============================================================================
!  Start calculations
!------------------------------------------------------------------------------

! Water-vapour correction function
  IF (LHOOK) CALL DR_HOOK('SFCFLX:SFCFLX_LWRADATM',0,ZHOOK_HANDLE)
  f_wvpres_corr = c_watvap_corr_min + c_watvap_corr_e*SQRT(e_a) 
  f_wvpres_corr = MIN(f_wvpres_corr, c_watvap_corr_max)

! Cloud-correction coefficients using the MGO formulation with linear interpolation 
IF(T_a.LT.T_low) THEN 
  c_cl_tot_corr     = corr_cl_tot(1)   
  c_cl_low_corr     = corr_cl_low(1)
  c_cl_midhigh_corr = corr_cl_midhigh(1)
ELSE IF(T_a.GE.T_low+(nband_coef-1)*del_T) THEN
  c_cl_tot_corr     = corr_cl_tot(nband_coef)   
  c_cl_low_corr     = corr_cl_low(nband_coef)
  c_cl_midhigh_corr = corr_cl_midhigh(nband_coef)
ELSE 
  T_corr = T_low
  DO i=1, nband_coef-1
    IF(T_a.GE.T_corr.AND.T_a.LT.T_corr+del_T) THEN 
      c_cl_tot_corr = (T_a-T_corr)/del_T
      c_cl_low_corr = corr_cl_low(i) + (corr_cl_low(i+1)-corr_cl_low(i))*c_cl_tot_corr
      c_cl_midhigh_corr = corr_cl_midhigh(i) + (corr_cl_midhigh(i+1)-corr_cl_midhigh(i))*c_cl_tot_corr
      c_cl_tot_corr = corr_cl_tot(i) + (corr_cl_tot(i+1)-corr_cl_tot(i))*c_cl_tot_corr
    END IF 
    T_corr = T_corr + del_T
  END DO
END IF
! Cloud correction function
IF(cl_low.LT.0.) THEN  ! Total cloud cover only 
  f_cloud_corr = 1. + c_cl_tot_corr*cl_tot*cl_tot
ELSE                          ! Total and low-level cloud cover
  f_cloud_corr = (1. + c_cl_low_corr*cl_low*cl_low)  &
                 * (1. + c_cl_midhigh_corr*(cl_tot*cl_tot-cl_low*cl_low))  
END IF

! Long-wave radiation flux [W m^{-2}]

!  The MGO formulation  
!_nu The MGO formulation  
!_nu sfcflx_lwradatm = -sfcflx_lwradatm*c_lwrad_emis  &
!_nu                 * (c_lmMGO_1*SQRT(tpsf_C_StefBoltz*T_a**4)-c_lmMGO_2)
!_nu 

!  "Conventional" formulation  
!  (see Fung et al. 1984, Zapadka and Wozniak 2000, Zapadka et al. 2001)  
sfcflx_lwradatm = -c_lwrad_emis*tpsf_C_StefBoltz*T_a**4  &
                  * f_wvpres_corr*f_cloud_corr  
IF (LHOOK) CALL DR_HOOK('SFCFLX:SFCFLX_LWRADATM',1,ZHOOK_HANDLE)

!------------------------------------------------------------------------------
!  End calculations
!==============================================================================

END FUNCTION sfcflx_lwradatm


!==============================================================================
!SURFEX include 'sfcflx_lwradwsfc.incf'
! File %M% from Library %Q%
! Version %I% from %G% extracted: %H%
!------------------------------------------------------------------------------

!SURFEX REAL  FUNCTION sfcflx_lwradwsfc (zts)
FUNCTION sfcflx_lwradwsfc (emis,pts)

!------------------------------------------------------------------------------
!
! Description:
!
!  Computes the surface long-wave radiation flux
!  as function of temperature. 
!  
!
! Current Code Owner: DWD, Dmitrii Mironov
!  Phone:  +49-69-8062 2705
!  Fax:    +49-69-8062 3721
!  E-mail: dmitrii.mironov@dwd.de
!
! History:
! Version    Date       Name
! ---------- ---------- ----
! 1.00       2005/11/17 Dmitrii Mironov 
!  Initial release
! !VERSION!  !DATE!     <Your name>
!  <Modification comments>
!
! Code Description:
! Language: Fortran 90.
! Software Standards: "European Standards for Writing and
! Documenting Exchangeable Fortran 90 Code".
!==============================================================================
!
! Declarations:
!
! Modules used:

!_dm Parameters are USEd in module "sfcflx".
!_nu USE modd_data_parameters , ONLY : &
!_nu     ireals,                  & ! KIND-type parameter for real variables
!_nu     iintegers                  ! KIND-type parameter for "normal" integer variables

!==============================================================================

IMPLICIT NONE

!==============================================================================
!
! Declarations
 
!  Input (function argument) 
REAL , INTENT(IN) ::   &
    emis                            , &   ! Emissivity
    pts                                   ! Temperature [K]  
 
!  Output (function result) 
REAL               ::   &
    sfcflx_lwradwsfc                      ! Long-wave radiation flux [W m^{-2}]  
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!==============================================================================
!  Start calculations
!------------------------------------------------------------------------------

! Long-wave radiation flux [W m^{-2}]

IF (LHOOK) CALL DR_HOOK('SFCFLX:SFCFLX_LWRADWSFC',0,ZHOOK_HANDLE)
sfcflx_lwradwsfc = emis*tpsf_C_StefBoltz*pts**4
IF (LHOOK) CALL DR_HOOK('SFCFLX:SFCFLX_LWRADWSFC',1,ZHOOK_HANDLE)

!------------------------------------------------------------------------------
!  End calculations
!==============================================================================

END FUNCTION sfcflx_lwradwsfc


!==============================================================================
!SURFEX include 'sfcflx_momsenlat.incf'
! File %M% from Library %Q%
! Version %I% from %G% extracted: %H%
!------------------------------------------------------------------------------

SUBROUTINE sfcflx_momsenlat ( height_u, height_tq, fetch,                &
                                U_a, T_a, q_a, T_s, P_a, h_ice,            &
                                Q_momentum, Q_sensible, Q_latent, Q_watvap,&
                                Ri, z0u_ini, z0t_ini, Qsat_out,            &
                                Q_latenti, Q_sublim                        )   

!------------------------------------------------------------------------------
!
! Description:
!
!  The sfcflx routine 
!  where fluxes of momentum and of sensible and latent heat 
!  at the air-water or air-ice (air-snow) interface are computed. 
!
!  Lines embraced with "!_tmp" contain temporary parts of the code.
!  Lines embraced/marked with "!_dev" may be replaced
!  as improved parameterizations are developed and tested.
!  Lines embraced/marked with "!_dm" are DM's comments
!  that may be helpful to a user.
!  Lines embraced/marked with "!_dbg" are used 
!  for debugging purposes only.
!
!
! Current Code Owner: DWD, Dmitrii Mironov
!  Phone:  +49-69-8062 2705
!  Fax:    +49-69-8062 3721
!  E-mail: dmitrii.mironov@dwd.de
!
! History:
! Version    Date       Name
! ---------- ---------- ----
! 1.00       2005/11/17 Dmitrii Mironov 
!  Initial release
! !VERSION!  !DATE!     <Your name>
!  <Modification comments>
!
! Code Description:
! Language: Fortran 90.
! Software Standards: "European Standards for Writing and
! Documenting Exchangeable Fortran 90 Code".
!==============================================================================
!
! Declarations:
!
! Modules used:

!_dm Parameters are USEd in module "sfcflx".
!_nu USE modd_data_parameters , ONLY : &
!_nu     ireals,                  & ! KIND-type parameter for real variables
!_nu     iintegers                  ! KIND-type parameter for "normal" integer variables

!==============================================================================

USE MODE_THERMOS
USE MODD_SURF_ATM, ONLY : XRIMAX

IMPLICIT NONE

!==============================================================================
!
! Declarations

!  Input (procedure arguments)

REAL , INTENT(IN) ::   &
    height_u                          ,  &! Height where wind is measured [m]
    height_tq                         ,  &! Height where temperature and humidity are measured [m]
    fetch                             ,  &! Typical wind fetch [m]
    U_a                               ,  &! Wind speed [m s^{-1}]
    T_a                               ,  &! Air temperature [K]
    q_a                               ,  &! Air specific humidity [-]
    T_s                               ,  &! Surface temperature (water, ice or snow) [K]
    P_a                               ,  &! Surface air pressure [N m^{-2} = kg m^{-1} s^{-2}]
    h_ice                             ,  &! Ice thickness [m]
  !
  !plm add z0u_ini and z0t_ini as input to fill z0u_sf and z0t_sf used in 
  !    flake interface for roughness length
    z0u_ini                           ,  &! Initial roughness for momentum
    z0t_ini                               ! Initial roughness for heat  

!  Output (procedure arguments)

REAL , INTENT(INOUT) :: &
    Q_momentum                             ! Momentum flux [N m^{-2}]    
REAL , INTENT(OUT) ::   &
    Q_sensible                         ,  &! Sensible heat flux [W m^{-2}]  
    Q_latent                           ,  &! Laten heat flux [W m^{-2}]
    Q_latenti                          ,  &! Sublimation Latent heat flux [W m^{-2}]
    Q_watvap                           ,  &! Flux of water vapout [kg m^{-2} s^{-1}]
    Q_sublim                           ,  &! Flux of sublimation [kg m^{-2} s^{-1}] 
    Ri                                 ,  &! Gradient Richardson number   
    Qsat_out                               ! specific humidity at saturation [kg.kg-1]


!  Local parameters of type INTEGER
INTEGER , PARAMETER ::  &
   n_iter_max     =  5                       ! Maximum number of iterations   

!  Local variables of type LOGICAL
LOGICAL ::          &
    l_conv_visc     ,  &! Switch, TRUE = viscous free convection, the Nu=C Ra^(1/3) law is used
    l_conv_cbl          ! Switch, TRUE = CBL scale convective structures define surface fluxes   

!  Local variables of type INTEGER
INTEGER  ::   &
    i                           ,  &! Loop index
    n_iter                          ! Number of iterations performed   

!  Local variables of type REAL
REAL  ::    &
    rho_a                  ,  &! Air density [kg m^{-3}]  
    wvpres_s               ,  &! Saturation water vapour pressure at T=T_s [N m^{-2}]
    q_s                        ! Saturation specific humidity at T=T_s [-]  

!  Local variables of type REAL
REAL  ::    &
    Q_mom_tur              ,  &! Turbulent momentum flux [N m^{-2}]
    Q_sen_tur              ,  &! Turbulent sensible heat flux [W m^{-2}]  
    Q_lat_tur              ,  &! Turbulent laten heat flux [W m^{-2}]
    Q_mom_mol              ,  &! Molecular momentum flux [N m^{-2}]
    Q_sen_mol              ,  &! Molecular sensible heat flux [W m^{-2}]  
    Q_lat_mol              ,  &! Molecular laten heat flux [W m^{-2}]
    Q_mom_con              ,  &! Momentum flux in free convection [N m^{-2}]
    Q_sen_con              ,  &! Sensible heat flux in free convection [W m^{-2}]  
    Q_lat_con                  ! Laten heat flux in free convection [W m^{-2}]  

!  Local variables of type REAL
REAL  ::    &
    par_conv_visc          ,  &! Viscous convection stability parameter
    par_conv_cbl           ,  &! CBL convection stability parameter
    c_z0u_fetch            ,  &! Fetch-dependent Charnock parameter
    U_a_thresh             ,  &! Threshld value of the wind speed [m s^{-1}] 
    u_star_thresh          ,  &! Threshld value of friction velocity [m s^{-1}]
    u_star_previter        ,  &! Friction velocity from previous iteration [m s^{-1}]
    u_star_n               ,  &! Friction velocity at neutral stratification [m s^{-1}]
    u_star_st              ,  &! Friction velocity with due regard for stratification [m s^{-1}]
    ZoL                    ,  &! The z/L ratio, z=height_u
  !salgado: Ri is passed as an argument (OUT) in order to be available in flake_interface
  !Ri                     , & ! Gradient Richardson number 
    Ri_cr                  ,  &! Critical value of Ri 
    R_z                    ,  &! Ratio of "height_tq" to "height_u"
    Fun                    ,  &! A function of generic variable "x"
    Fun_prime              ,  &! Derivative of "Fun" with respect to "x"
    Delta                  ,  &! Relative error 
    psi_u                  ,  &! The MO stability function for wind profile
    psi_t                  ,  &! The MO stability function for temperature profile
    psi_q                      ! The MO stability function for specific humidity profile  
REAL(KIND=JPRB) :: ZHOOK_HANDLE


!==============================================================================
!  Start calculations
!------------------------------------------------------------------------------

!_dm All fluxes are positive when directed upwards.

!------------------------------------------------------------------------------
!  Compute saturation specific humidity and the air density at T=T_s
!------------------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('SFCFLX:SFCFLX_MOMSENLAT',0,ZHOOK_HANDLE)
q_s = QSAT(T_s,P_a)
rho_a = sfcflx_rhoair(T_s, q_s, P_a)     ! Air density at T_s and q_s (surface values)
!------------------------------------------------------------------------------
!  Compute molecular fluxes of momentum and of sensible and latent heat
!------------------------------------------------------------------------------

!_dm The fluxes are in kinematic units
Q_mom_mol = -tpsf_nu_u_a*U_a/height_u 
Q_sen_mol = -tpsf_kappa_t_a*(T_a-T_s)/height_tq    
Q_lat_mol = -tpsf_kappa_q_a*(q_a-q_s)/height_tq  

!------------------------------------------------------------------------------
!  Compute fluxes in free convection
!------------------------------------------------------------------------------

par_conv_visc = (T_s-T_a)/T_s*SQRT(tpsf_kappa_t_a) + (q_s-q_a)*tpsf_alpha_q*SQRT(tpsf_kappa_q_a)
IF(par_conv_visc.GT.0.) THEN   ! Viscous convection takes place
  l_conv_visc = .TRUE.
  par_conv_visc = (par_conv_visc*tpl_grav/tpsf_nu_u_a)**num_1o3_sf
  Q_sen_con = c_free_conv*SQRT(tpsf_kappa_t_a)*par_conv_visc  
  Q_sen_con = Q_sen_con*(T_s-T_a)
  Q_lat_con = c_free_conv*SQRT(tpsf_kappa_q_a)*par_conv_visc
  Q_lat_con = Q_lat_con*(q_s-q_a)
ELSE                                  ! No viscous convection, set fluxes to zero
  l_conv_visc = .FALSE.
  Q_sen_con = 0. 
  Q_lat_con = 0.
END IF
Q_mom_con = 0.                 ! Momentum flux in free (viscous or CBL-scale) convection is zero  

!------------------------------------------------------------------------------
!  Compute turbulent fluxes
!------------------------------------------------------------------------------

R_z   = height_tq/height_u                        ! Ratio of "height_tq" to "height_u"
Ri_cr = c_MO_t_stab/c_MO_u_stab**2*R_z  ! Critical Ri
Ri    = tpl_grav*((T_a-T_s)/T_s+tpsf_alpha_q*(q_a-q_s))/MAX(U_a,u_wind_min_sf)**2
Ri    = MIN(XRIMAX,Ri*height_u/Pr_neutral)        ! Gradient Richardson number

Turb_Fluxes: IF(U_a.LT.u_wind_min_sf.OR.Ri.GT.Ri_cr-c_small_sf) THEN  ! Low wind or Ri>Ri_cr 

u_star_st = 0.                       ! Set turbulent fluxes to zero 
Q_mom_tur = 0.                       
Q_sen_tur = 0.   
Q_lat_tur = 0.  
z0u_sf    = z0u_ini
z0t_sf    = z0t_ini

ELSE Turb_Fluxes                            ! Compute turbulent fluxes using MO similarity

! Compute z/L, where z=height_u
IF(Ri.GE.0.) THEN   ! Stable stratification
  ZoL = SQRT(1.-4.*(c_MO_u_stab-R_z*c_MO_t_stab)*Ri)
  ZoL = ZoL - 1. + 2.*c_MO_u_stab*Ri
  ZoL = ZoL/2./c_MO_u_stab/c_MO_u_stab/(Ri_cr-Ri)
ELSE                       ! Convection
  n_iter = 0
  Delta = 1.                ! Set initial error to a large value (as compared to the accuracy)
  u_star_previter = Ri*MAX(1., SQRT(R_z*c_MO_t_conv/c_MO_u_conv)) ! Initial guess for ZoL
  DO WHILE (Delta.GT.c_accur_sf.AND.n_iter.LT.n_iter_max) 
    Fun = u_star_previter**2*(c_MO_u_conv*u_star_previter-1.)  &
          + Ri**2*(1.-R_z*c_MO_t_conv*u_star_previter)  
    Fun_prime = 3.*c_MO_u_conv*u_star_previter**2              &
                - 2.*u_star_previter - R_z*c_MO_t_conv*Ri**2  
    ZoL = u_star_previter - Fun/Fun_prime
    Delta = ABS(ZoL-u_star_previter)/MAX(c_accur_sf, ABS(ZoL+u_star_previter))
    u_star_previter = ZoL
    n_iter = n_iter + 1
  END DO 
!_dbg
!  IF(n_iter.GE.n_iter_max-1)  & 
!    WRITE(*,*) 'ZoL: Max No. iters. exceeded (n_iter = ', n_iter, ')!'
!_dbg
END IF

!  Compute fetch-dependent Charnock parameter, use "u_star_min_sf"
 CALL sfcflx_roughness (fetch, U_a, u_star_min_sf, h_ice, c_z0u_fetch, u_star_thresh, z0u_sf, z0t_sf, z0q_sf)

!  Threshold value of wind speed 
u_star_st = u_star_thresh
 CALL sfcflx_roughness (fetch, U_a, u_star_st, h_ice, c_z0u_fetch, u_star_thresh, z0u_sf, z0t_sf, z0q_sf)
IF(ZoL.GT.0.) THEN   ! MO function in stable stratification 
  psi_u = c_MO_u_stab*ZoL*(1.-MIN(z0u_sf/height_u, 1.))
ELSE                        ! MO function in convection
  psi_t = (1.-c_MO_u_conv*ZoL)**c_MO_u_exp
  psi_q = (1.-c_MO_u_conv*ZoL*MIN(z0u_sf/height_u, 1.))**c_MO_u_exp
  psi_u = 2.*(ATAN(psi_t)-ATAN(psi_q))                  &
          + 2.*LOG((1.+psi_q)/(1.+psi_t))   &
          + LOG((1.+psi_q*psi_q)/(1.+psi_t*psi_t))     
END IF 
U_a_thresh = u_star_thresh/c_Karman*(LOG(height_u/z0u_sf)+psi_u)

!  Compute friction velocity 
n_iter = 0
Delta = 1.                ! Set initial error to a large value (as compared to the accuracy)
!u_star_previter = u_star_thresh  ! Initial guess for friction velocity  
!* modif. V. Masson (Meteo-France) : uses previous time-step momentum flux for ustar guess
u_star_previter = max ( sqrt( - Q_momentum / rho_a ) , u_star_thresh )
!

IF(U_a.LE.U_a_thresh) THEN  ! Smooth surface
  DO WHILE (Delta.GT.c_accur_sf.AND.n_iter.LT.n_iter_max) 
    CALL sfcflx_roughness (fetch, U_a, MIN(u_star_thresh, u_star_previter), h_ice,   &
                             c_z0u_fetch, u_star_thresh, z0u_sf, z0t_sf, z0q_sf)  
    IF(ZoL.GE.0.) THEN  ! Stable stratification
      psi_u = c_MO_u_stab*ZoL*(1.-MIN(z0u_sf/height_u, 1.))
      Fun = LOG(height_u/z0u_sf) + psi_u
      Fun_prime = (Fun + 1. + c_MO_u_stab*ZoL*MIN(z0u_sf/height_u, 1.))/c_Karman
      Fun = Fun*u_star_previter/c_Karman - U_a
    ELSE                       ! Convection 
      psi_t = (1.-c_MO_u_conv*ZoL)**c_MO_u_exp
      psi_q = (1.-c_MO_u_conv*ZoL*MIN(z0u_sf/height_u, 1.))**c_MO_u_exp
      psi_u = 2.*(ATAN(psi_t)-ATAN(psi_q))                  &
              + 2.*LOG((1.+psi_q)/(1.+psi_t))   &
              + LOG((1.+psi_q*psi_q)/(1.+psi_t*psi_t))     
      Fun = LOG(height_u/z0u_sf) + psi_u
      Fun_prime = (Fun + 1./psi_q)/c_Karman
      Fun = Fun*u_star_previter/c_Karman - U_a
    END IF
    u_star_st = u_star_previter - Fun/Fun_prime
    Delta = ABS((u_star_st-u_star_previter)/(u_star_st+u_star_previter))
    u_star_previter = u_star_st
    n_iter = n_iter + 1
  END DO
ELSE                        ! Rough surface
  DO WHILE (Delta.GT.c_accur_sf.AND.n_iter.LT.n_iter_max.AND.z0t_sf>z0t_min_sf) 
    CALL sfcflx_roughness (fetch, U_a, MAX(u_star_thresh, u_star_previter), h_ice,   &
                             c_z0u_fetch, u_star_thresh, z0u_sf, z0t_sf, z0q_sf)  
    IF(ZoL.GE.0.) THEN  ! Stable stratification
      psi_u = c_MO_u_stab*ZoL*(1.-MIN(z0u_sf/height_u, 1.))
      Fun = LOG(height_u/z0u_sf) + psi_u
      Fun_prime = (Fun - 2. - 2.*c_MO_u_stab*ZoL*MIN(z0u_sf/height_u, 1.))/c_Karman
      Fun = Fun*u_star_previter/c_Karman - U_a
    ELSE                       ! Convection 
      psi_t = (1.-c_MO_u_conv*ZoL)**c_MO_u_exp
      psi_q = (1.-c_MO_u_conv*ZoL*MIN(z0u_sf/height_u, 1.))**c_MO_u_exp
      psi_u = 2.*(ATAN(psi_t)-ATAN(psi_q))                  &
              + 2.*LOG((1.+psi_q)/(1.+psi_t))   &
              + LOG((1.+psi_q*psi_q)/(1.+psi_t*psi_t))     
      Fun = LOG(height_u/z0u_sf) + psi_u
      Fun_prime = (Fun - 2./psi_q)/c_Karman
      Fun = Fun*u_star_previter/c_Karman - U_a
    END IF
    IF(h_ice.GE.h_Ice_min_flk) THEN   ! No iteration is required for rough flow over ice
      u_star_st = c_Karman*U_a/MAX(c_small_sf, LOG(height_u/z0u_sf)+psi_u)
      u_star_previter = u_star_st
    ELSE                              ! Iterate in case of open water
      u_star_st = u_star_previter - Fun/Fun_prime
    END IF
    Delta = ABS((u_star_st-u_star_previter)/(u_star_st+u_star_previter))
    u_star_previter = u_star_st
    n_iter = n_iter + 1
  END DO 
END IF
!
!* Modification: V. Masson (Meteo-France)
!* in case convergence did not occur :
!      - one keeps the values of the previous time-step for momentum roughness length
!      - one recomputes the thermal and water vapor roughness lengthes
!      - one estimates the momentum flux using neutral law
IF (n_iter == n_iter_max .OR. z0T_sf<=z0t_min_sf) THEN
  u_star_st = max ( sqrt( - Q_momentum / rho_a ) , u_star_min_sf )
  CALL sfcflx_roughness (fetch, U_a,  u_star_st, h_ice,   &
                           c_z0u_fetch, u_star_previter, z0u_sf, z0t_sf, z0q_sf)  
  z0u_sf    = z0u_ini
  u_star_st = c_Karman*U_a/MAX(c_small_sf, LOG(height_u/z0u_sf))
END IF

!_dbg
!  WRITE(*,*) 'MO stab. func. psi_u = ', psi_u, '   n_iter = ', n_iter
!  WRITE(*,*) '   Wind speed = ', U_a, '  u_* = ', u_star_st
!  WRITE(*,*) '   Fun = ', Fun
!_dbg

!_dbg
!  IF(n_iter.GE.n_iter_max-1)  & 
!    WRITE(*,*) 'u_*: Max No. iters. exceeded (n_iter = ', n_iter, ')!'
!_dbg

!  Momentum flux
Q_mom_tur = -u_star_st*u_star_st

!  Temperature and specific humidity fluxes
 CALL sfcflx_roughness (fetch, U_a, u_star_st, h_ice, c_z0u_fetch, u_star_thresh, z0u_sf, z0t_sf, z0q_sf)
!
IF(ZoL.GE.0.) THEN   ! Stable stratification 
  psi_t = c_MO_t_stab*R_z*ZoL*(1.-MIN(z0t_sf/height_tq, 1.))
  psi_q = c_MO_q_stab*R_z*ZoL*(1.-MIN(z0q_sf/height_tq, 1.))
!_dbg
!  WRITE(*,*) 'STAB: psi_t = ', psi_t, '   psi_q = ', psi_q
!_dbg
ELSE                        ! Convection 
  psi_u = (1.-c_MO_t_conv*R_z*ZoL)**c_MO_t_exp
  psi_t = (1.-c_MO_t_conv*R_z*ZoL*MIN(z0t_sf/height_tq, 1.))**c_MO_t_exp
  psi_t = 2.*LOG((1.+psi_t)/(1.+psi_u))
  psi_u = (1.-c_MO_q_conv*R_z*ZoL)**c_MO_q_exp
  psi_q = (1.-c_MO_q_conv*R_z*ZoL*MIN(z0q_sf/height_tq, 1.))**c_MO_q_exp
  psi_q = 2.*LOG((1.+psi_q)/(1.+psi_u))
!_dbg
!  WRITE(*,*) 'CONV: psi_t = ', psi_t, '   psi_q = ', psi_q
!_dbg
END IF

Q_sen_tur = -(T_a-T_s)*u_star_st*c_Karman/Pr_neutral  &
            / MAX(c_small_sf, LOG(height_tq/z0t_sf)+psi_t)  
Q_lat_tur = -(q_a-q_s)*u_star_st*c_Karman/Sc_neutral  &
            / MAX(c_small_sf, LOG(height_tq/z0q_sf)+psi_q)  

END IF Turb_Fluxes

!------------------------------------------------------------------------------
!  Decide between turbulent, molecular, and convective fluxes
!------------------------------------------------------------------------------

Q_momentum = MIN(Q_mom_tur, Q_mom_mol, Q_mom_con)  ! Momentum flux is negative          
IF(l_conv_visc) THEN    ! Convection, take fluxes that are maximal in magnitude 
  IF(ABS(Q_sen_tur).GE.ABS(Q_sen_con)) THEN
    Q_sensible = Q_sen_tur
  ELSE
    Q_sensible = Q_sen_con
  END IF
  IF(ABS(Q_sensible).LT.ABS(Q_sen_mol)) THEN
    Q_sensible = Q_sen_mol
  END IF
  IF(ABS(Q_lat_tur).GE.ABS(Q_lat_con)) THEN
    Q_latent = Q_lat_tur
  ELSE
    Q_latent = Q_lat_con
  END IF
  IF(ABS(Q_latent).LT.ABS(Q_lat_mol)) THEN
    Q_latent = Q_lat_mol
  END IF
ELSE                    ! Stable or neutral stratification, chose fluxes that are maximal in magnitude 
  IF(ABS(Q_sen_tur).GE.ABS(Q_sen_mol)) THEN 
    Q_sensible = Q_sen_tur
  ELSE 
    Q_sensible = Q_sen_mol    
  END IF
  IF(ABS(Q_lat_tur).GE.ABS(Q_lat_mol)) THEN 
    Q_latent = Q_lat_tur
  ELSE 
    Q_latent = Q_lat_mol  
  END IF
END IF

!------------------------------------------------------------------------------
!  Set output (notice that fluxes are no longer in kinematic units)
!------------------------------------------------------------------------------
!
Q_momentum = Q_momentum*rho_a 
Q_sensible = Q_sensible*rho_a*tpsf_c_a_p
Q_watvap   = Q_latent*rho_a
!
IF(h_ice.GE.h_Ice_min_flk)THEN
   Q_latent  = Q_watvap * (tpsf_L_evap + tpl_L_f)   ! Add latent heat of fusion over ice
   Q_latenti = Q_watvap * (tpsf_L_evap + tpl_L_f)
   Q_sublim  = Q_watvap
ELSE
   Q_latent  = Q_watvap * tpsf_L_evap
   Q_latenti = 0.0
   Q_sublim  = 0.0
ENDIF
!
Qsat_out = q_s
!
IF (LHOOK) CALL DR_HOOK('SFCFLX:SFCFLX_MOMSENLAT',1,ZHOOK_HANDLE)

!------------------------------------------------------------------------------
!  End calculations
!==============================================================================

END SUBROUTINE sfcflx_momsenlat


!==============================================================================

!==============================================================================
!SURFEX include 'sfcflx_rhoair.incf'
! File %M% from Library %Q%
! Version %I% from %G% extracted: %H%
!------------------------------------------------------------------------------

!SURFEX REAL  FUNCTION sfcflx_rhoair (T, q, P)
FUNCTION sfcflx_rhoair (T, q, P)

!------------------------------------------------------------------------------
!
! Description:
!
!  Computes the air density as function 
!  of temperature, specific humidity and pressure.
!  
!
! Current Code Owner: DWD, Dmitrii Mironov
!  Phone:  +49-69-8062 2705
!  Fax:    +49-69-8062 3721
!  E-mail: dmitrii.mironov@dwd.de
!
! History:
! Version    Date       Name
! ---------- ---------- ----
! 1.00       2005/11/17 Dmitrii Mironov 
!  Initial release
! !VERSION!  !DATE!     <Your name>
!  <Modification comments>
!
! Code Description:
! Language: Fortran 90.
! Software Standards: "European Standards for Writing and
! Documenting Exchangeable Fortran 90 Code".
!==============================================================================
!
! Declarations:
!
! Modules used:

!_dm Parameters are USEd in module "sfcflx".
!_nu USE modd_data_parameters , ONLY : &
!_nu     ireals,                  & ! KIND-type parameter for real variables
!_nu     iintegers                  ! KIND-type parameter for "normal" integer variables

!==============================================================================

IMPLICIT NONE

!==============================================================================
!
! Declarations
 
!  Input (function argument) 
REAL , INTENT(IN) ::   &
    T                                 ,  &! Temperature [K]
    q                                 ,  &! Specific humidity 
    P                                     ! Pressure [N m^{-2} = kg m^{-1} s^{-2}]  
 
!  Output (function result) 
REAL               ::  &
    sfcflx_rhoair                         ! Air density [kg m^{-3}]  
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!==============================================================================
!  Start calculations
!------------------------------------------------------------------------------

! Air density [kg m^{-3}] 

IF (LHOOK) CALL DR_HOOK('SFCFLX:SFCFLX_RHOAIR',0,ZHOOK_HANDLE)
sfcflx_rhoair = P/tpsf_R_dryair/T/(1.+(1./tpsf_Rd_o_Rv-1.)*q)
IF (LHOOK) CALL DR_HOOK('SFCFLX:SFCFLX_RHOAIR',1,ZHOOK_HANDLE)

!------------------------------------------------------------------------------
!  End calculations
!==============================================================================

END FUNCTION sfcflx_rhoair


!==============================================================================
!SURFEX include 'sfcflx_roughness.incf'
! File %M% from Library %Q%
! Version %I% from %G% extracted: %H%
!------------------------------------------------------------------------------

SUBROUTINE sfcflx_roughness (fetch, U_a, u_star, h_ice,    &
                               c_z0u_fetch, u_star_thresh, z0u, z0t, z0q)  

!------------------------------------------------------------------------------
!
! Description:
!
!  Computes the water-surface or the ice-surface roughness lengths
!  with respect to wind velocity, potential temperature and specific humidity.
!
!  The water-surface roughness lengths with respect to wind velocity is computed
!  from the Charnock formula when the surface is aerodynamically rough.
!  A simple empirical formulation is used to account for the dependence 
!  of the Charnock parameter on the wind fetch. 
!  When the flow is aerodynamically smooth, the roughness length with respect to 
!  wind velocity is proportional to the depth of the viscous sub-layer.
!  The water-surface roughness lengths for scalars are computed using the power-law 
!  formulations in terms of the roughness Reynolds number (Zilitinkevich et al. 2001).
!  The ice-surface aerodynamic roughness is taken to be constant.
!  The ice-surface roughness lengths for scalars 
!  are computed through the power-law formulations 
!  in terms of the roughness Reynolds number (Andreas 2002).
!
!
! Current Code Owner: DWD, Dmitrii Mironov
!  Phone:  +49-69-8062 2705
!  Fax:    +49-69-8062 3721
!  E-mail: dmitrii.mironov@dwd.de
!
! History:
! Version    Date       Name
! ---------- ---------- ----
! 1.00       2005/11/17 Dmitrii Mironov 
!  Initial release
! !VERSION!  !DATE!     <Your name>
!  <Modification comments>
!
! Code Description:
! Language: Fortran 90.
! Software Standards: "European Standards for Writing and
! Documenting Exchangeable Fortran 90 Code".
!==============================================================================
!
! Declarations:
!
! Modules used:

!_dm Parameters are USEd in module "sfcflx".
!_nu USE modd_data_parameters , ONLY :   &
!_nu   ireals                     , & ! KIND-type parameter for real variables
!_nu   iintegers                      ! KIND-type parameter for "normal" integer variables

!==============================================================================

IMPLICIT NONE

!==============================================================================
!
! Declarations

!  Input (procedure arguments)
REAL , INTENT(IN) ::   &
    fetch                             ,  &! Typical wind fetch [m]
    U_a                               ,  &! Wind speed [m s^{-1}]
    u_star                            ,  &! Friction velocity in the surface air layer [m s^{-1}]
    h_ice                                 ! Ice thickness [m]  

!  Output (procedure arguments)
REAL , INTENT(OUT) ::   &
    c_z0u_fetch                        ,  &! Fetch-dependent Charnock parameter
    u_star_thresh                      ,  &! Threshold value of friction velocity [m s^{-1}]
    z0u                                ,  &! Roughness length with respect to wind velocity [m]
    z0t                                ,  &! Roughness length with respect to potential temperature [m]
    z0q                                    ! Roughness length with respect to specific humidity [m]  

!  Local variables of type REAL
REAL  ::    &
    Re_s                   ,  &! Surface Reynolds number 
    Re_s_thresh                ! Threshold value of Re_s  
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!==============================================================================
!  Start calculations
!------------------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('SFCFLX:SFCFLX_ROUGHNESS',0,ZHOOK_HANDLE)
Water_or_Ice: IF(h_ice.LT.h_Ice_min_flk) THEN  ! Water surface  

! The Charnock parameter as dependent on dimensionless fetch
  c_z0u_fetch = MAX(U_a, u_wind_min_sf)**2/tpl_grav/fetch  ! Inverse dimensionless fetch
  c_z0u_fetch = c_z0u_rough + c_z0u_ftch_f*c_z0u_fetch**c_z0u_ftch_ex
  c_z0u_fetch = MIN(c_z0u_fetch, c_z0u_rough_L)                      ! Limit Charnock parameter

! Threshold value of friction velocity
  u_star_thresh = (c_z0u_smooth/c_z0u_fetch*tpl_grav*tpsf_nu_u_a)**num_1o3_sf

! Surface Reynolds number and its threshold value
  Re_s = u_star**3/tpsf_nu_u_a/tpl_grav
  Re_s_thresh = c_z0u_smooth/c_z0u_fetch

! Aerodynamic roughness
  IF(Re_s.LE.Re_s_thresh) THEN                 
    z0u = c_z0u_smooth*tpsf_nu_u_a/u_star     ! Smooth flow
  ELSE
    z0u = c_z0u_fetch*u_star*u_star/tpl_grav  ! Rough flow
  END IF 
! Roughness for scalars  
  z0q = c_z0u_fetch*MAX(Re_s, Re_s_thresh)
  z0t = c_z0t_rough_1*z0q**c_z0t_rough_3 - c_z0t_rough_2
  z0q = c_z0q_rough_1*z0q**c_z0q_rough_3 - c_z0q_rough_2
  z0t = z0u*EXP(-c_Karman/Pr_neutral*z0t)
  z0q = z0u*EXP(-c_Karman/Sc_neutral*z0q) 

ELSE Water_or_Ice                              ! Ice surface

! The Charnock parameter is not used over ice, formally set "c_z0u_fetch" to its minimum value
  c_z0u_fetch = c_z0u_rough

! Threshold value of friction velocity
  u_star_thresh = c_z0u_smooth*tpsf_nu_u_a/z0u_ice_rough

! Aerodynamic roughness
  z0u = MAX(z0u_ice_rough, c_z0u_smooth*tpsf_nu_u_a/u_star)

! Roughness Reynolds number 
  Re_s = MAX(u_star*z0u/tpsf_nu_u_a, c_accur_sf)

! Roughness for scalars  
  IF(Re_s.LE.Re_z0s_ice_t) THEN 
    z0t = c_z0t_ice_b0t + c_z0t_ice_b1t*LOG(Re_s)
    z0t = MIN(z0t, c_z0t_ice_b0s)
    z0q = c_z0q_ice_b0t + c_z0q_ice_b1t*LOG(Re_s)
    z0q = MIN(z0q, c_z0q_ice_b0s)
  ELSE 
    z0t = c_z0t_ice_b0r + c_z0t_ice_b1r*LOG(Re_s) + c_z0t_ice_b2r*LOG(Re_s)**2
    z0q = c_z0q_ice_b0r + c_z0q_ice_b1r*LOG(Re_s) + c_z0q_ice_b2r*LOG(Re_s)**2
  END IF
  z0t = z0u*EXP(z0t)
  z0q = z0u*EXP(z0q)

END IF Water_or_Ice
IF (LHOOK) CALL DR_HOOK('SFCFLX:SFCFLX_ROUGHNESS',1,ZHOOK_HANDLE)

!------------------------------------------------------------------------------
!  End calculations
!==============================================================================

END SUBROUTINE sfcflx_roughness

!==============================================================================
!SURFEX include 'sfcflx_satwvpres.incf'
! File %M% from Library %Q%
! Version %I% from %G% extracted: %H%
!------------------------------------------------------------------------------

!SURFEX REAL  FUNCTION sfcflx_satwvpres (T, h_ice)
FUNCTION sfcflx_satwvpres (T, h_ice)

!------------------------------------------------------------------------------
!
! Description:
!
!  Computes saturation water vapour pressure 
!  over the water surface or over the ice surface
!  as function of temperature. 
!  
!
! Current Code Owner: DWD, Dmitrii Mironov
!  Phone:  +49-69-8062 2705
!  Fax:    +49-69-8062 3721
!  E-mail: dmitrii.mironov@dwd.de
!
! History:
! Version    Date       Name
! ---------- ---------- ----
! 1.00       2005/11/17 Dmitrii Mironov 
!  Initial release
! !VERSION!  !DATE!     <Your name>
!  <Modification comments>
!
! Code Description:
! Language: Fortran 90.
! Software Standards: "European Standards for Writing and
! Documenting Exchangeable Fortran 90 Code".
!==============================================================================
!
! Declarations:
!
! Modules used:

!_dm Parameters are USEd in module "sfcflx".
!_nu USE modd_data_parameters  , ONLY : &
!_nu     ireals,                   & ! KIND-type parameter for real variables
!_nu     iintegers                   ! KIND-type parameter for "normal" integer variables

!_dm The variable is USEd in module "sfcflx".
!_nu USE flake_parameters , ONLY : &
!_nu   h_Ice_min_flk                 ! Minimum ice thickness [m]

!==============================================================================

IMPLICIT NONE

!==============================================================================
!
! Declarations
 
!  Input (function argument) 
REAL , INTENT(IN) ::   &
    T                                 ,  &! Temperature [K]
    h_ice                                 ! Ice thickness [m]  
 
!  Output (function result) 
REAL               ::  &
    sfcflx_satwvpres                      ! Saturation water vapour pressure [N m^{-2} = kg m^{-1} s^{-2}]  

!  Local parameters
REAL , PARAMETER ::   &
     b1_vap   = 610.78        ,  &! Coefficient [N m^{-2} = kg m^{-1} s^{-2}]
     b3_vap   = 273.16        ,  &! Triple point [K]
     b2w_vap  = 17.2693882    ,  &! Coefficient (water)
     b2i_vap  = 21.8745584    ,  &! Coefficient (ice) 
     b4w_vap  = 35.86         ,  &! Coefficient (temperature) [K]
     b4i_vap  = 7.66              ! Coefficient (temperature) [K]  
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!==============================================================================
!  Start calculations
!------------------------------------------------------------------------------

! Saturation water vapour pressure [N m^{-2} = kg m^{-1} s^{-2}]

IF (LHOOK) CALL DR_HOOK('SFCFLX:SFCFLX_SATWVPRES',0,ZHOOK_HANDLE)
IF(h_ice.LT.h_Ice_min_flk) THEN  ! Water surface
  sfcflx_satwvpres = b1_vap*EXP(b2w_vap*(T-b3_vap)/(T-b4w_vap))
ELSE                             ! Ice surface
  sfcflx_satwvpres = b1_vap*EXP(b2i_vap*(T-b3_vap)/(T-b4i_vap))
END IF 
IF (LHOOK) CALL DR_HOOK('SFCFLX:SFCFLX_SATWVPRES',1,ZHOOK_HANDLE)

!------------------------------------------------------------------------------
!  End calculations
!==============================================================================

END FUNCTION sfcflx_satwvpres


!==============================================================================
!SURFEX include 'sfcflx_spechum.incf'
! File %M% from Library %Q%
! Version %I% from %G% extracted: %H%
!------------------------------------------------------------------------------

!SURFEX REAL  FUNCTION sfcflx_spechum (wvpres, P)
FUNCTION sfcflx_spechum (wvpres, P)

!------------------------------------------------------------------------------
!
! Description:
!
!  Computes specific humidity as function 
!  of water vapour pressure and air pressure. 
!  
!
! Current Code Owner: DWD, Dmitrii Mironov
!  Phone:  +49-69-8062 2705
!  Fax:    +49-69-8062 3721
!  E-mail: dmitrii.mironov@dwd.de
!
! History:
! Version    Date       Name
! ---------- ---------- ----
! 1.00       2005/11/17 Dmitrii Mironov 
!  Initial release
! !VERSION!  !DATE!     <Your name>
!  <Modification comments>
!
! Code Description:
! Language: Fortran 90.
! Software Standards: "European Standards for Writing and
! Documenting Exchangeable Fortran 90 Code".
!==============================================================================
!
! Declarations:
!
! Modules used:

!_dm Parameters are USEd in module "sfcflx".
!_nu USE modd_data_parameters , ONLY : &
!_nu     ireals,                  & ! KIND-type parameter for real variables
!_nu     iintegers                  ! KIND-type parameter for "normal" integer variables

!==============================================================================

IMPLICIT NONE

!==============================================================================
!
! Declarations
 
!  Input (function argument) 
REAL , INTENT(IN) ::   &
    wvpres                            ,  &! Water vapour pressure [N m^{-2} = kg m^{-1} s^{-2}]
    P                                     ! Air pressure [N m^{-2} = kg m^{-1} s^{-2}]  
 
!  Output (function result) 
REAL               ::  &
    sfcflx_spechum                        ! Specific humidity  
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!==============================================================================
!  Start calculations
!------------------------------------------------------------------------------

! Specific humidity 

IF (LHOOK) CALL DR_HOOK('SFCFLX:SFCFLX_SPECHUM',0,ZHOOK_HANDLE)
sfcflx_spechum = tpsf_Rd_o_Rv*wvpres/(P-(1.-tpsf_Rd_o_Rv)*wvpres)
IF (LHOOK) CALL DR_HOOK('SFCFLX:SFCFLX_SPECHUM',1,ZHOOK_HANDLE)

!------------------------------------------------------------------------------
!  End calculations
!==============================================================================

END FUNCTION sfcflx_spechum


!==============================================================================
!SURFEX include 'sfcflx_wvpreswetbulb.incf'
! File %M% from Library %Q%
! Version %I% from %G% extracted: %H%
!------------------------------------------------------------------------------

!SURFEX REAL  FUNCTION sfcflx_wvpreswetbulb (T_dry, T_wetbulb, satwvpres_bulb, P)             
FUNCTION sfcflx_wvpreswetbulb (T_dry, T_wetbulb, satwvpres_bulb, P)             

!------------------------------------------------------------------------------
!
! Description:
!
!  Computes water vapour pressure as function of air temperature, 
!  wet bulb temperature, satururation vapour pressure at wet-bulb temperature,
!  and air pressure.
!  
!
! Current Code Owner: DWD, Dmitrii Mironov
!  Phone:  +49-69-8062 2705
!  Fax:    +49-69-8062 3721
!  E-mail: dmitrii.mironov@dwd.de
!
! History:
! Version    Date       Name
! ---------- ---------- ----
! 1.00       2005/11/17 Dmitrii Mironov 
!  Initial release
! !VERSION!  !DATE!     <Your name>
!  <Modification comments>
!
! Code Description:
! Language: Fortran 90.
! Software Standards: "European Standards for Writing and
! Documenting Exchangeable Fortran 90 Code".
!==============================================================================
!
! Declarations:
!
! Modules used:

!_dm Parameters are USEd in module "sfcflx".
!_nu USE modd_data_parameters , ONLY : &
!_nu     ireals,                  & ! KIND-type parameter for real variables
!_nu     iintegers                  ! KIND-type parameter for "normal" integer variables

!==============================================================================

IMPLICIT NONE

!==============================================================================
!
! Declarations
 
!  Input (function argument) 
REAL , INTENT(IN) ::   &
    T_dry                             ,  &! Dry air temperature [K]
    T_wetbulb                         ,  &! Wet bulb temperature [K]
    satwvpres_bulb                    ,  &! Satururation vapour pressure at wet-bulb temperature [N m^{-2}]
    P                                     ! Atmospheric pressure [N m^{-2}]  
 
!  Output (function result) 
REAL               ::  &
    sfcflx_wvpreswetbulb                  ! Water vapour pressure [N m^{-2} = kg m^{-1} s^{-2}]  
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!==============================================================================
!  Start calculations
!------------------------------------------------------------------------------

! Water vapour pressure [N m^{-2} = kg m^{-1} s^{-2}]

IF (LHOOK) CALL DR_HOOK('SFCFLX:SFCFLX_WVPRESWETBULB',0,ZHOOK_HANDLE)
sfcflx_wvpreswetbulb = satwvpres_bulb  &
                       - tpsf_c_a_p*P/tpsf_L_evap/tpsf_Rd_o_Rv*(T_dry-T_wetbulb)  
IF (LHOOK) CALL DR_HOOK('SFCFLX:SFCFLX_WVPRESWETBULB',1,ZHOOK_HANDLE)


!------------------------------------------------------------------------------
!  End calculations
!==============================================================================

END FUNCTION sfcflx_wvpreswetbulb


END MODULE mode_sfcflx

