!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
! File %M% from Library %Q%
! Version %I% from %G% extracted: %H%
!------------------------------------------------------------------------------

MODULE modd_flake_parameters

!------------------------------------------------------------------------------
!
! Description:
!
!  Values of empirical constants of the lake model FLake 
!  and of several thermodynamic parameters are set.
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
!!
!!    MODIFICATIONS
!!    -------------
!!      P. Le Moigne 04/2013 : homogeneization with surfex parameters
!!
!==============================================================================
!
! Declarations:
!
!==============================================================================

IMPLICIT NONE

!==============================================================================
!
! Declarations

!  Dimensionless constants 
!  in the equations for the mixed-layer depth 
!  and for the shape factor with respect to the temperature profile in the thermocline
REAL, PARAMETER ::         &
    c_cbl_1       = 0.17            ,  &! Constant in the CBL entrainment equation
    c_cbl_2       = 1.              ,  &! Constant in the CBL entrainment equation
    c_sbl_ZM_n    = 0.5             ,  &! Constant in the ZM1996 equation for the equilibrium SBL depth
    c_sbl_ZM_s    = 10.             ,  &! Constant in the ZM1996 equation for the equilibrium SBL depth
    c_sbl_ZM_i    = 20.             ,  &! Constant in the ZM1996 equation for the equilibrium SBL depth
    c_relax_h     = 0.030           ,  &! Constant in the relaxation equation for the SBL depth
    c_relax_C     = 0.0030              ! Constant in the relaxation equation for the shape factor  
                                             ! with respect to the temperature profile in the thermocline

!  Parameters of the shape functions 
!  Indices refer to T - thermocline, S - snow, I - ice,
!  B1 - upper layer of the bottom sediments, B2 - lower layer of the bottom sediments.
!  "pr0" and "pr1" denote zeta derivatives of the corresponding shape function 
!  at "zeta=0" ad "zeta=1", respectively.
REAL, PARAMETER ::         &
    C_T_min       = 0.5             ,  &! Minimum value of the shape factor C_T (thermocline)
    C_T_max       = 0.8             ,  &! Maximum value of the shape factor C_T (thermocline)
    Phi_T_pr0_1   = 40./3.   ,  &! Constant in the expression for the T shape-function derivative 
    Phi_T_pr0_2   = 20./3.   ,  &! Constant in the expression for the T shape-function derivative 
    C_TT_1        = 11./18.  ,  &! Constant in the expression for C_TT (thermocline)
    C_TT_2        = 7./45.   ,  &! Constant in the expression for C_TT (thermocline)
    C_B1          = 2./3.    ,  &! Shape factor (upper layer of bottom sediments)
    C_B2          = 3./5.    ,  &! Shape factor (lower layer of bottom sediments)
    Phi_B1_pr0    = 2.              ,  &! B1 shape-function derivative 
    C_S_lin       = 0.5             ,  &! Shape factor (linear temperature profile in the snow layer)
    Phi_S_pr0_lin = 1.              ,  &! S shape-function derivative (linear profile) 
    C_I_lin       = 0.5             ,  &! Shape factor (linear temperature profile in the ice layer)
    Phi_I_pr0_lin = 1.              ,  &! I shape-function derivative (linear profile) 
    Phi_I_pr1_lin = 1.              ,  &! I shape-function derivative (linear profile) 
    Phi_I_ast_MR  = 2.              ,  &! Constant in the MR2004 expression for I shape factor
    C_I_MR        = 1./12.   ,  &! Constant in the MR2004 expression for I shape factor
    H_Ice_max     = 3.                  ! Maximum ice tickness in   
                                             ! the Mironov and Ritter (2004, MR2004) ice model [m] 

!  Security constants
REAL , PARAMETER ::         &
    h_Snow_min_flk = 1.0E-5         ,  &! Minimum snow thickness [m]
    h_Ice_min_flk  = 1.0E-9         ,  &! Minimum ice thickness [m]
    h_ML_min_flk   = 1.0E-2         ,  &! Minimum mixed-layer depth [m]
    h_ML_max_flk   = 1.0E+3         ,  &! Maximum mixed-layer depth [m]
    H_B1_min_flk   = 1.0E-3         ,  &! Minimum thickness of the upper layer of bottom sediments [m]
    u_star_min_flk = 1.0E-6             ! Minimum value of the surface friction velocity [m s^{-1}]  

!  Security constant(s)
REAL , PARAMETER ::         &
    c_small_flk    = 1.0E-10            ! A small number  

!  Thermodynamic parameters
REAL , PARAMETER ::        &
    tpl_grav          = 9.80665    ,  &! Acceleration due to gravity [m s^{-2}]
    tpl_T_r           = 277.13     ,  &! Temperature of maximum density of fresh water [K]
    tpl_T_f           = 273.15     ,  &! Fresh water freezing point [K]
    tpl_a_T           = 1.6509E-05 ,  &! Constant in the fresh-water equation of state [K^{-2}]
    tpl_rho_w_r       = 1.0E+03    ,  &! Maximum density of fresh water [kg m^{-3}]
    tpl_rho_I         = 9.17E+02   ,  &! Density of ice [kg m^{-3}]
    tpl_rho_S_min     = 1.0E+02    ,  &! Minimum snow density [kg m^{-3}]
    tpl_rho_S_max     = 3.0E+02    ,  &! Maximum snow density [kg m^{-3}]
    tpl_Gamma_rho_S   = 2.0E+02    ,  &! Empirical parameter [kg m^{-4}]  
                                            ! in the expression for the snow density 
    tpl_L_f           = 3.335E+05  ,  &! Latent heat of fusion [J kg^{-1}]
    tpl_c_w           = 4.218E+03  ,  &! Specific heat of water [J kg^{-1} K^{-1}]
    tpl_c_I           = 2.106E+03  ,  &! Specific heat of ice [J kg^{-1} K^{-1}]
    tpl_c_S           = 2.1E+03    ,  &! Specific heat of snow [J kg^{-1} K^{-1}]
    tpl_kappa_w       = 5.46E-01   ,  &! Molecular heat conductivity of water [J m^{-1} s^{-1} K^{-1}]
    tpl_kappa_I       = 2.29       ,  &! Molecular heat conductivity of ice [J m^{-1} s^{-1} K^{-1}]
    tpl_kappa_S_min   = 0.2        ,  &! Minimum molecular heat conductivity of snow [J m^{-1} s^{-1} K^{-1}]
    tpl_kappa_S_max   = 1.5        ,  &! Maximum molecular heat conductivity of snow [J m^{-1} s^{-1} K^{-1}]
    tpl_Gamma_kappa_S = 1.3            ! Empirical parameter [J m^{-2} s^{-1} K^{-1}]   
                                       ! in the expression for the snow heat conductivity 

!  Skin temperature parameters
REAL , PARAMETER ::        &
    h_skinlayer_flk   = 0.001          ! Skin layer thickness [m]                                            
!==============================================================================

END MODULE modd_flake_parameters

