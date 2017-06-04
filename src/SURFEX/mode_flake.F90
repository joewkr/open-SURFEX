!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
! File %M% from Library %Q%
! Version %I% from %G% extracted: %H%
!------------------------------------------------------------------------------

MODULE mode_flake

!------------------------------------------------------------------------------
!
! Description:
!
!  The main program unit of the lake model FLake,  
!  containing most of the FLake procedures.
!  Most FLake variables and local parameters are declared.
!
!  FLake (Fresh-water Lake) is a lake model capable of predicting the surface temperature 
!  in lakes of various depth on the time scales from a few hours to a year.
!  The model is based on a two-layer parametric representation of
!  the evolving temperature profile, where the structure of the stratified layer between the
!  upper mixed layer and the basin bottom, the lake thermocline,
!  is described using the concept of self-similarity of the temperature-depth curve.
!  The concept was put forward by Kitaigorodskii and Miropolsky (1970) 
!  to describe the vertical temperature structure of the oceanic seasonal thermocline.
!  It has since been successfully used in geophysical applications.
!  The concept of self-similarity of the evolving temperature profile
!  is also used to describe the vertical structure of the thermally active upper layer 
!  of bottom sediments and of the ice and snow cover.
!
!  The lake model incorporates the heat budget equations
!  for the four layers in question, viz., snow, ice, water and bottom sediments,
!  developed with due regard for the vertically distributed character
!  of solar radiation heating.
!  The entrainment equation that incorporates the Zilitinkevich (1975) spin-up term
!  is used to compute the depth of a convectively-mixed layer. 
!  A relaxation-type equation is used
!  to compute the wind-mixed layer depth in stable and neutral stratification,
!  where a multi-limit formulation for the equilibrium mixed-layer depth
!  proposed by Zilitinkevich and Mironov (1996)
!  accounts for the effects of the earth's rotation, of the surface buoyancy flux
!  and of the static stability in the thermocline.
!  The equations for the mixed-layer depth are developed with due regard for  
!  the volumetric character of the radiation heating.
!  Simple thermodynamic arguments are invoked to develop
!  the evolution equations for the ice thickness and for the snow thickness.
!  The heat flux through the water-bottom sediment interface is computed,
!  using a parameterization proposed by Golosov et al. (1998).
!  The heat flux trough the air-water interface 
!  (or through the air-ice or air-snow interface)
!  is provided by the driving atmospheric model.
!
!  Empirical constants and parameters of the lake model
!  are estimated, using independent empirical and numerical data.
!  They should not be re-evaluated when the model is applied to a particular lake.
!  The only lake-specific parameters are the lake depth,
!  the optical characteristics of lake water,
!  the temperature at the bottom of the thermally active layer
!  of bottom sediments and the depth of that layer.
!
!  A detailed description of the lake model is given in
!  Mironov, D. V., 2005:
!  Parameterization of Lakes in Numerical Weather Prediction.
!  Part 1: Description of a Lake Model.
!  Manuscript is available from the author.
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

!USE modd_data_parameters , ONLY : &
!    ireals                   ,  &! KIND-type parameter for real variables
!    iintegers                    ! KIND-type parameter for "normal" integer variables  

!==============================================================================

!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODD_SURFEX_OMP, ONLY : NBLOCK
!
IMPLICIT NONE

!==============================================================================
!
! Declarations
!
!  The variables declared below
!  are accessible to all program units of the MODULE flake.
!  Some of them should be USEd by the driving routines that call flake routines.
!  These are basically the quantities computed by FLake.
!  All variables declared below have a suffix "flk".

!  FLake variables of type REAL

!RJ: provide default unreasonable value to init for 'ifort -fpic -openmp', to avoid ICE
REAL,PARAMETER,PRIVATE :: Z_=-HUGE(0.0)

!  Temperatures at the previous time step ("p") and the updated temperatures ("n") 
REAL :: T_mnw_p_flk=Z_, T_mnw_n_flk=Z_      ! Mean temperature of the water column [K] 
REAL :: T_snow_p_flk=Z_, T_snow_n_flk=Z_    ! Temperature at the air-snow interface [K] 
REAL :: T_ice_p_flk=Z_, T_ice_n_flk=Z_      ! Temperature at the snow-ice or air-ice interface [K] 
REAL :: T_wML_p_flk=Z_, T_wML_n_flk=Z_      ! Mixed-layer temperature [K] 
REAL :: T_bot_p_flk=Z_, T_bot_n_flk=Z_      ! Temperature at the water-bottom sediment interface [K] 
REAL :: T_B1_p_flk=Z_, T_B1_n_flk=Z_        ! Temperature at the bottom of the upper layer of the sediments [K]   

!  Thickness of various layers at the previous time step ("p") and the updated values ("n") 
REAL :: h_snow_p_flk=Z_, h_snow_n_flk=Z_    ! Snow thickness [m]*
REAL :: h_ice_p_flk=Z_, h_ice_n_flk=Z_      ! Ice thickness [m]
REAL :: h_ML_p_flk=Z_, h_ML_n_flk=Z_        ! Thickness of the mixed-layer [m] 
REAL :: H_B1_p_flk=Z_, H_B1_n_flk=Z_        ! Thickness of the upper layer of bottom sediments [m]   

!  The shape factor(s) at the previous time step ("p") and the updated value(s) ("n") 
REAL :: C_T_p_flk=Z_, C_T_n_flk=Z_          ! Shape factor (thermocline)
REAL :: C_TT_flk=Z_                      ! Dimensionless parameter (thermocline)
REAL :: C_Q_flk=Z_                       ! Shape factor with respect to the heat flux (thermocline)
REAL :: C_I_flk=Z_                       ! Shape factor (ice)
REAL :: C_S_flk=Z_                       ! Shape factor (snow) 

!  Derivatives of the shape functions
REAL :: Phi_T_pr0_flk=Z_                 ! d\Phi_T(0)/d\zeta   (thermocline)
REAL :: Phi_I_pr0_flk=Z_                 ! d\Phi_I(0)/d\zeta_I (ice)
REAL :: Phi_I_pr1_flk=Z_                 ! d\Phi_I(1)/d\zeta_I (ice)
REAL :: Phi_S_pr0_flk=Z_                 ! d\Phi_S(0)/d\zeta_S (snow)  

!  Heat and radiation fluxes
REAL :: Q_snow_flk=Z_                    ! Heat flux through the air-snow interface [W m^{-2}]
REAL :: Q_ice_flk=Z_                     ! Heat flux through the snow-ice or air-ice interface [W m^{-2}]
REAL :: Q_w_flk=Z_                       ! Heat flux through the ice-water or air-water interface [W m^{-2}]
REAL :: Q_bot_flk=Z_                     ! Heat flux through the water-bottom sediment interface [W m^{-2}]
REAL :: I_atm_flk=Z_                     ! Radiation flux at the lower boundary of the atmosphere [W m^{-2}],
                                    ! i.e. the incident radiation flux with no regard for the surface albedo.
REAL :: I_snow_flk=Z_                    ! Radiation flux through the air-snow interface [W m^{-2}]
REAL :: I_ice_flk=Z_                     ! Radiation flux through the snow-ice or air-ice interface [W m^{-2}]
REAL :: I_w_flk=Z_                       ! Radiation flux through the ice-water or air-water interface [W m^{-2}]
REAL :: I_h_flk=Z_                       ! Radiation flux through the mixed-layer-thermocline interface [W m^{-2}]
REAL :: I_bot_flk=Z_                     ! Radiation flux through the water-bottom sediment interface [W m^{-2}]
REAL :: I_intm_0_h_flk=Z_                ! Mean radiation flux over the mixed layer [W m^{-1}]
REAL :: I_intm_h_D_flk=Z_                ! Mean radiation flux over the thermocline [W m^{-1}]
REAL :: Q_star_flk=Z_                        ! A generalized heat flux scale [W m^{-2}]  

!  Velocity scales
REAL :: u_star_w_flk=Z_                  ! Friction velocity in the surface layer of lake water [m s^{-1}]
REAL :: w_star_sfc_flk=Z_                 ! Convective velocity scale,   
                                    ! using a generalized heat flux scale [m s^{-1}]

!  The rate of snow accumulation
REAL :: dMsnowdt_flk=Z_                      ! The rate of snow accumulation [kg m^{-2} s^{-1}]  

!==============================================================================
! Procedures 
!==============================================================================

CONTAINS

!==============================================================================
!  The codes of the FLake procedures are stored in separate "*.incf" files
!  and are included below.
!------------------------------------------------------------------------------

!==============================================================================
! For SURFEX needs, separate *.incf files are explicitly expanded
!==============================================================================
!SURFEX include 'flake_radflux.incf'
! File %M% from Library %Q%
! Version %I% from %G% extracted: %H%
!------------------------------------------------------------------------------

SUBROUTINE flake_radflux(depth_w,albedo,opticpar_water,opticpar_ice,opticpar_snow)         

!------------------------------------------------------------------------------
!
! Description:
!
!  Computes the radiation fluxes 
!  at the snow-ice, ice-water, air-water, 
!  mixed layer-thermocline and water column-bottom sediment interfaces,
!  the mean radiation flux over the mixed layer,
!  and the mean radiation flux over the thermocline.
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

!_dm Parameters are USEd in module "flake".
!_nu USE modd_data_parameters , ONLY : &
!_nu     ireals,                  & ! KIND-type parameter for real variables
!_nu     iintegers                  ! KIND-type parameter for "normal" integer variables

USE modd_flake_derivedtypes          ! Definitions of derived TYPEs

USE modd_flake_parameters , ONLY :  &
    h_Snow_min_flk            ,  &! Minimum snow thickness [m]
    h_Ice_min_flk             ,  &! Minimum ice thickness [m]
    h_ML_min_flk                  ! Minimum mixed-layer depth [m]  

!==============================================================================

IMPLICIT NONE

!==============================================================================
!
! Declarations

!  Input (procedure arguments)

REAL, INTENT(IN) ::   &
    depth_w                           ,  &! The lake depth [m]
    albedo                                ! Albedo of all surfaces

TYPE (opticpar_medium), INTENT(IN) ::  &
    opticpar_water                    ,  &! Optical characteristics of water
    opticpar_ice                      ,  &! Optical characteristics of ice
    opticpar_snow                         ! Optical characteristics of snow   


!  Local variables of type INTEGER
INTEGER  ::  &! Help variable(s)
    i                             ! DO loop index  
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!==============================================================================
!  Start calculations
!------------------------------------------------------------------------------

  IF (LHOOK) CALL DR_HOOK('FLAKE:FLAKE_RADFLUX',0,ZHOOK_HANDLE)
  IF(h_ice_p_flk.GE.h_Ice_min_flk) THEN            ! Ice exists
    IF(h_snow_p_flk.GE.h_Snow_min_flk) THEN        ! There is snow above the ice
      I_snow_flk = I_atm_flk*(1.-albedo) 
      I_bot_flk = 0.
      DO i=1, opticpar_snow%nband_optic
        I_bot_flk = I_bot_flk +                     &
          opticpar_snow%frac_optic(i)*EXP(-opticpar_snow%extincoef_optic(i)*h_snow_p_flk)   
      END DO 
      I_ice_flk  = I_snow_flk*I_bot_flk
    ELSE                                           ! No snow above the ice 
      I_snow_flk = I_atm_flk  
      I_ice_flk  = I_atm_flk*(1.-albedo)
    END IF 
    I_bot_flk = 0.
    DO i=1, opticpar_ice%nband_optic
      I_bot_flk = I_bot_flk +                       &
        opticpar_ice%frac_optic(i)*EXP(-opticpar_ice%extincoef_optic(i)*h_ice_p_flk)   
    END DO 
    I_w_flk      = I_ice_flk*I_bot_flk
  ELSE                                             ! No ice-snow cover
    I_snow_flk   = I_atm_flk  
    I_ice_flk    = I_atm_flk
    I_w_flk      = I_atm_flk*(1.-albedo)
  END IF 

  IF(h_ML_p_flk.GE.h_ML_min_flk) THEN           ! Radiation flux at the bottom of the mixed layer
    I_bot_flk = 0.
    DO i=1, opticpar_water%nband_optic
      I_bot_flk = I_bot_flk +             &
        opticpar_water%frac_optic(i)*EXP(-opticpar_water%extincoef_optic(i)*h_ML_p_flk)   
    END DO 
    I_h_flk = I_w_flk*I_bot_flk
  ELSE                                          ! Mixed-layer depth is less then a minimum value
    I_h_flk = I_w_flk
  END IF

  I_bot_flk = 0.                       ! Radiation flux at the lake bottom
  DO i=1, opticpar_water%nband_optic
    I_bot_flk = I_bot_flk +               &
      opticpar_water%frac_optic(i)*EXP(-opticpar_water%extincoef_optic(i)*depth_w)   
  END DO 
  I_bot_flk = I_w_flk*I_bot_flk

  IF(h_ML_p_flk.GE.h_ML_min_flk) THEN           ! Integral-mean radiation flux over the mixed layer
    I_intm_0_h_flk = 0.
    DO i=1, opticpar_water%nband_optic
      I_intm_0_h_flk = I_intm_0_h_flk +                                &
        opticpar_water%frac_optic(i)/opticpar_water%extincoef_optic(i)*  &
        (1. - EXP(-opticpar_water%extincoef_optic(i)*h_ML_p_flk))  
    END DO 
    I_intm_0_h_flk = I_w_flk*I_intm_0_h_flk/h_ML_p_flk
  ELSE
    I_intm_0_h_flk = I_h_flk
  END IF

  IF(h_ML_p_flk.LE.depth_w-h_ML_min_flk) THEN   ! Integral-mean radiation flux over the thermocline
    I_intm_h_D_flk = 0. 
    DO i=1, opticpar_water%nband_optic
      I_intm_h_D_flk = I_intm_h_D_flk +                                &
        opticpar_water%frac_optic(i)/opticpar_water%extincoef_optic(i)*  &
        ( EXP(-opticpar_water%extincoef_optic(i)*h_ML_p_flk)             &
        - EXP(-opticpar_water%extincoef_optic(i)*depth_w) )  
    END DO 
    I_intm_h_D_flk = I_w_flk*I_intm_h_D_flk/(depth_w-h_ML_p_flk)
  ELSE
    I_intm_h_D_flk = I_h_flk
  END IF
IF (LHOOK) CALL DR_HOOK('FLAKE:FLAKE_RADFLUX',1,ZHOOK_HANDLE)

!------------------------------------------------------------------------------
!  End calculations
!==============================================================================

END SUBROUTINE flake_radflux


!==============================================================================

!==============================================================================
!SURFEX include 'flake_driver.incf'
! File %M% from Library %Q%
! Version %I% from %G% extracted: %H%
!------------------------------------------------------------------------------

SUBROUTINE flake_driver ( depth_w, depth_bs, T_bs, par_Coriolis,       &
                            extincoef_water_typ,                         &
                            del_time, T_sfc_p, T_sfc_n )           

!------------------------------------------------------------------------------
!
! Description:
!
!  The main driving routine of the lake model FLake 
!  where computations are performed.
!  Advances the surface temperature
!  and other FLake variables one time step.
!  At the moment, the Euler explicit scheme is used.
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

!_dm Parameters are USEd in module "flake".
!_nu USE modd_data_parameters , ONLY : &
!_nu     ireals,                  & ! KIND-type parameter for real variables
!_nu     iintegers                  ! KIND-type parameter for "normal" integer variables

USE modd_flake_parameters            ! Thermodynamic parameters and dimensionless constants of FLake

USE modd_flake_configure             ! Switches and parameters that configure FLake

!==============================================================================

IMPLICIT NONE

!==============================================================================
!
! Declarations

!  Input (procedure arguments)

REAL, INTENT(IN) ::   &
    depth_w                           ,  &! The lake depth [m]
    depth_bs                          ,  &! Depth of the thermally active layer of bottom sediments [m]
    T_bs                              ,  &! Temperature at the outer edge of 
                                        ! the thermally active layer of bottom sediments [K]
    par_Coriolis                      ,  &! The Coriolis parameter [s^{-1}]
    extincoef_water_typ               ,  &! "Typical" extinction coefficient of the lake water [m^{-1}],
                                        ! used to compute the equilibrium CBL depth
    del_time                          ,  &! The model time step [s]
    T_sfc_p                               ! Surface temperature at the previous time step [K]    
                                        ! (equal to either T_ice, T_snow or to T_wML)

!  Output (procedure arguments)

REAL, INTENT(OUT) ::  &
    T_sfc_n                               ! Updated surface temperature [K]   
                                        ! (equal to the updated value of either T_ice, T_snow or T_wML)


!  Local variables of type LOGICAL
LOGICAL ::          &
    l_ice_create    ,  &! Switch, .TRUE. = ice does not exist but should be created
    l_snow_exists   ,  &! Switch, .TRUE. = there is snow above the ice
    l_ice_meltabove     ! Switch, .TRUE. = snow/ice melting from above takes place  

!  Local variables of type INTEGER
INTEGER  :: &
    i                             ! Loop index  

!  Local variables of type REAL
REAL ::    &
    d_T_mnw_dt             ,  &! Time derivative of T_mnw [K s^{-1}] 
    d_T_ice_dt             ,  &! Time derivative of T_ice [K s^{-1}] 
    d_T_bot_dt             ,  &! Time derivative of T_bot [K s^{-1}] 
    d_T_B1_dt              ,  &! Time derivative of T_B1 [K s^{-1}] 
    d_h_snow_dt            ,  &! Time derivative of h_snow [m s^{-1}]
    d_h_ice_dt             ,  &! Time derivative of h_ice [m s^{-1}]
    d_h_ML_dt              ,  &! Time derivative of h_ML [m s^{-1}]
    d_H_B1_dt              ,  &! Time derivative of H_B1 [m s^{-1}]
    d_C_T_dt                   ! Time derivative of C_T [s^{-1}]  

!  Local variables of type REAL
REAL ::    &
    N_T_mean               ,  &! The mean buoyancy frequency in the thermocline [s^{-1}] 
    ZM_h_scale             ,  &! The ZM96 equilibrium SBL depth scale [m] 
    conv_equil_h_scale         ! The equilibrium CBL depth scale [m]  

!  Local variables of type REAL
REAL :: &
    h_ice_threshold     ,  &! If h_ice<h_ice_threshold, use quasi-equilibrium ice model 
    flk_str_1           ,  &! Help storage variable
    flk_str_2           ,  &! Help storage variable
    R_H_icesnow         ,  &! Dimensionless ratio, used to store intermediate results
    R_rho_c_icesnow     ,  &! Dimensionless ratio, used to store intermediate results
    R_TI_icesnow        ,  &! Dimensionless ratio, used to store intermediate results
    R_Tstar_icesnow         ! Dimensionless ratio, used to store intermediate results  
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!rsal
!REAL :: aux1,aux2,aux3,aux4,aux5

!==============================================================================
!  Start calculations
!------------------------------------------------------------------------------

!_dm 
! Security. Set time-rate-of-change of prognostic variables to zero.
! Set prognostic variables to their values at the previous time step.
! (This is to avoid spurious changes of prognostic variables 
! when FLake is used within a 3D model, e.g. to avoid spurious generation of ice 
! at the neighbouring lake points as noticed by Burkhardt Rockel.)
!_dm 


IF (LHOOK) CALL DR_HOOK('FLAKE:FLAKE_DRIVER',0,ZHOOK_HANDLE)
d_T_mnw_dt   = 0. 
d_T_ice_dt   = 0. 
d_T_bot_dt   = 0. 
d_T_B1_dt    = 0. 
d_h_snow_dt  = 0. 
d_h_ice_dt   = 0. 
d_h_ML_dt    = 0. 
d_H_B1_dt    = 0. 
d_C_T_dt     = 0. 
T_snow_n_flk = T_snow_p_flk   
T_ice_n_flk  = T_ice_p_flk    
T_wML_n_flk  = T_wML_p_flk   
T_mnw_n_flk  = T_mnw_p_flk     
T_bot_n_flk  = T_bot_p_flk  
T_B1_n_flk   = T_B1_p_flk      
h_snow_n_flk = h_snow_p_flk 
h_ice_n_flk  = h_ice_p_flk   
h_ML_n_flk   = h_ML_p_flk    
H_B1_n_flk   = H_B1_p_flk   
C_T_n_flk    = C_T_p_flk    

!------------------------------------------------------------------------------
!  Compute fluxes, using variables from the previous time step.
!------------------------------------------------------------------------------

!_dm
! At this point, the heat and radiation fluxes, namely,
! Q_snow_flk, Q_ice_flk, Q_w_flk, 
! I_atm_flk, I_snow_flk, I_ice_flk, I_w_flk, I_h_flk, I_bot_flk,     
! the mean radiation flux over the mixed layer, I_intm_0_h_flk, 
! and the mean radiation flux over the thermocline, I_intm_h_D_flk, 
! should be known.
! They are computed within "flake_interface" (or within the driving model)
! and are available to "flake_driver"
! through the above variables declared in the MODULE "flake".
! In case a lake is ice-covered, Q_w_flk is re-computed below.
!_dm

! Heat flux through the ice-water interface
IF(h_ice_p_flk.GE.h_Ice_min_flk) THEN    ! Ice exists 
  IF(h_ML_p_flk.LE.h_ML_min_flk) THEN    ! Mixed-layer depth is zero, compute flux 
    Q_w_flk = -tpl_kappa_w*(T_bot_p_flk-T_wML_p_flk)/depth_w  ! Flux with linear T(z) 
    Phi_T_pr0_flk = Phi_T_pr0_1*C_T_p_flk-Phi_T_pr0_2         ! d\Phi(0)/d\zeta (thermocline)
    Q_w_flk = Q_w_flk*MAX(Phi_T_pr0_flk, 1.)           ! Account for an increased d\Phi(0)/d\zeta 
  ELSE                    
    Q_w_flk = 0.                  ! Mixed-layer depth is greater than zero, set flux to zero
  END IF   
END IF   

! A generalized heat flux scale 
Q_star_flk = Q_w_flk + I_w_flk + I_h_flk - 2.*I_intm_0_h_flk

! Heat flux through the water-bottom sediment interface
IF(lflk_botsed_use) THEN
  Q_bot_flk = -tpl_kappa_w*(T_B1_p_flk-T_bot_p_flk)/MAX(H_B1_p_flk, H_B1_min_flk)*Phi_B1_pr0
ELSE  
  Q_bot_flk = 0.   ! The bottom-sediment scheme is not used
END IF


!------------------------------------------------------------------------------
!  Check if ice exists or should be created.
!  If so, compute the thickness and the temperature of ice and snow.
!------------------------------------------------------------------------------

!_dm
! Notice that a quasi-equilibrium ice-snow model is used 
! to avoid numerical instability when the ice is thin.
! This is always the case when new ice is created.
!_dm

!_dev
! The dependence of snow density and of snow heat conductivity 
! on the snow thickness is accounted for parametrically.
! That is, the time derivatives of \rho_S and \kappa_S are neglected.
! The exception is the equation for the snow thickness 
! in case of snow accumulation and no melting, 
! where d\rho_S/dt is incorporated.
! Furthermore, some (presumably small) correction terms incorporating 
! the snow density and the snow heat conductivity are dropped out.
! Those terms may be included as better formulations 
! for \rho_S and \kappa_S are available.
!_dev

! Default values
l_ice_create    = .FALSE.  
l_ice_meltabove = .FALSE.  

Ice_exist: IF(h_ice_p_flk.LT.h_Ice_min_flk) THEN   ! Ice does not exist 

  l_ice_create = T_wML_p_flk.LE.(tpl_T_f+c_small_flk).AND.Q_w_flk.LT.0.
  IF(l_ice_create) THEN                            ! Ice does not exist but should be created
    d_h_ice_dt = -Q_w_flk/tpl_rho_I/tpl_L_f                                  
    h_ice_n_flk = h_ice_p_flk + d_h_ice_dt*del_time                          ! Advance h_ice 
    T_ice_n_flk = tpl_T_f + h_ice_n_flk*Q_w_flk/tpl_kappa_I/Phi_I_pr0_lin    ! Ice temperature
    d_h_snow_dt = dMsnowdt_flk/tpl_rho_S_min 
    h_snow_n_flk = h_snow_p_flk + d_h_snow_dt*del_time                       ! Advance h_snow
    Phi_I_pr1_flk = Phi_I_pr1_lin                                     &
                    + Phi_I_ast_MR*MIN(1., h_ice_n_flk/H_Ice_max)       ! d\Phi_I(1)/d\zeta_I (ice)  
    R_H_icesnow = Phi_I_pr1_flk/Phi_S_pr0_lin*tpl_kappa_I/flake_snowheatconduct(h_snow_n_flk) &
                  * h_snow_n_flk/MAX(h_ice_n_flk, h_Ice_min_flk)  
    T_snow_n_flk = T_ice_n_flk + R_H_icesnow*(T_ice_n_flk-tpl_T_f)           ! Snow temperature
  ELSE !kk, 30092004 - to avoid unsertainty here
   h_ice_n_flk = h_ice_p_flk
   h_snow_n_flk = h_snow_p_flk
   T_ice_n_flk = T_ice_p_flk
   T_snow_n_flk = T_snow_p_flk    
  END IF

ELSE Ice_exist                                     ! Ice exists

  l_snow_exists = h_snow_p_flk.GE.h_Snow_min_flk   ! Check if there is snow above the ice

  Melting: IF(T_snow_p_flk.GE.(tpl_T_f-c_small_flk)) THEN  ! T_sfc = T_f, check for melting from above
                                                           ! T_snow = T_ice if snow is absent 
    IF(l_snow_exists) THEN   ! There is snow above the ice
      flk_str_1 = Q_snow_flk + I_snow_flk - I_ice_flk        ! Atmospheric forcing
      IF(flk_str_1.GE.0.) THEN  ! Melting of snow and ice from above
        l_ice_meltabove = .TRUE.
        d_h_snow_dt = (-flk_str_1/tpl_L_f+dMsnowdt_flk)/flake_snowdensity(h_snow_p_flk)
        d_h_ice_dt  = -(I_ice_flk - I_w_flk - Q_w_flk)/tpl_L_f/tpl_rho_I 
      END IF 
    ELSE                     ! No snow above the ice
      flk_str_1 = Q_ice_flk + I_ice_flk - I_w_flk - Q_w_flk  ! Atmospheric forcing + heating from the water
      IF(flk_str_1.GE.0.) THEN  ! Melting of ice from above, snow accumulation may occur
        l_ice_meltabove = .TRUE.
        d_h_ice_dt  = -flk_str_1/tpl_L_f/tpl_rho_I 
        d_h_snow_dt = dMsnowdt_flk/tpl_rho_S_min
      END IF 
    END IF 
    IF(l_ice_meltabove) THEN  ! Melting from above takes place
      h_ice_n_flk  = h_ice_p_flk  + d_h_ice_dt *del_time  ! Advance h_ice
      h_snow_n_flk = h_snow_p_flk + d_h_snow_dt*del_time  ! Advance h_snow
      T_ice_n_flk  = tpl_T_f                              ! Set T_ice to the freezing point
      T_snow_n_flk = tpl_T_f                              ! Set T_snow to the freezing point
    END IF

  END IF Melting

  No_Melting: IF(.NOT.l_ice_meltabove) THEN                 ! No melting from above

    d_h_snow_dt = flake_snowdensity(h_snow_p_flk)  
    IF(d_h_snow_dt.LT.tpl_rho_S_max) THEN    ! Account for d\rho_S/dt
     flk_str_1 = h_snow_p_flk*tpl_Gamma_rho_S/tpl_rho_w_r
     flk_str_1 = flk_str_1/(1.-flk_str_1)
    ELSE                                     ! Snow density is equal to its maximum value, d\rho_S/dt=0
     flk_str_1 = 0.
    END IF
    d_h_snow_dt = dMsnowdt_flk/d_h_snow_dt/(1.+flk_str_1)       ! Snow accumulation
    h_snow_n_flk = h_snow_p_flk + d_h_snow_dt*del_time                         ! Advance h_snow
    
    Phi_I_pr0_flk = h_ice_p_flk/H_Ice_max                              ! h_ice relative to its maximum value
    C_I_flk = C_I_lin - C_I_MR*(1.+Phi_I_ast_MR)*Phi_I_pr0_flk  ! Shape factor (ice)
    Phi_I_pr1_flk = Phi_I_pr1_lin + Phi_I_ast_MR*Phi_I_pr0_flk         ! d\Phi_I(1)/d\zeta_I (ice)
    Phi_I_pr0_flk = Phi_I_pr0_lin - Phi_I_pr0_flk                      ! d\Phi_I(0)/d\zeta_I (ice)

    h_ice_threshold = MAX(1., 2.*C_I_flk*tpl_c_I*(tpl_T_f-T_ice_p_flk)/tpl_L_f)
    h_ice_threshold = Phi_I_pr0_flk/C_I_flk*tpl_kappa_I/tpl_rho_I/tpl_c_I*h_ice_threshold
    h_ice_threshold = SQRT(h_ice_threshold*del_time)                   ! Threshold value of h_ice
    h_ice_threshold = MIN(0.9*H_Ice_max, MAX(h_ice_threshold, h_Ice_min_flk))
                                                                       ! h_ice(threshold) < 0.9*H_Ice_max

    IF(h_ice_p_flk.LT.h_ice_threshold) THEN  ! Use a quasi-equilibrium ice model

      IF(l_snow_exists) THEN   ! Use fluxes at the air-snow interface
        flk_str_1 = Q_snow_flk + I_snow_flk - I_w_flk
      ELSE                     ! Use fluxes at the air-ice interface
        flk_str_1 = Q_ice_flk + I_ice_flk - I_w_flk
      END IF
      d_h_ice_dt = -(flk_str_1-Q_w_flk)/tpl_L_f/tpl_rho_I
      h_ice_n_flk = h_ice_p_flk + d_h_ice_dt *del_time                         ! Advance h_ice
      T_ice_n_flk = tpl_T_f + h_ice_n_flk*flk_str_1/tpl_kappa_I/Phi_I_pr0_flk  ! Ice temperature

    ELSE                                     ! Use a complete ice model

      d_h_ice_dt = tpl_kappa_I*(tpl_T_f-T_ice_p_flk)/h_ice_p_flk*Phi_I_pr0_flk
      d_h_ice_dt = (Q_w_flk+d_h_ice_dt)/tpl_L_f/tpl_rho_I
      h_ice_n_flk = h_ice_p_flk  + d_h_ice_dt*del_time                         ! Advance h_ice

      R_TI_icesnow = tpl_c_I*(tpl_T_f-T_ice_p_flk)/tpl_L_f         ! Dimensionless parameter
      R_Tstar_icesnow = 1. - C_I_flk                        ! Dimensionless parameter
      IF(l_snow_exists) THEN  ! There is snow above the ice
        R_H_icesnow = Phi_I_pr1_flk/Phi_S_pr0_lin*tpl_kappa_I/flake_snowheatconduct(h_snow_p_flk) &
                      * h_snow_p_flk/h_ice_p_flk  
        R_rho_c_icesnow = flake_snowdensity(h_snow_p_flk)*tpl_c_S/tpl_rho_I/tpl_c_I 
!_dev 
!_dm 
! These terms should be included as an improved understanding of the snow scheme is gained, 
! of the effect of snow density in particular. 
!_dm 
!_nu        R_Tstar_icesnow = R_Tstar_icesnow                                                           &
!_nu                        + (1.+C_S_lin*h_snow_p_flk/h_ice_p_flk)*R_H_icesnow*R_rho_c_icesnow
!_dev

        R_Tstar_icesnow = R_Tstar_icesnow*R_TI_icesnow             ! Dimensionless parameter

!_dev
!_nu        R_Tstar_icesnow = R_Tstar_icesnow                                                         &
!_nu                        + (1.-R_rho_c_icesnow)*tpl_c_I*T_ice_p_flk/tpl_L_f
!_dev
        flk_str_2 = Q_snow_flk+I_snow_flk-I_w_flk                  ! Atmospheric fluxes
        flk_str_1  = C_I_flk*h_ice_p_flk + (1.+C_S_lin*R_H_icesnow)*R_rho_c_icesnow*h_snow_p_flk
        d_T_ice_dt = -(1.-2.*C_S_lin)*R_H_icesnow*(tpl_T_f-T_ice_p_flk)              &
                     * tpl_c_S*dMsnowdt_flk                          ! Effect of snow accumulation  
      ELSE                    ! No snow above the ice
        R_Tstar_icesnow = R_Tstar_icesnow*R_TI_icesnow             ! Dimensionless parameter
        flk_str_2 = Q_ice_flk+I_ice_flk-I_w_flk                    ! Atmospheric fluxes
        flk_str_1  = C_I_flk*h_ice_p_flk
        d_T_ice_dt = 0.
      END IF 
      d_T_ice_dt = d_T_ice_dt + tpl_kappa_I*(tpl_T_f-T_ice_p_flk)/h_ice_p_flk*Phi_I_pr0_flk       &
                   * (1.-R_Tstar_icesnow)                     ! Add flux due to heat conduction  
      d_T_ice_dt = d_T_ice_dt - R_Tstar_icesnow*Q_w_flk            ! Add flux from water to ice
      d_T_ice_dt = d_T_ice_dt + flk_str_2                          ! Add atmospheric fluxes
      d_T_ice_dt = d_T_ice_dt/tpl_rho_I/tpl_c_I                    ! Total forcing
      d_T_ice_dt = d_T_ice_dt/flk_str_1                            ! dT_ice/dt 
      T_ice_n_flk = T_ice_p_flk + d_T_ice_dt*del_time                          ! Advance T_ice
    END IF

    Phi_I_pr1_flk = MIN(1., h_ice_n_flk/H_Ice_max)          ! h_ice relative to its maximum value
    Phi_I_pr1_flk = Phi_I_pr1_lin + Phi_I_ast_MR*Phi_I_pr1_flk     ! d\Phi_I(1)/d\zeta_I (ice)
    R_H_icesnow = Phi_I_pr1_flk/Phi_S_pr0_lin*tpl_kappa_I/flake_snowheatconduct(h_snow_n_flk) &
                   *h_snow_n_flk/MAX(h_ice_n_flk, h_Ice_min_flk)  
    T_snow_n_flk = T_ice_n_flk + R_H_icesnow*(T_ice_n_flk-tpl_T_f)             ! Snow temperature

  END IF No_Melting

END IF Ice_exist   

! Security, limit h_ice by its maximum value
h_ice_n_flk = MIN(h_ice_n_flk, H_Ice_max)      

! Security, limit the ice and snow temperatures by the freezing point 
T_snow_n_flk = MIN(T_snow_n_flk, tpl_T_f)  
T_ice_n_flk =  MIN(T_ice_n_flk,  tpl_T_f)    

!_tmp
! Security, avoid too low values (these constraints are used for debugging purposes)
! not good must be deleteted !!!!!!! But the snow scheme is not robust !!!!!!!
! not good must be deleteted !!!!!!! But the snow scheme is not robust !!!!!!!
! not good must be deleteted !!!!!!! But the snow scheme is not robust !!!!!!!
  T_snow_n_flk = MAX(T_snow_n_flk, 200.15)  
  T_ice_n_flk =  MAX(T_ice_n_flk,  200.15)    
!_tmp

! Remove too thin ice and/or snow
IF(h_ice_n_flk.LT.h_Ice_min_flk)  THEN        ! Check ice
  h_ice_n_flk = 0.       ! Ice is too thin, remove it, and
  T_ice_n_flk = tpl_T_f         ! set T_ice to the freezing point.
  h_snow_n_flk = 0.      ! Remove snow when there is no ice, and
  T_snow_n_flk = tpl_T_f        ! set T_snow to the freezing point.
  l_ice_create = .FALSE.        ! "Exotic" case, ice has been created but proved to be too thin
ELSE IF(h_snow_n_flk.LT.h_Snow_min_flk) THEN  ! Ice exists, check snow
  h_snow_n_flk = 0.      ! Snow is too thin, remove it, 
  T_snow_n_flk = T_ice_n_flk    ! and set the snow temperature equal to the ice temperature.
END IF


!------------------------------------------------------------------------------
!  Compute the mean temperature of the water column.
!------------------------------------------------------------------------------

IF(l_ice_create) Q_w_flk = 0.     ! Ice has just been created, set Q_w to zero
d_T_mnw_dt = (Q_w_flk - Q_bot_flk + I_w_flk - I_bot_flk)/tpl_rho_w_r/tpl_c_w/depth_w
T_mnw_n_flk = T_mnw_p_flk + d_T_mnw_dt*del_time   ! Advance T_mnw
T_mnw_n_flk = MAX(T_mnw_n_flk, tpl_T_f)           ! Limit T_mnw by the freezing point 


!------------------------------------------------------------------------------
!  Compute the mixed-layer depth, the mixed-layer temperature, 
!  the bottom temperature and the shape factor
!  with respect to the temperature profile in the thermocline. 
!  Different formulations are used, depending on the regime of mixing. 
!------------------------------------------------------------------------------

HTC_Water: IF(h_ice_n_flk.GE.h_Ice_min_flk) THEN    ! Ice exists

  T_mnw_n_flk = MIN(T_mnw_n_flk, tpl_T_r) ! Limit the mean temperature under the ice by T_r 
  T_wML_n_flk = tpl_T_f                   ! The mixed-layer temperature is equal to the freezing point 

  IF(l_ice_create) THEN                  ! Ice has just been created 
    IF(h_ML_p_flk.GE.depth_w-h_ML_min_flk) THEN    ! h_ML=D when ice is created 
      h_ML_n_flk = 0.                 ! Set h_ML to zero 
      C_T_n_flk = C_T_min                    ! Set C_T to its minimum value 
    ELSE                                          ! h_ML<D when ice is created 
      h_ML_n_flk = h_ML_p_flk                ! h_ML remains unchanged 
      C_T_n_flk = C_T_p_flk                  ! C_T (thermocline) remains unchanged 
    END IF 
    T_bot_n_flk = T_wML_n_flk - (T_wML_n_flk-T_mnw_n_flk)/C_T_n_flk/(1.-h_ML_n_flk/depth_w)
                                             ! Update the bottom temperature 

  ELSE IF(T_bot_p_flk.LT.tpl_T_r) THEN   ! Ice exists and T_bot < T_r, molecular heat transfer 
    h_ML_n_flk = h_ML_p_flk                  ! h_ML remains unchanged 
    C_T_n_flk = C_T_p_flk                    ! C_T (thermocline) remains unchanged 
    T_bot_n_flk = T_wML_n_flk - (T_wML_n_flk-T_mnw_n_flk)/C_T_n_flk/(1.-h_ML_n_flk/depth_w)
                                             ! Update the bottom temperature 

  ELSE                                   ! Ice exists and T_bot = T_r, convection due to bottom heating 
    T_bot_n_flk = tpl_T_r                      ! T_bot is equal to the temperature of maximum density 
    IF(h_ML_p_flk.GE.c_small_flk) THEN   ! h_ML > 0 
      C_T_n_flk = C_T_p_flk                     ! C_T (thermocline) remains unchanged 
      h_ML_n_flk = depth_w*(1.-(T_wML_n_flk-T_mnw_n_flk)/(T_wML_n_flk-T_bot_n_flk)/C_T_n_flk)
      h_ML_n_flk = MAX(h_ML_n_flk, 0.)   ! Update the mixed-layer depth  
    ELSE                                 ! h_ML = 0 
      h_ML_n_flk = h_ML_p_flk                   ! h_ML remains unchanged 
      C_T_n_flk = (T_wML_n_flk-T_mnw_n_flk)/(T_wML_n_flk-T_bot_n_flk) 
      C_T_n_flk = MIN(C_T_max, MAX(C_T_n_flk, C_T_min)) ! Update the shape factor (thermocline)  
    END IF 
  END IF 

  T_bot_n_flk = MIN(T_bot_n_flk, tpl_T_r)    ! Security, limit the bottom temperature by T_r 

ELSE HTC_Water                                      ! Open water

! Generalised buoyancy flux scale and convective velocity scale
  flk_str_1 = flake_buoypar(T_wML_p_flk)*Q_star_flk/tpl_rho_w_r/tpl_c_w                    
  IF(flk_str_1.LT.0.) THEN       
    w_star_sfc_flk = (-flk_str_1*h_ML_p_flk)**(1./3.)  ! Convection     
  ELSE 
    w_star_sfc_flk = 0.                                       ! Neutral or stable stratification
  END IF 

!_dm
! The equilibrium depth of the CBL due to surface cooling with the volumetric heating
! is not computed as a solution to the transcendental equation.
! Instead, an algebraic formula is used
! that interpolates between the two asymptotic limits.
!_dm
  conv_equil_h_scale = -Q_w_flk/MAX(I_w_flk, c_small_flk)
  IF(conv_equil_h_scale.GT.0. .AND. conv_equil_h_scale.LT.1.  &
      .AND. T_wML_p_flk.GT.tpl_T_r) THEN   ! The equilibrium CBL depth scale is only used above T_r  
    conv_equil_h_scale = SQRT(6.*conv_equil_h_scale)                 &
                         + 2.*conv_equil_h_scale/(1.-conv_equil_h_scale)  
    conv_equil_h_scale = MIN(depth_w, conv_equil_h_scale/extincoef_water_typ)
  ELSE
    conv_equil_h_scale = 0.       ! Set the equilibrium CBL depth to zero
  END IF

! Mean buoyancy frequency in the thermocline
  N_T_mean = flake_buoypar(0.5*(T_wML_p_flk+T_bot_p_flk))*MAX(0.,(T_wML_p_flk-T_bot_p_flk))
  IF(h_ML_p_flk.LE.depth_w-h_ML_min_flk) THEN
    N_T_mean = SQRT(N_T_mean/(depth_w-h_ML_p_flk))  ! Compute N                   
  ELSE 
    N_T_mean = 0.                            ! h_ML=D, set N to zero
  END IF 

! The rate of change of C_T
  d_C_T_dt = MAX(w_star_sfc_flk, u_star_w_flk, u_star_min_flk)**2
  d_C_T_dt = N_T_mean*(depth_w-h_ML_p_flk)**2       &
             / c_relax_C/d_C_T_dt                               ! Relaxation time scale for C_T  
  d_C_T_dt = (C_T_max-C_T_min)/MAX(d_C_T_dt, c_small_flk)     ! Rate-of-change of C_T 
  !rsal:
  d_C_T_dt = min(d_C_T_dt,0.01/del_time)

! Compute the shape factor and the mixed-layer depth, 
! using different formulations for convection and wind mixing

  C_TT_flk = C_TT_1*C_T_p_flk-C_TT_2         ! C_TT, using C_T at the previous time step
  C_Q_flk = 2.*C_TT_flk/C_T_p_flk     ! C_Q using C_T at the previous time step

  Mixing_regime: IF(flk_str_1.LT.0.) THEN  ! Convective mixing 

    C_T_n_flk = C_T_p_flk + d_C_T_dt*del_time                        ! Update C_T, assuming dh_ML/dt>0
    C_T_n_flk = MIN(C_T_max, MAX(C_T_n_flk, C_T_min))                ! Limit C_T 
    d_C_T_dt = (C_T_n_flk-C_T_p_flk)/del_time                        ! Re-compute dC_T/dt

    IF(h_ML_p_flk.LE.depth_w-h_ML_min_flk) THEN       ! Compute dh_ML/dt
      IF(h_ML_p_flk.LE.h_ML_min_flk) THEN    ! Use a reduced entrainment equation (spin-up)
        d_h_ML_dt = c_cbl_1/c_cbl_2*MAX(w_star_sfc_flk, c_small_flk)

!_dbg
! WRITE*, ' FLake: reduced entrainment eq. D_time*d_h_ML_dt  = ', d_h_ML_dt*del_time
! WRITE*, '         w_*       = ', w_star_sfc_flk
! WRITE*, '         \beta*Q_* = ', flk_str_1
!_dbg

      ELSE                                   ! Use a complete entrainment equation 
        R_H_icesnow     = depth_w/h_ML_p_flk
        R_rho_c_icesnow = R_H_icesnow-1.
        R_TI_icesnow    = C_T_p_flk/C_TT_flk
        R_Tstar_icesnow = (R_TI_icesnow/2.-1.)*R_rho_c_icesnow + 1.
        d_h_ML_dt = -Q_star_flk*(R_Tstar_icesnow*(1.+c_cbl_1)-1.) - Q_bot_flk
        d_h_ML_dt = d_h_ML_dt/tpl_rho_w_r/tpl_c_w                        ! Q_* and Q_b flux terms
        flk_str_2 = (depth_w-h_ML_p_flk)*(T_wML_p_flk-T_bot_p_flk)*C_TT_2/C_TT_flk*d_C_T_dt 
        d_h_ML_dt = d_h_ML_dt + flk_str_2                                 ! Add dC_T/dt term
        flk_str_2 = I_bot_flk + (R_TI_icesnow-1.)*I_h_flk - R_TI_icesnow*I_intm_h_D_flk
        flk_str_2 = flk_str_2 + (R_TI_icesnow-2.)*R_rho_c_icesnow*(I_h_flk-I_intm_0_h_flk)
        flk_str_2 = flk_str_2/tpl_rho_w_r/tpl_c_w
        d_h_ML_dt = d_h_ML_dt + flk_str_2                                 ! Add radiation terms
        flk_str_2 = -c_cbl_2*R_Tstar_icesnow*Q_star_flk/tpl_rho_w_r/tpl_c_w/MAX(w_star_sfc_flk, c_small_flk)
        flk_str_2 = flk_str_2 + C_T_p_flk*(T_wML_p_flk-T_bot_p_flk)
        d_h_ML_dt = d_h_ML_dt/flk_str_2                                   ! dh_ML/dt = r.h.s.
      END IF 
!_dm
! Notice that dh_ML/dt may appear to be negative  
! (e.g. due to buoyancy loss to bottom sediments and/or
! the effect of volumetric radiation heating),
! although a negative generalized buoyancy flux scale indicates 
! that the equilibrium CBL depth has not yet been reached
! and convective deepening of the mixed layer should take place.
! Physically, this situation reflects an approximate character of the lake model.
! Using the self-similar temperature profile in the thermocline, 
! there is always communication between the mixed layer, the thermocline 
! and the lake bottom. As a result, the rate of change of the CBL depth
! is always dependent on the bottom heat flux and the radiation heating of the thermocline.
! In reality, convective mixed-layer deepening may be completely decoupled
! from the processes underneath. In order to account for this fact,
! the rate of CBL deepening is set to a small value
! if dh_ML/dt proves to be negative.
! This is "double insurance" however, 
! as a negative dh_ML/dt is encountered very rarely.
!_dm

      d_h_ML_dt = MAX(d_h_ML_dt, c_small_flk)    
      h_ML_n_flk = h_ML_p_flk + d_h_ML_dt*del_time                       ! Update h_ML 
      h_ML_n_flk = MAX(h_ML_min_flk, MIN(h_ML_n_flk, depth_w))           ! Security, limit h_ML
    ELSE                                              ! Mixing down to the lake bottom
      h_ML_n_flk = depth_w
    END IF

  ELSE Mixing_regime                              ! Wind mixing

    d_h_ML_dt = MAX(u_star_w_flk, u_star_min_flk)                        ! The surface friction velocity
    ZM_h_scale = (ABS(par_Coriolis)/c_sbl_ZM_n + N_T_mean/c_sbl_ZM_i)*d_h_ML_dt**2
    ZM_h_scale = ZM_h_scale + flk_str_1/c_sbl_ZM_s
    ZM_h_scale = MAX(ZM_h_scale, c_small_flk)
    ZM_h_scale = d_h_ML_dt**3/ZM_h_scale 
    ZM_h_scale = MAX(h_ML_min_flk, MIN(ZM_h_scale, h_ML_max_flk))        ! The ZM96 SBL depth scale 
    ZM_h_scale = MAX(ZM_h_scale, conv_equil_h_scale)                     ! Equilibrium mixed-layer depth 

!_dm 
! In order to avoid numerical discretization problems,
! an analytical solution to the evolution equation 
! for the wind-mixed layer depth is used.
! That is, an exponential relaxation formula is applied
! over the time interval equal to the model time step.
!_dm 

    d_h_ML_dt = c_relax_h*d_h_ML_dt/ZM_h_scale*del_time
    h_ML_n_flk = ZM_h_scale - (ZM_h_scale-h_ML_p_flk)*EXP(-d_h_ML_dt)    ! Update h_ML 
    h_ML_n_flk = MAX(h_ML_min_flk, MIN(h_ML_n_flk, depth_w))             ! Limit h_ML 
    d_h_ML_dt = (h_ML_n_flk-h_ML_p_flk)/del_time                         ! Re-compute dh_ML/dt

    IF(h_ML_n_flk.LE.h_ML_p_flk)           &
        d_C_T_dt = -d_C_T_dt                 ! Mixed-layer retreat or stationary state, dC_T/dt<0  
    C_T_n_flk = C_T_p_flk + d_C_T_dt*del_time                            ! Update C_T
    C_T_n_flk = MIN(C_T_max, MAX(C_T_n_flk, C_T_min))                    ! Limit C_T 
    d_C_T_dt = (C_T_n_flk-C_T_p_flk)/del_time                            ! Re-compute dC_T/dt


  END IF Mixing_regime

! Compute the time-rate-of-change of the the bottom temperature, 
! depending on the sign of dh_ML/dt 
! Update the bottom temperature and the mixed-layer temperature

  IF(h_ML_n_flk.LE.depth_w-h_ML_min_flk) THEN       ! Mixing did not reach the bottom 

    IF(h_ML_n_flk.GT.h_ML_p_flk) THEN   ! Mixed-layer deepening 
      R_H_icesnow     = h_ML_p_flk/depth_w
      R_rho_c_icesnow = 1.-R_H_icesnow 
      R_TI_icesnow    = 0.5*C_T_p_flk*R_rho_c_icesnow+C_TT_flk*(2.*R_H_icesnow-1.)
      R_Tstar_icesnow = (0.5+C_TT_flk-C_Q_flk)/R_TI_icesnow
      R_TI_icesnow    = (1.-C_T_p_flk*R_rho_c_icesnow)/R_TI_icesnow
     
      d_T_bot_dt = (Q_w_flk-Q_bot_flk+I_w_flk-I_bot_flk)/tpl_rho_w_r/tpl_c_w
      d_T_bot_dt = d_T_bot_dt - C_T_p_flk*(T_wML_p_flk-T_bot_p_flk)*d_h_ML_dt
      d_T_bot_dt = d_T_bot_dt*R_Tstar_icesnow/depth_w                   ! Q+I fluxes and dh_ML/dt term

      flk_str_2 = I_intm_h_D_flk - (1.-C_Q_flk)*I_h_flk - C_Q_flk*I_bot_flk
      flk_str_2 = flk_str_2*R_TI_icesnow/(depth_w-h_ML_p_flk)/tpl_rho_w_r/tpl_c_w
      d_T_bot_dt = d_T_bot_dt + flk_str_2                               ! Add radiation-flux term

      flk_str_2 = (1.-C_TT_2*R_TI_icesnow)/C_T_p_flk
      flk_str_2 = flk_str_2*(T_wML_p_flk-T_bot_p_flk)*d_C_T_dt
      d_T_bot_dt = d_T_bot_dt + flk_str_2                               ! Add dC_T/dt term
      
    ELSE                                ! Mixed-layer retreat or stationary state
      d_T_bot_dt = 0.                                            ! dT_bot/dt=0
    END IF

    T_bot_n_flk = T_bot_p_flk + d_T_bot_dt*del_time                      ! Update T_bot  
    T_bot_n_flk = MAX(T_bot_n_flk, tpl_T_f)           ! Security, limit T_bot by the freezing point
    flk_str_2 = (T_bot_n_flk-tpl_T_r)*flake_buoypar(T_mnw_n_flk)
    IF(flk_str_2.LT.0.) T_bot_n_flk = tpl_T_r  ! Security, avoid T_r crossover 
    T_wML_n_flk = C_T_n_flk*(1.-h_ML_n_flk/depth_w)
    T_wML_n_flk = (T_mnw_n_flk-T_bot_n_flk*T_wML_n_flk)/(1.-T_wML_n_flk)
    T_wML_n_flk = MAX(T_wML_n_flk, tpl_T_f)           ! Security, limit T_wML by the freezing point

  ELSE                                              ! Mixing down to the lake bottom 

    h_ML_n_flk = depth_w
    T_wML_n_flk = T_mnw_n_flk
    T_bot_n_flk = T_mnw_n_flk
    C_T_n_flk = C_T_min
!rsal:
    C_T_n_flk = 0.65


  END IF

END IF HTC_Water

!------------------------------------------------------------------------------
!  Compute the depth of the upper layer of bottom sediments
!  and the temperature at that depth.
!------------------------------------------------------------------------------

!SURFEX
Sediment: IF(lflk_botsed_use) THEN   ! The bottom-sediment scheme is used
  
  IF(H_B1_p_flk.GE.depth_bs-H_B1_min_flk) THEN   ! No T(z) maximum (no thermal wave) 
    H_B1_p_flk = 0.                       ! Set H_B1_p to zero
    T_B1_p_flk = T_bot_p_flk                     ! Set T_B1_p to the bottom temperature
  END IF 

  flk_str_1 = 2.*Phi_B1_pr0/(1.-C_B1)*tpl_kappa_w/tpl_rho_w_r/tpl_c_w*del_time
  h_ice_threshold = SQRT(flk_str_1)                              ! Threshold value of H_B1
  h_ice_threshold = MIN(0.9*depth_bs, h_ice_threshold)    ! Limit H_B1
  flk_str_2 = C_B2/(1.-C_B2)*(T_bs-T_B1_p_flk)/(depth_bs-H_B1_p_flk)

  IF(H_B1_p_flk.LT.h_ice_threshold) THEN  ! Use a truncated equation for H_B1(t)
    H_B1_n_flk = SQRT(H_B1_p_flk**2+flk_str_1)  ! Advance H_B1
    d_H_B1_dt = (H_B1_n_flk-H_B1_p_flk)/del_time          ! Re-compute dH_B1/dt 
  ELSE                                    ! Use a full equation for H_B1(t)
    flk_str_1 = (Q_bot_flk+I_bot_flk)/H_B1_p_flk/tpl_rho_w_r/tpl_c_w
    flk_str_1 = flk_str_1 - (1.-C_B1)*(T_bot_n_flk-T_bot_p_flk)/del_time
    d_H_B1_dt = (1.-C_B1)*(T_bot_p_flk-T_B1_p_flk)/H_B1_p_flk + C_B1*flk_str_2
    d_H_B1_dt = flk_str_1/d_H_B1_dt
    H_B1_n_flk = H_B1_p_flk + d_H_B1_dt*del_time          ! Advance H_B1
  END IF 
  d_T_B1_dt = flk_str_2*d_H_B1_dt
  T_B1_n_flk = T_B1_p_flk + d_T_B1_dt*del_time            ! Advance T_B1

! Use a very simplistic procedure, where only the upper layer profile is used, 
! H_B1 is always set to depth_bs, and T_B1 is always set to T_bs.
! Then, the time derivatives are zero, and the sign of the bottom heat flux depends on 
! whether T_bot is smaller or greater than T_bs.
! This is, of course, an oversimplified scheme.

  l_snow_exists = H_B1_n_flk.GE.depth_bs-H_B1_min_flk                     &! H_B1 reached depth_bs, or
               .OR. H_B1_n_flk.LT.H_B1_min_flk                              &! H_B1 decreased to zero, or
               .OR.(T_bot_n_flk-T_B1_n_flk)*(T_bs-T_B1_n_flk).LE.0.   ! there is no T(z) maximum  
  IF(l_snow_exists) THEN      
    H_B1_n_flk = depth_bs                     ! Set H_B1 to the depth of the thermally active layer
    T_B1_n_flk = T_bs                         ! Set T_B1 to the climatological temperature 
  END IF

ELSE Sediment                        ! The bottom-sediment scheme is not used

  H_B1_n_flk = rflk_depth_bs_ref              ! H_B1 is set to a reference value 
  T_B1_n_flk = tpl_T_r                        ! T_B1 is set to the temperature of maximum density

END IF Sediment


!------------------------------------------------------------------------------
!  Impose additional constraints.
!------------------------------------------------------------------------------

! In case of unstable stratification, force mixing down to the bottom
flk_str_2 = (T_wML_n_flk-T_bot_n_flk)*flake_buoypar(T_mnw_n_flk)
IF(flk_str_2.LT.0.) THEN 

  h_ML_n_flk = depth_w
  T_wML_n_flk = T_mnw_n_flk
  T_bot_n_flk = T_mnw_n_flk
  C_T_n_flk = C_T_min
!rsal:
    C_T_n_flk = 0.65

END IF


!------------------------------------------------------------------------------
!  Update the surface temperature.
!------------------------------------------------------------------------------

IF(h_snow_n_flk.GE.h_Snow_min_flk) THEN   
  T_sfc_n = T_snow_n_flk                   ! Snow exists, use the snow temperature
ELSE IF(h_ice_n_flk.GE.h_Ice_min_flk) THEN
  T_sfc_n = T_ice_n_flk                    ! Ice exists but there is no snow, use the ice temperature
ELSE 
  T_sfc_n = T_wML_n_flk                    ! No ice-snow cover, use the mixed-layer temperature
END IF
IF (LHOOK) CALL DR_HOOK('FLAKE:FLAKE_DRIVER',1,ZHOOK_HANDLE)

!------------------------------------------------------------------------------
!  End calculations
!==============================================================================

END SUBROUTINE flake_driver


!==============================================================================

!==============================================================================
!SURFEX include 'flake_buoypar.incf'
! File %M% from Library %Q%
! Version %I% from %G% extracted: %H%
!------------------------------------------------------------------------------

!SURFEX REAL  FUNCTION flake_buoypar (T_water)
FUNCTION flake_buoypar (T_water)

!------------------------------------------------------------------------------
!
! Description:
!
!  Computes the buoyancy parameter,
!  using a quadratic equation of state for the fresh-water.
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

!_dm Parameters are USEd in module "flake".
!_nu USE modd_data_parameters , ONLY : &
!_nu     ireals,                  & ! KIND-type parameter for real variables
!_nu     iintegers                  ! KIND-type parameter for "normal" integer variables

USE modd_flake_parameters , ONLY : &
    tpl_grav                  ,  &! Acceleration due to gravity [m s^{-2}]
    tpl_T_r                   ,  &! Temperature of maximum density of fresh water [K]
    tpl_a_T                       ! Constant in the fresh-water equation of state [K^{-2}]  

!==============================================================================

IMPLICIT NONE

!==============================================================================
!
! Declarations
 
!  Input (function argument) 
REAL , INTENT(IN) :: &
    T_water                             ! Water temperature [K]  

!  Output (function result) 
REAL               :: &
    flake_buoypar                       ! Buoyancy parameter [m s^{-2} K^{-1}]  
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!==============================================================================
!  Start calculations
!------------------------------------------------------------------------------

! Buoyancy parameter [m s^{-2} K^{-1}]

  IF (LHOOK) CALL DR_HOOK('FLAKE:FLAKE_BUOYPAR',0,ZHOOK_HANDLE)
  flake_buoypar = tpl_grav*tpl_a_T*(T_water-tpl_T_r)
IF (LHOOK) CALL DR_HOOK('FLAKE:FLAKE_BUOYPAR',1,ZHOOK_HANDLE)

!------------------------------------------------------------------------------
!  End calculations
!==============================================================================

END FUNCTION flake_buoypar


!==============================================================================
!SURFEX include 'flake_snowdensity.incf'
! File %M% from Library %Q%
! Version %I% from %G% extracted: %H%
!------------------------------------------------------------------------------

!SURFEX REAL  FUNCTION flake_snowdensity (h_snow)
FUNCTION flake_snowdensity (h_snow)

!------------------------------------------------------------------------------
!
! Description:
!
!  Computes the snow density,
!  using an empirical approximation from Heise et al. (2003).
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

!_dm Parameters are USEd in module "flake".
!_nu USE modd_data_parameters , ONLY : &
!_nu     ireals,                  & ! KIND-type parameter for real variables
!_nu     iintegers                  ! KIND-type parameter for "normal" integer variables

USE modd_flake_parameters , ONLY : &
    tpl_rho_w_r               ,  &! Maximum density of fresh water [kg m^{-3}]
    tpl_rho_S_min             ,  &! Minimum snow density [kg m^{-3}]
    tpl_rho_S_max             ,  &! Maximum snow density [kg m^{-3}]
    tpl_Gamma_rho_S           ,  &! Empirical parameter [kg m^{-4}] in the expression for the snow density
    c_small_flk                   ! A small number  

!==============================================================================

IMPLICIT NONE

!==============================================================================
!
! Declarations
 
!  Input (function argument) 
REAL , INTENT(IN) :: &
    h_snow                              ! Snow thickness [m]  

!  Output (function result) 
REAL               :: &
    flake_snowdensity                   ! Snow density [kg m^{-3}]  
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!==============================================================================
!  Start calculations
!------------------------------------------------------------------------------

! Snow density [kg m^{-3}]

!  Security. Ensure that the expression in () does not become negative at a very large h_snow.
  IF (LHOOK) CALL DR_HOOK('FLAKE:FLAKE_SNOWDENSITY',0,ZHOOK_HANDLE)
  flake_snowdensity = MAX( c_small_flk, (1. - h_snow*tpl_Gamma_rho_S/tpl_rho_w_r) )
  flake_snowdensity = MIN( tpl_rho_S_max, tpl_rho_S_min/flake_snowdensity )
IF (LHOOK) CALL DR_HOOK('FLAKE:FLAKE_SNOWDENSITY',1,ZHOOK_HANDLE)

!------------------------------------------------------------------------------
!  End calculations
!==============================================================================

END FUNCTION flake_snowdensity 


!==============================================================================
!SURFEX include 'flake_snowheatconduct.incf'
! File %M% from Library %Q%
! Version %I% from %G% extracted: %H%
!------------------------------------------------------------------------------

!SURFEX REAL  FUNCTION flake_snowheatconduct (h_snow)
FUNCTION flake_snowheatconduct (h_snow)

!------------------------------------------------------------------------------
!
! Description:
!
!  Computes the snow heat conductivity,
!  using an empirical approximation from Heise et al. (2003).
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

!_dm Parameters are USEd in module "flake".
!_nu USE modd_data_parameters , ONLY : &
!_nu     ireals,                  & ! KIND-type parameter for real variables
!_nu     iintegers                  ! KIND-type parameter for "normal" integer variables

USE modd_flake_parameters , ONLY : &
    tpl_rho_w_r               ,  &! Maximum density of fresh water [kg m^{-3}]
    tpl_kappa_S_min           ,  &! Minimum molecular heat conductivity of snow [J m^{-1} s^{-1} K^{-1}]
    tpl_kappa_S_max           ,  &! Maximum molecular heat conductivity of snow [J m^{-1} s^{-1} K^{-1}]
    tpl_Gamma_kappa_S             ! Empirical parameter [J m^{-2} s^{-1} K^{-1}] in the expression for kappa_S  

!==============================================================================

IMPLICIT NONE

!==============================================================================
!
! Declarations
 
!  Input (function argument) 
REAL , INTENT(IN) :: &
    h_snow                              ! Snow thickness [m]  

!  Output (function result) 
REAL               :: &
    flake_snowheatconduct               ! Snow heat conductivity [J m^{-1} s^{-1} K^{-1} = kg m s^{-3} K^{-1}]  
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!==============================================================================
!  Start calculations
!------------------------------------------------------------------------------

! Snow heat conductivity [J m^{-1} s^{-1} K^{-1} = kg m s^{-3} K^{-1}]

  IF (LHOOK) CALL DR_HOOK('FLAKE:FLAKE_SNOWHEATCONDUCT',0,ZHOOK_HANDLE)
  flake_snowheatconduct = flake_snowdensity( h_snow )   ! Compute snow density
  flake_snowheatconduct = MIN( tpl_kappa_S_max, tpl_kappa_S_min                      &
                          + h_snow*tpl_Gamma_kappa_S*flake_snowheatconduct/tpl_rho_w_r )  
IF (LHOOK) CALL DR_HOOK('FLAKE:FLAKE_SNOWHEATCONDUCT',1,ZHOOK_HANDLE)

!------------------------------------------------------------------------------
!  End calculations
!==============================================================================

END FUNCTION flake_snowheatconduct

END MODULE mode_flake

