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
! ======================== MODULE modi_glt_oceflx_r =========================
! =======================================================================
!
! Goal:
! -----
!   Transform sea ice ablation/accretion or snow ablation into salt, 
! water and heat fluxes directed to the ocean.
!
! Method:
! -------
!   For every category of ice or snow involved in melting / freezing 
! in the grid cell, the following quantities are needed:
!    - mass (kg.m-2)
!    - massic gltools_enthalpy (J.kg-1)
!    - salt (g.kg-1) 
!   As Gelato can be coupled to rigid lid models (that need a virtual 
! freshwater flux) or a free-surface model (that need a virtual 
! freshwater flux and a water mass flux). A heat flux is also 
! computed
! 
! Remarks:
! --------
!   Note that dmass (the provided mass of melted ice or snow) are 
! in kg.m-2. In the case of sea ice:
!            (sea ice density) * (fraction) * (thickness variation).
! 
!
! Created : 2001/07 (D. Salas y Melia)
!           Taken out from imod_thermo_ice.f90 routine (rigid lid 
!           assumption is made). 
!
! Modified: 2002/10 (D. Salas y Melia)
!           Introduction of free surface approach.
!
! Modified: 2009/06 (D. Salas y Melia) reduced grid
!
! Modified: 2009/12 (D. Salas y Melia) make glt_updtfl consistent with the
!  new gltools_enthalpy formulation in Gelato.
!
! ---------------------- BEGIN MODULE modi_glt_oceflx_r ---------------------
!
!THXS_SFX!MODULE modi_glt_oceflx_r
!THXS_SFX!INTERFACE 
!THXS_SFX!!
!THXS_SFX!SUBROUTINE glt_oceflx_r( tpdom,pustar,tpmxl )
!THXS_SFX!!
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  USE modd_glt_param
!THXS_SFX!  TYPE(t_dom), DIMENSION(np), INTENT(in) ::  &
!THXS_SFX!        tpdom
!THXS_SFX!  REAL, DIMENSION(np), INTENT(in) ::  &
!THXS_SFX!        pustar
!THXS_SFX!  TYPE(t_mxl), DIMENSION(np), INTENT(inout) ::  &
!THXS_SFX!        tpmxl
!THXS_SFX!END SUBROUTINE glt_oceflx_r
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_glt_oceflx_r
!
! ---------------------- END MODULE modi_glt_oceflx_r -----------------------
!
!
!
! -----------------------------------------------------------------------
! ------------------------- SUBROUTINE glt_oceflx_r -------------------------
!
SUBROUTINE glt_oceflx_r( tpdom,pustar,tpmxl )
!
  USE modd_glt_const_thm
  USE modd_glt_const_evp     ! omega
  USE modd_types_glt
  USE modd_glt_param
  USE modi_glt_salflx_r
!
  IMPLICIT NONE
!
! .. Arguments
!
  TYPE(t_dom), DIMENSION(np), INTENT(in) ::  &
        tpdom
  REAL, DIMENSION(np), INTENT(in) ::  &
        pustar
  TYPE(t_mxl), DIMENSION(np), INTENT(inout) ::  &
        tpmxl
!
! .. Local variables
!
! G_mole_T,G_mole_S molecular diffusion terms for heat/salt
!   G_mole_T = 12.5 * 13.8d0**(2d0/3d0) - 6. = 65.9d0
!   G_mole_S = 12.5 * 2432d0**(2d0/3d0) - 6. = 2255d0
  REAL, PARAMETER ::  &
        ppg_mole_T = 65.9d0 , ppg_mole_S = 2255d0
!
! g_T,g_S turbulent exchange velocities (m/s)
!   G_turb turbulent diffusion term
  REAL, DIMENSION(np) ::  &
        zg_turb,zg_T,zg_S
!
  REAL, DIMENSION(np) ::  &
        zfcor,zustar
!
!
!
! 1. Initializations
! ===================
!
! Coriolis parameter
!
zfcor(:) = 2. * omega * ABS( sin ( tpdom(:)%lat ) )
!
! Bound ustar
!
zustar(:) = MAX( pustar,0.005 )
!
! Calculate turbulent exchange velocities zg_T, zg_S
!   G_turb = (1/k) ln (u* E n^2 / f h) + 1 / (2 E n) - (1/k)
!          = 2.5 ln ( 5300*(u*)^2/coriol ) + 9.62 - 2.5 (assuming n=1)
! Note: n could theoretically depend on the buoyancy flux
!
zg_turb(:) = 2.5d0 * LOG ( 5300.*zustar(:)*zustar(:)/zfcor(:) ) + 7.12d0
!
! Calculate exchange coefficient
!   g  = u* / ( G_turb + G_mole )
!
zg_T(:) = zustar(:) / ( zg_turb(:) + ppg_mole_T )
zg_S(:) = zustar(:) / ( zg_turb(:) + ppg_mole_S )
!
!
!
! 2. Compute ocean-ice fluxes
! ============================
!
tpmxl(:)%qoc = rhosw * cpsw * zg_T(:) *  &
  MAX( tpmxl(:)%tml-tpmxl(:)%mlf, 0. )
!
END SUBROUTINE glt_oceflx_r
!
! ----------------------- END SUBROUTINE glt_oceflx_r -----------------------
! -----------------------------------------------------------------------
