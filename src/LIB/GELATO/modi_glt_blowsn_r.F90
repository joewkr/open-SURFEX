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
! ======================== MODULE modi_glt_blowsn_r =========================
! =======================================================================
!
! Goal:
! -----
!   When the thickness of the snow cover exceeds an arbitrary value
! (here 1 m), it is assumed that all the snow over 1 m is blown to
! the leads. It should be noted this value is quite rarely met under 
! "realistic" forcing. 
! 
! Method:
! -------
!   A threshold is applied on snow thickness. Energy is perfectly 
! conserved, as the gltools_enthalpy, latent heat and water quantities 
! represented by the removed part of snow is delivered to the ocean.
!
! Created : 1998 (D. Salas y Melia) 
!           part of thermo_ice
! Modified: 2001/07 (D. Salas y Melia) 
!           taken out of thermo_ice
! Modified: 2002/10 (D. Salas y Melia)
!           energy conservation is now computed by glt_updtfl routine
! Modified: 2009/06 (D. Salas y Melia) 
!           reduced grid
! Modified: 2011/12 (A. Voldoire)
!           new ice/water fluxes interface CALL
!
! -------------------- BEGIN MODULE modi_glt_blowsn_r -----------------------
!
!THXS_SFX!MODULE modi_glt_blowsn_r
!THXS_SFX!INTERFACE 
!THXS_SFX!!
!THXS_SFX!SUBROUTINE glt_blowsn_r( tpmxl,tpsil,tptfl,tpsit )
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  USE modd_glt_param
!THXS_SFX!  TYPE(t_mxl), DIMENSION(np), INTENT(in) ::  &
!THXS_SFX!        tpmxl 
!THXS_SFX!  TYPE(t_vtp), DIMENSION(nl,nt,np), INTENT(in) ::  &
!THXS_SFX!        tpsil
!THXS_SFX!  TYPE(t_tfl), DIMENSION(np), INTENT(inout) ::  &
!THXS_SFX!        tptfl
!THXS_SFX!  TYPE(t_sit), DIMENSION(nt,np), INTENT(inout) ::  &
!THXS_SFX!        tpsit
!THXS_SFX!END SUBROUTINE glt_blowsn_r
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_glt_blowsn_r
!
! --------------------- END MODULE modi_glt_blowsn_r ------------------------
!
!
!
! -----------------------------------------------------------------------
! ------------------------ SUBROUTINE glt_blowsn_r --------------------------
!
SUBROUTINE glt_blowsn_r( tpmxl,tpsil,tptfl,tpsit )
!
  USE modd_glt_const_thm
  USE modd_types_glt
  USE modd_glt_param
  USE modi_glt_updtfl_r
!
  IMPLICIT NONE
!
  TYPE(t_mxl), DIMENSION(np), INTENT(in) ::  &
        tpmxl 
  TYPE(t_vtp), DIMENSION(nl,nt,np), INTENT(in) ::  &
        tpsil
  TYPE(t_tfl), DIMENSION(np), INTENT(inout) ::  &
        tptfl
  TYPE(t_sit), DIMENSION(nt,np), INTENT(inout) ::  &
        tpsit
!
  REAL ::  &
        zhsnm
  REAL, DIMENSION(nt,np) ::  &
        zdmsn,zent
!
!
!
! 1. Initializations
! ======================== 
!
! .. Critical snow thickness
!
  zhsnm = 1.
!
! .. Snow mass variation
!
  zdmsn(:,:) = 0.
!
!
!
! 2. Remove in excess snow
! ========================
!
! .. Compute snow mass variation (kg.m-2) and update snow thickness
!
  WHERE ( tpsit(:,:)%hsn>=zhsnm )
      zdmsn(:,:) = tpsit(:,:)%rsn *  &
        tpsit(:,:)%fsi * ( zhsnm-tpsit(:,:)%hsn )
      tpsit(:,:)%hsn = zhsnm
  ENDWHERE
!
!
!
! 3. Energy conservation
! ======================
!
! Note that the glt_updtfl_r routine will be invoked without salinity
! argument - it will then be assumed that the medium that has melted
! is without salt (snow)
!
!
! 3.1. Massic gltools_enthalpy of removed snow
! -------------------------------------
!
  zent(:,:) =  &
    SUM( tpsil(nilay+1:nl,:,:)%ent, DIM=1 )/FLOAT(nslay)
!
!
! 3.2. Update water, heat and salt fluxes affecting the ocean
! ------------------------------------------------------------
!
! .. Water, heat and salt flux due to the inclusion of the water mass
! generated by the melting of snow
!
  CALL glt_updtfl_r( 'FW2O',tpmxl,tptfl,zdmsn,pent=zent )
!
END SUBROUTINE glt_blowsn_r
!
! ---------------------- END SUBROUTINE glt_blowsn_r ------------------------
! -----------------------------------------------------------------------
