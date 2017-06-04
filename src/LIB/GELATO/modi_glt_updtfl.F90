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
! ========================= MODULE modi_glt_updtfl ==========================
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
! Modified: 2010/06 (D. Salas y Melia) this version works on full grid
!
! Modified: 2011/12 (A. Voldoire) Re-formulation of this ice/water fluxes
!           interface, depending on the source and target media involved.
!
! ----------------------- BEGIN MODULE modi_glt_updtfl ----------------------
!
!THXS_SFX!MODULE modi_glt_updtfl
!THXS_SFX!INTERFACE 
!THXS_SFX!!
!THXS_SFX!SUBROUTINE glt_updtfl( hflag,tpmxl,tptfl,pdmass,pent,psalt )
!THXS_SFX!!
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  USE modd_glt_param
!THXS_SFX!  CHARACTER(*), INTENT(in) ::  &
!THXS_SFX!        hflag  
!THXS_SFX!  TYPE(t_mxl), DIMENSION(nx,ny), INTENT(in) ::  &
!THXS_SFX!        tpmxl
!THXS_SFX!  TYPE(t_tfl), DIMENSION(nx,ny), INTENT(inout) ::  &
!THXS_SFX!        tptfl
!THXS_SFX!  REAL, DIMENSION(nt,nx,ny), INTENT(in) ::  &
!THXS_SFX!        pdmass
!THXS_SFX!  REAL, DIMENSION(nt,nx,ny), INTENT(in), OPTIONAL ::  &
!THXS_SFX!        psalt,pent
!THXS_SFX!END SUBROUTINE glt_updtfl
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_glt_updtfl
!
! ----------------------- END MODULE modi_glt_updtfl ------------------------
!
!
!
! -----------------------------------------------------------------------
! -------------------------- SUBROUTINE glt_updtfl --------------------------
!
SUBROUTINE glt_updtfl( hflag,tpmxl,tptfl,pdmass,pent,psalt )
!
  USE modd_glt_const_thm
  USE modd_types_glt
  USE modd_glt_param
  USE modi_glt_salflx
!
  IMPLICIT NONE
!
! .. Arguments
!
  CHARACTER(*), INTENT(in) ::  &
        hflag  
  TYPE(t_mxl), DIMENSION(nx,ny), INTENT(in) ::  &
        tpmxl
  TYPE(t_tfl), DIMENSION(nx,ny), INTENT(inout) ::  &
        tptfl
  REAL, DIMENSION(nt,nx,ny), INTENT(in) ::  &
        pdmass
  REAL, DIMENSION(nt,nx,ny), INTENT(in), OPTIONAL ::  &
        psalt,pent
!
! .. Local variables
!
  REAL, DIMENSION(nx,ny) ::  &
        zqm,zqsalt,zqent
  REAL, DIMENSION(nt,nx,ny) ::  &
        zsalt,zaux,zsml,zqsalt2,zent
!
!
!
! 1. Case of sea ice volume variation
! ===================================
!
! 1.1. Compute agregated mass flux
! ---------------------------------
!
! .. zqm is in kg.m-2.s-1
!
  zqm(:,:) = SUM( pdmass(:,:,:),DIM=1 ) / dtt
!
! .. Salinity of considered medium (in g.kg-1)
!
!
! 1.2. Compute agregated salt flux
! ---------------------------------
!
! .. zsalt is in g.kg-1
!
  IF ( PRESENT( psalt ) ) THEN       
      zsalt(:,:,:) = psalt(:,:,:)
    ELSE
      zsalt(:,:,:) = 0.
  ENDIF
  IF ( PRESENT( pent ) ) THEN       
      zent(:,:,:) = pent(:,:,:)
    ELSE
      zent(:,:,:) = 0.
  ENDIF
!
! .. The salt flux is expressed in g.m-2.s-1
!
  zqsalt2(:,:,:) = pdmass(:,:,:)*zsalt(:,:,:)        ! For glt_salflx routine
  zqsalt(:,:) = SUM( zqsalt2(:,:,:),DIM=1 ) / dtt
!
!
! 1.2. Compute agregated gltools_enthalpy flux
! -------------------------------------
!
! .. Note there is no gltools_enthalpy exchange if sea ice / snow mass increases
!
  zqent(:,:) = SUM( AMIN1( pdmass(:,:,:),0. )*zent(:,:,:),DIM=1 ) / dtt
!
!
!
! 2. Compute glt_output fluxes
! =========================
! 3 types_glt of exchanges :
!   'FW2O' - between fresh water and ocean
!            only a wlo flux
!   'I2O'  - between ice and the ocean
!            only a cio flux
!   'FW2I' - between fresh water and ice
!             a wio and a cio flux (cio=-wio)
!
!   Ice <-> Ocean
  IF ( hflag=='I2O' ) THEN
    IF ( nleviti==1 ) THEN
! .. Update virtual water flux (concentration/dilution) in kg.m-2.s-1 
!
      CALL glt_salflx( zqsalt2,tpmxl,tptfl,pdmass=pdmass,psalt=zsalt )
      tptfl(:,:)%sio = tptfl(:,:)%sio - 1.e-3 * zqsalt(:,:)
    ENDIF
!   Fresh Water <-> Ocean
  ELSE IF ( hflag=='FW2O') THEN
    tptfl(:,:)%wlo = tptfl(:,:)%wlo - zqm(:,:)
!   Fresh Water <-> Ice
  ELSE IF ( hflag=='FW2I') THEN
    tptfl(:,:)%wio = tptfl(:,:)%wio + zqm(:,:)
    IF ( nleviti==1 ) THEN
      tptfl(:,:)%cio = tptfl(:,:)%cio - zqm(:,:)
    ENDIF
  ENDIF

!
! .. Heat flux in W.m-2
! 
  tptfl(:,:)%tio = tptfl(:,:)%tio - zqent(:,:)
!
!
END SUBROUTINE glt_updtfl
!
! ------------------------ END SUBROUTINE glt_updtfl ------------------------
! -----------------------------------------------------------------------
