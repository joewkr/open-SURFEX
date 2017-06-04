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
! ======================== MODULE modi_glt_sublim_r =========================
! =======================================================================
!
! 
! Goal:
! -----
!   This module contains a routine manages top the sea ice-snow slab 
! sublimation.
!   The input is sublimation rate (in meters of equivalent liquid water 
! per second). Note that if there is sublimation, the corresponding rate
! is taken to be negative.
!   Snow layer thickness is modified, and if necessary (in case there
! is no more snow), the sea ice thickness.
!
! Created : 2009/06 (D. Salas y Melia)
!           Reduced grid
! Modified: 2010/01 (D. Salas y Melia)
!           Consider the removal of salt (only fresh water evaporates)
!           Salt is supposed to go to the mixed layer. Associated 
!           gltools_enthalpy changes are not treated.
! Modified: 2011/12 (A. Voldoire)
!           New formulation of the effect of sublimation on sea ice 
!           thickness and mass balance + new ice/water fluxes interface 
!           CALL
!   
! -------------------- BEGIN MODULE modi_glt_sublim_r ----------------------
!
!THXS_SFX!MODULE modi_glt_sublim_r
!THXS_SFX!INTERFACE
!THXS_SFX!!
!THXS_SFX!SUBROUTINE glt_sublim_r(tpmxl,tpblki,tpsit,tpsil,tptfl,tpdia)
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  USE modd_glt_param 
!THXS_SFX!  TYPE(t_mxl), DIMENSION(np), INTENT(in) ::   &
!THXS_SFX!        tpmxl
!THXS_SFX!  TYPE(t_blk), DIMENSION(nt,np), INTENT(in) ::  &
!THXS_SFX!        tpblki
!THXS_SFX!  TYPE(t_sit), DIMENSION(nt,np), INTENT(inout) ::  &
!THXS_SFX!        tpsit
!THXS_SFX!  TYPE(t_vtp), DIMENSION(nl,nt,np), INTENT(inout) ::  &
!THXS_SFX!        tpsil
!THXS_SFX!  TYPE(t_tfl), DIMENSION(np), INTENT(inout) ::   &
!THXS_SFX!        tptfl
!THXS_SFX!  TYPE(t_dia), DIMENSION(np), INTENT(inout) ::  &
!THXS_SFX!        tpdia
!THXS_SFX!END SUBROUTINE glt_sublim_r
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_glt_sublim_r 
!
! ---------------------- END MODULE modi_glt_sublim_r -----------------------
!
!
!
! -----------------------------------------------------------------------
! ------------------------ SUBROUTINE glt_sublim_r --------------------------
!
SUBROUTINE glt_sublim_r(tpmxl,tpblki,tpsit,tpsil,tptfl,tpdia)
  USE modd_types_glt
  USE modd_glt_param 
  USE modd_glt_const_thm
  USE modi_glt_updtfl_r
  USE mode_gltools_enthalpy
!
  IMPLICIT NONE
!
  TYPE(t_mxl), DIMENSION(np), INTENT(in) ::   &
        tpmxl
  TYPE(t_blk), DIMENSION(nt,np), INTENT(in) ::  &
        tpblki
  TYPE(t_sit), DIMENSION(nt,np), INTENT(inout) ::  &
        tpsit
  TYPE(t_vtp), DIMENSION(nl,nt,np), INTENT(inout) ::  &
        tpsil
  TYPE(t_tfl), DIMENSION(np), INTENT(inout) ::  &
        tptfl
  TYPE(t_dia), DIMENSION(np), INTENT(inout) ::  &
        tpdia
! 
  REAL, DIMENSION(np) ::  &
        zei1,zei2,zes1,zes2
  REAL, DIMENSION(nt,np) ::  &
        zhsn,zhsi,zwork,zsus,zsui,zsuw
!
!
!
! 1. Initialize local variables
! ==============================
!
! Initial snow and ice thickness
  zhsn(:,:) = tpsit(:,:)%hsn
  zhsi(:,:) = tpsit(:,:)%hsi
!
! Get initial snow and ice gltools_enthalpy
  CALL glt_aventh( tpsit,tpsil,zei1,zes1 )
!
! 
! 2. Apply sublimation rate
! ==========================
!
! 2.1. Modify the snow layer thickness
! -------------------------------------
!
! .. First try on the snow layer 
! Note that the test is performed on sea ice thickness (means where there 
! is sea ice)
! Note that if condensation takes place (tpblki%eva>0), deposition
! is allowed.
!print*,'SUBLIM...'
!print*,'hsi (1) =',tpsit(:,:)%hsi
!print*,'hsn (1) =',tpsit(:,:)%hsn
  zwork(:,:) = 0.
  WHERE( tpsit(:,:)%hsi>epsil1 )
    tpsit(:,:)%hsn = tpsit(:,:)%hsn +  &
      dtt*tpblki(:,:)%eva/tpsit(:,:)%rsn
    zwork(:,:) = tpblki(:,:)%eva*tpsit(:,:)%fsi
  ENDWHERE
  zsus(:,:) = zwork(:,:) * dtt
!print*,'hsn (2) =',tpsit(:,:)%hsn
!
!
! 2.2. Modify the ice layer (if no snow is left)
! -----------------------------------------------
!
! .. If there is no more snow, try on the ice part of the slab
!
  zwork(:,:) = 0.
  WHERE ( tpsit(:,:)%hsn<0. ) 
    tpsit(:,:)%hsi = tpsit(:,:)%hsi +  &
      tpsit(:,:)%hsn*tpsit(:,:)%rsn/rhoice
!      tpsit(:,:)%hsn*tpsit(:,:)%rsn/rhoice/  &
!        ( 1.-1.e-3*tpsit(:,:)%ssi )
    zwork(:,:) = tpsit(:,:)%hsn*tpsit(:,:)%rsn*tpsit(:,:)%fsi
    tpsit(:,:)%hsn = 0.
  ENDWHERE
  zsui(:,:) = zwork(:,:)
  zsus(:,:) = zsus(:,:) - zsui(:,:)
!
!
! 2.3. Evaporate the ocean mixed layer (if no ice is left)
! ---------------------------------------------------------
!
! .. If the ice layer totally disappears (rare cases !), reinitialize 
! sea ice category, conserve water and salt (but accept that the 
! ovewhelming energy is lost) 
!    In this case, the salt goes to the mixed layer
!
!
  tptfl(:)%sio = tptfl(:)%sio -  &
    1.e-3*rhoice*  &
    SUM( tpsit(:,:)%fsi*tpsit(:,:)%ssi*( tpsit(:,:)%hsi ),  &
         MASK=(tpsit(:,:)%hsi<0.), DIM=1 ) / dtt
!
  zwork(:,:) = 0.
  WHERE( tpsit(:,:)%hsi<0. )
    tpsit(:,:)%hsi = 0.
    tpsit(:,:)%esi = .FALSE.
    zwork(:,:) = rhoice*tpsit(:,:)%fsi*tpsit(:,:)%hsi* &
                 (1.-1.e-3*tpsit(:,:)%ssi)
  ENDWHERE
  zsuw(:,:) = zwork(:,:)
  zsui(:,:) = zsui(:,:) - zsuw(:,:)
  tpdia(:)%sut = SUM(tpblki(:,:)%eva*tpsit(:,:)%fsi, DIM=1)
  tpdia(:)%sui = SUM(zsui(:,:), DIM=1) / dtt
  tpdia(:)%sus = SUM(zsus(:,:), DIM=1) / dtt
  tpdia(:)%suw = SUM(zsuw(:,:), DIM=1) / dtt
! Ice sublimated has to be put in wlo and the inverse in cio => water flux
! impacted but not the salt flux (???? Aurore)
  CALL glt_updtfl_r('FW2I',tpmxl,tptfl,zsui)
  CALL glt_updtfl_r('FW2O',tpmxl,tptfl,zsuw)
!print*,'hsi (4) =',tpsit(:,:)%hsi
!print*,'hsn (4) =',tpsit(:,:)%hsn
!
!
! 2.3. Conserve salt
! -------------------
!
! .. Water is accepted by the atmosphere, but salt does not evaporate...
! If a sea ice category did not sublimate completely, the salt concentration
! of sea ice is modified, else it goes to the mixed layer.
!
  WHERE ( tpsit(:,:)%hsi>epsil1 )
    tpsit(:,:)%ssi = tpsit(:,:)%ssi * zhsi(:,:)/tpsit(:,:)%hsi
  ENDWHERE
!
!
! 2.4. Conserve gltools_enthalpy
! -----------------------
!
! .. Note that sublimation is automatically linked with an increase in
! sea ice enthalpy. This sea ice gltools_enthalpy change should be given 
! back to sea ice, or to the ocean if there is no sea ice.
! For simplicity, here we deliver this gltools_enthalpy only to the ocean.
!
  CALL glt_aventh( tpsit,tpsil,zei2,zes2 )
  tptfl(:)%tio = tptfl(:)%tio - (zei2+zes2-zei1-zes1)/dtt
!
END SUBROUTINE glt_sublim_r

! ---------------------- END SUBROUTINE glt_sublim_r ------------------------
! -----------------------------------------------------------------------
