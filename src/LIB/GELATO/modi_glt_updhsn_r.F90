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
! ======================== MODULE modi_glt_updhsn_r =========================
! =======================================================================
!
! Goal:
! -----
!   Update the thickness of a sea ice floe snow cover, knowing the 
! surface net ablation flux.
!
! Created : 2001/07 (D. Salas)
!           Taken out from thermo_ice routine.  
! Modified: 2009/06 (D. Salas y Melia)
!           Reduced grid
! Modified: (A. Voldoire)
!           new ice/water fluxes interface CALL
!
! ---------------------- BEGIN MODULE modi_glt_updhsn_r ---------------------
!
!THXS_SFX!MODULE modi_glt_updhsn_r
!THXS_SFX!INTERFACE 
!THXS_SFX!!
!THXS_SFX!SUBROUTINE glt_updhsn_r( gsmelt,pdhmelt,tpmxl,tptfl,tpsit,tpsil,tpdia )
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  USE modd_glt_param
!THXS_SFX!  LOGICAL, DIMENSION(nt,np), INTENT(in) ::  &
!THXS_SFX!        gsmelt
!THXS_SFX!  REAL, DIMENSION(nl,nt,np), INTENT(in) ::  &
!THXS_SFX!        pdhmelt
!THXS_SFX!  TYPE(t_mxl), DIMENSION(np), INTENT(in) ::  &
!THXS_SFX!        tpmxl
!THXS_SFX!  TYPE(t_tfl), DIMENSION(np), INTENT(inout) ::  &
!THXS_SFX!        tptfl
!THXS_SFX!  TYPE(t_sit), DIMENSION(nt,np), INTENT(inout) ::  &
!THXS_SFX!        tpsit
!THXS_SFX!  TYPE(t_vtp), DIMENSION(nl,nt,np), INTENT(inout) ::  &
!THXS_SFX!        tpsil
!THXS_SFX!  TYPE(t_dia), DIMENSION(np), INTENT(inout) ::  &
!THXS_SFX!        tpdia
!THXS_SFX!END SUBROUTINE glt_updhsn_r
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_glt_updhsn_r
!
! ---------------------- END MODULE modi_glt_updhsn_r -----------------------
!
!
!
! -----------------------------------------------------------------------
! ------------------------- SUBROUTINE glt_updhsn_r -------------------------
!
SUBROUTINE glt_updhsn_r( gsmelt,pdhmelt,tpmxl,tptfl,tpsit,tpsil,tpdia )
!
  USE modd_glt_const_thm
  USE modd_types_glt
  USE modd_glt_param
  USE modi_glt_updtfl_r
  USE mode_gltools_enthalpy
!
  IMPLICIT NONE
!
!* Arguments
!
  LOGICAL, DIMENSION(nt,np), INTENT(in) ::  &
        gsmelt
  REAL, DIMENSION(nl,nt,np), INTENT(in) ::  &
        pdhmelt
  TYPE(t_mxl), DIMENSION(np), INTENT(in) ::  &
        tpmxl
  TYPE(t_tfl), DIMENSION(np), INTENT(inout) ::  &
        tptfl
  TYPE(t_sit), DIMENSION(nt,np), INTENT(inout) ::  &
        tpsit
  TYPE(t_vtp), DIMENSION(nl,nt,np), INTENT(inout) ::  &
        tpsil
  TYPE(t_dia), DIMENSION(np), INTENT(inout) ::  &
        tpdia
!
!* Local variables
!
  REAL, DIMENSION(nt,np) ::  &
        zqmelt,zhsn,zdmsn,zent
!real,dimension(np) :: zei1,zei2,zes1,zes2
!
!
!
! 1. Initializations
! ==================
!
! .. Surface melting boolean 
!
  zqmelt = 0.
!
! NOTE: here we assume there is only one snow layer
!
! .. Take the overwhelming energy due to vertical diffusion + solar radiation
! intake
!
  WHERE ( pdhmelt(nl,:,:)>0. )
    zqmelt(:,:) = pdhmelt(nl,:,:) / dtt 
  ENDWHERE
!
! .. Snow thickness
!
  zhsn(:,:) = tpsit(:,:)%hsn
!
!CALL glt_aventh(tpsit,tpsil,zei1,zes1)
!print*,'Enthalpie avant =',zei1+zes1
!    
!
! 2. Snow vertical melting
! ========================
!
! .. Compute new snow thickness
!
  WHERE ( gsmelt(:,:) )
    zhsn(:,:) = tpsit(:,:)%hsn -  &
      dtt*zqmelt(:,:) / ( tpsit(:,:)%rsn*xmhofusn0 )
!
! .. If updated snow thickness is positive, it means that all zqmelt
! has contributed to snow melting. Then it should be zeroed.
!
    WHERE ( zhsn(:,:)>=0. )
      zqmelt(:,:) = 0.
!
! .. If updated snow thickness is negative, it means that only part of
! zqmelt could be used for snow melting. In excess energy at the
! top of the snow layer is computed :
!
    ELSEWHERE
      zqmelt(:,:) = -xmhofusn0*tpsit(:,:)%rsn*zhsn(:,:) / dtt
      zhsn(:,:) = 0.
    ENDWHERE
!
  ENDWHERE
!
! .. Compute snow mass variation
!
  zdmsn(:,:) = tpsit(:,:)%rsn * tpsit(:,:)%fsi *  &
    ( zhsn(:,:)-tpsit(:,:)%hsn )
!
! .. AR5 diagnostic
!
  tpdia(:)%snm = SUM( zdmsn(:,:), DIM=1 ) / dtt
!
! .. Update snow thickness and density
!
  tpsit(:,:)%hsn = zhsn(:,:)
!
! .. Note that if zqmelt is not equal to zero at this stage, it will be
! transmitted to the ocean.
!
!
!
! 3. Energy conservation
! ======================
!
! 3.1. Transmit remaining energy to the ocean
! --------------------------------------------
!
  tptfl(:)%tio = tptfl(:)%tio +  &
    SUM( zqmelt(:,:)*tpsit(:,:)%fsi, DIM=1 )
!
! 
! 3.2. Massic gltools_enthalpy of removed snow
! -------------------------------------
!
! .. Since the removed snow has a temperature of 0C (melting) its 
! gltools_enthalpy is also zero.
! Not useful anymore, done directly in glt_updtfl
!  zent(:,:) = 0.
!
!
! 3.3. Update water, heat and salt fluxes affecting the ocean
! -----------------------------------------------------------
!
! .. Water, heat and salt flux due to the inclusion of the water mass
! generated by the melting of snow
!
  CALL glt_updtfl_r( 'FW2O',tpmxl,tptfl,zdmsn)
!CALL glt_aventh(tpsit,tpsil,zei2,zes2)
!print*,'Enthalpie apres =',zei2+zes2
!print*,'Delta(Enthalpie) =',(zei2+zes2-zei1-zes1)/dtt
!print*,'qtio=',SUM( zqmelt(:,:)*tpsit(:,:)%fsi, DIM=1 )
!
END SUBROUTINE glt_updhsn_r
!
! ----------------------- END SUBROUTINE glt_updhsn_r -----------------------
! -----------------------------------------------------------------------
