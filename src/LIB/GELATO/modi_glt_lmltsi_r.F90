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
! ======================== MODULE modi_glt_lmltsi_r =========================
! =======================================================================
!
! Goal:
! ----- 
!    Here the lateral sea ice ablation rate is assessed by means of a 
! Hakkinen & Mellor (1992) parameterization.
! 
! Method: 
! -------
!    Let a sea ice floe of thickness hsi[k] and concentration 
! fsi[k] (total concentration is fsit, undergoing bottom ablation
! ( dhsi[k]<0 during time step dt ). The lateral ablation is supposed to 
! be :
!      dfsi[k] = 0.7 * fsi[k]/fsit * ( 1.-fsit ) * dhsi[k]/hsi[k] 
!
!    The energy used for this process is taken from the ocean and 
! computed:
!      dE[k] = cpice0 * dfsi[k] * hsi[k] 
!  
! Created : 2001/07 (D. Salas y Melia) 
!           Taken out from thermo_ice routine.
! Modified: 2009/06 (D. Salas y Melia)
!           Reduced grid 
! Modified: 2011/12 (A. Voldoire)
!           new ice/water fluxes interface CALL
!
! ---------------------- BEGIN MODULE modi_glt_lmltsi_r ---------------------
!
!THXS_SFX!MODULE modi_glt_lmltsi_r
!THXS_SFX!INTERFACE 
!THXS_SFX!!
!THXS_SFX!SUBROUTINE glt_lmltsi_r( tpmxl,tpsil,tpsit,tpdia,tptfl ) 
!THXS_SFX!!
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  USE modd_glt_param
!THXS_SFX!  TYPE(t_mxl), DIMENSION(np), INTENT(in) ::  &
!THXS_SFX!        tpmxl
!THXS_SFX!  TYPE(t_vtp), DIMENSION(nl,nt,np), INTENT(in) ::  &
!THXS_SFX!        tpsil
!THXS_SFX!  TYPE(t_sit), DIMENSION(nt,np), INTENT(inout) ::  &
!THXS_SFX!        tpsit
!THXS_SFX!  TYPE(t_dia), DIMENSION(np), INTENT(inout) ::  &
!THXS_SFX!        tpdia
!THXS_SFX!  TYPE(t_tfl), DIMENSION(np), INTENT(inout) ::  &
!THXS_SFX!        tptfl
!THXS_SFX!END SUBROUTINE glt_lmltsi_r
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_glt_lmltsi_r
!
! ---------------------- END MODULE modi_glt_lmltsi_r -----------------------
!
!
!
! -----------------------------------------------------------------------
! ------------------------- SUBROUTINE glt_lmltsi_r -------------------------
!
SUBROUTINE glt_lmltsi_r  &
        ( tpmxl,tpsil,tpsit,tpdia,tptfl ) 
  USE modd_glt_const_thm
  USE modd_types_glt
  USE modd_glt_param
  USE modi_glt_updtfl_r
!
  IMPLICIT NONE
!
!* Arguments
!
  TYPE(t_mxl), DIMENSION(np), INTENT(in) ::  &
        tpmxl
  TYPE(t_vtp), DIMENSION(nl,nt,np), INTENT(in) ::  &
        tpsil
  TYPE(t_sit), DIMENSION(nt,np), INTENT(inout) ::  &
        tpsit
  TYPE(t_dia), DIMENSION(np), INTENT(inout) ::  &
        tpdia
  TYPE(t_tfl), DIMENSION(np), INTENT(inout) ::  &
        tptfl
!
!* Local variables
!
  INTEGER ::  &
        jl
  REAL, DIMENSION(np) ::  &
        zdtml
  REAL, DIMENSION(nt,np) ::  &
        zfsia,zdmsi,zdmsn,zmrate3,zent
!
!
!
! 1. Initializations
! ===================
!
! .. Sea ice concentration field "after" lateral ablation
!
  zfsia(:,:) = tpsit(:,:)%fsi
!
! .. Total sea ice concentration field (3D expanded) --> for Hakkinen
!
!  zfsit(:) = SUM( tpsit(:,:)%fsi,DIM=1 ) 
!  zfsit3(:,:) = SPREAD( zfsit(:),1,nt )
!
! .. Total ocean heat flux (3D expanded) --> for Hakkinen
!
!  zqoct3(:,:) = SPREAD( tpmxl(:)%qoc+tpmxl(:)%qml,1,nt )
!
!
!
! 2. Compute sea ice concentration field after lateral melting
! ============================================================
!
! .. Hakkinen method : quite OK in the Arctic, too rough in the 
! Antarctic 
!
!  WHERE ( tpsit(:,:)%hsi>=xhsimin .AND. zfsit3(:,:)>=epsil1 )
!    zdhsi(:,:) = -dtt*hofusni0*zqoct3(:,:)
!    zfsia(:,:) = zfsia(:,:) +  & 
!      0.7*tpsit(:,:)%fsi/zfsit3(:,:)*( 1.-zfsit3(:,:) )*  & 
!      AMIN1( zdhsi(:,:),0. ) / tpsit(:,:)%hsi
!    zfsia(:,:) = AMAX1( zfsia(:,:),0. )
!  ENDWHERE
!
! .. Steele et al. (1992), Maykut & Perovich (1987), summed up by 
! Schmidt et al. (2003) : certainly closer to the real world, but
! requires glt_info on the geometry and size of floes ( xlmelt parameter ).
!
  zdtml(:) = tpmxl(:)%tml - tpmxl(:)%mlf
  zdtml(:) = AMAX1( zdtml(:), 0. )
  zmrate3(:,:) = SPREAD( xm1 * zdtml(:)**xm2,1,nt )
!
! .. We suppose ice floes are squares:
!      xlmelt = P / A (P: perimeter of a floe, A its surface)
!  Let a be the characteristic size of a floe ; P=4*a, A=a*a
!  hence:
!      xlmelt = 4/a
!  The total ice fraction change should be:
!      dfsitot = -fsitot*[ 4/a*MR*dt ] = -fsitot*[ xlmelt*MR*dt ]
!  where MR is the lateral melting rate in m.s-1
! But this is also true for every ice category (the intensity of the
! lateral melting does not depend on the number of ice categories).
!
  zfsia(:,:) = zfsia(:,:) *  &
    ( 1. - xlmelt*dtt*zmrate3(:,:) )
  zfsia(:,:) = AMAX1( zfsia(:,:),0. )
!
! .. Compute diagnostic
!
  tpdia(:)%mrl =  &
    SUM( ( zfsia(:,:)-tpsit(:,:)%fsi )*  &
    tpsit(:,:)%hsi, DIM=1 ) * rhoice / dtt
!
!
!
! 3. Energy conservation
! ======================
!
! It is assumed that the amount of energy needed to melt sea ice 
! laterally comes from the ocean.
!
! 
! 3.1. Compute involved masses of ice and snow 
! --------------------------------------------
!
! .. Variation of sea ice mass due to lateral melting
!
  zdmsi(:,:) = rhoice *  &
    ( zfsia(:,:)-tpsit(:,:)%fsi ) * tpsit(:,:)%hsi
!
! .. Variation of snow mass due to lateral melting
!
  zdmsn(:,:) = tpsit(:,:)%rsn *  &
    ( zfsia(:,:)-tpsit(:,:)%fsi ) * tpsit(:,:)%hsn
!
!
! 3.2. Massic gltools_enthalpy and salinity of removed ice
! -------------------------------------------------
!
! .. Massic gltools_enthalpy
!
  zent(:,:) = 0.
  DO jl=1,nilay
    zent(:,:) = zent(:,:) +  &
      sf3t(nilay+1-jl)*tpsil(jl,:,:)%ent
  END DO
!
!
! 3.3. Update water, heat and salt fluxes affecting the ocean 
! ------------------------------------------------------------
!
! .. This is the contribution of sea ice melting
!
  CALL glt_updtfl_r( 'I2O',tpmxl,tptfl,zdmsi,pent=zent,psalt=tpsit%ssi )
!
!
! 3.4. Massic gltools_enthalpy of removed snow
! -------------------------------------
!
  zent(:,:) = SUM( tpsil(nilay+1:nl,:,:)%ent, DIM=1 )/FLOAT(nslay)
!
!
! 3.5. Update water, heat and salt fluxes affecting the ocean
! ------------------------------------------------------------
!
  CALL glt_updtfl_r( 'FW2O',tpmxl,tptfl,zdmsn,pent=zent )
  tpdia(:)%snml = SUM( zdmsn(:,:), DIM=1 ) / dtt
!    
!
!
! 4. Update ice state
! =====================================
!
! .. Concentration field
!
  tpsit(:,:)%fsi = zfsia(:,:)
!
! .. If necessary, re-initialize sea ice with nil concentration
!
  WHERE ( tpsit(:,:)%fsi<epsil1 .AND. tpsit(:,:)%hsi>epsil1 ) 
    tpsit(:,:)%esi = .FALSE.
    tpsit(:,:)%fsi = 0.
    tpsit(:,:)%hsi = 0.
    tpsit(:,:)%asn = albw
    tpsit(:,:)%hsn = 0.
    tpsit(:,:)%rsn = rhosnwmin
  ENDWHERE
!
  IF ( niceage==1 ) THEN
    WHERE ( tpsit(:,:)%fsi<epsil1 .AND. tpsit(:,:)%hsi>=epsil1 ) 
      tpsit(:,:)%age = 0.
    ENDWHERE
  ENDIF
!
  IF ( nicesal==1 ) THEN
    WHERE ( tpsit(:,:)%fsi<epsil1 .AND. tpsit(:,:)%hsi>=epsil1 ) 
      tpsit(:,:)%ssi = 0.
    ENDWHERE
  ENDIF  
!
  IF ( nmponds==1 ) THEN
    WHERE ( tpsit(:,:)%fsi<epsil1 .AND. tpsit(:,:)%hsi>=epsil1 ) 
      tpsit(:,:)%vmp = 0.
    ENDWHERE
  ENDIF
!
END SUBROUTINE glt_lmltsi_r

! ----------------------- END SUBROUTINE glt_lmltsi_r -----------------------
! -----------------------------------------------------------------------
