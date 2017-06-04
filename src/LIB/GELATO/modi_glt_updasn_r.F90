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
! ======================== MODULE modi_glt_updasn_r =========================
! =======================================================================
!
! Goal:
! -----
!   This module contains a subroutine that is used to update snow 
! albedo, depending on wheather conditions.
!
! IMPORTANT NOTICE: this routine should be placed just after the
! vertical heat diffusion, before a correction on snow temperature
! MAX( T_melt, T_snow) was applied.
!
! Created : 2001/08 (D. Salas y Melia)
!           Taken out from thermo_ice, which used to do this job. 
! Modified: 2009/06 (D. Salas y Melia)
!           Reduced grid
! Modified: 2010/02 (D. Salas y Melia)
!           - Rain is no longer considered here.
!           - bare thin ice albedo coefficients are adapted from
!          Flato and Brown (1996) - see also Curry et. al (2001)
!           - Douville et al. (1995) snow ageing parameterisations are
!          removed 
! Modified: 2012/01 (M. Chevallier & D. Salas y Melia)
!           A melt pond parameterization is included (updaponds_r).
!
!           IF THE MELT POND PARAMETERIZATION IS DISABLED:
!
!           *************** (3)  snow cover: asn=asnow if hsnow>val else asi
!
!           ----___------__ (2.1 + 2.3)  bare ice+melt ponds: asi=albimlt if
!                                        ice surface is melting, else asi=asi(hi)
!
!           IF THE MELT POND PARAMETERIZATION IS ENABLED:
!
!           *************** (3)  snow cover: asn=asnow if hsnow>val else asi
!
!           ________    ___ (2.2) melt ponds: asi=fmp*abi + (1-fmp)*amp
!           --------------- (2.1) bare ice:   asi=abi if ice surface is melting, 
!                                 else asi=asi(hi)
!
! --------------------- BEGIN MODULE modi_glt_updasn_r ----------------------
!
!THXS_SFX!MODULE modi_glt_updasn_r
!THXS_SFX!INTERFACE 
!THXS_SFX!!
!THXS_SFX!SUBROUTINE glt_updasn_r( gsmelt,tpatm,tpblki,pvsp,tpsit,tpdia )
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  USE modd_glt_param
!THXS_SFX!  LOGICAL, DIMENSION(nt,np), INTENT(in) ::  &
!THXS_SFX!        gsmelt
!THXS_SFX!  TYPE(t_atm), DIMENSION(np), INTENT(in) ::  &
!THXS_SFX!        tpatm   
!THXS_SFX!  TYPE(t_blk), DIMENSION(nt,np), INTENT(in) ::  &
!THXS_SFX!        tpblki
!THXS_SFX!  REAL, DIMENSION(nl,nt,np), INTENT(in) ::  &
!THXS_SFX!        pvsp
!THXS_SFX!  TYPE(t_sit), DIMENSION(nt,np), INTENT(inout) ::  &
!THXS_SFX!        tpsit   
!THXS_SFX!  TYPE(t_dia), DIMENSION(np), INTENT(inout) ::  &
!THXS_SFX!        tpdia   
!THXS_SFX!END SUBROUTINE glt_updasn_r
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_glt_updasn_r
!
! ---------------------- END MODULE modi_glt_updasn_r -----------------------
!
!
!
! -----------------------------------------------------------------------
! ------------------------- SUBROUTINE glt_updasn_r -------------------------
!
! * Subroutine used to update snow albedo (takes into account snow or
! thermodynamic surface melting). 
! * (ASN = Albedo SNow)
!
SUBROUTINE glt_updasn_r( gsmelt,tpatm,tpblki,pvsp,tpsit,tpdia )
!
  USE modd_glt_const_thm
  USE modd_types_glt
  USE modd_glt_param
!
  USE modi_gltools_updaponds_r
!
  IMPLICIT NONE
!
  LOGICAL, DIMENSION(nt,np), INTENT(in) ::  &
        gsmelt
  TYPE(t_atm), DIMENSION(np), INTENT(in) ::  &
        tpatm   
  TYPE(t_blk), DIMENSION(nt,np), INTENT(in) ::  &
        tpblki
  REAL, DIMENSION(nl,nt,np), INTENT(in) ::  &
        pvsp
  TYPE(t_sit), DIMENSION(nt,np), INTENT(inout) ::  &
        tpsit   
  TYPE(t_dia), DIMENSION(np), INTENT(inout) ::  &
        tpdia   
!
  LOGICAL, DIMENSION(nt,np) ::  &
        gsnmelt,gsimelt
  REAL ::  &
        zhsicr
  REAL, DIMENSION(np) ::  &
        zfsit
  REAL, DIMENSION(nt,np) ::  &
        zpcps,zpcpr,zasi,zasn,zent0,zalf,zt
!
!
!
! 1. Initializations
! ==================
!
! .. Compute critical thickness (where albedo parameterization for thin ice
! reaches standard sea ice albedo)
!
  zhsicr = ( ( albi-albw )/xalf1 )**( 1./xpow )
!
! .. Initialize ancillary real arrays
!
! Sea ice and snow albedo
  zasi(:,:) = 0.
  zasn(:,:) = 0.
!
! Melting point gltools_enthalpy
  zent0 = -cpsw*mu*pvsp(nilay,:,:)
!
! .. Initialize sea ice and snow melting flags (criterion: as this routine is 
! placed just after the heat diffusion scheme, without any correction
! on snow temperature, all slabs with T_snow > tice_m are melting)
!
  gsnmelt = .FALSE.
  WHERE( tpsit(:,:)%hsn>=epsil1 .AND. gsmelt(:,:) )
    gsnmelt(:,:) = .TRUE.
  ENDWHERE
!
  gsimelt(:,:) = .FALSE.
  WHERE( tpsit(:,:)%hsi > 0.1 .AND. tpsit(:,:)%hsn < epsil1 .AND. gsmelt(:,:) )
    gsimelt(:,:) = .TRUE.
  ENDWHERE
!
! .. Compute the amount of fallen precipitation.
!
  zpcps(:,:) = SPREAD( tpatm(:)%sop,1,nt )
  zpcpr(:,:) = SPREAD( tpatm(:)%lip,1,nt )
!
!
!
! 2. Compute the albedo of snow-free ice
! ======================================
!
! 2.1. Albedo of bare, non-melting sea ice (without ponds)
! ---------------------------------------------------------
!
! The ice-thickness dependence of bare sea ice albedo was eliminated in this version
! While based on physical grounds for young sea ice, this thickness dependence is
! clearly not valid for old, thinning sea ice.
! The lower albedo of young sea ice is due to the fact this ice tends to be 
! rather translucid. However, the SW radiation transmission coefficient though
! sea ice does not take into account the fact sea ice is more or less translucid.
! Actually, this parameterization probably caused the sea ice to absorb too much 
! SW radiation.
! So here we prefer simply assuming the albedo of melting sea ice is just equal to
! a standard value (albi)
!
!  IF ( niceage==1 ) THEN
!    zalf(:,:) = EXP( - MAX( tpsit(:,:)%age/xmonth2sec-6.,0. ) )
!  ELSE
!    zalf(:,:) = 1.
!  ENDIF 
!  zalf(:,:) = albyngi * zalf(:,:)
!!
!  WHERE( tpsit(:,:)%hsi<zhsicr )
!    zt(:,:) = tpsit(:,:)%hsi
!    zasi(:,:) = zalf(:,:) * ( xalf1*AMAX1( zt(:,:),0. )**xpow + albw ) +  &
!      ( 1.-zalf(:,:) ) * albi
!!
!! .. No thermodynamic surface melting on thick ice without snow.
!! The albedo is set to bare ice albedo.
!!
!  ELSEWHERE
!    zasi(:,:) = albi
!!
!  ENDWHERE
!
  zasi(:,:) = albi
!
!
! 2.2. Albedo of bare, melting sea ice (pond parameterization is enabled)
! ------------------------------------------------------------------------
!
! .. Thermodynamic surface melting on ice without snow.
! The albedo is set to melting ice albedo.
!
  IF ( nmponds==1) THEN
! .. Melt pond case: melting bare ice albedo is considered as a physical
! constant (=0.65).
!
    WHERE( gsimelt(:,:) )
      zasi(:,:) = AMIN1( zasi(:,:),xalbareimlt )
    ENDWHERE
! .. Invoke the pond parameterization. The melting ice surface consists in a fraction (fmp) 
! covered with meltwater ponds, and in melting bare ice (1-fmp). This parameterization 
! updates the global melting ice albedo.
!
    CALL gltools_updaponds_r(gsmelt,tpatm,tpblki,tpdia,tpsit,zasi)
  ELSE
!
!
! 2.3. Albedo of bare, melting sea ice (pond parameterization is disabled)
! -------------------------------------------------------------------------
!
! .. In this case, melting ice surface is a mix of melting bare ice and melt ponds.
! Melting ice albedo is prescribed as a "tuning" parameter.
    WHERE( gsimelt(:,:) )
      zasi(:,:) = AMIN1( zasi(:,:),albimlt )
    ENDWHERE
!
  ENDIF
!
!
!
! 3. Compute the albedo of the snow covered part of the ice
! =========================================================
!
! 3.1. Determine the initial snow albedo
! --------------------------------------
!
! .. Now compute albedo of the snow covered part of the ice slab from:
!       - initial averaged snow+bare ice albedo, tpsit%asn
!       - computed bare ice albedo, zasi
!       - snow thickness, tpsit%hsn
!
  zt(:,:) = tpsit(:,:)%rsn*tpsit(:,:)%hsn / rhofw
  zalf(:,:) = AMIN1( zt(:,:)/wnew,1. )
!
! .. Snow cover albedo: melting and dry snow cases
!
  WHERE( gsnmelt(:,:) )
    zasn(:,:) = albsmlt
  ELSEWHERE
    zasn(:,:) = albsdry 
  ENDWHERE
!
! 
! 3.2. Case of new snow falls
! ----------------------------
!
! .. Snow accumulation : albedo is refreshed to its maximum value if 
! snow amount reaches a threshold wnew. If new snow thickness is less 
! than wnew, a linear combination of initial snow surface albedo and 
! maximum snow albedo gives new albedo.   
!
  WHERE ( zpcps(:,:)>zpcpr(:,:) )
      zasn(:,:) = albsdry
  ENDWHERE
!
!
!
! 4. Weighted surface albedo (snow covered + bare ice parts of the slab)
! =======================================================================
!
! .. Now that albedos were computed both on a sea ice slab with and 
! without snow, the global surface albedo will be recomposed to take
! into account the fact that a thin snow cover does not actually cover
! the entire slab. 
!
  tpsit(:,:)%asn =  &
    zalf(:,:)*zasn(:,:) + (1.-zalf(:,:))*zasi(:,:)
!
! .. For AR5 diagnostics: weighted bare sea ice albedo
! Weights for final computation of the average bare ice albedo 
! [i.e. SUM(ftot)] must be incremented here, not where diagnostics 
! are writtenSUM(ftot))
!   The reason for this is that tpsit%fsi in the present routine would
! not be consistent with total sea ice fraction in the diagnostics.
!
!* Accumulate sea ice fraction
  zfsit(:) = SUM( tpsit(:,:)%fsi, DIM=1 )
  tpdia(:)%aiw = tpdia(:)%aiw + zfsit(:)
!* Ice surface albedo (bare ice + melt ponds)
  tpdia(:)%asi = SUM( tpsit(:,:)%fsi*zasi(:,:),DIM=1 )
!* Snow albedo: approximation (suppose zalf is generally close to 1)
  tpdia(:)%asn = SUM( tpsit(:,:)%fsi*zasn(:,:),DIM=1 )
!
END SUBROUTINE glt_updasn_r
!
! ---------------------- END SUBROUTINE glt_updasn_r ------------------------
! -----------------------------------------------------------------------
