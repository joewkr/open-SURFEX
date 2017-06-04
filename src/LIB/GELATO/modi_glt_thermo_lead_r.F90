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
! ======================= MODULE modi_glt_thermo_lead_r =====================
! =======================================================================
!
!
! * Do the thermodynamics for the lead covered fraction of the grid
! cell. 
!
! Modified: 2007/11 (D. Salas y Melia)
!            thick(jh) < Hsi < thick(jh+1) is not correct: misses cases
!            like Hsi = thick(jh)
!            corrected to: thick(jh) < Hsi <= thick(jh+1)
! Modified: 2009/06 (D. Salas y Melia) reduced grid
! Modified: 2011/12 (A. Voldoire) modified diagnostics and ice/water 
!           fluxes interface CALL
! Modified: 2012/07 (D. Salas y Melia) suppress tpopw variable
!
! ------------------- BEGIN MODULE modi_glt_thermo_lead_r -------------------
!
!THXS_SFX!MODULE modi_glt_thermo_lead_r
!THXS_SFX!INTERFACE
!THXS_SFX!!
!THXS_SFX!SUBROUTINE glt_thermo_lead_r   &
!THXS_SFX!  (tpdom,pustar,tpmxl,tpatm,tpblkw,  &
!THXS_SFX!  tpdia,tptfl,tpsit,tpsil,  &
!THXS_SFX!  tpldsit,tpldsil)
!THXS_SFX!!
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  USE modd_glt_param
!THXS_SFX!!
!THXS_SFX!  TYPE(t_dom), DIMENSION(np), INTENT(in) ::  &
!THXS_SFX!        tpdom
!THXS_SFX!  REAL, DIMENSION(np), INTENT(in) ::  &
!THXS_SFX!        pustar
!THXS_SFX!  TYPE(t_mxl), DIMENSION(np), INTENT(inout) ::  &
!THXS_SFX!        tpmxl
!THXS_SFX!  TYPE(t_atm), DIMENSION(np), INTENT(in) ::  &
!THXS_SFX!        tpatm
!THXS_SFX!!
!THXS_SFX!  TYPE(t_blk), DIMENSION(np), INTENT(inout) ::  &
!THXS_SFX!        tpblkw
!THXS_SFX!  TYPE(t_dia), DIMENSION(np), INTENT(inout) ::  &
!THXS_SFX!        tpdia
!THXS_SFX!  TYPE(t_tfl), DIMENSION(np), INTENT(inout) ::  &
!THXS_SFX!        tptfl
!THXS_SFX!  TYPE(t_sit), DIMENSION(nt,np), INTENT(inout) ::  &
!THXS_SFX!        tpsit
!THXS_SFX!  TYPE(t_vtp), DIMENSION(nl,nt,np), INTENT(inout) ::  &
!THXS_SFX!        tpsil
!THXS_SFX!!
!THXS_SFX!  TYPE(t_sit), DIMENSION(nt,np), INTENT(out) ::  &
!THXS_SFX!        tpldsit
!THXS_SFX!  TYPE(t_vtp), DIMENSION(nl,nt,np), INTENT(out) ::  &
!THXS_SFX!        tpldsil
!THXS_SFX!END SUBROUTINE glt_thermo_lead_r
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_glt_thermo_lead_r
!
! -------------------- END MODULE modi_glt_thermo_lead_r --------------------
!
!
! -----------------------------------------------------------------------
! --------------------- SUBROUTINE glt_thermo_lead_r ------------------------
!
SUBROUTINE glt_thermo_lead_r  &
  (tpdom,pustar,tpmxl,tpatm,tpblkw,  &
  tpdia,tptfl,tpsit,tpsil,  &
  tpldsit,tpldsil)
!
!
!
! 1. Declarations
! ===============
!
! 1.1. Module declarations
! ------------------------
!
  USE modd_types_glt
  USE modd_glt_param
  USE modd_glt_const_thm
  USE mode_glt_info_r
  USE modi_gltools_chkglo_r
  USE modi_glt_saltrap_r
  USE modi_glt_updtfl_r
  USE modi_glt_oceflx_r
  USE mode_glt_stats_r
!
  IMPLICIT NONE
!
!
! 1.2. Dummy arguments declarations
! ---------------------------------
!
! .. INTENT(in) arguments.
!
  TYPE(t_dom), DIMENSION(np), INTENT(in) ::  &
        tpdom
  REAL, DIMENSION(np), INTENT(in) ::  &
        pustar
  TYPE(t_mxl), DIMENSION(np), INTENT(inout) ::  &
        tpmxl
  TYPE(t_atm), DIMENSION(np), INTENT(in) ::   &
        tpatm
!
! .. INTENT(inout) arguments.
!
  TYPE(t_blk), DIMENSION(np), INTENT(inout) ::  &
        tpblkw
  TYPE(t_dia), DIMENSION(np), INTENT(inout) ::  &
        tpdia
  TYPE(t_tfl), DIMENSION(np), INTENT(inout) ::  &
        tptfl
  TYPE(t_sit), DIMENSION(nt,np), INTENT(inout) ::  &
        tpsit
  TYPE(t_vtp), DIMENSION(nl,nt,np), INTENT(inout) ::  &
        tpsil
!
! .. INTENT(out) arguments.
!
  TYPE(t_sit), DIMENSION(nt,np), INTENT(out) ::  &
        tpldsit
  TYPE(t_vtp), DIMENSION(nl,nt,np), INTENT(out) ::  &
        tpldsil
!
!
! 1.3. Local variables declarations
! ---------------------------------
!
  INTEGER ::  &
        jk,jp,jt
  LOGICAL, DIMENSION(np) ::  &
        yfreeze
  REAL ::  &
        zibflx0,zlat0,zsst0,zfac0
  REAL, DIMENSION(np) ::  &
        zfld,zfldsi,zhldsi,ztldsi,zdhldsi,za,zdtml,zhef,zsnflx,  &
        zdmsi,zfsit,zent0,zsalt0,ztem0
  REAL, DIMENSION(nt,np) ::  &
        zdmsi2,zent,zsalt
!
!
! 1.4. Array initializations
! -------------------------
!
  zfldsi(:) = 0.
  zhldsi(:) = 0.
  ztldsi(:) = 0.
  za(:) = 0.
  zdtml(:) = 0.
  zhef(:) = 0.
  zsnflx(:) = 0.
  zdmsi(:) = 0.
  zfsit(:) = 0.
  zdmsi2(:,:) = 0.
!  print*,'(1) gltools_enthalpy =',tptfl%tlo
!
!
! 1.5. Compute the ocean-ice heat flux (if requested)
! ----------------------------------------------------
!
! .. We update the ocean-ice heat flux (if not provided by the ocean model)
!
  IF ( nextqoc==0 ) THEN
      CALL glt_oceflx_r( tpdom,pustar,tpmxl )
  ENDIF
!
!
! 1.6. Define lead freezing boolean
! ---------------------------------
!
  yfreeze(:) = .FALSE.
  WHERE ( tpdom(:)%tmk==1 .AND. tpmxl(:)%tml<=tpmxl(:)%mlf+0.1 )
      yfreeze(:) = .TRUE.
  ENDWHERE
!
! 
! 1.7. Welcome message
! --------------------
!
  IF (lp1) THEN
    WRITE(noutlu,*) ' '
    WRITE(noutlu,*) '**** LEVEL 4 - SUBROUTINE THERMO_LEAD_R'
    WRITE(noutlu,*) ' '
  ENDIF
!
!
! 1.8. Check in
! -------------
!
  CALL gltools_chkglo_r( 'BEFORE THERMO_LEAD_R',tpdom,tpsit )
!
!
!
! 2. Leads thermodynamics
! =======================
!
! 2.1. Compute new lead features
! ------------------------------
!
! 2.1.1. Lead fraction variation due to lateral sea ice melting
! .............................................................
!
! .. In thermo_ice, the concentration of sea ice changed, due to lateral
! melting. So here, the concentration of leads should be changed
! accordingly. Note these changes in concentration will cause a lack of
! energy concentration, as the global flux sent by atmosphere to the 
! sea ice/open water ensemble won't be conserved. 
!
! .. Compute total sea ice concentration
! 
  zfsit(:) = SUM(tpsit(:,:)%fsi,DIM=1)
!
! .. Concentration of leads. Note that here, to ensure accuracy of energy
! conservation, it is not directly taken from the restart lead variable.
!
  zfld(:) = 1.-zfsit(:)
!  
!
! 2.1.2. Compute P-E sent to the ocean through the leads
! ......................................................
!
! .. Note : evaporation and precipitations on leads (water in m.s-1).
!
  tptfl(:)%wlo = tptfl(:)%wlo +  &
    zfld(:)*( tpatm(:)%lip + tpatm(:)%sop + tpblkw(:)%eva )
  tpdia(:)%l_pr = zfld(:) * tpatm(:)%lip
  tpdia(:)%l_prsn = zfld(:) * tpatm(:)%sop
  tpdia(:)%sul  = zfld(:) * tpblkw(:)%eva
!
!
!
! 2.1.3. Parameterization of slush sea ice
! .........................................
!
! .. Compute energy transfer due to snow going to leads (per m2)
!
  IF ( nsnwrad==1 ) THEN
      zsnflx(:) = -xmhofusn0*tpatm(:)%sop
    ELSE
      zsnflx(:) = 0.
  ENDIF
!
!
! 2.1.5. Compute solar flux absorbed by the ocean through the leads
! .................................................................
!
! .. Solar flux (totally transmitted to the ocean in all cases)
!
  tptfl(:)%llo = tptfl(:)%llo + zfld(:)*tpblkw(:)%swa
!
!
! 2.1.6. Compute total non-solar heat flux on leads
! .................................................
!
! .. This is actually the sum of the non-solar flux sent by the atmosphere
! model and of the flux qml sent by the ocean.
!
  zhef(:) = tpmxl(:)%qml + tpblkw(:)%nsf + zsnflx(:)
!
! .. Taking into account the ocean-ice heat flux
!  - If the ocean-ice flux is larger than the non-solar flux, no sea 
! ice will be formed; we assume that the oce-ice heat flux exactly compensates
! the non-solar heat flux
!
! A PRECISER
  WHERE( yfreeze(:) )
    WHERE( zhef(:)>0. )
      yfreeze = .FALSE.
    ELSEWHERE
      WHERE ( zhef(:) + tpmxl(:)%qoc > 0. )
        yfreeze = .FALSE.
      ELSEWHERE
        zhef(:) = zhef(:) + tpmxl(:)%qoc  
      ENDWHERE
    ENDWHERE
  ENDWHERE
!
!print*,glt_avg_r(tpdom,tptfl%tlo,0)
  WHERE( .NOT. yfreeze(:) )
    tptfl(:)%tlo = tptfl(:)%tlo + zfld(:) * zhef(:)
  ELSEWHERE
    tptfl(:)%tlo = tptfl(:)%tlo - zfld(:) * tpmxl(:)%qoc
    tpdia(:)%qoi = tpdia(:)%qoi + zfld(:) * tpmxl(:)%qoc
  ENDWHERE
!print*,glt_avg_r(tpdom,tptfl%tlo,0)
!   
! Ajouter les diags %qoi et modifier %tlo eventuellement 

!  WHERE( zhef(:)<0. .AND. tpmxl(:)%qoc > -zhef(:) )
!      yfreeze(:) = .FALSE.
!! tptfl%tlo is updated in 2.2.1.
!      tpdia(:)%qoi = tpdia(:)%qoi - zfld(:) * zhef(:)
!  ELSEWHERE 
!      zhef(:) = zhef(:) + tpmxl(:)%qoc  
!  ENDWHERE
!  print*,'(2) gltools_enthalpy =',tptfl%tlo
!
!
! 2.2. Compute the characteristics of new sea ice formed in leads
! ---------------------------------------------------------------
!
! 2.2.1. No ice freezing in the lead
! ....................................
!
! .. In this case, no sea ice will be created in the lead; the solar and
! non solar heat flux can be directly sent to the ocean surface.
!
  WHERE ( .NOT.yfreeze(:) )
      ztldsi(:) = tpmxl(:)%mlf
      zhldsi(:) = 0.
  ENDWHERE
!
! REPRENDRE ICI
!
! 2.2.2. Ice freezing in the lead
! .................................
!
! .. If the non-solar heat flux applied to leads is positive, no sea ice
! is formed. The heat flux goes to the ocean.
!
!print*,yfreeze
!print*,zhef
!  WHERE ( yfreeze(:) .AND. zhef(:)>=0. )
!      tptfl(:)%tlo = zfld(:) * zhef(:)
!      ztldsi(:) = tpmxl(:)%mlf
!      zhldsi(:) = 0.
!  ENDWHERE
!  print*,'(4) gltools_enthalpy =',tptfl%tlo
!
!
! 2.2.3. Third case: lead at freezing point and zhef<0
! ....................................................
!
!   Q = tpmxl%qml + tpblkw%nsf is used to create new sea ice in the
! lead. This negative flux will be used for three different things:
!       - build up ice of thickness hi : term Lf*hi
!       - if ice surface temperature is ts, cool down this new ice
!       and use : rhoi*cpi*hi*(Tf-ts)/2
!       - maintain flux balance at the surface : ki*dt*(Tf-ts)/hi
!   The sum of these three terms should be equal to : -Q*dt.
!
! .. No non-solar heat flux is sent to the ocean
!
!  WHERE ( yfreeze(:) .AND. zhef(:)<0. )
!      tptfl(:)%tlo = 0.
!  ENDWHERE
!
! .. We assume that lead sea ice is formed at -5 deg C
!
  ztem0(:) = -5.
!
! .. Get salinity, gltools_enthalpy and thickness of the new ice
!
  CALL glt_saltrap_r( yfreeze,zhef,ztem0,tpmxl,zsalt0,zent0,zhldsi ) 
!
! .. Compute surface temperature and thickness of new ice formed in leads
!
   WHERE ( yfreeze(:) )
     ztldsi(:) = tpmxl(:)%mlf
   ENDWHERE
!
! .. Corrections: depending on new ice computed thickness, compute new
! ice fraction and if necessary update new ice thickness
!
  WHERE ( yfreeze(:) )
    WHERE ( zhldsi(:)<=xhsimin )
      zfldsi(:) = zfld(:)*zhldsi(:)/xhsimin
      zhldsi(:) = xhsimin
    ELSEWHERE
      zfldsi(:) = zfld(:)
    ENDWHERE
  ENDWHERE
!
! 
! 2.3. Define lead sea ice state variable
! ---------------------------------------
!
! .. Massic salt content of new sea ice (2d array)
!
  zsalt(:,:) = SPREAD( zsalt0(:),1,nt )
!
! Compute all ice state variables for sea ice formed in the leads
!
! ..Sea ice 3D variables. 
!
  tpldsit(:,:)%esi = .FALSE. 
  tpldsit(:,:)%asn = albw
  tpldsit(:,:)%fsi = 0. 
  tpldsit(:,:)%hsi = 0. 
  tpldsit(:,:)%hsn = 0.
  tpldsit(:,:)%rsn = rhosnwmin
  tpldsit(:,:)%tsf = SPREAD( tpmxl(:)%mlf,1,nt )
  tpldsit(:,:)%ssi = zsalt(:,:)
  tpldsit(:,:)%age = 0.
  tpldsit(:,:)%vmp = 0.
!
  DO jk = 1,nt
    WHERE ( thick(jk)<zhldsi(:) .AND. zhldsi(:)<=thick(jk+1)  &
    .AND. zhldsi(:)>0. )
        tpldsit(jk,:)%esi = .TRUE.
!        tpldsit(jk,:)%asn =  &
!          albyngi*AMIN1( albi,xalf1*zhldsi(:)**xpow+albw ) +  &
!          ( 1.-albyngi )*albi
        tpldsit(jk,:)%asn = albi
        tpldsit(jk,:)%fsi = zfldsi(:)
        tpldsit(jk,:)%hsi = zhldsi(:)
        tpldsit(jk,:)%tsf = ztldsi(:)
    ENDWHERE
  END DO
!
! .. Sea ice 4D variables.
!
  tpldsil(:,:,:)%ent = SPREAD( SPREAD( zent0(:),1,nt ),1,nl )
!
!
! 2.4. Prepare lead sea ice diagnostics
! --------------------------------------
!
  tpdia(:)%lsi = zfldsi(:)*zhldsi(:)*rhoice/dtt
!
!
! 2.5. Print out information about new sea ice
! ---------------------------------------------
!
  CALL glt_info_si_r( 'About lead sea ice', tpsit=tpldsit )
!
!
! 
! 3. Energy conservation
! =======================
!
! .. Compute consequences of new sea ice formation on the mixed layer. 
!
! 3.1. Compute mass of newly formed ice
! --------------------------------------
!
  WHERE ( yfreeze(:) )
    zdmsi(:) = rhoice * zfldsi(:) * zhldsi(:)
  ENDWHERE
  zdmsi2(1,:) = zdmsi(:)
!
!
! 3.3. Update water, heat and salt fluxes affecting the ocean
! ------------------------------------------------------------
!
  zent(:,:) = 0.
  CALL glt_updtfl_r( 'I2O',tpmxl,tptfl,zdmsi2,pent=zent,psalt=zsalt )
!
!  print*,'(5) gltools_enthalpy =',tptfl%llo+tptfl%tlo
!
!
! 4. Farewell message
! ====================
!
  IF (lp1) THEN
    WRITE(noutlu,*) ' '
    WRITE(noutlu,*) '**** LEVEL 4 - END SUBROUTINE THERMO_LEAD_R'
    WRITE(noutlu,*) ' '
  ENDIF
!
END SUBROUTINE glt_thermo_lead_r
!
! ------------------- END SUBROUTINE glt_thermo_lead_r ----------------------
! -----------------------------------------------------------------------
