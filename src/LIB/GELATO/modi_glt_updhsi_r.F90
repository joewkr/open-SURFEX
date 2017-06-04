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
! ======================== MODULE modi_glt_updhsi_r =========================
! =======================================================================
!
! Goal:
! -----
!   Update the thickness of a sea ice cover, knowing the different 
! fluxes at the top and bottom of the slab.
!
! Created : 2001/07 (D. Salas y Melia)
!           Taken out from thermo_ice routine.  
! Modified: 2009/06 (D. Salas y Melia)
!           Reduced grid
! Modified: (A. Voldoire)
!           new ice/water fluxes interface CALL
!           Split of ice/water fluxes for melting and freezing for conservation
!           (not linear)
!
! ---------------------- BEGIN MODULE modi_glt_updhsi_r ---------------------
!
!THXS_SFX!MODULE modi_glt_updhsi_r
!THXS_SFX!INTERFACE 
!THXS_SFX!!
!THXS_SFX!SUBROUTINE glt_updhsi_r  &
!THXS_SFX!        ( pcondb,pqtopmelt,pdhmelt,tpmxl,tpdia,tptfl,tpsit,tpsil )
!THXS_SFX!!
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  USE modd_glt_param
!THXS_SFX!  REAL, DIMENSION(nt,np), INTENT(in) ::  &
!THXS_SFX!        pcondb,pqtopmelt
!THXS_SFX!  REAL, DIMENSION(nl,nt,np), INTENT(in) ::  &
!THXS_SFX!        pdhmelt
!THXS_SFX!  TYPE(t_mxl), DIMENSION(np), INTENT(in) ::  &
!THXS_SFX!        tpmxl
!THXS_SFX!  TYPE(t_dia), DIMENSION(np), INTENT(inout) ::   &
!THXS_SFX!        tpdia
!THXS_SFX!  TYPE(t_tfl), DIMENSION(np), INTENT(inout) ::   &
!THXS_SFX!        tptfl
!THXS_SFX!  TYPE(t_sit), DIMENSION(nt,np), INTENT(inout) ::  &
!THXS_SFX!        tpsit
!THXS_SFX!  TYPE(t_vtp), DIMENSION(nl,nt,np), INTENT(inout) ::  &
!THXS_SFX!        tpsil
!THXS_SFX!END SUBROUTINE glt_updhsi_r
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_glt_updhsi_r
!
! ---------------------- END MODULE modi_glt_updhsi_r -----------------------
!
!
! -----------------------------------------------------------------------
! ------------------------- SUBROUTINE glt_updhsi_r -------------------------
!
SUBROUTINE glt_updhsi_r  &
        ( pcondb,pqtopmelt,pdhmelt,tpmxl,tpdia,tptfl,tpsit,tpsil )
!
  USE modd_glt_const_thm
  USE modd_types_glt
  USE modd_glt_param
  USE modi_glt_updtfl_r
  USE modi_glt_saltrap_r
  USE modi_glt_frzvtp_r
  USE modi_glt_mltvtp_r
  USE mode_glt_stats_r
  USE mode_gltools_enthalpy
!
  IMPLICIT NONE
!
!* Arguments
!
  REAL, DIMENSION(nt,np), INTENT(in) ::  &
        pcondb,pqtopmelt
  REAL, DIMENSION(nl,nt,np), INTENT(in) ::  &
        pdhmelt
  TYPE(t_mxl), DIMENSION(np), INTENT(in) ::  &
        tpmxl
  TYPE(t_dia), DIMENSION(np), INTENT(inout) ::   &
        tpdia
  TYPE(t_tfl), DIMENSION(np), INTENT(inout) ::   &
        tptfl
  TYPE(t_sit), DIMENSION(nt,np), INTENT(inout) ::  &
        tpsit
  TYPE(t_vtp), DIMENSION(nl,nt,np), INTENT(inout) ::  &
        tpsil
!
!* Local variables
!
  CHARACTER(4) ::  &
        yword
  INTEGER ::  &
        jk,jl
  LOGICAL, DIMENSION(np) ::  &
        yfreeze
  REAL, DIMENSION(np) ::  &
        zfsit,zqtio,zwork,zssib,zentb,zdhsib,ztem
  REAL, DIMENSION(nt,np) ::  &
        zlf,zhsi,zhsn,zssi,zdmsi,zdmsn,zqfac,zqres,zwork2,zent0,zqtio2,zhsi_m
  REAL, DIMENSION(nilay,nt,np) ::  &
        zdhi
!  real,dimension(np) :: zei1,zei2,zes1,zes2
!
!
!
! 1. Initializations
! ==================
!
! .. Enthalpy transfer between melted ice and ocean
! 
  zqtio2(:,:) = 0.
!
! .. Total available flux (zqfac) and flux affecting the ocean (zqtio)
!
  zfsit(:) = SUM( tpsit(:,:)%fsi,DIM=1 )
  zqfac(:,:) = pcondb(:,:) +  &
    SPREAD( tpmxl(:)%qoc + tpmxl(:)%qml,1,nt )
!write(noutlu,*) '(1) pdhmelt =',pdhmelt
!write(noutlu,*) '(7) pcondb =',pcondb
!write(noutlu,*) '(7) qoc =',tpmxl%qoc
!write(noutlu,*) '(7) qoc =',tpmxl%qml
!write(noutlu,*) '(7) qfac =',zqfac
!
! .. Diagnostic of ocean-ice heat flux per m2 of the marine surface
!
  tpdia(:)%qoi = tpdia(:)%qoi + tpmxl(:)%qoc*zfsit(:)
!
! .. Heat flux sent to the ocean
!
  zqtio(:) = - tpmxl(:)%qoc*zfsit(:) - tpmxl(:)%qml
!
! .. Initial snow and ice thicknesses
!
  zhsn(:,:)=tpsit(:,:)%hsn
  zhsi(:,:)=tpsit(:,:)%hsi
!
! .. Enthalpy of melting sea ice
!
  zent0(:,:) = -cpsw * mu * tpsit(:,:)%ssi
!
!
!
! 2. Treat sea ice temperatures exceeding melting point 
! ======================================================
!
!   1) For every level, compute the energy flux that is necessary to 
! bring sea ice temperature down to tice_m
!   2) Melt part of the concerned level
!   3) If the level has melted completely, the rest of the energy is sent
! to the mixed layer
!
!!write(noutlu,*)'zfsit=',zfsit
!!write(noutlu,*)'fsi  =',tpsit%fsi
  DO jl=1,nilay
!
! Initialize sea ice vertical level thicknesses
    zdhi(jl,:,:) = zhsi(:,:) * sf3tinv(jl)
!
!!write(noutlu,*)'****************** JL =',jl
!!write(noutlu,*) 'stop 1'
!!write(noutlu,*) 'zdhi(jl,2,:) = ',zdhi(jl,2,:)
!!write(noutlu,*) 'pdhmelt(jl,2,:) = ',pdhmelt(jl,2,:)
!!write(noutlu,*) '(6) qfac =',zqfac
!!write(noutlu,*) 'zqfac(2,:) = ',zqfac(2,:)
! pdhmelt > 0 means that sea ice gltools_enthalpy is at its maximum 
! (i.e. sea ice has melted completely); sea ice gltools_enthalpy goes to the mixed layer
    WHERE( pdhmelt(jl,:,:)>0. )
      zqtio2(:,:) = zqtio2(:,:) +  &
        ( rhoice * zdhi(jl,:,:) * tpsil(jl,:,:)%ent + pdhmelt(jl,:,:) ) / dtt 
      zdhi(jl,:,:) = 0.
!      zqfac(:,:) = zqfac(:,:) + pdhmelt(jl,:,:)/dtt
    ENDWHERE
!!write(noutlu,*) '(5) qfac =',zqfac
!!write(noutlu,*) 'stop 2'
!!write(noutlu,*) 'tpsil(jl,2,:)=',tpsil(jl,2,:)%ent
!!write(noutlu,*) 'zdhi(jl,2,:) = ',zdhi(jl,2,:)
!!write(noutlu,*) 'zqfac(2,:) = ',zqfac(2,:)
!
! Melt some ice (if at this level sea ice temperature > tice_m)
    WHERE( tpsil(jl,:,:)%ent>=zent0(:,:) .AND. zhsi(:,:)>0. )
      zqtio2(:,:) = zqtio2(:,:) +  &
        rhoice*( tpsil(jl,:,:)%ent-zent0(:,:) )* zdhi(jl,:,:)/dtt
!      zqtio2(:,:) = zqtio2(:,:) +  &
!        rhoice*zent0(:,:)*tpsit(:,:)%fsi*zdhi(jl,:,:)/dtt
      zdhi(jl,:,:) = 0.
      tpsil(jl,:,:)%ent = zent0(:,:)
    ENDWHERE
!!write(noutlu,*) 'stop 3'
!!write(noutlu,*) 'tpsil(jl,2,:)=',tpsil(jl,2,:)%ent
!!write(noutlu,*) 'zdhi(jl,2,:) = ',zdhi(jl,2,:)
!!write(noutlu,*) '(4) qfac =',zqfac
!!write(noutlu,*) 'zqfac(2,:) = ',zqfac(2,:)
!
  END DO
!
! .. Diagnose top melting (Melting Rate Top) - negative if melting occurs 
! [kg.m-2.s-1]
!
  tpdia(:)%mrt =  &
    rhoice*SUM(  &
      tpsit(:,:)%fsi*( SUM( zdhi(:,:,:),DIM=1 )-tpsit(:,:)%hsi ), DIM=1 )/dtt
! 
!
!
! 3. Case of basal sea ice melting
! =================================
!
! .. Now that zqfac is completely defined, update thickness of sea ice layers
!
!
! 3.1. Update sea ice thickness (level by level)
! -----------------------------------------------
!
  DO jl=1,nilay
!
! .. Note that this thickness can become negative if zqfac is too intense.
!!write(noutlu,*) '(7) qfac =',zqfac
!!write(noutlu,*) '(LOOP 4.1) qfac =',zqfac
!
    WHERE ( zqfac(:,:)>0. .AND. zdhi(jl,:,:)>0. )
      zdhi(jl,:,:) = zdhi(jl,:,:) + dtt*zqfac(:,:)/  &
        ( tpsil(jl,:,:)%ent*rhoice )
!
! No problem in melting current ice layer: all zqfac was used.
      WHERE ( zdhi(jl,:,:)>=0. )
        zqfac(:,:) = 0.
! 
! Current ice layer melting was not enough to use all of qmelt ;
! zqfac will be used to melt all of or part of next ice level.
      ELSEWHERE
        zqfac(:,:) = rhoice*tpsil(jl,:,:)%ent*zdhi(jl,:,:) / dtt
        zdhi(jl,:,:) = 0.
      ENDWHERE
!
    ENDWHERE
!!write(noutlu,*) '(LOOP 4.2) qfac =',zqfac
!!write(noutlu,*) 'zqfac residuel (jl=',jl,'), zqfac=',zqfac(2,:)
!
  END DO
!
! 
! 3.2. Update snow thickness
! --------------------------
!
! .. If zqfac is still positive, it means all the sea ice has melted. All the 
! snow must disappear. All of/part of it contributes to reducing zqfac, the
! rest will go to the mixed layer.
!
!!write(noutlu,*) '(8) qfac =',zqfac
  zqres(:,:) = 0.
  WHERE ( zqfac(:,:)>0. )
    zqfac(:,:) =  zqfac(:,:) +  &
      tpsit(:,:)%hsn*tpsit(:,:)%rsn/dtt *  &
        SUM( tpsil(nilay+1:nl,:,:)%ent,DIM=1 )/FLOAT(nslay)
    zhsn(:,:) = 0.
    WHERE ( zqfac(:,:)<0. )  ! Compute residual heat flux
      zqres(:,:) = zqfac(:,:)
      zqfac(:,:) = 0.
    ENDWHERE
  ENDWHERE
!
!!write(noutlu,*) '(3) qfac =',zqfac
!!write(noutlu,*) 'zqfac residuel apres neige zqfac=',zqfac(2,:)
!
!
!
! 4. Correct temperature vertical profiles 
! =========================================
!
! 4.1. Case of sea ice melting
! -----------------------------
!
!CALL glt_aventh(tpsit,tpsil,zei1,zes1)
!print*,'Enthalpie avant =',zei1+zes1
  CALL glt_mltvtp_r( zdhi,zhsi,tpsil )
!print*,'zqtio2 =',sum(zqtio2*tpsit%fsi,dim=1)
!    print*,'(2) zqfac=',sum(zqfac*tpsit%fsi,dim=1)
!    print*,'(2) zqres=',sum(zqres*tpsit%fsi,dim=1)
! CHANGEMENT d'enthalpie = CHANGEMENT de zqfac

! Update water, heat and salt fluxes affecting the ocean due to melting
  zdmsi(:,:) = rhoice *  &
    ( zhsi(:,:)-tpsit(:,:)%hsi ) * tpsit(:,:)%fsi
  zhsi_m(:,:) = zhsi(:,:)
  zwork2(:,:) = 0.
  CALL glt_updtfl_r( 'I2O',tpmxl,tptfl,zdmsi,pent=zwork2,psalt=tpsit(:,:)%ssi )
!  print*,'Enthalpie envoyee a l ocean =',SUM(0.*zdmsi,dim=1)/dtt
!
!
! 4.2. Case of sea ice freezing
! ------------------------------
!
! .. AR5 diagnostic: compute the rate of congelation at the base of the ice
! slab due to the bottom conduction heat flux separately from other 
! processes (reproduce what is in glt_frzvtp_r, but for pcondb alone)
! [kg.m-2.s-1]
  tpdia(:)%cgl = 0.
  DO jk=1,nt
    ztem(:) = -2.
    yfreeze(:) = ( pcondb(jk,:)<0. )
    CALL glt_saltrap_r( yfreeze,pcondb(jk,:),ztem(:),tpmxl,zssib,zentb,zdhsib )
    tpdia(:)%cgl = tpdia(:)%cgl + tpsit(jk,:)%fsi*zdhsib(:)
  END DO
  tpdia(:)%cgl = rhoice*tpdia(:)%cgl/dtt   ! Convert to kg.m-2.s-1
!
  CALL glt_frzvtp_r( tpmxl,tpsit,zqfac,zhsi,zssi,tpsil )
!
! .. Sea ice freezing 
!   - zqfac was completely used: set it to 0.
!   - reference salinity is computed by glt_frzvtp_r
  WHERE( zqfac(:,:)<0. ) 
    zqfac(:,:) = 0.
  ENDWHERE
!
! Update water, heat and salt fluxes affecting the ocean due to freezing
  zdmsi(:,:) = rhoice *  &
    ( zhsi(:,:)-zhsi_m(:,:) ) * tpsit(:,:)%fsi
  CALL glt_updtfl_r( 'I2O',tpmxl,tptfl,zdmsi,pent=zent0,psalt=zssi )
!
!
!
! 5. Energy conservation
! =======================
! 
! 5.1. Flux sent to the mixed layer due to residual zqfac and zqtio
! ------------------------------------------------------------------
! 
!!write(noutlu,*) '(avant) tio =',tptfl(:)%tio
!!write(noutlu,*) '(avant)qfac =',zqfac
!!write(noutlu,*) '(avant)qtio2=',zqtio2
  tptfl(:)%tio = tptfl(:)%tio +  &
    SUM( ( zqfac(:,:)+zqtio2(:,:)+zqres(:,:) )*tpsit(:,:)%fsi,DIM=1 ) +  &
    zqtio(:)
!print*,'tio=',tptfl(:)%tio
!print*,'zqfac=',zqfac
!print*,'fsi=',tpsit%fsi
!print*,'zqtio2=',zqtio2
!print*,'zqtio=',zqtio
!print*,'zqres=',zqres
  zqfac(:,:) = 0.
!!write(noutlu,*) '(apres) tio =',tptfl(:)%tio
!
!
! 5.2. Compute involved fluxes of snow 
! ------------------------------------
!
! .. Variation of snow mass due to vertical freezing/melting
!
! If the ice has melted completely, the snow (if any) should go to the ocean
!
  WHERE ( tpsit(:,:)%hsi<epsil1 .AND. tpsit(:,:)%fsi>=epsil1 ) 
    zhsn(:,:) = 0.
  ENDWHERE
  zdmsn(:,:) = tpsit(:,:)%rsn *  &
    ( zhsn(:,:)-tpsit(:,:)%hsn ) * tpsit(:,:)%fsi
!
!
! 5.3. Massic gltools_enthalpy of removed snow
! -------------------------------------
!
! Before going to the mixed layer, snow has been warmed up to gltools_enthalpy=0
! Not needed anymore, done directly in glt_updtfl_r 
!  zent0(:,:) = 0.
!
!
! 5.4. Update water, heat and salt fluxes affecting the ocean
! ------------------------------------------------------------
!
  CALL glt_updtfl_r( 'FW2O',tpmxl,tptfl,zdmsn)
!
!
!
! 6. Last updates
! ===============
!
! 6.1. AR5 diagnostic: bottom melting
! ------------------------------------
!
! .. We tried to separate congelation from ocean-heat flux melting at the
! bottom of the sea ice slab. However, it may turn out that the diagnosed
! sea ice mass vertical rate due to ocean-heat flux is positive. In this 
! case we set it to zero and increase the congelation ice growth rate.
!
  tpdia(:)%mrb = SUM( zdmsi(:,:),DIM=1 )/dtt - tpdia(:)%cgl - tpdia(:)%mrt
!
  WHERE( tpdia(:)%mrb>0. )
    tpdia(:)%cgl = tpdia(:)%cgl + tpdia(:)%mrb
    tpdia(:)%mrb = 0.
  ENDWHERE
!
!
! 6.2. All cases: update sea ice/snow thicknesses
! -----------------------------------------------
!
! .. Update ice layer thickness
!
  tpsit(:,:)%hsi = zhsi(:,:) 
!
! .. Update snow layer thickness
!
  tpsit(:,:)%hsn = zhsn(:,:)
!
!CALL glt_aventh(tpsit,tpsil,zei2,zes2)
!print*,'Enthalpie apres  =',zei2+zes2
!print*,'Delta Enthalpie  =',(zei2+zes2-zei1-zes1)/dtt
!
!
! 6.3. Reinitialize a sea ice type in case of total melting
! ---------------------------------------------------------
!
  WHERE ( tpsit(:,:)%hsi<epsil1 .AND. tpsit(:,:)%fsi>=epsil1 ) 
    tpsit(:,:)%esi = .FALSE.
    tpsit(:,:)%fsi = 0.
    tpsit(:,:)%hsi = 0.
    tpsit(:,:)%asn = albw
    tpsit(:,:)%hsn = 0.
    tpsit(:,:)%rsn = rhosnwmin
  ENDWHERE
!
  IF ( niceage==1 ) THEN
    WHERE ( tpsit(:,:)%hsi<epsil1 .AND. tpsit(:,:)%fsi>=epsil1 ) 
      tpsit(:,:)%age = 0.
    ENDWHERE
  ENDIF
!
  IF ( nicesal==1 ) THEN
    WHERE ( tpsit(:,:)%hsi<epsil1 .AND. tpsit(:,:)%fsi>=epsil1 ) 
      tpsit(:,:)%ssi = 0.
    ENDWHERE
  ENDIF
!
  IF ( nmponds==1 ) THEN
    WHERE ( tpsit(:,:)%hsi<epsil1 .AND. tpsit(:,:)%fsi>=epsil1 ) 
      tpsit(:,:)%vmp = 0.
    ENDWHERE
  ENDIF
!
END SUBROUTINE glt_updhsi_r
!
! ----------------------- END SUBROUTINE glt_updhsi_r -----------------------
! -----------------------------------------------------------------------
