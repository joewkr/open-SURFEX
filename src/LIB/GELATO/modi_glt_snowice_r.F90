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
! ======================== MODULE modi_glt_snowice_r ========================
! =======================================================================
!
! Goal:
! -----
!   This module contains a subroutine that considers the formation of
! snow ice. If snow accumulates over sea ice, part of the snow layer
! will be invaded by sea water, creating snow ice.
! 
! Method:
! -------
!   See Fichefet and Morales Maqueda, JGR (1997)
!
! Created : 1998 (D. Salas y Melia)
! Modified: 2002/09 (D. Salas y Melia) Better conservation of energy
! Modified: 2009/06 (D. Salas y Melia) Reduced grid
! Modified: 2011/12 (A. Voldoire) New formulation of the zdh change in
!           sea ice thickness + new ice/water fluxes interface CALL 
! Modified: 2012/02 (A. Voldoire) Former change revised, go back to former
! formulation but with correct implementation of glt_updtfl
!
! -------------------- BEGIN MODULE modi_glt_snowice_r ----------------------
!
!THXS_SFX!MODULE modi_glt_snowice_r
!THXS_SFX!INTERFACE 
!THXS_SFX!!
!THXS_SFX!SUBROUTINE glt_snowice_r( tpmxl,tpsil,tptfl,tpsit,tpdia )
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  USE modd_glt_param
!THXS_SFX!  TYPE(t_mxl), DIMENSION(np), INTENT(in) ::  &
!THXS_SFX!        tpmxl 
!THXS_SFX!  TYPE(t_vtp), DIMENSION(nl,nt,np), INTENT(inout) ::  &
!THXS_SFX!        tpsil
!THXS_SFX!  TYPE(t_tfl), DIMENSION(np), INTENT(inout) ::  &
!THXS_SFX!        tptfl
!THXS_SFX!  TYPE(t_sit), DIMENSION(nt,np), INTENT(inout) ::  &
!THXS_SFX!        tpsit
!THXS_SFX!  TYPE(t_dia), DIMENSION(np), INTENT(inout) ::  &
!THXS_SFX!        tpdia
!THXS_SFX!END SUBROUTINE glt_snowice_r
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_glt_snowice_r
!
! --------------------- END MODULE modi_glt_snowice_r -----------------------
!
!
!
! -----------------------------------------------------------------------
! ----------------------- SUBROUTINE glt_snowice_r --------------------------
!
! * Subroutine which takes into account the formation of snow ice.
!
SUBROUTINE glt_snowice_r( tpmxl,tpsil,tptfl,tpsit,tpdia )
!
  USE modd_glt_const_thm
  USE modd_types_glt
  USE modd_glt_param
  USE mode_gltools_enthalpy
  USE modi_glt_updtfl_r
!
  IMPLICIT NONE
!
  TYPE(t_mxl), DIMENSION(np), INTENT(in) ::  &
        tpmxl 
  TYPE(t_vtp), DIMENSION(nl,nt,np), INTENT(inout) ::  &
        tpsil
  TYPE(t_tfl), DIMENSION(np), INTENT(inout) ::  &
        tptfl
  TYPE(t_sit), DIMENSION(nt,np), INTENT(inout) ::  &
        tpsit
  TYPE(t_dia), DIMENSION(np), INTENT(inout) ::  &
        tpdia
!
  INTEGER ::  &
        jl,jp,jk 
  REAL ::  &
        zenti,zentf,zssinew,zhsinew
  REAL, DIMENSION(nt,np) ::  &
        zdh,zdmass,zdmsi,zdmsn,zentsi,zentsn,zsalt,ztmp
!  real,dimension(np) :: zei1,zes1,zei2,zes2
!
!
!
! 1. Array initializations
! ======================== 
!
  zdh(:,:) = 0.
!call glt_aventh(tpsit,tpsil,zei1,zes1)
!print*,'enthalpie au debut =',zei1+zes1
!
!
!
! 2. Compute the effects of snow ice formation
! ============================================
!
! 2.1. Criterion for snow ice formation
! -------------------------------------
!
  zdmass(:,:) = tpsit(:,:)%rsn*tpsit(:,:)%hsn -  &
    (rhosw-rhoice)*tpsit(:,:)%hsi
!!    write(noutlu,*)'AAAAAAAAAAAAAA'
!!    write(noutlu,*)'zdmass=',zdmass
!!    write(noutlu,*)'rsn=',tpsit(:,:)%rsn
!!    write(noutlu,*)'hsi=',tpsit(:,:)%hsi
!!    write(noutlu,*)'hsn=',tpsit(:,:)%hsn
!
  WHERE ( zdmass(:,:)<=0. ) 
    zdmass(:,:) = 0.
  ENDWHERE
!
!
! 2.2. Change in ice thickness = change in snow thickness
! -------------------------------
!
!  The "missing" mass is taken from the ocean
  zdh(:,:) =  zdmass(:,:) /  &
    ( tpsit(:,:)%rsn+rhosw-rhoice )
!
!
!
! 3. Energy conservation
! =======================
!
! .. We consider here the snow ice transformation is equivalent to
! incorporating a thickness zh of snow to the mix layer AND to freeze
! the same thickness of ice. 
!    We assume that the sea ice we form has a temperature t_f (sea 
! water freezing point).
!
!
! 3.1. Compute involved masses of ice and snow
! --------------------------------------------
! 
! .. Ice mass variation due to snow ice formation
!
  zdmsi(:,:) = rhoice * tpsit(:,:)%fsi * zdh(:,:) 
!!    write(noutlu,*)'zdmsi=',zdmsi
!
! .. Diagnostic in kg.m-2.s-1 (AR5)
!
  tpdia(:)%sni = SUM( zdmsi(:,:),DIM=1 ) / dtt
!
! .. Snow mass variation due to snow ice formation
!
  zdmsn(:,:) = -tpsit(:,:)%rsn * tpsit(:,:)%fsi * zdh(:,:) 
!!    write(noutlu,*)'zdmsn=',zdmsn
!
!
! 3.2. Massic gltools_enthalpy and salinity of created ice
! -------------------------------------------------
!
! .. Massic gltools_enthalpy and salt content
!
  zentsi(:,:) = 0.
  zsalt(:,:) = 0.
  DO jp=1,np
    DO jk=1,nt
      IF ( zdh(jk,jp)>epsil1 ) THEN
          zsalt(jk,jp) = tpmxl(jp)%sml*( 1. - tpsit(jk,jp)%rsn/rhoice )
          zentsi(jk,jp) = glt_enthalpy0d( tpmxl(jp)%mlf,zsalt(jk,jp) )
!!          print*,'zenti(',jk,')=',zentsi(jk,jp),' zsalt =', zsalt(jk,jp)
!!          print*,'zrsn (',jk,')=',tpsit(jk,jp)%rsn
      ENDIF
    END DO
  END DO
!
!
! 3.3. Massic gltools_enthalpy of removed snow
! -------------------------------------
!
! .. Massic gltools_enthalpy
!
  zentsn(:,:) =  &
    SUM( tpsil(nilay+1:nl,:,:)%ent, DIM=1 )/FLOAT(nslay)
!
!
! 3.4. Update water, heat and salt fluxes affecting the ocean
! ------------------------------------------------------------
!
! .. This is the contribution of formed ice that is incorporated to the slab
! The mass considered is the difference between the ice mass created and the
! snow mass removed
!  but we give glt_updtfl the total change in ice mass with its actual salinity
! this should be equivalent to providing zdmsi+zdmsn and psalt = tpmxl(jp)%sml
!  attention : this is not the case if salinity is fixed or if NEMO coupling of salt is made by a dilution flux!
!
!!    write(noutlu,*)'(1) tio=',tptfl%tio
!!    write(noutlu,*)'(1) wio=',tptfl%wio
!!    write(noutlu,*)'(1) wlo=',tptfl%wlo
!  CALL glt_updtfl_r( 'I2O',tpmxl,tptfl,zdmsi,zentsi,psalt=zsalt)
   DO jk=1,nt
     ztmp(jk,:) = tpmxl(:)%sml
   END DO
  CALL glt_updtfl_r( 'I2O',tpmxl,tptfl,zdmsi+zdmsn,pent=zentsi,psalt=ztmp)
!
! A recrire de facon plus logique apres recombinaison des 2 glt_updtfl_r
  tptfl(:)%tio = tptfl(:)%tio -  &
    SUM(zdmsi*zentsi,DIM=1)/dtt - &
    SUM(zdmsn*zentsn,DIM=1)/dtt

!!    write(noutlu,*)'(2) tio=',tptfl%tio
!!    write(noutlu,*)'(2) wio=',tptfl%wio
!!    write(noutlu,*)'(2) wlo=',tptfl%wlo
!
!
! 3.5. Update water, heat and salt fluxes affecting the ocean
! ------------------------------------------------------------
!
! .. This is the contribution of snow that is removed from the slab
!
!!    write(noutlu,*)'(3) tio=',tptfl%tio
!!    write(noutlu,*)'(3) wio=',tptfl%wio
!!    write(noutlu,*)'(3) wlo=',tptfl%wlo
  CALL glt_updtfl_r( 'FW2I',tpmxl,tptfl,-1.*zdmsn,zentsn )
!!    write(noutlu,*)'(4) tio=',tptfl%tio
!!    write(noutlu,*)'(4) wio=',tptfl%wio
!!    write(noutlu,*)'(4) wlo=',tptfl%wlo
!
!
! 3.6. Final adjustment
! ----------------------
!
! .. Here, for simplicity, we suppose the ice slab has grown 
! vertically homothetically, keeping the vertical gltools_enthalpy
! profile unchanged. However the ice slab has grown thicker, leading
! to a change in vertically integrated enthalpy. This change, which 
! is rather small, will be delivered to the ocean to ensure energy
! conservation. 
!    The best way would be to alter frzvtp (called from updhsi, to
! allow sea ice formation at the top of the slab.
!
!
!write(noutlu,*)'Avant enth. change'
!zenti=0.
!do jl=1,nilay
!zenti=zenti+sf3tinv(jl)*rhoice*  &
!  SUM(tpsil(jl,:,:)%ent*tpsit%hsi*tpsit%fsi)/dtt
!end do
!write(noutlu,*)'Avant enth. change', zenti
!zenti=zenti+  &
! SUM( tpsit%rsn*tpsit%hsn*tpsit%fsi*tpsil(nl,:,:)%ent)/dtt
!write(noutlu,*)'with snow', zenti
  DO jp=1,np
    DO jk=1,nt
      IF ( zdh(jk,jp)>epsil1 ) THEN
!
! .. Temporary new thickness
          zhsinew = tpsit(jk,jp)%hsi + zdh(jk,jp) 
!
! .. Update gltools_enthalpy (zenti, zentf in J.m-3)
          zenti = 0.
          DO jl=1,nilay
            zenti = zenti + sf3tinv(jl)*tpsil(jl,jk,jp)%ent 
          END DO
!!         print*,'Enth. Glace avant (jk=',jk,')=',  &
!!           zenti*tpsit(jk,jp)%hsi/dtt*rhoice
          zentf = ( zenti*tpsit(jk,jp)%hsi + zentsi(jk,jp)*zdh(jk,jp) ) /  &
            zhsinew
!!          print*,'Enth. Glace apres (jk=',jk,')=',  &
!!            zentf*zhsinew/dtt*rhoice
          IF ( zenti/=0. ) THEN
              tpsil(1:nilay,jk,jp)%ent =  &
                tpsil(1:nilay,jk,jp)%ent * zentf/zenti
            ELSE
              tpsil(1:nilay,jk,jp)%ent = zentf 
          ENDIF
!
! .. Update snow thickness
          tpsit(jk,jp)%hsn = tpsit(jk,jp)%hsn - zdh(jk,jp)
!
! .. Update sea ice salinity
          tpsit(jk,jp)%ssi =  &
            ( tpsit(jk,jp)%ssi*tpsit(jk,jp)%hsi + zsalt(jk,jp)*zdh(jk,jp) ) /  &
            zhsinew
!
! .. Update sea ice thickness
          tpsit(jk,jp)%hsi = zhsinew
!
      ENDIF
    END DO
  END DO
!print*,'calcul enthalpie (a la fin) =',  &
!sum(rhoice*tpsit%fsi*zentsi*zdh,dim=1)/dtt - &
!sum(tpsit%fsi*tpsit%rsn*zentsn*zdh,dim=1)/dtt
!print*,'(1) calcul enthalpie=',sum(rhoice*tpsit%fsi*zentsi*zdh,dim=1)/dtt
!print*,'(2) calcul enthalpie=',sum(tpsit%fsi*tpsit%rsn*zentsn*zdh,dim=1)/dtt
!
! .. The loss in sea ice gltools_enthalpy has to be compensated by an
! energy flux sent to the ocean
!
!  tptfl(:)%tio = tptfl(:)%tio -  &
!    rhoice/dtt * SUM( zentsi(:,:)*zdh(:,:)*tpsit(:,:)%fsi,DIM=1 )
!    print*,'ajout %tio =',  &
!    - rhoice/dtt * SUM( zentsi(:,:)*zdh(:,:)*tpsit(:,:)%fsi,DIM=1 )
!
!!    write(noutlu,*)'(6) tio=',tptfl%tio
!!    write(noutlu,*)'(6) wio=',tptfl%wio
!!    write(noutlu,*)'(6) wlo=',tptfl%wlo
!  write(noutlu,*)'hsn apres=',tpsit%hsn
!call glt_aventh(tpsit,tpsil,zei2,zes2)
!print*,'enthalpie a la fin =',zei2+zes2
!print*,'delta / dtt =',(zei2+zes2-zei1-zes1)/dtt
!zenti=0.
!do jl=1,nilay
!zenti=zenti+sf3tinv(jl)*rhoice* &
!SUM(tpsil(jl,:,:)%ent*tpsit%hsi*tpsit%fsi)/dtt
!end do
!write(noutlu,*)'Apres enth. change', zenti
!zenti=zenti+ &
!SUM( tpsit%rsn*tpsit%hsn*tpsit%fsi*tpsil(nl,:,:)%ent)/dtt
!write(noutlu,*)'with snow', zenti
!
END SUBROUTINE glt_snowice_r
!
! ---------------------- END SUBROUTINE glt_snowice_r -----------------------
! -----------------------------------------------------------------------
