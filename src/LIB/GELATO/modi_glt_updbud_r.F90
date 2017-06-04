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
! ======================= MODULE modi_glt_updbud_r ======================
! =======================================================================
!
! Goal:
! -----
!   This module contains a subroutine that manages the energy budget 
! variable on the thermodynamics, reduced grid:
!       - total sea ice gltools_enthalpy (reference temperature is the melting
!       temperature of snow and ice)
!       - total sea ice latent heat (i.e. the latent heat needed to 
!       melt sea ice and snow covers on the whole ocean domain). Note 
!       sea ice stored heat is included in that.
!       - total stored heat in sea ice.
!       - input and glt_output heat concerning leads and sea ice.
!   All these quantities are given in J. The energy budget is also 
! printed (total values on the whole mesh).
!
! Created : 2009/06 (D. Salas y Melia)
! Modified: 2009/12 (D. Salas y Melia) Enthalpy replaces temperature
!   as a state variable
! Modified: 2010/01 (D. Salas y Melia) Introduce fresh water budget
! Modified: 2010/03 (D. Salas y Melia) Introduce salinity budget
!
! --------------------- BEGIN MODULE modi_glt_updbud_r ----------------------
!
!THXS_SFX!MODULE modi_glt_updbud_r
!THXS_SFX!INTERFACE 
!THXS_SFX!!
!THXS_SFX!SUBROUTINE glt_updbud_r  &
!THXS_SFX!  ( kinit,omsg,tpdom,tpmxl,tptfl,tpatm,tpblkw,tpblki,tpsit,tpsil,tpbud )
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  USE modd_glt_param
!THXS_SFX!  INTEGER, INTENT(in) ::  &
!THXS_SFX!         kinit
!THXS_SFX!  CHARACTER(*), INTENT(in) ::  &
!THXS_SFX!        omsg
!THXS_SFX!  TYPE(t_dom), DIMENSION(np), INTENT(in) ::  &
!THXS_SFX!        tpdom
!THXS_SFX!  TYPE(t_mxl), DIMENSION(np), INTENT(in) ::  &
!THXS_SFX!        tpmxl
!THXS_SFX!  TYPE(t_tfl), DIMENSION(np), INTENT(in) ::  &
!THXS_SFX!        tptfl
!THXS_SFX!  TYPE(t_atm), DIMENSION(np), INTENT(in) ::  &
!THXS_SFX!        tpatm
!THXS_SFX!  TYPE(t_blk), DIMENSION(np), INTENT(in) ::  &
!THXS_SFX!        tpblkw
!THXS_SFX!  TYPE(t_blk), DIMENSION(nt,np), INTENT(in) ::  &
!THXS_SFX!        tpblki
!THXS_SFX!  TYPE(t_sit), DIMENSION(nt,np), INTENT(in) ::  &
!THXS_SFX!        tpsit
!THXS_SFX!  TYPE(t_vtp), DIMENSION(nl,nt,np), INTENT(in) ::  &
!THXS_SFX!        tpsil
!THXS_SFX!  TYPE(t_bud), DIMENSION(np), INTENT(inout) ::  &
!THXS_SFX!        tpbud
!THXS_SFX!END SUBROUTINE glt_updbud_r
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_glt_updbud_r
!
! ---------------------- END MODULE modi_glt_updbud_r -----------------------
!
!
!
! -----------------------------------------------------------------------
! ------------------------ SUBROUTINE glt_updbud_r --------------------------
!
! .. Subroutine used to check global heat budget.
!
SUBROUTINE glt_updbud_r  &
  ( kinit,omsg,tpdom,tpmxl,tptfl,tpatm,tpblkw,tpblki,tpsit,tpsil,tpbud )
!
  USE modd_types_glt
  USE modd_glt_param
  USE modd_glt_const_thm
  USE mode_glt_stats_r
  USE mode_glt_info_r
!
  IMPLICIT NONE
!
  INTEGER, INTENT(in) ::  &
         kinit
  CHARACTER(*), INTENT(in) ::  &
         omsg
  TYPE(t_dom), DIMENSION(np), INTENT(in) ::  &
        tpdom
  TYPE(t_mxl), DIMENSION(np), INTENT(in) ::  &
        tpmxl
  TYPE(t_tfl), DIMENSION(np), INTENT(in) ::  &
        tptfl
  TYPE(t_atm), DIMENSION(np), INTENT(in) ::  &
        tpatm
  TYPE(t_blk), DIMENSION(np), INTENT(in) ::  &
        tpblkw
  TYPE(t_blk), DIMENSION(nt,np), INTENT(in) ::  &
        tpblki
  TYPE(t_sit), DIMENSION(nt,np), INTENT(in) ::  &
        tpsit
  TYPE(t_vtp), DIMENSION(nl,nt,np), INTENT(in) ::  &
        tpsil
  TYPE(t_bud), DIMENSION(np), INTENT(inout) ::  &
        tpbud
!
  INTEGER ::  &
        jl
  REAL ::  &
        zenthalpy0,zhiit0,zbiit0,zhii0,zhio0,   &
        zhli0,zhlo0,zwio0,zwlo0,zwii0,zwli0,zcio0,zclo0,  &
        zwater0,zsalt0
  REAL, DIMENSION(np) ::  &
        zfsit
  REAL, DIMENSION(np) ::  &
        zenthalpy2,zhiit2,zbiit2,  &
        znli2,zniit2,zhli2,zhlo2,zwii2,zwli2,zwater2,zsalt2
  REAL, DIMENSION(nt,np) ::  &
        zenthalpys,zenthalpyi,zmsi,zmsn
!
!
!
! 1. Initializations
! ==================
!
  IF (lp1) THEN
      WRITE(noutlu,*) ' ' 
      WRITE(noutlu,*) ' **** glt_updbud_r ****' 
      WRITE(noutlu,*) omsg, '   (energy fluxes in W.m-2)'
      WRITE(noutlu,*) omsg, '   (water and salt fluxes in kg.m-2.day-1)'
  ENDIF
!
!
! 1.1. Initialize arrays
! -----------------------
!
  zfsit(:) = SUM( tpsit(:,:)%fsi,DIM=1 )
  zmsi(:,:) = rhoice * tpsit(:,:)%fsi * tpsit(:,:)%hsi
  zmsn(:,:) = tpsit(:,:)%rsn * tpsit(:,:)%fsi * tpsit(:,:)%hsn
!
!
! 1.2. Print information 
! -----------------------
!
  CALL glt_info_si_r( omsg,tpsit=tpsit )
!
!
!
! 2. Incoming fluxes
! ===================
!
! 2.1. Incoming fluxes on sea ice (top+bottom of the slab)
! ---------------------------------------------------------
!
! .. Note that the incoming energy at the top represents on the one
! hand the sum of solar and non solar fluxes, the snow layer latent
! heat change due to new snowfalls, and the related snow layer gltools_enthalpy 
! variation.
! (note that tpatm%sop is in kg.m-2.s-1)
!
  IF ( kinit==1 ) THEN
!
! Energy
      zniit2(:) = tpatm(:)%sop *  &
        SUM( tpsit(:,:)%fsi*tpsil(nl,:,:)%ent, DIM=1 )
!        ( -xmhofusn0*zfsit(:) +  &
!          SUM( tpsit(:,:)%fsi *  &
!               ( cpice0*tpsit(:,:)%tsf ),DIM=1 ) )  ! +  &
!                  tpatm(:)%lip*  &
!        xmhofusn0*zfsit(:)
! atm -> ice energy
      zhiit2(:) =  &
        SUM( tpsit(:,:)%fsi*  &
        ( tpblki(:,:)%nsf+tpblki(:,:)%swa ), DIM=1 )
! oce -> ice energy ( qoc can only be determined a posteriori, in glt_updhsi_r )
      zbiit2(:) =  &
        tpmxl(:)%qml*zfsit(:)    ! + tpmxl(:)%qoc
!
      tpbud(:)%nii = zniit2(:)
      tpbud(:)%hii = zhiit2(:)+zniit2(:)
      tpbud(:)%bii = zbiit2(:)
!
! Water
      zwii2(:) = zfsit(:)*( tpatm(:)%sop + tpatm(:)%lip ) +  &
        SUM( tpsit(:,:)%fsi*tpblki(:,:)%eva, DIM=1 )
      tpbud(:)%wii = zwii2(:)*xday2sec 
  ENDIF
!
  IF ( nprinto>=1 ) THEN
!
! Energy
      zhiit0 = glt_avg_r( tpdom,tpbud(:)%hii,0 )
      zbiit0 = glt_avg_r( tpdom,tpbud(:)%bii,0 )
      zhii0 = zhiit0 + zbiit0
!
! Water
      zwii0 = glt_avg_r( tpdom,tpbud(:)%wii,0 )
!
      IF (lwg) THEN
        WRITE(noutlu,*)  &
          '--------------------------------------------------------------------'
        WRITE(noutlu,*) '    Incoming ENERGY under sea ice      :',  &
          zbiit0
        WRITE(noutlu,*) '    Incoming ENERGY top of sea ice     :',  &
          zhiit0
        WRITE(noutlu,*) '    Total incoming ENERGY on sea ice   :',  &
          zhii0
        WRITE(noutlu,*)  &
          '--------------------------------------------------------------------'
        WRITE(noutlu,*) '    Total incoming WATER on sea ice    :',  &
          zwii0
        WRITE(noutlu,*)  &
          '--------------------------------------------------------------------'
      ENDIF
  ENDIF
!
!
! 2.2. Incoming fluxes on leads
! ------------------------------
!
  IF ( kinit==1 ) THEN
!
! Energy
      zhli2(:) =  &
        ( 1.-zfsit(:) )*   &
        ( tpmxl(:)%qml+tpblkw(:)%nsf+tpblkw(:)%swa )    
      IF ( nsnwrad==1 ) THEN
          znli2(:) = ( 1.-zfsit(:) )*  &
!            ( cpice0*tpmxl(:)%mlf-xmhofusn0 )*tpatm(:)%sop    ! Not justified
            ( -xmhofusn0 )*tpatm(:)%sop
        ELSE
          znli2(:) = 0.
      ENDIF
      tpbud(:)%nli = znli2(:)
      tpbud(:)%hli = zhli2(:)+znli2(:)
!
! Water
      zwli2(:) = ( 1.-zfsit(:) )*  &
        ( tpatm(:)%sop + tpatm(:)%lip + tpblkw(:)%eva )*xday2sec
      tpbud(:)%wli = zwli2(:) 
  ENDIF
!
  IF ( nprinto>=1 ) THEN
!
! Energy
      zhli0 = glt_avg_r( tpdom,tpbud(:)%hli,0 )
      IF (lwg) THEN
        WRITE(noutlu,*) '    Total incoming ENERGY (on leads)   :',  &
          zhli0
        WRITE(noutlu,*)  &
          '--------------------------------------------------------------------'
      ENDIF
!
! Water
      zwli0 = glt_avg_r( tpdom,tpbud(:)%wli,0 )
      IF (lwg) THEN
        WRITE(noutlu,*) '    Total incoming WATER (on leads)    :',  &
          zwli0
        WRITE(noutlu,*)  &
          '--------------------------------------------------------------------'
      ENDIF 
  ENDIF
!
!
!
! 3. Outgoing fluxes
! ===================
!
! 3.1. Outgoing fluxes at the bottom of the sea ice slab
! -------------------------------------------------------
!
! Energy
  tpbud(:)%hio = tptfl(:)%lio+tptfl(:)%tio
!
  IF ( nprinto>=1 ) THEN
      zhio0 = glt_avg_r( tpdom,tpbud(:)%hio,0 )
      IF (lwg) THEN
        WRITE(noutlu,*) '    Total outgoing ENERGY (under sea ice) :',  &
          zhio0
        WRITE(noutlu,*)  &
          '--------------------------------------------------------------------'
      ENDIF 
!
! Water
! This flux is just described by tptfl%wio
!
      zwio0 = glt_avg_r( tpdom,tptfl(:)%wio,0 )*xday2sec
      IF (lwg) THEN
        WRITE(noutlu,*) '    Total outgoing WATER (under sea ice)  :',  &
          zwio0
        WRITE(noutlu,*)  &
          '--------------------------------------------------------------------'
      ENDIF
!
! Salt
! A conversion is necessary to convert the concentration/dilution flux to
! salt mass changes ( in kg.m-2 ):
!     D S = sml * cio
!
      zsalt2(:) = tptfl(:)%sio
      zcio0 = glt_avg_r( tpdom,zsalt2,0 )*xday2sec
      IF (lwg) THEN
        WRITE(noutlu,*) '    Total outgoing SALT (under sea ice)   :',  &
          zcio0
        WRITE(noutlu,*)  &
          '--------------------------------------------------------------------'
      ENDIF
  ENDIF
!
!
! 3.2. Outgoing fluxes through the water surface
! -----------------------------------------------
!
! Energy
  zhlo2(:) = tptfl(:)%llo+tptfl(:)%tlo 
!
  tpbud(:)%hlo = zhlo2(:)
!
  IF ( nprinto>=1 ) THEN
      zhlo0 = glt_avg_r( tpdom,tpbud(:)%hlo,0 )
      IF (lwg) THEN
        WRITE(noutlu,*) '    Total outgoing ENERGY (under leads)   :',  &
          zhlo0
        WRITE(noutlu,*)  &
          '--------------------------------------------------------------------'
      ENDIF
!
! Water
      zwlo0 = glt_avg_r( tpdom,tptfl%wlo,0 )*xday2sec
      IF (lwg) THEN
        WRITE(noutlu,*) '    Total outgoing WATER (under leads)    :',  &
          zwlo0
        WRITE(noutlu,*)  &
          '--------------------------------------------------------------------'
      ENDIF
!
! Salt
! A conversion is necessary to convert the concentration/dilution flux to
! salt mass changes ( in kg.m-2 ):
!     D S = sml * wlo
!
!      zsalt2(:) = 1.e-3 * tpmxl(:)%sml * tptfl(:)%wlo
!      zclo0 = glt_avg_r( tpdom,zsalt2,0 )*xday2sec
!      WRITE(noutlu,*) '    Total outgoing SALT (under leads)     :',  &
!        zclo0
!      WRITE(noutlu,*)  &
!        '--------------------------------------------------------------------'
  ENDIF
!
!
!
! 4. Energy/water/salt retained by sea ice/snow per square meter
! ===============================================================
!
! 4.1. Enthalpy in sea ice/snow per square meter
! ----------------------------------------------
!
  zenthalpyi(:,:) = 0.
  DO jl=1,nilay
    zenthalpyi(:,:) = zenthalpyi(:,:) +  &
      sf3tinv(jl) * zmsi(:,:) * tpsil(jl,:,:)%ent
  END DO
!
  zenthalpys(:,:) =  &
    zmsn(:,:) * SUM( tpsil(nilay+1:nl,:,:)%ent,DIM=1 )/FLOAT(nslay)
!
  zenthalpy2(:) = SUM( zenthalpys(:,:)+zenthalpyi(:,:), DIM=1 )
!
! .. Initial gltools_enthalpy is set to computed gltools_enthalpy if kinit flag is on. 
! Else initial gltools_enthalpy is kept as such.
!
  IF ( kinit==1 ) THEN
      tpbud(:)%eni = zenthalpy2(:)
      tpbud(:)%enn = zenthalpy2(:)
    ELSE
      tpbud(:)%enn = zenthalpy2(:)
  ENDIF
!
  IF ( nprinto>=1 ) THEN
      zenthalpy0 = glt_avg_r( tpdom, tpbud(:)%enn-tpbud(:)%eni,0 ) / dtt
      IF (lwg) THEN
        WRITE(noutlu,*)  &
          '    D(enthalpy) from beg. of time step :',  &
          zenthalpy0
        WRITE(noutlu,*)  &
          '--------------------------------------------------------------------'
      ENDIF
  ENDIF
!
!
! 4.2. Water stored by sea ice and snow
! --------------------------------------
!
! .. Note that we exclude salt (only fresh water is taken into account)  
!
! Sea ice + snow fresh water content
  zwater2(:) =  xday2sec*( &
    SUM( zmsi(:,:)*( 1.-1.e-3*tpsit(:,:)%ssi ), DIM=1 ) +  &
    SUM( zmsn(:,:), DIM=1 ) )
!
! .. Initial fresh water content of sea ice + snow cover is initialised if
! kinit flag is on.
!
  IF ( kinit==1 ) THEN
      tpbud(:)%fwi = zwater2(:)
  ENDIF
  tpbud(:)%fwn = zwater2(:)
!
  IF ( nprinto>=1 ) THEN
      zwater0 = glt_avg_r( tpdom,tpbud(:)%fwn-tpbud(:)%fwi,0 ) / dtt
      IF (lwg) THEN
        WRITE(noutlu,*) '    D(water) from beg. of time step   :',  &
          zwater0
        WRITE(noutlu,*)  &
          '--------------------------------------------------------------------'
      ENDIF
  ENDIF
! 
!
! 4.3. Salt stored by sea ice
! ----------------------------
!
! Sea ice salt content
  zsalt2(:) = xday2sec*  &
    SUM( zmsi(:,:)*1.e-3*tpsit(:,:)%ssi, DIM=1 )
!
! .. Initial salt content of sea ice is initialised if kinit flag is on.
!
  IF ( kinit==1 ) THEN
      tpbud(:)%isi = zsalt2(:)
  ENDIF
  tpbud(:)%isn = zsalt2(:)
!
  IF ( nprinto>=1 ) THEN
      zsalt0 = glt_avg_r( tpdom,tpbud(:)%isn-tpbud(:)%isi,0 ) / dtt
      IF (lwg) THEN
        WRITE(noutlu,*) '    D(salt) from beg. of time step    :',  &
          zsalt0
        WRITE(noutlu,*)  &
          '--------------------------------------------------------------------'
      ENDIF
  ENDIF
!
!
! 5. Final prints: general budget
! ===============================
!
! .. At the end of the time step, the sum of incoming energy on leads
! and on sea ice should be equal to the sum of the total variation of
! gltools_enthalpy in sea ice+snow and of the total outgoing energy.
!    A print of these two quantities is done here.
!
  IF (lp1) THEN
      WRITE(noutlu,*) '    Total incoming energy (leads+ice)  :',  &
        zhii0+zhli0
      WRITE(noutlu,*) '    D(enthalpy) + outg. ENERGY         :',  &
        zenthalpy0+zhio0+zhlo0
      WRITE(noutlu,*) '    ENERGY BALANCE                     :',  &
        zenthalpy0+zhio0+zhlo0-(zhii0+zhli0)
      WRITE(noutlu,*)  &
        '--------------------------------------------------------------------'
      WRITE(noutlu,*) '    Total incoming + outgoing water (leads+ice)   :',  &
        zwii0+zwli0-zwio0-zwlo0
      WRITE(noutlu,*) '    WATER  BALANCE                                :',  &
        zwii0+zwli0-zwio0-zwlo0-zwater0
      WRITE(noutlu,*)  &
        '--------------------------------------------------------------------'
      WRITE(noutlu,*) '    SALT  BALANCE                                 :',  &
        -zcio0-zsalt0
      WRITE(noutlu,*)  &
        '--------------------------------------------------------------------'
  ENDIF
!
END SUBROUTINE glt_updbud_r

! ---------------------- END SUBROUTINE glt_updbud_r ------------------------
! -----------------------------------------------------------------------
