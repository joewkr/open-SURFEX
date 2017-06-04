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
! ========================= MODULE modi_glt_sndmlrf =========================
! =======================================================================
!
! This routine was created for Gelato version 3, i.e. Gelato is under the
! form of a routine inserted in the OPA 8 code. 
! It allows Gelato to transmit the forcing the ocean needs as a routine 
! glt_output argument.
!  
! Created : 10/1999 (D. Salas y Melia)
! Modified: 02/2000 (D. Salas y Melia)
!    Suppress the computation of stress derivatives
! Modified: 08/2009 (D. Salas y Melia)
!    Adaptation to the new Gelato interface
! Modified: 06/2010 (D. Salas y Melia)
!    Collects the mass of snow that is lost/gained due to advection
!  processes and redistribute it under sea ice (by hemisphere) 
! Modified: 12/2011 (A. Voldoire)
!    New ice/water fluxes interface CALL + add sio from tzdfl to tptfl
! Modified: 07/2012 (D. Salas y Melia) 
!    Parallelism 
!
! --------------------- BEGIN MODULE modi_glt_sndmlrf -----------------------
!
!THXS_SFX!MODULE modi_glt_sndmlrf
!THXS_SFX!INTERFACE
!THXS_SFX!!
!THXS_SFX!SUBROUTINE glt_sndmlrf(pbathy,tpdom,tpatc,tpml,tpdia,tpsit,tptfl,  &
!THXS_SFX!  pustar,tpall_oce )
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  USE modd_glt_param
!THXS_SFX!  REAL, DIMENSION(nx,ny), INTENT(in) ::  &
!THXS_SFX!        pbathy
!THXS_SFX!  TYPE(t_dom), DIMENSION(nx,ny), INTENT(in) ::  &
!THXS_SFX!        tpdom
!THXS_SFX!  TYPE(t_atm), DIMENSION(nx,ny), INTENT(in) ::  &
!THXS_SFX!        tpatc
!THXS_SFX!  TYPE(t_mxl), DIMENSION(nx,ny), INTENT(in) ::  &
!THXS_SFX!        tpml
!THXS_SFX!  TYPE(t_dia), DIMENSION(nx,ny), INTENT(inout) ::  &
!THXS_SFX!        tpdia
!THXS_SFX!  TYPE(t_sit), DIMENSION(nt,nx,ny), INTENT(in) ::  &
!THXS_SFX!        tpsit
!THXS_SFX!  TYPE(t_tfl), DIMENSION(nx,ny), INTENT(inout) ::  &
!THXS_SFX!        tptfl
!THXS_SFX!  REAL, DIMENSION(nx,ny), INTENT(out) ::  &
!THXS_SFX!        pustar
!THXS_SFX!  TYPE(t_2oc), DIMENSION(nx,ny), INTENT(inout) ::  &
!THXS_SFX!        tpall_oce
!THXS_SFX!END SUBROUTINE glt_sndmlrf
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_glt_sndmlrf
!
! ---------------------- END MODULE modi_glt_sndmlrf ------------------------
!
!
!
! -----------------------------------------------------------------------
! ----------------------- SUBROUTINE glt_sndmlrf ----------------------------
!
SUBROUTINE glt_sndmlrf( pbathy,tpdom,tpatc,tpml,tpdia,tpsit,tptfl,  &
  pustar,tpall_oce )
  USE modd_types_glt
  USE modd_glt_param
  USE modd_glt_const_thm
  USE modd_glt_const_evp
#if ! defined in_surfex
  USE mode_gltools_bound
#endif
  USE modi_gltools_adjflx
  USE modi_glt_updtfl
!
  IMPLICIT NONE

  REAL, DIMENSION(nx,ny), INTENT(in) ::  &
        pbathy
  TYPE(t_dom), DIMENSION(nx,ny), INTENT(in) ::  &
        tpdom
  TYPE(t_atm), DIMENSION(nx,ny), INTENT(in) ::  &
        tpatc
  TYPE(t_mxl), DIMENSION(nx,ny), INTENT(in) ::  &
        tpml
  TYPE(t_dia), DIMENSION(nx,ny), INTENT(inout) ::  &
        tpdia
  TYPE(t_sit), DIMENSION(nt,nx,ny), INTENT(in) ::  &
        tpsit
  TYPE(t_tfl), DIMENSION(nx,ny), INTENT(inout) ::  &
        tptfl
  REAL, DIMENSION(nx,ny), INTENT(out) ::  &
        pustar
  TYPE(t_2oc), DIMENSION(nx,ny), INTENT(inout) ::  &
        tpall_oce
!
  INTEGER ::  &
        ji,jj
  LOGICAL, DIMENSION(nx,ny) ::  &
        ycrit
  REAL, DIMENSION(nx,ny) ::  &
        zfsit,zfld,ztxgw,ztygw,zmsi,zmsa,zssi
  REAL, DIMENSION(nt,nx,ny) ::  &
        zdm,zent,zsalt
  REAL, DIMENSION(nx,ny) ::  &
        zwork2,zhsit_ext,zdx_ext,zdy_ext
  TYPE(t_tfl), DIMENSION(nx,ny) ::  &
        tzdfl
!
!
! 
! 1. Momentum flux
! =================
!
! .. Transmitted u and v stress components (N.m-2). This stress is the
! result of a weighing between ice-ocean stress (for ice covered areas)
! and air-ocean stress.
!       - Note that in glt_gelato, Tx(i,j) and Ty(i,j) are respectively 
! defined at the middle of the western and southern corners of T(i,j) 
! cell. 
!       - The glt_output stress should be defined also on Arakawa-C grid,
! but with Tx(i,j) at the middle of the eastern corner of T(i,j) cell 
! and Ty(i,j) at the middle of the northern corner of T(i,j) cell.
!       - So the glt_output grid should be shifted by one grid cell along
! the X and Y axis. To achieve that, the initial fields are bounded, 
! then 
!
! .. Sea ice total fraction and lead fraction
!
  zfsit(:,:) = SUM( tpsit(:,:,:)%fsi,DIM=1 )
  zfld(:,:) = 1.-zfsit(:,:)
!
! .. Ice/water friction velocity
!
  pustar(:,:) = SQRT( SQRT(tptfl(:,:)%xio**2+tptfl(:,:)%yio**2)/rhosw )
!
! .. Where ocean depth is less than 80m, assume that ice-water stress 
! is equal to ice-air stress multiplied by leads fraction
! (we assume that sea ice does not exert any stress on water)
!
  WHERE ( pbathy(:,:)<=80. )
    tptfl(:,:)%xio = tpatc(:,:)%ztx
    tptfl(:,:)%yio = tpatc(:,:)%mty
  ENDWHERE
!
! .. Global ice+air/water stress : u-component
!
  tpdia(:,:)%atx = zfsit(:,:)*tpatc(:,:)%ztx
  tpdia(:,:)%otx = -zfsit(:,:)*tptfl(:,:)%xio
  ztxgw(:,:) = -tpdia(:,:)%otx + zfld(:,:)*tpatc(:,:)%ztx
!
  ztxgw(:,:) = ztxgw(:,:)*FLOAT( tpdom(:,:)%umk ) 
!
! .. Global ice+air/water stress : v-component
!
  tpdia(:,:)%aty = zfsit(:,:)*tpatc(:,:)%mty
  tpdia(:,:)%oty = -zfsit(:,:)*tptfl(:,:)%yio
  ztygw(:,:) = -tpdia(:,:)%oty + zfld(:,:)*tpatc(:,:)%mty
!
  ztygw(:,:) = ztygw(:,:)*FLOAT( tpdom(:,:)%vmk ) 

#if ! defined in_surfex
  CALL gltools_bound( 'U','vector',ztxgw )
  CALL gltools_bound( 'V','vector',ztygw )
#endif
!
!
!
! 2. Take the effect of snow, salt and ice mass changes due to dynamics
! ======================================================================
!
! 2.1. Initialise fluxes transmitted to the ocean
! ------------------------------------------------
!
  CALL initfl( tzdfl )
!
  IF ( nadvect==1 .AND. ndyncor==1 ) THEN
!
!
! 2.2. Effect of snow
! --------------------
!
! .. The snow mass change is computed per hemisphere and redistributed
! to the ice zones.
!
    zdm(:,:,:) = 0.
    zent(:,:,:) = 0.
!
! -> north hemisphere
    ycrit = ( tpdom(:,:)%lat>0..AND.tpdom(:,:)%tmk==1.AND.zfsit(:,:)>epsil1 ) 
    zdm(1,:,:) = zdm(1,:,:) + gltools_adjflx( tpdom,ycrit,tpdia%ddn )
! -> south hemisphere
    ycrit = ( tpdom(:,:)%lat<0..AND.tpdom(:,:)%tmk==1.AND.zfsit(:,:)>epsil1 ) 
    zdm(1,:,:) = zdm(1,:,:) + gltools_adjflx( tpdom,ycrit,tpdia%ddn )
!
    CALL glt_updtfl('FW2O', tpml,tzdfl,zdm,pent=zent )
!
!
! 2.3. Effect of sea ice 
! -----------------------
!
    zdm(:,:,:) = 0.
    zent(:,:,:) = 0.
    zsalt(:,:,:) = 0.
!
! .. Compute average ice mass and salinity changes 
! (note that %dds is in kg.kg-1)
!
! -> north hemisphere
    ycrit = ( tpdom(:,:)%lat>0..AND.tpdom(:,:)%tmk==1.AND.zfsit(:,:)>epsil1 ) 
    zmsi = gltools_adjflx( tpdom,ycrit,tpdia%ddi )
    zmsa = gltools_adjflx( tpdom,ycrit,tpdia%dds )
    WHERE( ABS( zmsi(:,:) )>epsil2 )
        zssi(:,:) = zmsa(:,:)/zmsi(:,:)
      ELSEWHERE
        zmsi(:,:) = 0. 
        zssi(:,:) = 0.
    ENDWHERE
    zdm(1,:,:) = zdm(1,:,:) + zmsi(:,:)
!    print*,'compensatory flux (north)=',glt_avg(tpdom,zmsi(:,:),0)
    zsalt(1,:,:) = zsalt(1,:,:) + 1000.*zssi(:,:) ! should be provided in g.kg-1
!
! -> south hemisphere
    ycrit = ( tpdom(:,:)%lat<0..AND.tpdom(:,:)%tmk==1.AND.zfsit(:,:)>epsil1 ) 
    zmsi = gltools_adjflx( tpdom,ycrit,tpdia%ddi )
    zmsa = gltools_adjflx( tpdom,ycrit,tpdia%dds )
    WHERE( ABS( zmsi(:,:) )>epsil2 )
        zssi(:,:) = zmsa(:,:)/zmsi(:,:)
      ELSEWHERE
        zmsi(:,:) = 0. 
        zssi(:,:) = 0.
    ENDWHERE
    zdm(1,:,:) = zdm(1,:,:) + zmsi(:,:)
!    print*,'compensatory flux (south)=',glt_avg(tpdom,zmsi(:,:),0)
    zsalt(1,:,:) = zsalt(1,:,:) + 1000.*zssi(:,:) ! should be provided in g.kg-1
!    print*,'Compare...'
!    print*,'sea ice mass change =',glt_avg(tpdom,tpdia%ddi,0)*dtt
!    print*,'compensatory flux=',glt_avg(tpdom,zdm(1,:,:),0)
!    print*,'Compare...'
!    print*,'salt mass change =',glt_avg(tpdom,tpdia%dds,0)*dtt
!    print*,'compensatory flux=',glt_avg(tpdom,zsalt(1,:,:)*zdm(1,:,:),0)/1000.
!
    CALL glt_updtfl('I2O', tpml,tzdfl,zdm,pent=zent,psalt=zsalt )
!
! 
! 2.4. Add up correction to initial fluxes
! -----------------------------------------
!
    tptfl(:,:)%tio = tptfl(:,:)%tio + tzdfl(:,:)%tio
    tptfl(:,:)%tlo = tptfl(:,:)%tlo + tzdfl(:,:)%tlo
    tptfl(:,:)%wio = tptfl(:,:)%wio + tzdfl(:,:)%wio
    tptfl(:,:)%wlo = tptfl(:,:)%wlo + tzdfl(:,:)%wlo
    tptfl(:,:)%cio = tptfl(:,:)%cio + tzdfl(:,:)%cio
    tptfl(:,:)%sio = tptfl(:,:)%sio + tzdfl(:,:)%sio
  ENDIF
!
!
!
! 3. Define all the fields sent to the ocean model
! =================================================
!
! * Please note that total sea ice fraction is used by the ocean but was
! already computed by glt_sndatmf routine.
!
!
! 3.1. Transmitted non solar heat flux (W.m-2)
! ---------------------------------------------
!
  zwork2(:,:) =  &
    ( tptfl(:,:)%tio + tptfl(:,:)%tlo )*  &
    FLOAT( tpdom(:,:)%tmk )
#if ! defined in_surfex
  CALL gltools_bound( 'T','scalar',zwork2 )
#endif
  tpall_oce(:,:)%nsf = zwork2(:,:)
!
!
! 3.2. Transmitted solar heat flux (W.m-2)
! -----------------------------------------
!
  zwork2(:,:) = ( tptfl(:,:)%lio + tptfl(:,:)%llo )*  &
    FLOAT( tpdom(:,:)%tmk )
#if ! defined in_surfex
  CALL gltools_bound( 'T','scalar',zwork2 )
#endif
  tpall_oce(:,:)%swa = zwork2(:,:)
!
!
! 3.3. Transmitted water fluxes (kg.m-2.s-1)
! -------------------------------------------
!
! .. Note that here the sign convention is that a water flux is 
! positive is the ocean gains fresh water
!
! Concentration / dilution flux
!
  zwork2(:,:) =  &
    ( tptfl(:,:)%cio + tptfl(:,:)%wlo + tptfl(:,:)%wio )*  &
    FLOAT( tpdom(:,:)%tmk )
#if ! defined in_surfex
  CALL gltools_bound( 'T','scalar',zwork2 )
#endif
  tpall_oce(:,:)%cdf = zwork2(:,:)
!
! Salt flux
!  Note that the ocean uses the concentration/dilution flux or the salt flux not both  
!
  zwork2(:,:) =  &
    ( tptfl(:,:)%sio )*FLOAT( tpdom(:,:)%tmk )
#if ! defined in_surfex
  CALL gltools_bound( 'T','scalar',zwork2 )
#endif
  tpall_oce(:,:)%saf = zwork2(:,:)
!
! Water flux
!
! If the ocean model ignores the mass of water exchanged between sea ice and
! ocean ("levitating sea ice")
  zwork2(:,:) =  &
    ( tptfl(:,:)%wio + tptfl(:,:)%wlo )*FLOAT( tpdom(:,:)%tmk )
!
! Just comment these two lines if you do not want to have %sp1, %sp2 as outputs
  tpdia%sp1 = tzdfl%wio
  tpdia%sp2 = tzdfl%wlo
!
#if ! defined in_surfex
  CALL gltools_bound( 'T','scalar',zwork2 )
#endif
  tpall_oce(:,:)%wfl = zwork2(:,:)
!
!
! 3.4. Momentum fluxes (N.m-2)
! -----------------------------
!
! .. u-component
!
  zwork2(:,:) = ztxgw(:,:)*FLOAT( tpdom(:,:)%umk ) 
#if ! defined in_surfex
  CALL gltools_bound( 'U','vector',zwork2 )
#endif
  tpall_oce(:,:)%ztx = zwork2(:,:)
!
! .. v-component
!
  zwork2(:,:) = ztygw(:,:)*FLOAT( tpdom(:,:)%vmk )
#if ! defined in_surfex
  CALL gltools_bound( 'V','vector',zwork2 )
#endif
  tpall_oce(:,:)%mty = zwork2(:,:)
!
!
! 3.5. Friction velocity (m.s-1)
! -------------------------------
!
  zwork2(:,:) = pustar(:,:)*FLOAT( tpdom(:,:)%tmk )
#if ! defined in_surfex
  CALL gltools_bound( 'T','scalar',zwork2 )
#endif
  tpall_oce(:,:)%ust = zwork2(:,:)


! 
END SUBROUTINE glt_sndmlrf
!
! --------------------- END SUBROUTINE glt_sndmlrf --------------------------
! -----------------------------------------------------------------------
