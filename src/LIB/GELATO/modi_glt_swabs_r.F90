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
!* Note that here, pent is the vertical gltools_enthalpy profile (updated as
! the routine is called); pvsp is the vertical salinity profile. 
!* pswtra is the absorbed solar heat flux by level by level due to 
! solar irradiance transmission.
!* An gltools_enthalpy difference is returned, by levels (pdhmelt). It is set 
! to zero if the sea ice does not melt, else it is positive where there 
! is melting
!
! Modified: 11/2009 (D. Salas y Melia, Ouessant) 
!  sea ice specific heat is now a function of temperature and salinity
! 
!THXS_SFX!MODULE modi_glt_swabs_r
!THXS_SFX!INTERFACE
!THXS_SFX!!
!THXS_SFX!SUBROUTINE glt_swabs_r  &
!THXS_SFX!        ( tpsit,pswtra,pent,pvsp,pdhmelt )
!THXS_SFX!  USE modd_glt_param
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  TYPE(t_sit), DIMENSION(nt,np), INTENT(in) ::  &
!THXS_SFX!    tpsit
!THXS_SFX!  REAL, DIMENSION(nl,nt,np), INTENT(in) ::  &
!THXS_SFX!    pswtra
!THXS_SFX!  REAL, DIMENSION(nl,nt,np), INTENT(inout) ::  &
!THXS_SFX!    pent
!THXS_SFX!  REAL, DIMENSION(nl,nt,np), INTENT(inout) ::  &
!THXS_SFX!    pvsp
!THXS_SFX!  REAL, DIMENSION(nl,nt,np), INTENT(inout) ::  &
!THXS_SFX!    pdhmelt
!THXS_SFX!END SUBROUTINE glt_swabs_r
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_glt_swabs_r
!
! ----------------------- END MODULE modi_glt_swabs_r -----------------------
!
!
! -----------------------------------------------------------------------
! ------------------------- SUBROUTINE glt_swabs_r --------------------------
!
SUBROUTINE glt_swabs_r  &
        ( tpsit,pswtra,pent,pvsp,pdhmelt )
!
  USE modd_glt_const_thm
  USE modd_glt_param
  USE modd_types_glt
!
  IMPLICIT NONE
!
!
!
! 1. Variables
! ============
!
! 1.1. Dummy arguments
! --------------------
  TYPE(t_sit), DIMENSION(nt,np), INTENT(in) ::  &
        tpsit
  REAL, DIMENSION(nl,nt,np), INTENT(in) ::  &
        pswtra
  REAL, DIMENSION(nl,nt,np), INTENT(inout) ::  &
        pent
  REAL, DIMENSION(nl,nt,np), INTENT(inout) ::  &
        pvsp
  REAL, DIMENSION(nl,nt,np), INTENT(inout) ::  &
        pdhmelt
!
!
! 1.2. Local variables
! --------------------
!
  LOGICAL, DIMENSION(nt,np) ::  &
        omask
  CHARACTER(80) ::  &
        ymess
  INTEGER ::  &
        jl
  REAL ::  &
        zdhmelt
  REAL ::  &
        zicondt,zicondb,zidhi_swa,zidhs_swa,ziswa,zinrg,zfsit
  REAL, DIMENSION(nilay,nt,np) ::  &
        ztsi_m
  REAL, DIMENSION(nl,nt,np) ::  &
        zento
!
!
!
! 1. Initializations 
! ===================
!
! 1.1. Initializations
! ---------------------
!
! .. Sea ice and snow melting point
!
  ztsi_m(:,:,:) = -mu * pvsp(1:nilay,:,:)
!
! .. Initial gltools_enthalpy
!
  zento(:,:,:) = pent(:,:,:)
!
!
!!zfsit = SUM( tpsit(:,:)%fsi, MASK=(omask) )
!!zidhi_swa=0.
!!      DO jl=1,nilay
!!        zidhi_swa = zidhi_swa + rhoice*sf3tinv(jl)*  &
!!          SUM( tpsit(:,:)%fsi*tpsit(:,:)%hsi*zento(jl,:,:))/dtt
!!      END DO
!!      write(noutlu,*)'enthalpy (1)=',zidhi_swa/zfsit
!!!
!
! .. Take solar radiation into account (direct impact on gltools_enthalpy, 
! doing that on temperature leads to non-conservation)
!
! Sea ice part
  DO jl=1,nilay
    WHERE ( tpsit(:,:)%fsi>epsil1 .AND. tpsit(:,:)%hsi>epsil1 )
      pent(jl,:,:) = pent(jl,:,:) +  &
        dtt*pswtra(jl,:,:)/( rhoice*sf3tinv(jl)*tpsit(:,:)%hsi )
    ENDWHERE
  END DO
!!!
!!write(noutlu,*)'pfsi=',tpsit%fsi
!!write(noutlu,*)'phsi=',tpsit%hsi
!!write(noutlu,*)'pswtra=',pswtra(:,3,1)
!!  write(noutlu,*)'---------------'
!!  write(noutlu,*)'dans swabs=',  &
!!    ( pent(1:nilay,3,1)-zento(1:nilay,3,1))*  &
!!    sf3tinv*rhoice*tpsit(3,1)%hsi/dtt
!!  write(noutlu,*)'dans swabs=',  &
!!    sum(( pent(1:nilay,3,1)-zento(1:nilay,3,1))*  &
!!    sf3tinv*rhoice*tpsit(3,1)%hsi/dtt)
!!  write(noutlu,*)'---------------'
!!zidhi_swa=0.
!!      DO jl=1,nilay
!!        zidhi_swa = zidhi_swa + rhoice*sf3tinv(jl)*  &
!!          SUM( tpsit(:,:)%fsi*tpsit(:,:)%hsi*pent(jl,:,:))/dtt
!!      END DO
!!      write(noutlu,*)'enthalpy (3)=',zidhi_swa/zfsit
!!!
!
! Snow part
!!  write(noutlu,*)'enth. snow avant=', &
!!    sum(tpsit%fsi*tpsit(:,:)%rsn*tpsit%hsn*pent(nilay+1,:,:))
  WHERE ( tpsit(:,:)%fsi>epsil1 .AND. tpsit(:,:)%hsn>epsil1 )
    pent(nilay+1,:,:) = pent(nilay+1,:,:) +  &
      dtt*pswtra(nilay+1,:,:)/( tpsit(:,:)%rsn*tpsit(:,:)%hsn )
  ENDWHERE
!!  write(noutlu,*)'enth. snow max=',-xmhofusn0
!!  write(noutlu,*)'enth. snow apres=',  &
!!    sum(tpsit%fsi*tpsit%rsn*tpsit%hsn*pent(nilay+1,:,:))
!
! .. Add up excess energy due to solar radiation (in J.m-2)
!
! Sea ice part
!!write(noutlu,*)'pdhmelt (1)=',sum(pdhmelt,dim=1)/dtt
  DO jl=1,nilay
    WHERE ( pent(jl,:,:)>cpsw*ztsi_m(jl,:,:) )
      pdhmelt(jl,:,:) = pdhmelt(jl,:,:) +  &
        rhoice*tpsit(:,:)%hsi*sf3tinv(jl)*  &
        ( pent(jl,:,:)-cpsw*ztsi_m(jl,:,:) )
      pent(jl,:,:) = cpsw*ztsi_m(jl,:,:)
    ENDWHERE
  END DO
!!!
!!write(noutlu,*)'pdhmelt (2)=',sum(pdhmelt,dim=1)/dtt
!!zidhi_swa=0.
!!      DO jl=1,nilay
!!        zidhi_swa = zidhi_swa +  &
!!          rhoice*sf3tinv(jl)*  &
!!          SUM( tpsit%fsi*tpsit%hsi*pent(jl,:,:))/dtt
!!      END DO
!!      write(noutlu,*)'enthalpy (4)=',zidhi_swa/zfsit
!!!
!
! Snow part
  WHERE ( pent(nilay+1,:,:)>-xmhofusn0 )
    pdhmelt(nilay+1,:,:) = pdhmelt(nilay+1,:,:) +  &
      tpsit(:,:)%rsn*tpsit(:,:)%hsn*( pent(nilay+1,:,:)+xmhofusn0 )
    pent(nilay+1,:,:) = -xmhofusn0
  ENDWHERE
!!write(noutlu,*)'pdhmelt (3)=',sum(pdhmelt,dim=1)/dtt
!!  write(noutlu,*)'enth. snow final=',  &
!!    sum(tpsit%fsi*tpsit%rsn*tpsit%hsn*pent(nilay+1,:,:))
!
!
!
! 6. Final checks
! ================
!
  IF (lp1) THEN 
      WRITE(noutlu,*)
      WRITE(noutlu,*)  &
        'At the end of SWABS (vert. heat diffusion scheme), W/m2 of ice'
!
! Total sea ice concentration
      omask(:,:) = .FALSE.
      WHERE( tpsit(:,:)%hsi>epsil1 )
          omask(:,:) = .TRUE.
      ENDWHERE
      zfsit = SUM( tpsit(:,:)%fsi, MASK=(omask) )
      zfsit = AMAX1( zfsit,epsil1 )
!
! .. Print input solar energy
!
     ziswa = 0.
     DO jl=1,nilay+1
       ziswa = ziswa + SUM( tpsit(:,:)%fsi*pswtra(jl,:,:) ) / zfsit
     END DO
!
     WRITE(noutlu,*)  &
       '    Input solar energy:', ziswa
!!       write(noutlu,*) SUM( pswtra(:,:,1),DIM=1 )
!
! .. Print ice gltools_enthalpy change integral (solar)
!
     zidhi_swa = 0.
     DO jl=1,nilay
       zidhi_swa = zidhi_swa +  &
         rhoice*sf3tinv(jl)*  &
         SUM( tpsit(:,:)%fsi*tpsit(:,:)%hsi*( pent(jl,:,:)-zento(jl,:,:) ) )
     END DO
     zidhi_swa = zidhi_swa / (zfsit*dtt)
!
     WRITE(noutlu,*)  &
       '    Ice gltools_enthalpy change due to solar warming:',  &
       zidhi_swa
!
! .. Print snow gltools_enthalpy change integral (solar)
!
      zidhs_swa =  &
        SUM( tpsit(:,:)%fsi*tpsit(:,:)%rsn*tpsit(:,:)%hsn*  &
        ( pent(nilay+1,:,:)-zento(nilay+1,:,:) ) ) / (zfsit*dtt)
!
      WRITE(noutlu,*)  &
        '    Snow gltools_enthalpy change due to solar warming:',  &
        zidhs_swa
!
      zdhmelt = 0.
      DO jl=1,nl
        zdhmelt = zdhmelt +  &
          SUM( tpsit(:,:)%fsi*pdhmelt(jl,:,:) ) / (zfsit*dtt)
      END DO
!
      WRITE(noutlu,*)  &
        '    dhmelt:', zdhmelt
!
! .. Energy variation of the whole system (due to non-solar + solar flux)
!
      zinrg = ziswa - ( zdhmelt + zidhi_swa + zidhs_swa )
!
      WRITE(noutlu,*)  &
        '    Energy variation of the whole system (solar):', zinrg
!
! .. If the energy variation of the system is too large, issue a warning
!
      IF ( ABS(zinrg)>0.01 ) THEN
          WRITE(noutlu,*)
          WRITE(noutlu,*) '                         ** WARNING **'
          WRITE(noutlu,*)  &
            '          Heat conduction scheme not conservative'
          WRITE(noutlu,*)  &
            '         ========================================='
      ENDIF
!
  ENDIF
!
  END SUBROUTINE glt_swabs_r
