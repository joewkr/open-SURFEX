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
!
!* Note that here, pent is the vertical profile (updated as
! the routine is called); pvsp is the vertical salinity profile. 
!* pswtra is the absorbed solar heat flux by level by level due to 
! solar irradiance transmission.
!* An enthalpy difference is returned, by levels (pdhmelt), in J.m-2. 
! It is set to zero if the sea ice does not melt, else it is positive 
! where there is melting
!
! Modified: 11/2009 (D. Salas y Melia, Ouessant - storm outside) 
!  sea ice specific heat is now a function of temperature and salinity
! Modified: 02/2010 (D. Salas y Melia)
!  to make the routine shorter, ice only / ice+snow slabs are treated 
! through two routine CALL.
! 
!THXS_SFX!MODULE modi_glt_vhdiff_r
!THXS_SFX!INTERFACE
!THXS_SFX!!
!THXS_SFX!SUBROUTINE glt_vhdiff_r  &
!THXS_SFX!        ( tpdom,pmlf,pderiv,tpsit,tpdia,  &
!THXS_SFX!        pnsftop,pswtra,pent,pvsp,pcondb,pqtopmelt,pdhmelt,gsmelt )
!THXS_SFX!  USE modd_glt_param
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  TYPE(t_dom), DIMENSION(np), INTENT(in) ::  &
!THXS_SFX!    tpdom
!THXS_SFX!  REAL, DIMENSION(np), INTENT(in) ::  &
!THXS_SFX!    pmlf
!THXS_SFX!  REAL, DIMENSION(nt,np), INTENT(inout) ::  &
!THXS_SFX!    pderiv
!THXS_SFX!  TYPE(t_sit), DIMENSION(nt,np), INTENT(inout) ::  &
!THXS_SFX!    tpsit
!THXS_SFX!  TYPE(t_dia), DIMENSION(np), INTENT(inout) ::  &
!THXS_SFX!    tpdia
!THXS_SFX!! Note that this input pnsftop is improper (input is NS heat flux)
!THXS_SFX!! Output is top melt heat flux
!THXS_SFX!  REAL, DIMENSION(nt,np), INTENT(inout) ::  &
!THXS_SFX!    pnsftop
!THXS_SFX!  REAL, DIMENSION(nl,nt,np), INTENT(in) ::  &
!THXS_SFX!    pswtra
!THXS_SFX!  REAL, DIMENSION(nl,nt,np), INTENT(inout) ::  &
!THXS_SFX!    pent
!THXS_SFX!  REAL, DIMENSION(nl,nt,np), INTENT(inout) ::  &
!THXS_SFX!    pvsp
!THXS_SFX!  REAL, DIMENSION(nt,np), INTENT(out) ::  &
!THXS_SFX!    pcondb,pqtopmelt
!THXS_SFX!  REAL, DIMENSION(nl,nt,np), INTENT(out) ::  &
!THXS_SFX!    pdhmelt
!THXS_SFX!  LOGICAL, DIMENSION(nt,np), INTENT(out) ::  &
!THXS_SFX!    gsmelt
!THXS_SFX!END SUBROUTINE glt_vhdiff_r
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_glt_vhdiff_r
!
! ---------------------- END MODULE modi_glt_vhdiff_r -----------------------
!
!
! -----------------------------------------------------------------------
! ------------------------ SUBROUTINE glt_vhdiff_r --------------------------
!
SUBROUTINE glt_vhdiff_r  &
        ( tpdom,pmlf,pderiv,tpsit,tpdia,  &
        pnsftop,pswtra,pent,pvsp,pcondb,pqtopmelt,pdhmelt,gsmelt )
!
  USE modd_glt_const_thm
  USE modd_glt_param
  USE modd_types_glt
  USE modd_glt_vhd
  USE modi_gltools_temper_r
  USE mode_gltools_enthalpy
  USE modi_gltools_glterr
  USE modi_glt_vhdslab_r
  USE mode_glt_stats_r
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
  TYPE(t_dom), DIMENSION(np), INTENT(in) ::  &
    tpdom
  REAL, DIMENSION(np), INTENT(in) ::  &
    pmlf
  REAL, DIMENSION(nt,np), INTENT(inout) ::  &
    pderiv
  TYPE(t_sit), DIMENSION(nt,np), INTENT(inout) ::  &
    tpsit
  TYPE(t_dia), DIMENSION(np), INTENT(inout) ::  &
    tpdia
  REAL, DIMENSION(nt,np), INTENT(inout) ::  &
    pnsftop
  REAL, DIMENSION(nl,nt,np), INTENT(in) ::  &
    pswtra
  REAL, DIMENSION(nl,nt,np), INTENT(inout) ::  &
    pent
  REAL, DIMENSION(nl,nt,np), INTENT(inout) ::  &
    pvsp
  REAL, DIMENSION(nt,np), INTENT(out) ::  &
    pcondb,pqtopmelt
  REAL, DIMENSION(nl,nt,np), INTENT(out) ::  &
    pdhmelt
  LOGICAL, DIMENSION(nt,np), INTENT(out) ::  &
    gsmelt
!
!
! 1.2. Local variables
! --------------------
!
  LOGICAL ::  &
        llice,llsnw,llmelt,llconv,lltempcond
  LOGICAL, DIMENSION(nt,np) ::  &
        omask
  CHARACTER(80) ::  &
        ymess
  INTEGER, PARAMETER ::  &
        nit=20
  INTEGER ::  &
        jp,jk,jl,jit
  INTEGER, DIMENSION(nt,np) ::  &
        icv,icount
  LOGICAL, DIMENSION(nt,np) ::  &
    gmask
  REAL ::  &
        zcva,zcvb,zdhmelt,zhs
  REAL ::  &
        zcpsn,zrks,zfsi,zrkeq,zderiv,   &
        zinsftop,zicondb,zidhi,zidhs,  &
        ziswa,zinrg,ztsfa,zcondb,zqtopmelta
  REAL, DIMENSION(nilay) ::  &
        zdzi
  REAL, DIMENSION(0:nilay) ::  &
        ztsia
  REAL, DIMENSION(nilay) ::  &
        zrkice,zvsp0
  REAL, DIMENSION(np) ::  &
        zfsit
  REAL, DIMENSION(nl) ::  &
        zvtpa,zdhmelta,zdh
  REAL, DIMENSION(nt,np) ::  &
        ztsftrd,zderiv2,ztint,zwork
  REAL, DIMENSION(nt,np) ::  &
        z1,z2,z3,z4,z5,z6,z7,  &
        zswtra_all,zcondb_all,zderiv_all,zdh_all,zqtopmelta_all,  &
        zdhmelt0,zdhmelt1,zdhmelt2,zdhmelt3,zdhmelt4,zdhmelt5,  &
        zei1,zes1,zei2,zes2
  type(t_vtp), dimension(nl,nt,np) :: &
       tzsil
  REAL, DIMENSION(nl,nt,np) ::  &
        ztsi_m
  REAL, DIMENSION(nl,nt,np) ::  &
        zvtptrd,zvtp,zdent,zdenti
  REAL  :: &
       ztot_ice_snow, zfailures
!
!
!
! 2. Initializations 
! ===================
!
! 2.1. Parameter error checks
! ----------------------------
!
  IF ( nslay/=1 ) THEN 
      CALL gltools_glterr( 'vhdiff',  &
        'The number of snow layers should be equal to 1','STOP' ) 
  ENDIF
!
  IF ( nilay<3 ) THEN
      CALL gltools_glterr( 'vhdiff',  &
        'The number of ice layers should be >= 3','STOP' )
  ENDIF
!
!
! 2.2. Allocations
! -----------------
!
! .. Note that for some platforms, these locally-allocated arrays need be
! deallocated (in reverse order) after use.
!
  ALLOCATE( zetai(0:nilay))
  ALLOCATE( zinvetai(0:nilay) )
  ALLOCATE( zetaik(0:nilay))
  ALLOCATE( zetaikp1(0:nilay) )
  ALLOCATE( zrhocpsi(nilay))
  ALLOCATE( ztsib(0:nilay))
  ALLOCATE( ztsi0(0:nilay) )
  ALLOCATE( ztsi_m0(0:nilay) )
  ALLOCATE( zkodzi(0:nilay+1) )
!
!
! 2.3. Global initializations
! ----------------------------
!
! .. Temperature at snow/ice interface diagnostic
!
  ztint(:,:) = xbig20
!
! .. Sea ice and snow melting point
!
  ztsi_m(:,:,:) = -mu * pvsp(:,:,:)
!
! .. Surface flux derivative by T
!
  zderiv2(:,:) = 0.
!
! .. Save initial enthalpy (must be done before first operation on enthalpy)
!
!  zento(:,:,:) = pent(:,:,:)
!
! .. Initialise enthalpy change due to heat diffusion
!
  zdent(:,:,:) = 0.
!
! .. Temperature trends
!
  ztsftrd(:,:) = 0.
  zvtptrd(:,:,:) = 0.
!
! .. Output gltools_enthalpy used to melt ice
!
  pdhmelt(:,:,:) = 0.
!
! .. Output bottom heat flux
!
  pcondb(:,:) = 0.
!
! .. Output boolean indicating melting
!
  gsmelt(:,:) = .FALSE.
!
! .. Constants used for bottom conduction flux
!
!  zg1 = 3.
!  zg2 = -1./3. 
  zg1 = 1.
  zg2 = 0.
!
! .. Initialize convergence boolean and loop counter
!
  icv(:,:) = 0
!
! .. Sea ice mask
  omask(:,:) = .FALSE.
  WHERE( tpsit(:,:)%hsi>epsil1 )
      omask(:,:) = .TRUE.
  ENDWHERE
!
! .. Enthalpy may be greater than melting sea ice gltools_enthalpy, e.g. due to
! transport: correct this, and collect the corresponding energy to melt ice or
! snow.
!
! Sea ice part
!write(noutlu,*)'dhmelt(1)=',sum(sum(pdhmelt(:,:,1),dim=1)*pfsi(:,1))/dtt
! Note that pdhmelt is in J.m-2 and pent is in J.kg-1
!tzsil%vsp = 0.
!tzsil%ent = pent
!call glt_aventh(tpsit,tzsil,zei1,zes1)
!print*,'step 1', sum( sum( pdhmelt,dim=1 )*tpsit%fsi )/dtt
  zdenti = pent(:,:,:)
!
  DO jl=1,nilay
    WHERE ( pent(jl,:,:)>cpsw*ztsi_m(jl,:,:) )
      pdhmelt(jl,:,:) = pdhmelt(jl,:,:) +  &
        rhoice*tpsit(:,:)%hsi*sf3tinv(jl)*( pent(jl,:,:)-cpsw*ztsi_m(jl,:,:) )
      pent(jl,:,:) = cpsw*ztsi_m(jl,:,:)
    ENDWHERE
  END DO
!write(noutlu,*)'dhmelt(2)=',sum(sum(pdhmelt(:,:,1),dim=1)*pfsi(:,1))/dtt
!print*,'step 2', sum( sum( pdhmelt,dim=1 )*tpsit%fsi )/dtt
!
! Snow part
!print*,'enthalpy =',pent(nilay+1,:,:)
!print*,'xmhofusn0=',xmhofusn0
  WHERE ( pent(nilay+1,:,:)>-xmhofusn0 )
    pdhmelt(nilay+1,:,:) = pdhmelt(nilay+1,:,:) +  &
      tpsit(:,:)%rsn*tpsit(:,:)%hsn*( pent(nilay+1,:,:)+xmhofusn0 )
    pent(nilay+1,:,:) = -xmhofusn0
  ENDWHERE
  zdenti = pent(:,:,:) - zdenti(:,:,:)
!tzsil%ent = pent
!call glt_aventh(tpsit,tzsil,zei2,zes2)
!print*,'delta enth (W) in vhdiff (part 1) =',sum(zei2+zes2-zei1-zes1)/dtt
!print*,'detail ='
!write(noutlu,*)'dhmelt(3)=',sum(sum(pdhmelt(:,:,1),dim=1)*pfsi(:,1))/dtt
zdhmelt0 = sum(pdhmelt,dim=1)/dtt
!
! .. Compute temperature vertical profile from gltools_enthalpy
!
  zvtp(:,:,:) = gltools_temper_r( pent(:,:,:),pvsp(:,:,:) )
!
!
! 2.4. Grid point initializations
! --------------------------------
!
  DO jp=1,np
!
! Sea water freezing point
    zmlf = pmlf(jp)
!
    DO jk = 1,nt
!
! PREVIOUS ITERATION STUFF
! -------------------------
! Previous iteration temperature of the atmosphere / bare ice or snow interface
      ztsfb = tpsit(jk,jp)%tsf
!
! Previous iteration ice vertical temperature profile (downward)
      DO jl=1,nilay
        ztsib(jl) = zvtp(nilay+1-jl,jk,jp)
      END DO
!
! Previous iteration snow temperature (middle of the snow layer)
      ztsib(0) = zvtp(nl,jk,jp)
!
! INITIAL STUFF
! --------------
! Initial thermodynamical state of the ice slab
      zfsi = tpsit(jk,jp)%fsi
      zdzi(:) = tpsit(jk,jp)%hsi * sf3t(:)
!
! Initial ice melting temperature (downward)
      DO jl=1,nilay
        ztsi_m0(jl) = ztsi_m(nilay+1-jl,jk,jp)
        zvsp0(jl) = pvsp(nilay+1-jl,jk,jp)
      END DO
!
! Initial snow melting temperature
      ztsi_m0(0) = 0.
!
! Initial ice vertical temperature profile (downward)
      ztsi0(:) = ztsib(:)
!
! Initial temperature of the atmosphere / bare ice or snow interface
      ztsf0 = ztsfb
!
! Initial top conduction heat flux
      znsftop0 = pnsftop(jk,jp)
!
! Top conductive flux
      zcondt_m = znsftop0 
!
! Initial snow caracteristics
      zrks = 0.30 ! rkice * ( prsn(jk,jp)/rhow )**1.885
      zcpsn = cpice0
!
! CURRENT ITERATION STUFF
! ------------------------
! Current iteration vertical temperature profile (to compute sea ice spec. heat
! at first iteration)
!
      ztsia(:) = ztsi0(:)
!
! .. Define booleans and associated parameters 
!
      llconv = .FALSE.
      llice = ( zfsi>epsil1 .AND. tpsit(jk,jp)%hsi>epsil1 )
      llsnw = ( llice .AND. zfsi>epsil1 .AND. tpsit(jk,jp)%hsn>epsil1 )
!
! Initialize iteration counter
    jit = 0
!
    DO WHILE ( icv(jk,jp)==0 .AND. jit<=nit )
!
      jit = jit+1
!
      llredo = .false.
!
10    CONTINUE
!
! .. Prints
!
      IF ( llredo .AND. lwg ) THEN
          WRITE(noutlu,*) 'Sea ice concentr. = ',zfsi
          WRITE(noutlu,*) 'Sea ice thickness = ',tpsit(jk,jp)%hsi
          WRITE(noutlu,*) 'Snow thickness    = ',tpsit(jk,jp)%hsn
          WRITE(noutlu,*) 'Snow density      = ',tpsit(jk,jp)%rsn
      ENDIF
!
! Current iteration excess gltools_enthalpy vertical profile (downward)
      zdhmelta(:) = 0.
!
! .. All necessary quantities are computed from the top to the bottom of
! the ice-snow slab.
!
      IF ( llice ) THEN 
!
! Update sea ice volumic specific heat
!  - in the case of pure ice ( ztsi_m0=0. ) ztsi0 and or ztsia may be =0
          zrhocpsi(:) = rhocpice0
          WHERE( ztsi_m0(1:nilay)<-epsil1 )
            zrhocpsi(:) = zrhocpsi(:) -  &
              hofusn0*ztsi_m0(1:nilay) / ( ztsi0(1:nilay)*ztsia(1:nilay) )
          ENDWHERE
!!          write(noutlu,*)'zrhocpsi=',zrhocpsi
!!          write(noutlu,*)'tsi_m0  =',ztsi_m0(1:nilay)
!!          write(noutlu,*)'tsi0    =',ztsi0(1:nilay)
!!          write(noutlu,*)'tsia    =',ztsia(1:nilay)
!
! Update sea ice conductivity
          WHERE( ztsia(1:nilay)<xtempc )
            zrkice(:) =  rhoice/rhoice0 *  &
              ( xrki1 + xrki2*ztsia(1:nilay) +  &
                xrki3 * zvsp0(1:nilay) / ztsia(1:nilay) )
          ELSEWHERE
            zrkice(:) = rhoice/rhoice0 *  &
              ( xrki1 + xrki2*xtempc + xrki3 * zvsp0(:) / xtempc ) 
          ENDWHERE
          zetai(1:nilay) = dtt / ( zrhocpsi(:)*zdzi(:) )
          zinvetai(1:nilay) = 1./zetai(1:nilay)
!
          IF ( llsnw ) THEN
!IF ( tpsit(jk,jp)%rsn<epsil1 ) print*,'prsn,jk,jp=',tpsit(jk,jp)%rsn,jk,jp
!IF ( zcpsn<epsil1 ) print*,'zcpsn,jk,jp=',zcpsn,jk,jp
!IF ( tpsit(jk,jp)%hsn<epsil1 ) print*,'phsn,jk,jp=',tpsit(jk,jp)%hsn,jk,jp
!
! - heat capacity factor
              zetai(0) = dtt / ( tpsit(jk,jp)%rsn*zcpsn*tpsit(jk,jp)%hsn )
              zinvetai(0) = 1./zetai(0)
! - snow layer conductivity
              zkodzi(0) = 2.*zrks / tpsit(jk,jp)%hsn
! - equivalent conductivity at the ice-snow interface
              zkodzi(1) = 2.*zrkice(1)*zrks /  &
                ( zrkice(1)*tpsit(jk,jp)%hsn + zrks*zdzi(1) )
!
            ELSE
! - heat capacity factor (unused -> "1" is just to avoid a division by 0)
              zetai(0) = 1.
              zinvetai(0) = 0.
! - snow layer conductivity (unused)
              zkodzi(0) = 0.
! - top of the ice slab conductivity
              zkodzi(1) = 2.*zrkice(1)/zdzi(1)
          ENDIF
!
! - effective conductivity (general case)
          DO jl=2,nilay
            zkodzi(jl) = 2.*zrkice(jl-1)*zrkice(jl) /  &
              ( zrkice(jl-1)*zdzi(jl) + zrkice(jl)*zdzi(jl-1) )
          END DO 
! - effective conductivity (bottom of the ice slab)
          zkodzi(nilay+1) = 2.*zrkice(nilay)/zdzi(nilay)
!
! Derived coefficients
          zetaik(:) = zetai(:)*zkodzi(0:nilay) 
          zetaikp1(:) = zetai(:)*zkodzi(1:nilay+1) 
      ENDIF
!
!
!
! 3. Vertical heat diffusion in and ice (+snow) slab
! ===================================================
!
      IF ( llice ) THEN
!
          zderiv = pderiv(jk,jp)
!
          CALL glt_vhdslab_r   &
            ( jit,pnsftop(jk,jp),pswtra(:,jk,jp),  &
            zderiv,zcondb,ztsfa,zqtopmelta,zdh,ztsia,llmelt,osnow=llsnw )
!
          pcondb(jk,jp) = zcondb
          zderiv2(jk,jp) = zderiv
          gsmelt(jk,jp) = llmelt
          zdent(:,jk,jp) = zdh(:)
!
          IF (lp4) THEN
              WRITE(noutlu,*) ' Non-solar heat flux at the top:',  &
                      pnsftop(jk,jp)
              zswtra_all(jk,jp) = xswhdfr * SUM(pswtra(:,jk,jp))
              WRITE(noutlu,*) 'Absorbed solar heat flux:',  &
                zswtra_all(jk,jp)     ! Difference de calcul avec modi_glt_vhdslab_r
              zcondb_all(jk,jp) = zcondb
              WRITE(noutlu,*) ' Cond. heat flux at the bottom:',  &
                zcondb_all(jk,jp)    
              zderiv_all(jk,jp) = zderiv*( ztsfa-ztsf0 )
              WRITE(noutlu,*) &
                      ' Additional heat flux at the top dQ/dT*(Tnew-Told):',  &
                 zderiv_all(jk,jp)
              zdh_all(jk,jp) = &
                ( rhoice*sum(zdh(1:nilay)*zdzi(nilay:1:-1))+  &
                      tpsit(jk,jp)%hsn*tpsit(jk,jp)%rsn*zdh(nilay+1) ) / dtt
              WRITE(noutlu,*) '  Enthalpy change per time unit:',  &
                zdh_all(jk,jp)
              zqtopmelta_all(jk,jp) = zqtopmelta
              WRITE(noutlu,*) '  Top melting flux:',  &
                    zqtopmelta_all(jk,jp)
              WRITE(noutlu,*) 'Balance =', &
              zdh_all(jk,jp) + zqtopmelta_all(jk,jp) -  &
                (zcondb_all(jk,jp) + zswtra_all(jk,jp) + pnsftop(jk,jp) +  &
                zderiv_all(jk,jp) )
          ENDIF
      ENDIF 
!
!
!
! 5. Update vertical temperature profile
! ======================================
!
!
! .. Control of the results
!
      IF ( llice .OR. llsnw ) THEN
!
! .. Error in heat conduction scheme
          IF ( llredo ) THEN
               CALL gltools_glterr( 'vhdiff (heat conduction scheme)',  &
                 'Error in heat conduction scheme. ' //  &
                 'See gltout for details','WARN' ) 
               llredo=.FALSE.
               ztsfb = ztsf0
               ztsfa = ztsf0
               ztsib(:) = ztsi0(:)
               ztsia(:) = ztsi0(:)
               pcondb(jk,jp) = 0.
               zderiv2(jk,jp) = 0.
               GOTO 20
          ENDIF
!
! .. No convergence: oscillations
          IF ( jit>nit ) THEN 
            IF (lp2) THEN
              WRITE( ymess,  &
                FMT='("Vertical diffusion scheme did not converge &
              & Error at jk=",I2," jp=",I6)' ) jk,jp
              CALL gltools_glterr( 'vhdiff (heat conduction scheme)', ymess, 'WARN' )
            ENDIF
            llconv = .TRUE.
!!! All temperatures are recomputed as the half sum of new and previous ones
!!            ztsfa = 0.5*( ztsfa+ztsfb )
!!            ztsia(:) = 0.5*( ztsia(:)+ztsib(:) )
!!! Recompute bottom heat flux
!!            pcondb(jk,jp) =  &
!!              zkodzi(nilay+1)*( zg1*( zmlf-ztsia(nilay) )+  &
!!                zg2*( zmlf-ztsia(nilay-1) ) )
!!! Recompute top melting flux
!!            IF ( llsnw ) THEN  
!!              llmelt = ( ztsfa>=ztsi_m0(0) )
!!              IF ( llmelt ) zcondt_m = -zkodzi(0)*( ztsia(0) - ztsfa )
!!            ELSE
!!              llmelt = ( ztsfa>=ztsi_m0(1) )
!!              IF ( llmelt ) zcondt_m = -zkodzi(1)*( ztsia(1) - ztsfa )
!!            ENDIF
!!            lltempcond = ( llmelt .AND. ABS(znsftop0-zcondt_m)>epsil1 )
!!! Recompute top flux derivative zderiv2
!!            IF ( lltempcond ) THEN
!!              zderiv2(jk,jp) = 0.
!!            ELSE
!!              zderiv2(jk,jp) = pderiv(jk,jp)
!!            ENDIF
!!! Recompute the part of the top non-solar flux is devoted to melting
!!            zqtopmelta = znsftop0-zcondt_m
          ENDIF
!
! .. Detect unrealistic temperatures 
          IF ( ztsfa<=-51. .AND. .NOT.llredo ) THEN
             IF(lwg) WRITE(noutlu,*) 'Error at ',jp,  &
               ' ztsfa(',jk,') =',ztsfa
             llredo = .TRUE.
             GOTO 10
          ENDIF
          IF (zmlf<=-10. .and. .NOT.llredo) THEN
             IF(lwg) WRITE(noutlu,*) 'Error at ',jp,' zmlf =',zmlf
             llredo = .TRUE.
             GOTO 10
          ENDIF
      ENDIF
!   
20    CONTINUE
!
      IF ( .NOT.llice .AND. .NOT.llsnw ) THEN
          zvtpa(:) = zmlf
          ztsfa = zmlf
          pcondb(jk,jp) = 0.
          IF (lp4)  &
            WRITE(noutlu,*) 'no ice / snow: convergence at jk,jp=',jk,jp
          icv(jk,jp) = -1    ! Will stop the loop
      ENDIF
!
      IF ( llice ) THEN
!          WHERE( ztsia(1:nilay)>ztsi_m0(1:nilay) )
!            zdhmelta(2:nilay+1) = dtt/zetai(1:nilay) *  &
!              (ztsia(1:nilay)-ztsi_m0(1:nilay))
!            ztsia(1:nilay) = ztsi_m0(1:nilay)
!          ENDWHERE
          IF ( llsnw ) THEN 
!              IF ( ztsia(0)>ztsi_m0(0) ) THEN
!                zdhmelta(1) = dtt/zetai(0) * ( ztsia(0)-ztsi_m0(0) )
!                ztsia(0) = ztsi_m0(0)
!              ENDIF
              zdhmelta(1) = zdhmelta(1) + dtt*zqtopmelta
              ztsfa = AMIN1( ztsfa,ztsi_m0(0) )
            ELSE
              zdhmelta(2) = zdhmelta(2) + dtt*zqtopmelta
              ztsfa = AMIN1( ztsfa,ztsi_m0(1) )
          ENDIF
          IF (lp5) THEN
              WRITE(noutlu,*) 'ZDHMELTA (1) llsnow = true = ', zdhmelta 
              WRITE(noutlu,*) 'ZTSFA (1)    llsnow = true = ', ztsfa
              WRITE(noutlu,*) 'ZTSFB (1)    llsnow = true = ', ztsfb
          ENDIF  
! 
! .. If the scheme has converged: prepare glt_output (downward / upward convention)
          IF ( jit==1 ) THEN
              zcvb = 0.
            ELSE
              zcvb = zcva
          ENDIF
          zcva = SUM( (ztsia(1:nilay)-ztsib(1:nilay))*sf3t )
          llconv = ( llconv .OR. ABS(zcva)<0.001 )
! This 0.01 threshold should be a parameter of the model
          IF ( llconv ) THEN
              IF ( jit>nit ) THEN
                icv(jk,jp) = 0  ! Cases with sea ice/snow that did not converge
              ELSE
                icv(jk,jp) = 1  ! Cases with sea ice/snow that converged
              ENDIF
              IF (lp4) THEN
                  WRITE(noutlu,*) 'Scheme has converged at jk,jp=',jk,jp
                  WRITE(noutlu,*) '  (iteration #',jit,')'
                  WRITE(noutlu,*) 'Z DT                         =',zcva
              ENDIF
! Updated ice temperature
              DO jl=1,nilay
                zvtpa(jl) = ztsia(nilay+1-jl)
              END DO 
! Updated snow temperature
              IF ( llsnw ) THEN 
                  zvtpa(nilay+1) = ztsia(0)
                ELSE
                  zvtpa(nilay+1) = ztsia(1)
              ENDIF
! Compute in excess energy fluxes due to heat diffusion only
              DO jl=1,nl
                pdhmelt(jl,jk,jp) = pdhmelt(jl,jk,jp) + zdhmelta(nl+1-jl)
              END DO 
!write(noutlu,*)'dhmelt(4)=',sum(sum(pdhmelt(:,:,1),dim=1)*pfsi(:,1))/dtt
!
! .. Compute the trend on surface temperature & vertical temperature profile
! (non-solar only)
              zvtptrd(:,jk,jp)=zvtpa(:)-zvtp(:,jk,jp)
              ztsftrd(jk,jp)=ztsfa-ztsf0
!
! .. Compute temperature at snow-ice interface (for AR5 diagnostics)
! (already initialized at 1.e20, where there is no sea ice at the beginning 
! of this routine)
              IF ( llice ) THEN
                IF ( llsnw ) THEN
                  zhs = tpsit(jk,jp)%hsn
                  ztint(jk,jp) =  &
                    ( zvtpa(nilay)*zrkice(1)*zhs +  &
                      zvtpa(nilay+1)*zrks*zdzi(1) )  /  &
                    ( zrks*zdzi(1) + zrkice(1)*zhs )
                  ELSE
                    ztint(jk,jp) = zvtpa(nilay+1)
                ENDIF 
              ENDIF
!
! .. If the scheme has not converged: prepare next iteration
            ELSE
              IF (lp4) THEN 
                  WRITE(noutlu,*) 'Scheme has not converged yet at jk,jp=',jk,jp
                  WRITE(noutlu,*) '  (iteration #',jit,')'
                  WRITE(noutlu,*) 'Z DT                             =',  &
                    zcva
              ENDIF
              IF (lp5) THEN
                  WRITE(noutlu,*)  &
                    'Previous temperature profile (top -> bottom )=', ztsi0
                  WRITE(noutlu,*)  &
                    'New temperature profile (top -> bottom )     =', ztsia
              ENDIF
! 
! .. Update previous time step data for new iteration
              IF ( zcva*zcvb < 0. ) THEN  
                  ztsfa = 0.5*( ztsfa+ztsfb )
                  ztsia(:) = 0.5*( ztsia(:)+ztsib(:) )
              ENDIF
              ztsfb = ztsfa
              ztsib(:) = ztsia(:)
          ENDIF
      ENDIF
!
    END DO       ! End of convergence boolean condition
!
    END DO       ! End of loop on categories
!
  END DO         ! End of loop on grid cells
!
! .. Change sign of pcondb
  IF (lwg) THEN
    ztot_ice_snow= np*nt+SUM(icv,MASK=icv<0)
!    WRITE(noutlu,*)
!    WRITE(noutlu,*) '                         ** WARNING **'
!    WRITE(noutlu,*)  &
!      '  Total number of cases with ice/snow            : ',  &
!      ztot_ice_snow
!    WRITE(noutlu,*)  &
!      '  Convergence failed on # of points              : ',  &
!      np*nt-SUM(ABS(icv))
!    IF ( ztot_ice_snow > 0) THEN 
!       zfailures=100.*FLOAT(np*nt-SUM(ABS(icv)))/ztot_ice_snow
!    ELSE
!       zfailures=0.
!    ENDIF
!    WRITE(noutlu,FMT='(A,F6.2)')  &
!            '   % of failure                                  : ',  &
!            zfailures
!    WRITE(noutlu,*)
  ENDIF
!
  pcondb(:,:) = -pcondb(:,:)
!
! .. Update vertical temperature profile and surface temperature
!
  tpsit(:,:)%tsf = tpsit(:,:)%tsf+ztsftrd(:,:)
!
! .. Update sea ice and snow gltools_enthalpy (and save gltools_enthalpy after heat diffusion)
!
!print*,'temp avant=',zvtp(:,4,1)
!print*
!print*,'trend     =',zvtptrd(:,4,1)
!print*
!print*,'temp apres=',zvtp(:,4,1)+zvtptrd(:,4,1)
!  pent(:,:,:) = glt_enthalpy3d( zvtp(:,:,:)+zvtptrd(:,:,:),pvsp=pvsp(:,:,:) )
!  zentns(:,:,:) = pent(:,:,:)
  pent(:,:,:) = pent(:,:,:) + zdent(:,:,:)
!print*,'correct enth. change=',zdh
!print*,'from vhdiff: d(enth)=',zentns(:,4,1)-zento(:,4,1)
!
! .. Take solar radiation into account (direct impact on gltools_enthalpy, doing that
! on temperature leads to non-conservation)
!
! tzsil%ent = pent
!call glt_aventh(tpsit,tzsil,zei1,zes1)
!print*,'delta enth (W) in vhdiff (after adding zdent)=', &
!  sum(zei1+zes1-zei2-zes2)/dtt
!
! Sea ice part
  DO jl=1,nilay
    WHERE ( tpsit(:,:)%fsi>epsil1 .AND. tpsit(:,:)%hsi>epsil1 )
      pent(jl,:,:) = pent(jl,:,:) +  &
        dtt * (1.-xswhdfr) * pswtra(jl,:,:) /  &
        ( rhoice*sf3tinv(jl)*tpsit(:,:)%hsi )
    ENDWHERE
  END DO
!
! Snow part
  WHERE ( tpsit(:,:)%fsi>epsil1 .AND. tpsit(:,:)%hsn>epsil1 )
    pent(nilay+1,:,:) = pent(nilay+1,:,:) +  &
      dtt * (1.-xswhdfr) * pswtra(nilay+1,:,:) /  &
      ( tpsit(:,:)%rsn*tpsit(:,:)%hsn )
  ENDWHERE
!
! .. Add up excess energy due to solar radiation (in J.m-2)
!
zdhmelt1 = sum(pdhmelt,dim=1)/dtt
! Sea ice part
  WHERE( pent(nilay,:,:)>cpsw*ztsi_m(nilay,:,:) )
    gsmelt(:,:) = .TRUE.
  ENDWHERE
  DO jl=1,nilay
    WHERE ( pent(jl,:,:)>cpsw*ztsi_m(jl,:,:) )
      pdhmelt(jl,:,:) = pdhmelt(jl,:,:) +  &
        rhoice*tpsit(:,:)%hsi*sf3tinv(jl)*( pent(jl,:,:)-cpsw*ztsi_m(jl,:,:) )
      pent(jl,:,:) = cpsw*ztsi_m(jl,:,:)
    ENDWHERE
  END DO
!write(noutlu,*)'dhmelt(5)=',sum(sum(pdhmelt(:,:,1),dim=1)*pfsi(:,1))/dtt
zdhmelt2 = sum(pdhmelt,dim=1)/dtt
!
! Snow part
  WHERE ( pent(nilay+1,:,:)>-xmhofusn0 )
    gsmelt(:,:) = .TRUE.
    pdhmelt(nilay+1,:,:) = pdhmelt(nilay+1,:,:) +  &
      tpsit(:,:)%rsn*tpsit(:,:)%hsn*( pent(nilay+1,:,:)+xmhofusn0 )
    pent(nilay+1,:,:) = -xmhofusn0
  ENDWHERE
zdhmelt3 = sum(pdhmelt,dim=1)/dtt
!write(noutlu,*)'dhmelt(6)=',sum(sum(pdhmelt(:,:,1),dim=1)*pfsi(:,1))/dtt
!
! In some cases, due to a negative qtopmelt, pdhmelt can be negative
! at nilay level (without snow) or nilay+1 level (with snow)
  WHERE( pdhmelt(nilay,:,:)<0. )
    pcondb(:,:) = pcondb(:,:) + pdhmelt(nilay,:,:)/dtt
    pdhmelt(nilay,:,:) = 0.
  ENDWHERE
  WHERE( pdhmelt(nilay+1,:,:)<0. )
    pcondb(:,:) = pcondb(:,:) + pdhmelt(nilay+1,:,:)/dtt
    pdhmelt(nilay+1,:,:) = 0.
  ENDWHERE
zdhmelt4 = sum(pdhmelt,dim=1)/dtt
!
! Save top melt flux (only the one affecting sea ice - for diagnostic)
!
  pqtopmelt(:,:) = -zderiv2(:,:)*ztsftrd(:,:)
  WHERE( .NOT. gsmelt(:,:) .OR. tpsit(:,:)%hsn>epsil1 .OR. pqtopmelt(:,:)<0. )
    pqtopmelt(:,:) = 0.
  ENDWHERE
!
! Final update of the bottom conductive heat flux
!
  pcondb(:,:) = pcondb(:,:) - zderiv2(:,:)*ztsftrd(:,:)
!
! .. Save temperature at snow / ice interface
! Weights for final computation of the average must be incremented 
! here, not where diagnostics are written.
! The reason for this is that tpsit%fsi in the present routine would
! not be consistent with total sea ice fraction in the diagnostics.
!  
! .. Total sea ice concentration
!
  zfsit(:) = SUM( tpsit(:,:)%fsi, MASK=(omask), DIM=1 )
  tpdia(:)%tin =  &
    SUM( (ztint(:,:)+t0deg)*tpsit(:,:)%fsi, MASK=(omask), DIM=1 )
  tpdia(:)%tiw = tpdia(:)%tiw + zfsit(:)
!
!
!
! 6. Final checks
! ================
!
! Compute the different terms of the energy balance (part of the physics)
  zdent(:,:,:) = zdent(:,:,:)+zdenti(:,:,:)  ! Total gltools_enthalpy variation
  z1 = pnsftop(:,:)
  z2 = - pcondb(:,:) 
  z3 = tpsit(:,:)%rsn*tpsit(:,:)%hsn*zdent(nilay+1,:,:)/dtt
  z4 = SUM( pswtra(:,:,:),DIM=1 )
  z5 = 0.
  DO jl = 1,nilay
    z5(:,:) = z5(:,:) +  &
  ( rhoice*sf3tinv(jl)*zdent(jl,:,:)*tpsit(:,:)%hsi/dtt )
  END DO
  z6 = SUM( pdhmelt(:,:,:),DIM=1 )/dtt
  zwork = tpsit%fsi * ( z1 + z4 + z2 - z3 - z5 - z6 )
!
! .. Finally, we collect the small imbalance due to rare cases of non
! convergence of the vertical heat diffusion scheme, and put it in
! the bottom conduction flux.
!
  icount(:,:) = 0
  WHERE( ABS(zwork)>0.1 )
    icount = 1
  ENDWHERE
  IF (lp1) THEN
    WRITE(noutlu,*)
    WRITE(noutlu,*) '                         ** WARNING **'
    WRITE(noutlu,*)  &
      '  The energy balance is not closed on # of points : ',  &
      SUM(icount)
    WRITE(noutlu,*)  &
      '  Total number of points                                    : ',  &
      np*nt
    WRITE(noutlu,FMT='(A,F6.2)')  &
      '% of failure:                                               : ',  &
      100.*FLOAT(SUM(icount))/FLOAT(np*nt)
    WRITE(noutlu,*)
  ENDIF
!
  pcondb(:,:) = pcondb(:,:)+zwork(:,:)
  z2 = - pcondb(:,:)
!      
  IF (lp1) THEN 
!
      ! print*,'condt=',glt_avg_r(tpdom,sum(tpsit%fsi*z1,dim=1),0)
      ! print*,'condb=',glt_avg_r(tpdom,sum(tpsit%fsi*z2,dim=1),0)
      ! print*,'dhsnow=',glt_avg_r(tpdom,sum(tpsit%fsi*z3,dim=1),0)
      ! print*,'solar=',glt_avg_r(tpdom,sum(tpsit%fsi*z4,dim=1),0)
      ! print*,'dhice=',glt_avg_r(tpdom,sum(tpsit%fsi*z5,dim=1),0)
      ! print*,'dhmelt=',glt_avg_r(tpdom,sum(tpsit%fsi*z6,dim=1),0)
      ! print*,'*****'
      ! print*,'melt0=',glt_avg_r(tpdom,sum(tpsit%fsi*zdhmelt0,dim=1),0)
      ! print*,'melt1=',glt_avg_r(tpdom,sum(tpsit%fsi*zdhmelt1,dim=1),0)
      ! print*,'melt2=',glt_avg_r(tpdom,sum(tpsit%fsi*zdhmelt2,dim=1),0)
      ! print*,'melt3=',glt_avg_r(tpdom,sum(tpsit%fsi*zdhmelt3,dim=1),0)
      ! print*,'melt4=',glt_avg_r(tpdom,sum(tpsit%fsi*zdhmelt4,dim=1),0)
!
      WRITE(noutlu,*)
      WRITE(noutlu,*)  &
        'At the end of VHDIFF (vert. heat diffusion scheme), W/m2 of ice'
!
! .. Print top flux integral
!
      zinsftop = glt_avg_r( tpdom, SUM( tpsit%fsi*z1, DIM=1 ),0 )
      WRITE(noutlu,FMT='(A,F8.2)')  &
        '    Top non-solar flux integral                 :', zinsftop
!
! .. Print input solar energy
!
      ziswa = glt_avg_r( tpdom, SUM( tpsit%fsi*z4, DIM=1 ),0 )
      WRITE(noutlu,FMT='(A,F8.2)')  &
        '    Input solar energy                          :', ziswa
!
! .. Print bottom flux integral
!
      zicondb = glt_avg_r( tpdom, SUM( tpsit%fsi*z2, DIM=1 ),0 )
      WRITE(noutlu,FMT='(A,F8.2)')  &
        '    Bottom flux integral:', zicondb 
! .. Total incoming energy
!
      WRITE(noutlu,FMT='(A,F10.4)')  &
        '      ==> Total incoming energy                 :',  &
       zinsftop+ziswa+zicondb
!
      WRITE(noutlu,*)
!
! .. Print ice enthalpy change integral (non-solar + solar)
!
      zidhi = glt_avg_r( tpdom, SUM( tpsit%fsi*z5, DIM=1 ),0 )
      WRITE(noutlu,FMT='(A,F8.2)')  &
        '    Ice enthalpy change due to heat conduction  :', zidhi
!
! .. Print snow enthalpy change integral (non-solar + solar)
!
      zidhs = glt_avg_r( tpdom, SUM( tpsit%fsi*z3, DIM=1 ),0 )
      WRITE(noutlu,FMT='(A,F8.2)')  &
        '    Snow enthalpy change due to heat conduction :', zidhs
!
! .. Print overwhelming energy due to melting
!
      zdhmelt = glt_avg_r( tpdom, SUM( tpsit%fsi*z6, DIM=1 ),0 )
      WRITE(noutlu,FMT='(A,F8.2)')  &
        '    Energy devoted to melting                   :', zdhmelt
!
! .. Print snow + ice enthalpy change integral
!
      WRITE(noutlu,FMT='(A,F10.4)')  &
        '      ==> Total energy change for sea ice / snow:',  &
        zidhi + zidhs + zdhmelt
!
      WRITE(noutlu,*)
!
! .. Energy variation of the whole system
!
!           =    z1    +  z4   +   z2    -  z5   -  z3   -   z6 
      zinrg = zinsftop + ziswa + zicondb - zidhi - zidhs - zdhmelt
!
      WRITE(noutlu,FMT='(A,F10.4)')  &
        '    ==> Energy variation of the whole system    :', zinrg
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
  ENDIF
!
  IF (lp4) THEN
      WRITE(noutlu,*) ' Non-solar heat flux at the top:',  &
        glt_avg_r( tpdom,SUM( pnsftop(:,:)*tpsit%fsi, DIM=1 ),0 )
      WRITE(noutlu,*) ' Absorbed solar heat flux:',  &
        glt_avg_r( tpdom,SUM( zswtra_all(:,:)*tpsit%fsi, DIM=1 ),0 )
      WRITE(noutlu,*) ' Cond. heat flux at the bottom:',  &
        glt_avg_r( tpdom,SUM( zcondb_all(:,:)*tpsit%fsi, DIM=1 ),0 )
      WRITE(noutlu,*) ' Additional heat flux at the top dQ/dT*(Tnew-Told):',  &
        glt_avg_r( tpdom,SUM( zderiv_all(:,:)*tpsit%fsi, DIM=1 ),0 )
      WRITE(noutlu,*) ' Sum of the previous two form final condb:',  &
        glt_avg_r( tpdom,SUM( (zcondb_all(:,:)+zderiv_all(:,:) )*tpsit%fsi, DIM=1 ),0 )
      WRITE(noutlu,*) ' Enthalpy change per time unit:',  &
        glt_avg_r( tpdom,SUM( zdh_all(:,:)*tpsit%fsi, DIM=1 ),0 )
      WRITE(noutlu,*) ' Top melting flux:',  &
        glt_avg_r( tpdom,SUM( zqtopmelta_all(:,:)*tpsit%fsi, DIM=1 ),0 )
      WRITE(noutlu,*) 'Imbalance =', &
        glt_avg_r( tpdom,  &
          SUM( tpsit%fsi*( zdh_all(:,:)+zqtopmelta_all(:,:) -  &
            ( zcondb_all(:,:)+zswtra_all(:,:)+pnsftop(:,:)+zderiv_all(:,:) ) ), &
            DIM=1 ),0 )
  ENDIF
!
! .. Note that deallocations must be done in reverse order on some specific
! platforms (wrt allocations)
!
  DEALLOCATE( zkodzi )
  DEALLOCATE( ztsi_m0 )
  DEALLOCATE( ztsi0 )
  DEALLOCATE( ztsib )
  DEALLOCATE( zrhocpsi )
  DEALLOCATE( zetaikp1 )
  DEALLOCATE( zetaik )
  DEALLOCATE( zinvetai )
  DEALLOCATE( zetai )
!
  END SUBROUTINE glt_vhdiff_r
