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
! ======================= MODULE modi_glt_thermo ========================
! =======================================================================
!
!
! Goal:
! -----
!   Computation of the thermodynamic forcing over open water, sea ice
! and snow-covered sea ice. Version with thickness within each box
! which is constant, compensated with variable fractional ice
! covers.
!   This part of the code runs on a reduced grid, the work grid only includes 
! ocean points where SST is less than a certain threshold or where there
! is sea ice. We consider that:
!       - if there is a lot of sea ice, there is a possibility for warm 
! water advection in this area, hence any place where there is sea ice 
! should be treated to avoid losing sea ice
!       - a reasonable temperature threshold is 1°, to include areas 
! where salinity is zero (freezing point = 0°C)
!  
! Created : 2009/06 - Reduced grid option introduced - full grid still 
!   available (D. Salas y Melia)
! Modified: 2009/11 - Full grid is no longer treated (D. Salas y Melia)
! Modified: 2010/05 - mpi Multi-processing introduced (S.Senesi)
! Modified: 2011/04 - (Re)-ensure portability on both NEC and PC
! Modified: 2011/05 - Solve issue occurring when less ice points than 
!   processors (S.Senesi)
! Modified: 2011/12 - Introduce new diagnostics (A. Voldoire)
! Modified: 2012/07 (D. Salas y Melia)
!           Input now consists of fields defined on subdomains
! Modified: 2012/11 (D. Salas y Melia)
!           Sea ice damping
! Modified: 2015/07 (D. Salas y Melia)
!           Sea ice damping has been externalized from thermo_r routine
!
! -------------------- BEGIN MODULE modi_glt_thermo -------------------------

!THXS_SFX!MODULE modi_glt_thermo
!THXS_SFX!INTERFACE
!THXS_SFX!
!THXS_SFX!SUBROUTINE glt_thermo  &
!THXS_SFX!  ( tpdom,pustar,tpmxl,tpatm,  &
!THXS_SFX!    tpblkw,tpblki,tpbud,tpdia,tptfl,tpsit,tpsil,tpsit_d )
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  USE modd_glt_param
!THXS_SFX!  TYPE(t_dom), DIMENSION(nx,ny), INTENT(in) ::  &
!THXS_SFX!        tpdom
!THXS_SFX!  REAL, DIMENSION(nx,ny), INTENT(in) ::  &
!THXS_SFX!        pustar
!THXS_SFX!  TYPE(t_mxl), DIMENSION(nx,ny), INTENT(in) ::  &
!THXS_SFX!        tpmxl
!THXS_SFX!  TYPE(t_atm), DIMENSION(nx,ny), INTENT(in) ::  &
!THXS_SFX!        tpatm
!THXS_SFX!  TYPE(t_blk), DIMENSION(nx,ny), INTENT(inout) ::  &
!THXS_SFX!        tpblkw
!THXS_SFX!  TYPE(t_blk), DIMENSION(nt,nx,ny), INTENT(in) ::  &
!THXS_SFX!        tpblki
!THXS_SFX!  TYPE(t_bud), DIMENSION(nx,ny), INTENT(inout) ::  &
!THXS_SFX!        tpbud
!THXS_SFX!  TYPE(t_dia), DIMENSION(nx,ny), INTENT(inout) ::  &
!THXS_SFX!        tpdia
!THXS_SFX!  TYPE(t_tfl), DIMENSION(nx,ny), INTENT(inout) ::  &
!THXS_SFX!        tptfl
!THXS_SFX!  TYPE(t_sit), DIMENSION(nt,nx,ny), INTENT(inout) ::  &
!THXS_SFX!        tpsit
!THXS_SFX!  TYPE(t_vtp), DIMENSION(nl,nt,nx,ny), INTENT(inout) ::  &
!THXS_SFX!        tpsil
!THXS_SFX!  TYPE(t_sit), DIMENSION(ntd,nx,ny), OPTIONAL, INTENT(in) ::  &
!THXS_SFX!        tpsit_d
!THXS_SFX!END SUBROUTINE glt_thermo
!THXS_SFX!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_glt_thermo

! ---------------------- END MODULE modi_glt_thermo -------------------------
!
!
! -----------------------------------------------------------------------
! ----------------------- SUBROUTINE glt_thermo -----------------------------
!
SUBROUTINE glt_thermo  &
  ( tpdom,pustar,tpmxl,tpatm,  &
    tpblkw,tpblki,tpbud,tpdia,tptfl,tpsit,tpsil,tpsit_d )
!
!
! 1. DECLARATIONS
! ===============
!
! 1.1. Module declarations
! ------------------------
!
  USE modd_glt_const_thm
  USE modd_types_glt
  USE modd_glt_param
  USE mode_glt_stats
  USE mode_glt_stats_r
  USE modi_glt_thermo_r
  USE modi_glt_constrain_r
!
  IMPLICIT NONE
!
!
! 1.2. Dummy arguments declarations
! ---------------------------------
!
  TYPE(t_dom), DIMENSION(nx,ny), INTENT(in) ::  &
        tpdom
  REAL, DIMENSION(nx,ny), INTENT(in) ::  &
        pustar
  TYPE(t_mxl), DIMENSION(nx,ny), INTENT(in) ::  &
        tpmxl
  TYPE(t_atm), DIMENSION(nx,ny), INTENT(in) ::  &
        tpatm
  TYPE(t_blk), DIMENSION(nx,ny), INTENT(inout) ::  &
        tpblkw
  TYPE(t_blk), DIMENSION(nt,nx,ny), INTENT(in) ::  &
        tpblki
  TYPE(t_bud), DIMENSION(nx,ny), INTENT(inout) ::  &
        tpbud
  TYPE(t_dia), DIMENSION(nx,ny), INTENT(inout) ::  &
        tpdia
  TYPE(t_tfl), DIMENSION(nx,ny), INTENT(inout) ::  &
        tptfl
  TYPE(t_sit), DIMENSION(nt,nx,ny), INTENT(inout) ::  &
        tpsit
  TYPE(t_vtp), DIMENSION(nl,nt,nx,ny), INTENT(inout) ::  &
        tpsil
  TYPE(t_sit), DIMENSION(ntd,nx,ny), OPTIONAL, INTENT(in) ::  &
        tpsit_d
!
!
! 1.3. Local variables declarations
! ---------------------------------
!
  INTEGER ::  &
        jk,jl
  LOGICAL, DIMENSION(nx,ny) ::  &
        gsel
  INTEGER, DIMENSION(nx,ny) ::  &
        isel
  REAL, DIMENSION(nx,ny) ::  &
        zfsit,zsnflx
  TYPE(t_dia), DIMENSION(nx,ny) ::  &
        tzdia0
  TYPE(t_tfl), DIMENSION(nx,ny) ::  &
        tztfl0
  TYPE(t_sit), DIMENSION(nt,nx,ny) ::  &
        tzsit0
  TYPE(t_vtp), DIMENSION(nl,nt,nx,ny) ::  &
        tzsil0
!  INTEGER ::  &
!        ji,jj
!  INTEGER, DIMENSION(2,nx,ny) ::  &
!        ind
!  INTEGER, DIMENSION(:,:), ALLOCATABLE ::  &
!        ind_r
  TYPE(t_dom), DIMENSION(:), ALLOCATABLE ::  &
        tzdom_r
  REAL, DIMENSION(:), ALLOCATABLE ::  &
        zustar_r
  TYPE(t_mxl), DIMENSION(:), ALLOCATABLE ::  &
        tzmxl_r
  TYPE(t_atm), DIMENSION(:), ALLOCATABLE ::  &
        tzatm_r
  TYPE(t_blk), DIMENSION(:), ALLOCATABLE ::  &
        tzblkw_r
  TYPE(t_blk), DIMENSION(:,:), ALLOCATABLE ::  &
        tzblki_r
  TYPE(t_bud), DIMENSION(:), ALLOCATABLE ::  &
        tzbud_r
  TYPE(t_dia), DIMENSION(:), ALLOCATABLE ::  &
        tzdia_r
  TYPE(t_tfl), DIMENSION(:), ALLOCATABLE ::  &
        tztfl_r
  TYPE(t_sit), DIMENSION(:,:), ALLOCATABLE ::  &
        tzsit_d_r
  TYPE(t_sit), DIMENSION(:,:), ALLOCATABLE ::  &
        tzsit_r
  TYPE(t_vtp), DIMENSION(:,:,:), ALLOCATABLE ::  &
        tzsil_r
!
!
! 1.4. Welcome message
! --------------------
!
  IF (lp1) THEN
    WRITE(noutlu,*) ' '
    WRITE(noutlu,*) ' *** LEVEL 3 - SUBROUTINE THERMO'
    WRITE(noutlu,*) ' '
  ENDIF
!
!
!
! 2. THERMODYNAMICS ON REDUCED GRID
! ==================================
!
! 2.1. Define grid point selection criterion
! -------------------------------------------
!
  zfsit(:,:) = glt_iceconcm(tpdom,tpsit) 
  isel(:,:) = 0
!
! In principe, calculations on tpdom%imk==1 would suffice. But it
! would require bounding in the end (thermodynamics would not be 
! purely 1d...)
!
  WHERE( tpdom(:,:)%tmk==1 .AND.  &
  (tpmxl(:,:)%tml <= 1. .OR. zfsit(:,:)>0.0) )
      isel(:,:) = 1
  ENDWHERE
!
  gsel(:,:) = ( isel(:,:)==1 )
! 
!
! 2.2. Pack all fields and allocate reduced grid arrays
! ------------------------------------------------------
!
! .. Reduced grid dimension
!
  np = SUM(isel)
!
  IF (lp3) THEN
    WRITE(noutlu,*)  &
      '**********************************************************'
    WRITE(noutlu,*) 'REDUCED GRID:'
    WRITE(noutlu,1000) gelato_myrank, np, nx*ny, nx, ny
    WRITE(noutlu,*)  &
      '**********************************************************'
  ENDIF
!
! .. Note that on some platforms, every locally-allocated array needs to be
! deallocated explicitly (this is done at the end of this subroutine).
! More over, the deallocations need be done in reverse order.
!
  IF ( np /= 0 ) THEN
!    ALLOCATE( ind_r(2,np) )
    ALLOCATE( tzdom_r(np) )
    ALLOCATE( zustar_r(np) )
    ALLOCATE( tzmxl_r(np) )
    ALLOCATE( tzatm_r(np) )
    ALLOCATE( tzblkw_r(np) )
    ALLOCATE( tzblki_r(nt,np) )
    ALLOCATE( tzbud_r(np) )
    ALLOCATE( tzdia_r(np) )
    ALLOCATE( tztfl_r(np) )
! Note that the error check on ntd dimension will be done in glt_constrain_r
  IF ( PRESENT(tpsit_d) ) ALLOCATE( tzsit_d_r(ntd,np) )
    ALLOCATE( tzsit_r(nt,np) )
    ALLOCATE( tzsil_r(nl,nt,np) )
!
!
! 2.3. From global to reduced grid arrays
! ----------------------------------------
!
! Ces tableaux devraient passer en variable globale, pour des recherches
! d'indices...
!ind_r(1,:) = PACK( ind(1,:,:),gsel )
!ind_r(2,:) = PACK( ind(2,:,:),gsel )
!if (np>=6832) write(noutlu,*)'index 6832 =',ind_r(1,6832),ind_r(2,6832)
! .. Domain
!
     tzdom_r%tmk = PACK( tpdom%tmk ,gsel )
     tzdom_r%umk = PACK( tpdom%umk ,gsel )
     tzdom_r%vmk = PACK( tpdom%vmk ,gsel )
     tzdom_r%imk = PACK( tpdom%imk ,gsel )
     tzdom_r%indi = PACK( tpdom%indi ,gsel )
     tzdom_r%indj = PACK( tpdom%indj ,gsel )
     tzdom_r%lon = PACK( tpdom%lon ,gsel )
     tzdom_r%lat = PACK( tpdom%lat ,gsel )
     tzdom_r%dxc = PACK( tpdom%dxc ,gsel )
     tzdom_r%dyc = PACK( tpdom%dyc ,gsel )
     tzdom_r%srf = PACK( tpdom%srf ,gsel )
!
! .. Friction velocity
!
     zustar_r = PACK( pustar, gsel )
!
! .. Mixed layer conditions
!
     tzmxl_r%hco = PACK( tpmxl%hco ,gsel )
     tzmxl_r%qoc = PACK( tpmxl%qoc ,gsel )
     tzmxl_r%sml = PACK( tpmxl%sml ,gsel )
     tzmxl_r%ssh = PACK( tpmxl%ssh ,gsel )
     tzmxl_r%tml = PACK( tpmxl%tml ,gsel )
     tzmxl_r%mlf = PACK( tpmxl%mlf ,gsel )
     tzmxl_r%uml = PACK( tpmxl%uml ,gsel )
     tzmxl_r%vml = PACK( tpmxl%vml ,gsel )
     tzmxl_r%qml = PACK( tpmxl%qml ,gsel )
!
! .. Atmospheric conditions
!
     tzatm_r%lip = PACK( tpatm%lip ,gsel )
     tzatm_r%sop = PACK( tpatm%sop ,gsel )
     tzatm_r%ztx = PACK( tpatm%ztx ,gsel )
     tzatm_r%mty = PACK( tpatm%mty ,gsel )
!
! .. Surface fluxes (over ocean)
!     
     tzblkw_r%swa = PACK( tpblkw%swa ,gsel )
     tzblkw_r%nsf = PACK( tpblkw%nsf ,gsel )
     tzblkw_r%dfl = PACK( tpblkw%dfl ,gsel )
     tzblkw_r%eva = PACK( tpblkw%eva ,gsel )
!
! .. Surface fluxes (over ice)
!
    DO jk=1,nt
        tzblki_r(jk,:)%swa = PACK( tpblki(jk,:,:)%swa ,gsel )
        tzblki_r(jk,:)%nsf = PACK( tpblki(jk,:,:)%nsf ,gsel )
        tzblki_r(jk,:)%dfl = PACK( tpblki(jk,:,:)%dfl ,gsel )
        tzblki_r(jk,:)%eva = PACK( tpblki(jk,:,:)%eva ,gsel )
    END DO
!
! .. Energy budget
!
     tzbud_r%eni = PACK( tpbud%eni ,gsel )
     tzbud_r%enn = PACK( tpbud%enn ,gsel )
     tzbud_r%bii = PACK( tpbud%bii ,gsel )
     tzbud_r%nii = PACK( tpbud%nii ,gsel )
     tzbud_r%nli = PACK( tpbud%nli ,gsel )
     tzbud_r%hii = PACK( tpbud%hii ,gsel )
     tzbud_r%hli = PACK( tpbud%hli ,gsel )
     tzbud_r%hio = PACK( tpbud%hio ,gsel )
     tzbud_r%hlo = PACK( tpbud%hlo ,gsel )
     tzbud_r%wii = PACK( tpbud%wii ,gsel )
     tzbud_r%wli = PACK( tpbud%wli ,gsel )
     tzbud_r%fwi = PACK( tpbud%fwi ,gsel )
     tzbud_r%fwn = PACK( tpbud%fwn ,gsel )
     tzbud_r%isi = PACK( tpbud%isi ,gsel )
     tzbud_r%isn = PACK( tpbud%isn ,gsel )
!
! .. Diagnostics
!
     tzdia_r%uvl = PACK( tpdia%uvl ,gsel )
     tzdia_r%vvl = PACK( tpdia%vvl ,gsel )
     tzdia_r%aiw = PACK( tpdia%aiw ,gsel )
     tzdia_r%asi = PACK( tpdia%asi ,gsel )
     tzdia_r%amp = PACK( tpdia%amp ,gsel )
     tzdia_r%asn = PACK( tpdia%asn ,gsel )
     tzdia_r%cgl = PACK( tpdia%cgl ,gsel )
     tzdia_r%dsa = PACK( tpdia%dsa ,gsel )
     tzdia_r%dsn = PACK( tpdia%dsn ,gsel )
     tzdia_r%dsi = PACK( tpdia%dsi ,gsel )
     tzdia_r%dwi = PACK( tpdia%dwi ,gsel )
     tzdia_r%lip = PACK( tpdia%lip ,gsel )
     tzdia_r%lsi = PACK( tpdia%lsi ,gsel )
     tzdia_r%mrb = PACK( tpdia%mrb ,gsel )
     tzdia_r%mrt = PACK( tpdia%mrt ,gsel )
     tzdia_r%mrl = PACK( tpdia%mrl ,gsel )
     tzdia_r%sie = PACK( tpdia%sie ,gsel )
     tzdia_r%sne = PACK( tpdia%sne ,gsel )
     tzdia_r%sni = PACK( tpdia%sni ,gsel )
     tzdia_r%snm = PACK( tpdia%snm ,gsel )
     tzdia_r%snml = PACK( tpdia%snml ,gsel )
     tzdia_r%sop = PACK( tpdia%sop ,gsel )
     tzdia_r%the = PACK( tpdia%the ,gsel )
     tzdia_r%tin = PACK( tpdia%tin ,gsel )
     tzdia_r%tiw = PACK( tpdia%tiw ,gsel )
     tzdia_r%qoi = PACK( tpdia%qoi ,gsel )
     tzdia_r%xtr = PACK( tpdia%xtr ,gsel )
     tzdia_r%ytr = PACK( tpdia%ytr ,gsel )
     tzdia_r%sp1 = PACK( tpdia%sp1 ,gsel )
     tzdia_r%sp2 = PACK( tpdia%sp2 ,gsel )
     tzdia_r%sui = PACK( tpdia%sui ,gsel )
     tzdia_r%sut = PACK( tpdia%sut ,gsel )
     tzdia_r%sus = PACK( tpdia%sus ,gsel )
     tzdia_r%suw = PACK( tpdia%suw ,gsel )
     tzdia_r%s_pr = PACK( tpdia%s_pr ,gsel )
     tzdia_r%s_prsn = PACK( tpdia%s_prsn ,gsel )
     tzdia_r%o_pr = PACK( tpdia%o_pr ,gsel )
     tzdia_r%o_prsn = PACK( tpdia%o_prsn ,gsel )
     tzdia_r%subcio = PACK( tpdia%subcio ,gsel )
     tzdia_r%snicio = PACK( tpdia%snicio ,gsel )
     tzdia_r%hsicio = PACK( tpdia%hsicio ,gsel )
     tzdia_r%lmlcio = PACK( tpdia%lmlcio ,gsel )
     tzdia_r%salcio = PACK( tpdia%salcio ,gsel )
     tzdia_r%l_pr = PACK( tpdia%l_pr ,gsel )
     tzdia_r%l_prsn = PACK( tpdia%l_prsn ,gsel )
     tzdia_r%sul = PACK( tpdia%sul ,gsel )
! These ones should not be packed: unused in thermodynamics, and contain
! data outside the ice domain (the data will be lost if packing):
!   - tzdia_r%ifw
!   - tzdia_r%sic
!   - tzdia_r%sit
!   - tzdia_r%atx,%aty
!   - tzdia_r%otx,%oty
!
! .. Fluxes transmitted to the mixed layer
!
     tztfl_r%lio = PACK( tptfl%lio ,gsel )
     tztfl_r%llo = PACK( tptfl%llo ,gsel )
     tztfl_r%tio = PACK( tptfl%tio ,gsel )
     tztfl_r%tlo = PACK( tptfl%tlo ,gsel )
     tztfl_r%sio = PACK( tptfl%sio ,gsel )
     tztfl_r%cio = PACK( tptfl%cio ,gsel )
     tztfl_r%wio = PACK( tptfl%wio ,gsel )
     tztfl_r%wlo = PACK( tptfl%wlo ,gsel )
     tztfl_r%xio = PACK( tptfl%xio ,gsel )
     tztfl_r%yio = PACK( tptfl%yio ,gsel )
!
! .. Damping / restoring data
!
  IF ( PRESENT(tpsit_d) ) THEN
    DO jk=1,ntd
      tzsit_d_r(jk,:)%esi = PACK( tpsit_d(jk,:,:)%esi, gsel )
      tzsit_d_r(jk,:)%asn = PACK( tpsit_d(jk,:,:)%asn, gsel )
      tzsit_d_r(jk,:)%fsi = PACK( tpsit_d(jk,:,:)%fsi, gsel )
      tzsit_d_r(jk,:)%hsi = PACK( tpsit_d(jk,:,:)%hsi, gsel )
      tzsit_d_r(jk,:)%hsn = PACK( tpsit_d(jk,:,:)%hsn, gsel )
      tzsit_d_r(jk,:)%rsn = PACK( tpsit_d(jk,:,:)%rsn, gsel )
      tzsit_d_r(jk,:)%tsf = PACK( tpsit_d(jk,:,:)%tsf, gsel )
      tzsit_d_r(jk,:)%ssi = PACK( tpsit_d(jk,:,:)%ssi, gsel )
      tzsit_d_r(jk,:)%age = PACK( tpsit_d(jk,:,:)%age, gsel )
      tzsit_d_r(jk,:)%vmp = PACK( tpsit_d(jk,:,:)%vmp ,gsel )
    END DO
  ENDIF
!
! .. Sea ice 2D 
!
    DO jk=1,nt
        tzsit_r(jk,:)%esi = PACK( tpsit(jk,:,:)%esi ,gsel )
        tzsit_r(jk,:)%asn = PACK( tpsit(jk,:,:)%asn ,gsel )
        tzsit_r(jk,:)%fsi = PACK( tpsit(jk,:,:)%fsi ,gsel )
        tzsit_r(jk,:)%hsi = PACK( tpsit(jk,:,:)%hsi ,gsel )
        tzsit_r(jk,:)%hsn = PACK( tpsit(jk,:,:)%hsn ,gsel )
        tzsit_r(jk,:)%rsn = PACK( tpsit(jk,:,:)%rsn ,gsel )
        tzsit_r(jk,:)%tsf = PACK( tpsit(jk,:,:)%tsf ,gsel )
        tzsit_r(jk,:)%ssi = PACK( tpsit(jk,:,:)%ssi ,gsel )
        tzsit_r(jk,:)%age = PACK( tpsit(jk,:,:)%age ,gsel )
        tzsit_r(jk,:)%vmp = PACK( tpsit(jk,:,:)%vmp ,gsel )
    END DO
!
! .. Sea ice 3D
!
    DO jl=1,nl
      DO jk=1,nt
          tzsil_r(jl,jk,:)%ent = PACK( tpsil(jl,jk,:,:)%ent ,gsel )
      END DO
    END DO 
! 
!
! 2.4. Update domain surface
! ---------------------------
!
    xdomsrf_r = SUM( tzdom_r(:)%dxc*tzdom_r(:)%dyc )
!
!
! 2.5. Thermodynamics on the reduced grid
! ----------------------------------------
!
    IF ( nthermo==1 ) THEN
      CALL glt_thermo_r  &
        ( tzdom_r,zustar_r,tzmxl_r,tzatm_r,tzblkw_r,tzblki_r,tzbud_r,tzdia_r,  &
        tztfl_r,tzsit_r,tzsil_r )
    ENDIF
!
!
! 2.6. Apply the constraint
! --------------------------
!
    IF ( ntd==1 ) THEN
      CALL glt_constrain_r( tzdom_r,tzmxl_r,tzsit_r,tzsil_r,tzdia_r,tzsit_d_r )
    ENDIF
!
!
! 2.7. From reduced to global grid arrays
! ----------------------------------------
!
! .. Energy budget 
! Nothing to do : this is a non-cumulative diagnostic (initialized at 
! every glt_updtfl CALL)
!
! .. Diagnostics
! Note that only INTENT(inout) arguments must be unpacked
!
    tzdia0(:,:)%uvl = 0.
    tzdia0(:,:)%vvl = 0.
    tzdia0(:,:)%aiw = 0.
    tzdia0(:,:)%asi = 0.
    tzdia0(:,:)%amp = 0.
    tzdia0(:,:)%asn = 0.
    tzdia0(:,:)%cgl = 0.
    tzdia0(:,:)%dsa = 0.
    tzdia0(:,:)%dsn = 0.
    tzdia0(:,:)%dsi = 0.
    tzdia0(:,:)%dci = 0.
    tzdia0(:,:)%cst = 0.
    tzdia0(:,:)%dwi = 0.
    tzdia0(:,:)%lip = 0.
    tzdia0(:,:)%lsi = 0.
    tzdia0(:,:)%mrb = 0.
    tzdia0(:,:)%mrt = 0.
    tzdia0(:,:)%mrl = 0.
    tzdia0(:,:)%sie = 0.
    tzdia0(:,:)%sne = 0.
    tzdia0(:,:)%sni = 0.
    tzdia0(:,:)%snm = 0.
    tzdia0(:,:)%snml = 0.
    tzdia0(:,:)%sop = 0.
    tzdia0(:,:)%the = 0.
    tzdia0(:,:)%tin = 0.
    tzdia0(:,:)%tiw = 0.
    tzdia0(:,:)%qoi = 0.
    tzdia0(:,:)%xtr = 0.
    tzdia0(:,:)%ytr = 0.
    tzdia0(:,:)%sp1 = 0.
    tzdia0(:,:)%sp2 = 0.
    tzdia0(:,:)%sui = 0.
    tzdia0(:,:)%sut = 0.
    tzdia0(:,:)%sus = 0.
    tzdia0(:,:)%suw = 0.
    tzdia0(:,:)%sul = 0.
    tzdia0(:,:)%s_pr = 0.
    tzdia0(:,:)%s_prsn = 0.
    tzdia0(:,:)%o_pr = 0.
    tzdia0(:,:)%o_prsn = 0.
    tzdia0(:,:)%l_pr = 0.
    tzdia0(:,:)%l_prsn = 0.
    tzdia0(:,:)%subcio = 0.
    tzdia0(:,:)%snicio = 0.
    tzdia0(:,:)%hsicio = 0.
    tzdia0(:,:)%lmlcio = 0.
    tzdia0(:,:)%salcio = 0.
!
    tpdia%uvl = UNPACK( tzdia_r%uvl,gsel,tzdia0%uvl )
    tpdia%vvl = UNPACK( tzdia_r%vvl,gsel,tzdia0%vvl )
    tpdia%aiw = UNPACK( tzdia_r%aiw,gsel,tzdia0%aiw )
    tpdia%asi = UNPACK( tzdia_r%asi,gsel,tzdia0%asi )
    tpdia%amp = UNPACK( tzdia_r%amp,gsel,tzdia0%amp )
    tpdia%asn = UNPACK( tzdia_r%asn,gsel,tzdia0%asn )
    tpdia%cgl = UNPACK( tzdia_r%cgl,gsel,tzdia0%cgl )
    tpdia%dsa = UNPACK( tzdia_r%dsa,gsel,tzdia0%dsa )
    tpdia%dsn = UNPACK( tzdia_r%dsn,gsel,tzdia0%dsn )
    tpdia%dsi = UNPACK( tzdia_r%dsi,gsel,tzdia0%dsi )
    tpdia%dci = UNPACK( tzdia_r%dci,gsel,tzdia0%dci )
    tpdia%cst = UNPACK( tzdia_r%cst,gsel,tzdia0%cst )
    tpdia%dwi = UNPACK( tzdia_r%dwi,gsel,tzdia0%dwi )
    tpdia%lip = UNPACK( tzdia_r%lip,gsel,tzdia0%lip )
    tpdia%lsi = UNPACK( tzdia_r%lsi,gsel,tzdia0%lsi )
    tpdia%mrb = UNPACK( tzdia_r%mrb,gsel,tzdia0%mrb )
    tpdia%mrt = UNPACK( tzdia_r%mrt,gsel,tzdia0%mrt )
    tpdia%mrl = UNPACK( tzdia_r%mrl,gsel,tzdia0%mrl )
    tpdia%sie = UNPACK( tzdia_r%sie,gsel,tzdia0%sie )
    tpdia%sne = UNPACK( tzdia_r%sne,gsel,tzdia0%sne )
    tpdia%sni = UNPACK( tzdia_r%sni,gsel,tzdia0%sni )
    tpdia%snm = UNPACK( tzdia_r%snm,gsel,tzdia0%snm )
    tpdia%snml = UNPACK( tzdia_r%snml,gsel,tzdia0%snml )
    tpdia%sop = UNPACK( tzdia_r%sop,gsel,tzdia0%sop )
    tpdia%the = UNPACK( tzdia_r%the,gsel,tzdia0%the )
    tpdia%tin = UNPACK( tzdia_r%tin,gsel,tzdia0%tin )
    tpdia%tiw = UNPACK( tzdia_r%tiw,gsel,tzdia0%tiw )
    tpdia%qoi = UNPACK( tzdia_r%qoi,gsel,tzdia0%qoi )
    tpdia%xtr = UNPACK( tzdia_r%xtr,gsel,tzdia0%xtr )
    tpdia%ytr = UNPACK( tzdia_r%ytr,gsel,tzdia0%ytr )
    tpdia%sp1 = UNPACK( tzdia_r%sp1,gsel,tzdia0%sp1 )
    tpdia%sp2 = UNPACK( tzdia_r%sp2,gsel,tzdia0%sp2 )
    tpdia%sus = UNPACK( tzdia_r%sus,gsel,tzdia0%sus )
    tpdia%sut = UNPACK( tzdia_r%sut,gsel,tzdia0%sut )
    tpdia%sui = UNPACK( tzdia_r%sui,gsel,tzdia0%sui )
    tpdia%suw = UNPACK( tzdia_r%suw,gsel,tzdia0%suw )
    tpdia%s_pr = UNPACK( tzdia_r%s_pr,gsel,tzdia0%s_pr )
    tpdia%s_prsn = UNPACK( tzdia_r%s_prsn,gsel,tzdia0%s_prsn )
    tpdia%o_pr = UNPACK( tzdia_r%o_pr,gsel,tzdia0%o_pr )
    tpdia%o_prsn = UNPACK( tzdia_r%o_prsn,gsel,tzdia0%o_prsn )
    tpdia%subcio = UNPACK( tzdia_r%subcio,gsel,tzdia0%subcio )
    tpdia%snicio = UNPACK( tzdia_r%snicio,gsel,tzdia0%snicio )
    tpdia%hsicio = UNPACK( tzdia_r%hsicio,gsel,tzdia0%hsicio )
    tpdia%lmlcio = UNPACK( tzdia_r%lmlcio,gsel,tzdia0%lmlcio )
    tpdia%salcio = UNPACK( tzdia_r%salcio,gsel,tzdia0%salcio )
    tpdia%l_pr = UNPACK( tzdia_r%l_pr,gsel,tzdia0%l_pr )
    tpdia%l_prsn = UNPACK( tzdia_r%l_prsn,gsel,tzdia0%l_prsn )
    tpdia%sul = UNPACK( tzdia_r%sul,gsel,tzdia0%sul )
!
! .. Fluxes transmitted to the mixed layer
!
! Components of tptfl to be updated out of the reduced grid:
!  - llo : short wave on leads : only thermo_lead
!  - tlo : non solar on leads : only in thermo_lead
!  - wlo : fresh water mass flux through leads : only in thermo_lead
! (Redundant with what is in imod_thermo_lead... Caution !)
!
! Short wave on leads
    tztfl0(:,:)%llo = tptfl(:,:)%llo + tpblkw(:,:)%swa
    tptfl%llo = UNPACK( tztfl_r%llo,gsel,tztfl0%llo )
!
! Non solar flux on leads
    IF ( nsnwrad==1 ) THEN
        zsnflx(:,:) = -xmhofusn0*tpatm(:,:)%sop
      ELSE
        zsnflx(:,:) = 0.
    ENDIF
    tztfl0(:,:)%tlo = tptfl(:,:)%tlo +  &
      tpmxl(:,:)%qml + tpblkw(:,:)%nsf + zsnflx(:,:)
    tptfl%tlo = UNPACK( tztfl_r%tlo,gsel,tztfl0%tlo )
!
! Water flux on leads
    tztfl0(:,:)%wlo = tptfl(:,:)%wlo +  &
      tpatm(:,:)%lip + tpatm(:,:)%sop + tpblkw(:,:)%eva
    tptfl%wlo = UNPACK( tztfl_r%wlo,gsel,tztfl0%wlo )
!
! Short wave flux on leads
    tztfl0(:,:)%lio = 0.
    tptfl%lio = UNPACK( tztfl_r%lio,gsel,tztfl0%lio )
!
! Non solar flux under sea ice
    tztfl0(:,:)%tio = 0.
    tptfl%tio = UNPACK( tztfl_r%tio,gsel,tztfl0%tio )
!
! Salinity flux under sea ice
    tztfl0(:,:)%sio = 0.
    tptfl%sio = UNPACK( tztfl_r%sio,gsel,tztfl0%sio )
!
! Concentration/dilution flux under sea ice
    tztfl0(:,:)%cio = 0.
    tptfl%cio = UNPACK( tztfl_r%cio,gsel,tztfl0%cio )
!
! Water flux mass flux under sea ice
    tztfl0(:,:)%wio = 0.
    tptfl%wio = UNPACK( tztfl_r%wio,gsel,tztfl0%wio )
!
! Sea ice-ocean stress X-component
    tztfl0(:,:)%xio = 0.
    tptfl%xio = UNPACK( tztfl_r%xio,gsel,tztfl0%xio )
!
! Sea ice-ocean stress Y-component
    tztfl0(:,:)%yio = 0.
    tptfl%yio = UNPACK( tztfl_r%yio,gsel,tztfl0%yio )
!
! .. Sea ice 2D
!
    tzsit0(:,:,:)%esi = .FALSE.
    tzsit0(:,:,:)%asn = albw
    tzsit0(:,:,:)%fsi = 0.
    tzsit0(:,:,:)%hsi = 0.
    tzsit0(:,:,:)%hsn = 0.
    tzsit0(:,:,:)%rsn = rhosnwmin
    tzsit0(:,:,:)%tsf = SPREAD( tpmxl(:,:)%mlf,1,nt )
    tzsit0(:,:,:)%ssi = 0.
    tzsit0(:,:,:)%age = 0.
    tzsit0(:,:,:)%vmp = 0.
!
    DO jk=1,nt
      tpsit(jk,:,:)%esi = UNPACK( tzsit_r(jk,:)%esi,gsel,tzsit0(jk,:,:)%esi )
      tpsit(jk,:,:)%asn = UNPACK( tzsit_r(jk,:)%asn,gsel,tzsit0(jk,:,:)%asn )
      tpsit(jk,:,:)%fsi = UNPACK( tzsit_r(jk,:)%fsi,gsel,tzsit0(jk,:,:)%fsi )
      tpsit(jk,:,:)%hsi = UNPACK( tzsit_r(jk,:)%hsi,gsel,tzsit0(jk,:,:)%hsi )
      tpsit(jk,:,:)%hsn = UNPACK( tzsit_r(jk,:)%hsn,gsel,tzsit0(jk,:,:)%hsn )
      tpsit(jk,:,:)%rsn = UNPACK( tzsit_r(jk,:)%rsn,gsel,tzsit0(jk,:,:)%rsn )
      tpsit(jk,:,:)%tsf = UNPACK( tzsit_r(jk,:)%tsf,gsel,tzsit0(jk,:,:)%tsf )
      tpsit(jk,:,:)%ssi = UNPACK( tzsit_r(jk,:)%ssi,gsel,tzsit0(jk,:,:)%ssi )
      tpsit(jk,:,:)%age = UNPACK( tzsit_r(jk,:)%age,gsel,tzsit0(jk,:,:)%age )
      tpsit(jk,:,:)%vmp = UNPACK( tzsit_r(jk,:)%vmp,gsel,tzsit0(jk,:,:)%vmp )
    END DO
!
! .. Sea ice 3D
!
    tzsil0(:,:,:,:)%ent = 0.
!
    DO jl=1,nl
      DO jk=1,nt
        tpsil(jl,jk,:,:)%ent = UNPACK( tzsil_r(jl,jk,:)%ent,gsel,  &
          tzsil0(jl,jk,:,:)%ent )
      END DO
    END DO
!
!
!
! 3. Deallocations
! =================
!
! .. Note that on some platforms, in contradiction with the Fortran 95 norm,
! locally-allocated arrays are not released automatically. More over, some
! platforms require that the last allocated array in deallocated first.
! These rules have to be respected if more arrays are allocated/deallocated 
! here.
!
!    DEALLOCATE( ind_r )
    DEALLOCATE( tzsil_r )
    DEALLOCATE( tzsit_r )
    DEALLOCATE( tztfl_r )
    DEALLOCATE( tzdia_r )
    DEALLOCATE( tzbud_r )
    DEALLOCATE( tzblki_r )
    DEALLOCATE( tzblkw_r )
    DEALLOCATE( tzatm_r )
    DEALLOCATE( tzmxl_r )
    DEALLOCATE( zustar_r )
    DEALLOCATE( tzdom_r )
  ENDIF
!
!
!
! 4. Formats + Farewell message
! ==============================
!
1000 FORMAT( " Processor ", I5," ==> Running on ", I5,  &
  " points instead of ", I6, "(" I5, " times " , I5, ")" ) 
!
  IF (lp1) THEN
    WRITE(noutlu,*) ' '
    WRITE(noutlu,*) ' *** LEVEL 3 - END SUBROUTINE THERMO'
    WRITE(noutlu,*) ' '
  ENDIF
!
END SUBROUTINE glt_thermo

! ----------------------- END SUBROUTINE glt_thermo -------------------------
! -----------------------------------------------------------------------
