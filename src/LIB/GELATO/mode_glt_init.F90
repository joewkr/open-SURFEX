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
! ========================== MODULE mode_glt_init ===========================
! =======================================================================
!
! 
!  This module contains subroutines that are used to implement all
!  necessary initial conditions in Gelato model: domain, ice state and 
!  superficial ocean conditions. It also manages forcing fields, 
!  initial surface fluxes, initial salinity and results storage. 
!
! Modified: 2012/05 (D. Salas y Melia)
!           Change in dimensions / multi-tasking 
!  
! ----------------------- BEGIN MODULE mode_glt_init ------------------------

MODULE mode_glt_init 
INTERFACE

#if ! defined in_surfex
SUBROUTINE inidmn(tpglt)
  USE modd_types_glt
  TYPE(t_glt), INTENT(inout) ::  &
    tpglt
END SUBROUTINE inidmn
#endif

SUBROUTINE initfl(tptfl)
  USE modd_types_glt
  USE modd_glt_param
  TYPE(t_tfl), DIMENSION(nx,ny), INTENT(inout) ::  &
        tptfl
END SUBROUTINE initfl

SUBROUTINE inisal  &
  ( tpdom,tpmxl,tpsit )
  USE modd_types_glt
  USE modd_glt_param
  TYPE(t_dom), DIMENSION(nx,ny), INTENT(in) ::  &
    tpdom
  TYPE(t_mxl), DIMENSION(nx,ny), INTENT(in) ::  &
    tpmxl
  TYPE(t_sit), DIMENSION(nt,nx,ny), INTENT(inout) ::  &
    tpsit
END SUBROUTINE inisal

SUBROUTINE inidia(tpind,tpdia,pcumdia0,pcumdia)
  USE modd_types_glt
  USE modd_glt_param
  TYPE(t_ind), INTENT(in) ::  &
        tpind
  TYPE(t_dia), DIMENSION(nx,ny), INTENT(inout) ::  &
        tpdia
  REAL, DIMENSION(ndiamax,1,1), INTENT(out) ::  &
        pcumdia0
  REAL, DIMENSION(ndiamax,nx,ny), INTENT(out) ::  &
        pcumdia
END SUBROUTINE inidia

SUBROUTINE init_timers
END SUBROUTINE init_timers

END INTERFACE
END MODULE mode_glt_init

! ------------------------ END MODULE mode_glt_init -------------------------


#if ! defined in_surfex
! -----------------------------------------------------------------------
! ------------------------- SUBROUTINE INIDMN ---------------------------
!
! The mesh is read in this routine, and scattered. This routine could 
! be replaced with anything else that does the same. 
! Note that bnddmn must be called after, to be sure boundaries are 
! well-defined.
!
SUBROUTINE inidmn( tpglt )
!
  USE modd_types_glt
  USE modd_glt_param
  USE modd_glt_const_thm
  USE mode_gltools_mpi
  USE modi_gltools_mskerr
  IMPLICIT NONE 
!
  TYPE(t_glt), INTENT(inout) ::  &
    tpglt
!
  CHARACTER(80) ::  &
        yfname,yword
  INTEGER, DIMENSION(SIZE(tpglt%dom,1),SIZE(tpglt%dom,2)) ::  &
        iwork2_p
  INTEGER, DIMENSION(nxglo,nyglo) ::  &
        iwork2
  REAL, DIMENSION(SIZE(tpglt%dom,1),SIZE(tpglt%dom,2)) ::  &
        zwork2_p
  REAL, DIMENSION(nxglo,nyglo) ::  &
        zwork2
!
!
!
! 1. Initializations
! ===================
!
! .. Welcome message
!
  IF (lwg) THEN
    WRITE(noutlu,*) ' '
    WRITE(noutlu,*) ' *** LEVEL 3 - SUBROUTINE INIDMN'
    WRITE(noutlu,*) ' '
!
! .. Written confirmation
!
    WRITE(noutlu,*) '    The grid we use is : '//cn_grdname
  ENDIF
!
!
!
! 2. Read grid definitions
! =========================
!
! This should be done on all gelato's processors, since tdom_p must be known 
! by all processors
!  
! * Read the land-sea mask (= 1 if ocean, 0 if land)
!
  yfname = 'opamesh.asc'
!
  IF (lwg) THEN
    WRITE(noutlu,'("Processor ",I5," : read grid from ",A)')  &
      gelato_myrank,yfname
    WRITE(noutlu,*) ' '
  ENDIF

! * Read land-sea T-mask.
!
  OPEN(UNIT=ngrdlu,FILE=yfname,FORM='FORMATTED')
  READ(ngrdlu,*) yword
  CALL gltools_mskerr( 'TMK',yfname,yword )
  READ(ngrdlu,*) iwork2(:,:)
!
!  iwork2(120,9)=0
!  iwork2(120,9)=1
  CALL scatter2d( iwork2,iwork2_p )
  tpglt%dom(:,:)%tmk = iwork2_p(:,:)
!
! * Read land-sea U-mask. This is to comply the current opamesh.asc 
! format. This information is actually not used.
! 
  READ(ngrdlu,*) yword
  READ(ngrdlu,*) iwork2
!
! * Read land-sea V-mask. This is to comply the current opamesh.asc 
! format. This information is actually not used.
!
  READ(ngrdlu,*) yword
  READ(ngrdlu,*) iwork2
!
! * Read grid points latitude, in degrees and convert to radians.
!
  READ(ngrdlu,*) yword
  CALL gltools_mskerr( 'TLA',yfname,yword )
  READ(ngrdlu,*) zwork2
  zwork2(:,:) = zwork2(:,:) * pi / 180.
  CALL scatter2d( zwork2,zwork2_p )
  tpglt%dom(:,:)%lat = zwork2_p(:,:)
!
! * Read grid points longitude, in degrees and convert to radians.
!
  READ(ngrdlu,*) yword
  CALL gltools_mskerr( 'TLO',yfname,yword )
  READ(ngrdlu,*) zwork2
  zwork2(:,:) = zwork2(:,:) * pi / 180.
  CALL scatter2d( zwork2,zwork2_p )
  tpglt%dom(:,:)%lon = zwork2_p(:,:)
!
! * Read grid cell dimensions (m) in the X and Y directions. 
!
  READ(ngrdlu,*) yword
  CALL gltools_mskerr( 'TSX',yfname,yword )
  READ(ngrdlu,*) zwork2
  CALL scatter2d( zwork2,zwork2_p )
  tpglt%dom(:,:)%dxc = zwork2_p(:,:)
!
  READ(ngrdlu,*) yword
  CALL gltools_mskerr( 'TSY',yfname,yword )
  READ(ngrdlu,*) zwork2
  CALL scatter2d( zwork2,zwork2_p )
  tpglt%dom(:,:)%dyc = zwork2_p(:,:)
!
!
! *** Close file
!
  CLOSE(ngrdlu)
!
! .. Farewell message
!
  IF (lwg) THEN
    WRITE(noutlu,*) ' '
    WRITE(noutlu,*) ' *** LEVEL 3 - END SUBROUTINE INIDMN'
    WRITE(noutlu,*) ' '
  ENDIF
!
END SUBROUTINE inidmn
!
! ----------------------- END SUBROUTINE INIDMN -------------------------
! -----------------------------------------------------------------------
!
! -----------------------------------------------------------------------
! ------------------------- SUBROUTINE BNDMN ---------------------------
!
SUBROUTINE bnddmn( tpglt )
!
  USE dom_oce
  USE modd_types_glt
  USE modd_glt_param
  USE modd_glt_const_thm
  USE mode_gltools_bound
  USE lib_mpp
  IMPLICIT NONE 
!
  TYPE(t_glt), INTENT(inout) ::  &
    tpglt
!
  INTEGER ::  &
        ji,jj
  INTEGER, DIMENSION(SIZE(tpglt%dom,1),SIZE(tpglt%dom,2)) ::  &
        iwork2_p
  REAL, DIMENSION(SIZE(tpglt%dom,1),SIZE(tpglt%dom,2)) ::  &
        zwork2_p
!
!
!
! 1. Initializations
! ===================
!
! .. Welcome message
!
  IF (lwg) THEN
    WRITE(noutlu,*) ' '
    WRITE(noutlu,*) ' *** LEVEL 3 - SUBROUTINE BNDDMN'
    WRITE(noutlu,*) ' '
  ENDIF
!
!
!
! 2. Define new fields and gltools_bound 
! ===============================
!
! * Land-sea T-mask.
!
  zwork2_p(:,:) = FLOAT( tpglt%dom(:,:)%tmk )
  CALL gltools_bound( 'T','scalar',zwork2_p )
  tpglt%dom(:,:)%tmk = INT( zwork2_p(:,:) ) 
!
! * Interior T-mask (ghost points are masked)
  iwork2_p(:,:) = tpglt%dom(:,:)%tmk
  iwork2_p(1:jpreci,:) = 0
  iwork2_p(nlci-jpreci+1:nx,:) = 0
  iwork2_p(:,1:jprecj) = 0
  iwork2_p(:,nlcj-jprecj+1:ny) = 0 
  tpglt%dom(:,:)%imk = iwork2_p(:,:)
!
! * i and j index in global grid. This is a particular case. The global
! array has to be defined in this routine, not in glt_init, since 
! glt_init is not defined within NEMO. However, in principle, the 
! subdomain array could probably be defined directly.
!
  DO jj=1,ny
    DO ji=1,nx
      tpglt%dom(ji,jj)%indi = nimpp+ji-nldi
      tpglt%dom(ji,jj)%indj = njmpp+jj-nldj
    END DO
  END DO
  zwork2_p(:,:) = FLOAT( tpglt%dom(:,:)%indi )
  CALL gltools_bound( 'T','scalar',zwork2_p )
  tpglt%dom(:,:)%indi = INT( zwork2_p(:,:) )
  zwork2_p(:,:) = FLOAT( tpglt%dom(:,:)%indj )
  CALL gltools_bound( 'T','scalar',zwork2_p )
  tpglt%dom(:,:)%indj = INT( zwork2_p(:,:) )
!  
! * Land-sea U-mask
!
  iwork2_p(:,:) = 0
!
  DO jj=2,ny-1
    DO ji=2,nx-1
    iwork2_p(ji,jj) = tpglt%dom(ji+1,jj)%tmk * tpglt%dom(ji,jj)%tmk
    END DO
  END DO
!
  zwork2_p(:,:) = FLOAT( iwork2_p(:,:) )
  CALL gltools_bound( 'U','scalar',zwork2_p )
  tpglt%dom(:,:)%umk = INT( zwork2_p(:,:) ) 
!
! * Land-sea V-mask
!
  iwork2_p(:,:) = 0
!
  DO jj=2,ny-1
    DO ji=2,nx-1
      iwork2_p(ji,jj) = tpglt%dom(ji,jj+1)%tmk * tpglt%dom(ji,jj)%tmk
    END DO
  END DO
!
  zwork2_p(:,:) = FLOAT( iwork2_p(:,:) )
  CALL gltools_bound( 'V','scalar',zwork2_p )
  tpglt%dom(:,:)%vmk = INT( zwork2_p(:,:) ) 
!
! * Grid points latitudes
!
  zwork2_p(:,:) = tpglt%dom(:,:)%lat
  CALL gltools_bound( 'T','scalar',zwork2_p )
  tpglt%dom(:,:)%lat = zwork2_p(:,:)
!
! * Grid points longitudes
!
  zwork2_p(:,:) = tpglt%dom(:,:)%lon
  CALL gltools_bound( 'T','scalar',zwork2_p )
  tpglt%dom(:,:)%lon = zwork2_p(:,:)
!
! * Grid cell dimensions (m) in the X direction
!
  zwork2_p(:,:) = tpglt%dom(:,:)%dxc
  CALL gltools_bound( 'T','scafac',zwork2_p )
  tpglt%dom(:,:)%dxc = zwork2_p(:,:)
!
! * Grid cell dimensions (m) in the Y direction
!
  zwork2_p(:,:) = tpglt%dom(:,:)%dyc
  CALL gltools_bound( 'T','scafac',zwork2_p )
  tpglt%dom(:,:)%dyc = zwork2_p(:,:)
!
! * Compute grid cell area (m2) - to avoid computing it many times 
! in different parts of the code
!
  zwork2_p(:,:) = tpglt%dom(:,:)%dxc*tpglt%dom(:,:)%dyc
  zwork2_p(1,:) = 0.
  zwork2_p(nx,:) = 0.
  zwork2_p(:,1) = 0.
  zwork2_p(:,ny) = 0. 
  tpglt%dom(:,:)%srf = zwork2_p(:,:)
!
! * Surface of local ocean domain (ghost points are masked out)
!
! Note this one cannot change from one time step to the other.
! In contrast, the reduced grid changes at every time step. Hence, 
! the reduced grid surface xdomsrf_r has to be re-computed every time
! this reduced grid is redefined (in thermo.f90)
  xdomsrf = SUM( tpglt%dom(:,:)%srf, MASK=(tpglt%dom(:,:)%tmk==1) )
!
! * Total surface of the ocean domain (ghost points are masked out)
  xdomsrf_g = xdomsrf
  CALL mpp_sum(xdomsrf_g)
!
! *** Close file
!
! .. Farewell message
!
  IF (lwg) THEN
    WRITE(noutlu,*) ' '
    WRITE(noutlu,*) ' *** LEVEL 3 - END SUBROUTINE BNDDMN'
    WRITE(noutlu,*) ' '
  ENDIF
!
END SUBROUTINE bnddmn
!
! ----------------------- END SUBROUTINE BNDDMN -------------------------
! -----------------------------------------------------------------------
#endif 

! -----------------------------------------------------------------------
! ------------------------- SUBROUTINE INITFL ---------------------------
!
SUBROUTINE initfl(tptfl)
  USE modd_types_glt
  USE modd_glt_param
  IMPLICIT NONE
!
  TYPE(t_tfl), DIMENSION(nx,ny), INTENT(inout) ::  &
        tptfl
!
!
! * Initialize fluxes transmitted by sea ice and water
!
  tptfl(:,:)%lio = 0.
  tptfl(:,:)%llo = 0.
  tptfl(:,:)%tio = 0.
  tptfl(:,:)%tlo = 0.
  tptfl(:,:)%sio = 0.
  tptfl(:,:)%cio = 0.
  tptfl(:,:)%wio = 0.
  tptfl(:,:)%wlo = 0.
  tptfl(:,:)%xio = 0.
  tptfl(:,:)%yio = 0.
!
END SUBROUTINE initfl
!
! ----------------------- END SUBROUTINE INITFL -------------------------
! -----------------------------------------------------------------------


! -----------------------------------------------------------------------
! ------------------------- SUBROUTINE INISAL ---------------------------
!
SUBROUTINE inisal  &
  ( tpdom,tpmxl,tpsit )
!
  USE modd_types_glt
  USE modd_glt_param
  USE modd_glt_const_thm
  IMPLICIT NONE
!
  TYPE(t_dom), DIMENSION(nx,ny), INTENT(in) ::  &
    tpdom
  TYPE(t_mxl), DIMENSION(nx,ny), INTENT(in) ::  &
    tpmxl
  TYPE(t_sit), DIMENSION(nt,nx,ny), INTENT(inout) ::  &
    tpsit
!
  INTEGER ::  &
    jk
!
!
!
! 1. Initialize sea ice salinity
! ===============================
!
! .. If a negative salinity is encountered, it means it was not 
! initialized
!
  IF ( ANY( tpsit(:,:,:)%ssi < -0.5 ) ) THEN
      IF ( nicesal==1 ) THEN
        DO jk=1,nt
          tpsit(jk,:,:)%ssi =  &
            ssinew / ssw0 * tpmxl(:,:)%sml * FLOAT( tpdom(:,:)%tmk )
        END DO
      ELSE
        tpsit(:,:,:)%ssi = sice 
      ENDIF 
  ENDIF
!
END SUBROUTINE inisal

! ----------------------- END SUBROUTINE INISAL -------------------------
! -----------------------------------------------------------------------


! -----------------------------------------------------------------------
! ------------------------- SUBROUTINE INIDIA ---------------------------
!
SUBROUTINE inidia(tpind,tpdia,pcumdia0,pcumdia)
  USE modd_types_glt
  USE modd_glt_param
  IMPLICIT NONE
!
  TYPE(t_ind), INTENT(inout) ::  &
        tpind
  TYPE(t_dia), DIMENSION(nx,ny), INTENT(inout) ::  &
        tpdia
  REAL, DIMENSION(ndiamax,1,1), INTENT(out) ::  &
        pcumdia0
  REAL, DIMENSION(ndiamax,nx,ny), INTENT(out) ::  &
        pcumdia
!
!
! * Initialize diagnostic fields (see definitions in dmod_types.f90)
!
  tpdia(:,:)%uvl = 0.
  tpdia(:,:)%vvl = 0.
  tpdia(:,:)%asi = 0.
  tpdia(:,:)%amp = 0.
  tpdia(:,:)%asn = 0.
  tpdia(:,:)%cgl = 0.
  tpdia(:,:)%dsa = 0.
  tpdia(:,:)%dds = 0.
  tpdia(:,:)%dsn = 0.
  tpdia(:,:)%ddn = 0.
  tpdia(:,:)%dsi = 0.
  tpdia(:,:)%ddi = 0.
  tpdia(:,:)%dci = 0.
  tpdia(:,:)%cst = 0.
  tpdia(:,:)%dwi = 0.
  tpdia(:,:)%lip = 0.
  tpdia(:,:)%lsi = 0.
  tpdia(:,:)%mrb = 0.
  tpdia(:,:)%mrt = 0.
  tpdia(:,:)%mrl = 0.
  tpdia(:,:)%sie = 0.
  tpdia(:,:)%sne = 0.
  tpdia(:,:)%sni = 0.
  tpdia(:,:)%snm = 0.
  tpdia(:,:)%snml = 0.
  tpdia(:,:)%sop = 0.
  tpdia(:,:)%the = 0.
  tpdia(:,:)%tin = 0.
  tpdia(:,:)%qoi = 0.
  tpdia(:,:)%xtr = 0.
  tpdia(:,:)%ytr = 0.
  tpdia(:,:)%sp1 = 0.
  tpdia(:,:)%sp2 = 0.
  tpdia(:,:)%sui = 0.
  tpdia(:,:)%sut = 0.
  tpdia(:,:)%sus = 0.
  tpdia(:,:)%suw = 0.
  tpdia(:,:)%sul = 0.
  tpdia(:,:)%s_pr = 0.
  tpdia(:,:)%s_prsn = 0.
  tpdia(:,:)%o_pr = 0.
  tpdia(:,:)%o_prsn = 0.
  tpdia(:,:)%l_prsn = 0.
  tpdia(:,:)%subcio = 0.
  tpdia(:,:)%snicio = 0.
  tpdia(:,:)%hsicio = 0.
  tpdia(:,:)%lmlcio = 0.
  tpdia(:,:)%salcio = 0.
  tpdia(:,:)%dmp = 0.
! Accumulated diagnostic: initialization at first time step only
! The condition may change if diagnostic frequency is other than 
! time-step frequency or once per run, the only options in Gelato 6
  IF( tpind%cur==tpind%beg ) THEN
      tpdia(:,:)%aiw = 0.
      tpdia(:,:)%sic = 0.
      tpdia(:,:)%sit = 0.
      tpdia(:,:)%snd = 0.
      tpdia(:,:)%tiw = 0.
      tpind%nts = 0
      pcumdia0(:,:,:) = 0.
      pcumdia(:,:,:) = 0.
  ENDIF
! - Note that tpdia%ifw, %swi, %sww were already initialized in glt_getatmf,
! so they must not be set to zero here ! (DOES NOT HOLD ANYMORE HERE: NO
! DANGER TO DO SO AS inidia IS NOW INVOKED BEFORE GETATMF...)
! - Note that tpdia%atx, %aty, %otx, %oty need not be initialized here
! as they will be defined at the end of the code (sndmlrf)
!
END SUBROUTINE inidia
!
! ----------------------- END SUBROUTINE INIDIA -------------------------
! -----------------------------------------------------------------------
!
!
! -----------------------------------------------------------------------
! ----------------------- SUBROUTINE INIT_TIMERS ------------------------
SUBROUTINE init_timers
  USE modd_glt_param
!
  IF ( ntimers==1 ) THEN
    CALL CPU_TIME( xtime )
    clabel = 'FIRST000'
    ntimnum = 1
  ENDIF
END SUBROUTINE init_timers
! --------------------- END SUBROUTINE INIT_TIMERS ----------------------
! -----------------------------------------------------------------------
