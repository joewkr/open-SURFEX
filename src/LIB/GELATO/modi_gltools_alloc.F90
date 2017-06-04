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
! ====================== MODULE modi_gltools_alloc ========================
! =======================================================================
!
! Goal:
! -----
!   Allocates glt_gelato main arrays and structures    
!
! Created : 2004/01 (D. Salas y Melia)
! Modified: 2011/12 (D. Salas y Melia)
!           Collect the names of allocated arrays in a linked list, and
!           eventually dump this list in an array of strings, in order
!           to keep the allocation order in memory. This order will be 
!           necessary to deallocate the arrays in reverse order.
! Modified: 2012/11 (D. Salas y Melia)
!           Inverse order deallocation seems unuseful - simplify.
!           Only the main tglt super-structure is allocated.
!
! -------------------- BEGIN MODULE modi_gltools_alloc --------------------
!
!THXS_SFX!MODULE modi_gltools_alloc
!THXS_SFX!INTERFACE
!THXS_SFX!!
!THXS_SFX!SUBROUTINE gltools_alloc (tpglt)
!THXS_SFX!USE modd_types_glt 
!THXS_SFX!USE modd_glt_param
!THXS_SFX!TYPE(t_glt), INTENT(inout) ::  &
!THXS_SFX!    tpglt
!THXS_SFX!END SUBROUTINE gltools_alloc
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_gltools_alloc
!
! -------------------- END MODULE modi_gltools_alloc ----------------------
!
!
! -----------------------------------------------------------------------
! -------------------------- SUBROUTINE gltools_alloc ---------------------------
!
SUBROUTINE gltools_alloc(tpglt)
!
USE modd_types_glt 
USE modd_glt_param
#if ! defined in_surfex
USE modd_CB_DynVariables
USE modd_CB_DynDim
USE modd_CB_const
USE modd_CB_mask
USE modd_CB_DynForcing
!USE modd_CB_ThermoVariables
#endif
!
IMPLICIT NONE
!
TYPE(t_glt), INTENT(inout) ::  &
    tpglt
!
!
! 1. Initialisations 
! ===================
!
IF (lp1) THEN
  WRITE(noutlu,*) ' '
  WRITE(noutlu,*) '  ** LEVEL 3 - SUBROUTINE ALLOC'
  WRITE(noutlu,*) ' '
ENDIF
!
!
#if ! defined in_surfex
!
! 2. Allocate arrays for McGill's dynamics
! =========================================
!
IF ( ndynami==2 ) THEN
! modd_CB_dynvariables.F90
    ALLOCATE( h        (0:nxd+2,0:nyd+2) )
    ALLOCATE( A        (0:nxd+2,0:nyd+2) )
    ALLOCATE( uice     (0:nxd+2,0:nyd+2) )
    ALLOCATE( vice     (0:nxd+2,0:nyd+2) )
    ALLOCATE( ut1      (0:nxd+2,0:nyd+2) )
    ALLOCATE( vt1      (0:nxd+2,0:nyd+2) )
    ALLOCATE( p        (0:nxd+2,0:nyd+2) )
    ALLOCATE( etaC     (0:nxd+2,0:nyd+2) )
    ALLOCATE( etaB     (0:nxd+2,0:nyd+2) )
    ALLOCATE( zetaC    (0:nxd+2,0:nyd+2) )
    ALLOCATE( etaCf    (0:nxd+2,0:nyd+2) )
    ALLOCATE( etaBf    (0:nxd+2,0:nyd+2) )
    ALLOCATE( zetaCf   (0:nxd+2,0:nyd+2) )
    ALLOCATE( tracer   (0:nxd+2,0:nyd+2,2) )
! modd_CB_Dyndim.F90
    ALLOCATE( CdwC1    (0:nxd+2,0:nyd+2) )
    ALLOCATE( CdwC2    (0:nxd+2,0:nyd+2) )
    ALLOCATE( CdwC1f   (0:nxd+2,0:nyd+2) )
    ALLOCATE( CdwC2f   (0:nxd+2,0:nyd+2) )
! modd_CB_const.F90
    ALLOCATE( sinlat(0:nxd+2,0:nyd+2) ) ! ok
    ALLOCATE( coslat(0:nxd+2,0:nyd+2) ) ! ok
! modd_CB_mask.F90
    ALLOCATE( maskB    ( 0:nxd+2, 0:nyd+2 ) ) ! ok
    ALLOCATE( maskC    ( 0:nxd+2, 0:nyd+2 ) ) ! ok
! modd_CB_DynForcing.F90
    ALLOCATE( uwatnd   (0:nxd+2,0:nyd+2) )
    ALLOCATE( vwatnd   (0:nxd+2,0:nyd+2) )
    ALLOCATE( speediw  (0:nxd+2,0:nyd+2) )
    ALLOCATE( R1       (0:nxd+2,0:nyd+2) )
    ALLOCATE( R2       (0:nxd+2,0:nyd+2) )
    ALLOCATE( R1n      (0:nxd+2,0:nyd+2) )
    ALLOCATE( R2n      (0:nxd+2,0:nyd+2) )
    ALLOCATE( bu_ind   (0:nxd+2,0:nyd+2) )
    ALLOCATE( bv_ind   (0:nxd+2,0:nyd+2) )
    ALLOCATE( bu       (0:nxd+2,0:nyd+2) )
    ALLOCATE( bv       (0:nxd+2,0:nyd+2) )
ENDIF
#endif
!
!
!
! 3. Allocate all tpglt arrays and structures
! ============================================
!
ALLOCATE( tpglt%bat(nx,ny) )
ALLOCATE( tpglt%dom(nx,ny) )  
!
#if ! defined in_surfex
! Dans ce qui suit, les champs marqués 'adv' sont nécessaires à l'advection
! Les autres ne le sont pas, mais il faut voir s'ils ne sont pas définis 
! en même temps...
! (possibilité d'économiser de la mémoire si on utilise l'advection après
! le traitement de la dynamique par la méthode du McGill)
! Même traitement dans 'dealloc'
!
IF ( ndynami==1 .OR. nadvect==1 ) THEN
    ALLOCATE( tpglt%dxtr(ilo:ihi,jlo:jhi) )
    ALLOCATE( tpglt%dytr(ilo:ihi,jlo:jhi) )
    ALLOCATE( tpglt%dxtr4(ilo:ihi,jlo:jhi) )
    ALLOCATE( tpglt%dytr4(ilo:ihi,jlo:jhi) )
    ALLOCATE( tpglt%fcor(ilo:ihi,jlo:jhi) )
    ALLOCATE( tpglt%tarear(ilo:ihi,jlo:jhi) )  ! adv
!
    ALLOCATE( tpglt%HTN(nx,ny) )
    ALLOCATE( tpglt%HTE(nx,ny) )
    ALLOCATE( tpglt%HTS(nx,ny) )   ! adv 
    ALLOCATE( tpglt%HTW(nx,ny) )   ! adv
    ALLOCATE( tpglt%hm(nx,ny) )    ! adv
    ALLOCATE( tpglt%um(nx,ny) )
    ALLOCATE( tpglt%dxta(nx,ny) )  ! adv
    ALLOCATE( tpglt%dyta(nx,ny) )  ! adv
    ALLOCATE( tpglt%dxt2r(nx,ny) ) ! adv
    ALLOCATE( tpglt%dyt2r(nx,ny) ) ! adv
    ALLOCATE( tpglt%HTNa(nx,ny) )  ! adv
    ALLOCATE( tpglt%HTEa(nx,ny) )  ! adv
    ALLOCATE( tpglt%cx(nx,ny) )    ! adv
    ALLOCATE( tpglt%cy(nx,ny) )    ! adv 
    ALLOCATE( tpglt%cxx(nx,ny) )   ! adv
    ALLOCATE( tpglt%cxy(nx,ny) )   ! adv
    ALLOCATE( tpglt%cyy(nx,ny) )   ! adv
    ALLOCATE( tpglt%cxxx(nx,ny) )  ! adv
    ALLOCATE( tpglt%cxxy(nx,ny) )  ! adv
    ALLOCATE( tpglt%cxyy(nx,ny) )  ! adv
    ALLOCATE( tpglt%cyyy(nx,ny) )  ! adv
!
    ALLOCATE( tpglt%mne(2,2,nx,ny) )! adv
    ALLOCATE( tpglt%mnw(2,2,nx,ny) )! adv 
    ALLOCATE( tpglt%msw(2,2,nx,ny) )! adv
    ALLOCATE( tpglt%mse(2,2,nx,ny) )! adv
ENDIF
!
! Arrays for McGill's dynamics (all other arrays declared in 3. should
! end up in this part !)
!
IF ( ndynami==2 ) THEN
    ALLOCATE( tpglt%index2d(2,0:nxd+2,0:nyd+2) )
    ALLOCATE( tpglt%mskice(2,0:nxd+2,0:nyd+2) )
ENDIF
#endif
!
ALLOCATE( tpglt%oce_all(nx,ny) )
ALLOCATE( tpglt%atm_all(nx,ny) )
ALLOCATE( tpglt%all_oce(nx,ny) )
!
IF ( nnflxin==0 ) THEN
    ALLOCATE( tpglt%atm_mix(1,nx,ny) )
    ALLOCATE( tpglt%mix_atm(1,nx,ny) )
  ELSE
    ALLOCATE( tpglt%atm_ice(nnflxin,nx,ny) )
    ALLOCATE( tpglt%atm_wat(nx,ny) )
    ALLOCATE( tpglt%ice_atm(nnflxin,nx,ny) )
ENDIF
!
IF ( ntd/=0 ) ALLOCATE( tpglt%sit_d(ntd,nx,ny) )
!
IF ( ndynami==1 ) THEN
    ALLOCATE( tpglt%evp(nx,ny) ) 
ELSE IF ( ndynami==2 ) THEN
    ALLOCATE( tpglt%jfn(nx,ny) ) 
ENDIF
!
ALLOCATE( tpglt%sit(nt,nx,ny) )
ALLOCATE( tpglt%sil(nl,nt,nx,ny) ) 
ALLOCATE( tpglt%tml(nx,ny) ) 
!
ALLOCATE( tpglt%ust(nx,ny) )
ALLOCATE( tpglt%cdia0(ndiamax,1,1) )
ALLOCATE( tpglt%cdia(ndiamax,nx,ny) )
ALLOCATE( tpglt%blkw(nx,ny) ) 
ALLOCATE( tpglt%blki(nt,nx,ny) ) 
ALLOCATE( tpglt%tfl(nx,ny) ) 
ALLOCATE( tpglt%bud(nx,ny) )
ALLOCATE( tpglt%dia(nx,ny) ) 
!
!
!
! 4. The end
! ===========
!
IF (lp1) THEN
  WRITE(noutlu,*) ' '
  WRITE(noutlu,*) '  ** LEVEL 3 - END SUBROUTINE ALLOC'
  WRITE(noutlu,*) ' '
ENDIF
!
END SUBROUTINE gltools_alloc
!
! ------------------------ END SUBROUTINE gltools_alloc -------------------------
! -----------------------------------------------------------------------
