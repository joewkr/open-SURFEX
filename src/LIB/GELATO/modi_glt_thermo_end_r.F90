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
! ======================= MODULE modi_glt_thermo_end_r ======================
! =======================================================================
!
!
! * Previous calls to other thermodynamic subroutines may have led to 
! disruptions in ice types_glt classification, for several reasons :
!       - an ice type has grown in thickness, moving to the next class.
!       - an ice type has disappeared totally because of lateral or
!         (and) vertical melting.
!       - sea ice may has grown on an open water surface. 
!
! * The goal of the present subroutine is to assess which ice type(s)
! are now in the different classes. If necessary, merge them, as well
! as associated leads.
!
! Modified : 2007/11 (D. Salas y Melia)
!            thick(jh) < Hsi < thick(jh+1) is not correct: misses cases
!            like Hsi = thick(jh)
!            corrected to: thick(jh) < Hsi <= thick(jh+1)
! Modified : 2009/06 (D. Salas y Melia)
!            Reduced grid version
!
! ------------------- BEGIN MODULE modi_glt_thermo_end_r --------------------

!THXS_SFX!MODULE modi_glt_thermo_end_r
!THXS_SFX!INTERFACE
!THXS_SFX!
!THXS_SFX!SUBROUTINE glt_thermo_end_r( tpdom,tpml,tpldsit,tpldsil,tpsit,tpsil )
!THXS_SFX!!
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  USE modd_glt_param
!THXS_SFX!
!THXS_SFX!! --- INTENT(in) arguments.
!THXS_SFX!  TYPE(t_dom), DIMENSION(np), INTENT(in) ::  &
!THXS_SFX!        tpdom           
!THXS_SFX!  TYPE(t_mxl), DIMENSION(np), INTENT(in) ::  &
!THXS_SFX!        tpml
!THXS_SFX!  TYPE(t_sit), DIMENSION(nt,np), INTENT(in) ::  &
!THXS_SFX!        tpldsit
!THXS_SFX!  TYPE(t_vtp), DIMENSION(nl,nt,np), INTENT(in) ::  &
!THXS_SFX!        tpldsil
!THXS_SFX!        
!THXS_SFX!! --- INTENT(inout) arguments.
!THXS_SFX!  TYPE(t_sit), DIMENSION(nt,np), INTENT(inout) ::  &
!THXS_SFX!        tpsit
!THXS_SFX!  TYPE(t_vtp), DIMENSION(nl,nt,np), INTENT(inout) ::  &
!THXS_SFX!        tpsil
!THXS_SFX!END SUBROUTINE glt_thermo_end_r
!THXS_SFX!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_glt_thermo_end_r

! -------------------- END MODULE modi_glt_thermo_end_r ---------------------



! -----------------------------------------------------------------------
! ----------------------- SUBROUTINE glt_thermo_end_r -----------------------

SUBROUTINE glt_thermo_end_r( tpdom,tpml,tpldsit,tpldsil,tpsit,tpsil )
!
!
! 1. Declarations
! ================
!
! 1.1. Module declarations
! -------------------------
!
  USE modd_types_glt
  USE modd_glt_param
  USE modd_glt_const_thm
  USE modi_gltools_mixice_r
  USE modi_gltools_chkglo_r
!
  IMPLICIT NONE
!
!
! 1.2. Dummy arguments declarations
! ----------------------------------
!
! --- INTENT(in) arguments.
  TYPE(t_dom), DIMENSION(np), INTENT(in) ::  &
        tpdom           
  TYPE(t_mxl), DIMENSION(np), INTENT(in) ::  &
        tpml
  TYPE(t_sit), DIMENSION(nt,np), INTENT(in) ::  &
        tpldsit
  TYPE(t_vtp), DIMENSION(nl,nt,np), INTENT(in) ::  &
        tpldsil
!        
! --- INTENT(inout) arguments.
  TYPE(t_sit), DIMENSION(nt,np), INTENT(inout) ::  &
        tpsit
  TYPE(t_vtp), DIMENSION(nl,nt,np), INTENT(inout) ::  &
        tpsil
!
!
! 1.3. Local variables declarations
! ----------------------------------

  INTEGER ::  &
        it,jp,jh,jk,intype
  INTEGER, DIMENSION(nt,np) ::  &
        inbth_in_cl
  REAL, DIMENSION(np) ::  &
        z2_scell
  TYPE(t_sit), DIMENSION(:,:,:), ALLOCATABLE ::  &
        tzsit
  TYPE(t_vtp), DIMENSION(:,:,:,:), ALLOCATABLE ::  &
        tzsil
!        
!
!
! 2. Initializations
! ===================
!
! 2.1. Welcome message
! ---------------------
!
  IF (lp1) THEN  
    WRITE(noutlu,*) ' '
    WRITE(noutlu,*) '**** LEVEL 4 - SUBROUTINE THERMO_END_R'
    WRITE(noutlu,*) ' '
  ENDIF
!
!
! 2.2. Check in
! --------------
!
  CALL gltools_chkglo_r( 'Before THERMO_END_R',tpdom,tpsit )
!
!
!
! 3. Final computation of thermodynamic variables
! ================================================
!
! 3.1. Merging newly frozen ice with the rest of the ice
! -------------------------------------------------------
!
! .. inbth_in_cl(jk,jp) gives the number of ice types_glt which are now
! gathered in class number jk = ]thick(jk), thickl(jk+1)] at (jp)
! grid point.
!
!  - Initialize inbth_in_cl. 
!
  inbth_in_cl(:,:) = 0
!
! .. Include first ice formed in leads into sea ice types_glt count, then
! sea ice that was already present. 
!
  DO jk = 1,nt
    DO jh = 1,nt
      WHERE (tpldsit(jk,:)%esi .AND. thick(jh)<tpldsit(jk,:)%hsi    &
      .AND. tpldsit(jk,:)%hsi<=thick(jh+1))
        inbth_in_cl(jh,:) = inbth_in_cl(jh,:) + 1 
      ENDWHERE
      WHERE (tpsit(jk,:)%esi .AND. thick(jh)<tpsit(jk,:)%hsi        &
      .AND. tpsit(jk,:)%hsi<=thick(jh+1))
        inbth_in_cl(jh,:) = inbth_in_cl(jh,:) + 1
      ENDWHERE
    END DO
  END DO
!
! .. Determine (on the whole domain) what is the maximum number of sea 
! ice types_glt gathered in the same class.
!
  intype = MAXVAL(inbth_in_cl(:,:))
!
! .. Allocate memory to the arrays that contains all ice types_glt existing 
! on the domain (formatted input to gltools_mixice_r subroutine).
!
  ALLOCATE(tzsit(intype,nt,np))
  ALLOCATE(tzsil(intype,nl,nt,np))
!
! .. Re-initialize inbth_in_cl(:,:) 
!
  inbth_in_cl(:,:) = 0
!
! .. Compute tzsit and tzsil arrays.
! (the following should be improved by the use of pointers).
!
! .. Initialize ice thermodynamics and vertical temperature profile
!
  tzsit(:,:,:)%esi = .FALSE.
  tzsit(:,:,:)%asn = albw
  tzsit(:,:,:)%fsi = 0.
  tzsit(:,:,:)%hsi = 0.
  tzsit(:,:,:)%hsn = 0.
  tzsit(:,:,:)%rsn = rhosnwmax
  tzsit(:,:,:)%tsf = SPREAD(SPREAD(tpml(:)%tml,1,nt),1,intype)
  tzsit(:,:,:)%age = 0.
  tzsit(:,:,:)%ssi = SPREAD(SPREAD(tpml(:)%sml,1,nt),1,intype)
  tzsit(:,:,:)%vmp = 0.
!
! .. Ice vertical gltools_enthalpy profile.
!
  tzsil(:,:,:,:)%ent = -xmhofusn0
!
  DO jp = 1,np
    DO jk = 1,nt
      DO jh = 1,nt
!
! .. Case of new ice that was formed on leads
!
        IF (tpldsit(jk,jp)%esi .AND.  &
        thick(jh)<tpldsit(jk,jp)%hsi .AND.  &
        tpldsit(jk,jp)%hsi<=thick(jh+1)) THEN
!
          inbth_in_cl(jh,jp) = inbth_in_cl(jh,jp) + 1 
          it = inbth_in_cl(jh,jp)
!
          tzsit(it,jh,jp)%esi = tpldsit(jk,jp)%esi
          tzsit(it,jh,jp)%asn = tpldsit(jk,jp)%asn
          tzsit(it,jh,jp)%fsi = tpldsit(jk,jp)%fsi
          tzsit(it,jh,jp)%hsi = tpldsit(jk,jp)%hsi
          tzsit(it,jh,jp)%hsn = tpldsit(jk,jp)%hsn
          tzsit(it,jh,jp)%rsn = tpldsit(jk,jp)%rsn
          tzsit(it,jh,jp)%tsf = tpldsit(jk,jp)%tsf
          tzsil(it,:,jh,jp)%ent = tpldsil(:,jk,jp)%ent
          tzsit(it,jh,jp)%age = tpldsit(jk,jp)%age
          tzsit(it,jh,jp)%ssi = tpldsit(jk,jp)%ssi
          tzsit(it,jh,jp)%vmp = tpldsit(jk,jp)%vmp
        ENDIF
!
! .. Case of 'older' ice
!
        IF (tpsit(jk,jp)%esi .AND.  &
        thick(jh)<tpsit(jk,jp)%hsi .AND.   &
        tpsit(jk,jp)%hsi<=thick(jh+1)) THEN
!
          inbth_in_cl(jh,jp) = inbth_in_cl(jh,jp) + 1
          it = inbth_in_cl(jh,jp)
!
          tzsit(it,jh,jp)%esi = tpsit(jk,jp)%esi
          tzsit(it,jh,jp)%asn = tpsit(jk,jp)%asn
          tzsit(it,jh,jp)%fsi = tpsit(jk,jp)%fsi
          tzsit(it,jh,jp)%hsi = tpsit(jk,jp)%hsi
          tzsit(it,jh,jp)%hsn = tpsit(jk,jp)%hsn
          tzsit(it,jh,jp)%rsn = tpsit(jk,jp)%rsn
          tzsit(it,jh,jp)%tsf = tpsit(jk,jp)%tsf
          tzsil(it,:,jh,jp)%ent = tpsil(:,jk,jp)%ent
          tzsit(it,jh,jp)%age = tpsit(jk,jp)%age
          tzsit(it,jh,jp)%ssi = tpsit(jk,jp)%ssi
          tzsit(it,jh,jp)%vmp = tpsit(jk,jp)%vmp
        ENDIF
      END DO
    END DO
  END DO
!
! .. Mix together all ice types_glt that are gathered in the same class.
!
  CALL gltools_mixice_r(tpml,tzsit,tzsil,tpsit,tpsil)
!
! .. Deallocate auxiliary arrays memory.
!
  DEALLOCATE(tzsil)
  DEALLOCATE(tzsit)
!
!
! 3.2. Increment ice age
! -----------------------
!
  IF ( niceage==1 ) THEN
      WHERE ( tpsit(:,:)%fsi>epsil1 ) 
        tpsit(:,:)%age = tpsit(:,:)%age + dtt
      ENDWHERE
  ENDIF
!
!
! 3.3. Check out
! ---------------
!
  CALL gltools_chkglo_r( 'After THERMO_END_R',tpdom,tpsit )
!
!
! 3.4. Farewell message
! ----------------------
!
  IF (lp1) THEN
    WRITE(noutlu,*) ' '
    WRITE(noutlu,*) '**** LEVEL 4 - END SUBROUTINE THERMO_END_R'
    WRITE(noutlu,*) ' '
  ENDIF
!
END SUBROUTINE glt_thermo_end_r
!                                                                        
! -------------------- END SUBROUTINE glt_thermo_end_r ---------------------- 
! -----------------------------------------------------------------------
