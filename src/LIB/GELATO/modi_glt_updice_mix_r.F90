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
! ======================== MODULE modi_glt_updice_mix_r =========================
! =======================================================================
!
! Goal:
! -----
!   This module contains a subroutine that manages the ice content
! --------------------- BEGIN MODULE modi_glt_updice_r ----------------------
!THXS_SFX!MODULE modi_glt_updice_mix_r
!THXS_SFX!INTERFACE 
!THXS_SFX!!
!THXS_SFX!SUBROUTINE glt_updice_mix_r  &
!THXS_SFX!  ( kinit,intype,omsg,tpdom,tzsit,psalt_a,pice_a)
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  USE modd_glt_param
!THXS_SFX!  INTEGER, INTENT(in) ::  &
!THXS_SFX!         kinit, intype
!THXS_SFX!  CHARACTER(*), INTENT(in) ::  &
!THXS_SFX!        omsg
!THXS_SFX!  TYPE(t_dom), DIMENSION(np), INTENT(in) ::  &
!THXS_SFX!        tpdom
!THXS_SFX!  TYPE(t_sit), DIMENSION(intype,nt,np), INTENT(in) ::  &
!THXS_SFX!        tzsit
!THXS_SFX!  REAL, INTENT(inout) :: &
!THXS_SFX!        psalt_a,pice_a
!THXS_SFX!END SUBROUTINE glt_updice_mix_r
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_glt_updice_mix_r
!
! --------------------- BEGIN MODULE modi_glt_updice_r ----------------------
!
!
!
! -----------------------------------------------------------------------
! ------------------------ SUBROUTINE glt_updice_r --------------------------
!
! .. Subroutine used to check global water budget.
!
SUBROUTINE glt_updice_mix_r  &
  ( kinit,intype, omsg,tpdom,tzsit,psalt_a,pice_a)
!
  USE modd_types_glt
  USE modd_glt_param
  USE modd_glt_const_thm
  USE mode_glt_stats_r
  USE mode_glt_info_r
!
  IMPLICIT NONE
  INTEGER, INTENT(in) ::  &
         kinit, intype
  CHARACTER(*), INTENT(in) ::  &
        omsg
  TYPE(t_dom), DIMENSION(np), INTENT(in) ::  &
        tpdom
  TYPE(t_sit), DIMENSION(intype,nt,np), INTENT(in) ::  &
        tzsit
  REAL, INTENT(inout) :: &
        psalt_a,pice_a
!
  REAL, DIMENSION(np) :: &
        zice, zsalt
  REAL :: &
       zice_a, zdmice, zdmsalt, zsalt_a
   zice(:) = rhoice*SUM(SUM(&
             tzsit(:,:,:)%fsi*tzsit(:,:,:)%hsi, DIM=2 ), DIM=1 )
   zice_a = glt_avg_r(tpdom, zice(:), 1)
! Mass of salt in ice 
   zsalt(:) = rhoice*SUM(SUM( &
     tzsit(:,:,:)%fsi*tzsit(:,:,:)%hsi*tzsit(:,:,:)%ssi, &
              DIM=2 ), DIM=1 )*1.e-3
   zsalt_a = glt_avg_r(tpdom, zsalt(:), 1)
   IF ( kinit > 0) THEN
     zdmice = ( zice_a - pice_a) / dtt
     zdmsalt = ( zsalt_a - psalt_a) / dtt
     IF (lwg) THEN
       WRITE(noutlu,*)  &
       '--------------------------------------------------------------------'
       WRITE(noutlu,*) omsg,'    Salt Content      :',  zsalt_a
       WRITE(noutlu,*) '    Change in ice mass content  :', zdmice 
       WRITE(noutlu,*) '    Change in Salt content      :', zdmsalt 
     ENDIF
   ELSE
     IF(lwg) WRITE(noutlu,*) omsg,'    Salt Content      :',  zsalt_a
   ENDIF
   psalt_a = zsalt_a
   pice_a = zice_a

END SUBROUTINE glt_updice_mix_r







