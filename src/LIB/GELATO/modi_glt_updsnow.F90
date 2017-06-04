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
! ======================== MODULE modi_glt_updsnow =========================
! =======================================================================
!
! Goal:
! -----
!   This module contains a subroutine that manages the snow content
! --------------------- BEGIN MODULE modi_glt_updsnow ----------------------
!THXS_SFX!MODULE modi_glt_updsnow
!THXS_SFX!INTERFACE 
!THXS_SFX!!
!THXS_SFX!SUBROUTINE glt_updsnow  &
!THXS_SFX!  ( kinit,omsg,tpdom,tptfl,tpsit,psnow_a,pemp_a,paddterm,paddterm2)
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  USE modd_glt_param
!THXS_SFX!  INTEGER, INTENT(in) ::  &
!THXS_SFX!         kinit
!THXS_SFX!  CHARACTER(*), INTENT(in) ::  &
!THXS_SFX!        omsg
!THXS_SFX!  TYPE(t_dom), DIMENSION(nx,ny), INTENT(in) ::  &
!THXS_SFX!        tpdom
!THXS_SFX!  TYPE(t_tfl), DIMENSION(nx,ny), INTENT(in) ::  &
!THXS_SFX!        tptfl
!THXS_SFX!  TYPE(t_sit), DIMENSION(nt,nx,ny), INTENT(in) ::  &
!THXS_SFX!        tpsit
!THXS_SFX!  REAL, INTENT(inout) :: &
!THXS_SFX!        pemp_a, psnow_a
!THXS_SFX!  REAL, DIMENSION(nx,ny), INTENT(in), OPTIONAL :: &
!THXS_SFX!        paddterm,paddterm2
!THXS_SFX!END SUBROUTINE glt_updsnow
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_glt_updsnow
!
! --------------------- BEGIN MODULE modi_glt_updsnow ----------------------
!
!
!
! -----------------------------------------------------------------------
! ------------------------ SUBROUTINE glt_updsnow --------------------------
!
! .. Subroutine used to check global water budget.
!
SUBROUTINE glt_updsnow  &
  ( kinit,omsg,tpdom,tptfl,tpsit,psnow_a,pemp_a,paddterm,paddterm2)
!
  USE modd_types_glt
  USE modd_glt_param
  USE modd_glt_const_thm
  USE mode_glt_stats
  USE mode_glt_info
!
  IMPLICIT NONE
  INTEGER, INTENT(in) ::  &
         kinit
  CHARACTER(*), INTENT(in) ::  &
        omsg
  TYPE(t_dom), DIMENSION(nx,ny), INTENT(in) ::  &
        tpdom
  TYPE(t_tfl), DIMENSION(nx,ny), INTENT(in) ::  &
        tptfl
  TYPE(t_sit), DIMENSION(nt,nx,ny), INTENT(in) ::  &
        tpsit
  REAL, INTENT(inout) :: &
        pemp_a, psnow_a
  REAL, DIMENSION(nx,ny), INTENT(in), OPTIONAL :: &
        paddterm,paddterm2
!
  REAL, DIMENSION(nx,ny) :: &
        zsnow, zemp
  REAL :: &
        zsnow_a, zemp_a, zdemp, zdmsnow, zaddterm_a
!
!
!
   zsnow(:,:) = SUM( tpsit(:,:,:)%fsi*tpsit(:,:,:)%rsn*tpsit(:,:,:)%hsn, DIM=1 )
   zsnow_a = glt_avg(tpdom, zsnow(:,:), 1)
   zemp(:,:) = tptfl(:,:)%wlo
   zemp_a = glt_avg(tpdom, zemp(:,:), 1)
   IF ( kinit > 0) THEN
     zdmsnow = ( zsnow_a - psnow_a) / dtt
     zdemp = zemp_a - pemp_a 
     IF (lwg) THEN
       WRITE(noutlu,*)  &
       '--------------------------------------------------------------------'
       WRITE(noutlu,*) omsg  ,'    Snow Content      :',  zsnow_a
       WRITE(noutlu,*) '    Change in snow content      :', zdmsnow 
       WRITE(noutlu,*) '    Change in emp               :', zdemp
       WRITE(noutlu,*) '    BILAN DMSNOW-EMP            :', zdmsnow + zdemp
     ENDIF
     IF ( PRESENT(paddterm) .AND. PRESENT(paddterm2) ) THEN
        zaddterm_a = glt_avg(tpdom, paddterm(:,:), 1)
        IF(lwg) WRITE(noutlu,*) '    Bilan snow content         :', zdmsnow + zaddterm_a 
        zaddterm_a = glt_avg(tpdom, paddterm2(:,:), 1)
        IF(lwg) WRITE(noutlu,*) '    Bilan emp                  :', zdemp + zaddterm_a 
     ELSE IF ( PRESENT(paddterm) ) THEN
        zaddterm_a = glt_avg(tpdom, paddterm(:,:), 1)
        IF (lwg) THEN
          WRITE(noutlu,*) '    Terme additif            :', zaddterm_a
          WRITE(noutlu,*) '   BILAN DMSNOW-EMP+terme additif  :', zdmsnow + zdemp + zaddterm_a
        ENDIF
     ENDIF
   ENDIF
   pemp_a = zemp_a
   psnow_a = zsnow_a

END SUBROUTINE glt_updsnow







