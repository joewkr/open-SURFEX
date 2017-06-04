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
! ======================== MODULE mode_glt_stats_r ==========================
! =======================================================================
!
!
!   This module contains some subroutines that help the icestate user or 
! developper to have better control on model outputs, while it is 
! running.  
!   Note: this is the reduced grid version.
!
! Created : 2009/06 
!
! -------------------- BEGIN MODULE mode_glt_stats_r ------------------------ 

MODULE mode_glt_stats_r 
INTERFACE

FUNCTION glt_iceconcm_r(tpsit)
  USE modd_types_glt
  USE modd_glt_param
  TYPE(t_sit), DIMENSION(nt,np), INTENT(in) ::               &
        tpsit
  REAL, DIMENSION(np) ::                                             &
        glt_iceconcm_r 
END FUNCTION glt_iceconcm_r

FUNCTION glt_thinice_concm_r(tpsit)
  USE modd_types_glt
  USE modd_glt_param
  TYPE(t_sit), DIMENSION(nt,np), INTENT(in) ::               &
        tpsit
  REAL, DIMENSION(np) ::                                             &
        glt_thinice_concm_r
END FUNCTION glt_thinice_concm_r

FUNCTION glt_thickice_concm_r(tpsit)
  USE modd_types_glt
  USE modd_glt_param
  TYPE(t_sit), DIMENSION(nt,np), INTENT(in) ::               &
        tpsit
  REAL, DIMENSION(np) ::                                             &
        glt_thickice_concm_r 
END FUNCTION glt_thickice_concm_r

FUNCTION glt_icesurfg_r(tpdom,tpsit)
  USE modd_types_glt
  USE modd_glt_param
  TYPE(t_dom), DIMENSION(np), INTENT(in) ::                          &
        tpdom
  TYPE(t_sit), DIMENSION(nt,np), INTENT(in) ::                       &
        tpsit
  REAL ::                                                               &
        glt_icesurfg_r
END FUNCTION glt_icesurfg_r

FUNCTION glt_avg_r(tpdom,pfield,ktot)
  USE modd_types_glt
  USE modd_glt_param
  TYPE(t_dom), DIMENSION(np), INTENT(in) ::  &
    tpdom 
  REAL, DIMENSION(np), INTENT(in) ::  &
    pfield
  INTEGER, INTENT(in) ::  &
    ktot
  REAL ::  &
    glt_avg_r
END FUNCTION glt_avg_r

FUNCTION glt_avhicem_r(tpsit)
  USE modd_types_glt
  USE modd_glt_param
  TYPE(t_sit), DIMENSION(nt,np), INTENT(in) ::                       &
    tpsit
  REAL, DIMENSION(np) ::                                             &
    glt_avhicem_r
END FUNCTION glt_avhicem_r

FUNCTION glt_avhsnwm_r(tpsit)
  USE modd_types_glt
  USE modd_glt_param
  TYPE(t_sit), DIMENSION(nt,np), INTENT(in) ::                       &
    tpsit
  REAL, DIMENSION(np) ::                                             &
    glt_avhsnwm_r
END FUNCTION glt_avhsnwm_r

FUNCTION glt_avmsnwm_r(tpsit)
  USE modd_types_glt
  USE modd_glt_param
  TYPE(t_sit), DIMENSION(nt,np), INTENT(in) ::                       &
    tpsit
  REAL, DIMENSION(np) ::                                             &
    glt_avmsnwm_r
END FUNCTION glt_avmsnwm_r

FUNCTION glt_avhiceg_r(tpdom,tpsit)
  USE modd_types_glt
  USE modd_glt_param 
  TYPE(t_dom), DIMENSION(np), INTENT(in) ::                          &
    tpdom
  TYPE(t_sit), DIMENSION(nt,np), INTENT(in) ::                       &
    tpsit
  REAL ::                                                               &
    glt_avhiceg_r
END FUNCTION glt_avhiceg_r

FUNCTION glt_voliceg_r(tpdom,tpsit)
  USE modd_types_glt
  USE modd_glt_param
  TYPE(t_dom), DIMENSION(np), INTENT(in) ::  &
        tpdom
  TYPE(t_sit), DIMENSION(nt,np), INTENT(in) ::  &
        tpsit
  REAL ::  &
        glt_voliceg_r
END FUNCTION glt_voliceg_r

END INTERFACE
END MODULE mode_glt_stats_r

! --------------------- END MODULE mode_glt_stats_r -------------------------


! -----------------------------------------------------------------------
! ------------------------ FUNCTION glt_iceconcm_r --------------------------

! Returns the array of fractional ice coverage on all the cells of 
! the domain. 

FUNCTION glt_iceconcm_r(tpsit)
!
  USE modd_types_glt
  USE modd_glt_param
!
  IMPLICIT NONE

  TYPE(t_sit), DIMENSION(nt,np), INTENT(in) ::                       &
        tpsit
  REAL, DIMENSION(np) ::                                             &
        glt_iceconcm_r 

! * Compute total sea ice concentration.
  glt_iceconcm_r(:) = SUM( tpsit(:,:)%fsi,DIM=1 )
!
END FUNCTION glt_iceconcm_r
                                                                        
! ----------------------- END FUNCTION glt_iceconcm_r ----------------------- 
! -----------------------------------------------------------------------


! -----------------------------------------------------------------------
! ---------------------- FUNCTION glt_thinice_concm_r -----------------------

! Returns the array of fractional ice coverage on all the cells of 
! the domain (ONLY thin ice, thinner than a threshold thickness that
! is given as a namelist parameter).   

FUNCTION glt_thinice_concm_r(tpsit)
!
  USE modd_types_glt
  USE modd_glt_param
!
  IMPLICIT NONE

  TYPE(t_sit), DIMENSION(nt,np), INTENT(in) ::               &
        tpsit
  REAL, DIMENSION(np) ::                                             &
        glt_thinice_concm_r 

  glt_thinice_concm_r(:) =                                                  &
    SUM(tpsit(:,:)%fsi,MASK=(tpsit(:,:)%hsi<xicethcr),DIM=1)
!
END FUNCTION glt_thinice_concm_r
                                                                        
! -------------------- END FUNCTION glt_thinice_concm_r --------------------- 
! -----------------------------------------------------------------------


! -----------------------------------------------------------------------
! ---------------------- FUNCTION glt_thickice_concm_r ----------------------

! Returns the array of fractional ice coverage on all the cells of 
! the domain (ONLY thick ice, thicker than a threshold thickness that
! is given as a namelist parameter : xicethcr).   

FUNCTION glt_thickice_concm_r(tpsit)
!
  USE modd_types_glt
  USE modd_glt_param
!
  IMPLICIT NONE

  TYPE(t_sit), DIMENSION(nt,np), INTENT(in) ::               &
        tpsit
  REAL, DIMENSION(np) ::                                             &
        glt_thickice_concm_r 

  glt_thickice_concm_r(:) =                                                 &
    SUM(tpsit(:,:)%fsi,MASK=(tpsit(:,:)%hsi>xicethcr),DIM=1)
!
END FUNCTION glt_thickice_concm_r
                                                                        
! ------------------- END FUNCTION glt_thickice_concm_r --------------------- 
! -----------------------------------------------------------------------


! -----------------------------------------------------------------------
! ------------------------- FUNCTION glt_icesurfg_r -------------------------

! Returns the total surface of the ice cover in a given region (sq. m) 

FUNCTION glt_icesurfg_r(tpdom,tpsit)
!
  USE modd_types_glt
  USE modd_glt_param
!
  IMPLICIT NONE

  TYPE(t_dom), DIMENSION(np), INTENT(in) ::                          &
        tpdom
  TYPE(t_sit), DIMENSION(nt,np), INTENT(in) ::                       &
        tpsit
  REAL ::                                                                &
        glt_icesurfg_r
  REAL, DIMENSION(np) ::                                             &
        z2_sumfsi

  z2_sumfsi(:) = SUM( tpsit(:,:)%fsi,DIM=1 )
  glt_icesurfg_r = SUM( z2_sumfsi(:)*tpdom(:)%srf )
!
END FUNCTION glt_icesurfg_r
                                                                        
! ----------------------- END FUNCTION glt_icesurfg_r ----------------------- 
! -----------------------------------------------------------------------


! -----------------------------------------------------------------------
! -------------------------- FUNCTION glt_avg_r -----------------------------

! Returns the global average of a 2D array over the entire domain
!  ktot=1 : integral
!  ktot=0 : average

FUNCTION glt_avg_r(tpdom,pfield,ktot)
!
  USE modd_types_glt
  USE modd_glt_param
  USE modd_glt_const_thm
!
  IMPLICIT NONE
!
  TYPE(t_dom), DIMENSION(np), INTENT(in) ::  &
    tpdom 
  REAL, DIMENSION(np), INTENT(in) ::  &
    pfield
  INTEGER, INTENT(in) ::  &
    ktot
  REAL ::  &
    glt_avg_r
!
  glt_avg_r = SUM( pfield(:)*tpdom(:)%srf )
!
  IF ( ktot==0 ) THEN
      glt_avg_r = glt_avg_r / AMAX1(xdomsrf_r,epsil2)
  ENDIF
!
END FUNCTION glt_avg_r
                                                                        
! ------------------------ END FUNCTION glt_avg_r --------------------------- 
! -----------------------------------------------------------------------


! -----------------------------------------------------------------------
! ------------------------ FUNCTION glt_avhicem_r ---------------------------

! Returns the array of average ice thickness on all the cells of 
! the domain. 

FUNCTION glt_avhicem_r(tpsit)
!
  USE modd_types_glt
  USE modd_glt_param
!
  IMPLICIT NONE

  TYPE(t_sit), DIMENSION(nt,np), INTENT(in) ::                       &
    tpsit
  REAL, DIMENSION(np) ::                                             &
    glt_avhicem_r

  glt_avhicem_r(:) = SUM( tpsit(:,:)%fsi*tpsit(:,:)%hsi,DIM=1 )
!
END FUNCTION glt_avhicem_r
                                                                        
! ----------------------- END FUNCTION glt_avhicem_r ------------------------ 
! -----------------------------------------------------------------------


! -----------------------------------------------------------------------
! ------------------------- FUNCTION glt_avhsnwm_r -------------------------- 

! Returns the array of average snow thickness on all the cells of 
! the domain. 

FUNCTION glt_avhsnwm_r(tpsit)
  USE modd_types_glt
  USE modd_glt_param
!
  IMPLICIT NONE

  TYPE(t_sit), DIMENSION(nt,np), INTENT(in) ::                       &
    tpsit
  REAL, DIMENSION(np) ::                                             &
    glt_avhsnwm_r

  glt_avhsnwm_r(:) = SUM(tpsit(:,:)%fsi*tpsit(:,:)%hsn,DIM=1)
!
END FUNCTION glt_avhsnwm_r

! ----------------------- END FUNCTION glt_avhsnwm_r ------------------------ 
! -----------------------------------------------------------------------


! -----------------------------------------------------------------------
! ------------------------- FUNCTION glt_avmsnwm_r -------------------------- 

! Returns the array of average snow mass on all the cells of 
! the domain (kg/m2)

FUNCTION glt_avmsnwm_r(tpsit)
  USE modd_types_glt
  USE modd_glt_param
  USE modd_glt_const_thm
!
  IMPLICIT NONE 

  TYPE(t_sit), DIMENSION(nt,np), INTENT(in) ::  &
    tpsit
  REAL, DIMENSION(np) ::  &
    glt_avmsnwm_r

  glt_avmsnwm_r(:) = SUM(  &
    tpsit(:,:)%fsi*tpsit(:,:)%rsn*tpsit(:,:)%hsn, DIM=1 )
!
END FUNCTION glt_avmsnwm_r

! ----------------------- END FUNCTION glt_avmsnwm_r ------------------------ 
! -----------------------------------------------------------------------


! -----------------------------------------------------------------------
! ------------------------- FUNCTION glt_avhiceg_r --------------------------

! Returns the average ice thickness on the whole domain 

FUNCTION glt_avhiceg_r(tpdom,tpsit)
!
  USE modd_types_glt
  USE modd_glt_param 
!
  IMPLICIT NONE

  TYPE(t_dom), DIMENSION(np), INTENT(in) ::                           &
        tpdom
  TYPE(t_sit), DIMENSION(nt,np), INTENT(in) ::                       &
        tpsit
  REAL ::                                                               &
        glt_avhiceg_r
  REAL, DIMENSION(np) ::                                             &
        z2_avhsi

  z2_avhsi(:) =                                                       &
    SUM( tpsit(:,:)%fsi*tpsit(:,:)%hsi,DIM=1 )
  glt_avhiceg_r = SUM( z2_avhsi(:)*tpdom(:)%srf ) / xdomsrf
!
END FUNCTION glt_avhiceg_r
                                                                        
! ----------------------- END FUNCTION glt_avhiceg_r ------------------------ 
! -----------------------------------------------------------------------


! -----------------------------------------------------------------------
! ------------------------- FUNCTION glt_voliceg_r --------------------------

! Returns the total volume of ice on the whole region of study, in 
! cubic m.

FUNCTION glt_voliceg_r(tpdom,tpsit)
!
  USE modd_types_glt
  USE modd_glt_param
!
  IMPLICIT NONE

  TYPE(t_dom), DIMENSION(np), INTENT(in) ::                          &
        tpdom
  TYPE(t_sit), DIMENSION(nt,np), INTENT(in) ::                              &
        tpsit
  REAL ::                                                               &
        glt_voliceg_r
  REAL, DIMENSION(np) ::                                             &
        z2_avhsi

  z2_avhsi(:) = SUM( tpsit(:,:)%fsi*tpsit(:,:)%hsi,DIM=1 )
  glt_voliceg_r = SUM( z2_avhsi(:)*tpdom(:)%srf )
!
END FUNCTION glt_voliceg_r
                                                                        
! ----------------------- END FUNCTION glt_voliceg_r ------------------------ 
! -----------------------------------------------------------------------
