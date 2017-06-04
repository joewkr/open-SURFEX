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
! ========================= MODULE mode_glt_stats ===========================
! =======================================================================
!
!
!   This module contains some subroutines that help the icestate user or 
! developper to have better control on model outputs, while it is 
! running.  
!
                                                                        
! --------------------- BEGIN MODULE mode_glt_stats ------------------------- 

MODULE mode_glt_stats 
INTERFACE

FUNCTION glt_get_class(phsi)
  REAL, INTENT(in) ::                                                   &
        phsi
  INTEGER ::                                                            &
        glt_get_class
END FUNCTION glt_get_class 

FUNCTION glt_iceconc(kicell,kjcell,tpsit)
  USE modd_types_glt
  USE modd_glt_param
  INTEGER, INTENT(in) ::                                                &
    kicell,kjcell
  TYPE(t_sit), DIMENSION(nt,nx,ny), INTENT(in) ::                       &
    tpsit
  REAL ::                                                               &
    glt_iceconc
END FUNCTION glt_iceconc

FUNCTION glt_iceconcm(tpdom,tpsit)
  USE modd_types_glt
  USE modd_glt_param
  TYPE(t_dom), DIMENSION(nx,ny), INTENT(in) ::                          &
        tpdom
  TYPE(t_sit), DIMENSION(nt,nx,ny), INTENT(in) ::               &
        tpsit
  REAL, DIMENSION(nx,ny) ::                                             &
        glt_iceconcm 
END FUNCTION glt_iceconcm

FUNCTION glt_thinice_concm(tpdom,tpsit)
  USE modd_types_glt
  USE modd_glt_param
  TYPE(t_dom), DIMENSION(nx,ny), INTENT(in) ::                          &
        tpdom
  TYPE(t_sit), DIMENSION(nt,nx,ny), INTENT(in) ::               &
        tpsit
  REAL, DIMENSION(nx,ny) ::                                             &
        glt_thinice_concm 
END FUNCTION glt_thinice_concm

FUNCTION glt_thickice_concm(tpdom,tpsit)
  USE modd_types_glt
  USE modd_glt_param
  TYPE(t_dom), DIMENSION(nx,ny), INTENT(in) ::                          &
        tpdom
  TYPE(t_sit), DIMENSION(nt,nx,ny), INTENT(in) ::               &
        tpsit
  REAL, DIMENSION(nx,ny) ::                                             &
        glt_thickice_concm 
END FUNCTION glt_thickice_concm

FUNCTION glt_icesurfg(tpdom,tpsit)
  USE modd_types_glt
  USE modd_glt_param
  TYPE(t_dom), DIMENSION(nx,ny), INTENT(in) ::                          &
        tpdom
  TYPE(t_sit), DIMENSION(nt,nx,ny), INTENT(in) ::                       &
        tpsit
  REAL ::                                                               &
        glt_icesurfg
END FUNCTION glt_icesurfg

FUNCTION glt_avg(tpdom,pfield,ktot)
  USE modd_types_glt
  USE modd_glt_param
  TYPE(t_dom), DIMENSION(nx,ny), INTENT(in) ::  &
    tpdom 
  REAL, DIMENSION(nx,ny), INTENT(in) ::  &
    pfield
  INTEGER, INTENT(in) ::  &
    ktot
  REAL ::  &
    glt_avg
END FUNCTION glt_avg

FUNCTION glt_avhice(kicell,kjcell,tpsit)
  USE modd_types_glt
  USE modd_glt_param
  INTEGER, INTENT(in) ::                                                &
    kicell,kjcell
  TYPE(t_sit), DIMENSION(nt,nx,ny), INTENT(in) ::                       &
    tpsit
  REAL ::                                                               &
    glt_avhice 
END FUNCTION glt_avhice 
                                                                        
FUNCTION glt_avhicem(tpdom,tpsit)
  USE modd_types_glt
  USE modd_glt_param
  TYPE(t_dom), DIMENSION(nx,ny), INTENT(in) ::                          &
    tpdom
  TYPE(t_sit), DIMENSION(nt,nx,ny), INTENT(in) ::                       &
    tpsit
  REAL, DIMENSION(nx,ny) ::                                             &
    glt_avhicem
END FUNCTION glt_avhicem

FUNCTION glt_avhsnwm(tpdom,tpsit)
  USE modd_types_glt
  USE modd_glt_param
  TYPE(t_dom), DIMENSION(nx,ny), INTENT(in) ::                          &
    tpdom
  TYPE(t_sit), DIMENSION(nt,nx,ny), INTENT(in) ::                       &
    tpsit
  REAL, DIMENSION(nx,ny) ::                                             &
    glt_avhsnwm
END FUNCTION glt_avhsnwm

FUNCTION glt_avmsnwm(tpdom,tpsit)
  USE modd_types_glt
  USE modd_glt_param
  TYPE(t_dom), DIMENSION(nx,ny), INTENT(in) ::                          &
    tpdom
  TYPE(t_sit), DIMENSION(nt,nx,ny), INTENT(in) ::                       &
    tpsit
  REAL, DIMENSION(nx,ny) ::                                             &
    glt_avmsnwm
END FUNCTION glt_avmsnwm

FUNCTION glt_avhiceg(tpdom,tpsit)
  USE modd_types_glt
  USE modd_glt_param 
  TYPE(t_dom), DIMENSION(nx,ny), INTENT(in) ::                          &
    tpdom
  TYPE(t_sit), DIMENSION(nt,nx,ny), INTENT(in) ::                       &
    tpsit
  REAL ::                                                               &
    glt_avhiceg
END FUNCTION glt_avhiceg

FUNCTION glt_voliceg(tpdom,tpsit)
  USE modd_types_glt
  USE modd_glt_param
  TYPE(t_dom), DIMENSION(nx,ny), INTENT(in) ::  &
        tpdom
  TYPE(t_sit), DIMENSION(nt,nx,ny), INTENT(in) ::  &
        tpsit
  REAL ::  &
        glt_voliceg
END FUNCTION glt_voliceg

FUNCTION glt_vtpint(pz,pvtpo,plevo)
  REAL, INTENT(in) ::  &
    pz
  REAL, DIMENSION(:), INTENT(in) ::  &
    pvtpo
  REAL, DIMENSION(:), INTENT(in) ::  &
    plevo
  REAL ::  &
    glt_vtpint
END FUNCTION glt_vtpint

END INTERFACE
END MODULE mode_glt_stats

! ---------------------- END MODULE mode_glt_stats --------------------------



! -----------------------------------------------------------------------
! ------------------------- FUNCTION glt_get_class --------------------------

!   The input argument is a thickness, expressed in meters. Function
! glt_get_class returns the class to which this thickness belongs.

FUNCTION glt_get_class(phsi)
!
  USE modd_glt_param
!
  IMPLICIT none

  REAL, INTENT(in) ::                                                   &
        phsi
  INTEGER ::                                                            &
        glt_get_class
  INTEGER ::                                                            &
        ji

  ji = 0
  DO ji = 1,nt
    IF (thick(ji) < phsi .AND. phsi <= thick(ji+1)) EXIT
  END DO
  glt_get_class = ji
!
END FUNCTION glt_get_class                                                          

! ----------------------- END FUNCTION glt_get_class ------------------------ 
! -----------------------------------------------------------------------


! -----------------------------------------------------------------------
! ------------------------- FUNCTION glt_iceconc ----------------------------

! Returns the surface of the ice cover on a specified grid cell (a
! percentage given in fractions of unity)

FUNCTION glt_iceconc(kicell,kjcell,tpsit)
!
  USE modd_types_glt
  USE modd_glt_param
!
  IMPLICIT NONE

  INTEGER, INTENT(in) ::                                                &
    kicell,kjcell
  TYPE(t_sit), DIMENSION(nt,nx,ny), INTENT(in) ::                       &
    tpsit
  REAL ::                                                               &
    glt_iceconc 

  glt_iceconc = SUM( tpsit(:,kicell,kjcell)%fsi )
!
END FUNCTION glt_iceconc
                                                                        
! ------------------------ END FUNCTION glt_iceconc ------------------------- 
! -----------------------------------------------------------------------


! -----------------------------------------------------------------------
! ------------------------- FUNCTION glt_iceconcm ---------------------------

! Returns the array of fractional ice coverage on all the cells of 
! the domain. 

FUNCTION glt_iceconcm(tpdom,tpsit)
!
  USE modd_types_glt
  USE modd_glt_param
!
  IMPLICIT NONE

  TYPE(t_dom), DIMENSION(nx,ny), INTENT(in) ::                          &
        tpdom
  TYPE(t_sit), DIMENSION(nt,nx,ny), INTENT(in) ::                       &
        tpsit
  REAL, DIMENSION(nx,ny) ::                                             &
        glt_iceconcm 

! * Compute total sea ice concentration.
  glt_iceconcm(:,:) = SUM( tpsit(:,:,:)%fsi,DIM=1 )
  WHERE (tpdom(:,:)%tmk==0)
    glt_iceconcm(:,:) = 0.
  ENDWHERE
!
END FUNCTION glt_iceconcm
                                                                        
! ------------------------ END FUNCTION glt_iceconcm ------------------------ 
! -----------------------------------------------------------------------


! -----------------------------------------------------------------------
! ----------------------- FUNCTION glt_thinice_concm ------------------------

! Returns the array of fractional ice coverage on all the cells of 
! the domain (ONLY thin ice, thinner than a threshold thickness that
! is given as a namelist parameter).   

FUNCTION glt_thinice_concm(tpdom,tpsit)
!
  USE modd_types_glt
  USE modd_glt_param
!
  IMPLICIT NONE

  TYPE(t_dom), DIMENSION(nx,ny), INTENT(in) ::                          &
        tpdom
  TYPE(t_sit), DIMENSION(nt,nx,ny), INTENT(in) ::               &
        tpsit
  REAL, DIMENSION(nx,ny) ::                                             &
        glt_thinice_concm 

  glt_thinice_concm(:,:) =                                                  &
    SUM(tpsit(:,:,:)%fsi,MASK=(tpsit(:,:,:)%hsi<xicethcr),DIM=1)
  WHERE (tpdom(:,:)%tmk == 0) 
    glt_thinice_concm(:,:) = 0.
  ENDWHERE
!
END FUNCTION glt_thinice_concm
                                                                        
! --------------------- END FUNCTION glt_thinice_concm ---------------------- 
! -----------------------------------------------------------------------


! -----------------------------------------------------------------------
! ----------------------- FUNCTION glt_thickice_concm -----------------------

! Returns the array of fractional ice coverage on all the cells of 
! the domain (ONLY thick ice, thicker than a threshold thickness that
! is given as a namelist parameter : xicethcr).   

FUNCTION glt_thickice_concm(tpdom,tpsit)
!
  USE modd_types_glt
  USE modd_glt_param
!
  IMPLICIT NONE

  TYPE(t_dom), DIMENSION(nx,ny), INTENT(in) ::                          &
        tpdom
  TYPE(t_sit), DIMENSION(nt,nx,ny), INTENT(in) ::               &
        tpsit
  REAL, DIMENSION(nx,ny) ::                                             &
        glt_thickice_concm 

  glt_thickice_concm(:,:) =                                                 &
    SUM(tpsit(:,:,:)%fsi,MASK=(tpsit(:,:,:)%hsi>xicethcr),DIM=1)
  WHERE (tpdom(:,:)%tmk == 0) 
    glt_thickice_concm(:,:) = 0.
  ENDWHERE
!
END FUNCTION glt_thickice_concm
                                                                        
! -------------------- END FUNCTION glt_thickice_concm ---------------------- 
! -----------------------------------------------------------------------


! -----------------------------------------------------------------------
! -------------------------- FUNCTION glt_icesurfg --------------------------

! Returns the total surface of the ice cover in a given region (sq. m) 

FUNCTION glt_icesurfg(tpdom,tpsit)
!
  USE modd_types_glt
  USE modd_glt_param
  USE lib_mpp
!
  IMPLICIT NONE

  TYPE(t_dom), DIMENSION(nx,ny), INTENT(in) ::                          &
        tpdom
  TYPE(t_sit), DIMENSION(nt,nx,ny), INTENT(in) ::                       &
        tpsit
  REAL ::                                                                &
        glt_icesurfg
  REAL, DIMENSION(nx,ny) ::                                             &
        z2_sumfsi

  z2_sumfsi(:,:) = SUM( tpsit(:,:,:)%fsi,DIM=1 )
  WHERE (tpdom(:,:)%tmk==0)
    z2_sumfsi(:,:) = 0.
  ENDWHERE  
  glt_icesurfg = SUM(z2_sumfsi(:,:)*tpdom(:,:)%srf, MASK=(tpdom(:,:)%tmk==1))
!
  CALL mpp_sum( glt_icesurfg )
!
END FUNCTION glt_icesurfg
                                                                        
! ------------------------ END FUNCTION glt_icesurfg ------------------------ 
! -----------------------------------------------------------------------


! -----------------------------------------------------------------------
! --------------------------- FUNCTION glt_avg ------------------------------

! Returns the global average of a 2D array over the entire domain
!  ktot=1 : integral
!  ktot=0 : average

FUNCTION glt_avg(tpdom,pfield,ktot)
!
  USE modd_types_glt
  USE modd_glt_param
  USE lib_mpp
!
  IMPLICIT NONE
!
  TYPE(t_dom), DIMENSION(nx,ny), INTENT(in) ::  &
    tpdom 
  REAL, DIMENSION(nx,ny), INTENT(in) ::  &
    pfield
  INTEGER, INTENT(in) ::  &
    ktot
  REAL ::  &
    glt_avg
!
  REAL ::  &
    zsrf
!
  glt_avg =  &
    SUM( pfield(:,:)*tpdom(:,:)%srf, MASK=(tpdom(:,:)%tmk==1) )
!
  CALL mpp_sum( glt_avg )
!
  IF ( ktot==0 ) THEN
      glt_avg = glt_avg / xdomsrf_g
  ENDIF
!
END FUNCTION glt_avg
                                                                        
! ------------------------- END FUNCTION glt_avg ---------------------------- 
! -----------------------------------------------------------------------


! -----------------------------------------------------------------------
! -------------------------- FUNCTION glt_avhice ----------------------------

! Returns the average ice thickness on a specified grid cell.

FUNCTION glt_avhice(kicell,kjcell,tpsit)
!
  USE modd_types_glt
  USE modd_glt_param
!
  IMPLICIT NONE

  INTEGER, INTENT(in) ::                                                &
    kicell,kjcell
  TYPE(t_sit), DIMENSION(nt,nx,ny), INTENT(in) ::                       &
    tpsit
  REAL ::                                                               &
    glt_avhice

  glt_avhice =                                                              &
    SUM( tpsit(:,kicell,kjcell)%fsi*tpsit(:,kicell,kjcell)%hsi )
!
END FUNCTION glt_avhice
                                                                        
! ------------------------ END FUNCTION glt_avhice -------------------------- 
! -----------------------------------------------------------------------


! -----------------------------------------------------------------------
! ------------------------- FUNCTION glt_avhicem ----------------------------

! Returns the array of average ice thickness on all the cells of 
! the domain. 

FUNCTION glt_avhicem(tpdom,tpsit)
!
  USE modd_types_glt
  USE modd_glt_param
!
  IMPLICIT NONE

  TYPE(t_dom), DIMENSION(nx,ny), INTENT(in) ::                          &
    tpdom 
  TYPE(t_sit), DIMENSION(nt,nx,ny), INTENT(in) ::                       &
    tpsit
  REAL, DIMENSION(nx,ny) ::                                             &
    glt_avhicem

  glt_avhicem(:,:) = SUM( tpsit(:,:,:)%fsi*tpsit(:,:,:)%hsi,DIM=1 )
  WHERE ( tpdom(:,:)%tmk==0 )
    glt_avhicem(:,:) = 0.
  ENDWHERE
!
END FUNCTION glt_avhicem
                                                                        
! ------------------------ END FUNCTION glt_avhicem ------------------------- 
! -----------------------------------------------------------------------


! -----------------------------------------------------------------------
! -------------------------- FUNCTION glt_avhsnwm --------------------------- 

! Returns the array of average snow thickness on all the cells of 
! the domain. 

FUNCTION glt_avhsnwm(tpdom,tpsit)
  USE modd_types_glt
  USE modd_glt_param
!
  IMPLICIT NONE

  TYPE(t_dom), DIMENSION(nx,ny), INTENT(in) ::                          &
    tpdom
  TYPE(t_sit), DIMENSION(nt,nx,ny), INTENT(in) ::                       &
    tpsit
  REAL, DIMENSION(nx,ny) ::                                             &
    glt_avhsnwm

  glt_avhsnwm(:,:) = SUM(tpsit(:,:,:)%fsi*tpsit(:,:,:)%hsn,DIM=1)
  WHERE (tpdom(:,:)%tmk == 0)
    glt_avhsnwm(:,:) = 0.
  ENDWHERE
!
END FUNCTION glt_avhsnwm

! ------------------------ END FUNCTION glt_avhsnwm ------------------------- 
! -----------------------------------------------------------------------


! -----------------------------------------------------------------------
! -------------------------- FUNCTION glt_avmsnwm --------------------------- 

! Returns the array of average snow mass on all the cells of 
! the domain (kg/m2)

FUNCTION glt_avmsnwm(tpdom,tpsit)
  USE modd_types_glt
  USE modd_glt_param
  USE modd_glt_const_thm
!
  IMPLICIT NONE 

  TYPE(t_dom), DIMENSION(nx,ny), INTENT(in) ::  &
    tpdom
  TYPE(t_sit), DIMENSION(nt,nx,ny), INTENT(in) ::  &
    tpsit
  REAL, DIMENSION(nx,ny) ::  &
    glt_avmsnwm

  glt_avmsnwm(:,:) = SUM(  &
    tpsit(:,:,:)%fsi*tpsit(:,:,:)%rsn*tpsit(:,:,:)%hsn, DIM=1 )
!
END FUNCTION glt_avmsnwm

! ------------------------ END FUNCTION glt_avmsnwm ------------------------- 
! -----------------------------------------------------------------------


! -----------------------------------------------------------------------
! -------------------------- FUNCTION glt_avhiceg ---------------------------

! Returns the average ice thickness on the whole domain 

FUNCTION glt_avhiceg(tpdom,tpsit)
!
  USE modd_types_glt
  USE modd_glt_param 
  USE lib_mpp
!
  IMPLICIT NONE

  TYPE(t_dom), DIMENSION(nx,ny), INTENT(in) ::                           &
        tpdom
  TYPE(t_sit), DIMENSION(nt,nx,ny), INTENT(in) ::                       &
        tpsit
  REAL ::                                                               &
        glt_avhiceg
  REAL, DIMENSION(nx,ny) ::                                             &
        z2_avhsi

  z2_avhsi(:,:) =                                                       &
    SUM( tpsit(:,:,:)%fsi*tpsit(:,:,:)%hsi,DIM=1 )
  WHERE (tpdom(:,:)%tmk==0)
    z2_avhsi(:,:) = 0.
  ENDWHERE
  glt_avhiceg =  &
    SUM(z2_avhsi(:,:)*tpdom(:,:)%srf, MASK=(tpdom(:,:)%tmk==1)) 
!
  CALL mpp_sum( glt_avhiceg )
  glt_avhiceg = glt_avhiceg / xdomsrf
!
END FUNCTION glt_avhiceg
                                                                        
! ------------------------ END FUNCTION glt_avhiceg ------------------------- 
! -----------------------------------------------------------------------


! -----------------------------------------------------------------------
! -------------------------- FUNCTION glt_voliceg ---------------------------

! Returns the total volume of ice on the whole region of study, in 
! cubic m.

FUNCTION glt_voliceg(tpdom,tpsit)
!
  USE modd_types_glt
  USE modd_glt_param
  USE lib_mpp
!
  IMPLICIT NONE

  TYPE(t_dom), DIMENSION(nx,ny), INTENT(in) ::                          &
        tpdom
  TYPE(t_sit), DIMENSION(nt,nx,ny), INTENT(in) ::                              &
        tpsit
  REAL ::                                                               &
        glt_voliceg
  REAL, DIMENSION(nx,ny) ::                                             &
        z2_avhsi

  z2_avhsi(:,:) = SUM( tpsit(:,:,:)%fsi*tpsit(:,:,:)%hsi,DIM=1 )
  WHERE (tpdom(:,:)%tmk==0)
    z2_avhsi(:,:) = 0.
  ENDWHERE
  glt_voliceg = SUM(z2_avhsi(:,:)*tpdom(:,:)%srf, MASK=(tpdom(:,:)%tmk==1))
!
  CALL mpp_sum( glt_voliceg )
!
END FUNCTION glt_voliceg
!                                                                        
! ------------------------ END FUNCTION glt_voliceg ------------------------- 
! -----------------------------------------------------------------------
!
!
! -----------------------------------------------------------------------
! --------------------------- FUNCTION glt_vtpint ---------------------------
!
! * If pvtpo (old vertical tracer profile) has dimension (n), 
! plevo (old, normalized levels: bottom=0, top=1) should have 
! dimension (n+1)
! * Output: integral between z=0 and z=pz of vertical tracer.
!
FUNCTION glt_vtpint(pz,pvtpo,plevo)
!
  IMPLICIT NONE
!
  REAL, INTENT(in) ::  &
    pz
  REAL, DIMENSION(:), INTENT(in) ::  &
    pvtpo
  REAL, DIMENSION(:), INTENT(in) ::  &
    plevo
  REAL ::  &
    glt_vtpint
!
  INTEGER :: jl
!
  glt_vtpint = 0.
!
  DO jl=1,SIZE(pvtpo)
    glt_vtpint = glt_vtpint +  &
      pvtpo(jl)*  &
      ( ( AMIN1(pz,plevo(jl+1))-plevo(jl)) *  &
      ( 1.+SIGN(1.,pz-plevo(jl)) )/2. )
  END DO
!
END FUNCTION glt_vtpint
!
! ------------------------- END FUNCTION glt_vtpint -------------------------
! -----------------------------------------------------------------------
