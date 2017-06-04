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
! ====================== MODULE mode_gltools_swfrzt =====================
! =======================================================================
!
!   This module contains functions that allow to know sea water
! freezing temperature as a function of salinity (no pressure term,
! because we only consider surface sea water, for which p=0).
!

! ------------------- BEGIN MODULE mode_gltools_swfrzt --------------------

MODULE mode_gltools_swfrzt
INTERFACE 

FUNCTION glt_swfrzt0d(ps)
  REAL ::                                                               &
    glt_swfrzt0d
  REAL, INTENT(in) ::                                                   &
    ps
END FUNCTION glt_swfrzt0d

FUNCTION glt_swfrzt2d(ps)
  USE modd_glt_param, only:nx,ny
  REAL, DIMENSION(nx,ny) ::                                       &
    glt_swfrzt2d
  REAL, DIMENSION(nx,ny), INTENT(in) ::                           &
    ps
END FUNCTION glt_swfrzt2d

FUNCTION glt_swfrzt3d(ps)
  USE modd_glt_param, only:nt,nx,ny
  REAL, DIMENSION(nt,nx,ny) ::                                    &
    glt_swfrzt3d
  REAL, DIMENSION(nt,nx,ny), INTENT(in) ::                        &
    ps
END FUNCTION glt_swfrzt3d

END INTERFACE
END MODULE mode_gltools_swfrzt

! -------------------- END MODULE mode_gltools_swfrzt ---------------------


! -----------------------------------------------------------------------
! --------------------------- FUNCTION glt_swfrzt0d -------------------------
!
! * glt_swfrzt0d represents surface sea water freezing point temperature,
! (Celsius) a function of salinity (computation is made at only one 
! point here).
!   Units : salinity                    ps in psu units.
!           freezing point temperature  glt_swfrzt0d  degrees celsius
!   Reference : unesco tech. papers in the marine science no 28 1978
!               eigth report jpots
!               annex 6 freezing point of seawater F.J.Millero pp.29-35 

FUNCTION glt_swfrzt0d(ps)
!
  USE modd_glt_const_thm
!
  IMPLICIT NONE

  REAL ::  &
    glt_swfrzt0d
  REAL, INTENT(in) ::  &
    ps

  glt_swfrzt0d = (-0.0575 + 1.710523e-3*sqrt(ps) - 2.154996e-4*ps) * ps
!
END FUNCTION glt_swfrzt0d

! ------------------------ END FUNCTION glt_swfrzt0d ------------------------
! -----------------------------------------------------------------------


! -----------------------------------------------------------------------
! --------------------------- FUNCTION glt_swfrzt2d -------------------------
!
! * glt_swfrzt2d (Celsius) represents surface sea water freezing point
! temperature, a function of salinity. This subroutine handles 2D arrays, 
! unlike glt_swfrzt0d which computes freezing point temperature only for one
! point. 
!   Units : salinity                    ps in psu units.
!           freezing point temperature  gltools_swfrzt  degrees celsius
!   Reference : unesco tech. papers in the marine science no 28 1978
!               eigth report jpots
!               annex 6 freezing point of seawater F.J.Millero pp.29-35 

FUNCTION glt_swfrzt2d(ps)
!
  USE modd_glt_const_thm
  USE modd_glt_param, only:nx,ny
!
  IMPLICIT NONE

  REAL, DIMENSION(nx,ny) ::  &
    glt_swfrzt2d
  REAL, DIMENSION(nx,ny), INTENT(in) ::  &
    ps

  glt_swfrzt2d(:,:) =  &
    (-0.0575 + 1.710523e-3*sqrt(ps(:,:)) - 2.154996e-4*ps(:,:)) *  &
    ps(:,:) 
!
END FUNCTION glt_swfrzt2d

! ------------------------ END FUNCTION glt_swfrzt2d ------------------------
! -----------------------------------------------------------------------


! -----------------------------------------------------------------------
! --------------------------- FUNCTION glt_swfrzt3d -------------------------
!
! * glt_swfrzt3d (Celsius) represents surface sea water freezing point
! temperature, a function of salinity. This subroutine handles 3D arrays,
! unlike, for example glt_swfrzt0d which computes freezing point temperature
! only for one point. 
!   Units : salinity                    ps in psu units.
!           freezing point temperature  glt_swfrzt3d  degrees celsius
!   Reference : unesco tech. papers in the marine science no 28 1978
!               eigth report jpots
!               annex 6 freezing point of seawater F.J.Millero pp.29-35 

FUNCTION glt_swfrzt3d(ps)
!
  USE modd_glt_const_thm
  USE modd_glt_param, only:nt,nx,ny
!
  IMPLICIT NONE

  REAL, DIMENSION(nt,nx,ny) ::  &
    glt_swfrzt3d
  REAL, DIMENSION(nt,nx,ny), INTENT(in) ::  &
    ps

  glt_swfrzt3d(:,:,:) =  &
    (-0.0575 + 1.710523e-3*sqrt(ps(:,:,:)) - 2.154996e-4*ps(:,:,:)) *  &
    ps(:,:,:) 
!
END FUNCTION glt_swfrzt3d

! ------------------------ END FUNCTION glt_swfrzt3d ------------------------
! -----------------------------------------------------------------------
