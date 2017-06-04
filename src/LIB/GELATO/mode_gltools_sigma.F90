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
! ======================= MODULE mode_gltools_sigma =======================
! =======================================================================
!
!   This module contains functions that allow to know sea water density
! as a function of temperature and salinity (polynomial fits)
!   Contains also
!
! Modified : (D. Salas y Melia) 
!   Add a function to compute salinity entrapment following 
!   Cox and Weeks, JGR (1988)
!
! ------------------- BEGIN MODULE mode_gltools_sigma ---------------------

MODULE mode_gltools_sigma
INTERFACE 

FUNCTION glt_sigma(pt,ps)
  USE modd_glt_param, only:nx,ny
  REAL, DIMENSION(nx,ny) ::                                             &
    glt_sigma
  REAL, DIMENSION(nx,ny), INTENT(in) ::                                 &
    pt,ps
END FUNCTION glt_sigma

FUNCTION glt_dsigmadt(pt,ps)
  REAL ::                                                               &
    glt_dsigmadt
  REAL, INTENT(in) ::                                                   &
    pt,ps
END FUNCTION glt_dsigmadt

FUNCTION glt_dsigmads(pt,ps)
  REAL ::                                                               &
    glt_dsigmads
  REAL, INTENT(in) ::                                                   &
    pt,ps
END FUNCTION glt_dsigmads

FUNCTION glt_salfrac( pv )
  REAL ::  &
    glt_salfrac
  REAL, INTENT(in) ::          &! in m.s-1
    pv
END FUNCTION glt_salfrac

END INTERFACE
END MODULE mode_gltools_sigma

! -------------------- END MODULE mode_gltools_sigma ----------------------


! -----------------------------------------------------------------------
! ---------------------------- FUNCTION glt_sigma ---------------------------
!
! * sigma-theta as a function of temp (deg c) and salinity (mil)
! (friedrich-levitus 3rd degree polynomial fit)

FUNCTION glt_sigma(pt,ps)
!
  USE modd_glt_const_thm
  USE modd_glt_param, only:nx,ny
!
  IMPLICIT NONE

  REAL, DIMENSION(nx,ny) ::                                             &
    glt_sigma
  REAL, DIMENSION(nx,ny), INTENT(in) ::                                 &
    pt,ps

  glt_sigma(:,:) =                                                          &
     (c1 + c3 * ps(:,:) + pt(:,:) * (c2 + c5 * ps(:,:) +                &
     pt(:,:) * (c4 + c7 * ps(:,:) + pt(:,:) * c6))) * 1.E-03 
!
END FUNCTION glt_sigma

! ------------------------- END FUNCTION glt_sigma --------------------------
! -----------------------------------------------------------------------


! -----------------------------------------------------------------------
! -------------------------- FUNCTION glt_dsigmadt --------------------------
!
! Computes d(sigma)/dt

FUNCTION glt_dsigmadt(pt,ps)
!
  USE modd_glt_const_thm
!
  IMPLICIT NONE

  REAL ::                                                               &
    glt_dsigmadt
  REAL, INTENT(in) ::                                                   &
    pt,ps

  glt_dsigmadt = (c2 + c5 * ps + 2. * pt * (c4 + c7 * ps +                  &
    1.5 * pt * c6)) * 1.E-3
!
END FUNCTION glt_dsigmadt

! ------------------------ END FUNCTION glt_dsigmadt ------------------------
! -----------------------------------------------------------------------


! -----------------------------------------------------------------------
! -------------------------- FUNCTION glt_dsigmads --------------------------
! 
! Computes d(sigma)/ds

FUNCTION glt_dsigmads(pt,ps)
!
  USE modd_glt_const_thm
!
  IMPLICIT NONE

  REAL ::                                                               &
    glt_dsigmads
  REAL, INTENT(in) ::                                                   &
    pt,ps

  glt_dsigmads = (c3 + pt * (c5 + pt * c7)) * 1.E-3
!
END FUNCTION glt_dsigmads

! ------------------------ END FUNCTION glt_dsigmads ------------------------
! -----------------------------------------------------------------------


! -----------------------------------------------------------------------
! -------------------------- FUNCTION glt_salfrac ---------------------------
! 
! If S is the ocean salinity, as sea ice forms at a rate V (in m.s-1),
! the salinity of the new formed sea ice is : Si = keff.Sw
! Here we follow Cox and Weeks (1988) to compute keff as a function of V.
!
FUNCTION glt_salfrac( pv )
!
  IMPLICIT NONE
  REAL ::  &
    glt_salfrac
  REAL, INTENT(in) ::  &
    pv
  REAL ::  &
    zalpha

!!  IF ( pv > 3.8e-7 ) THEN
!!      glt_salfrac = 0.26 / ( 0.26+0.74*exp( -724300.*pv ) )
!!    ELSE IF ( pv > 3.4e-7 ) THEN
!!      zalpha = ( pv-3.4e-7 ) / 0.4e-7 
!!      glt_salfrac = zalpha * 0.26 / ( 0.26+0.74*exp( -724300.*pv ) ) +  &
!!        ( 1.-zalpha ) * 0.8925 + 0.0568*log( 100.*pv )
!!    ELSE IF ( pv > 2.2e-8 ) THEN
!!      glt_salfrac = 0.8925 + 0.0568*log( 100.*pv )
!!    ELSE IF ( pv > 1.8e-8 ) THEN
!!      zalpha = ( pv-1.8e-8 ) / 0.4e-8 
!!      glt_salfrac = zalpha * ( 0.8925 + 0.0568*log( 100.*pv ) ) +  &
!!        ( 1.-zalpha ) * 0.12
!!    ELSE 
!!      glt_salfrac = 0.12
!!  ENDIF
  IF ( pv > 3.6e-7 ) THEN
      glt_salfrac = 0.26 / ( 0.26+0.74*EXP( -724300.*pv ) )
    ELSE IF ( pv > 1.24e-8 ) THEN
      glt_salfrac = 0.8925 + 0.0568*LOG( 100.*pv )
    ELSE 
      glt_salfrac = 0.12
  ENDIF
!        
END FUNCTION glt_salfrac
!
! ------------------------- END FUNCTION glt_salfrac ------------------------
! -----------------------------------------------------------------------
