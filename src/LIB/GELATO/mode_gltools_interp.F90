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
! ======================== MODULE mode_gltools_interp =====================
! =======================================================================
!
!   
! -------------------- BEGIN MODULE mode_gltools_interp -------------------
!
MODULE mode_gltools_interp
INTERFACE
!
FUNCTION glt_interpz(plevn,pvtpo,plevo) RESULT(tab_interp)
  USE modd_glt_param, only : nilay
  REAL, DIMENSION(nilay+1), INTENT(in) ::  &
    plevn
  REAL, DIMENSION(:), INTENT(in) ::  &
    pvtpo
  REAL, DIMENSION(:), INTENT(in) ::  &
    plevo
  REAL, DIMENSION(nilay) ::  &
    tab_interp
END FUNCTION glt_interpz
!
#if ! defined in_surfex
SUBROUTINE glt_c2b(pcu,pcv,pbu,pbv)
USE modd_glt_param
REAL, DIMENSION(nx,ny),INTENT(in) ::  &
  pcu,pcv
REAL, DIMENSION(nx,ny),INTENT(out) ::  &
  pbu,pbv
END SUBROUTINE glt_c2b
!
SUBROUTINE glt_b2c(pbu,pbv,pcu,pcv)
USE modd_glt_param
REAL, DIMENSION(nx,ny),INTENT(in) ::  &
  pbu,pbv
REAL, DIMENSION(ilo:ihi,jlo:jhi), INTENT(out) ::  &
  pcu,pcv
END SUBROUTINE glt_b2c
!
#endif
END INTERFACE
END MODULE mode_gltools_interp
!
! -------------------- END MODULE mode_gltools_interp ---------------------
!
!
! -----------------------------------------------------------------------
! --------------------------- FUNCTION glt_interpz --------------------------
!
! Goal: Interpolate a vertical tracer profile, pvtpo (dimension n),
! defined on a vertical, normalized grid :
!   [ plevo(1)=1, plevo(2), ..., plevo(n), plevo(n+1)=1 ],
! where plevo(jl),plevo(jl+1) define the height (from ice/water bottom
! interface) of respectively the lower and upper boundaries of layer jl.
! Note that n can be any number.
! 
! The glt_output is delivered on the model's standard vertical levels.
!
FUNCTION glt_interpz(plevn,pvtpo,plevo) RESULT(tab_interp)
!
  USE modd_glt_param , only: nilay
  USE mode_glt_stats
!
  IMPLICIT NONE
!
  REAL, DIMENSION(nilay+1), INTENT(in) ::  &
    plevn
  REAL, DIMENSION(:), INTENT(in) ::  &
    pvtpo
  REAL, DIMENSION(:), INTENT(in) ::  &
    plevo
  REAL, DIMENSION(nilay) ::  &
    tab_interp
!
  INTEGER :: jl
!
  DO jl=1,nilay
    tab_interp(jl) =  &
      ( glt_vtpint(plevn(jl+1),pvtpo,plevo)-  &
        glt_vtpint(plevn(jl),pvtpo,plevo) ) /  &
      ( plevn(jl+1)-plevn(jl) )
  END DO
!
END FUNCTION glt_interpz
!
! ------------------------- END FUNCTION glt_interpz ------------------------
! -----------------------------------------------------------------------
!
#if ! defined in_surfex
!
! ----------------------------------------------------------------------
! --------------------------- SUBROUTINE glt_c2b ---------------------------
!
! .. Interpolates a vector field defined by two components on a C-grid
! to the EVP grid (both components defined at the top right corner of
! the grid cell). Note the interpolated fields must be bounded 
! afterwards.
!
SUBROUTINE glt_c2b(pcu,pcv,pbu,pbv)
!
USE modd_glt_param
!
IMPLICIT NONE
!
REAL, DIMENSION(nx,ny),INTENT(in) ::  &
  pcu,pcv
REAL, DIMENSION(nx,ny),INTENT(out) ::  &
  pbu,pbv
INTEGER ::  &
  ji,jj
!
!
!
! 1. Initializations
! ==================
!
  pbu(:,:) = 0.
  pbv(:,:) = 0.
!
!
!
! 2. Interpolate
! ===============
!
! Here it is assumed that the input C-grid velocities (u,v) are 
! respectively defined at the middle of the East and North edges
! of the grid cell
! B-grid velocities (u,v) are both defined at the northeast corner
! of the grid cell
!
  DO jj = 2,ny-1
    DO ji = 2,nx/2-1
      pbu(ji,jj) = .5*( pcu(ji,jj)+pcu(ji,jj+1) ) 
      pbv(ji,jj) = .5*( pcv(ji,jj)+pcv(ji+1,jj) )
    END DO
! Not sure that a special operation has to be done for ji=nx/2
    pbu(nx/2,jj) = .5*( pcu(nx/2,jj)+pcu(nx/2,jj+1) ) 
    pbv(nx/2,jj) = .5*( pcv(nx/2,jj)+pcv(nx/2+1,jj) ) 
    DO ji = nx/2+1,nx-1
      pbu(ji,jj) = .5*( pcu(ji,jj)+pcu(ji,jj+1) ) 
      pbv(ji,jj) = .5*( pcv(ji,jj)+pcv(ji+1,jj) )
    END DO
  END DO
!
END SUBROUTINE glt_c2b
!
! ------------------------ END SUBROUTINE glt_c2b --------------------------
! ----------------------------------------------------------------------
!
!
! ----------------------------------------------------------------------
! --------------------------- SUBROUTINE glt_b2c ---------------------------
!
! .. Interpolates a vector field defined by two components on a B-grid
! (or EVP grid, both components defined at the top right corner of
! the grid cell) to the C-grid (OPA type: u and v are respectively 
! defined at the middle of the Eastern and Northern edges of the grid 
! cell.
!
SUBROUTINE glt_b2c(pbu,pbv,pcu,pcv)
!
USE modd_glt_param
USE mode_gltools_bound
!
IMPLICIT NONE
!
REAL, DIMENSION(nx,ny),INTENT(in) ::  &
  pbu,pbv
REAL, DIMENSION(ilo:ihi,jlo:jhi), INTENT(out) ::  &
  pcu,pcv
!
INTEGER ::  &
  ji,jj
!
!
!
! 1.1. Define ancillary arrays
! -----------------------------
!
! ...
!
!
! 1.2. Compute velocity components
! ---------------------------------
!
  DO jj=jlo,jhi
    DO ji=ilo,ihi
      pcu(ji,jj) = 0.5*( pbu(ji,jj)+pbu(ji,jj-1) )
      pcv(ji,jj) = 0.5*( pbv(ji,jj)+pbv(ji-1,jj) )
    END DO
  END DO 
!
END SUBROUTINE glt_b2c
!
! ------------------------ END SUBROUTINE glt_b2c --------------------------
! ----------------------------------------------------------------------
#endif
