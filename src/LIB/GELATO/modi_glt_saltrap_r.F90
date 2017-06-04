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
! ======================== MODULE modi_glt_saltrap_r ========================
! =======================================================================
!
! Goal:
! -----
!   This module allows to compute initial sea ice salinity as a function
! of the input heat flux.
!   
! Method:
! -------
!   From an initial guess of salinity, we compute gltools_enthalpy of the newly
! formed ice, then deduce the thickness variation during the time-step
! (i.e. accretion rate in m.s-1).
!   From the accretion rate, we compute the brine fraction that is 
! retained by the new ice, following Cox and Weeks, JGR (1988), and 
! iteration until convergence is effective (in many cases, much less
! than 10 iterations)
!
! Inputs:
! -------
!  - a boolean indicating where the marine surface is at freezing point
!  (gfreeze)
!  - a heat flux, that will freeze new ice (phef)
!  - the surface marine conditions (tpmxl)
!
! Outputs:
! --------
!  - new sea ice salinity (psalt)
!  - new sea ice gltools_enthalpy (pent)
!  - new sea ice thickness (phsi)
!
! Created : 2010/03 (D. Salas y Melia)
!
! -------------------- BEGIN MODULE modi_glt_saltrap_r ----------------------
!
!THXS_SFX!MODULE modi_glt_saltrap_r
!THXS_SFX!INTERFACE 
!THXS_SFX!!
!THXS_SFX!SUBROUTINE glt_saltrap_r  &
!THXS_SFX!  ( gfreeze,phef,ptem,tpmxl,psalt,pent,phsi )
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  USE modd_glt_param
!THXS_SFX!  LOGICAL, DIMENSION(np), INTENT(in) ::  &
!THXS_SFX!    gfreeze
!THXS_SFX!  REAL, DIMENSION(np), INTENT(in) ::  &
!THXS_SFX!    phef,ptem 
!THXS_SFX!  TYPE(t_mxl), DIMENSION(np), INTENT(in) ::  &
!THXS_SFX!    tpmxl
!THXS_SFX!  REAL, DIMENSION(np), INTENT(inout) ::  &
!THXS_SFX!    psalt,pent,phsi
!THXS_SFX!END SUBROUTINE glt_saltrap_r
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_glt_saltrap_r
!
! --------------------- END MODULE modi_glt_saltrap_r -----------------------
!
!
!
! -----------------------------------------------------------------------
! ----------------------- SUBROUTINE glt_saltrap_r --------------------------
!
SUBROUTINE glt_saltrap_r  &
  ( gfreeze,phef,ptem,tpmxl,psalt,pent,phsi )
!
  USE modd_types_glt
  USE modd_glt_param
  USE modd_glt_const_thm
  USE mode_gltools_enthalpy
  USE mode_gltools_sigma
!
  IMPLICIT NONE
!
  LOGICAL, DIMENSION(np), INTENT(in) ::  &
    gfreeze
  REAL, DIMENSION(np), INTENT(in) ::  &
    phef,ptem
  TYPE(t_mxl), DIMENSION(np), INTENT(in) ::  &
    tpmxl
  REAL, DIMENSION(np), INTENT(inout) ::  &
    psalt,pent,phsi
!
  INTEGER, PARAMETER ::  &
    nit=100               ! Maximum number of iterations
  REAL, PARAMETER ::  &
    ppssinew=10.          ! First guess for new sea ice salinity
  REAL, PARAMETER ::  &
    ppsmax=16.            ! Maximum sea ice salinity
  REAL, PARAMETER ::  &
    ppdssi=0.1            ! Tolerance on sea ice salinity convergence
  INTEGER ::  &
    jit,jp
real:: x
  LOGICAL, DIMENSION(np) ::  &
    ycont 
  REAL, DIMENSION(np) ::  &
    psaltb 
!
!
!
! 1. Initialize the algorithm
! ============================
!
! .. Points where at least one more iteration is needed
!
  ycont(:) = ( gfreeze(:) .AND. phef(:)<0. )
!
! .. Initial salinity guess 
!
  WHERE( ycont(:) )
    psaltb(:) = ppssinew / ssw0 * tpmxl(:)%sml
  ENDWHERE
!
!
!
! 2. Loop
! ========
!
!  DO WHILE ( ANY(ycont(:)) .AND. jit<=nit )
!
! Enthalpy of new sea ice
do jp=1,np
  jit = 1
  do while( ycont(jp) .AND. jit<=nit )
    pent(jp) =  &
      glt_enthalpy0d( ptem(jp),psaltb(jp) ) +  &
      cpsw*mu*tpmxl(jp)%sml
!
! Rate of formation of new ice in m.s-1
    IF( ycont(jp) ) phsi(jp) = phef(jp)/( pent(jp)*rhoice )
!
! Compute new ice salinity
    IF ( ycont(jp) )  &
      psalt(jp) = tpmxl(jp)%sml * AMIN1( ppsmax/ssw0,glt_salfrac( phsi(jp) ) ) 
!
! Stop convergence where convergence criterion is met
    IF( ycont(jp) ) THEN
      IF( ABS(psalt(jp)-psaltb(jp))<ppdssi ) THEN
        ycont(jp) = .FALSE.
      ELSE
        psaltb(jp) = psalt(jp)
      ENDIF
    ENDIF
!
    jit = jit + 1
!
end do
  END DO
!
!
!
! 3. Compute final quantities for all points
! ===========================================
!
  ycont(:) = ( gfreeze(:) .AND. phef(:)<0. )
!
  WHERE( .NOT. ycont(:) )
    psalt(:) = 0.
  ENDWHERE
!
! Enthalpy
  pent(:) = glt_enthalpy1d( ycont(:),ptem(:),psalt(:) )
!
! Thickness
  WHERE( ycont(:) )
    phsi(:) = phef(:)*dtt/( pent(:)*rhoice )
  ELSEWHERE 
    phsi(:) = 0.
  ENDWHERE
!
END SUBROUTINE glt_saltrap_r

! ---------------------- END SUBROUTINE glt_saltrap_r -----------------------
! -----------------------------------------------------------------------
