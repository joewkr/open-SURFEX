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
! ===================== MODULE modi_gltools_temper_r ======================
! =======================================================================
!
! Goal:
! -----
!   This module contains a function that allows to compute temperature 
! vertical profile from gltools_enthalpy vertical profile (ent) in the slab
! (both sea ice and snow parts)
! In addition to temperature, a vertical salinity profile can also
! be passed to the routine (optionally).   
!   Note that the computed glt_output temperature is in Celsius.
!
! Created : 12/2009 (D. Salas y Melia)
! Modified: no
!  
! ----------------- BEGIN MODULE modi_gltools_temper_r -------------------- 
!
MODULE modi_gltools_temper_r 
INTERFACE
!
FUNCTION gltools_temper_r(pent,pvsp)
  USE modd_glt_param
  REAL, DIMENSION(nl,nt,np), INTENT(in) ::  &
    pent
  REAL, DIMENSION(nl,nt,np), OPTIONAL, INTENT(in) ::  &
    pvsp
  REAL, DIMENSION(nl,nt,np) ::  &
    gltools_temper_r
END FUNCTION gltools_temper_r
!
END INTERFACE
END MODULE modi_gltools_temper_r
!
! ------------------ END MODULE modi_gltools_temper_r ---------------------
!
!
! -----------------------------------------------------------------------
! ------------------------- FUNCTION gltools_temper_r ---------------------------

!   The input argument is sea ice vertical gltools_enthalpy profile, in K.

FUNCTION gltools_temper_r(pent,pvsp)
!
  USE modd_glt_param
  USE modd_glt_const_thm
!
  IMPLICIT NONE
!
  REAL, DIMENSION(nl,nt,np), INTENT(in) ::  &
    pent
  REAL, DIMENSION(nl,nt,np), OPTIONAL, INTENT(in) ::  &
    pvsp
  REAL, DIMENSION(nl,nt,np) ::  &
    gltools_temper_r
!
  INTEGER ::  &
    jl
  REAL, DIMENSION(nt,np) ::  &
    zb,zc,zdelta
  REAL, DIMENSION(nl,nt,np) ::  &
    ztice_m
!
!
! 1. Initializations
! ===================
!
! .. Compute sea ice melting point as a function of salinity 
!
  IF ( PRESENT(pvsp) ) THEN
! Salinity profile passed in argument
      ztice_m(:,:,:) = -mu * pvsp(:,:,:)
    ELSE
! Prescribed salinity profile in the sea ice part of the slab
      ztice_m(1:nilay,:,:) = -mu * sice 
! Salinity profile in the snow part of the slab
      ztice_m(nilay+1:nl,:,:) = 0.
  ENDIF
!
!
!
! 2. Temperature of the sea ice part of the slab
! ===============================================
!
!* A second-order equation : aX^2 + bX + c = 0 has to be solved
! ( a = cpice0 )
! Let delta be: delta = b^2 - 4ac
! The only physical root is:
!    X0 = -1/(2a) * ( b + delta^0.5 )
!
  DO jl=1,nilay
!
! If gltools_enthalpy is lower than melted sea ice gltools_enthalpy
    WHERE ( pent(jl,:,:)<cpsw*ztice_m(jl,:,:) )
        zb(:,:) = (cpsw-cpice0)*ztice_m(jl,:,:)-  &
          pent(jl,:,:)-xmhofusn0
        zc(:,:) = xmhofusn0 * ztice_m(jl,:,:)
        zdelta(:,:) = zb(:,:)**2-4.*cpice0*zc(:,:)
        gltools_temper_r(jl,:,:) = -1./( 2.*cpice0 )*  &
          ( zb(:,:)+zdelta(:,:)**0.5 ) 
      ELSEWHERE
! If gltools_enthalpy is higher than that of melting sea ice
        gltools_temper_r(jl,:,:) = ztice_m(jl,:,:)
    ENDWHERE
!
  END DO
!
!
! 3. Temperature of the snow part of the slab
! ============================================
! 
  DO jl=nilay+1,nl
! If snow gltools_enthalpy is lower than melted snow gltools_enthalpy
    WHERE ( pent(jl,:,:)<-xmhofusn0 )
        gltools_temper_r(jl,:,:) = 1./cpice0 *  &
          ( pent(jl,:,:) + xmhofusn0 )
      ELSEWHERE
! If snow gltools_enthalpy is higher than melted snow gltools_enthalpy
        gltools_temper_r(jl,:,:) = 0.
    ENDWHERE
  END DO
!
END FUNCTION gltools_temper_r
!
! ------------------------ END FUNCTION gltools_temper_r ------------------------ 
! -----------------------------------------------------------------------
