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
! ======================== MODULE modi_glt_updsal_r =========================
! =======================================================================
!
! Goal:
! -----
!   This module contains a subroutine that is used to update sea ice
! salinity, taking into account desalination processes.
!
! Method:
! --------
!   Vancoppenolle et al., O. Modelling (2009)
!   We assume that the sea ice gltools_enthalpy does not change. Implicitly,
! it means that the temperature of sea ice will increase if 
! desalination occurs. The temperature change will be taken into 
! account at the next time step, in glt_vhdiff_r routine.
!
! Created : 2010/03 (D. Salas y Melia)
!
! --------------------- BEGIN MODULE modi_glt_updsal_r ----------------------
!
!THXS_SFX!MODULE modi_glt_updsal_r
!THXS_SFX!INTERFACE 
!THXS_SFX!!
!THXS_SFX!SUBROUTINE glt_updsal_r( gsmelt,tpmxl,tpsit,tptfl )
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  USE modd_glt_param
!THXS_SFX!  LOGICAL, DIMENSION(nt,np), INTENT(in) ::  &
!THXS_SFX!        gsmelt
!THXS_SFX!  TYPE(t_mxl), DIMENSION(np), INTENT(in) ::  &
!THXS_SFX!        tpmxl
!THXS_SFX!  TYPE(t_sit), DIMENSION(nt,np), INTENT(inout) ::  &
!THXS_SFX!        tpsit   
!THXS_SFX!  TYPE(t_tfl), DIMENSION(np), INTENT(inout) ::  &
!THXS_SFX!        tptfl
!THXS_SFX!END SUBROUTINE glt_updsal_r
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_glt_updsal_r
!
! ---------------------- END MODULE modi_glt_updsal_r -----------------------
!
!
!
! -----------------------------------------------------------------------
! ------------------------- SUBROUTINE glt_updsal_r -------------------------
!
SUBROUTINE glt_updsal_r( gsmelt,tpmxl,tpsit,tptfl )
!
  USE modd_glt_const_thm
  USE modd_types_glt
  USE modd_glt_param
  USE modi_glt_salflx_r
  USE mode_glt_stats_r
!
  IMPLICIT NONE
!
  LOGICAL, DIMENSION(nt,np), INTENT(in) ::  &
        gsmelt
  TYPE(t_mxl), DIMENSION(np), INTENT(in) ::  &
        tpmxl
  TYPE(t_sit), DIMENSION(nt,np), INTENT(inout) ::  &
        tpsit   
  TYPE(t_tfl), DIMENSION(np), INTENT(inout) ::  &
        tptfl
!
! .. Local variables
!
  REAL, DIMENSION(np) ::  &
    zqsalt
  REAL, DIMENSION(nt,np) ::  &
    zdssi,zssieq,zqsalt2,zssi
!
!
!
! 1. Initialization
! ==================
!
  zdssi(:,:) = 0.
  zssi(:,:) = tpsit(:,:)%ssi
!
!
!
! 2. Desalination of sea ice 
! ===========================
!
! In both cases, these processes are used only if salinity is greater than 
! the equilibrium salinity (no re-salination !)
!
! .. Summer 
! Criterion is: surface melting
!
  zssieq(:,:) = ssisummer0/ssw0 * SPREAD( tpmxl(:)%sml,1,nt )
  WHERE( tpsit(:,:)%ssi > zssieq(:,:) .AND. gsmelt(:,:) )
    zdssi(:,:) =  &
      ( zssieq(:,:) - tpsit(:,:)%ssi )*dtt / ( ssisummer_ts * xday2sec )
    tpsit(:,:)%ssi = MAX( tpsit(:,:)%ssi + zdssi(:,:), zssieq(:,:) )
  ENDWHERE
!
! .. Winter
! Criterion is: surface temperature is less than bottom temperature
!
  zssieq(:,:) = ssiwinter0/ssw0 * SPREAD( tpmxl(:)%sml,1,nt )
  WHERE( tpsit(:,:)%ssi > zssieq(:,:) .AND.  &
  tpsit(:,:)%tsf < SPREAD( tpmxl(:)%mlf,1,nt ) )
    zdssi(:,:) =  &
      ( zssieq(:,:) - tpsit(:,:)%ssi )*dtt / ( ssiwinter_ts * xday2sec )
    tpsit(:,:)%ssi = MAX( tpsit(:,:)%ssi + zdssi(:,:), zssieq(:,:) )
  ENDWHERE
  zdssi(:,:) = tpsit(:,:)%ssi - zssi(:,:)
!
!
! 
! 3. Flux to the ocean
! =====================
!
! .. Salt flux in g.m-2.s-1
!
  zqsalt2(:,:) = -rhoice*tpsit(:,:)%fsi*tpsit(:,:)%hsi*zdssi(:,:)
  zqsalt(:) = SUM( zqsalt2(:,:), DIM=1 ) / dtt
!
! .. The salt flux is associated to a water flux (in kg.m-2.s-1)
!
  IF ( nleviti==0 ) THEN
    tptfl%wio = tptfl%wio - 1.e-3*zqsalt(:)
  ENDIF
!
! .. Update virtual water flux (concentration/dilution) in kg.m-2.s-1
!
  CALL glt_salflx_r( -zqsalt2,tpmxl,tptfl )
!
! .. Salt flux in kg.m-2.s-1
!
  tptfl(:)%sio = tptfl(:)%sio + 1.e-3 * zqsalt(:)
!
END SUBROUTINE glt_updsal_r
!
! ---------------------- END SUBROUTINE glt_updsal_r ------------------------
! -----------------------------------------------------------------------
