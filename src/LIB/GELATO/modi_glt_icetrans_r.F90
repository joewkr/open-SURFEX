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
! ======================= MODULE modi_glt_icetrans_r ========================
! =======================================================================
!
! Goal:
! -----
!   This module contains a subroutine that computes the fraction of
! solar short wave radiation SWI (absorbed by sea ice) that :
!   - is absorbed at the surface if there is snow
!   - is absorbed in a snowless ice slab
!   - crosses sea ice and goes to the ocean
!
! Method:
! -------
!   Then the short wave flux crossing sea ice, tptfl%lio is updated.
!
! Created : 1996 (D. Salas y Melia)
! Modified: 2001/07 (D. Salas y Melia)
!           Rewriting: part of the job formerly done by thermo_ice
!           routine is now done here. 
! Modified: 2003/11 (D. Salas y Melia)
!           Now solar radiation is no longer stored in brine pockets, but
!           directly contributes to heating the different layers of the
!           ice slab
! Modified: 2009/06 (D. Salas y Melia)
!           Reduced grid 
! Modified: 2009/11 (D. Salas y Melia)
!           Surface and vertical temperature profile trends are no longer
!           outputs of this routine. The absorbed flux, level by level,
!           is sent out instead - this is more convenient to handle 
!           for the solar transmission + vertical diffusion scheme
!           afterwards.
!
! --------------------- BEGIN MODULE modi_glt_icetrans_r --------------------
!
!THXS_SFX!MODULE modi_glt_icetrans_r
!THXS_SFX!INTERFACE 
!THXS_SFX!!
!THXS_SFX!SUBROUTINE glt_icetrans_r( tpblki,tpmxl,tptfl,tpsit,tpdia,pswtra ) 
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  USE modd_glt_param
!THXS_SFX!  TYPE(t_blk), DIMENSION(nt,np), INTENT(in) ::   &
!THXS_SFX!        tpblki
!THXS_SFX!  TYPE(t_mxl), DIMENSION(np), INTENT(in) ::  &
!THXS_SFX!        tpmxl
!THXS_SFX!  TYPE(t_tfl), DIMENSION(np), INTENT(inout) ::  &
!THXS_SFX!        tptfl
!THXS_SFX!  TYPE(t_sit), DIMENSION(nt,np), INTENT(in) ::  &
!THXS_SFX!        tpsit
!THXS_SFX!  TYPE(t_dia), DIMENSION(np), INTENT(inout) ::  &
!THXS_SFX!        tpdia
!THXS_SFX!  REAL, DIMENSION(nl,nt,np), INTENT(out) ::  &
!THXS_SFX!        pswtra 
!THXS_SFX!END SUBROUTINE glt_icetrans_r
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_glt_icetrans_r
!
! --------------------- END MODULE modi_glt_icetrans_r ----------------------
!
!
!
! -----------------------------------------------------------------------
! ------------------------ SUBROUTINE glt_icetrans_r ------------------------
!
! * If pqsw is the solar flux at the ice-air interface, only part
! of it, (1 - I0) * qsw is absorbed by the top of the ice slab.
!
!   - If the ice is snow covered, I0 = 0.
!   - If there is no snow layer and hsi > hsi0, then 
!                I0 = 0.17
!   - If there is no snow layer and hsi < hsi0, then 
!                I0 = 1 - 0.83*hsi/hsi0,
! with: 
!                hsi0 = 0.1 m 
!
SUBROUTINE glt_icetrans_r( tpblki,tpmxl,tptfl,tpsit,tpdia,pswtra )
!
  USE modd_glt_const_thm
  USE modd_types_glt
  USE modd_glt_param
!
  IMPLICIT NONE
!
!* Arguments
!
  TYPE(t_blk), DIMENSION(nt,np), INTENT(in) ::   &
        tpblki
  TYPE(t_mxl), DIMENSION(np), INTENT(in) ::  &
        tpmxl
  TYPE(t_tfl), DIMENSION(np), INTENT(inout) ::  &
        tptfl
  TYPE(t_sit), DIMENSION(nt,np), INTENT(in) ::  &
        tpsit
  TYPE(t_dia), DIMENSION(np), INTENT(inout) ::  &
        tpdia
  REAL, DIMENSION(nl,nt,np), INTENT(out) ::  &
        pswtra 
!
!* Local variables
!
  INTEGER ::  &
        jk,jl
  REAL, DIMENSION(nilay+1) ::  &
        zaux
  REAL, DIMENSION(np) ::  &
        zfsit,zalbm,zswnet
  REAL, DIMENSION(nt,np) ::  &
        zqsw2si,ztraml,zdmsn3,zqmelt
!
!
!
! 1. Initialize parameters and arrays
! ====================================
!
! .. Trends
!
  pswtra(:,:,:) = 0. 
!
!
! 2. Compute the transmission, storage and surface absorbed fractions
! ===================================================================
!
! .. Compute heat storage and transmission through sea ice.
!   -->> ztraml is the proportion of ISW continuing to the mixed layer
!
! Case 1: a snow layer, or no ice 
!   -->> No heat storage inside sea ice
!   -->> No heat transmission through the ice to the mixed layer
!   -->> Warm up snow layer
!   -->> If melting point is exceeded, melt part of the snow
!   -->> Deliver meltwater to the mixed layer
!
! .. Transmitted SW to the sea ice part of the slab: generally this
! is the incoming SW at the slab's upper surface, but it may be 
! modified if there is snow.
!
  zqsw2si(:,:) = tpblki(:,:)%swa
!
  WHERE ( tpsit(:,:)%hsn>epsil1 )
    pswtra(nilay+1,:,:) = tpblki(:,:)%swa
    zqsw2si(:,:) = 0.
    ztraml(:,:) = 0.
  ENDWHERE
!
  WHERE ( tpsit(:,:)%hsn<=epsil1 )
    ztraml(:,:) = exp( -kappa*tpsit(:,:)%hsi )
  ENDWHERE 
!
!  WHERE ( tpsit(:,:)%hsn>epsil1 .AND. zvtpn(nilay+1,:,:)>tice_m )
!    zqmelt(:,:) = cpice0 * tpsit(:,:)%rsn * tpsit(:,:)%hsn *  &
!      ( zvtpn(nilay+1,:,:)-tice_m ) / dtt
!    zhsnn(:,:) = tpsit(:,:)%hsn -  &
!      dtt*hofusni0*zqmelt(:,:)*rhoice/tpsit(:,:)%rsn
!  ENDWHERE 
!  WHERE ( zhsnn(:,:)<0. )
!    zqsw2si(:,:) = -hofusn0*tpsit(:,:)%rsn*zhsnn(:,:) /  &
!      ( rhoice * dtt )
!    zhsnn = 0.
!!    ztraml(:,:) = exp( -kappa*tpsit(:,:)%hsi ) 
!!    tpsit(:,:)%rsn = rhosnwmin
!  ENDWHERE
!!
!! Case 2 : ice without a snow layer (after being in case 1 the ice/snow
!! pack can be in case 1)
!!
!  WHERE ( zhsnn<=epsil1 )
!    ztraml(:,:) = exp( -kappa*tpsit(:,:)%hsi ) 
!  ENDWHERE
!
!
!
! 3. Update sea ice heat storage and heat transm. to the ocean
! ============================================================
!
! .. Ancillary array
!
  DO jl=1,nilay+1
    zaux(jl) = exp( -kappa*depth(jl) )
  END DO
!
! .. Impact on mixed layer (transmitted radiative flux)
!
  tptfl(:)%lio = tptfl(:)%lio +  &
    SUM( tpsit(:,:)%fsi*ztraml(:,:)*zqsw2si(:,:), DIM=1 )
!
! * Compute the trend on vertical temperature profile due to
! solar short wave absorption
!
  DO jl = 1,nilay
    WHERE ( tpsit(:,:)%hsi>epsil1 .AND. tpsit(:,:)%hsn<=epsil1 )
      pswtra(jl,:,:) =  zqsw2si(:,:)*  &
        ( zaux(jl+1)**tpsit(:,:)%hsi -  &
          zaux(jl)**tpsit(:,:)%hsi  &
        )
    ENDWHERE
  END DO 
!
END SUBROUTINE glt_icetrans_r
!
! ---------------------- END SUBROUTINE glt_icetrans_r ----------------------
! -----------------------------------------------------------------------
