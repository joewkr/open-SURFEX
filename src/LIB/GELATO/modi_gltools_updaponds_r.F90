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
! ===================== MODULE modi_gltools_updaponds_r ======================
! =======================================================================
!
! Goal:
! -----
!   This module contains a subroutine that is used to update the volume
!   and albedo of melt ponds. LEVITATING MELT PONDS!!!
!
!
! Created : 2010/02 (M. Chevallier)
! Modified: 2010/11 (M. Chevallier)
!       Called from updasn_r. Full computation of ponds thermodynamics 
!       and ponds albedo.
!       Ponds albedo is used to correct sea-ice albedo. Fraction of the ice
!       surface that is not covered by ponds consists in melting bare ice.
!
! ------------------ BEGIN MODULE modi_gltools_updaponds_r -------------------
!
!THXS_SFX!MODULE modi_gltools_updaponds_r
!THXS_SFX!INTERFACE
!THXS_SFX!!
!THXS_SFX!SUBROUTINE gltools_updaponds_r (omelt,tpatm,tpblki,tpdia,tpsit,pasi )
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  USE modd_glt_param
!THXS_SFX!  LOGICAL, DIMENSION(nt,np), INTENT(in) :: &
!THXS_SFX!        omelt
!THXS_SFX!  TYPE(t_atm), DIMENSION(np), INTENT(in) :: &
!THXS_SFX!        tpatm  
!THXS_SFX!  TYPE(t_blk), DIMENSION(nt,np), INTENT(in) :: &
!THXS_SFX!        tpblki
!THXS_SFX!  TYPE(t_dia), DIMENSION(np), INTENT(inout) :: &
!THXS_SFX!        tpdia
!THXS_SFX!  TYPE(t_sit), DIMENSION(nt,np), INTENT(inout) :: &
!THXS_SFX!        tpsit
!THXS_SFX!  REAL, DIMENSION(nt,np), INTENT(inout) :: &
!THXS_SFX!        pasi
!THXS_SFX!END SUBROUTINE gltools_updaponds_r
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_gltools_updaponds_r
!
! ------------------- END MODULE modi_gltools_updaponds_r --------------------
!
!
!
! -----------------------------------------------------------------------
! ------------------------- SUBROUTINE gltools_updaponds_r -------------------------
!
! * Subroutine used to update melt ponds volume and albedo (takes into account rain, snow, or thermodynamic surface melting). 
! * (APONDS = Albedo of PONDS)
!
SUBROUTINE gltools_updaponds_r ( omelt,tpatm,tpblki,tpdia,tpsit,pasi )
!
  USE modd_glt_const_thm
  USE modd_types_glt
  USE modd_glt_param
!
  IMPLICIT NONE
!
  LOGICAL, DIMENSION(nt,np), INTENT(in) :: &
        omelt
  TYPE(t_atm), DIMENSION(np), INTENT(in) :: &
        tpatm  
  TYPE(t_blk), DIMENSION(nt,np), INTENT(in) :: &
        tpblki
  TYPE(t_dia), DIMENSION(np), INTENT(inout) :: &
        tpdia
  TYPE(t_sit), DIMENSION(nt,np), INTENT(inout) :: &
        tpsit
  REAL, DIMENSION(nt,np), INTENT(inout) :: &
        pasi
!
  LOGICAL, DIMENSION(nt,np) :: &
        gsipond, gsnpond
  INTEGER :: &
        jp,jt
  REAL :: &
        zrhwinv, zdelta
  REAL, DIMENSION(np) :: &
        zwork1, zwork2
  REAL, DIMENSION(nt,np) :: &
        zvmp, zpcpr, ztsf,zmeltt, zmelts, &
        zfmp, zdmp, zamp, zdiftp, zfblki
!
!
! 1. Initializations
! ==================
!
! Volume (fraction * depth) of melt ponds covering the fractional cell
  zvmp(:,:) = tpsit(:,:)%vmp
!
  ztsf(:,:) = tpsit(:,:)%tsf
!
!  print*, 'tsf = ', MAXVAL(SUM(tpsit(:,:)%fsi * tpsit(:,:)%tsf,DIM=1))
!  print*, 'vmp = ', MAXVAL(SUM(tpsit(:,:)%fsi * tpsit(:,:)%vmp,DIM=1))
!
! .. Compute the rate of liquid precipitation
!
  zpcpr(:,:) = SPREAD( tpatm(:)%lip,1,nt)
!
! .. Melting ice with snow, ice thickness> hice_mp
!
  gsnpond = .FALSE.
  WHERE ( tpsit(:,:)%hsi>= hsi_mp .AND. tpsit(:,:)%hsn >= hsn_mp .AND. omelt(:,:) )
    gsnpond(:,:) = .TRUE.
  ENDWHERE
!
! .. Melting ice without snow, ice thickness> hice_mp 
!
  gsipond = .FALSE.
  WHERE ( tpsit(:,:)%hsi>= hsi_mp .AND. tpsit(:,:)%hsn < hsn_mp .AND. omelt(:,:) )
    gsipond(:,:) = .TRUE.
  ENDWHERE
!
! .. Sea ice top ablation
!
!   zwork1(:) = MAX(tpdia(:)%mrt, 0.)
!
! .. Snow ablation (positive values)
!
  zwork2(:) =  ( SUM( tpsit(:,:)%rsn*tpsit(:,:)%fsi*tpsit(:,:)%hsn, DIM=1 )-  &
        tpdia(:)%dsn )/ dtt  
! .. Only consider melting snow: dhsn<0 i.e. -zworks2(:)>0.
  zwork2(:) = MAX( -zwork2(:),0. )
!
! .. Assuming uniform distribution of melt rates over all categories...
!  zmeltt(:,:) = SPREAD( zwork1(:), 1, nt)
  zmeltt(:,:) = SPREAD( -tpdia(:)%mrt, 1, nt )
  zmelts(:,:) = SPREAD( -zwork2(:), 1, nt)
!
!  print*, 'melti = ', MAXVAL(SUM(tpsit(:,:)%fsi * zmeltt(:,:), DIM=1))
!  print*, 'melts = ', MAXVAL(SUM(tpsit(:,:)%fsi * zmelts(:,:), DIM=1))
!
  zdiftp = 0.
  WHERE ( ( tp - ztsf(:,:) ) >= epsil5 )
       zdiftp(:,:) = ( tp - ztsf(:,:) ) / tp
   ELSEWHERE
       zdiftp(:,:) = 0.
  ENDWHERE
!
! .. Incoming fluxes over sea-ice (approximation)
!
  zfblki(:,:) = tpblki(:,:)%nsf + tpblki(:,:)%swa
!
  zrhwinv = dtt / rhofw ! in s*m3/kg
! 
! 2. Compute the characteristics of melt ponds
! ============================================
!
! 2.0. Dry ice or thin ice
! ------------------------
!
! .. No melt or ice too thin!
!
!    zvmp(:,:) = 0.
    zfmp(:,:) = 0.
    zdmp(:,:) = 0.
!
!
 DO jp=1,np
!  print*, 'jp= ', jp
  DO jt=1,nt
!   print*, 'jt= ', jt
!
!  WHERE ( gsnpond(:,:) )
!IF ( tpsit(jt,jp)%hsi>=hsi_mp .AND. &
 !         tpsit(jt,jp)%hsn>= epsil1 .AND. &
  !        omelt(jt,jp)=.TRUE. ) THEN
!   print*, 'gsnpond(jt,jp) = ',gsnpond(jt,jp)
!   print*, 'gsipond(jt,jp) = ',gsipond(jt,jp)
   IF ( gsnpond(jt,jp) ) THEN
!
! 2.1. Melting ice with snow
! --------------------------
!
! .. Melting ice, but too much snow.
! .. Only increase the total melt water volume 
! at the sea ice surface.
! .. No ponds.
!
       zvmp(jt,jp) = zvmp(jt,jp) + &
                 zrhwinv * ( zmeltt(jt,jp) + zmelts(jt,jp) + zpcpr(jt,jp) )
       zfmp(jt,jp) = 0.
       zdmp(jt,jp) = 0.
! 
!  ENDWHERE
!
!
!  WHERE ( gsipond(:,:) )
!
!        IF ( tpsit(jt,jp)%hsi>=hsi_mp .AND. &
 !               tpsit(jt,jp)%hsn< epsil1 .AND. &
  !              omelt(jt,jp)=.TRUE. ) THEN
    ELSE IF ( gsipond(jt,jp) ) THEN
!          print*, 'tp - tsf = ' , zdiftp(jt,jp)
!          print*, 'NSF + SWA = ', zfblki(jt,jp)
!          print*, 'DVPI = ', zrhwinv * hofusni0 * rhoice0 * zfblki(jt,jp)
!
! 2.2. Melting ice without snow cover
! -----------------------------------
!
! .. Melting ice and no more snow.
! .. Only a fraction of surface melt water is 
! used to form the melt ponds.
!
! 2.2.1. Update melt pond volume
! ------------------------------
!
          zvmp(jt,jp) = zvmp(jt,jp) + xr1 * &
              zrhwinv * ( zmeltt(jt,jp) + zmelts(jt,jp) + zpcpr(jt,jp) )
!
!    zvmp(:,:) = zvmp(:,:) - drainrate * dtt / xday2sec !(MAYBE NOT)!
!
! .. During autumn freezing, pond volume decays exponentially. Residual volume
! could be used to form pond ice (incorporated in sea ice: not done!).
!
!          zvmp(jt,jp) = zvmp(jt,jp)*exp(xr2 * zdiftp(jt,jp))
!
          zvmp(jt,jp) = zvmp(jt,jp) +  &
                AMIN1 ( zrhwinv * hofusni0 * rhoice0 * zfblki(jt,jp), 0.)
!
! 2.2.2 Compute melt pond fraction and depth
! ------------------------------------------
! .. Melt pond depth and fraction: simple linear relation.
! (dmp = 0.8*fmp, vmp = fmp * dmp )
!
          zdelta = AMAX1( dptfr2 * dptfr2 + 4.*dptfr1*zvmp(jt,jp),0. )
! .. Only one physical root...
          zfmp(jt,jp) = 0.5*(- dptfr2 + SQRT(zdelta) ) / dptfr1
          zfmp(jt,jp) = AMIN1( zfmp(jt,jp),1. )
          zdmp(jt,jp) = AMAX1( dptfr1 * zfmp(jt,jp) + dptfr2, 0. )
!
! 2.2.3. Limit pond depth to 90% of ice thickness
! -----------------------------------------------
!
          zdmp(jt,jp) = AMIN1( zdmp(jt,jp), dpthhi * tpsit(jt,jp)%hsi )
!
! .. When limit depth reached, pond volume is corrected with respect to hpond
! equal to limit depth. 
!
          zvmp(jt,jp) = zfmp(jt,jp) * zdmp(jt,jp) 
!       
!          print*, 'fmp: ', zfmp(jt,jp)
!          print*, 'dmp: ', zdmp(jt,jp)
!          print*, 'vmp: ', zvmp(jt,jp)
!
!  ENDWHERE
    ELSE
             !zvmp(jt,jp) = zvmp(jt,jp)*EXP(xr2 * zdiftp(jt,jp))
!             print*, 'DVPI = ', zrhwinv * hofusni0 * rhoice0 * zfblki(jt,jp)
             zvmp(jt,jp) = zvmp(jt,jp) +  &
                AMIN1 ( zrhwinv * hofusni0 * rhoice0 * zfblki(jt,jp), 0.)
!            zfmp(jt,jp) = 0.
!            zdmp(jt,jp) = 0.
!
! .. NEW: 13/12/10. Computation of fmp and dmp is done even when melt flag is
! false. Snow depth is the ultimate limitation of the pond formation.
!
             zdelta = AMAX1( dptfr2 * dptfr2 + 4.*dptfr1*zvmp(jt,jp), 0. )
             zfmp(jt,jp) = 0.5*(- dptfr2 + SQRT(zdelta) ) / dptfr1
             zfmp(jt,jp) = AMIN1( zfmp(jt,jp),1. )
             zdmp(jt,jp) = AMAX1( dptfr1 * zfmp(jt,jp) + dptfr2, 0. )
   ENDIF
!        
! .. Negative volume means all melt ponds have disappeared.
!  
   zvmp(jt,jp) = AMAX1(zvmp(jt,jp),0.)
  END DO
 END DO
!
! .. Cutoff to avoid vmp>0 during the winter.
!
 WHERE (zvmp(:,:)<1E-8)
    zvmp(:,:) = 0.
    zfmp(:,:) = 0.
    zdmp(:,:) = 0.
 ENDWHERE
!
! .. Cutoff to avoid fmp>0 when dmp=0.
!
 WHERE (zdmp(:,:)<epsil5)
    zfmp(:,:) = 0.
 ENDWHERE
!
!
!  print*, 'MAX vmp: ', MAXVAL(SUM(tpsit(:,:)%fsi * zvmp(:,:),DIM=1))
!  print*, 'MAX fmp: ', MAXVAL(SUM(tpsit(:,:)%fsi * zfmp(:,:),DIM=1))
!  print*, 'MAX dmp: ', MAXVAL(SUM(tpsit(:,:)%fsi * zdmp(:,:),DIM=1))
!  print*, 'MAX hsi: ', MAXVAL(SUM(tpsit(:,:)%fsi * tpsit(:,:)%hsi,DIM=1))
!
! 2.3. Update glt_output fields
! -------------------------
!
! .. Melt pond volume:
!
  tpsit(:,:)%vmp = zvmp(:,:)
!
!
! 3. Compute the albedo of melt ponds
! ===================================
!
! 3.1. Compute albedo of melt ponds (function of pond depth)
! ----------------------------------------------------------
!
  zamp(:,:) = xwmp1 * (xamp1 + EXP(-xbmp1*zdmp(:,:)-xcmp1))
  zamp(:,:) = zamp(:,:) + xwmp2 * (xamp2 + EXP(-xbmp2*zdmp(:,:)-xcmp2))
  zamp(:,:) = zamp(:,:) + xwmp3 * (xamp3 + EXP(-xbmp3*zdmp(:,:)-xcmp3))
  zamp(:,:) = zamp(:,:) + xwmp4 * xamp4
!
! 3.2. Update sea-ice albedo
! --------------------------
!
  pasi(:,:) = (1.-zfmp(:,:)) * pasi(:,:) + zfmp(:,:) * zamp(:,:)
!
!
  tpdia(:)%amp = SUM( tpsit(:,:)%fsi * pasi(:,:), DIM=1 ) 
!
!
!
END SUBROUTINE gltools_updaponds_r
!
! ---------------------- END SUBROUTINE gltools_updaponds_r ------------------------
! -----------------------------------------------------------------------


