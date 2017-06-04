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
! ====================== MODULE modi_glt_constrain_r ====================
! =======================================================================
!
! Goal:
! ----- 
!    This module contains a subroutine that allows to constrain 
! sea ice. So far, the only option is newtonian damping. The variables
! that can be constrained are: ice surface temperature, concentration
! and thickness.
!  
! Method:
! -------
!    Newtonian damping. This method does not ensure energy is conserved.
! Note that prescribing sea ice concentration and thickness can lead to
! conflicts if constraints are contradictory (e.g. constraining 
! thickness to 1m but concentration to 0). Note that we chose to constrain
! sea ice concentration last (seen as more important...)
!
! Created : 2012/03 (D. Salas y Melia) 
! Modified: 2015/07 (D. Salas y Melia) Complete rewriting; suppress 
!   multi-category damping, and add the possibility to prescribe sea ice 
!   fraction and/or thickness. 
! 
! ------------------ BEGIN MODULE modi_glt_constrain_r ------------------
!
!THXS_SFX!MODULE modi_glt_constrain_r
!THXS_SFX!INTERFACE
!THXS_SFX!!
!THXS_SFX!SUBROUTINE glt_constrain_r( tpdom,tpmxl,tpsit,tpsil,tpdia,tpsit_d )
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  USE modd_glt_param
!THXS_SFX!  TYPE(t_dom), DIMENSION(np), INTENT(in) ::  &
!THXS_SFX!        tpdom
!THXS_SFX!  TYPE(t_mxl), DIMENSION(np), INTENT(inout) ::  &
!THXS_SFX!        tpmxl
!THXS_SFX!  TYPE(t_sit), DIMENSION(nt,np), INTENT(inout) ::  &
!THXS_SFX!        tpsit
!THXS_SFX!  TYPE(t_vtp), DIMENSION(nt,np), INTENT(inout) ::  &
!THXS_SFX!        tpsil
!THXS_SFX!  TYPE(t_dia), DIMENSION(np), INTENT(inout) ::  &
!THXS_SFX!        tpdia
!THXS_SFX!  TYPE(t_sit), DIMENSION(ntd,np), INTENT(in) ::  &
!THXS_SFX!        tpsit_d
!THXS_SFX!END SUBROUTINE glt_constrain_r
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_glt_constrain_r
!
! --------------------- END MODULE modi_glt_constrain_r ---------------------
!
!
! -----------------------------------------------------------------------
! ---------------------- SUBROUTINE glt_constrain_r ---------------------
!
!
SUBROUTINE glt_constrain_r( tpdom,tpmxl,tpsit,tpsil,tpdia,tpsit_d )
  USE modd_types_glt
  USE modd_glt_param
  USE modd_glt_const_thm
  USE mode_glt_stats_r
  USE modi_gltools_newice_r
  USE mode_gltools_enthalpy
  USE modi_gltools_glterr
!
  IMPLICIT NONE 
!
  TYPE(t_dom), DIMENSION(np), INTENT(in) ::  &
        tpdom
  TYPE(t_mxl), DIMENSION(np), INTENT(inout) ::  &
        tpmxl
  TYPE(t_sit), DIMENSION(nt,np), INTENT(inout) ::  &
        tpsit
  TYPE(t_vtp), DIMENSION(nt,np), INTENT(inout) ::  &
        tpsil
  TYPE(t_dia), DIMENSION(np), INTENT(inout) ::  &
        tpdia
  TYPE(t_sit), DIMENSION(ntd,np), INTENT(in) ::  &
        tpsit_d
!
  LOGICAL ::  &
        gcontinue
  INTEGER, PARAMETER ::  &
        ppcent=0.01
  INTEGER ::  &
        jk,jp
  REAL ::  &
        zmin,zdhsit,zfsit0
  REAL, DIMENSION(np) ::  &
        zdamp,zwork,zfsit_i,zhsit_i,zfac,zfacfsi,  &
        zenti_i,zents_i,zenti_f,zents_f
  REAL, DIMENSION(nt,np) ::  &
        zfsi,zhsi,zfsinew,zhsinew
!
!
!
! 1. Initialization
! ==================
!
! .. Initial snow and ice enthalpy
!
  CALL glt_aventh( tpsit,tpsil,zenti_i,zents_i )
!
! .. Global initializations
!
  tpdia(:)%dci = 0.
!
!
!
! 2. Damp sea ice thickness
! ==========================
!
  IF ( chsidmp(1:4)=='DAMP' .OR. TRIM(chsidmp)=='PRESCRIBE' ) THEN
!
! Initial state
!
    zfsinew(:,:) = tpsit(:,:)%fsi
    zhsinew(:,:) = tpsit(:,:)%hsi
    zfsit_i(:) = SUM( tpsit(:,:)%fsi,DIM=1 )  ! sea ice total concentration
    zhsit_i(:) = glt_avhicem_r( tpsit )     ! sea ice mean thickness
!
! These arrays need to be initialized here but also in the sea ice
! concentration section
!
    zfsi(:,:) = 0.
    zhsi(:,:) = 0.
!
! .. Check thickness data toward which we would like to restore
!
    IF ( (SIZE(tpsit_d(1,:)%hsi) > 0) .AND. &
         (MAXVAL( tpsit_d(1,:)%hsi ) < -1.) ) THEN 
      CALL gltools_glterr( 'constrain_r',  &
        'Wrong ice thickness damping data (all %hsi < -1).','STOP' )
    ENDIF
!
    DO jp=1,np
!
! .. Compute sea ice thickness change (damping case)
!
      IF ( chsidmp(1:4)=='DAMP' ) THEN
        zdhsit = dtt / ( xhsidmpeft*xday2sec ) *  &
          ( tpsit_d(1,jp)%hsi - zhsit_i(jp) )
      ELSE IF ( TRIM(chsidmp)=='PRESCRIBE' ) THEN
        zdhsit = tpsit_d(1,jp)%hsi - zhsit_i(jp)
      ENDIF
!
      IF ( zfsit_i(jp)>epsil1 ) THEN
!
!
! 2.1. Method #1: add the same correction to all categories
! ----------------------------------------------------------
!
! .. Now, modify the thickness of the ice categories to modify the mean
! ice thickness by zdhsit. Note the contribution of a dynamic bias should 
! be proportional to the thickness of every category. This will not be the 
! case at all for thermodynamic biases (due to e.g. an atmospheric radiative 
! bias or an ocean temperature bias). In more detail (examples):
!   - ocean heat flux bias: affects all ice categories in the same way
!   - air temperature bias: affects heat conduction (broadly proportional 
! to the inverse of ice thickness)
!   - SW radiative bias: tends to affect all ice categories in the same way
!
! .. Update ice thickness
!
        IF ( TRIM(chsidmp)=='DAMP_ADD' .OR. TRIM(chsidmp)=='PRESCRIBE') THEN
          gcontinue = .TRUE.
          zfsit0 = zfsit_i(jp)
          jk=0
          DO WHILE( gcontinue .AND. jk<=5 )
            jk=jk+1
            zmin = MINVAL(zhsinew(:,jp),MASK=zfsinew(:,jp)>epsil1)
! First case: sea thickness is uniformly increased to meet the constraint
! (no problem), or sea ice thickness that is uniformly removed is less than
! minimum thickness for all categories.
            IF ( zdhsit/zfsit0+zmin>0. ) THEN 
              WHERE( zfsinew(:,jp)>epsil1 )
                zhsinew(:,jp) = zhsinew(:,jp) + zdhsit/zfsit0
              ENDWHERE
              gcontinue = .FALSE.
! Second case: first, uniformly remove zmin to all categories, then remove
! more ice to the remaining categories, until the constraint is met.
            ELSE
              WHERE( zfsinew(:,jp)>epsil1 )
                zhsinew(:,jp) = zhsinew(:,jp)-zmin
              ENDWHERE  
              zdhsit = zdhsit+zmin*zfsit0
            ENDIF
            WHERE ( zhsinew(:,jp)<epsil1 ) 
              zfsinew(:,jp)=0.
            ENDWHERE
            zfsit0 = SUM( zfsinew(:,jp) )
          END DO
!
          WHERE ( tpsit(:,jp)%hsi>epsil1 .AND. zhsinew(:,jp)<=epsil1 )
            tpsit(:,jp)%esi = .FALSE. 
          ENDWHERE
!
!
! 2.2. Method #2: multiply all thicknesses by a factor
! -----------------------------------------------------
!
        ELSE IF ( TRIM(chsidmp)=='DAMP_FAC' ) THEN
          zfac(jp) = 1.
          IF ( zhsit_i(jp)>epsil1 ) THEN
            zfac(jp) = 1. + zdhsit/zhsit_i(jp)
          ENDIF
!
! Define a multiplicative factor for sea ice concentration (to help reducing or 
! increasing sea ice thickness)
          zfacfsi(jp) = 1.
          IF ( ABS(zfac(jp)-1.) > ppcent ) THEN
! Low values of the factor: decrease sea ice concentration to contribute to 
! reducing mean sea ice thickness
            IF ( zfac(jp) < 1.-ppcent ) THEN
              zfacfsi(jp) = zfac(jp)/(1.-ppcent)
              zfac(jp) = 1.-ppcent
! High values of the factor: increase very low sea ice concentrations, 
! but not more than 0.15
            ELSE
              IF ( zfsit_i(jp)<xfsic ) THEN
                zfacfsi(jp) = EXP( dtt/(3.*xday2sec)*LOG( xfsic/zfsit_i(jp) ) )
                zfac(jp) = AMIN1( zfac(jp)/zfacfsi(jp),1.+ppcent )
              ENDIF
            ENDIF
          ENDIF
!
! We do not want to modify sea ice thickness by more than 1%, in order to 
! avoid runaway thickness of categories
          zfsinew(:,jp) = zfacfsi(jp) * tpsit(:,jp)%fsi
          zhsinew(:,jp) = zfac(jp) * tpsit(:,jp)%hsi
        ENDIF
!
      ELSE    ! IF zfsit_i(jp)<=epsil1
!
! Case without initial sea ice: DAMP_ADD or DAMP_FAC plays no role
        zfsi(1,jp) = xfsic
        zhsi(1,jp) = zdhsit / xfsic
      ENDIF
    END DO   ! Loop on jp
!
    tpsit(:,:)%hsi = zhsinew(:,:)
    tpsit(:,:)%fsi = zfsinew(:,:)
!
! This routine will be active only where zfsi(:,:)>=epsil1 and tpsit(:,:)<epsil1
!
    CALL gltools_newice_r( zfsi,zhsi,tpmxl,tpsit,tpsil )
!
! Add rate of change of sea ice mass due to thickness constraint
!
    tpdia(:)%dci = tpdia(:)%dci +  &
      rhoice * ( glt_avhicem_r(tpsit) - zhsit_i(:) ) / dtt
!
  ENDIF
!
!
!
! 3. Damp/prescribe sea ice concentration
! ========================================
!
!   Note that concentration should be damped before thickness 
! (if both are damped)
!
!   We assume here that we modify sea ice concentrations, without  
! conserving total, nor per-category, ice volume.
!   If total sea ice concentration is less than epsil1=1.e-10, we consider
! there was no sea ice at all initially, meaning that we must create
! new sea ice from nothing.
!
  IF ( TRIM(cfsidmp)=='DAMP' .OR. TRIM(cfsidmp)=='PRESCRIBE' ) THEN
!
! Initial state
!
    zfsinew(:,:) = tpsit(:,:)%fsi
    zhsinew(:,:) = tpsit(:,:)%hsi
    zfsit_i(:) = SUM( tpsit(:,:)%fsi,DIM=1 )  ! sea ice total concentration
    zhsit_i(:) = glt_avhicem_r( tpsit )     ! sea ice mean thickness
!
! These arrays need to be initialized here but also in the sea ice thickness
! damping section
!
    zfsi(:,:) = 0.
    zhsi(:,:) = 0.
!
! .. Check concentration data toward which we would like to restore
!
    IF ( (SIZE(tpsit_d(1,:)%fsi) > 0) .AND. &
         ( MINVAL( tpsit_d(1,:)%fsi ) < 0. .OR.  &
         MAXVAL( tpsit_d(1,:)%fsi ) > 1. )) THEN
      CALL gltools_glterr( 'constrain_r',  &
        'Wrong ice concentration damping data &
        & (probably given in % instead of fraction of unity).','STOP' )
    ENDIF
    DO jp=1,np
!
      IF ( zfsit_i(jp)>epsil1 ) THEN
!      
! .. Case 1: total sea ice fraction > epsil1
!
        IF ( TRIM(cfsidmp)=='DAMP' ) THEN
          zdamp(jp) = dtt / ( xfsidmpeft*xday2sec ) *  &
            ( MIN(tpsit_d(1,jp)%fsi,xfsimax) - zfsit_i(jp) )
!
          DO jk=1,nt
            tpsit(jk,jp)%fsi = tpsit(jk,jp)%fsi *  &
              ( 1. + zdamp(jp) / zfsit_i(jp) )
! Conserve sea ice volume
            tpsit(jk,jp)%hsi = tpsit(jk,jp)%hsi /  &
              ( 1. + zdamp(jp) / zfsit_i(jp) )
          END DO 
!
        ELSE IF ( TRIM(cfsidmp)=='PRESCRIBE' ) THEN
          zwork(jp) = MAX(  &
            MIN(tpsit_d(1,jp)%fsi,xfsimax) / zfsit_i(jp), epsil1 )
          DO jk=1,nt
            tpsit(jk,jp)%fsi = tpsit(jk,jp)%fsi * zwork(jp)
! Conserve sea ice volume
            tpsit(jk,jp)%hsi = tpsit(jk,jp)%hsi / zwork(jp)
          END DO
!
        ENDIF 
!
      ELSE ! IF zfsit_i(jp)<=epsil1
!
! .. Case 2: total sea ice fraction <= epsil1
!
! If zfsit_i<=epsil1, the concentration of every category tpsit(jk,:)<=epsil1.
! In particular, the thinnest category has a concentration <=epsil1. If new
! ice has to appear here, we decide to increase only the concentration of the 
! thinnest category, and to assign it a small thickness, in order to limit 
! the ice volume change.
! Note that if this concentration constraint is not consistent with the 
! thickness constraint (e.g. hsi_d = 0. but fsi_d /= 0.), we choose to respect 
! the concentration constraint but not the thickness constraint.
!
        IF ( TRIM(cfsidmp)=='DAMP' ) THEN
          zfsi(1,jp) = dtt / ( xfsidmpeft*xday2sec ) *  &
            ( MIN(tpsit_d(1,jp)%fsi,xfsimax) - zfsit_i(jp) )
        ELSE IF ( TRIM(cfsidmp)=='PRESCRIBE' ) THEN
          zfsi(1,jp) = MIN(tpsit_d(1,jp)%fsi,xfsimax)
        ENDIF
        zhsi(1,jp) = xhsimin
!
      ENDIF
!
    END DO   ! Loop on jp
!
! This routine will be active only where zfsi(:,:)>=epsil1 and tpsit(:,:)<epsil1
!
    CALL gltools_newice_r( zfsi,zhsi,tpmxl,tpsit,tpsil )
!
! We want to make sure that the new total sea ice concentration does not
! exceed xfsimax
!
    zwork(:) = SUM( tpsit(:,:)%fsi,DIM=1 )
    DO jk=1,nt
      WHERE ( zwork(:) > xfsimax )
        tpsit(jk,:)%fsi = tpsit(jk,:)%fsi * xfsimax / zwork(:)
      ENDWHERE
    END DO
!
! Add rate of change of sea ice mass due to ice concentration constraint
!
    tpdia(:)%dci = tpdia(:)%dci +  &
      rhoice * ( glt_avhicem_r(tpsit) - zhsit_i(:) ) / dtt
!
! Diagnose constraint
!
   tpdia(:)%cst = 100.*tpsit_d(1,:)%fsi
!
  ENDIF
!
!
!
! 4. Final operations
! ====================
!
! .. Diagnose changes in snow/ice enthalpy due to damping/restoring 
! (there is no separation of the effects of the different operations)
!
  CALL glt_aventh( tpsit,tpsil,zenti_f,zents_f )
  tpdia(:)%dmp = ( zenti_f+zents_f-zenti_i-zents_i ) / dtt
!
END SUBROUTINE glt_constrain_r
!
! ------------------- END SUBROUTINE glt_constrain_r --------------------
! -----------------------------------------------------------------------
