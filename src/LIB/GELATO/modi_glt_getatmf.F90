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
! ========================= MODULE modi_glt_getatmf =========================
! =======================================================================
!
! This routine was created for Gelato version 3, i.e. Gelato is under the
! form of a routine inserted in the OPA 8 code. 
! It allows Gelato to get the needed forcing from the main routine input
! atmospheric fields.
!  
! Created : 10/1999 (D. Salas y Melia)
! Modified: 06/2009 (D. Salas y Melia)
!    Patch (non energy-conserving) to discard unrealistic non-solar 
!    heat fluxes in the "single physics" approach 
! Modified: 08/2009 (D. Salas y Melia)
!    Manages single or double physics
! Modified: 12/2012 (D. Salas y Melia)
!    Parallelism & super-type
!
! --------------------- BEGIN MODULE modi_glt_getatmf -----------------------
!
!THXS_SFX!MODULE modi_glt_getatmf
!THXS_SFX!INTERFACE
!THXS_SFX!!
!THXS_SFX!SUBROUTINE glt_getatmf( tpglt )
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  USE modd_glt_param
!THXS_SFX!  TYPE(t_glt), INTENT(inout) ::  &
!THXS_SFX!    tpglt
!THXS_SFX!END SUBROUTINE glt_getatmf
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_glt_getatmf
!
! ---------------------- END MODULE modi_glt_getatmf ------------------------
!
!
! -----------------------------------------------------------------------
! ----------------------- SUBROUTINE glt_getatmf ----------------------------

SUBROUTINE glt_getatmf( tpglt )
  USE modd_types_glt
  USE modd_glt_const_thm
  USE modd_glt_param
#if ! defined in_surfex
  USE mode_gltools_bound
#endif
  USE mode_gltools_prtrarr
!
  IMPLICIT NONE 
!
  TYPE(t_glt), INTENT(inout) ::  &
    tpglt
!
  INTEGER, PARAMETER :: jkmax=3   ! Order of the mask
  INTEGER ::  &
        ji,jj,jk 
  INTEGER, DIMENSION(jkmax,SIZE(tpglt%dom,1),SIZE(tpglt%dom,2)) ::  &
        itmk
  INTEGER, DIMENSION(nt,SIZE(tpglt%dom,1),SIZE(tpglt%dom,2)) ::  &
        itmk0
  REAL, DIMENSION(SIZE(tpglt%dom,1),SIZE(tpglt%dom,2)) ::  &
        zfsit,ztsfm,zalbm,  &
        zfsite,ztmke,ztmke0,zwork2
  TYPE(t_sit),  &
    DIMENSION(SIZE(tpglt%sit,1),SIZE(tpglt%sit,2),SIZE(tpglt%sit,3)) ::  &
        tzsit
!
!
! TO BE INCORPORATED PROPERLY LATER ON (WITH AN OPTION)
! *** First, correct short wave and non-solar fluxes.
! In case the input short wave is negative (normally, slightly...),
! add this negative short wave flux to the non-solar, and set the
! short wave to zero. This ensures that energy is conserved.
! If this is not done, serious problems may arise:
!       - if there is a snow layer, this layer totally absorbs the 
! negative solar short wave
!       - it this snow layer is extremely thin, the temperature trend
! can reach O(10^6) K / day !
!       - since the code has no criterion to correct a very low
! temperature, it will normally fail in the vertical heat diffusion 
! scheme afterwards.
! It would be better however to let the user choose whether this 
! operation (convert negative short wave to non-solar) is legal. 
! A flag in the namelist will be made available later on. The 
! alternative is to stop if a negative short wave flux is 
! encountered.
!
!  znsf(:,:) = pnsf(:,:)
!  zswa(:,:) = pswa(:,:)
!  WHERE( pswa(:,:)<0. )
!    znsf(:,:) = pnsf(:,:)+pswa(:,:)
!    zswa(:,:) = 0.
!  ENDWHERE
!
!
! .. Recreate the following quantities for all considered types_glt of 
! surfaces:
!       - non solar heat flux (W.m-2), positive if it is supposed to
!         warm up the surface
!       - solar short wave heat flux (W.m-2)
!       - derivative of non solar heat flux (W.m-2.K-1)
!       - evaporation flux. Sorry, we don't have the derivative of this
!         flux by temperature, so we will merely assume this flux is 
!         the same for all surfaces.
! .. This process depends on whether we are in single or double physics
! mode
!
! Get ice state (just for better code readibility...)
!
  tzsit = tpglt%sit
!
! Compute total sea ice concentration 
!
  zfsit(:,:) = SUM( tzsit(:,:,:)%fsi,DIM=1 )
!
! 
! 1. Double physics case
! =======================
!
! 1.1. Repartition of fluxes on sea ice
! --------------------------------------
!
! .. The repartition of solar, non-solar heat fluxes and evaporation 
! between water and ice is already done. However, this repartition has
! to be done on ice among the different ice categories, if only one 
! flux has been provided (case nnflxin=1)
!                          _ 
! .. Note that the average X of an extensive quantity X computed
! over the ice part of the grid cell (this ice part covers a fraction f_tot
! that is generally not equal to 1) is:
!  _
!  X = 1./f_tot * SUM ( f_i * X_i ),
! 
! where f_i and X_i are respectively the fraction and value of X for 
! ice category index i, and 
!
! f_tot = SUM ( f_i )
!    _  
! If F is the average non-solar flux over ice, the flux for ice category i 
! (with temperature T_i), F_i can be written as (linear approximation):
!       _                    _
! F_i = F + dF/dT * ( T_i - T ),
!
! where dF/dT is the non-solar heat flux derivative by surface temperature 
! and 
!  _
!  T = 1./f_tot * SUM ( f_i * T_i ). 
!
! It can be showed that the following equation is true:
!  _
!  F = 1./f_tot * SUM ( f_i * F_i )
! 
  IF ( nnflxin/=0 ) THEN 
!
      IF (lp1) THEN
        WRITE(noutlu,*) '*********************************************'
        WRITE(noutlu,*) '  DOUBLE / MULTIPLE FLUX MODE (INPUT)'
      ENDIF
!
! Ice average temperature and albedo
      WHERE( zfsit(:,:)>epsil1 )
        ztsfm(:,:) = SUM( tzsit(:,:,:)%fsi*tzsit(:,:,:)%tsf,DIM=1 ) /  &
          zfsit(:,:)
        zalbm(:,:) = SUM( tzsit(:,:,:)%fsi*tzsit(:,:,:)%asn,DIM=1 ) /  &
          zfsit(:,:)
      ENDWHERE
      WHERE( zfsit(:,:)<=epsil1 )
        ztsfm(:,:) = tpglt%tml(:,:)%mlf
        zalbm(:,:) = albi
      ENDWHERE
!
! == First case: only one flux is provided for all sea ice categories
!
      IF ( nnflxin==1 ) THEN
!
          IF (lp1) THEN
            WRITE(noutlu,*) ' (only 1 flux provided for all ice cats)'
            WRITE(noutlu,*) '*********************************************'
          ENDIF
!
          DO jk=1,nt
!
! Non-solar heat flux
            tpglt%blki(jk,:,:)%nsf = tpglt%atm_ice(1,:,:)%nsf +  &
              tpglt%atm_ice(1,:,:)%dfl *  &
              ( tzsit(jk,:,:)%tsf-ztsfm(:,:) )
! Solar heat flux
            tpglt%blki(jk,:,:)%swa = tpglt%atm_ice(1,:,:)%swa *        &
              ( 1.-tzsit(jk,:,:)%asn ) / ( 1.-zalbm(:,:) )
!
! Derivative of non solar heat flux
            tpglt%blki(jk,:,:)%dfl = tpglt%atm_ice(1,:,:)%dfl
!
! Evaporation
!       - The water fluxes are now in kg.m-2.s-1 throughout the code
            tpglt%blki(jk,:,:)%eva = tpglt%atm_ice(1,:,:)%eva
!
          END DO
!
        ELSE
!
          IF (lwg) THEN
            WRITE(noutlu,*) '  (1 flux provided for every ice cat)'
            WRITE(noutlu,*) '*****************************************'
          ENDIF
!
! == Second case: one flux is provided per sea ice category
!
! Non-solar heat flux
          tpglt%blki(:,:,:)%nsf = tpglt%atm_ice(:,:,:)%nsf
!
! Solar heat flux
          tpglt%blki(:,:,:)%swa = tpglt%atm_ice(:,:,:)%swa
!
! Derivative of non solar heat flux
          tpglt%blki(:,:,:)%dfl = tpglt%atm_ice(:,:,:)%dfl
!
! Evaporation 
!       - The water fluxes are now in kg.m-2.s-1 throughout the code
          tpglt%blki(:,:,:)%eva = tpglt%atm_ice(:,:,:)%eva
      ENDIF  
!
!
! 1.2. Fluxes on water
! ---------------------
!
! Non-solar heat flux
      tpglt%blkw(:,:)%nsf = tpglt%atm_wat(:,:)%nsf
!
! Solar heat flux
      tpglt%blkw(:,:)%swa = tpglt%atm_wat(:,:)%swa
!
! Derivative of non solar heat flux
      tpglt%blkw(:,:)%dfl = tpglt%atm_wat(:,:)%dfl

! * Evaporation 
!       - The water fluxes are now in kg.m-2.s-1 throughout the code
      tpglt%blkw(:,:)%eva = tpglt%atm_wat(:,:)%eva
!
    ELSE     
!
! 
! 2. Single physics case
! =======================
!
      IF (lwg) THEN
        WRITE(noutlu,*) '*****************************************'
        WRITE(noutlu,*) '  SINGLE PHYSICS MODE'
        WRITE(noutlu,*) '*****************************************'
      ENDIF
!
!
! 2.1. Compute average surface quantities
! ----------------------------------------
!
! Surface average temperature and albedo
      ztsfm(:,:) = SUM( tzsit(:,:,:)%fsi*tzsit(:,:,:)%tsf,DIM=1 ) +  &
        ( 1.-zfsit(:,:) )*tpglt%tml(:,:)%tml
      zalbm(:,:) = SUM( tzsit(:,:,:)%fsi*tzsit(:,:,:)%asn,DIM=1 ) +  &
        ( 1.-zfsit(:,:) )*albw
!
!
! 2.2. Fluxes on sea ice
! -----------------------
!
! .. The repartition of solar, non-solar heat fluxes and evaporation 
! between water and ice is not done yet. This repartition can be
! applied following the same principles as for double physics. The
! surface we deal with is not only ice, but ice+water (total fraction
! ftot is now always 1)
!
      DO jk=1,nt
!
! Non-solar heat flux
        tpglt%blki(jk,:,:)%nsf = tpglt%atm_mix(1,:,:)%nsf +  &
          tpglt%atm_mix(1,:,:)%dfl *  &
          ( tzsit(jk,:,:)%tsf-ztsfm(:,:) )
!
! Solar heat flux
        tpglt%blki(jk,:,:)%swa = tpglt%atm_mix(1,:,:)%swa *  &
          ( 1.-tzsit(jk,:,:)%asn ) / ( 1.-zalbm(:,:) )
!
!
! Derivative of non solar heat flux
        tpglt%blki(jk,:,:)%dfl = tpglt%atm_mix(1,:,:)%dfl
!
! Evaporation
!       - The water fluxes are now in kg.m-2.s-1 throughout the code
        tpglt%blki(jk,:,:)%eva = tpglt%atm_mix(1,:,:)%eva
!
      END DO
!
!
! 2.3. Modify unrealistic fluxes (near ice edge)
! -----------------------------------------------
! TO BE TRANSFORMED INTO A ROUTINE LATER ON
!
! BEGIN FUTURE ROUTINE
!
! First iteration
!
      zfsite(:,:)=zfsit(:,:)
!
      ztmke0(:,:) = FLOAT(tpglt%dom%tmk)
      itmk(1,:,:)=1
      WHERE( zfsite(:,:)<0.5 .OR. ztmke0(:,:)<0.5 )
        itmk(1,:,:) = 0
      ENDWHERE
!
! More iterations
      DO jk=2,jkmax
        ztmke=FLOAT(itmk(jk-1,:,:))
        DO jj=2,ny-1
          DO ji=2,nx-1
            IF( ANY( ( zfsite(ji-1:ji+1,jj-1:jj+1)<0.5 .OR.  &
                     itmk(jk-1,ji-1:ji+1,jj-1:jj+1)==0 ).AND.  &
                     ztmke0(ji-1:ji+1,jj-1:jj+1)>0.5 ) ) THEN
                ztmke(ji,jj)=0.
            ENDIF
          END DO
        END DO
#if ! defined in_surfex
        CALL gltools_bound( 'T','scalar',ztmke )
#endif
        itmk(jk,:,:)=INT(ztmke)
      END DO
!
! Now we have a mask defining which points are not too close to the
! ice edge
      itmk0 = SPREAD( itmk(jkmax,:,:),1,nt ) 
!
! END FUTURE ROUTINE
!
! * Correct unrealistic non-solar heat flux
      WHERE( tzsit(:,:,:)%tsf<245.-t0deg .AND. tzsit(:,:,:)%fsi>epsil1 .AND.  &
      itmk0==0 ) 
        tpglt%blki(:,:,:)%nsf = AMAX1(  &
          tpglt%blki(:,:,:)%nsf,  &
          tpglt%blki(:,:,:)%dfl*AMAX1( tzsit(:,:,:)%tsf-(233.-t0deg),0. ) ) 
      ENDWHERE
!
! 
! 2.4. Fluxes on water
! ---------------------
!
! Non solar heat flux
      tpglt%blkw(:,:)%nsf = tpglt%atm_mix(1,:,:)%nsf +   &
        tpglt%atm_mix(1,:,:)%dfl * ( tpglt%tml(:,:)%tml-ztsfm(:,:) )
!
! Solar heat flux
      tpglt%blkw(:,:)%swa = tpglt%atm_mix(1,:,:)%swa *  &
        ( 1.-albw ) / ( 1.-zalbm(:,:) )
!
! Derivative of non solar heat flux
      tpglt%blkw(:,:)%dfl = tpglt%atm_mix(1,:,:)%dfl

! * Evaporation
!       - The water fluxes are now in kg.m-2.s-1 throughout the code
      tpglt%blkw(:,:)%eva = tpglt%atm_mix(1,:,:)%eva
!
  ENDIF
!
!
! 2.5. Diagnostics
! -----------------
! 
! Diagnose the total Input Fresh Water flux
  tpglt%dia(:,:)%ifw = tpglt%atm_all(:,:)%sop + tpglt%atm_all(:,:)%lip +  &
    ( 1.-zfsit(:,:) )*tpglt%blkw(:,:)%eva +  &
    SUM( tzsit(:,:,:)%fsi*tpglt%blki(:,:,:)%eva, DIM=1 )
!
! Diagnose the Input Solar Flux to Sea Ice
  tpglt%dia(:,:)%swi = SUM( tzsit(:,:,:)%fsi*tpglt%blki(:,:,:)%swa, DIM=1 )
!
! Diagnose the Input Solar Flux to Leads
  tpglt%dia(:,:)%sww = ( 1.-zfsit(:,:) )*tpglt%blkw(:,:)%swa
!
END SUBROUTINE glt_getatmf
!
! --------------------- END SUBROUTINE glt_getatmf --------------------------
! -----------------------------------------------------------------------
