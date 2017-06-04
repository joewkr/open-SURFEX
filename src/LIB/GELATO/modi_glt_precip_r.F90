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
! ========================= MODULE modi_glt_precip_r ========================
! =======================================================================
!
! Goal:
! -----
!   This module contains a subroutine that takes the physical effect 
! of precipitations on sea ice. Depending on the kind of precipitation,
! snow accumulation can take place, the density of the snow layer 
! increasing in time; or if it rains, the bottom of the snow layer
! can be transformed into ice...
!    Note that the physics treated by this routine is altered by 
! nrn2ice flag (for the time being local to this routine)
!   ==> If put to 1, the rain is transformed into ice as it 
! comes in contact with the surface snow, if snow has reached its 
! maximum density. However, perfect gltools_enthalpy conservation is not 
! ensured with this method.
!  ==> If put to 0, if snow has reached its maximum density, more 
! rain goes to the mixed layer in an energy conservating way. 
! 
!
! Method:
! -------
!   Based on Douville (1995)
! 
! Created : 1996/10 (D. Salas y Melia)
!           Later on modified for rewriting.
! Modified: 2009/06 (D. Salas y Melia) 
!           Reduced grid
! Modified: 2009/07 (D. Salas y Melia) 
!           Second possible treatment for rain / snow layer interactions.
!           Overwhelming energy due to rainfall does not go to the
!           mixed layer, but more logically to the heat diffusion scheme
! Modified: 2009/12 (D. Salas y Melia)
!           New treatment of energy (gltools_enthalpy replaces temperature as 
!           a reference variable)
! Modified: (A. Voldoire)
!           new ice/water fluxes interface CALL
!
! --------------------- BEGIN MODULE modi_glt_precip_r ----------------------
!
!THXS_SFX!MODULE modi_glt_precip_r
!THXS_SFX!INTERFACE 
!THXS_SFX!!
!THXS_SFX!SUBROUTINE glt_precip_r( orain,osnow,tpmxl,tpatm,tpsit,tpsil,tptfl,tpdia,  &
!THXS_SFX!  pqmelt )
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  USE modd_glt_param
!THXS_SFX!  LOGICAL, DIMENSION(np), INTENT(in) ::  &
!THXS_SFX!        orain,osnow
!THXS_SFX!  TYPE(t_mxl), DIMENSION(np), INTENT(in) ::  &
!THXS_SFX!        tpmxl
!THXS_SFX!  TYPE(t_atm), DIMENSION(np), INTENT(in) ::  &
!THXS_SFX!        tpatm   
!THXS_SFX!  TYPE(t_sit), DIMENSION(nt,np), INTENT(inout) ::  &
!THXS_SFX!        tpsit
!THXS_SFX!  TYPE(t_vtp), DIMENSION(nl,nt,np), INTENT(inout) ::  &
!THXS_SFX!        tpsil
!THXS_SFX!  TYPE(t_tfl), DIMENSION(np), INTENT(inout) ::  &
!THXS_SFX!        tptfl
!THXS_SFX!  TYPE(t_dia), DIMENSION(np), INTENT(inout) ::  &
!THXS_SFX!        tpdia
!THXS_SFX!  REAL, DIMENSION(nt,np), INTENT(inout) ::   &
!THXS_SFX!        pqmelt
!THXS_SFX!END SUBROUTINE glt_precip_r
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_glt_precip_r
!
! ---------------------- END MODULE modi_glt_precip_r -----------------------
!
!
!
! -----------------------------------------------------------------------
! ------------------------ SUBROUTINE glt_precip_r --------------------------
!
! * Subroutine which takes physical effects of precipitations over ice 
! or snow into account, except the case of rain over non snow covered
! sea ice.
!
SUBROUTINE glt_precip_r( orain,osnow,tpmxl,tpatm,tpsit,tpsil,tptfl,tpdia,  &
  pqmelt )
!
  USE modd_glt_const_thm
  USE modd_types_glt
  USE modd_glt_param
  USE mode_gltools_enthalpy
  USE modi_glt_updtfl_r
!
  IMPLICIT NONE
!
  LOGICAL, DIMENSION(np), INTENT(in) ::  &
        orain,osnow
  TYPE(t_mxl), DIMENSION(np), INTENT(in) ::  &
        tpmxl
  TYPE(t_atm), DIMENSION(np), INTENT(in) ::  &
        tpatm   
  TYPE(t_sit), DIMENSION(nt,np), INTENT(inout) ::  &
        tpsit
  TYPE(t_vtp), DIMENSION(nl,nt,np), INTENT(inout) ::  &
        tpsil
  TYPE(t_tfl), DIMENSION(np), INTENT(inout) ::  &
        tptfl
  TYPE(t_dia), DIMENSION(np), INTENT(inout) ::  &
        tpdia
  REAL, DIMENSION(nt,np), INTENT(inout) ::   &
        pqmelt
!
  INTEGER ::  &
        jk,jl,jp
  INTEGER, PARAMETER ::  &
        nrn2ice=0
  REAL, DIMENSION(np) ::  &
        zpcps,zpcpr,zfsit,zwork,zqm
  REAL, DIMENSION(nt,np) ::  &
        zrsn,zhsn,zmsn,zt,zdmwat,zent,zentsn,zsalt
        real,dimension(np) :: zei1,zei2,zes1,zes2
!        real,dimension(nl,nt,np) :: zenth
!
!
!
! 1. Array initializations
! ========================
!
! .. 3D real arrays
!
  pqmelt(:,:) = 0.
  zdmwat(:,:) = 0.
!  CALL glt_aventh(tpsit,tpsil,zei1,zes1)
!print*,'rsn =',tpsit%rsn
!print*,'hsn =',tpsit%hsn
!print*,'msn =',tpsit%hsn*tpsit%rsn
!print*,'hsi =',tpsit%hsi
!print*,'Enthalpie avant =',zei1+zes1
!do jk=1,nt
!  do jl=1,nilay
!  zenth(jl,jk,:) = rhoice*tpsit(jk,:)%hsi*tpsit(jk,:)%fsi*sf3tinv(jl)* &
!  tpsil(jl,jk,:)%ent
!  end do
!  zenth(jl,jk,:) = tpsit(jk,:)%rsn * tpsit(jk,:)%fsi * tpsit(jk,:)%hsn * &
!  tpsil(nilay+1,jk,:)%ent
!end do

!
! .. Save snow density before new computations
!
  zrsn(:,:) = tpsit(:,:)%rsn
!
! .. We assume that new snow massic gltools_enthalpy is the massic gltools_enthalpy of
! snow (salinity=0) of temperature tsf 
!
  zsalt(:,:) = 0.
  zentsn(:,:) = tpsil(nl,:,:)%ent
  DO jl=nilay+1,nl
    WHERE( tpsit(:,:)%esi .AND. tpsit(:,:)%hsn<epsil1 )
      tpsil(jl,:,:)%ent = zentsn(:,:)
    ENDWHERE
  END DO
!
!
!
! 2. Snow aging includes snow density increase 
! ============================================
! 
! .. The density increase is exponential towards rhosnwmax
!
  WHERE ( tpsit(:,:)%esi )
      tpsit(:,:)%rsn = rhosnwmax +  &
        (tpsit(:,:)%rsn-rhosnwmax)*exp(-tauf*dtt/xday2sec)
      tpsit(:,:)%hsn =  &
        tpsit(:,:)%hsn * zrsn(:,:) / tpsit(:,:)%rsn
  ENDWHERE 
!
  WHERE ( .NOT.tpsit(:,:)%esi )
      tpsit(:,:)%rsn = rhosnwmin
      tpsit(:,:)%hsn = 0.
  ENDWHERE
!
!
!
! 3. Effect of precipitations
! ===========================
!
! 3.1. Snow and rain amounts in kg.m-2
! -------------------------------------
!
  zpcps(:) = dtt * tpatm(:)%sop
  zpcpr(:) = dtt * tpatm(:)%lip
!
! .. Now in case there is precipitation, update tpsit
  tpdia(:)%s_pr = 0.
  tpdia(:)%o_pr = 0.
!
  DO jk = 1,nt
!
!
! 3.2. Precipitation is rain
! --------------------------
!
! .. The superficial snow layer absorbs rain and its density increases.
! But as rain is transformed into ice, some latent heat is released.
! If there is no snow layer, the rain is just drained through the ice.
!
! .. New snow thickness and density reference
!
    zhsn(jk,:) = tpsit(jk,:)%hsn
    zrsn(jk,:) = tpsit(jk,:)%rsn
    zmsn(jk,:) = zhsn(jk,:)*zrsn(jk,:)
!
    zwork(:) = 0.
!    print*,'avant =',tpsil(nilay+1,jk,:)%ent
    WHERE ( orain(:) .AND. tpsit(jk,:)%esi .AND.  &
    zhsn(jk,:)>epsil1 )
!
! .. Density increase without increase in snow layer thickness
      tpsit(jk,:)%rsn =  &
        ( zmsn(jk,:) + zpcpr(:) ) / zhsn(jk,:)
      zwork(:) =  zpcpr(:)*tpsit(jk,:)%fsi / dtt
    ENDWHERE
!    print*,'apres =',tpsil(nilay+1,jk,:)%ent
!    print*,'fact  =',zmsn(jk,:) / ( zmsn(jk,:)+zpcpr(:) )
    tpdia(:)%o_pr =  tpdia(:)%o_pr + zpcpr(:)*tpsit(jk,:)%fsi / dtt - zwork(:)
    tpdia(:)%s_pr =  tpdia(:)%s_pr + zwork(:)
!
!
! .. If there is no snow layer on ice: water goes directly to the ocean
! (it is assumed this happens not by percolation through sea ice, but goes 
! to the leads)
!
    WHERE ( orain(:) .AND. tpsit(jk,:)%esi .AND.  &
    zhsn(jk,:)<=epsil1 )
      tptfl(:)%wlo = tptfl(:)%wlo +  &
        tpsit(jk,:)%fsi*zpcpr(:)/dtt
    ENDWHERE
!
!
! 3.3. Handle case where snow density > max. density (Method I)
! --------------------------------------------------------------
!
! .. If calculated snow density is more than maximum density, but still
! less than rhoice, then part of the snow is transformed into ice, and
! the rest remains at rhosnwmax.
!   - this is rather physical, however it creates a slight energetic
! inconsistency (sea ice thickness increases without any temperature
! change, modifying the gltools_enthalpy stored in sea ice)
!   ==> it should be decided by a namelist parameter whether we do 
!   this or decide to deliver the precipitation water to the mixed
! layer
!
    IF ( nrn2ice == 1 ) THEN    ! Transform snow into ice 
!
        WHERE ( orain(:) .AND. tpsit(jk,:)%esi .AND.  &
        tpsit(jk,:)%hsn>epsil1 .AND.  &
        tpsit(jk,:)%rsn>rhosnwmax .AND. tpsit(jk,:)%rsn<rhoice )
          tpsit(jk,:)%hsi = tpsit(jk,:)%hsi + tpsit(jk,:)%hsn*  &
            (tpsit(jk,:)%rsn-rhosnwmax)/(rhoice-rhosnwmax)
          tpsit(jk,:)%hsn = tpsit(jk,:)%hsn*  &
            (rhoice-tpsit(jk,:)%rsn)/(rhoice-rhosnwmax) 
          tpsit(jk,:)%rsn = rhosnwmax
        ENDWHERE
!
! .. If calculated snow density is more than ice density, then all the
! snow is turned into sea ice
!
        WHERE ( orain(:) .AND. tpsit(jk,:)%esi .AND.  &
        tpsit(jk,:)%hsn>epsil1 .AND. tpsit(jk,:)%rsn>rhoice )
          tpsit(jk,:)%hsi = tpsit(jk,:)%hsi + tpsit(jk,:)%hsn
          tpsit(jk,:)%hsn = 0.
          tpsit(jk,:)%rsn = rhosnwmin
        ENDWHERE
!
! .. pqmelt contributes to melting snow, not ice (goes to the mixed layer
! instead)
!
        WHERE ( orain(:) .AND. tpsit(jk,:)%esi .AND. pqmelt(jk,:)>0. )
          tpsit(jk,:)%hsn = tpsit(jk,:)%hsn -  &
            dtt*pqmelt(jk,:) / ( xmhofusn0*tpsit(jk,:)%rsn )
          pqmelt(jk,:) = -AMIN1( 0.,tpsit(jk,:)%hsn )*  &
            xmhofusn0*tpsit(jk,:)%rsn / dtt
        ENDWHERE
!
! zqmelt  should go to the mixed layer in this case, not to the surface of sea
! ice !
!
        WHERE ( orain(:) .AND.  &
        (.NOT. tpsit(jk,:)%esi .OR. tpsit(jk,:)%hsn<epsil1) )
          tpsit(jk,:)%hsn = 0.
          tpsit(jk,:)%rsn = rhosnwmin
        ENDWHERE
!
!
! 3.4. Handle case where snow density > max. density (Method II)
! -------------------------------------------------------------
!
! We do not transform snow into ice (in excess rain goes to the ocean)
!
      ELSE
! 
        WHERE ( orain(:) .AND. tpsit(jk,:)%esi .AND.  &
        tpsit(jk,:)%hsn>epsil1 .AND. tpsit(jk,:)%rsn>rhosnwmax  )
          zdmwat(jk,:) = -zhsn(jk,:)*tpsit(jk,:)%fsi*  &
            ( tpsit(jk,:)%rsn-rhosnwmax )
          tpsit(jk,:)%rsn = rhosnwmax
!          pqmelt(jk,:) =  &
!            zhsn(jk,:)*( tpsit(jk,:)%rsn-zrsn(jk,:) ) *  &
!              ( xmhofusn0 + tpmxl(:)%hco ) / dtt
        ENDWHERE
        tpdia(:)%s_pr = tpdia(:)%s_pr + zdmwat(jk,:) / dtt
        tpdia(:)%o_pr = tpdia(:)%o_pr - zdmwat(jk,:) / dtt
!
    ENDIF
!
!
! 3.5. Precipitation is snow
! --------------------------
!
    zwork(:) = 0.
    WHERE ( osnow(:) .AND. tpsit(jk,:)%esi ) 
!
! .. Total mass of snow (current snow layer + new snowfalls)
      zqm(:) =  &
        tpsit(jk,:)%hsn*tpsit(jk,:)%rsn + zpcps(:)
!
! .. Total snow thickness (current snow layer + new snowfalls) 
      tpsit(jk,:)%hsn =  &
        tpsit(jk,:)%hsn + zpcps(:)/rhosnwmin
!
! .. New density is the ratio of new snow mass over new snow thickness
      tpsit(jk,:)%rsn =  &
        zqm(:) / tpsit(jk,:)%hsn
      zwork(:) = zpcps(:) * tpsit(jk,:)%fsi / dtt
    ENDWHERE
    tpdia(:)%s_prsn = tpdia(:)%s_prsn + zwork(:)
!
    zwork(:) = 0.
    WHERE ( osnow(:) .AND. .NOT.tpsit(jk,:)%esi )
      tpsit(jk,:)%hsn = 0.
      tpsit(jk,:)%rsn = rhosnwmin
      zwork(:) = zpcps(:) * tpsit(jk,:)%fsi / dtt
    ENDWHERE
    tpdia(:)%o_prsn = tpdia(:)%o_prsn + zwork(:)
!
! .. The change in snow density due to rainfall is not related to a change
! in snow thickness, but to a liquid water input. Here the temperature of 
! liquid precipitation is unknown, so we suppose that the gltools_enthalpy of snow
! does not change. To achieve this, we have to modify the snow gltools_enthalpy per
! mass unit to avoid a change in snow layer enthalpy.
!
do jp=1,np
if ( orain(jp) .AND. tpsit(jk,jp)%esi .AND.  &
      tpsit(jk,jp)%hsn>epsil1 ) then
!      print*,'essai'
    DO jl=nilay+1,nl
!!      WHERE( orain(:) .AND. tpsit(jk,:)%esi .AND.  &
!!      tpsit(jk,:)%hsn>epsil1 )
!!            tpsil(jl,jk,:)%ent = tpsil(jl,jk,:)%ent *  &
!!              zrsn(jk,:) / tpsit(jk,:)%rsn
!print*,'avant =',tpsil(jl,jk,jp)%ent
        tpsil(jl,jk,jp)%ent =   &
          tpsil(jl,jk,jp)%ent* ( zmsn(jk,jp) + zpcps(jp) ) /  &
          ( tpsit(jk,jp)%rsn * tpsit(jk,jp)%hsn )
!print*,'apres =',tpsil(jl,jk,jp)%ent
!!      ENDWHERE
!!
    END DO
endif
end do
!
!
! .. Without any information about rain temperature, we assume that rain falls
! at 0C (i.e. gltools_enthalpy = 0), hence total snow gltools_enthalpy does not change (i.e. 
! new snow massic gltools_enthalpy is:
!    enth_snow = enth_snow_old * mass_snow_old / (mass_snow_old + mass_rain)
!      tpsil(nilay+1,jk,:)%ent = tpsil(nilay+1,jk,:)%ent *  &
!        zmsn(jk,:) / ( zmsn(jk,:)+zpcpr(:) )
!
  END DO
!
!
! 3.6. Collect water that goes to the mixed layer
! ------------------------------------------------
!
! glt_updtfl_r cannot be used here, since rain has no energy as in input 
! (see updbud.f90), just consider the water flux 
!  ok since the gltools_enthalpy flux is now optional in glt_updtfl_r
!
! This is tricky... in excess rain has to go to the %wlo flux to be transmitted
! to the ocean (if the ocean model assumes levitating sea ice). 
! If this flux is put in %wio, it won't be transmitted to the ocean and water 
! (coming from the atmosphere) will be lost.
!
  IF ( nrn2ice==0 ) THEN
    CALL glt_updtfl_r('FW2O',tpmxl,tptfl,zdmwat)
  ENDIF  
!
!
! 4. Correct to avoid further problems due to computer precision
! ==============================================================
!
  zt(:,:) = tpsit(:,:)%rsn
  tpsit(:,:)%rsn = AMAX1( zt(:,:),rhosnwmin )
  zt(:,:) = tpsit(:,:)%rsn
  tpsit(:,:)%rsn = AMIN1( zt(:,:),rhosnwmax )
  zt(:,:) = tpsit(:,:)%hsn
  tpsit(:,:)%hsn = AMAX1( zt(:,:),0. )
!
!
! 5. AR5 diagnostics
! ===================
!
! .. Compute total sea ice concentration
!
  zfsit(:) = SUM( tpsit(:,:)%fsi ,DIM=1 )
!
! .. Rain flux falling on the sea ice part of the grid cell 
!
  tpdia(:)%lip = zfsit(:)*tpatm(:)%lip
!
! .. Snow flux falling on the sea ice part of the grid cell 
!
  tpdia(:)%sop = zfsit(:)*tpatm(:)%sop
!CALL glt_aventh(tpsit,tpsil,zei2,zes2)
!print*,'Enthalpie apres  =',zei2+zes2
!print*,'Delta Enthalpie  =',(zei2+zes2-zei1-zes1)/dtt
!print*,'Delta Enthalpie neige =',(zes2-zes1)/dtt
!print*,'rsn=',tpsit(:,:)%rsn 
!print*,'hsn=',tpsit(:,:)%hsn 
!print*,'msn =',tpsit%hsn*tpsit%rsn
!print*,zpcpr
!print*,'hsi=',tpsit(:,:)%hsi
!do jk=1,nt
!print*,'jk=',jk
!  do jl=1,nilay
!print*,'d ent jl =',jl, &
!  ( rhoice*tpsit(jk,:)%hsi*tpsit(jk,:)%fsi*sf3tinv(jl)* &
!  tpsil(jl,jk,:)%ent - zenth(jl,jk,:) )/dtt
!  end do
!print*,'d ent snow=', &
!  ( tpsit(jk,:)%rsn * tpsit(jk,:)%fsi * tpsit(jk,:)%hsn * &
!  tpsil(nilay+1,jk,:)%ent-zenth(nilay+1,jk,:) )/dtt
!end do
!
END SUBROUTINE glt_precip_r
!
! ---------------------- END SUBROUTINE glt_precip_r ------------------------
! -----------------------------------------------------------------------
