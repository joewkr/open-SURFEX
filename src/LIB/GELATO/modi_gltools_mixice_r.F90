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
! ====================== MODULE modi_gltools_mixice_r ===================== 
! =======================================================================
!
! Goal:
! -----
!   This module contains functions and subroutines which are used as 
! tools in the rest of the Gelato model. Some of them are purely 
! thermodynamic functions, others are used to have some input and
! glt_output of interest displayed in a convenient way.
!
! Created : 1996/04 (D. Salas y Melia)
!           Case of a 1-D model at one point
! Modified: 1997/03 (D. Salas y Melia)
!           Adapted to a 2-D model and rewritten to follow the DOCTOR 
!           norm.
! Modified: 2007/11 (D. Salas y Melia)
!           Merger should be done for surface temperature also 
!           (or it is subsequently set to melting point in the 
!           particular case of an ice class change, creating problems)
! Modified: 2009/06 (D. Salas y Melia)
!           Reduced grid version
!
! ------------------ BEGIN MODULE modi_gltools_mixice_r -------------------  
!
!THXS_SFX!MODULE modi_gltools_mixice_r
!THXS_SFX!INTERFACE
!THXS_SFX!!  
!THXS_SFX!SUBROUTINE gltools_mixice_r( tpmxl,tplsit,tplsil,tpsit,tpsil ) 
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  USE modd_glt_param
!THXS_SFX!  TYPE(t_mxl),DIMENSION(np), INTENT(in) ::                           &
!THXS_SFX!        tpmxl
!THXS_SFX!  TYPE(t_sit), DIMENSION(:,:,:), INTENT(in) ::                        &
!THXS_SFX!        tplsit  
!THXS_SFX!  TYPE(t_vtp), DIMENSION(:,:,:,:), INTENT(in) ::                      &
!THXS_SFX!        tplsil          
!THXS_SFX!  TYPE(t_sit), DIMENSION(nt,np), INTENT(inout) ::                    &
!THXS_SFX!        tpsit 
!THXS_SFX!  TYPE(t_vtp), DIMENSION(nl,nt,np), INTENT(inout) ::                 &
!THXS_SFX!        tpsil
!THXS_SFX!END SUBROUTINE gltools_mixice_r
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_gltools_mixice_r
!
! ------------------- END MODULE modi_gltools_mixice_r --------------------
!
!
!
! -----------------------------------------------------------------------
! ------------------------ SUBROUTINE gltools_mixice_r --------------------------
!
!   This subroutine is used for merging several ice classes together, 
! given their areal fractions, overlying snow layers thickness and
! density, and vertical temperature profiles. 
!
SUBROUTINE gltools_mixice_r( tpmxl,tplsit,tplsil,tpsit,tpsil )
!
  USE modd_types_glt
  USE modd_glt_const_thm
  USE modd_glt_param
!
  IMPLICIT NONE
!
  TYPE(t_mxl),DIMENSION(np), INTENT(in) ::                           &
        tpmxl
  TYPE(t_sit), DIMENSION(:,:,:), INTENT(in) ::                        &
        tplsit
  TYPE(t_vtp), DIMENSION(:,:,:,:), INTENT(in) ::                      &
        tplsil
  TYPE(t_sit), DIMENSION(nt,np), INTENT(inout) ::                    &
        tpsit
  TYPE(t_vtp), DIMENSION(nl,nt,np), INTENT(inout) ::                 &
        tpsil
  INTEGER ::                                                            &
        jl,jt
  REAL, DIMENSION(nt,np) ::                                          &
        zmlf3,zvsi,zvsn,zmsn,zagevsi,zssivsi,zaux,zvmpvsi
!
!
!
! 1. Compute auxiliary array
! ==========================
!
! .. Expanded mixed layer freezing point
!
  zmlf3(:,:) = SPREAD(tpmxl(:)%mlf,1,nt) 
!
! .. For every ice category, volume of ice per sq. meter
!
  zvsi(:,:) = SUM( tplsit(:,:,:)%fsi*tplsit(:,:,:)%hsi,DIM=1 )
!
! .. For every ice category, volume and mass of snow cover per sq. meter
!
  zvsn(:,:) = SUM( tplsit(:,:,:)%fsi*tplsit(:,:,:)%hsn,DIM=1 )
  zmsn(:,:) = SUM( tplsit(:,:,:)%fsi*tplsit(:,:,:)%hsn*  &
    tplsit(:,:,:)%rsn,DIM=1 )
!
! .. For every ice category, volume x age
!
  IF ( niceage==1 )  &
    zagevsi(:,:) = SUM( tplsit(:,:,:)%fsi*tplsit(:,:,:)%hsi*  &
      tplsit(:,:,:)%age,DIM=1 )
!
! .. For every ice category, volume x ssi
!
  IF ( nicesal==1 )  &
    zssivsi(:,:) = SUM( tplsit(:,:,:)%fsi*tplsit(:,:,:)%hsi*  &
      tplsit(:,:,:)%ssi,DIM=1 )
!
! .. For every ice category, volume x vmp
!
  IF ( nmponds==1 )  &
    zvmpvsi(:,:) = SUM( tplsit(:,:,:)%fsi*tplsit(:,:,:)%hsi*  &
      tplsit(:,:,:)%vmp,DIM=1 )
!
!
! 2. Compute all ice state variables
! ==================================
!
! 2.1. Compute merged sea ice fraction
! ------------------------------------
!
! .. Compute merged sea ice fractions. It is simply the sum of the 
! concentrations of the different ice types_glt that fell into the same
! thickness category.
!
  tpsit(:,:)%fsi = SUM(tplsit(:,:,:)%fsi,DIM=1)
!
!
! 2.2. Compute other merged 3D quantities
! ---------------------------------------
! 
! .. Compute existence boolean, merged sea ice thicknesses, snow
! thickness, surface temperature:
!
  WHERE ( tpsit(:,:)%fsi>epsil1 )
    tpsit(:,:)%esi = .TRUE.
    tpsit(:,:)%hsi = zvsi(:,:) / tpsit(:,:)%fsi
    tpsit(:,:)%hsn = zvsn(:,:) / tpsit(:,:)%fsi
    tpsit(:,:)%tsf = SUM(  &
      tplsit(:,:,:)%fsi*tplsit(:,:,:)%tsf, DIM=1 ) /  &
      tpsit(:,:)%fsi 
    tpsit(:,:)%asn = SUM(  &
      tplsit(:,:,:)%fsi*tplsit(:,:,:)%asn, DIM=1 ) /  &
      tpsit(:,:)%fsi 
  ENDWHERE
  WHERE ( tpsit(:,:)%fsi<=epsil1 )
    tpsit(:,:)%esi = .FALSE.
    tpsit(:,:)%hsi = 0.
    tpsit(:,:)%hsn = 0.
    tpsit(:,:)%tsf = zmlf3(:,:)
    tpsit(:,:)%asn = albw
  ENDWHERE
!
! .. Compute snow density:
!
  WHERE ( tpsit(:,:)%fsi>epsil1 .AND. tpsit(:,:)%hsn>epsil1 )   
    tpsit(:,:)%rsn = zmsn(:,:) /  &
      ( tpsit(:,:)%fsi*tpsit(:,:)%hsn )
  ENDWHERE
  WHERE ( tpsit(:,:)%fsi<=epsil1 .OR. tpsit(:,:)%hsn<=epsil1 )
    tpsit(:,:)%rsn = rhosnwmin
  ENDWHERE
!
! .. Compute ice age:
!
  IF ( niceage==1 ) THEN
      WHERE ( tpsit(:,:)%fsi>epsil1 .AND. tpsit(:,:)%hsi>epsil1 )   
        tpsit(:,:)%age = zagevsi(:,:) /  &
          ( tpsit(:,:)%fsi*tpsit(:,:)%hsi )
      ENDWHERE
      WHERE ( tpsit(:,:)%fsi<=epsil1 .OR. tpsit(:,:)%hsi<=epsil1 )
        tpsit(:,:)%age = 0.
      ENDWHERE
    ELSE
      tpsit(:,:)%age = 0.
  ENDIF
!
! .. Compute ice salinity
!
  IF ( nicesal==1 ) THEN
      WHERE ( tpsit(:,:)%fsi>epsil1 .AND. tpsit(:,:)%hsi>epsil1 )   
        tpsit(:,:)%ssi = zssivsi(:,:) /  &
          ( tpsit(:,:)%fsi*tpsit(:,:)%hsi )
      ENDWHERE
      WHERE ( tpsit(:,:)%fsi<=epsil1 .OR. tpsit(:,:)%hsi<=epsil1 )
        tpsit(:,:)%ssi = 0.
      ENDWHERE
  ENDIF
!
! .. Compute melt pond volume:
!
  IF ( nmponds==1 ) THEN
      WHERE ( tpsit(:,:)%fsi>epsil1 .AND. tpsit(:,:)%hsi>epsil1 )   
        tpsit(:,:)%vmp = zvmpvsi(:,:) /  &
          ( tpsit(:,:)%fsi*tpsit(:,:)%hsi )
      ENDWHERE
      WHERE ( tpsit(:,:)%fsi<=epsil1 .OR. tpsit(:,:)%hsi<=epsil1 )
        tpsit(:,:)%vmp = 0.
      ENDWHERE
    ELSE
      tpsit(:,:)%vmp = 0.
  ENDIF
! 
!
! 2.3. Compute merged 4D quantities
! ---------------------------------
!
! .. For the time being, only the temperature is concerned. 
!
! Ice
!
  DO jl = 1,nilay
    zaux(:,:) = SUM( tplsit(:,:,:)%fsi*tplsit(:,:,:)%hsi*          &
      tplsil(:,jl,:,:)%ent,DIM=1 )
    WHERE ( tpsit(:,:)%fsi>epsil1 .AND. tpsit(:,:)%hsi>epsil1 )
      tpsil(jl,:,:)%ent = zaux(:,:) /  &
        ( tpsit(:,:)%fsi*tpsit(:,:)%hsi )
    ENDWHERE
  END DO
!
! Snow
!
  DO jl = nilay+1,nl
    zaux(:,:) = SUM( tplsit(:,:,:)%fsi*tplsit(:,:,:)%hsn*         &
      tplsit(:,:,:)%rsn*tplsil(:,jl,:,:)%ent,DIM=1 )
    WHERE ( tpsit(:,:)%fsi>epsil1 .AND. tpsit(:,:)%hsn>epsil1 .AND. &
    tpsit(:,:)%rsn>epsil1 )
      tpsil(jl,:,:)%ent = zaux(:,:) /  &
        ( tpsit(:,:)%fsi*tpsit(:,:)%hsn*tpsit(:,:)%rsn )
    ENDWHERE
  END DO
!
  DO jl = 1,nl
    WHERE ( tpsit(:,:)%fsi<=epsil1 .OR. tpsit(:,:)%hsi<=epsil1 )
      tpsil(jl,:,:)%ent = 0.
    ENDWHERE
  END DO
!
END SUBROUTINE gltools_mixice_r
!
! ----------------------- END SUBROUTINE gltools_mixice_r -----------------------
! -----------------------------------------------------------------------
