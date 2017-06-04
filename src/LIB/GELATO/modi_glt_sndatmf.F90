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
! ========================= MODULE modi_glt_sndatmf =========================
! =======================================================================
!
! This routine was created for Gelato version 3, i.e. Gelato is under 
! the form of a routine inserted in the OPA 8 code. 
! It allows Gelato to transmit the forcing the atmosphere model needs as 
! a routine glt_output argument.
!  
! Created : 10/1999 (D. Salas y Melia)
! Modified: 08/2009 (D. Salas y Melia) Manages single or double physics      
! Modified: 07/2012 (D. Salas y Melia) Parallelism
!
! --------------------- BEGIN MODULE modi_glt_sndatmf -----------------------
!
!THXS_SFX!MODULE modi_glt_sndatmf
!THXS_SFX!INTERFACE
!THXS_SFX!! 
!THXS_SFX!SUBROUTINE glt_sndatmf(tpglt)
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  USE modd_glt_param
!THXS_SFX!  TYPE(t_glt), INTENT(inout) ::  &
!THXS_SFX!    tpglt
!THXS_SFX!END SUBROUTINE glt_sndatmf
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_glt_sndatmf
!
! ---------------------- END MODULE modi_glt_sndatmf ------------------------
!
!
! -----------------------------------------------------------------------
! ----------------------- SUBROUTINE glt_sndatmf ----------------------------

SUBROUTINE glt_sndatmf(tpglt, xtmlf)
  USE modd_types_glt
  USE modd_glt_param
  USE modd_glt_const_thm
!USE MODI_ABOR1_SFX
#if ! defined in_surfex
  USE mode_gltools_bound
  USE modi_gltools_advmsk
  USE modi_gltools_expand
#endif
!
  IMPLICIT NONE 
!
  TYPE(t_glt), INTENT(inout)  ::  &
    tpglt
  ! Useful in Surfex init phase, when SST+SSS are not yet known, and hence 
  ! tml%mlf not yet filled in, but one wants a sensible value for TICE everywhere:
  REAL, OPTIONAL, INTENT(IN)  ::  &
    xtmlf
!
  INTEGER, PARAMETER ::  &
    jporder=5 
  INTEGER, DIMENSION(jporder,SIZE(tpglt%dom,1),SIZE(tpglt%dom,2)) ::  &
    iadvmsk
  REAL, DIMENSION(SIZE(tpglt%dom,1),SIZE(tpglt%dom,2)) ::  &
    zalbc
  REAL, DIMENSION(SIZE(tpglt%dom,1),SIZE(tpglt%dom,2)) ::  &
    zalbm,ztsfm,zfsit
  TYPE(t_sit),  &
    DIMENSION(SIZE(tpglt%sit,1),SIZE(tpglt%sit,2),SIZE(tpglt%sit,3)) ::  &
    tzsit
!
!
!
! 1. Initializations 
! ===================
!
! Get ice state from super-type
!
  tzsit = tpglt%sit
!
! Total sea ice cover (fraction of unity)
  zfsit(:,:) =  &
    SUM( tzsit(:,:,:)%fsi,DIM=1 )*FLOAT( tpglt%dom(:,:)%tmk )
#if ! defined in_surfex
  CALL gltools_bound( 'T','scalar',zfsit ) 
#endif
!
! Stratus clouds albedo 
  zalbc(:,:) = 0.
  WHERE( zfsit(:,:)>=0.05 )
    zalbc(:,:) = alblc
  ENDWHERE
!
!
!
! 2. Double physics case
! =======================
!
  IF ( nnflxin/=0 ) THEN
!
!
! 2.1. Only ice cats-averaged data is sent to the atmosphere
! -----------------------------------------------------------
!
! .. Define only sea ice surface data. The open water fraction can be deduced 
! from total sea ice fraction, its temperature is computed by the ocean and
! its albedo is normally computed by the atmospheric model.
!
      IF ( nnflxin==1 ) THEN
! Ice average temperature and albedo (without stratus clouds)
          WHERE( zfsit(:,:)>epsil5 )
            ztsfm(:,:) =  &
              SUM( tzsit(:,:,:)%fsi*tzsit(:,:,:)%tsf,DIM=1 ) / zfsit(:,:)
            zalbm(:,:) =  &
              SUM( tzsit(:,:,:)%fsi*tzsit(:,:,:)%asn,DIM=1 ) / zfsit(:,:)
          ENDWHERE
          IF (PRESENT(XTMLF)) THEN 
             WHERE( zfsit(:,:)<=epsil5 )
                ztsfm(:,:) = xtmlf
             ENDWHERE
          ELSE
             WHERE( zfsit(:,:)<=epsil5 )
                ztsfm(:,:) = tpglt%tml(:,:)%mlf
             ENDWHERE
          ENDIF
          WHERE( zfsit(:,:)<=epsil5 )
            zalbm(:,:) = albi
          ENDWHERE
!
! Add stratus clouds (if alblc /= 0)
          zalbm(:,:) = 1.-( 1.-zalbm(:,:) )*( 1.-zalbc(:,:) )
!
#if ! defined in_surfex
! Extend the data near the ice edge to avoid interpolation problems when
! coupling (note that concentration should not be extended, but ice temperature
! and albedo should)
          iadvmsk(:,:,:) = gltools_advmsk( jporder, tpglt%dom, zfsit(:,:)>epsil5 )
          ztsfm(:,:) = gltools_expand( iadvmsk,ztsfm )
          zalbm(:,:) = gltools_expand( iadvmsk,zalbm )
#endif
!
! Define glt_output structure: gltools_bound, gather and broadcast
!
! Ice fraction
          tpglt%ice_atm(1,:,:)%fsi = zfsit(:,:)
!
! Ice albedo
#if ! defined in_surfex
          CALL gltools_bound( 'T','scalar',zalbm )
#endif
          tpglt%ice_atm(1,:,:)%alb = zalbm(:,:)
!
! Ice temperature
#if ! defined in_surfex
          CALL gltools_bound( 'T','scalar',ztsfm ) 
#endif
          ztsfm(:,:) = ztsfm(:,:)+t0deg
          tpglt%ice_atm(1,:,:)%tsf = ztsfm(:,:)
!
        ELSE
!
!
! 2.2. Data from every ice category is sent to the atmosphere
! ------------------------------------------------------------
!
! NOTE : A DATA EXTENSION SHOULD ALSO BE APPLIED HERE !!
!
! Ice fraction
          tpglt%ice_atm(:,:,:)%fsi = tzsit(:,:,:)%fsi
!
! Ice albedo
          tpglt%ice_atm(:,:,:)%alb = tzsit(:,:,:)%asn 
!
! Ice temperature
          tpglt%ice_atm(:,:,:)%tsf = tzsit(:,:,:)%tsf + t0deg
!
      ENDIF
    ELSE
!
!
!
! 3. Single physics case
! =======================
!
! Weighted (ice+ocean) temperature and albedo (without stratus clouds) 
      zalbm(:,:) =  &
        SUM( tzsit(:,:,:)%fsi*tzsit(:,:,:)%asn,DIM=1 ) +  &
        ( 1.-zfsit(:,:) )*albw
! Add stratus (no effect if alblc = 0 in gltpar)
      zalbm(:,:) = 1.-( 1.-zalbm(:,:) )*( 1.-zalbc(:,:) )
!
! Weighted surface temperature
      ztsfm(:,:) =  &
        SUM( tzsit(:,:,:)%fsi*tzsit(:,:,:)%tsf,DIM=1 ) +  &
        ( 1.-zfsit(:,:) )*tpglt%tml(:,:)%tml + t0deg
!
! Apply boundary conditions
#if ! defined in_surfex
      CALL gltools_bound( 'T','scalar',zalbm )
      CALL gltools_bound( 'T','scalar',ztsfm )
#endif
!
! Define glt_output arrays
!
! Ice fraction
      tpglt%mix_atm(1,:,:)%fsi = zfsit(:,:)
!
! Ice albedo
      tpglt%mix_atm(1,:,:)%alb = zalbm(:,:) 
!
! Ice temperature
      tpglt%mix_atm(1,:,:)%tsf = ztsfm(:,:) 
!
  ENDIF
!
END SUBROUTINE glt_sndatmf
!
! --------------------- END SUBROUTINE glt_sndatmf --------------------------
! -----------------------------------------------------------------------
