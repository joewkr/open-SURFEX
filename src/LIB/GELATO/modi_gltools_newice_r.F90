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
! ======================= MODULE modi_gltools_newice_r ====================
! =======================================================================
!
!
! * Completely define a new ice category from input information
!
! Created : 2012/03 (D. Salas y Melia)
! Modified: No
!
! ------------------- BEGIN MODULE modi_gltools_newice_r ------------------
!
!THXS_SFX!MODULE modi_gltools_newice_r
!THXS_SFX!INTERFACE
!THXS_SFX!!
!THXS_SFX!SUBROUTINE gltools_newice_r   &
!THXS_SFX!  ( pfsi,phsi,tpmxl,tpsit,tpsil,  &
!THXS_SFX!    ptsf,pssi,phsn,prsn,pasn,pent )
!THXS_SFX!!
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  USE modd_glt_param
!THXS_SFX!!
!THXS_SFX!  REAL, DIMENSION(nt,np), INTENT(in) ::  &
!THXS_SFX!        pfsi,phsi
!THXS_SFX!  TYPE(t_mxl), DIMENSION(np), INTENT(inout) ::  &
!THXS_SFX!        tpmxl
!THXS_SFX!  TYPE(t_sit), DIMENSION(nt,np), INTENT(inout) ::  &
!THXS_SFX!        tpsit
!THXS_SFX!  TYPE(t_vtp), DIMENSION(nl,nt,np), INTENT(inout) ::  &
!THXS_SFX!        tpsil
!THXS_SFX!  REAL, DIMENSION(nt,np), OPTIONAL, INTENT(inout) ::  &
!THXS_SFX!        ptsf,pssi,phsn,prsn,pasn
!THXS_SFX!  REAL, DIMENSION(nl,nt,np), OPTIONAL, INTENT(inout) ::  &
!THXS_SFX!        pent
!THXS_SFX!END SUBROUTINE gltools_newice_r
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_gltools_newice_r
!
! -------------------- END MODULE modi_gltools_newice_r -------------------
!
!
! -----------------------------------------------------------------------
! ------------------------- SUBROUTINE gltools_newice_r -------------------------
!
SUBROUTINE gltools_newice_r  &
  ( pfsi,phsi,tpmxl,tpsit,tpsil,  &
    ptsf,pssi,phsn,prsn,pasn,pent )
!
!
!
! 1. Declarations
! ===============
!
! 1.1. Module declarations
! ------------------------
!
  USE modd_types_glt
  USE modd_glt_param
  USE modd_glt_const_thm
  USE mode_gltools_enthalpy
  USE mode_glt_info_r
  USE mode_glt_stats_r
!
  IMPLICIT NONE
!
!
! 1.2. Dummy arguments declarations
! ---------------------------------
!
! .. INTENT(in) arguments.
!
  REAL, DIMENSION(nt,np), INTENT(in) ::  &
        pfsi,phsi
  TYPE(t_mxl), DIMENSION(np), INTENT(inout) ::  &
        tpmxl
!
! .. INTENT(inout) arguments.
!
  TYPE(t_sit), DIMENSION(nt,np), INTENT(inout) ::  &
        tpsit
  TYPE(t_vtp), DIMENSION(nl,nt,np), INTENT(inout) ::  &
        tpsil
!
! .. OPTIONAL, INTENT(in) arguments
!
  REAL, DIMENSION(nt,np), OPTIONAL, INTENT(inout) ::  &
        ptsf,pssi,phsn,prsn,pasn
  REAL, DIMENSION(nl,nt,np), OPTIONAL, INTENT(inout) ::  &
        pent
!
!
! 1.3. Local variables declarations
! ---------------------------------
!
  INTEGER ::  &
        jl,jk,jp
  REAL, DIMENSION(nt,np) ::  &
        ztsf,zssi,zhsn,zrsn,zasn
  REAL, DIMENSION(nl,nt,np) ::  &
        zent
!
!
!
! 2. Define new ice
! ==================
!
! 2.1. Handle missing arguments
! ------------------------------
!
  IF ( PRESENT(ptsf) ) THEN
    ztsf(:,:) = ptsf(:,:)
  ELSE
    ztsf(:,:) = SPREAD( tpmxl(:)%mlf,1,nt )
  ENDIF
  IF ( PRESENT(pssi) ) THEN
    zssi(:,:) = pssi(:,:)
  ELSE
    zssi(:,:) = sice
  ENDIF
  IF ( PRESENT(phsn) ) THEN
    zhsn(:,:) = phsn(:,:)
  ELSE
    zhsn(:,:) = 0.
  ENDIF
  IF ( PRESENT(prsn) ) THEN
    zrsn(:,:) = prsn(:,:)
  ELSE
    zrsn(:,:) = rhosnwmin
  ENDIF
  IF ( PRESENT(pasn) ) THEN
    zasn(:,:) = pasn(:,:)
  ELSE
    WHERE( zhsn(:,:) > epsil1 )
      zasn(:,:) = rhosnwmax
    ELSEWHERE
      zasn(:,:) = albi
    ENDWHERE
  ENDIF
!
! .. For enthalpy, assume that the temperature is equal to surface temperature
! over the vertical
  IF ( PRESENT(pent) ) THEN 
    zent(:,:,:) = pent(:,:,:)
  ELSE
    zent(:,:,:) = SPREAD( glt_enthalpy2d( ztsf(:,:),zssi(:,:) ),1,nl ) 
  ENDIF

! 
! 2.2. Define lead sea ice state variable
! ---------------------------------------
!
! Compute all ice state variables for sea ice for
!
! ..Sea ice 3D variables. 
!
  WHERE( pfsi(:,:)>=epsil1 .AND. tpsit(:,:)%fsi<=epsil1 )
    tpsit(:,:)%esi = .TRUE.
    tpsit(:,:)%age = 0.
    tpsit(:,:)%asn = zasn(:,:)
    tpsit(:,:)%fsi = pfsi(:,:)
    tpsit(:,:)%hsi = phsi(:,:)
    tpsit(:,:)%ssi = zssi(:,:)
    tpsit(:,:)%hsn = zhsn(:,:)
    tpsit(:,:)%rsn = zrsn(:,:)
    tpsit(:,:)%tsf = ztsf(:,:)
  ENDWHERE
!
! .. Sea ice 4D variables.
!
  DO jl=1,nl
    WHERE( pfsi(:,:)>=epsil1 .AND. tpsit(:,:)%fsi<=epsil1 )
      tpsil(jl,:,:)%ent = zent(jl,:,:)
    ENDWHERE
  END DO
! 
END SUBROUTINE gltools_newice_r
!
! ---------------------- END SUBROUTINE gltools_newice_r ------------------------
! -----------------------------------------------------------------------
