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
! ======================== MODULE modi_glt_mltvtp_r =========================
! =======================================================================
!
! Goal:
! -----
!   This module contains a subroutine that updates vertical temperature
! profiles in sea ice, only in case of sea ice melting.
!   The updated thickness of every sea ice level should be provided as
! an input (4D pdhi array), as well as the sea ice object variable 
! before thickness is updated.
!
! Method:
! -------
!
! Created : 2007/12 (D. Salas y Melia)
! Modified: 2009/06 (D. Salas y Melia) Reduced grid
! 
! ---------------------- BEGIN MODULE modi_glt_mltvtp_r ---------------------
!
!THXS_SFX!MODULE modi_glt_mltvtp_r
!THXS_SFX!INTERFACE
!THXS_SFX!!
!THXS_SFX!SUBROUTINE glt_mltvtp_r( pdhi,phsi,tpsil )
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  USE modd_glt_param
!THXS_SFX!  REAL, DIMENSION(nilay,nt,np), INTENT(in) ::  &
!THXS_SFX!        pdhi
!THXS_SFX!  REAL, DIMENSION(nt,np), INTENT(inout) ::  &
!THXS_SFX!        phsi
!THXS_SFX!  TYPE(t_vtp), DIMENSION(nl,nt,np), INTENT(inout) ::  &
!THXS_SFX!        tpsil   
!THXS_SFX!END SUBROUTINE glt_mltvtp_r
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_glt_mltvtp_r
!
! ----------------------- END MODULE modi_glt_mltvtp_r ----------------------
!
!
! -----------------------------------------------------------------------
! ------------------------ SUBROUTINE glt_mltvtp_r --------------------------
!
! * Subroutine used to update sea ice vertical temperature profile, due 
! to sea ice melting.
!
SUBROUTINE glt_mltvtp_r( pdhi,phsi,tpsil )
!
  USE modd_glt_const_thm
  USE modd_types_glt
  USE modd_glt_param
  USE mode_glt_stats_r
  USE mode_gltools_interp
!
  IMPLICIT NONE
!
  REAL, DIMENSION(nilay,nt,np), INTENT(in) ::  &
        pdhi
  REAL, DIMENSION(nt,np), INTENT(inout) ::  &
        phsi
  TYPE(t_vtp), DIMENSION(nl,nt,np), INTENT(inout) ::   &
        tpsil
!
  INTEGER ::  &
        jp,jk,jl
  REAL :: zavti,zavtf,zdavt
  REAL, DIMENSION(nilay) ::  &
        zentn,zsf3tinvo
  REAL, DIMENSION(nilay+1) ::  &
        zlevo
  REAL, DIMENSION(nt,np) ::  &
        zhsi
!
!
!
! 1. Update vertical temperature profile
! =======================================
!
! Compute new sea ice thickness
  zhsi(:,:) = SUM( pdhi(:,:,:),DIM=1 )
!
  DO jp=1,np
    DO jk=1,nt
      IF ( zhsi(jk,jp)>0. .AND. zhsi(jk,jp)<phsi(jk,jp) ) THEN
! Normalized level boundaries in the melted ice slab (former)
          zsf3tinvo=pdhi(:,jk,jp) / zhsi(jk,jp)
          zlevo(1)=0.
          DO jl=2,nilay+1
            zlevo(jl)=zlevo(jl-1)+pdhi(jl-1,jk,jp)
          END DO
          zlevo(:)=zlevo/zlevo(nilay+1)
! Interpolate
          zentn=glt_interpz( height,tpsil(1:nilay,jk,jp)%ent,zlevo )
! In principle, the following is now impossible...
          IF (lp3) THEN
              zavti=SUM( zsf3tinvo*tpsil(1:nilay,jk,jp)%ent )
              zavtf=SUM( sf3tinv*zentn )
              zdavt=ABS(zavtf-zavti)
              IF ( zdavt>epsil5 ) THEN
                  print*,'PB in mltvtp at jp,jk =',jp,jk
                  print*,'Average temperature is not conserved.'
                  print*,'  Initial   =',zavti 
                  print*,'  Final     =',zavtf
              ENDIF
          ENDIF
          tpsil(1:nilay,jk,jp)%ent = zentn
      ENDIF
    END DO
  END DO
!
! .. Update phsi
!
  phsi(:,:) = zhsi(:,:)
!
END SUBROUTINE glt_mltvtp_r
!
! ---------------------- END SUBROUTINE glt_mltvtp_r ------------------------
! -----------------------------------------------------------------------
