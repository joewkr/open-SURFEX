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
! ======================== MODULE modi_glt_frzvtp_r =========================
! =======================================================================
!
! Goal:
! -----
!   This module contains a subroutine that updates vertical tracer 
! profiles in sea ice, depending on whether bottom ablation or accretion
! took place.
!   Note: here phsi is the sea ice thickness field before bottom freezing,
! and pqfac represents the heat flux used to freeze sea ice at the
! bottom of the slab.
!
! Method:
! -------
!
! Created : 1996/10 (D. Salas y Melia)
! Modified: 2009/10 (D. Salas y Melia) Reduced grid
! Modified: 2009/12 (D. Salas y Melia) Replace temperature field with 
!   an gltools_enthalpy field
! 
! ---------------------- BEGIN MODULE modi_glt_frzvtp_r ---------------------
!
!THXS_SFX!MODULE modi_glt_frzvtp_r
!THXS_SFX!INTERFACE 
!THXS_SFX!!
!THXS_SFX!SUBROUTINE glt_frzvtp_r( tpmxl,tpsit,pqfac,phsi,pssi,tpsil )
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  USE modd_glt_param
!THXS_SFX!  TYPE(t_mxl), DIMENSION(np), INTENT(in) ::  &
!THXS_SFX!        tpmxl
!THXS_SFX!  TYPE(t_sit), DIMENSION(nt,np), INTENT(inout) ::  &
!THXS_SFX!        tpsit
!THXS_SFX!  REAL, DIMENSION(nt,np), INTENT(in) ::  &
!THXS_SFX!        pqfac
!THXS_SFX!  REAL, DIMENSION(nt,np), INTENT(inout) ::  &
!THXS_SFX!        phsi
!THXS_SFX!  REAL, DIMENSION(nt,np), INTENT(out) ::  &
!THXS_SFX!        pssi
!THXS_SFX!  TYPE(t_vtp), DIMENSION(nl,nt,np), INTENT(inout) ::  &
!THXS_SFX!        tpsil   
!THXS_SFX!END SUBROUTINE glt_frzvtp_r
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_glt_frzvtp_r
!
! ----------------------- END MODULE modi_glt_frzvtp_r ----------------------
!
!
! -----------------------------------------------------------------------
! ------------------------ SUBROUTINE glt_frzvtp_r --------------------------
!
! * Subroutine used to update sea ice vertical tracer profile, due 
! to sea ice thickness increase from the bottom of the slab.
!
! * Note that in the list of arguments, phsi is sea ice new thickness,
! not the thickness variation.
!
SUBROUTINE glt_frzvtp_r( tpmxl,tpsit,pqfac,phsi,pssi,tpsil )
!
  USE modd_glt_const_thm
  USE modd_types_glt
  USE modd_glt_param
  USE mode_gltools_enthalpy
  USE mode_gltools_interp
  USE modi_glt_saltrap_r
!
  IMPLICIT NONE
!
  TYPE(t_mxl), DIMENSION(np), INTENT(in) ::  &
        tpmxl
  TYPE(t_sit), DIMENSION(nt,np), INTENT(inout) ::  &
        tpsit
  REAL, DIMENSION(nt,np), INTENT(in) ::  &
        pqfac
  REAL, DIMENSION(nt,np), INTENT(inout) ::  &
        phsi
  REAL, DIMENSION(nt,np), INTENT(out) ::  &
        pssi
  TYPE(t_vtp), DIMENSION(nl,nt,np), INTENT(inout) ::   &
        tpsil
!
  INTEGER ::  &
        jp,jk,jl
  LOGICAL, DIMENSION(np) ::  &
        yfreeze
  REAL ::  &
        zavei,zavef,zdave,zhsinew
  REAL, DIMENSION(nilay) ::  &
        zentn
  REAL, DIMENSION(nilay+1) ::  &
        zsf3tinvo,zento
  REAL, DIMENSION(nilay+2) ::  &
        zlevo
  REAL, DIMENSION(np) ::  &
        zssib,zentb,zdhsib,ztem
!
!
!
! 1. Update vertical temperature profile and mixed layer water flux
! ==================================================================
!
  DO jk=1,nt
!
! Define where the salt trapping process should be applied
    yfreeze(:) = ( pqfac(jk,:)<0. )
!
! Compute the salinity, gltools_enthalpy and thickness of the new ice
    ztem(:) = -2.
    CALL glt_saltrap_r( yfreeze,pqfac(jk,:),ztem(:),tpmxl,zssib,zentb,zdhsib )
!
    DO jp=1,np
      IF ( pqfac(jk,jp)<0. ) THEN
!
! The former vertical gltools_enthalpy profile consists in two parts:
! a) Newly frozen ice at the bottom of the slab
          zento(1)=zentb(jp)
! b) Rest of the slab
          zento(2:nilay+1)=tpsil(1:nilay,jk,jp)%ent
!
! Normalized vertical level scale factors + level boundaries in an ice 
! slab that froze from the bottom:
!  - the nilay former layers + one new layer corresponding to the sea ice
! thickness increase (this newly formed sea ice is at sea water freezing
! point temperature).
          zsf3tinvo(1)=zdhsib(jp)
          zsf3tinvo(2:nilay+1)=sf3tinv(:)*phsi(jk,jp)
          zsf3tinvo(:)=zsf3tinvo/SUM(zsf3tinvo)
          zlevo(1)=0.
          DO jl=2,nilay+2
            zlevo(jl)=zlevo(jl-1)+zsf3tinvo(jl-1)
          END DO
          zlevo(:)=zlevo/zlevo(nilay+2)
!
! Interpolate
          zentn=glt_interpz( height,zento,zlevo )
          tpsil(1:nilay,jk,jp)%ent = zentn(:)
!
! Compute initial average gltools_enthalpy in the ice slab
          zavei = SUM( zsf3tinvo*zento )
!
! Compute final average gltools_enthalpy in the ice slab
          zavef = SUM( sf3tinv*tpsil(1:nilay,jk,jp)%ent )
!
! Print out possible errors
          zdave = ABS( zavef-zavei )
!
! In principle, this is now impossible...
          IF (lp3) THEN
              IF ( zdave>1.e-5 ) THEN
                  WRITE(noutlu,*) 'jp= ',jp,' jk= ',jk
                  WRITE(noutlu,*)  &
                    '    Difference in av. vert. temp. =',  &
                    ( zavef-zavei )
              ENDIF
          ENDIF
!
! Update salinity and thickness
          zhsinew = phsi(jk,jp)+zdhsib(jp)          
          tpsit(jk,jp)%ssi =  &
            ( tpsit(jk,jp)%ssi*phsi(jk,jp)+zssib(jp)*zdhsib(jp) ) / zhsinew
          phsi(jk,jp) = zhsinew
          pssi(jk,jp) = zssib(jp)
!
        ELSE
! It is important to define a salinity anyway here (else NaNs will occur in
! glt_updhsi_r !) - since pssi is INTENT(out) - unlike phsi
          pssi(jk,jp) = tpsit(jk,jp)%ssi
      ENDIF
    END DO
  END DO
!
END SUBROUTINE glt_frzvtp_r

! ---------------------- END SUBROUTINE glt_frzvtp_r ------------------------
! -----------------------------------------------------------------------
