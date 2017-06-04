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
! ===================== MODULE mode_gltools_enthalpy ======================
! =======================================================================
!
! Goal:
! -----
!   This module contains two functions that allow to compute sea ice or 
! snow massic gltools_enthalpy (J.kg-1) from temperature (and salinity in the 
! case of sea ice).
!   - enthalpy3d:
!   In addition to temperature, salinity profile can also be passed 
! to the routine (optionally).
!   - enthalpy0d:
!   Computes gltools_enthalpy at only one point. As we are not dealing with a
! vertical profile, we do not know a priori if we are dealing with snow
! or ice. Hence for glt_enthalpy0d the salinity argument is compulsory. If 
! salinity is 0, we know that we are dealing with snow.
!
! Created : 12/2009 (D. Salas y Melia)
! Modified: 02/2010 (D. Salas y Melia) 
!   Introduction of glt_enthalpy1d and glt_enthalpy2d (to avoid CALL glt_enthalpy0d
!   in loops)
!  
! ----------------- BEGIN MODULE mode_gltools_enthalpy -------------------- 
!
MODULE mode_gltools_enthalpy
INTERFACE
!
SUBROUTINE glt_aventh(tpsit,tpsil,pentsi,pentsn)
  USE modd_glt_param
  USE modd_types_glt
  TYPE(t_sit), DIMENSION(nt,np), INTENT(in) ::  &
    tpsit
  TYPE(t_vtp), DIMENSION(nl,nt,np), INTENT(in) ::  &
    tpsil
  REAL, DIMENSION(np), INTENT(out) ::  &
    pentsi,pentsn
END SUBROUTINE glt_aventh
!
FUNCTION glt_enthalpy0d(pt,ps)
  REAL, INTENT(in) ::  &
    pt
  REAL, INTENT(in) ::  &
    ps
  REAL ::  &
    glt_enthalpy0d
END FUNCTION glt_enthalpy0d
!
FUNCTION glt_enthalpy1d(gmsk,pt,ps)
  USE modd_glt_param, only: np
  LOGICAL, DIMENSION(np), INTENT(in) ::  &
    gmsk
  REAL, DIMENSION(np), INTENT(in) ::  &
    pt
  REAL, DIMENSION(np), INTENT(in) ::  &
    ps
  REAL, DIMENSION(np) ::  &
    glt_enthalpy1d
END FUNCTION glt_enthalpy1d
!
FUNCTION glt_enthalpy2d(pt,ps)
  USE modd_glt_param, only: np,nt
  REAL, DIMENSION(nt,np), INTENT(in) ::  &
    pt
  REAL, DIMENSION(nt,np), INTENT(in) ::  &
    ps
  REAL, DIMENSION(nt,np) ::  &
    glt_enthalpy2d
END FUNCTION glt_enthalpy2d
!
FUNCTION glt_enthalpy3d(pvtp,pvsp)
  USE modd_glt_param, only: nl,np,nt, nilay
  REAL, DIMENSION(nl,nt,np), INTENT(in) ::  &
    pvtp
  REAL, DIMENSION(nl,nt,np), OPTIONAL, INTENT(in) ::  &
    pvsp
  REAL, DIMENSION(nl,nt,np) ::  &
    glt_enthalpy3d
END FUNCTION glt_enthalpy3d
!
END INTERFACE
END MODULE mode_gltools_enthalpy
!
! ------------------ END MODULE mode_gltools_enthalpy ---------------------
!
!
! -----------------------------------------------------------------------
! ------------------------- SUBROUTINE glt_aventh ---------------------------
!
!  Computes the total gltools_enthalpy (J.m-2) of sea-ice and snow (separately) 
! in an ice-snow slab
!
SUBROUTINE glt_aventh(tpsit,tpsil,pentsi,pentsn)
  USE modd_glt_const_thm
  USE modd_glt_param
  USE modd_types_glt
  TYPE(t_sit), DIMENSION(nt,np), INTENT(in) ::  &
    tpsit
  TYPE(t_vtp), DIMENSION(nl,nt,np), INTENT(in) ::  &
    tpsil
  REAL, DIMENSION(np), INTENT(out) ::  &
    pentsi,pentsn
!
  INTEGER ::  &
    jl
  REAL, DIMENSION(nt,np) ::  &
    zmsi,zmsn
!
! Enthalpy in the ice part of the slab
  zmsi(:,:) = rhoice * tpsit(:,:)%fsi * tpsit(:,:)%hsi
  pentsi(:) = 0.
  DO jl=1,nilay
    pentsi(:) = pentsi(:) +  &
      SUM( sf3tinv(jl) * zmsi(:,:) * tpsil(jl,:,:)%ent, DIM=1 )
  END DO
!
! Enthalpy in the snow part of the slab
  zmsn(:,:) = tpsit(:,:)%rsn * tpsit(:,:)%fsi * tpsit(:,:)%hsn
  pentsn(:) =  &
    SUM( zmsn(:,:)*SUM( tpsil(nilay+1:nl,:,:)%ent,DIM=1 ), DIM=1 ) /  &
     FLOAT(nslay)
!
END SUBROUTINE glt_aventh
!
! ------------------------- END SUBROUTINE glt_aventh -----------------------
! -----------------------------------------------------------------------
!
!
! -----------------------------------------------------------------------
! ------------------------ FUNCTION glt_enthalpy0d --------------------------
!
!   The input arguments are temperature profile, in Celsius
! and salinity (g.kg-1). Note that both arguments are compulsory.
!
FUNCTION glt_enthalpy0d(pt,ps)
!
  USE modd_glt_param
  USE modd_glt_const_thm
!
  IMPLICIT NONE
!
  REAL, INTENT(in) ::  &
    pt
  REAL, INTENT(in) ::  &
    ps
  REAL ::  &
    glt_enthalpy0d
!
  REAL ::  &
    ztice_m
!
!
! 1. Initializations
! ===================
!
! ..  Compute sea ice melting point as a function of salinity
!
  ztice_m = -mu * ps
!
! 
! 2. If the slab is salty ice
! ============================
!
!* Compute the amount of energy needed to raise sea ice temperature to
! melting point and melt sea ice completely
!
  IF ( ps>0. ) THEN
! If temperature is lower than melting point
    IF ( pt<ztice_m ) THEN
        glt_enthalpy0d = cpice0*( pt-ztice_m ) - xmhofusn0*( 1.-ztice_m/pt )
      ELSE
! If temperature is melting point
        glt_enthalpy0d = 0.
    ENDIF
!
!* Add a term for the energy needed to increase the meltwater temperature to 0
! Celsius
!
    glt_enthalpy0d = glt_enthalpy0d + cpsw*ztice_m
!
  ELSE
!
! 
! 3. If the slab is pure ice
! ===========================
!
    IF ( pt<0. ) THEN
      glt_enthalpy0d = cpice0*pt - xmhofusn0
    ELSE
      glt_enthalpy0d = 0.
    ENDIF
!
  ENDIF   
!
END FUNCTION glt_enthalpy0d
!
! ------------------------ FUNCTION glt_enthalpy0d --------------------------
! -----------------------------------------------------------------------
!
!
! -----------------------------------------------------------------------
! ------------------------ FUNCTION glt_enthalpy1d --------------------------
!
!   The input arguments are temperature, in Celsius and salinity (g.kg-1)
! spatial fields. Note that both arguments are compulsory.
!
FUNCTION glt_enthalpy1d(gmsk,pt,ps)
!
  USE modd_glt_param, only: np
  USE modd_glt_const_thm
!
  IMPLICIT NONE
!
  LOGICAL, DIMENSION(np), INTENT(in) ::  &
    gmsk
  REAL, DIMENSION(np), INTENT(in) ::  &
    pt
  REAL, DIMENSION(np), INTENT(in) ::  &
    ps
  REAL, DIMENSION(np) ::  &
    glt_enthalpy1d
!
  integer ::  &
    jp
  REAL, DIMENSION(np) ::  &
    ztice_m
!
!
! THE CASE OF PURE ICE SHOULD BE ADDRESSED AS WELL
!
! 1. Initializations
! ===================
!
do jp=1,np
  IF( gmsk(jp) ) then
!
! ..  Compute sea ice melting point as a function of salinity
!
    ztice_m(jp) = -mu * ps(jp)
!
! 
! 2. Enthalpy of the sea ice part of the slab
! ============================================
!
!* Compute the amount of energy needed to raise sea ice temperature to
! melting point and melt sea ice completely
!
! If temperature is lower than melting point
IF ( pt(jp)<ztice_m (jp) ) then
        glt_enthalpy1d(jp) =  &
          cpice0*( pt(jp)-ztice_m(jp) ) -  &
          xmhofusn0*( 1.-ztice_m(jp)/pt(jp) )
  ELSE
! If temperature is melting point
        glt_enthalpy1d(jp) = cpice0*( pt(jp)-ztice_m(jp) )
ENDIF
!
!* Add a term for the energy needed to increase the meltwater temperature to 0
! Celsius
!
    glt_enthalpy1d(jp) = glt_enthalpy1d(jp) + cpsw*ztice_m(jp)
!
  ELSE
!
!
! 3. Masked part of the domain
! =============================
!
    glt_enthalpy1d(jp) = 0.
!
  ENDIF
  end do
!
END FUNCTION glt_enthalpy1d
!
! ------------------------ FUNCTION glt_enthalpy1d --------------------------
! -----------------------------------------------------------------------
!
!
! -----------------------------------------------------------------------
! ------------------------ FUNCTION glt_enthalpy2d --------------------------
!
!   The input arguments are temperature, in Celsius and salinity (g.kg-1)
! spatial fields. Note that both arguments are compulsory.
!
FUNCTION glt_enthalpy2d(pt,ps)
!
  USE modd_glt_param, only: np,nt
  USE modd_glt_const_thm
!
  IMPLICIT NONE
!
  REAL, DIMENSION(nt,np), INTENT(in) ::  &
    pt
  REAL, DIMENSION(nt,np), INTENT(in) ::  &
    ps
  REAL, DIMENSION(nt,np) ::  &
    glt_enthalpy2d
!
  REAL, DIMENSION(nt,np) ::  &
    ztice_m
!
!
! THE CASE OF PURE ICE SHOULD BE ADDRESSED AS WELL
!
! 1. Initializations
! ===================
!
! ..  Compute sea ice melting point as a function of salinity
!
  ztice_m(:,:) = -mu * ps(:,:)
!
! 
! 2. Enthalpy of the sea ice part of the slab
! ============================================
!
!* Compute the amount of energy needed to raise sea ice temperature to
! melting point and melt sea ice completely
!
! If temperature is lower than melting point
  WHERE ( pt(:,:)<ztice_m (:,:) )
      glt_enthalpy2d(:,:) =  &
        cpice0*( pt(:,:)-ztice_m(:,:) ) -  &
        xmhofusn0*( 1.-ztice_m(:,:)/pt(:,:) )
    ELSEWHERE
! If temperature is melting point
      glt_enthalpy2d(:,:) = cpice0*( pt(:,:)-ztice_m(:,:) )
  ENDWHERE
!
!* Add a term for the energy needed to increase the meltwater temperature to 0
! Celsius
!
    glt_enthalpy2d(:,:) = glt_enthalpy2d(:,:) + cpsw*ztice_m(:,:)
!
END FUNCTION glt_enthalpy2d
!
! ------------------------ FUNCTION glt_enthalpy2d --------------------------
! -----------------------------------------------------------------------
!
!
! -----------------------------------------------------------------------
! ------------------------ FUNCTION glt_enthalpy3d --------------------------
!
!   The input arguments are sea ice vertical temperature profile, in 
! Celsius and, if available, vertical salinity profile (g.kg-1).
!
FUNCTION glt_enthalpy3d(pvtp,pvsp)
!
  USE modd_glt_param, only: nl,np,nt, nilay
  USE modd_glt_const_thm
!
  IMPLICIT NONE
!
  REAL, DIMENSION(nl,nt,np), INTENT(in) ::  &
    pvtp
  REAL, DIMENSION(nl,nt,np), OPTIONAL, INTENT(in) ::  &
    pvsp
  REAL, DIMENSION(nl,nt,np) ::  &
    glt_enthalpy3d
!
  INTEGER ::  &
    jl
  REAL, DIMENSION(nl,nt,np) ::  &
    ztice_m
!
!
! 1. Initializations
! ===================
!
! ..  Compute sea ice melting point as a function of salinity
  IF ( PRESENT(pvsp) ) THEN
      ztice_m(:,:,:) = -mu * pvsp(:,:,:)
    ELSE
      ztice_m(1:nilay,:,:) = -mu * sice 
      ztice_m(nilay+1:nl,:,:) = 0.
  ENDIF
!
! 
! 2. Enthalpy of the sea ice part of the slab
! ============================================
!
  DO jl=1,nilay
!
!* Compute the amount of energy needed to raise sea ice temperature to
! melting point and melt sea ice completely
!
! If temperature is lower than melting point
    WHERE ( pvtp(jl,:,:)<ztice_m(jl,:,:) )
        glt_enthalpy3d(jl,:,:) =  &
          cpice0*( pvtp(jl,:,:)-ztice_m(jl,:,:) ) -  &
          xmhofusn0*( 1.-ztice_m(jl,:,:)/pvtp(jl,:,:) )
      ELSEWHERE    
! If temperature is melting point
        glt_enthalpy3d(jl,:,:) = 0.
    ENDWHERE
!
!* Add a term for the energy needed to increase the meltwater temperature to 0C
!
    glt_enthalpy3d(jl,:,:) = glt_enthalpy3d(jl,:,:) + cpsw*ztice_m(jl,:,:)
!
  END DO
!
!
! 3. Enthalpy of the snow part of the slab
! =========================================
!
  DO jl=nilay+1,nl
    glt_enthalpy3d(jl,:,:) = cpice0*pvtp(jl,:,:)-xmhofusn0
  END DO 
!
END FUNCTION glt_enthalpy3d
!
! ----------------------- END FUNCTION glt_enthalpy3d ----------------------- 
! -----------------------------------------------------------------------
