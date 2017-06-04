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
! ========================= MODULE modi_glt_salflx ==========================
! =======================================================================
!
! Goal:
! -----
!   This module contains a subroutine that is computes the
! concentration / dilution flux, due to desalination processes (ice 
! mass flux = 0) or freezing / melting of sea ice (ice mass flux /= 0)
!
! Method:
! --------
!   Three different methods can be used: 
!      - simple, approximated method
!      - exact calculation, ocean surface salinity is the true salinity 
!      - exact calculation, ocean surface salinity is the ref. salinity 
!
! Input:
! ------
!   pdmass (the provided mass of melted ice or snow) is in kg.m-2. 
! In the case of sea ice:
!            (sea ice density) * (fraction) * (thickness variation)
!   pqsalt (the provided salt flux) is in g.m-2. 
! In the case of sea ice melting / freezing, it is:
!            pdmass * ( ice salinity )
! In the case of sea ice desalination: 
!  (sea ice density) * (fraction) * (thickness) * (salinity variation)
!
! Created : 2010/03 (D. Salas y Melia)
! Modified: 2010/06 (D. Salas y Melia) glt_salflx on full grid
! Modified: 2010/07 (D. Salas y Melia) Take ssh into account
! Modified: 2011/12 (A. Voldoire) New formulation of the %cio flux
!           (method 4)
!
! ---------------------- BEGIN MODULE modi_glt_salflx -----------------------
!
!THXS_SFX!MODULE modi_glt_salflx
!THXS_SFX!INTERFACE 
!THXS_SFX!!
!THXS_SFX!SUBROUTINE glt_salflx( pqsalt,tpmxl,tptfl,pdmass,psalt )
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  USE modd_glt_param
!THXS_SFX!  REAL, DIMENSION(nt,nx,ny), INTENT(in) ::  &
!THXS_SFX!        pqsalt
!THXS_SFX!  TYPE(t_mxl), DIMENSION(nx,ny), INTENT(in) ::  &
!THXS_SFX!        tpmxl
!THXS_SFX!  TYPE(t_tfl), DIMENSION(nx,ny), INTENT(inout) ::  &
!THXS_SFX!        tptfl
!THXS_SFX!  REAL, DIMENSION(nt,nx,ny),OPTIONAL, INTENT(in) ::  &
!THXS_SFX!        pdmass,psalt
!THXS_SFX!END SUBROUTINE glt_salflx
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_glt_salflx
!
! ----------------------- END MODULE modi_glt_salflx ------------------------
!
!
!
! -----------------------------------------------------------------------
! -------------------------- SUBROUTINE glt_salflx --------------------------
!
SUBROUTINE glt_salflx( pqsalt,tpmxl,tptfl,pdmass,psalt )
!
  USE modd_glt_const_thm
  USE modd_types_glt
  USE modd_glt_param
!
  IMPLICIT NONE
!
  REAL, DIMENSION(nt,nx,ny), INTENT(in) ::  &
        pqsalt
  TYPE(t_mxl), DIMENSION(nx,ny), INTENT(in) ::  &
        tpmxl
  TYPE(t_tfl), DIMENSION(nx,ny), INTENT(inout) ::  &
        tptfl
  REAL, DIMENSION(nt,nx,ny),OPTIONAL, INTENT(in) ::  &
        pdmass,psalt
!
! .. Local variables
!
  REAL, DIMENSION(nt,nx,ny) ::  &
        zaux,zsml,zssh,zdmass,zsalt
  REAL, parameter :: ssmr=34.
!
!
!
! 1. Initialisations
! ===================
!
  IF ( PRESENT(pdmass) ) THEN
    zdmass(:,:,:) = pdmass(:,:,:) 
  ELSE
    zdmass(:,:,:) = 0.
  ENDIF
!
  IF ( PRESENT(psalt) ) THEN
    zsalt(:,:,:) = psalt(:,:,:) 
  ELSE
    zsalt(:,:,:) = 0.
  ENDIF
!
!
!
! 2. Compute virtual water flux (concentration/dilution)
! -------------------------------------------------------
!
! Method 1:
!  -> we suppose that the change in ice thickness during a time step is 
! negligible compared to the thickness of the uppermost ocean level
!
  zsml(:,:,:) = SPREAD( tpmxl(:,:)%sml,1,nt )
  IF ( ncdlssh==1 ) THEN
    zssh(:,:,:) = SPREAD( tpmxl(:,:)%ssh,1,nt )
  ELSE
    zssh(:,:,:) = 0.
  ENDIF
!
  IF ( nsalflx==1 ) THEN
    tptfl(:,:)%cio = tptfl(:,:)%cio -  &
      SUM( zdmass(:,:,:) - pqsalt(:,:,:)/AMAX1( zsml(:,:,:),epsil1 ),  &
          DIM=1) / dtt
!
! Method 2:
!  -> we do not make the approximation that the change in ice thickness 
! during a time step is negligible compared to the thickness of the uppermost 
! ocean level
!
  ELSE IF ( nsalflx==2 ) THEN
    zaux(:,:,:) = 1. / ( rhofw + zsalt(:,:,:) )
    tptfl(:,:)%cio = tptfl(:,:)%cio -       &
      SUM( ( rhoice*zdmass(:,:,:)*zaux(:,:,:) -  &
          rhoice/rhosw * pqsalt(:,:,:)/AMAX1( zsml(:,:,:),epsil1 ) ) /  &
           ( 1. - zaux(:,:,:)*zdmass(:,:,:)/( rn_htopoc+zssh(:,:,:) ) ),  &
          DIM=1 ) / dtt
!
! Method 3:
!  -> we use a reference salinity to describe ocean salinity (and still do not
! make the approximation from the nsalflx==1 case)
!
  ELSE IF ( nsalflx==3 ) THEN
    zaux(:,:,:) = 1. / ( rhofw + zsalt(:,:,:) )
    tptfl(:,:)%cio = tptfl(:,:)%cio -       &
      SUM( ( rhoice*zdmass(:,:,:)*zaux(:,:,:) -  &
          rhoice/rhosw * pqsalt(:,:,:)/ssw0 ) /  &
           ( 1. - zaux(:,:,:)*zdmass(:,:,:)/( rn_htopoc+zssh(:,:,:) ) ),  &
          DIM=1 ) / dtt
!
! Method 4: 
!   -> As method 1 but salinity is considered as fixed.
  ELSE IF ( nsalflx==4 ) THEN
    tptfl(:,:)%cio = tptfl(:,:)%cio -  &
      SUM( zdmass(:,:,:) * (ssmr - sice)/AMAX1( zsml(:,:,:),epsil1 ),  &
          DIM=1) / dtt
  ENDIF
!
END SUBROUTINE glt_salflx
!
! ----------------------- END SUBROUTINE glt_salflx -------------------------
! -----------------------------------------------------------------------
