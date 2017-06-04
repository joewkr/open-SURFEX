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
! ===================== MODULE modi_gltools_wriios ======================
! =======================================================================
!
! Goal:
! -----
!   Write a diagnostic field using XIOS
!
! Method :
! --------
!  - Convert fields to kind wp
!  - Applies weighting if applicable
!  - Cast to a scalar if size is (1,1)
!  - Invoke Nemo (or dummy) routine iom_put, assuming that  iom_init, 
! set_grid ...
!    was already done upstream 
!  - Warn : the IOX namelist should set default value to big20 for Gelato fields
!  - Warn : Only 2d fields (genuine or degenerated to a scalar) are handled, yet
!
! Created : 2013/08 (S. Senesi)
! Modified: no
!
! -------------------- BEGIN MODULE modi_gltools_wriios -------------------
!
!THXS_SFX!MODULE modi_gltools_wriios
!THXS_SFX!INTERFACE 
!THXS_SFX!!
!THXS_SFX!SUBROUTINE gltools_wriios  &
!THXS_SFX!         ( hnam,pfield,pwgt )
!THXS_SFX!  USE modd_glt_const_thm
!THXS_SFX!  USE modd_glt_param
!THXS_SFX!  USE iom
!THXS_SFX!  USE par_kind
!THXS_SFX!  CHARACTER(LEN=80), INTENT(IN) :: &
!THXS_SFX!       hnam
!THXS_SFX!  REAL, DIMENSION(:,:), INTENT(in) ::  &
!THXS_SFX!       pfield
!THXS_SFX!  REAL, DIMENSION(:,:), OPTIONAL, INTENT(in) ::  &
!THXS_SFX!       pwgt
!THXS_SFX! END SUBROUTINE gltools_wriios
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_gltools_wriios

!
!
! -------------------- END MODULE modi_gltools_wriios ---------------------
!
!
! -------------------------------------------------------------------------
! ----------------------- SUBROUTINE gltools_wriios -----------------------
!
SUBROUTINE gltools_wriios  &
        ( hnam,pfield,pwgt )
!
  USE modd_glt_param
  USE modd_glt_const_thm
#if ! defined in_surfex
  USE iom
  USE par_kind
#else
  USE modd_wp
#endif

  IMPLICIT NONE

!
!* Arguments
!
  CHARACTER(LEN=*), INTENT(IN) :: &
        hnam
  REAL, DIMENSION(:,:), INTENT(in) ::  & 
        pfield 
  REAL, DIMENSION(:,:), OPTIONAL, INTENT(in) ::  &
        pwgt
!
!* Local variables
!
  INTEGER ::  &
        ix,iy
  REAL(wp), DIMENSION(:,:), ALLOCATABLE ::  &
        zwork2 
!
! .. Get sizes of input data field
!
  ix = SIZE( pfield,1 )
  iy = SIZE( pfield,2 )

  IF ((ix == 1) .AND. (iy == 1 )) THEN
#if ! defined in_surfex
     CALL iom_put(hnam,pfield(1,1))
#else
        print*,"Surfex cannot yet iom_put ",hnam
#endif
  ELSE
     IF ((ix == nx) .AND. (iy == ny )) THEN
        ALLOCATE( zwork2(ix,iy))
        IF ( PRESENT(pwgt) ) THEN 
           !
           ! .. Weighting (pwgt is generally total sea ice concentration, or 
           ! average sea ice thickness)
           ! 
           WHERE( pwgt(:,:)>0. .AND. pfield(:,:)<xbig20 )
              zwork2(:,:) = pfield(:,:) / pwgt(:,:)
           ELSEWHERE
              zwork2(:,:) = xbig20
           ENDWHERE
        ELSE
           zwork2(:,:) = pfield(:,:) 
        ENDIF
!        IF(lwg) write(*,*) 'in wriios, field=',hnam,' min/max=',minval(pfield(:,:)),maxval(pfield(:,:))
#if ! defined in_surfex
        CALL iom_put(hnam,zwork2)
#else
        print*,"Surfex cannot yet iom_put ",hnam
#endif
        DEALLOCATE(zwork2)
     ELSE ! 2d case, with sizes consistent with iom.F90 assumptions
        write(*,*) 'Gelato cannot use IOserver for sizes : ', ix, iy , 'of field', hnam
     ENDIF
  ENDIF
!
END SUBROUTINE gltools_wriios
!
! ------------------------ END SUBROUTINE gltools_wriios ------------------------
! -------------------------------------------------------------------------------
