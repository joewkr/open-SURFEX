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
! ======================= MODULE mode_gltools_prtrarr =====================
! =======================================================================
!
!
! * This module contains :
!     - a subroutine that displays part of a 2D array of dimensions 
!       (nx,ny).
!     - a subroutine that displays part of a 3D array of dimensions
!       (nt,nx,ny).


! ------------------- BEGIN MODULE mode_gltools_prtrarr -------------------

MODULE mode_gltools_prtrarr
INTERFACE

SUBROUTINE glt_prtrarr2(omess,pfield,kin,kix,kjn,kjx,ktab)
  CHARACTER(*), INTENT(in) ::                                           &
        omess
  REAL, DIMENSION(:,:), INTENT(in) ::                                   &
        pfield
  INTEGER, INTENT(in) ::                                                &
        kin,kix,kjn,kjx
  INTEGER, INTENT(in) ::                                                &
        ktab
END SUBROUTINE glt_prtrarr2

SUBROUTINE glt_prtrarr3(omess,pfield,klay,kin,kix,kjn,kjx,ktab)
  CHARACTER(*), INTENT(in) ::                                           &
        omess
  REAL, DIMENSION(:,:,:), INTENT(in) ::                                 &
        pfield
  INTEGER, INTENT(in) ::                                                &
        klay
  INTEGER, INTENT(in) ::                                                &
        kin,kix,kjn,kjx
  INTEGER, INTENT(in) ::                                                &
        ktab
END SUBROUTINE glt_prtrarr3

END INTERFACE
END MODULE mode_gltools_prtrarr

! ------------------- END MODULE mode_gltools_prtrarr ---------------------


! -----------------------------------------------------------------------
! ------------------------- SUBROUTINE glt_prtrarr2 -------------------------
!   
SUBROUTINE glt_prtrarr2(omess,pfield,kin,kix,kjn,kjx,ktab)
!
  USE modd_glt_param
!
  IMPLICIT NONE
  CHARACTER(*), INTENT(in) ::                                           &
        omess
  REAL, DIMENSION(:,:), INTENT(in) ::                                   &
        pfield
  INTEGER, INTENT(in) ::                                                &
        kin,kix,kjn,kjx
  INTEGER, INTENT(in) ::                                                &
        ktab
  INTEGER ::                                                            &
        jl
  INTEGER ::                                                            &
        kx,ky 
  REAL, DIMENSION(:,:), ALLOCATABLE ::                                  &
        zwork21


! *** Get input array dimensions
  kx = SIZE(pfield,1)
  ky = SIZE(pfield,2)

! *** Test specified array sections vs array dimensions
  IF (kix<1 .OR. kix>kx .OR. kjx<1 .OR. kjx>ky .OR.                     &
      kin<1 .OR. kin>kx .OR. kjn<1 .OR. kjn>ky .OR.                     &
      kix<kin .OR. kjx<kjn) THEN
    IF(lwg) THEN 
      WRITE(noutlu,*) '      *** WARNING ***'
      WRITE(noutlu,*) '        glt_prtrarr2 : check array section boundaries'
      WRITE(noutlu,*)'kin=',kin,'kix=',kix
      WRITE(noutlu,*)'kjn=',kjn,'kjx=',kjx
      WRITE(noutlu,*)'kx =',kx, 'ky =',ky
      WRITE(noutlu,*) '  '
    ENDIF
  ELSE
! * Allocate work array
    ALLOCATE(zwork21(kix-kin+1,kjx-kjn+1)) 

! * Print requested information
    IF (ktab==0) THEN
      IF(lwg) WRITE(noutlu,1400) omess
    ELSE
      IF(lwg) WRITE(noutlu,2400) omess
    ENDIF
    zwork21 = pfield(kin:kix,kjn:kjx) 
    DO jl = kjx-kjn+1,1,-1
      IF (ktab==0) THEN
        IF(lwg) WRITE(noutlu,1300) kjn-1+jl,zwork21(:,jl)
      ELSE
        IF(lwg) WRITE(noutlu,2300) kjn-1+jl,zwork21(:,jl)
      ENDIF
    END DO
    IF(lwg) WRITE(noutlu,*) ' '

! * Deallocate work array
    DEALLOCATE(zwork21) 
  ENDIF 

! *** Formats
1300 FORMAT(6X,I3,10(1X,E10.4))
1400 FORMAT(6X,A)
2300 FORMAT(55X,I3,10(1X,E10.4))
2400 FORMAT(55X,A)
!  
END SUBROUTINE glt_prtrarr2

! ----------------------- END SUBROUTINE glt_prtrarr2 -----------------------
! -----------------------------------------------------------------------


! -----------------------------------------------------------------------
! ------------------------- SUBROUTINE glt_prtrarr3 -------------------------
!   
SUBROUTINE glt_prtrarr3(omess,pfield,klay,kin,kix,kjn,kjx,ktab)
!
  USE modd_glt_param
!
  IMPLICIT NONE
  CHARACTER(*), INTENT(in) ::                                           &
        omess
  REAL, DIMENSION(:,:,:), INTENT(in) ::                                 &
        pfield
  INTEGER, INTENT(in) ::                                                &
        klay
  INTEGER, INTENT(in) ::                                                &
        kin,kix,kjn,kjx
  INTEGER, INTENT(in) ::                                                &
        ktab
  INTEGER ::                                                            &
        jl
  INTEGER ::                                                            &
        kx,ky,kz 
  REAL, DIMENSION(:,:), ALLOCATABLE ::                                  &
        zwork21


! *** Get input array dimensions
  kx = SIZE(pfield,2)
  ky = SIZE(pfield,3)
  kz = SIZE(pfield,1)

! *** Test specified array sections vs array dimensions
  IF (kix<1 .OR. kix>kx .OR. kjx<1 .OR. kjx>ky .OR.                     &
      kin<1 .OR. kin>kx .OR. kjn<1 .OR. kjn>ky .OR.                     &
      klay<1 .OR. klay>kz .OR. kix<kin .OR. kjx<kjn) THEN
    IF(lwg) WRITE(noutlu,*) '      *** WARNING ***'
    IF(lwg) WRITE(noutlu,*) '        glt_prtrarr3 : check array section boundaries'
    IF(lwg) WRITE(noutlu,*) '  '
  ELSE
! * Allocate work array
    ALLOCATE(zwork21(kix-kin+1,kjx-kjn+1)) 

! * Print requested information
    IF (ktab==0) THEN
      IF(lwg) WRITE(noutlu,1400) omess,klay
    ELSE
      IF(lwg) WRITE(noutlu,2400) omess,klay
    ENDIF
    zwork21 = pfield(klay,kin:kix,kjn:kjx) 
    DO jl = kjx-kjn+1,1,-1
      IF (ktab==0) THEN
        IF(lwg) WRITE(noutlu,1300) kjn-1+jl,zwork21(:,jl)
      ELSE
        IF(lwg) WRITE(noutlu,2300) kjn-1+jl,zwork21(:,jl)
      ENDIF
    END DO
    IF(lwg) WRITE(noutlu,*) ' '

! * Deallocate work array
    DEALLOCATE(zwork21) 
  ENDIF 

! *** Formats
1300 FORMAT(6X,I3,5(1X,E10.4))
1400 FORMAT(6X,A," - thk = ",I3)
2300 FORMAT(55X,I3,5(1X,E10.4))
2400 FORMAT(55X,A," - thk = ",I3)
!  
END SUBROUTINE glt_prtrarr3

! ----------------------- END SUBROUTINE glt_prtrarr3 -----------------------
! -----------------------------------------------------------------------
