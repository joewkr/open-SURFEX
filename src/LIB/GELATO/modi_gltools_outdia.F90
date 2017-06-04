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
! ====================== MODULE modi_gltools_outdia =======================
! =======================================================================
!
! Goal:
! -----
!   Condenses two routine calls in one, to shorten results routine.
!
! Created : 2008/02 (D. Salas y Melia)
! Modified: 2010/09 (D. Salas y Melia) Adapted for CMIP5 diagnostics
!
! -------------------- BEGIN MODULE modi_gltools_outdia -------------------
!
!THXS_SFX!MODULE modi_gltools_outdia
!THXS_SFX!INTERFACE 
!THXS_SFX!!
!THXS_SFX!SUBROUTINE gltools_outdia  &
!THXS_SFX!        ( tpind,tpnam,tpdom,pfield,pcumdia,pwgt )
!THXS_SFX!!
!THXS_SFX!  USE modd_glt_param
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  USE modi_gltools_strlower
!THXS_SFX!  TYPE(t_ind), INTENT(inout) ::  &
!THXS_SFX!        tpind
!THXS_SFX!  TYPE(t_def), INTENT(in) ::  &
!THXS_SFX!        tpnam
!THXS_SFX!  TYPE(t_dom), DIMENSION(nx,ny), INTENT(in) ::  &
!THXS_SFX!        tpdom
!THXS_SFX!  REAL, DIMENSION(:,:), INTENT(inout) ::  &
!THXS_SFX!        pfield
!THXS_SFX!  REAL, DIMENSION(:,:,:), INTENT(inout) ::  &
!THXS_SFX!        pcumdia
!THXS_SFX!  REAL, DIMENSION(:,:), OPTIONAL, INTENT(inout) ::  &
!THXS_SFX!        pwgt
!THXS_SFX!END SUBROUTINE gltools_outdia
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_gltools_outdia
!
! -------------------- END MODULE modi_gltools_outdia ---------------------
!
!
! -----------------------------------------------------------------------
! -------------------------- SUBROUTINE gltools_outdia --------------------------
!
SUBROUTINE gltools_outdia  &
        ( tpind,tpnam,tpdom,pfield,pcumdia,pwgt )
!
  USE modd_glt_param
  USE modd_types_glt
  USE modd_glt_const_thm
  USE modi_gltools_strlower
  USE modi_gltools_wriios
  USE mode_gltools_wrivais
  USE modi_gltools_avevai
  IMPLICIT NONE
!
!* Arguments
!
  TYPE(t_ind), INTENT(inout) ::  &
        tpind
  TYPE(t_def), INTENT(in) ::  &
        tpnam
  TYPE(t_dom), DIMENSION(nx,ny), INTENT(in) ::  &
        tpdom
  REAL, DIMENSION(:,:), INTENT(inout) ::  & 
        pfield 
  REAL, DIMENSION(:,:,:), INTENT(inout) ::  &
        pcumdia
  REAL, DIMENSION(:,:), OPTIONAL, INTENT(inout) ::  &
        pwgt
!
!* Local variables
!
  LOGICAL ::  &
        yis0d,yis2d
  CHARACTER(80), PARAMETER ::  &
        yall='all'
  INTEGER ::  &
        ix,iy,ixc,iyc,ixw,iyw
  INTEGER, DIMENSION(nx,ny) ::  &
        imsk
!
!
!* Get sizes of input data field
  ix = SIZE(pfield,1)
  iy = SIZE(pfield,2)
  yis0d = ( ix==1 .AND. iy==1 )
  yis2d = ( ix==nx .AND. iy==ny )
!
  IF ( TRIM(cdiafmt)=='GELATO' .OR. TRIM(cdiafmt)=='VMAR5' .OR. &
       TRIM(cdiafmt)=='XIOS' ) THEN
!
      IF ( tpind%cur==tpind%beg ) THEN
         IF ( TRIM(cdiafmt)=='GELATO' .OR. TRIM(cdiafmt)=='VMAR5' ) THEN
!
!* Print field information to gltout (for subsequent use in post-processing)
          IF(lp1) WRITE(noutlu,1000) tpnam%sna,TRIM(tpnam%lna),  &
            gltools_strlower(TRIM(tpnam%loc)),TRIM(tpnam%def),TRIM(tpnam%uni) 
!
!* Check that input field and cumulative array are conformable 
            ixc = SIZE( pcumdia,2 )
            iyc = SIZE( pcumdia,3 )
            IF ( ix/=ixc .OR. iy/=iyc ) THEN
               IF (lwg) THEN
                  WRITE(noutlu,*) '==> Writing field '//TRIM(tpnam%sna)//':'
                  WRITE(noutlu,*) '==> Input field size=',ix,iy
                  WRITE(noutlu,*)  &
                    '==> not conformable with ndiamax space size=',ixc,iyc
                  WRITE(noutlu,*) '==> We stop.'
               ENDIF
               STOP
           ENDIF
        ENDIF
      ENDIF
!
!* Check that input field and weights (if any) are conformable
      IF ( PRESENT(pwgt) ) THEN
         ixw = SIZE( pwgt,1 )
         iyw = SIZE( pwgt,2 )
         IF ( ix/=ixw .OR. iy/=iyw ) THEN
            IF (lwg) THEN
               WRITE(noutlu,*) '==> Writing field '//TRIM(tpnam%sna)//':'
               WRITE(noutlu,*) '==> Input field size=',ix,iy
               WRITE(noutlu,*)  &
                    '==> not conformable with weights size=',ixw,iyw
               WRITE(noutlu,*) '==> We stop.'
            ENDIF
            STOP
         ENDIF
      ENDIF
!
!* Mask field before any use (if field is 2d)
      IF ( yis2d ) THEN  
          SELECT CASE( TRIM(tpnam%loc) )
            CASE('T') ; imsk(:,:) = tpdom(:,:)%tmk
            CASE('U') ; imsk(:,:) = tpdom(:,:)%umk
            CASE('V') ; imsk(:,:) = tpdom(:,:)%vmk
          END SELECT
          WHERE( imsk(:,:)==0 )
            pfield(:,:) = xbig20
          ENDWHERE
      ENDIF
!
!* Write data
      IF ( TRIM(cdiafmt)=='GELATO' .OR. TRIM(cdiafmt)=='VMAR5' ) THEN
         IF ( ninsdia==1 ) THEN
            IF ( ANY( cinsfld(:)==yall ) .OR.  &
                 ANY( cinsfld(:)==tpnam%sna ) .OR.  &
                 yis0d )  &
                 CALL gltools_wrivai( tpnam,pfield,pwgt=pwgt )
         ENDIF
         IF ( navedia==1 )  &
              CALL gltools_avevai( tpind,tpnam,pfield,pcumdia,pwgt=pwgt )
      ELSE
         CALL gltools_wriios (tpnam%sna,pfield,pwgt=pwgt )
      ENDIF
  ENDIF
!
1000 FORMAT(3X,A20," ; ",A," ; ",A1," ; ",A," ; ",A)
!
END SUBROUTINE gltools_outdia
!
! ------------------------ END SUBROUTINE gltools_outdia ------------------------
! -----------------------------------------------------------------------
