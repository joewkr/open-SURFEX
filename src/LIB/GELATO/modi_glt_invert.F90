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
! ========================= MODULE modi_glt_invert ==========================
! =======================================================================
!
! Goal:
! -----
! Solves a system of the form :
!                           M * X = Y
! where M is a (n,n) matrix), X and Y are two n component vectors 
! (Y is given and X is the solution).
!
! Method:
! -------
! Use a Gauss-Jordan method (note that the number of inferior diagonals
! should be specified). For example, if in any column of index j, all
! M(i,j) elements are zero for i>j+k (k fixed), and there is an index 
! j for which M(j+k,j) is non zero, the number of inferior diagonals is
! k.
!  
! Created: D. Salas y Melia, 05/02 
!
!
! -------------------- BEGIN MODULE modi_glt_invert -------------------------
!
!THXS_SFX!MODULE modi_glt_invert
!THXS_SFX!INTERFACE
!THXS_SFX!!
!THXS_SFX!SUBROUTINE glt_invert(kdiag,pmat)
!THXS_SFX!  INTEGER ::  &
!THXS_SFX!        kdiag
!THXS_SFX!  REAL, DIMENSION(:,:), INTENT(inout) ::  &
!THXS_SFX!        pmat
!THXS_SFX!END SUBROUTINE glt_invert
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_glt_invert
!
! ---------------------- END MODULE modi_glt_invert -------------------------
!
!
!
! -----------------------------------------------------------------------
! ----------------------- SUBROUTINE glt_invert -----------------------------
!
SUBROUTINE glt_invert(kdiag,pmat)
  USE modd_glt_param
  USE modi_gltools_glterr
!
  IMPLICIT NONE
  INTEGER ::  &
        kdiag
  REAL, DIMENSION(:,:), INTENT(inout) ::  &
        pmat
!
  INTEGER ::  &
        ji,jpiv,jdiag
  INTEGER ::  &
        iim,imax
  REAL ::  &
        zfac
  REAL, DIMENSION(:,:), ALLOCATABLE ::  &
        zmat
!
!
!
! 1. Initializations
! ==================
!
! .. Compute matrix dimension
!
  iim = SIZE(pmat,1)
!
! .. Allocate and initialize work matrix
!
  ALLOCATE( zmat(iim,2*iim) ) 
  zmat(:,:) = 0.
  zmat(:,1:iim) = pmat(:,:)
  DO ji=1,iim
    zmat(ji,ji+iim) = 1.
  END DO
!
!
! 2. Invert the initial matrix
! ============================
!
! 2.1. Checks
! -----------
!
  IF ( kdiag<1 .OR. kdiag>iim ) THEN
      IF(lwg) WRITE(noutlu,*) '      kdiag =',kdiag,' is lower than 1 or',  &
        ' greater than matrix dimension'
      CALL gltools_glterr( 'invert','','STOP')
  ENDIF
!
! 
! 2.2. Transformation of the matrix into an upper triangular matrix
! -----------------------------------------------------------------
!
! .. We try to get a triangular matrix
!
  DO jpiv=1,iim-1
    IF ( ABS(zmat(jpiv,jpiv))>1.e-10 ) THEN
        imax = MIN( jpiv+kdiag,iim )
        DO jdiag=jpiv+1,imax
          zfac = zmat(jdiag,jpiv) / zmat(jpiv,jpiv)
          zmat(jdiag,:) = zmat(jdiag,:) - zfac*zmat(jpiv,:)
        END DO
      ELSE
        IF (lwg) THEN
          WRITE(noutlu,*) '      This kind of matrix cannot be ',  &
            'inverted by this programme !'
          WRITE(noutlu,*) '      Current status of the matrix:'
          DO ji=1,iim
            WRITE(noutlu,*) '          ',zmat(ji,:)
          END DO
        ENDIF
        CALL gltools_glterr( 'invert','','STOP' )
    ENDIF
  END DO
!
!
! 2.3. Transformation of the upper triangular matrix into identity
! ----------------------------------------------------------------
!
  zmat(iim,:) = zmat(iim,:) / zmat(iim,iim)
  DO jpiv=iim,2,-1
    DO ji=1,jpiv-1
      zmat(ji,:) = zmat(ji,:)-zmat(ji,jpiv)*zmat(jpiv,:)
    END DO
    zmat(jpiv-1,:) = zmat(jpiv-1,:) / zmat(jpiv-1,jpiv-1)
  END DO
!
!
! 2.4. Output 
! -----------
!
  pmat(:,:) = zmat(:,iim+1:2*iim)

  DEALLOCATE( zmat)
END SUBROUTINE glt_invert

! --------------------- END SUBROUTINE glt_invert ---------------------------
! -----------------------------------------------------------------------
