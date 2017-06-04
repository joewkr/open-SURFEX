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
! ====================== MODULE mode_gltools_wrivais ====================
! =======================================================================
!
! Goal:
! -----
!   Write a diagnostic field in Vairmer format
!
! Created : 2004/03 (D. Salas y Melia)
! Modified: 2010/09 (D. Salas y Melia) Introduce pwgt weights
! Modified: 2012/07 (D. Salas y Melia) Parallelism
! Modified: 2014/01 (D. Salas y Melia) Generalize: choice of logical unit,
!   single or double precision, 2d or 3d fields
!
! -------------------- BEGIN MODULE mode_gltools_wrivais -------------------
!
MODULE mode_gltools_wrivais 
!
INTERFACE gltools_wrivai
  MODULE PROCEDURE gltools_wrivai_2d
  MODULE PROCEDURE gltools_wrivai_3d
END INTERFACE
!
CONTAINS
!
! -----------------------------------------------------------------------
! ------------------- SUBROUTINE gltools_wrivai_2d ----------------------
!
SUBROUTINE gltools_wrivai_2d  &
        ( tpnam,pfield,kunit,kdbl,pwgt )
!
  USE modd_glt_param
  USE modd_types_glt
  USE modd_glt_const_thm
  USE modi_gltools_strlower
#if ! defined in_surfex
  USE mode_gltools_mpi
  USE mode_gltools_bound
#else
#if ! defined in_arpege
  USE MODI_GATHER_AND_WRITE_MPI
#endif
#endif
  IMPLICIT NONE
!
!* Arguments
!
  TYPE(t_def), INTENT(in) ::  &
        tpnam
  REAL, DIMENSION(:,:), INTENT(in) ::  & 
        pfield 
  INTEGER, OPTIONAL, INTENT(in) ::  &
        kunit,kdbl
  REAL, DIMENSION(:,:), OPTIONAL, INTENT(in) ::  &
        pwgt
!
!* Local variables
!
  LOGICAL ::  &
        yis0d,yis2d
  CHARACTER(1) ::  &
        ypos
  CHARACTER(6) ::  &
        ytype
  INTEGER ::  &
        idbl,ix,iy,ixc,iyc,ilu
  REAL, DIMENSION(:,:), ALLOCATABLE ::  &
        zwork
  REAL, DIMENSION(:,:), ALLOCATABLE ::  &
        zwork_g
  REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE ::  &
        zwork_gr4
!
!
!
! 1. Initialisation
! ==================
!
!
! .. Define field metadata
!
  ypos = tpnam%loc
  ytype = gltools_strlower( TRIM(tpnam%typ) )
!
! .. Single or double precision write
!
  IF ( PRESENT(kdbl) ) THEN
      idbl = kdbl
    ELSE
      idbl = 0
  ENDIF
!
!
!
! 2. Bound input array and convert it to single precision
! ========================================================
!
! .. Get sizes of input data field
!
  ix = SIZE( pfield,1 )
  iy = SIZE( pfield,2 )
  yis0d = ( ix==1 .AND. iy==1 )
  yis2d = ( ix==nx .AND. iy==ny )
!
! .. Weighting (pwgt is generally total sea ice concentration, or 
! average sea ice thickness)
! 
  IF ( yis2d ) THEN 
      ALLOCATE( zwork(nx,ny))
      IF ( PRESENT(pwgt) ) THEN
          WHERE( pwgt(:,:)>0. .AND. pfield(:,:)<xbig20 )
            zwork(:,:) = pfield(:,:) / pwgt(:,:)
          ELSEWHERE
            zwork(:,:) = xbig20
          ENDWHERE
        ELSE
          zwork(:,:) = pfield(:,:)
      ENDIF
!
! .. Boundary conditions
!
#if ! defined in_surfex
      CALL gltools_bound( ypos,ytype,zwork,pval=xbig20 )
#endif
!
! .. Gather the field to be written
!
      ALLOCATE( zwork_g(nxglo,nyglo) )
      IF ( idbl==0 ) ALLOCATE( zwork_gr4(nxglo,nyglo) )
#if ! defined in_surfex
      CALL gather2d( zwork,zwork_g )
#else
#if ! defined in_arpege 
      CALL gather_and_write_mpi( zwork,zwork_g )
#endif
#endif
      DEALLOCATE( zwork)
!
! .. Correction of bounding with large value on mask for U and V fields
!
      IF ( lwg ) THEN
          IF ( ypos=='U' .OR. ypos=='V' ) THEN
              WHERE( zwork_g(:,:)<-xbig19 )
                zwork_g(:,:) = xbig20
              ENDWHERE
          ENDIF
      ENDIF
!
    ELSE
!
      ALLOCATE( zwork_g(ix,iy))
      IF ( idbl==0 ) ALLOCATE( zwork_gr4(ix,iy) )
      zwork_g(:,:) = pfield(:,:)
!
  ENDIF
!
  IF ( lwg ) THEN
!
! .. Convert to single precision
!
      IF ( idbl==0 ) zwork_gr4(:,:) = zwork_g(:,:)
!
!
!
! 3. Write data
! ==============
!
! .. Define logical unit for writing
!
      IF ( PRESENT(kunit) ) THEN
          ilu = kunit
        ELSE
          IF ( yis2d ) THEN
              ilu = n2vilu
            ELSE IF ( yis0d ) THEN
              ilu = n0vilu
            ELSE
              IF (lwg) THEN
                WRITE(noutlu,*) '==> Input field size=',ix,iy
                WRITE(noutlu,*) '==> Routine gltools_wrivai can only be used to write &
              &  fields with dimensions',nxglo,nyglo,' or 1,1.'
                WRITE(noutlu,*) 'We stop.'
              ENDIF
              STOP
          ENDIF
      ENDIF
!
! .. Write data
!
      WRITE(ilu) TRIM( tpnam%sna )
      IF ( idbl==0 ) THEN
          WRITE(ilu) zwork_gr4(:,:)
        ELSE
          WRITE(ilu) zwork_g(:,:)
      ENDIF
!
  ENDIF
!
! .. Deallocations
!
      IF ( idbl==0 ) DEALLOCATE( zwork_gr4 )
      DEALLOCATE( zwork_g )
!
!
END SUBROUTINE gltools_wrivai_2d
!
! ----------------- END SUBROUTINE gltools_wrivai_2d ----------------------
! -------------------------------------------------------------------------
!
!
! -----------------------------------------------------------------------
! ------------------- SUBROUTINE gltools_wrivai_3d ----------------------
!
SUBROUTINE gltools_wrivai_3d  &
        ( tpnam,pfield,kunit,kdbl,pwgt )
!
  USE modd_glt_param
  USE modd_types_glt
  USE modd_glt_const_thm
  USE modi_gltools_strlower
#if ! defined in_surfex
  USE mode_gltools_mpi
  USE mode_gltools_bound
#else
  USE MODI_GATHER_AND_WRITE_MPI
#endif
  IMPLICIT NONE
!
!* Arguments
!
  TYPE(t_def), INTENT(in) ::  &
        tpnam
  REAL, DIMENSION(:,:,:), INTENT(in) ::  & 
        pfield 
  INTEGER, OPTIONAL, INTENT(in) ::  &
        kunit,kdbl
  REAL, DIMENSION(:,:,:), OPTIONAL, INTENT(in) ::  &
        pwgt
!
!* Local variables
!
  LOGICAL ::  &
        yis0d,yis2d
  CHARACTER(1) ::  &
        ypos
  CHARACTER(6) ::  &
        ytype
  INTEGER ::  &
        idbl,it,ix,iy,ixc,iyc,ilu
  REAL, DIMENSION(:,:,:), ALLOCATABLE ::  &
        zwork
  REAL, DIMENSION(:,:,:), ALLOCATABLE ::  &
        zwork_g
  REAL(KIND=4), DIMENSION(:,:,:), ALLOCATABLE ::  &
        zwork_gr4
!
!
!
! 1. Initialisation
! ==================
!
!
! .. Define field metadata
!
  ypos = tpnam%loc
  ytype = gltools_strlower( TRIM(tpnam%typ) )
!
! .. Single or double precision write
!
  IF ( PRESENT(kdbl) ) THEN
      idbl = kdbl
    ELSE
      idbl = 0
  ENDIF
!
!
!
! 2. Bound input array and convert it to single precision
! ========================================================
!
! .. Get sizes of input data field
!
  ix = SIZE( pfield,2 )
  iy = SIZE( pfield,3 )
  yis0d = ( ix==1 .AND. iy==1 )
  yis2d = ( ix==nx .AND. iy==ny )
!
! .. Weighting (pwgt is generally total sea ice concentration, or 
! average sea ice thickness)
! 
  IF ( yis2d ) THEN 
      ALLOCATE( zwork(nt,nx,ny))
      IF ( PRESENT(pwgt) ) THEN
          WHERE( pwgt(:,:,:)>0. .AND. pfield(:,:,:)<xbig20 )
            zwork(:,:,:) = pfield(:,:,:) / pwgt(:,:,:)
          ELSEWHERE
            zwork(:,:,:) = xbig20
          ENDWHERE
        ELSE
          zwork(:,:,:) = pfield(:,:,:)
      ENDIF
!
! .. Boundary conditions
!
#if ! defined in_surfex
      CALL gltools_bound( ypos,ytype,zwork,pval=xbig20 )
#endif
!
! .. Gather the field to be written
!
      ALLOCATE( zwork_g(nt,nxglo,nyglo) )
      IF ( idbl==0 ) ALLOCATE( zwork_gr4(nt,nxglo,nyglo) )
#if ! defined in_surfex
      CALL gather3d( zwork,zwork_g )
#else
      ! Surfex Gather function cannot yet work on 3D fields and needs 
      ! that first dimension is the one over which MPI distribution occurs ...
      DO it=1,nt
         CALL gather_and_write_mpi( zwork(it,:,:),zwork_g(it,:,:))
      END DO
#endif
      DEALLOCATE( zwork)
!
! .. Correction of bounding with large value on mask for U and V fields
!
      IF ( gelato_myrank == gelato_leadproc ) THEN
          IF ( ypos=='U' .OR. ypos=='V' ) THEN
              WHERE( zwork_g(:,:,:)<-xbig19 )
                zwork_g(:,:,:) = xbig20
              ENDWHERE
          ENDIF
      ENDIF
!
    ELSE
!
      ALLOCATE( zwork_g(nt,ix,iy))
      IF ( idbl==0 ) ALLOCATE( zwork_gr4(nt,ix,iy) )
      zwork_g(:,:,:) = pfield(:,:,:)
!
  ENDIF
!
  IF ( gelato_myrank == gelato_leadproc ) THEN
!
! .. Convert to single precision
!
      IF ( idbl==0 ) zwork_gr4(:,:,:) = zwork_g(:,:,:)
!
!
!
! 3. Write data
! ==============
!
! .. Define logical unit for writing
!
      IF ( PRESENT(kunit) ) THEN
          ilu = kunit
        ELSE
          IF ( yis2d ) THEN
              ilu = n2vilu
            ELSE IF ( yis0d ) THEN
              ilu = n0vilu
            ELSE
              IF (lwg) THEN
                WRITE(noutlu,*) '==> Input field size=',ix,iy
                WRITE(noutlu,*) '==> Routine gltools_wrivai can only be used to write &
              &  fields with dimensions',nxglo,nyglo,' or 1,1.'
                WRITE(noutlu,*) 'We stop.'
              ENDIF
              STOP
          ENDIF
      ENDIF
!
! .. Write data
!
      WRITE(ilu) TRIM( tpnam%sna )
      IF ( idbl==0 ) THEN
          WRITE(ilu) zwork_gr4(:,:,:)
        ELSE
          WRITE(ilu) zwork_g(:,:,:)
      ENDIF
!
  ENDIF
!
! .. Deallocations
!
      IF ( idbl==0 ) DEALLOCATE( zwork_gr4 )
      DEALLOCATE( zwork_g )
!
!
END SUBROUTINE gltools_wrivai_3d
!
! ----------------- END SUBROUTINE gltools_wrivai_3d ----------------------
! -------------------------------------------------------------------------
END MODULE mode_gltools_wrivais
!
! -------------------- END MODULE mode_gltools_wrivais --------------------
! -------------------------------------------------------------------------
