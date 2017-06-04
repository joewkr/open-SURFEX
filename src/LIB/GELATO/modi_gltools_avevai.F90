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
! ====================== MODULE modi_gltools_avevai =======================
! =======================================================================
!
! Goal:
! -----
!   Write a diagnostic field in Vairmer format
!
! Created : 2004/03 (D. Salas y Melia)
! Modified: 2012/07 (D. Salas y Melia) Parallelism
!
! -------------------- BEGIN MODULE modi_gltools_avevai -------------------
!
!THXS_SFX!MODULE modi_gltools_avevai
!THXS_SFX!INTERFACE 
!THXS_SFX!!
!THXS_SFX!SUBROUTINE gltools_avevai  &
!THXS_SFX!        ( tpind,tpnam,pfield,pcumdia,pwgt )
!THXS_SFX!!
!THXS_SFX!  USE modd_glt_param
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  TYPE(t_ind), INTENT(inout) ::  &
!THXS_SFX!        tpind
!THXS_SFX!  TYPE(t_def), INTENT(in) ::  &
!THXS_SFX!        tpnam
!THXS_SFX!  REAL, DIMENSION(:,:), INTENT(in) ::  &
!THXS_SFX!        pfield
!THXS_SFX!  REAL, DIMENSION(:,:,:), INTENT(inout) ::  &
!THXS_SFX!        pcumdia
!THXS_SFX!  REAL, DIMENSION(:,:), OPTIONAL, INTENT(inout) ::  &
!THXS_SFX!        pwgt
!THXS_SFX!END SUBROUTINE gltools_avevai
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_gltools_avevai
!
! -------------------- END MODULE modi_gltools_avevai ---------------------
!
!
! -----------------------------------------------------------------------
! -------------------------- SUBROUTINE gltools_avevai --------------------------
!
SUBROUTINE gltools_avevai  &
        ( tpind,tpnam,pfield,pcumdia,pwgt )
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
  TYPE(t_ind), INTENT(inout) ::  &
        tpind
  TYPE(t_def), INTENT(in) ::  &
        tpnam
  REAL, DIMENSION(:,:), INTENT(in) ::  & 
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
  CHARACTER(1) ::  &
        ypos
  CHARACTER(6) ::  &
        ytype
  INTEGER ::  &
        ix,iy,ixc,iyc,ilu,ifld
  REAL, DIMENSION(:,:), ALLOCATABLE ::  &
        zwork2
  REAL, DIMENSION(:,:), ALLOCATABLE ::  &
        zwork2_g
  REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE ::  &
        zwork2_gr4
!
!
!
! 1. Initialisation
! ==================
!
! .. Get sizes of input data field
!
  ix = SIZE( pfield,1 )
  iy = SIZE( pfield,2 )
  yis0d = ( ix==1 .AND. iy==1 )
  yis2d = ( ix==nx .AND. iy==ny )
!
! .. Accumulation
!
  IF ( yis2d ) THEN 
      tpind%i2d = tpind%i2d+1
      ifld = tpind%i2d
    ELSE IF ( yis0d ) THEN
      tpind%i0d = tpind%i0d+1
      ifld = tpind%i0d
    ELSE
      IF (lwg) THEN
        WRITE(noutlu,*) '==> Input field size=',ix,iy
        WRITE(noutlu,*) '==> Routine gltools_avevai can only be used to write &
        &  fields with dimensions',nxglo,nyglo,' or 1,1.'
        WRITE(noutlu,*) 'We stop.'
      ENDIF
      STOP
  ENDIF
  pcumdia(ifld,:,:) = pcumdia(ifld,:,:) + pfield(:,:)
!
! .. Define parameters
!
  ypos = tpnam%loc
  ytype = gltools_strlower( TRIM(tpnam%typ) )
!
!
!
! 2. Average and save (if last time step)
! ========================================
!
  IF ( (tpind%cur==tpind%end) .OR. &
     ( MODULO(tpind%cur * dtt, dttave * xday2sec) .LE. epsil1) &
     ) THEN 
!
! .. Average
!
! Specialised averaging (for e.g. ice salinity, we want an average only
! for time steps when there is sea ice !)
!
      IF ( yis2d ) THEN 
          ALLOCATE( zwork2(nx,ny))
          IF ( PRESENT(pwgt) ) THEN
              WHERE( pwgt(:,:)>0. .AND. pfield(:,:)<xbig20 )
                zwork2(:,:) = pcumdia(ifld,:,:) / pwgt(:,:)
              ELSEWHERE
                zwork2(:,:) = xbig20
              ENDWHERE
! Regular averaging
            ELSE
              zwork2(:,:) = pcumdia(ifld,:,:) / FLOAT( tpind%nts )
          ENDIF
!
! .. Boundary conditions
!
#if ! defined in_surfex
          CALL gltools_bound( ypos,ytype,zwork2,pval=xbig20 )
#endif
!
! .. Gather the field to be written
!
          ALLOCATE( zwork2_g(nxglo,nyglo))
          ALLOCATE( zwork2_gr4(nxglo,nyglo) )
#if ! defined in_surfex
          CALL gather2d( zwork2,zwork2_g )
#else
#if ! defined in_arpege
          CALL gather_and_write_mpi( zwork2,zwork2_g )
#endif
#endif
          DEALLOCATE( zwork2)
!
! .. Correction of bounding with large value on mask for U and V fields
!
          IF ( lwg ) THEN
              IF ( ypos=='U' .OR. ypos=='V' ) THEN
                  WHERE( zwork2_g(:,:)<-xbig19 )
                    zwork2_g(:,:) = xbig20
                  ENDWHERE
              ENDIF
          ENDIF
!
        ELSE
!
          ALLOCATE( zwork2_g(ix,iy))
          ALLOCATE( zwork2_gr4(ix,iy) )
          zwork2_g(:,:) = pcumdia(ifld,:,:) / FLOAT( tpind%nts )
!
     IF ( MODULO(tpind%cur * dtt, dttave * xday2sec) .LE. epsil1) THEN
!       Reset accumulation field and weights
        pcumdia(ifld,:,:)=0.
        IF ( PRESENT(pwgt) ) pwgt(:,:)=0.
     ENDIF


      ENDIF
!
      IF ( gelato_myrank == gelato_leadproc ) THEN
!
! .. Convert to single precision
!
          zwork2_gr4(:,:) = zwork2_g(:,:)
!
!
!
! 3. Write data
! ==============
!
!
! .. Define logical unit for writing
!
          IF ( yis2d ) THEN
              ilu = n2valu
            ELSE IF ( yis0d ) THEN
              ilu = n0valu
          ENDIF
!
! .. Write data
!
          WRITE(ilu) TRIM( tpnam%sna )
          WRITE(ilu) zwork2_gr4(:,:)
!
      ENDIF
!
! .. Deallocate in reverse order (see comments in imod_thermo.f90)
!
      DEALLOCATE( zwork2_g )
      DEALLOCATE( zwork2_gr4 )
!
  ENDIF
!
    END SUBROUTINE gltools_avevai
!
! ------------------------ END SUBROUTINE gltools_avevai ------------------------
! -----------------------------------------------------------------------
