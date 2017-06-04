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
MODULE mode_glt_nemo_bound

   !!======================================================================
   !!                       ***  MODULE  lbclnk  ***
   !! Ocean        : lateral boundary conditions
   !!=====================================================================
   !!  OPA 9.0 , LOCEAN-IPSL (2005) 
   !! $Id: lbclnk.F90 1344 2009-03-27 14:02:19Z rblod $
   !! This software is governed by the CeCILL licence see modipsl/doc/NEMO_CeCILL.txt 
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   Default option                              shared memory computing
   !!----------------------------------------------------------------------
   !!   lbc_lnk      : generic interface for lbc_lnk_3d and lbc_lnk_2d
   !!   lbc_lnk_2d   : set the lateral boundary condition on a 2D variable
   !!                  on OPA ocean mesh
   !!----------------------------------------------------------------------
   !! * Modules used
   USE modd_glt_param
#if ! defined in_surfex
   USE modd_glt_mpp_opa
   USE modd_glt_mppv
   USE mpi
#else
#ifdef SFX_MPI
   !!  define mpp_min, mpp_max, mpp_sum for Offline Surfex case with MPI
   USE MODD_SURFEX_MPI, ONLY : mpi_comm_opa => NCOMM
#else
   !! Case of Offline without MPI : no call to MPI, 
   !! mpp_min, mpp_max, mpp_sum are dummies (see below)
#endif
#endif


   IMPLICIT NONE
#if ! defined in_surfex
   !! empty
#else
#ifdef SFX_MPI
   INCLUDE 'mpif.h'
#endif
#endif
   PRIVATE

#if ! defined in_surfex
   INTERFACE lbc_lnk       ! From NEMO lbclnk.F90 routine
      MODULE PROCEDURE lbc_lnk_3d_gather, lbc_lnk_3d, lbc_lnk_2d           
   END INTERFACE
   INTERFACE mpp_lnk       ! From NEMO lib_mpp.F90 routine
      MODULE PROCEDURE mpp_lnk_3d_gather, mpp_lnk_3d, mpp_lnk_2d
   END INTERFACE
   INTERFACE lbc_lnk_e     ! From NEMO lib_mpp.F90 routine
      MODULE PROCEDURE mpp_lnk_2d_e
   END INTERFACE 
   INTERFACE lbc_nfd       ! From NEMO lbcnfd.F90 routine
      MODULE PROCEDURE lbc_nfd_3d, lbc_nfd_2d
   END INTERFACE
#endif

   INTERFACE mpp_min
      MODULE PROCEDURE mppmin_a_int, mppmin_int, mppmin_a_real, mppmin_real
   END INTERFACE
   INTERFACE mpp_max
      MODULE PROCEDURE mppmax_a_int, mppmax_int, mppmax_a_real, mppmax_real
   END INTERFACE
   INTERFACE mpp_sum
      MODULE PROCEDURE mppsum_a_int, mppsum_int, mppsum_a_real, mppsum_real
   END INTERFACE

#if ! defined in_surfex
   INTERFACE mpp_lbc_north ! From NEMO lib_mpp.F90 routine
      MODULE PROCEDURE mpp_lbc_north_3d, mpp_lbc_north_2d 
   END INTERFACE
   INTERFACE mpp_minloc
      MODULE PROCEDURE mpp_minloc2d ,mpp_minloc3d
   END INTERFACE
   INTERFACE mpp_maxloc
      MODULE PROCEDURE mpp_maxloc2d ,mpp_maxloc3d
   END INTERFACE
#endif

   !! * Routines called outside this module

#if ! defined in_surfex
   PUBLIC lbc_lnk,mpp_lnk,mpp_ini_north !,mpp_alloc,mpp_dealloc
#endif

   PUBLIC mpp_sum,mpp_min,mpp_max

#if ! defined in_surfex
   !! * Extracted from lib_mpp.F90 : definition of arrays for the
   !! following routines
   !!
   INTEGER ::   ngrp_world        ! group ID for the world processors
   INTEGER ::   ngrp_north        ! group ID for the northern processors (to be fold)
   INTEGER ::   ncomm_north       ! communicator made by the processors belonging to ngrp_north
   INTEGER ::   ndim_rank_north   ! number of 'sea' processor in the northern line (can be /= jpni !)
   INTEGER ::   njmppmax          ! value of njmpp for the processors of the northern line

   INTEGER, DIMENSION(:), ALLOCATABLE ::   nrank_north   ! dimension ndim_rank_north
   !!----------------------------------------------------------------------
#endif

CONTAINS


#if ! defined in_surfex
   SUBROUTINE lbc_lnk_3d_gather( pt3d1, cd_type1, pt3d2, cd_type2, psgn )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE lbc_lnk_3d_gather  ***
      !!
      !! ** Purpose :   set lateral boundary conditions (non mpp case)
      !!
      !! ** Method  :
      !!
      !! History :
      !!        !  97-06  (G. Madec)     Original code
      !!   8.5  !  02-09  (G. Madec)     F90: Free form and module
      !!        !  09-03  (R. Benshila)  External north fold treatment  
      !!----------------------------------------------------------------------
      !! * Arguments
      CHARACTER(len=1), INTENT( in ) ::   &
         cd_type1, cd_type2       ! nature of pt3d grid-points
      !             !   = T ,  U , V , F or W  gridpoints
      REAL, DIMENSION(jpi,jpj,jpk), INTENT( inout ) ::   &
         pt3d1, pt3d2          ! 3D array on which the boundary condition is applied
      REAL, INTENT( in ) ::   &
         psgn          ! control of the sign change
      !             !   =-1 , the sign is changed if north fold boundary
      !             !   = 1 , no sign change
      !             !   = 0 , no sign change and > 0 required (use the inner
      !             !         row/column if closed boundary)

      CALL lbc_lnk_3d( pt3d1, cd_type1, psgn)
      CALL lbc_lnk_3d( pt3d2, cd_type2, psgn)

   END SUBROUTINE lbc_lnk_3d_gather


   SUBROUTINE lbc_lnk_3d( pt3d, cd_type, psgn, cd_mpp, pval )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE lbc_lnk_3d  ***
      !!
      !! ** Purpose :   set lateral boundary conditions (non mpp case)
      !!
      !! ** Method  :
      !!
      !! History :
      !!        !  97-06  (G. Madec)  Original code
      !!   8.5  !  02-09  (G. Madec)  F90: Free form and module
      !!        !  09-03  (R. Benshila)  External north fold treatment  
      !!----------------------------------------------------------------------
      !! * Arguments
      CHARACTER(len=1), INTENT( in ) ::   &
         cd_type       ! nature of pt3d grid-points
      !             !   = T ,  U , V , F or W  gridpoints
      REAL, DIMENSION(jpi,jpj,jpk), INTENT( inout ) ::   &
         pt3d          ! 3D array on which the boundary condition is applied
      REAL, INTENT( in ) ::   &
         psgn          ! control of the sign change
      !             !   =-1 , the sign is changed if north fold boundary
      !             !   = 1 , no sign change
      !             !   = 0 , no sign change and > 0 required (use the inner
      !             !         row/column if closed boundary)
      CHARACTER(len=3), INTENT( in ), OPTIONAL ::    &
         cd_mpp        ! fill the overlap area only (here do nothing)
      REAL        , INTENT(in   ), OPTIONAL           ::   pval      ! background value (used at closed boundaries)

      !! * Local declarations
      REAL ::   zland

      IF( PRESENT( pval ) ) THEN      ! set land value (zero by default)
         zland = pval
      ELSE
         zland = 0.e0
      ENDIF


      IF( PRESENT( cd_mpp ) ) THEN
         ! only fill the overlap area and extra allows 
         ! this is in mpp case. In this module, just do nothing
      ELSE

         !                                     !  East-West boundaries
         !                                     ! ======================
         SELECT CASE ( nperio )
         !
         CASE ( 1 , 4 , 6 )                       !**  cyclic east-west
            pt3d( 1 ,:,:) = pt3d(jpim1,:,:)            ! all points
            pt3d(jpi,:,:) = pt3d(  2  ,:,:)
            !
         CASE DEFAULT                             !**  East closed  --  West closed
            SELECT CASE ( cd_type )
            CASE ( 'T' , 'U' , 'V' , 'W' )             ! T-, U-, V-, W-points
               pt3d( 1 ,:,:) = zland
               pt3d(jpi,:,:) = zland
            CASE ( 'F' )                               ! F-point
               pt3d(jpi,:,:) = zland
            END SELECT
            !
         END SELECT

         !                                     ! North-South boundaries
         !                                     ! ======================
         SELECT CASE ( nperio )
         !
         CASE ( 2 )                               !**  South symmetric  --  North closed
            SELECT CASE ( cd_type )
            CASE ( 'T' , 'U' , 'W' )                   ! T-, U-, W-points
               pt3d(:, 1 ,:) = pt3d(:,3,:)
               pt3d(:,jpj,:) = zland
            CASE ( 'V' , 'F' )                         ! V-, F-points
               pt3d(:, 1 ,:) = psgn * pt3d(:,2,:)
               pt3d(:,jpj,:) = zland
            END SELECT
            !
         CASE ( 3 , 4 , 5 , 6 )                   !**  North fold  T or F-point pivot  --  South closed
            SELECT CASE ( cd_type )                    ! South : closed
            CASE ( 'T' , 'U' , 'V' , 'W' , 'I' )             ! all points except F-point
               pt3d(:, 1 ,:) = zland
            END SELECT
            !                                          ! North fold
            pt3d( 1 ,jpj,:) = zland
            pt3d(jpi,jpj,:) = zland
            CALL lbc_nfd( pt3d(:,:,:), cd_type, psgn )
            !
         CASE DEFAULT                             !**  North closed  --  South closed
            SELECT CASE ( cd_type )
            CASE ( 'T' , 'U' , 'V' , 'W' )             ! T-, U-, V-, W-points
               pt3d(:, 1 ,:) = zland
               pt3d(:,jpj,:) = zland
            CASE ( 'F' )                               ! F-point
               pt3d(:,jpj,:) = zland
            END SELECT
            !
         END SELECT

      ENDIF

   END SUBROUTINE lbc_lnk_3d


   SUBROUTINE lbc_lnk_2d( pt2d, cd_type, psgn, cd_mpp, pval )
      !!---------------------------------------------------------------------
      !!                 ***  ROUTINE lbc_lnk_2d  ***
      !!
      !! ** Purpose :   set lateral boundary conditions (non mpp case)
      !!
      !! ** Method  :
      !!
      !! History :
      !!        !  97-06  (G. Madec)  Original code
      !!        !  01-05  (E. Durand)  correction
      !!   8.5  !  02-09  (G. Madec)  F90: Free form and module
      !!        !  09-03  (R. Benshila)  External north fold treatment  
      !!----------------------------------------------------------------------
      !! * Arguments
      CHARACTER(len=1), INTENT( in ) ::   &
         cd_type       ! nature of pt2d grid-point
         !             !   = T , U , V , F or W  gridpoints
         !             !   = I sea-ice U-V gridpoint (= F ocean grid point with indice shift)
      REAL, INTENT( in ) ::   &
         psgn          ! control of the sign change
         !             !   =-1 , the sign is modified following the type of b.c. used
         !             !   = 1 , no sign change
      REAL, DIMENSION(jpi,jpj), INTENT( inout ) ::   &
         pt2d          ! 2D array on which the boundary condition is applied
      CHARACTER(len=3), INTENT( in ), OPTIONAL ::    &
         cd_mpp        ! fill the overlap area only (here do nothing)
      REAL        , INTENT(in   ), OPTIONAL           ::   pval      ! background value (used at closed boundaries)

      !! * Local declarations
      REAL ::   zland

      IF( PRESENT( pval ) ) THEN      ! set land value (zero by default)
         zland = pval
      ELSE
         zland = 0.e0
      ENDIF

      IF (PRESENT(cd_mpp)) THEN
         ! only fill the overlap area and extra allows 
         ! this is in mpp case. In this module, just do nothing
      ELSE      
      
         !                                     ! East-West boundaries
         !                                     ! ====================
         SELECT CASE ( nperio )
         !
         CASE ( 1 , 4 , 6 )                       !** cyclic east-west
            pt2d( 1 ,:) = pt2d(jpim1,:)               ! all points
            pt2d(jpi,:) = pt2d(  2  ,:)
            !
         CASE DEFAULT                             !** East closed  --  West closed
            SELECT CASE ( cd_type )
            CASE ( 'T' , 'U' , 'V' , 'W' )            ! T-, U-, V-, W-points
               pt2d( 1 ,:) = zland
               pt2d(jpi,:) = zland
            CASE ( 'F' )                              ! F-point
               pt2d(jpi,:) = zland
            END SELECT
            !
         END SELECT
 
         !                                     ! North-South boundaries
         !                                     ! ======================
         SELECT CASE ( nperio )
         !
         CASE ( 2 )                               !**  South symmetric  --  North closed
            SELECT CASE ( cd_type )
            CASE ( 'T' , 'U' , 'W' )                   ! T-, U-, W-points
               pt2d(:, 1 ) = pt2d(:,3)
               pt2d(:,jpj) = zland
            CASE ( 'V' , 'F' )                         ! V-, F-points
               pt2d(:, 1 ) = psgn * pt2d(:,2)
               pt2d(:,jpj) = zland
            END SELECT
            !
         CASE ( 3 , 4 , 5 , 6 )                   !**  North fold  T or F-point pivot  --  South closed
            SELECT CASE ( cd_type )                    ! South : closed
            CASE ( 'T' , 'U' , 'V' , 'W' , 'I' )             ! all points except F-point
               pt2d(:, 1 ) = zland
            END SELECT
            !                                          ! North fold
            pt2d( 1 ,1  ) = zland 
            pt2d( 1 ,jpj) = zland 
            pt2d(jpi,jpj) = zland
            CALL lbc_nfd( pt2d(:,:), cd_type, psgn )
            !
         CASE DEFAULT                             !**  North closed  --  South closed
            SELECT CASE ( cd_type )
            CASE ( 'T' , 'U' , 'V' , 'W' )             ! T-, U-, V-, W-points
               pt2d(:, 1 ) = zland
               pt2d(:,jpj) = zland
            CASE ( 'F' )                               ! F-point
               pt2d(:,jpj) = zland
            END SELECT
            !
         END SELECT

      ENDIF
      
   END SUBROUTINE lbc_lnk_2d


   SUBROUTINE mpp_lnk_3d_gather( ptab1, cd_type1, ptab2, cd_type2, psgn )
      !!----------------------------------------------------------------------
      !!                  ***  routine mpp_lnk_3d_gather  ***
      !!
      !! ** Purpose :   Message passing manadgement for two 3D arrays
      !!
      !! ** Method  :   Use mppsend and mpprecv function for passing mask 
      !!      between processors following neighboring subdomains.
      !!            domain parameters
      !!                    nlci   : first dimension of the local subdomain
      !!                    nlcj   : second dimension of the local subdomain
      !!                    nbondi : mark for "east-west local boundary"
      !!                    nbondj : mark for "north-south local boundary"
      !!                    noea   : number for local neighboring processors 
      !!                    nowe   : number for local neighboring processors
      !!                    noso   : number for local neighboring processors
      !!                    nono   : number for local neighboring processors
      !!
      !! ** Action  :   ptab1 and ptab2  with update value at its periphery
      !!
      !!----------------------------------------------------------------------
      REAL, DIMENSION(jpi,jpj,jpk), INTENT(inout) ::   ptab1     ! first and second 3D array on which 
      REAL, DIMENSION(jpi,jpj,jpk), INTENT(inout) ::   ptab2     ! the boundary condition is applied
      CHARACTER(len=1)                , INTENT(in   ) ::   cd_type1  ! nature of ptab1 and ptab2 arrays 
      CHARACTER(len=1)                , INTENT(in   ) ::   cd_type2  ! i.e. grid-points = T , U , V , F or W points
      REAL                        , INTENT(in   ) ::   psgn      ! =-1 the sign change across the north fold boundary
      !!                                                             ! =  1. , the sign is kept
      INTEGER  ::   jl   ! dummy loop indices
      INTEGER  ::   imigr, iihom, ijhom        ! temporary integers
      INTEGER  ::   ml_req1, ml_req2, ml_err   ! for key_mpi_isend
      INTEGER, DIMENSION(MPI_STATUS_SIZE) ::   ml_stat   ! for key_mpi_isend
      !!----------------------------------------------------------------------

      ! 1. standard boundary treatment
      ! ------------------------------
      !                                      ! East-West boundaries
      !                                           !* Cyclic east-west
      IF( nbondi == 2 .AND. (nperio == 1 .OR. nperio == 4 .OR. nperio == 6) ) THEN
         ptab1( 1 ,:,:) = ptab1(jpim1,:,:)
         ptab1(jpi,:,:) = ptab1(  2  ,:,:)
         ptab2( 1 ,:,:) = ptab2(jpim1,:,:)
         ptab2(jpi,:,:) = ptab2(  2  ,:,:)
      ELSE                                        !* closed
         IF( .NOT. cd_type1 == 'F' )   ptab1(     1       :jpreci,:,:) = 0.e0    ! south except at F-point
         IF( .NOT. cd_type2 == 'F' )   ptab2(     1       :jpreci,:,:) = 0.e0
                                       ptab1(nlci-jpreci+1:jpi   ,:,:) = 0.e0    ! north
                                       ptab2(nlci-jpreci+1:jpi   ,:,:) = 0.e0
      ENDIF

      
      !                                      ! North-South boundaries
      IF( .NOT. cd_type1 == 'F' )   ptab1(:,     1       :jprecj,:) = 0.e0    ! south except at F-point
      IF( .NOT. cd_type2 == 'F' )   ptab2(:,     1       :jprecj,:) = 0.e0
                                    ptab1(:,nlcj-jprecj+1:jpj   ,:) = 0.e0    ! north
                                    ptab2(:,nlcj-jprecj+1:jpj   ,:) = 0.e0


      ! 2. East and west directions exchange
      ! ------------------------------------
      ! we play with the neigbours AND the row number because of the periodicity 
      !
      SELECT CASE ( nbondi )      ! Read Dirichlet lateral conditions
      CASE ( -1, 0, 1 )                ! all exept 2 (i.e. close case)
         iihom = nlci-nreci
         DO jl = 1, jpreci
            t4ew(:,jl,:,1,1) = ptab1(jpreci+jl,:,:)
            t4we(:,jl,:,1,1) = ptab1(iihom +jl,:,:)
            t4ew(:,jl,:,2,1) = ptab2(jpreci+jl,:,:)
            t4we(:,jl,:,2,1) = ptab2(iihom +jl,:,:)
         END DO
      END SELECT
      !
      !                           ! Migrations
      imigr = jpreci * jpj * jpk *2
      !
      SELECT CASE ( nbondi ) 
      CASE ( -1 )
         CALL mppsend( 2, t4we(1,1,1,1,1), imigr, noea, ml_req1 )
         CALL mpprecv( 1, t4ew(1,1,1,1,2), imigr )
         IF(l_isend) CALL mpi_wait(ml_req1, ml_stat, ml_err)
      CASE ( 0 )
         CALL mppsend( 1, t4ew(1,1,1,1,1), imigr, nowe, ml_req1 )
         CALL mppsend( 2, t4we(1,1,1,1,1), imigr, noea, ml_req2 )
         CALL mpprecv( 1, t4ew(1,1,1,1,2), imigr )
         CALL mpprecv( 2, t4we(1,1,1,1,2), imigr )
         IF(l_isend) CALL mpi_wait(ml_req1, ml_stat, ml_err)
         IF(l_isend) CALL mpi_wait(ml_req2, ml_stat, ml_err)
      CASE ( 1 )
         CALL mppsend( 1, t4ew(1,1,1,1,1), imigr, nowe, ml_req1 )
         CALL mpprecv( 2, t4we(1,1,1,1,2), imigr )
         IF(l_isend) CALL mpi_wait(ml_req1, ml_stat, ml_err)
      END SELECT
      !
      !                           ! Write Dirichlet lateral conditions
      iihom = nlci - jpreci
      !
      SELECT CASE ( nbondi )
      CASE ( -1 )
         DO jl = 1, jpreci
            ptab1(iihom+jl,:,:) = t4ew(:,jl,:,1,2)
            ptab2(iihom+jl,:,:) = t4ew(:,jl,:,2,2)
         END DO
      CASE ( 0 ) 
         DO jl = 1, jpreci
            ptab1(jl      ,:,:) = t4we(:,jl,:,1,2)
            ptab1(iihom+jl,:,:) = t4ew(:,jl,:,1,2)
            ptab2(jl      ,:,:) = t4we(:,jl,:,2,2)
            ptab2(iihom+jl,:,:) = t4ew(:,jl,:,2,2)
         END DO
      CASE ( 1 )
         DO jl = 1, jpreci
            ptab1(jl      ,:,:) = t4we(:,jl,:,1,2)
            ptab2(jl      ,:,:) = t4we(:,jl,:,2,2)
         END DO
      END SELECT


      ! 3. North and south directions
      ! -----------------------------
      ! always closed : we play only with the neigbours
      !
      IF( nbondj /= 2 ) THEN      ! Read Dirichlet lateral conditions
         ijhom = nlcj - nrecj
         DO jl = 1, jprecj
            t4sn(:,jl,:,1,1) = ptab1(:,ijhom +jl,:)
            t4ns(:,jl,:,1,1) = ptab1(:,jprecj+jl,:)
            t4sn(:,jl,:,2,1) = ptab2(:,ijhom +jl,:)
            t4ns(:,jl,:,2,1) = ptab2(:,jprecj+jl,:)
         END DO
      ENDIF
      !
      !                           ! Migrations
      imigr = jprecj * jpi * jpk * 2
      !
      SELECT CASE ( nbondj )     
      CASE ( -1 )
         CALL mppsend( 4, t4sn(1,1,1,1,1), imigr, nono, ml_req1 )
         CALL mpprecv( 3, t4ns(1,1,1,1,2), imigr )
         IF(l_isend) CALL mpi_wait(ml_req1, ml_stat, ml_err)
      CASE ( 0 )
         CALL mppsend( 3, t4ns(1,1,1,1,1), imigr, noso, ml_req1 )
         CALL mppsend( 4, t4sn(1,1,1,1,1), imigr, nono, ml_req2 )
         CALL mpprecv( 3, t4ns(1,1,1,1,2), imigr )
         CALL mpprecv( 4, t4sn(1,1,1,1,2), imigr )
         IF(l_isend) CALL mpi_wait(ml_req1, ml_stat, ml_err)
         IF(l_isend) CALL mpi_wait(ml_req2, ml_stat, ml_err)
      CASE ( 1 ) 
         CALL mppsend( 3, t4ns(1,1,1,1,1), imigr, noso, ml_req1 )
         CALL mpprecv( 4, t4sn(1,1,1,1,2), imigr )
         IF(l_isend) CALL mpi_wait(ml_req1, ml_stat, ml_err)
      END SELECT
      !
      !                           ! Write Dirichlet lateral conditions
      ijhom = nlcj - jprecj
      !
      SELECT CASE ( nbondj )
      CASE ( -1 )
         DO jl = 1, jprecj
            ptab1(:,ijhom+jl,:) = t4ns(:,jl,:,1,2)
            ptab2(:,ijhom+jl,:) = t4ns(:,jl,:,2,2)
         END DO
      CASE ( 0 ) 
         DO jl = 1, jprecj
            ptab1(:,jl      ,:) = t4sn(:,jl,:,1,2)
            ptab1(:,ijhom+jl,:) = t4ns(:,jl,:,1,2)
            ptab2(:,jl      ,:) = t4sn(:,jl,:,2,2)
            ptab2(:,ijhom+jl,:) = t4ns(:,jl,:,2,2)
         END DO
      CASE ( 1 )
         DO jl = 1, jprecj
            ptab1(:,jl,:) = t4sn(:,jl,:,1,2)
            ptab2(:,jl,:) = t4sn(:,jl,:,2,2)
         END DO
      END SELECT


      ! 4. north fold treatment
      ! -----------------------
      IF( npolj /= 0 ) THEN
         !
         SELECT CASE ( jpni )
         CASE ( 1 )                                           
            CALL lbc_nfd      ( ptab1, cd_type1, psgn )   ! only for northern procs.
            CALL lbc_nfd      ( ptab2, cd_type2, psgn )
         CASE DEFAULT
            CALL mpp_lbc_north( ptab1, cd_type1, psgn )   ! for all northern procs.
            CALL mpp_lbc_north (ptab2, cd_type2, psgn)
         END SELECT 
         !
      ENDIF
      !
   END SUBROUTINE mpp_lnk_3d_gather


   SUBROUTINE mpp_lnk_3d( ptab, cd_type, psgn, cd_mpp, pval )
      !!----------------------------------------------------------------------
      !!                  ***  routine mpp_lnk_3d  ***
      !!
      !! ** Purpose :   Message passing manadgement
      !!
      !! ** Method  :   Use mppsend and mpprecv function for passing mask 
      !!      between processors following neighboring subdomains.
      !!            domain parameters
      !!                    nlci   : first dimension of the local subdomain
      !!                    nlcj   : second dimension of the local subdomain
      !!                    nbondi : mark for "east-west local boundary"
      !!                    nbondj : mark for "north-south local boundary"
      !!                    noea   : number for local neighboring processors 
      !!                    nowe   : number for local neighboring processors
      !!                    noso   : number for local neighboring processors
      !!                    nono   : number for local neighboring processors
      !!
      !! ** Action  :   ptab with update value at its periphery
      !!
      !!----------------------------------------------------------------------
      REAL, DIMENSION(jpi,jpj,jpk), INTENT(inout) ::   ptab     ! 3D array on which the boundary condition is applied
      CHARACTER(len=1)                , INTENT(in   ) ::   cd_type  ! define the nature of ptab array grid-points
      !                                                             ! = T , U , V , F , W points
      REAL                        , INTENT(in   ) ::   psgn     ! =-1 the sign change across the north fold boundary
      !                                                             ! =  1. , the sign is kept
      CHARACTER(len=3), OPTIONAL      , INTENT(in   ) ::   cd_mpp   ! fill the overlap area only 
      REAL        , OPTIONAL      , INTENT(in   ) ::   pval     ! background value (used at closed boundaries)
      !!
      INTEGER  ::   ji, jj, jk, jl             ! dummy loop indices
      INTEGER  ::   imigr, iihom, ijhom        ! temporary integers
      INTEGER  ::   ml_req1, ml_req2, ml_err   ! for key_mpi_isend
      REAL ::   zland
      INTEGER, DIMENSION(MPI_STATUS_SIZE) ::   ml_stat   ! for key_mpi_isend
      !!----------------------------------------------------------------------

      IF( PRESENT( pval ) ) THEN   ;   zland = pval      ! set land value
      ELSE                         ;   zland = 0.e0      ! zero by default
      ENDIF

      ! 1. standard boundary treatment
      ! ------------------------------
      IF( PRESENT( cd_mpp ) ) THEN      ! only fill added line/raw with existing values
         !
         ! WARNING ptab is defined only between nld and nle
         DO jk = 1, jpk
            DO jj = nlcj+1, jpj                 ! added line(s)   (inner only)
               ptab(nldi  :nlei  , jj          ,jk) = ptab(nldi:nlei,     nlej,jk)   
               ptab(1     :nldi-1, jj          ,jk) = ptab(nldi     ,     nlej,jk)
               ptab(nlei+1:nlci  , jj          ,jk) = ptab(     nlei,     nlej,jk)
            END DO
            DO ji = nlci+1, jpi                 ! added column(s) (full)
               ptab(ji           ,nldj  :nlej  ,jk) = ptab(     nlei,nldj:nlej,jk)
               ptab(ji           ,1     :nldj-1,jk) = ptab(     nlei,nldj     ,jk)
               ptab(ji           ,nlej+1:jpj   ,jk) = ptab(     nlei,     nlej,jk)
            END DO
         END DO
         !
      ELSE                              ! standard close or cyclic treatment 
         !
         !                                   ! East-West boundaries
         !                                        !* Cyclic east-west
         IF( nbondi == 2 .AND. (nperio == 1 .OR. nperio == 4 .OR. nperio == 6) ) THEN
            ptab( 1 ,:,:) = ptab(jpim1,:,:)
            ptab(jpi,:,:) = ptab(  2  ,:,:)
         ELSE                                     !* closed
            IF( .NOT. cd_type == 'F' )   ptab(     1       :jpreci,:,:) = zland    ! south except F-point
                                         ptab(nlci-jpreci+1:jpi   ,:,:) = zland    ! north
         ENDIF
         !                                   ! North-South boundaries (always closed)
         IF( .NOT. cd_type == 'F' )   ptab(:,     1       :jprecj,:) = zland       ! south except F-point
                                      ptab(:,nlcj-jprecj+1:jpj   ,:) = zland       ! north
         !
      ENDIF

      ! 2. East and west directions exchange
      ! ------------------------------------
      ! we play with the neigbours AND the row number because of the periodicity 
      !
      SELECT CASE ( nbondi )      ! Read Dirichlet lateral conditions
      CASE ( -1, 0, 1 )                ! all exept 2 (i.e. close case)
         iihom = nlci-nreci
         DO jl = 1, jpreci
            t3ew(:,jl,:,1) = ptab(jpreci+jl,:,:)
            t3we(:,jl,:,1) = ptab(iihom +jl,:,:)
         END DO
      END SELECT  
      !
      !                           ! Migrations
      imigr = jpreci * jpj * jpk
      !
      SELECT CASE ( nbondi ) 
      CASE ( -1 )
         CALL mppsend( 2, t3we(1,1,1,1), imigr, noea, ml_req1 )
         CALL mpprecv( 1, t3ew(1,1,1,2), imigr )
         IF(l_isend) CALL mpi_wait(ml_req1, ml_stat, ml_err)
      CASE ( 0 )
         CALL mppsend( 1, t3ew(1,1,1,1), imigr, nowe, ml_req1 )
         CALL mppsend( 2, t3we(1,1,1,1), imigr, noea, ml_req2 )
         CALL mpprecv( 1, t3ew(1,1,1,2), imigr )
         CALL mpprecv( 2, t3we(1,1,1,2), imigr )
         IF(l_isend) CALL mpi_wait(ml_req1, ml_stat, ml_err)
         IF(l_isend) CALL mpi_wait(ml_req2, ml_stat, ml_err)
      CASE ( 1 )
         CALL mppsend( 1, t3ew(1,1,1,1), imigr, nowe, ml_req1 )
         CALL mpprecv( 2, t3we(1,1,1,2), imigr )
         IF(l_isend) CALL mpi_wait(ml_req1, ml_stat, ml_err)
      END SELECT
      !
      !                           ! Write Dirichlet lateral conditions
      iihom = nlci-jpreci
      !
      SELECT CASE ( nbondi )
      CASE ( -1 )
         DO jl = 1, jpreci
            ptab(iihom+jl,:,:) = t3ew(:,jl,:,2)
         END DO
      CASE ( 0 ) 
         DO jl = 1, jpreci
            ptab(jl      ,:,:) = t3we(:,jl,:,2)
            ptab(iihom+jl,:,:) = t3ew(:,jl,:,2)
         END DO
      CASE ( 1 )
         DO jl = 1, jpreci
            ptab(jl      ,:,:) = t3we(:,jl,:,2)
         END DO
      END SELECT


      ! 3. North and south directions
      ! -----------------------------
      ! always closed : we play only with the neigbours
      !
      IF( nbondj /= 2 ) THEN      ! Read Dirichlet lateral conditions
         ijhom = nlcj-nrecj
         DO jl = 1, jprecj
            t3sn(:,jl,:,1) = ptab(:,ijhom +jl,:)
            t3ns(:,jl,:,1) = ptab(:,jprecj+jl,:)
         END DO
      ENDIF
      !
      !                           ! Migrations
      imigr = jprecj * jpi * jpk
      !
      SELECT CASE ( nbondj )     
      CASE ( -1 )
         CALL mppsend( 4, t3sn(1,1,1,1), imigr, nono, ml_req1 )
         CALL mpprecv( 3, t3ns(1,1,1,2), imigr )
         IF(l_isend) CALL mpi_wait(ml_req1, ml_stat, ml_err)
      CASE ( 0 )
         CALL mppsend( 3, t3ns(1,1,1,1), imigr, noso, ml_req1 )
         CALL mppsend( 4, t3sn(1,1,1,1), imigr, nono, ml_req2 )
         CALL mpprecv( 3, t3ns(1,1,1,2), imigr )
         CALL mpprecv( 4, t3sn(1,1,1,2), imigr )
         IF(l_isend) CALL mpi_wait(ml_req1, ml_stat, ml_err)
         IF(l_isend) CALL mpi_wait(ml_req2, ml_stat, ml_err)
      CASE ( 1 ) 
         CALL mppsend( 3, t3ns(1,1,1,1), imigr, noso, ml_req1 )
         CALL mpprecv( 4, t3sn(1,1,1,2), imigr )
         IF(l_isend) CALL mpi_wait(ml_req1, ml_stat, ml_err)
      END SELECT
      !
      !                           ! Write Dirichlet lateral conditions
      ijhom = nlcj-jprecj
      !
      SELECT CASE ( nbondj )
      CASE ( -1 )
         DO jl = 1, jprecj
            ptab(:,ijhom+jl,:) = t3ns(:,jl,:,2)
         END DO
      CASE ( 0 ) 
         DO jl = 1, jprecj
            ptab(:,jl      ,:) = t3sn(:,jl,:,2)
            ptab(:,ijhom+jl,:) = t3ns(:,jl,:,2)
         END DO
      CASE ( 1 )
         DO jl = 1, jprecj
            ptab(:,jl,:) = t3sn(:,jl,:,2)
         END DO
      END SELECT


      ! 4. north fold treatment
      ! -----------------------
      !
      IF( npolj /= 0 .AND. .NOT. PRESENT(cd_mpp) ) THEN
         !
         SELECT CASE ( jpni )
         CASE ( 1 )     ;   CALL lbc_nfd      ( ptab, cd_type, psgn )   ! only 1 northern proc, no mpp
         CASE DEFAULT   ;   CALL mpp_lbc_north( ptab, cd_type, psgn )   ! for all northern procs.
         END SELECT
         !
      ENDIF
      !
   END SUBROUTINE mpp_lnk_3d


   SUBROUTINE mpp_lnk_2d( pt2d, cd_type, psgn, cd_mpp, pval )
      !!----------------------------------------------------------------------
      !!                  ***  routine mpp_lnk_2d  ***
      !!                  
      !! ** Purpose :   Message passing manadgement for 2d array
      !!
      !! ** Method  :   Use mppsend and mpprecv function for passing mask 
      !!      between processors following neighboring subdomains.
      !!            domain parameters
      !!                    nlci   : first dimension of the local subdomain
      !!                    nlcj   : second dimension of the local subdomain
      !!                    nbondi : mark for "east-west local boundary"
      !!                    nbondj : mark for "north-south local boundary"
      !!                    noea   : number for local neighboring processors 
      !!                    nowe   : number for local neighboring processors
      !!                    noso   : number for local neighboring processors
      !!                    nono   : number for local neighboring processors
      !!
      !!----------------------------------------------------------------------
      REAL, DIMENSION(jpi,jpj), INTENT(inout) ::   pt2d     ! 2D array on which the boundary condition is applied
      CHARACTER(len=1)            , INTENT(in   ) ::   cd_type  ! define the nature of ptab array grid-points
      !                                                         ! = T , U , V , F , W and I points
      REAL                    , INTENT(in   ) ::   psgn     ! =-1 the sign change across the north fold boundary
      !                                                         ! =  1. , the sign is kept
      CHARACTER(len=3), OPTIONAL  , INTENT(in   ) ::   cd_mpp   ! fill the overlap area only 
      REAL        , OPTIONAL  , INTENT(in   ) ::   pval     ! background value (used at closed boundaries)
      !!
      INTEGER  ::   ji, jj, jl   ! dummy loop indices
      INTEGER  ::   imigr, iihom, ijhom        ! temporary integers
      INTEGER  ::   ml_req1, ml_req2, ml_err   ! for key_mpi_isend
      REAL ::   zland
      INTEGER, DIMENSION(MPI_STATUS_SIZE) ::   ml_stat   ! for key_mpi_isend
      !!----------------------------------------------------------------------

      IF( PRESENT( pval ) ) THEN   ;   zland = pval      ! set land value
      ELSE                         ;   zland = 0.e0      ! zero by default
      ENDIF

      ! 1. standard boundary treatment
      ! ------------------------------
      !
      IF( PRESENT( cd_mpp ) ) THEN      ! only fill added line/raw with existing values
         !
         ! WARNING pt2d is defined only between nld and nle
         DO jj = nlcj+1, jpj                 ! added line(s)   (inner only)
            pt2d(nldi  :nlei  , jj          ) = pt2d(nldi:nlei,     nlej)   
            pt2d(1     :nldi-1, jj          ) = pt2d(nldi     ,     nlej)
            pt2d(nlei+1:nlci  , jj          ) = pt2d(     nlei,     nlej)
         END DO
         DO ji = nlci+1, jpi                 ! added column(s) (full)
            pt2d(ji           ,nldj  :nlej  ) = pt2d(     nlei,nldj:nlej)
            pt2d(ji           ,1     :nldj-1) = pt2d(     nlei,nldj     )
            pt2d(ji           ,nlej+1:jpj   ) = pt2d(     nlei,     nlej)
         END DO
         !
      ELSE                              ! standard close or cyclic treatment 
         !
         !                                   ! East-West boundaries
         IF( nbondi == 2 .AND.   &                ! Cyclic east-west
            &    (nperio == 1 .OR. nperio == 4 .OR. nperio == 6) ) THEN
            pt2d( 1 ,:) = pt2d(jpim1,:)                                    ! west
            pt2d(jpi,:) = pt2d(  2  ,:)                                    ! east
         ELSE                                     ! closed
            IF( .NOT. cd_type == 'F' )   pt2d(     1       :jpreci,:) = zland    ! south except F-point
                                         pt2d(nlci-jpreci+1:jpi   ,:) = zland    ! north
         ENDIF
         !                                   ! North-South boundaries (always closed)
            IF( .NOT. cd_type == 'F' )   pt2d(:,     1       :jprecj) = zland    !south except F-point
                                         pt2d(:,nlcj-jprecj+1:jpj   ) = zland    ! north
         !
      ENDIF

      ! 2. East and west directions exchange
      ! ------------------------------------
      ! we play with the neigbours AND the row number because of the periodicity 
      !
      SELECT CASE ( nbondi )      ! Read Dirichlet lateral conditions
      CASE ( -1, 0, 1 )                ! all exept 2 (i.e. close case)
         iihom = nlci-nreci
         DO jl = 1, jpreci
            t2ew(:,jl,1) = pt2d(jpreci+jl,:)
            t2we(:,jl,1) = pt2d(iihom +jl,:)
         END DO
      END SELECT
      !
      !                           ! Migrations
      imigr = jpreci * jpj
      !
      SELECT CASE ( nbondi )
      CASE ( -1 )
         CALL mppsend( 2, t2we(1,1,1), imigr, noea, ml_req1 )
         CALL mpprecv( 1, t2ew(1,1,2), imigr )
         IF(l_isend) CALL mpi_wait(ml_req1,ml_stat,ml_err)
      CASE ( 0 )
         CALL mppsend( 1, t2ew(1,1,1), imigr, nowe, ml_req1 )
         CALL mppsend( 2, t2we(1,1,1), imigr, noea, ml_req2 )
         CALL mpprecv( 1, t2ew(1,1,2), imigr )
         CALL mpprecv( 2, t2we(1,1,2), imigr )
         IF(l_isend) CALL mpi_wait(ml_req1,ml_stat,ml_err)
         IF(l_isend) CALL mpi_wait(ml_req2,ml_stat,ml_err)
      CASE ( 1 )
         CALL mppsend( 1, t2ew(1,1,1), imigr, nowe, ml_req1 )
         CALL mpprecv( 2, t2we(1,1,2), imigr )
         IF(l_isend) CALL mpi_wait(ml_req1,ml_stat,ml_err)
      END SELECT
      !
      !                           ! Write Dirichlet lateral conditions
      iihom = nlci - jpreci
      !
      SELECT CASE ( nbondi )
      CASE ( -1 )
         DO jl = 1, jpreci
            pt2d(iihom+jl,:) = t2ew(:,jl,2)
         END DO
      CASE ( 0 )
         DO jl = 1, jpreci
            pt2d(jl      ,:) = t2we(:,jl,2)
            pt2d(iihom+jl,:) = t2ew(:,jl,2)
         END DO
      CASE ( 1 )
         DO jl = 1, jpreci
            pt2d(jl      ,:) = t2we(:,jl,2)
         END DO
      END SELECT


      ! 3. North and south directions
      ! -----------------------------
      ! always closed : we play only with the neigbours
      !
      IF( nbondj /= 2 ) THEN      ! Read Dirichlet lateral conditions
         ijhom = nlcj-nrecj
         DO jl = 1, jprecj
            t2sn(:,jl,1) = pt2d(:,ijhom +jl)
            t2ns(:,jl,1) = pt2d(:,jprecj+jl)
         END DO
      ENDIF
      !
      !                           ! Migrations
      imigr = jprecj * jpi
      !
      SELECT CASE ( nbondj )
      CASE ( -1 )
         CALL mppsend( 4, t2sn(1,1,1), imigr, nono, ml_req1 )
         CALL mpprecv( 3, t2ns(1,1,2), imigr )
         IF(l_isend) CALL mpi_wait(ml_req1,ml_stat,ml_err)
      CASE ( 0 )
         CALL mppsend( 3, t2ns(1,1,1), imigr, noso, ml_req1 )
         CALL mppsend( 4, t2sn(1,1,1), imigr, nono, ml_req2 )
         CALL mpprecv( 3, t2ns(1,1,2), imigr )
         CALL mpprecv( 4, t2sn(1,1,2), imigr )
         IF(l_isend) CALL mpi_wait(ml_req1,ml_stat,ml_err)
         IF(l_isend) CALL mpi_wait(ml_req2,ml_stat,ml_err)
      CASE ( 1 )
         CALL mppsend( 3, t2ns(1,1,1), imigr, noso, ml_req1 )
         CALL mpprecv( 4, t2sn(1,1,2), imigr )
         IF(l_isend) CALL mpi_wait(ml_req1,ml_stat,ml_err)
      END SELECT
      !
      !                           ! Write Dirichlet lateral conditions
      ijhom = nlcj - jprecj
      !
      SELECT CASE ( nbondj )
      CASE ( -1 )
         DO jl = 1, jprecj
            pt2d(:,ijhom+jl) = t2ns(:,jl,2)
         END DO
      CASE ( 0 )
         DO jl = 1, jprecj
            pt2d(:,jl      ) = t2sn(:,jl,2)
            pt2d(:,ijhom+jl) = t2ns(:,jl,2)
         END DO
      CASE ( 1 ) 
         DO jl = 1, jprecj
            pt2d(:,jl      ) = t2sn(:,jl,2)
         END DO
      END SELECT


      ! 4. north fold treatment
      ! -----------------------
      !
      IF( npolj /= 0 .AND. .NOT. PRESENT(cd_mpp) ) THEN
         !
         SELECT CASE ( jpni )
         CASE ( 1 )     ;   CALL lbc_nfd      ( pt2d, cd_type, psgn )   ! only 1 northern proc, no mpp
         CASE DEFAULT   ;   CALL mpp_lbc_north( pt2d, cd_type, psgn )   ! for all northern procs.
         END SELECT
         !
      ENDIF
      !
   END SUBROUTINE mpp_lnk_2d


   SUBROUTINE mpp_lnk_2d_e( pt2d, cd_type, psgn )
      !!----------------------------------------------------------------------
      !!                  ***  routine mpp_lnk_2d_e  ***
      !!                  
      !! ** Purpose :   Message passing manadgement for 2d array (with halo)
      !!
      !! ** Method  :   Use mppsend and mpprecv function for passing mask 
      !!      between processors following neighboring subdomains.
      !!            domain parameters
      !!                    nlci   : first dimension of the local subdomain
      !!                    nlcj   : second dimension of the local subdomain
      !!                    jpr2di : number of rows for extra outer halo
      !!                    jpr2dj : number of columns for extra outer halo
      !!                    nbondi : mark for "east-west local boundary"
      !!                    nbondj : mark for "north-south local boundary"
      !!                    noea   : number for local neighboring processors 
      !!                    nowe   : number for local neighboring processors
      !!                    noso   : number for local neighboring processors
      !!                    nono   : number for local neighboring processors
      !!
      !!----------------------------------------------------------------------
      REAL, DIMENSION(1-jpr2di:jpi+jpr2di,1-jpr2dj:jpj+jpr2dj), INTENT(inout) ::   pt2d     ! 2D array with extra halo
      CHARACTER(len=1)                                            , INTENT(in   ) ::   cd_type  ! nature of ptab array grid-points
      !                                                                                         ! = T , U , V , F , W and I points
      REAL                                                    , INTENT(in   ) ::   psgn     ! =-1 the sign change across the
      !!                                                                                        ! north boundary, =  1. otherwise
      INTEGER  ::   jl   ! dummy loop indices
      INTEGER  ::   imigr, iihom, ijhom        ! temporary integers
      INTEGER  ::   ipreci, iprecj             ! temporary integers
      INTEGER  ::   ml_req1, ml_req2, ml_err   ! for key_mpi_isend
      INTEGER, DIMENSION(MPI_STATUS_SIZE) ::   ml_stat   ! for key_mpi_isend
      !!----------------------------------------------------------------------

      ipreci = jpreci + jpr2di      ! take into account outer extra 2D overlap area
      iprecj = jprecj + jpr2dj


      ! 1. standard boundary treatment
      ! ------------------------------
      ! Order matters Here !!!!
      !
      !                                      !* North-South boundaries (always colsed)
      IF( .NOT. cd_type == 'F' )   pt2d(:,  1-jpr2dj   :  jprecj  ) = 0.e0    ! south except at F-point
                                   pt2d(:,nlcj-jprecj+1:jpj+jpr2dj) = 0.e0    ! north
                                
      !                                      ! East-West boundaries
      !                                           !* Cyclic east-west
      IF( nbondi == 2 .AND. (nperio == 1 .OR. nperio == 4 .OR. nperio == 6) ) THEN
         pt2d(1-jpr2di:     1    ,:) = pt2d(jpim1-jpr2di:  jpim1 ,:)       ! east
         pt2d(   jpi  :jpi+jpr2di,:) = pt2d(     2      :2+jpr2di,:)       ! west
         !
      ELSE                                        !* closed
         IF( .NOT. cd_type == 'F' )   pt2d(  1-jpr2di   :jpreci    ,:) = 0.e0    ! south except at F-point
                                      pt2d(nlci-jpreci+1:jpi+jpr2di,:) = 0.e0    ! north
      ENDIF
      !

      ! north fold treatment
      ! -----------------------
      IF( npolj /= 0 ) THEN
         !
         SELECT CASE ( jpni )
         CASE ( 1 )     ;   CALL lbc_nfd        ( pt2d(1:jpi,1:jpj+jpr2dj), cd_type, psgn, pr2dj=jpr2dj )
         CASE DEFAULT   ;   CALL mpp_lbc_north_e( pt2d                    , cd_type, psgn               )
         END SELECT 
         !
      ENDIF

      ! 2. East and west directions exchange
      ! ------------------------------------
      ! we play with the neigbours AND the row number because of the periodicity 
      !
      SELECT CASE ( nbondi )      ! Read Dirichlet lateral conditions
      CASE ( -1, 0, 1 )                ! all exept 2 (i.e. close case)
         iihom = nlci-nreci-jpr2di
         DO jl = 1, ipreci
            tr2ew(:,jl,1) = pt2d(jpreci+jl,:)
            tr2we(:,jl,1) = pt2d(iihom +jl,:)
         END DO
      END SELECT
      !
      !                           ! Migrations
      imigr = ipreci * ( jpj + 2*jpr2dj)
      !
      SELECT CASE ( nbondi )
      CASE ( -1 )
         CALL mppsend( 2, tr2we(1-jpr2dj,1,1), imigr, noea, ml_req1 )
         CALL mpprecv( 1, tr2ew(1-jpr2dj,1,2), imigr )
         IF(l_isend) CALL mpi_wait(ml_req1,ml_stat,ml_err)
      CASE ( 0 )
         CALL mppsend( 1, tr2ew(1-jpr2dj,1,1), imigr, nowe, ml_req1 )
         CALL mppsend( 2, tr2we(1-jpr2dj,1,1), imigr, noea, ml_req2 )
         CALL mpprecv( 1, tr2ew(1-jpr2dj,1,2), imigr )
         CALL mpprecv( 2, tr2we(1-jpr2dj,1,2), imigr )
         IF(l_isend) CALL mpi_wait(ml_req1,ml_stat,ml_err)
         IF(l_isend) CALL mpi_wait(ml_req2,ml_stat,ml_err)
      CASE ( 1 )
         CALL mppsend( 1, tr2ew(1-jpr2dj,1,1), imigr, nowe, ml_req1 )
         CALL mpprecv( 2, tr2we(1-jpr2dj,1,2), imigr )
         IF(l_isend) CALL mpi_wait(ml_req1,ml_stat,ml_err)
      END SELECT
      !
      !                           ! Write Dirichlet lateral conditions
      iihom = nlci - jpreci
      !
      SELECT CASE ( nbondi )
      CASE ( -1 )
         DO jl = 1, ipreci
            pt2d(iihom+jl,:) = tr2ew(:,jl,2)
         END DO
      CASE ( 0 )
         DO jl = 1, ipreci
            pt2d(jl-jpr2di,:) = tr2we(:,jl,2)
            pt2d( iihom+jl,:) = tr2ew(:,jl,2)
         END DO
      CASE ( 1 )
         DO jl = 1, ipreci
            pt2d(jl-jpr2di,:) = tr2we(:,jl,2)
         END DO
      END SELECT


      ! 3. North and south directions
      ! -----------------------------
      ! always closed : we play only with the neigbours
      !
      IF( nbondj /= 2 ) THEN      ! Read Dirichlet lateral conditions
         ijhom = nlcj-nrecj-jpr2dj
         DO jl = 1, iprecj
            tr2sn(:,jl,1) = pt2d(:,ijhom +jl)
            tr2ns(:,jl,1) = pt2d(:,jprecj+jl)
         END DO
      ENDIF
      !
      !                           ! Migrations
      imigr = iprecj * ( jpi + 2*jpr2di )
      !
      SELECT CASE ( nbondj )
      CASE ( -1 )
         CALL mppsend( 4, tr2sn(1-jpr2di,1,1), imigr, nono, ml_req1 )
         CALL mpprecv( 3, tr2ns(1-jpr2di,1,2), imigr )
         IF(l_isend) CALL mpi_wait(ml_req1,ml_stat,ml_err)
      CASE ( 0 )
         CALL mppsend( 3, tr2ns(1-jpr2di,1,1), imigr, noso, ml_req1 )
         CALL mppsend( 4, tr2sn(1-jpr2di,1,1), imigr, nono, ml_req2 )
         CALL mpprecv( 3, tr2ns(1-jpr2di,1,2), imigr )
         CALL mpprecv( 4, tr2sn(1-jpr2di,1,2), imigr )
         IF(l_isend) CALL mpi_wait(ml_req1,ml_stat,ml_err)
         IF(l_isend) CALL mpi_wait(ml_req2,ml_stat,ml_err)
      CASE ( 1 )
         CALL mppsend( 3, tr2ns(1-jpr2di,1,1), imigr, noso, ml_req1 )
         CALL mpprecv( 4, tr2sn(1-jpr2di,1,2), imigr )
         IF(l_isend) CALL mpi_wait(ml_req1,ml_stat,ml_err)
      END SELECT
      !
      !                           ! Write Dirichlet lateral conditions
      ijhom = nlcj - jprecj  
      !
      SELECT CASE ( nbondj )
      CASE ( -1 )
         DO jl = 1, iprecj
            pt2d(:,ijhom+jl) = tr2ns(:,jl,2)
         END DO
      CASE ( 0 )
         DO jl = 1, iprecj
            pt2d(:,jl-jpr2dj) = tr2sn(:,jl,2)
            pt2d(:,ijhom+jl ) = tr2ns(:,jl,2)
         END DO
      CASE ( 1 ) 
         DO jl = 1, iprecj
            pt2d(:,jl-jpr2dj) = tr2sn(:,jl,2)
         END DO
      END SELECT

   END SUBROUTINE mpp_lnk_2d_e


   SUBROUTINE mppsend( ktyp, pmess, kbytes, kdest, md_req )
      !!----------------------------------------------------------------------
      !!                  ***  routine mppsend  ***
      !!                   
      !! ** Purpose :   Send messag passing array
      !!
      !!----------------------------------------------------------------------
      REAL, INTENT(inout) ::   pmess(*)   ! array of real
      INTEGER , INTENT(in   ) ::   kbytes     ! size of the array pmess
      INTEGER , INTENT(in   ) ::   kdest      ! receive process number
      INTEGER , INTENT(in   ) ::   ktyp       ! tag of the message
! The intent is changed from 'in' to 'inout' (for NEC...)  
      INTEGER , INTENT(inout) ::   md_req     ! argument for isend
      !!
      INTEGER ::   iflag
      !!----------------------------------------------------------------------
      !
      SELECT CASE ( cn_mpi_send )
      CASE ( 'S' )                ! Standard mpi send (blocking)
         CALL mpi_send ( pmess, kbytes, mpi_double_precision, kdest , ktyp, mpi_comm_opa        , iflag )
      CASE ( 'B' )                ! Buffer mpi send (blocking)
         CALL mpi_bsend( pmess, kbytes, mpi_double_precision, kdest , ktyp, mpi_comm_opa        , iflag )
      CASE ( 'I' )                ! Immediate mpi send (non-blocking send)
         ! be carefull, one more argument here : the mpi request identifier..
         CALL mpi_isend( pmess, kbytes, mpi_double_precision, kdest , ktyp, mpi_comm_opa, md_req, iflag )
      END SELECT
      !
   END SUBROUTINE mppsend


   SUBROUTINE mpprecv( ktyp, pmess, kbytes )
      !!----------------------------------------------------------------------
      !!                  ***  routine mpprecv  ***
      !!
      !! ** Purpose :   Receive messag passing array
      !!
      !!----------------------------------------------------------------------
      REAL, INTENT(inout) ::   pmess(*)   ! array of real
      INTEGER , INTENT(in   ) ::   kbytes     ! suze of the array pmess
      INTEGER , INTENT(in   ) ::   ktyp       ! Tag of the recevied message
      !!
      INTEGER :: istatus(mpi_status_size)
      INTEGER :: iflag
      !!----------------------------------------------------------------------
      !
      CALL mpi_recv( pmess, kbytes, mpi_double_precision, mpi_any_source, ktyp, mpi_comm_opa, istatus, iflag )
      !
   END SUBROUTINE mpprecv
#endif

   

   SUBROUTINE mppmax_a_int( ktab, kdim, kcom )
      !!----------------------------------------------------------------------
      !!                  ***  routine mppmax_a_int  ***
      !! 
      !! ** Purpose :   Find maximum value in an integer layout array
      !!
      !!----------------------------------------------------------------------
      INTEGER , INTENT(in   )                  ::   kdim   ! size of array
      INTEGER , INTENT(inout), DIMENSION(kdim) ::   ktab   ! input array
      INTEGER , INTENT(in   ), OPTIONAL        ::   kcom   ! 
      !!
      INTEGER :: ierror, localcomm   ! temporary integer
      INTEGER, DIMENSION(kdim) ::   iwork
      !!----------------------------------------------------------------------
      !
#if !defined in_surfex || defined SFX_MPI
      localcomm = mpi_comm_opa
      IF( PRESENT(kcom) )   localcomm = kcom
      !
!$OMP SINGLE
      CALL mpi_allreduce( ktab, iwork, kdim, mpi_integer, mpi_max, localcomm, ierror )
!$OMP END SINGLE
      !
      ktab(:) = iwork(:)
#endif
      !
   END SUBROUTINE mppmax_a_int


   SUBROUTINE mppmax_int( ktab, kcom )
      !!----------------------------------------------------------------------
      !!                  ***  routine mppmax_int  ***
      !!
      !! ** Purpose :   Find maximum value in an integer layout array
      !!
      !!----------------------------------------------------------------------
      INTEGER, INTENT(inout)           ::   ktab      ! ???
      INTEGER, INTENT(in   ), OPTIONAL ::   kcom      ! ???
      !! 
      INTEGER ::   ierror, iwork, localcomm   ! temporary integer
      !!----------------------------------------------------------------------
      !
#if !defined in_surfex || defined SFX_MPI
      localcomm = mpi_comm_opa 
      IF( PRESENT(kcom) )   localcomm = kcom
      !
!$OMP SINGLE
      CALL mpi_allreduce( ktab, iwork, 1, mpi_integer, mpi_max, localcomm, ierror)
!$OMP END SINGLE
      !
      ktab = iwork
#endif
      !
   END SUBROUTINE mppmax_int


   SUBROUTINE mppmin_a_int( ktab, kdim, kcom )
      !!----------------------------------------------------------------------
      !!                  ***  routine mppmin_a_int  ***
      !! 
      !! ** Purpose :   Find minimum value in an integer layout array
      !!
      !!----------------------------------------------------------------------
      INTEGER , INTENT( in  )                  ::   kdim        ! size of array
      INTEGER , INTENT(inout), DIMENSION(kdim) ::   ktab        ! input array
      INTEGER , INTENT( in  ), OPTIONAL        ::   kcom        ! input array
      !!
      INTEGER ::   ierror, localcomm   ! temporary integer
      INTEGER, DIMENSION(kdim) ::   iwork
      !!----------------------------------------------------------------------
      !
#if !defined in_surfex || defined SFX_MPI
      localcomm = mpi_comm_opa
      IF( PRESENT(kcom) )   localcomm = kcom
      !
!$OMP SINGLE
      CALL mpi_allreduce( ktab, iwork, kdim, mpi_integer, mpi_min, localcomm, ierror )
!$OMP END SINGLE
      !
      ktab(:) = iwork(:)
#endif
      !
   END SUBROUTINE mppmin_a_int


   SUBROUTINE mppmin_int( ktab, kcom )
      !!----------------------------------------------------------------------
      !!                  ***  routine mppmin_int  ***
      !!
      !! ** Purpose :   Find minimum value in an integer layout array
      !!
      !!----------------------------------------------------------------------
      INTEGER, INTENT(inout) ::   ktab      ! ???
      INTEGER , INTENT( in  ), OPTIONAL        ::   kcom        ! input array
      !!
      INTEGER ::  ierror, iwork, localcomm
      !!----------------------------------------------------------------------
      !
#if !defined in_surfex || defined SFX_MPI
      localcomm = mpi_comm_opa
      IF( PRESENT(kcom) )   localcomm = kcom
      !
!$OMP SINGLE
     CALL mpi_allreduce( ktab, iwork, 1, mpi_integer, mpi_min, localcomm, ierror )
!$OMP END SINGLE
      !
      ktab = iwork
#endif
      !
   END SUBROUTINE mppmin_int


   SUBROUTINE mppsum_a_int( ktab, kdim )
      !!----------------------------------------------------------------------
      !!                  ***  routine mppsum_a_int  ***
      !!                    
      !! ** Purpose :   Global integer sum, 1D array case
      !!
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in   )                   ::   kdim      ! ???
      INTEGER, INTENT(inout), DIMENSION (kdim) ::   ktab      ! ???
      !!
      INTEGER :: ierror
      INTEGER, DIMENSION (kdim) ::  iwork
      !!----------------------------------------------------------------------
      !
#if !defined in_surfex || defined SFX_MPI
!$OMP SINGLE
      CALL mpi_allreduce( ktab, iwork, kdim, mpi_integer, mpi_sum, mpi_comm_opa, ierror )
!$OMP END SINGLE
      !
      ktab(:) = iwork(:)
#endif
      !
   END SUBROUTINE mppsum_a_int


   SUBROUTINE mppsum_int( ktab )
      !!----------------------------------------------------------------------
      !!                 ***  routine mppsum_int  ***
      !!                  
      !! ** Purpose :   Global integer sum
      !!
      !!----------------------------------------------------------------------
      INTEGER, INTENT(inout) ::   ktab
      !! 
      INTEGER :: ierror, iwork
      !!----------------------------------------------------------------------
      !
#if !defined in_surfex || defined SFX_MPI
!$OMP SINGLE
      CALL mpi_allreduce( ktab, iwork, 1, mpi_integer, mpi_sum, mpi_comm_opa, ierror )
!$OMP END SINGLE
      !
      ktab = iwork
#endif
      !
   END SUBROUTINE mppsum_int


   SUBROUTINE mppmax_a_real( ptab, kdim, kcom )
      !!----------------------------------------------------------------------
      !!                 ***  routine mppmax_a_real  ***
      !!                  
      !! ** Purpose :   Maximum
      !!
      !!----------------------------------------------------------------------
      INTEGER , INTENT(in   )                  ::   kdim
      REAL, INTENT(inout), DIMENSION(kdim) ::   ptab
      INTEGER , INTENT(in   ), OPTIONAL        ::   kcom
      !!
      INTEGER :: ierror, localcomm
      REAL, DIMENSION(kdim) ::  zwork
      !!----------------------------------------------------------------------
      !
#if !defined in_surfex || defined SFX_MPI
      localcomm = mpi_comm_opa
      IF( PRESENT(kcom) ) localcomm = kcom
      !
!$OMP SINGLE
      CALL mpi_allreduce( ptab, zwork, kdim, mpi_double_precision, mpi_max, localcomm, ierror )
!$OMP END SINGLE
      ptab(:) = zwork(:)
#endif
      !
   END SUBROUTINE mppmax_a_real


   SUBROUTINE mppmax_real( ptab, kcom )
      !!----------------------------------------------------------------------
      !!                  ***  routine mppmax_real  ***
      !!                    
      !! ** Purpose :   Maximum
      !!
      !!----------------------------------------------------------------------
      REAL, INTENT(inout)           ::   ptab   ! ???
      INTEGER , INTENT(in   ), OPTIONAL ::   kcom   ! ???
      !!
      INTEGER  ::   ierror, localcomm
      REAL ::   zwork
      !!----------------------------------------------------------------------
      !
#if !defined in_surfex || defined SFX_MPI
      localcomm = mpi_comm_opa 
      IF( PRESENT(kcom) )   localcomm = kcom
      !
!$OMP SINGLE
      CALL mpi_allreduce( ptab, zwork, 1, mpi_double_precision, mpi_max, localcomm, ierror )
!$OMP END SINGLE
      ptab = zwork
#endif
      !
   END SUBROUTINE mppmax_real


   SUBROUTINE mppmin_a_real( ptab, kdim, kcom )
      !!----------------------------------------------------------------------
      !!                 ***  routine mppmin_a_real  ***
      !!                  
      !! ** Purpose :   Minimum of REAL, array case
      !!
      !!-----------------------------------------------------------------------
      INTEGER , INTENT(in   )                  ::   kdim
      REAL, INTENT(inout), DIMENSION(kdim) ::   ptab
      INTEGER , INTENT(in   ), OPTIONAL        ::   kcom
      !!
      INTEGER :: ierror, localcomm
      REAL, DIMENSION(kdim) ::   zwork
      !!-----------------------------------------------------------------------
      !
#if !defined in_surfex || defined SFX_MPI
      localcomm = mpi_comm_opa 
      IF( PRESENT(kcom) ) localcomm = kcom
      !
!$OMP SINGLE
      CALL mpi_allreduce( ptab, zwork, kdim, mpi_double_precision, mpi_min, localcomm, ierror )
!$OMP END SINGLE
      ptab(:) = zwork(:)
#endif
      !
   END SUBROUTINE mppmin_a_real


   SUBROUTINE mppmin_real( ptab, kcom )
      !!----------------------------------------------------------------------
      !!                  ***  routine mppmin_real  ***
      !! 
      !! ** Purpose :   minimum of REAL, scalar case
      !!
      !!-----------------------------------------------------------------------
      REAL, INTENT(inout)           ::   ptab        ! 
      INTEGER , INTENT(in   ), OPTIONAL :: kcom
      !!
      INTEGER  ::   ierror
      REAL ::   zwork
      INTEGER :: localcomm
      !!-----------------------------------------------------------------------
      !
#if !defined in_surfex || defined SFX_MPI
      localcomm = mpi_comm_opa 
      IF( PRESENT(kcom) )   localcomm = kcom
      !
!$OMP SINGLE
      CALL mpi_allreduce( ptab, zwork, 1, mpi_double_precision, mpi_min, localcomm, ierror )
!$OMP END SINGLE
      ptab = zwork
#endif
      !
   END SUBROUTINE mppmin_real


   SUBROUTINE mppsum_a_real( ptab, kdim, kcom )
      !!----------------------------------------------------------------------
      !!                  ***  routine mppsum_a_real  ***
      !! 
      !! ** Purpose :   global sum, REAL ARRAY argument case
      !!
      !!-----------------------------------------------------------------------
      INTEGER , INTENT( in )                     ::   kdim      ! size of ptab
      REAL, DIMENSION(kdim), INTENT( inout ) ::   ptab      ! input array
      INTEGER , INTENT( in ), OPTIONAL           :: kcom
      !!
      INTEGER                   ::   ierror    ! temporary integer
      INTEGER                   ::   localcomm 
      REAL, DIMENSION(kdim) ::   zwork     ! temporary workspace 
      !!-----------------------------------------------------------------------
      !
#if !defined in_surfex || defined SFX_MPI
      localcomm = mpi_comm_opa 
      IF( PRESENT(kcom) )   localcomm = kcom
      !
!$OMP SINGLE
      CALL mpi_allreduce( ptab, zwork, kdim, mpi_double_precision, mpi_sum, localcomm, ierror )
!$OMP END SINGLE
      ptab(:) = zwork(:)
#endif
      !
   END SUBROUTINE mppsum_a_real


   SUBROUTINE mppsum_real( ptab, kcom )
      !!----------------------------------------------------------------------
      !!                  ***  routine mppsum_real  ***
      !!              
      !! ** Purpose :   global sum, SCALAR argument case
      !!
      !!-----------------------------------------------------------------------
      REAL, INTENT(inout)           ::   ptab   ! input scalar
      INTEGER , INTENT(in   ), OPTIONAL ::   kcom
      !!
      INTEGER  ::   ierror, localcomm 
      REAL ::   zwork
      !!-----------------------------------------------------------------------
      !
#if !defined in_surfex || defined SFX_MPI
      localcomm = mpi_comm_opa 
      IF( PRESENT(kcom) ) localcomm = kcom
      !
!$OMP SINGLE
      CALL mpi_allreduce( ptab, zwork, 1, mpi_double_precision, mpi_sum, localcomm, ierror )
!$OMP END SINGLE
      ptab = zwork
#endif
      !
   END SUBROUTINE mppsum_real


#if ! defined in_surfex
   SUBROUTINE mpp_minloc2d( ptab, pmask, pmin, ki,kj )
      !!------------------------------------------------------------------------
      !!             ***  routine mpp_minloc  ***
      !!
      !! ** Purpose :   Compute the global minimum of an array ptab
      !!              and also give its global position
      !!
      !! ** Method  :   Use MPI_ALLREDUCE with MPI_MINLOC
      !!
      !!--------------------------------------------------------------------------
      REAL, DIMENSION (jpi,jpj), INTENT(in   ) ::   ptab    ! Local 2D array
      REAL, DIMENSION (jpi,jpj), INTENT(in   ) ::   pmask   ! Local mask
      REAL                     , INTENT(  out) ::   pmin    ! Global minimum of ptab
      INTEGER                      , INTENT(  out) ::   ki, kj   ! index of minimum in global frame
      !!
      INTEGER , DIMENSION(2)   ::   ilocs
      INTEGER :: ierror
      REAL ::   zmin   ! local minimum
      REAL, DIMENSION(2,1) ::   zain, zaout
      !!-----------------------------------------------------------------------
      !
      zmin  = MINVAL( ptab(:,:) , mask= pmask == 1.e0 )
      ilocs = MINLOC( ptab(:,:) , mask= pmask == 1.e0 )
      !
      ki = ilocs(1) + nimpp - 1
      kj = ilocs(2) + njmpp - 1
      !
      zain(1,:)=zmin
      zain(2,:)=ki+10000.*kj
      !
      CALL MPI_ALLREDUCE( zain,zaout, 1, MPI_2DOUBLE_PRECISION,MPI_MINLOC,MPI_COMM_OPA,ierror)
      !
      pmin = zaout(1,1)
      kj = INT(zaout(2,1)/10000.)
      ki = INT(zaout(2,1) - 10000.*kj )
      !
   END SUBROUTINE mpp_minloc2d


   SUBROUTINE mpp_minloc3d( ptab, pmask, pmin, ki, kj ,kk)
      !!------------------------------------------------------------------------
      !!             ***  routine mpp_minloc  ***
      !!
      !! ** Purpose :   Compute the global minimum of an array ptab
      !!              and also give its global position
      !!
      !! ** Method  :   Use MPI_ALLREDUCE with MPI_MINLOC
      !!
      !!--------------------------------------------------------------------------
      REAL, DIMENSION (jpi,jpj,jpk), INTENT(in   ) ::   ptab         ! Local 2D array
      REAL, DIMENSION (jpi,jpj,jpk), INTENT(in   ) ::   pmask        ! Local mask
      REAL                         , INTENT(  out) ::   pmin         ! Global minimum of ptab
      INTEGER                          , INTENT(  out) ::   ki, kj, kk   ! index of minimum in global frame
      !!
      INTEGER  ::   ierror
      REAL ::   zmin     ! local minimum
      INTEGER , DIMENSION(3)   ::   ilocs
      REAL, DIMENSION(2,1) ::   zain, zaout
      !!-----------------------------------------------------------------------
      !
      zmin  = MINVAL( ptab(:,:,:) , mask= pmask == 1.e0 )
      ilocs = MINLOC( ptab(:,:,:) , mask= pmask == 1.e0 )
      !
      ki = ilocs(1) + nimpp - 1
      kj = ilocs(2) + njmpp - 1
      kk = ilocs(3)
      !
      zain(1,:)=zmin
      zain(2,:)=ki+10000.*kj+100000000.*kk
      !
      CALL MPI_ALLREDUCE( zain,zaout, 1, MPI_2DOUBLE_PRECISION,MPI_MINLOC,MPI_COMM_OPA,ierror)
      !
      pmin = zaout(1,1)
      kk   = INT( zaout(2,1) / 100000000. )
      kj   = INT( zaout(2,1) - kk * 100000000. ) / 10000
      ki   = INT( zaout(2,1) - kk * 100000000. -kj * 10000. )
      !
   END SUBROUTINE mpp_minloc3d


   SUBROUTINE mpp_maxloc2d( ptab, pmask, pmax, ki, kj )
      !!------------------------------------------------------------------------
      !!             ***  routine mpp_maxloc  ***
      !!
      !! ** Purpose :   Compute the global maximum of an array ptab
      !!              and also give its global position
      !!
      !! ** Method  :   Use MPI_ALLREDUCE with MPI_MINLOC
      !!
      !!--------------------------------------------------------------------------
      REAL, DIMENSION (jpi,jpj), INTENT(in   ) ::   ptab     ! Local 2D array
      REAL, DIMENSION (jpi,jpj), INTENT(in   ) ::   pmask    ! Local mask
      REAL                     , INTENT(  out) ::   pmax     ! Global maximum of ptab
      INTEGER                      , INTENT(  out) ::   ki, kj   ! index of maximum in global frame
      !!  
      INTEGER  :: ierror
      INTEGER, DIMENSION (2)   ::   ilocs
      REAL :: zmax   ! local maximum
      REAL, DIMENSION(2,1) ::   zain, zaout
      !!-----------------------------------------------------------------------
      !
      zmax  = MAXVAL( ptab(:,:) , mask= pmask == 1.e0 )
      ilocs = MAXLOC( ptab(:,:) , mask= pmask == 1.e0 )
      !
      ki = ilocs(1) + nimpp - 1
      kj = ilocs(2) + njmpp - 1
      !
      zain(1,:) = zmax
      zain(2,:) = ki + 10000. * kj
      !
      CALL MPI_ALLREDUCE( zain,zaout, 1, MPI_2DOUBLE_PRECISION,MPI_MAXLOC,MPI_COMM_OPA,ierror)
      !
      pmax = zaout(1,1)
      kj   = INT( zaout(2,1) / 10000.     )
      ki   = INT( zaout(2,1) - 10000.* kj )
      !
   END SUBROUTINE mpp_maxloc2d


   SUBROUTINE mpp_maxloc3d( ptab, pmask, pmax, ki, kj, kk )
      !!------------------------------------------------------------------------
      !!             ***  routine mpp_maxloc  ***
      !!
      !! ** Purpose :  Compute the global maximum of an array ptab
      !!              and also give its global position
      !!
      !! ** Method : Use MPI_ALLREDUCE with MPI_MINLOC
      !!
      !!--------------------------------------------------------------------------
      REAL, DIMENSION (jpi,jpj,jpk), INTENT(in   ) ::   ptab         ! Local 2D array
      REAL, DIMENSION (jpi,jpj,jpk), INTENT(in   ) ::   pmask        ! Local mask
      REAL                         , INTENT(  out) ::   pmax         ! Global maximum of ptab
      INTEGER                          , INTENT(  out) ::   ki, kj, kk   ! index of maximum in global frame
      !!   
      REAL :: zmax   ! local maximum
      REAL, DIMENSION(2,1) ::   zain, zaout
      INTEGER , DIMENSION(3)   ::   ilocs
      INTEGER :: ierror
      !!-----------------------------------------------------------------------
      !
      zmax  = MAXVAL( ptab(:,:,:) , mask= pmask == 1.e0 )
      ilocs = MAXLOC( ptab(:,:,:) , mask= pmask == 1.e0 )
      !
      ki = ilocs(1) + nimpp - 1
      kj = ilocs(2) + njmpp - 1
      kk = ilocs(3)
      !
      zain(1,:)=zmax
      zain(2,:)=ki+10000.*kj+100000000.*kk
      !
      CALL MPI_ALLREDUCE( zain,zaout, 1, MPI_2DOUBLE_PRECISION,MPI_MAXLOC,MPI_COMM_OPA,ierror)
      !
      pmax = zaout(1,1)
      kk   = INT( zaout(2,1) / 100000000. )
      kj   = INT( zaout(2,1) - kk * 100000000. ) / 10000
      ki   = INT( zaout(2,1) - kk * 100000000. -kj * 10000. )
      !
   END SUBROUTINE mpp_maxloc3d

   SUBROUTINE mpp_ini_north
      !!----------------------------------------------------------------------
      !!               ***  routine mpp_ini_north  ***
      !!
      !! ** Purpose :   Initialize special communicator for north folding 
      !!      condition together with global variables needed in the mpp folding
      !!
      !! ** Method  : - Look for northern processors
      !!              - Put their number in nrank_north
      !!              - Create groups for the world processors and the north processors
      !!              - Create a communicator for northern processors
      !!
      !! ** glt_output
      !!      njmppmax = njmpp for northern procs
      !!      ndim_rank_north = number of processors in the northern line
      !!      nrank_north (ndim_rank_north) = number  of the northern procs.
      !!      ngrp_world = group ID for the world processors
      !!      ngrp_north = group ID for the northern processors
      !!      ncomm_north = communicator for the northern procs.
      !!      north_root = number (in the world) of proc 0 in the northern comm.
      !!
      !!----------------------------------------------------------------------
      INTEGER ::   ierr
      INTEGER ::   jjproc
      INTEGER ::   ii, ji
      !!----------------------------------------------------------------------
      !
      njmppmax = MAXVAL( njmppt )
      !
      ! Look for how many procs on the northern boundary
      ndim_rank_north = 0
      DO jjproc = 1, jpnij
         IF( njmppt(jjproc) == njmppmax )   ndim_rank_north = ndim_rank_north + 1
      END DO
      !
      ! Allocate the right size to nrank_north
      IF (ALLOCATED (nrank_north)) DEALLOCATE(nrank_north)
      ALLOCATE( nrank_north(ndim_rank_north) )

      ! Fill the nrank_north array with proc. number of northern procs.
      ! Note : the rank start at 0 in mpi
      ii = 0
      DO ji = 1, jpnij
         IF ( njmppt(ji) == njmppmax   ) THEN
            ii=ii+1
            nrank_north(ii)=ji-1
         END IF
      END DO
      !
      ! create the world group
      CALL MPI_COMM_GROUP( mpi_comm_opa, ngrp_world, ierr )
      !
      ! Create the North group from the world group
      CALL MPI_GROUP_INCL( ngrp_world, ndim_rank_north, nrank_north, ngrp_north, ierr )
      !
      ! Create the North communicator , ie the pool of procs in the north group
      CALL MPI_COMM_CREATE( mpi_comm_opa, ngrp_north, ncomm_north, ierr )
      !
   END SUBROUTINE mpp_ini_north


   SUBROUTINE mpp_lbc_north_3d( pt3d, cd_type, psgn )
      !!---------------------------------------------------------------------
      !!                   ***  routine mpp_lbc_north_3d  ***
      !!
      !! ** Purpose :   Ensure proper north fold horizontal bondary condition 
      !!              in mpp configuration in case of jpn1 > 1
      !!
      !! ** Method  :   North fold condition and mpp with more than one proc
      !!              in i-direction require a specific treatment. We gather 
      !!              the 4 northern lines of the global domain on 1 processor
      !!              and apply lbc north-fold on this sub array. Then we
      !!              scatter the north fold array back to the processors.
      !!
      !!----------------------------------------------------------------------
      REAL, DIMENSION(jpi,jpj,jpk), INTENT(inout) ::   pt3d      ! 3D array on which the b.c. is applied
      CHARACTER(len=1)                , INTENT(in   ) ::   cd_type   ! nature of pt3d grid-points
      !                                                              !   = T ,  U , V , F or W  gridpoints
      REAL                        , INTENT(in   ) ::   psgn      ! = -1. the sign change across the north fold 
      !!                                                             ! =  1. , the sign is kept
      INTEGER ::   ji, jj, jr
      INTEGER ::   ierr, itaille, ildi, ilei, iilb
      INTEGER ::   ijpj, ijpjm1, ij, iproc
      REAL, DIMENSION(jpiglo,4,jpk)      ::   ztab
      REAL, DIMENSION(jpi   ,4,jpk)      ::   znorthloc
      REAL, DIMENSION(jpi   ,4,jpk,jpni) ::   znorthgloio
      !!----------------------------------------------------------------------
      !   
      ijpj   = 4
      ijpjm1 = 3
      !
      DO jj = nlcj - ijpj +1, nlcj          ! put in znorthloc the last 4 jlines of pt3d
         ij = jj - nlcj + ijpj
         znorthloc(:,ij,:) = pt3d(:,jj,:)
      END DO
      !
      !                                     ! Build in procs of ncomm_north the znorthgloio
      itaille = jpi * jpk * ijpj
      CALL MPI_ALLGATHER( znorthloc  , itaille, MPI_DOUBLE_PRECISION,                &
         &                znorthgloio, itaille, MPI_DOUBLE_PRECISION, ncomm_north, ierr )
      !
      !                                     ! recover the global north array
      DO jr = 1, ndim_rank_north
         iproc = nrank_north(jr) + 1
         ildi  = nldit (iproc)
         ilei  = nleit (iproc)
         iilb  = nimppt(iproc)
         DO jj = 1, 4
            DO ji = ildi, ilei
               ztab(ji+iilb-1,jj,:) = znorthgloio(ji,jj,:,jr)
            END DO
         END DO
      END DO
      !
      CALL lbc_nfd( ztab, cd_type, psgn )   ! North fold boundary condition
      !
      DO jj = nlcj-ijpj+1, nlcj             ! Scatter back to pt3d
         ij = jj - nlcj + ijpj
         DO ji= 1, nlci
            pt3d(ji,jj,:) = ztab(ji+nimpp-1,ij,:)
         END DO
      END DO
      !
   END SUBROUTINE mpp_lbc_north_3d


   SUBROUTINE mpp_lbc_north_2d( pt2d, cd_type, psgn)
      !!---------------------------------------------------------------------
      !!                   ***  routine mpp_lbc_north_2d  ***
      !!
      !! ** Purpose :   Ensure proper north fold horizontal bondary condition 
      !!              in mpp configuration in case of jpn1 > 1 (for 2d array )
      !!
      !! ** Method  :   North fold condition and mpp with more than one proc
      !!              in i-direction require a specific treatment. We gather 
      !!              the 4 northern lines of the global domain on 1 processor
      !!              and apply lbc north-fold on this sub array. Then we
      !!              scatter the north fold array back to the processors.
      !!
      !!----------------------------------------------------------------------
      REAL, DIMENSION(jpi,jpj), INTENT(inout) ::   pt2d      ! 3D array on which the b.c. is applied
      CHARACTER(len=1)            , INTENT(in   ) ::   cd_type   ! nature of pt3d grid-points
      !                                                          !   = T ,  U , V , F or W  gridpoints
      REAL                    , INTENT(in   ) ::   psgn      ! = -1. the sign change across the north fold 
      !!                                                             ! =  1. , the sign is kept
      INTEGER ::   ji, jj, jr
      INTEGER ::   ierr, itaille, ildi, ilei, iilb
      INTEGER ::   ijpj, ijpjm1, ij, iproc
      REAL, DIMENSION(jpiglo,4)      ::   ztab
      REAL, DIMENSION(jpi   ,4)      ::   znorthloc
      REAL, DIMENSION(jpi   ,4,jpni) ::   znorthgloio
      !!----------------------------------------------------------------------
      !
      ijpj   = 4
      ijpjm1 = 3
      !
      DO jj = nlcj-ijpj+1, nlcj             ! put in znorthloc the last 4 jlines of pt2d
         ij = jj - nlcj + ijpj
         znorthloc(:,ij) = pt2d(:,jj)
      END DO

      !                                     ! Build in procs of ncomm_north the znorthgloio
      itaille = jpi * ijpj
      CALL MPI_ALLGATHER( znorthloc  , itaille, MPI_DOUBLE_PRECISION,        &
         &                znorthgloio, itaille, MPI_DOUBLE_PRECISION, ncomm_north, ierr )
      !
      DO jr = 1, ndim_rank_north            ! recover the global north array
         iproc = nrank_north(jr) + 1
         ildi=nldit (iproc)
         ilei=nleit (iproc)
         iilb=nimppt(iproc)
         DO jj = 1, 4
            DO ji = ildi, ilei
               ztab(ji+iilb-1,jj) = znorthgloio(ji,jj,jr)
            END DO
         END DO
      END DO
      !
      CALL lbc_nfd( ztab, cd_type, psgn )   ! North fold boundary condition
      !
      !
      DO jj = nlcj-ijpj+1, nlcj             ! Scatter back to pt2d
         ij = jj - nlcj + ijpj
         DO ji = 1, nlci
            pt2d(ji,jj) = ztab(ji+nimpp-1,ij)
         END DO
      END DO
      !
   END SUBROUTINE mpp_lbc_north_2d


   SUBROUTINE mpp_lbc_north_e( pt2d, cd_type, psgn)
      !!---------------------------------------------------------------------
      !!                   ***  routine mpp_lbc_north_2d  ***
      !!
      !! ** Purpose :   Ensure proper north fold horizontal bondary condition 
      !!              in mpp configuration in case of jpn1 > 1 and for 2d 
      !!              array with outer extra halo
      !!
      !! ** Method  :   North fold condition and mpp with more than one proc
      !!              in i-direction require a specific treatment. We gather 
      !!              the 4+2*jpr2dj northern lines of the global domain on 1 
      !!              processor and apply lbc north-fold on this sub array. 
      !!              Then we scatter the north fold array back to the processors.
      !!
      !!----------------------------------------------------------------------
      REAL, DIMENSION(1-jpr2di:jpi+jpr2di,1-jpr2dj:jpj+jpr2dj), INTENT(inout) ::   pt2d     ! 2D array with extra halo
      CHARACTER(len=1)                                            , INTENT(in   ) ::   cd_type  ! nature of pt3d grid-points
      !                                                                                         !   = T ,  U , V , F or W -points
      REAL                                                    , INTENT(in   ) ::   psgn     ! = -1. the sign change across the  
      !!                                                                                        ! north fold, =  1. otherwise
      INTEGER ::   ji, jj, jr
      INTEGER ::   ierr, itaille, ildi, ilei, iilb
      INTEGER ::   ijpj, ij, iproc
      REAL, DIMENSION(jpiglo,4+2*jpr2dj)      ::   ztab
      REAL, DIMENSION(jpi   ,4+2*jpr2dj)      ::   znorthloc
      REAL, DIMENSION(jpi   ,4+2*jpr2dj,jpni) ::   znorthgloio
      !!----------------------------------------------------------------------
      !
      ijpj=4

      ij=0
      ! put in znorthloc the last 4 jlines of pt2d
      DO jj = nlcj - ijpj + 1 - jpr2dj, nlcj +jpr2dj
         ij = ij + 1
         DO ji = 1, jpi
            znorthloc(ji,ij)=pt2d(ji,jj)
         END DO
      END DO
      !
      itaille = jpi * ( ijpj + 2 * jpr2dj )
      CALL MPI_ALLGATHER( znorthloc(1,1)    , itaille, MPI_DOUBLE_PRECISION,    &
         &                znorthgloio(1,1,1), itaille, MPI_DOUBLE_PRECISION, ncomm_north, ierr )
      !
      DO jr = 1, ndim_rank_north            ! recover the global north array
         iproc = nrank_north(jr) + 1
         ildi = nldit (iproc)
         ilei = nleit (iproc)
         iilb = nimppt(iproc)
         DO jj = 1, ijpj+2*jpr2dj
            DO ji = ildi, ilei
               ztab(ji+iilb-1,jj) = znorthgloio(ji,jj,jr)
            END DO
         END DO
      END DO


      ! 2. North-Fold boundary conditions
      ! ----------------------------------
      CALL lbc_nfd( ztab(:,:), cd_type, psgn, pr2dj = jpr2dj )

      ij = jpr2dj
      !! Scatter back to pt2d
      DO jj = nlcj - ijpj + 1 , nlcj +jpr2dj
      ij  = ij +1 
         DO ji= 1, nlci
            pt2d(ji,jj) = ztab(ji+nimpp-1,ij)
         END DO
      END DO
      !
   END SUBROUTINE mpp_lbc_north_e


   SUBROUTINE lbc_nfd_3d( pt3d, cd_type, psgn )
      !!----------------------------------------------------------------------
      !!                  ***  routine lbc_nfd_3d  ***
      !!
      !! ** Purpose :   3D lateral boundary condition : North fold treatment
      !!       without processor exchanges. 
      !!
      !! ** Method  :   
      !!
      !! ** Action  :   pt3d with update value at its periphery
      !!
      !!----------------------------------------------------------------------
      !! * Arguments
      CHARACTER(len=1) , INTENT( in ) ::   &
         cd_type       ! define the nature of ptab array grid-points
      !             ! = T , U , V , F , W points
      !             ! = S : T-point, north fold treatment ???
      !             ! = G : F-point, north fold treatment ???
      REAL, INTENT( in ) ::   &
         psgn          ! control of the sign change
      !             !   = -1. , the sign is changed if north fold boundary
      !             !   =  1. , the sign is kept  if north fold boundary
      REAL, DIMENSION(:,:,:), INTENT( inout ) ::   &
         pt3d          ! 3D array on which the boundary condition is applied

      !! * Local declarations
      INTEGER  ::   ji, jk
      INTEGER  ::   ijt, iju, ijpj, ijpjm1


      SELECT CASE ( jpni )
      CASE ( 1 )  ! only one proc along I
         ijpj = nlcj
      CASE DEFAULT 
         ijpj   = 4
      END SELECT
      ijpjm1 = ijpj-1

      DO jk = 1, jpk

         SELECT CASE ( npolj )

         CASE ( 3 , 4 )                        ! *  North fold  T-point pivot

            SELECT CASE ( cd_type )
            CASE ( 'T' , 'W' )                         ! T-, W-point
               DO ji = 2, jpiglo
                  ijt = jpiglo-ji+2
                  pt3d(ji,ijpj,jk) = psgn * pt3d(ijt,ijpj-2,jk)
               END DO
               DO ji = jpiglo/2+1, jpiglo
                  ijt = jpiglo-ji+2
                  pt3d(ji,ijpjm1,jk) = psgn * pt3d(ijt,ijpjm1,jk)
               END DO
            CASE ( 'U' )                               ! U-point
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji+1
                  pt3d(ji,ijpj,jk) = psgn * pt3d(iju,ijpj-2,jk)
               END DO
               DO ji = jpiglo/2, jpiglo-1
                  iju = jpiglo-ji+1
                  pt3d(ji,ijpjm1,jk) = psgn * pt3d(iju,ijpjm1,jk)
               END DO
            CASE ( 'V' )                               ! V-point
               DO ji = 2, jpiglo
                  ijt = jpiglo-ji+2
                  pt3d(ji,ijpj-1,jk) = psgn * pt3d(ijt,ijpj-2,jk)
                  pt3d(ji,ijpj  ,jk) = psgn * pt3d(ijt,ijpj-3,jk)
               END DO
            CASE ( 'F' )                               ! F-point
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji+1
                  pt3d(ji,ijpj-1,jk) = psgn * pt3d(iju,ijpj-2,jk)
                  pt3d(ji,ijpj  ,jk) = psgn * pt3d(iju,ijpj-3,jk)
               END DO
            END SELECT

         CASE ( 5 , 6 )                        ! *  North fold  F-point pivot

            SELECT CASE ( cd_type )
            CASE ( 'T' , 'W' )                         ! T-, W-point
               DO ji = 1, jpiglo
                  ijt = jpiglo-ji+1
                  pt3d(ji,ijpj,jk) = psgn * pt3d(ijt,ijpj-1,jk)
               END DO
            CASE ( 'U' )                               ! U-point
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji
                  pt3d(ji,ijpj,jk) = psgn * pt3d(iju,ijpj-1,jk)
               END DO
            CASE ( 'V' )                               ! V-point
               DO ji = 1, jpiglo
                  ijt = jpiglo-ji+1
                  pt3d(ji,ijpj,jk) = psgn * pt3d(ijt,ijpj-2,jk)
               END DO
               DO ji = jpiglo/2+1, jpiglo
                  ijt = jpiglo-ji+1
                  pt3d(ji,ijpjm1,jk) = psgn * pt3d(ijt,ijpjm1,jk)
               END DO
            CASE ( 'F' )                               ! F-point
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji
                  pt3d(ji,ijpj  ,jk) = psgn * pt3d(iju,ijpj-2,jk)
               END DO
               DO ji = jpiglo/2+1, jpiglo-1
                  iju = jpiglo-ji
                  pt3d(ji,ijpjm1,jk) = psgn * pt3d(iju,ijpjm1,jk)
               END DO
            END SELECT

         CASE DEFAULT                           ! *  closed : the code probably never go through

            SELECT CASE ( cd_type)
            CASE ( 'T' , 'U' , 'V' , 'W' )             ! T-, U-, V-, W-points
               pt3d(:, 1  ,jk) = 0.e0
               pt3d(:,ijpj,jk) = 0.e0
            CASE ( 'F' )                               ! F-point
               pt3d(:,ijpj,jk) = 0.e0
            END SELECT

         END SELECT     !  npolj

      END DO

   END SUBROUTINE lbc_nfd_3d


   SUBROUTINE lbc_nfd_2d( pt2d, cd_type, psgn, pr2dj )
      !!----------------------------------------------------------------------
      !!                  ***  routine lbc_nfd_2d  ***
      !!
      !! ** Purpose :   2D lateral boundary condition : North fold treatment
      !!       without processor exchanges. 
      !!
      !! ** Method  :   
      !!
      !! ** Action  :   pt2d with update value at its periphery
      !!
      !!----------------------------------------------------------------------
      !! * Arguments
      CHARACTER(len=1) , INTENT( in ) ::   &
         cd_type       ! define the nature of ptab array grid-points
      !             ! = T , U , V , F , W points
      !             ! = S : T-point, north fold treatment ???
      !             ! = G : F-point, north fold treatment ???
      REAL, INTENT( in ) ::   &
         psgn          ! control of the sign change
      !             !   = -1. , the sign is changed if north fold boundary
      !             !   =  1. , the sign is kept  if north fold boundary
      REAL, DIMENSION(:,:), INTENT( inout ) ::   &
         pt2d          ! 3D array on which the boundary condition is applied
      INTEGER, OPTIONAL, INTENT(in) :: pr2dj

      !! * Local declarations
      INTEGER  ::   ji, jl, ipr2dj
      INTEGER  ::   ijt, iju, ijpj, ijpjm1

      SELECT CASE ( jpni )
      CASE ( 1 )  ! only one proc along I
         ijpj = nlcj
      CASE DEFAULT 
         ijpj = 4
      END SELECT


      IF( PRESENT(pr2dj) ) THEN
         ipr2dj = pr2dj
         IF (jpni .GT. 1) ijpj = ijpj + ipr2dj
      ELSE
         ipr2dj = 0 
      ENDIF

      ijpjm1 = ijpj-1


      SELECT CASE ( npolj )

      CASE ( 3, 4 )                       ! *  North fold  T-point pivot

         SELECT CASE ( cd_type )

         CASE ( 'T', 'S', 'W' )
            DO jl = 0, ipr2dj
               DO ji = 2, jpiglo
                  ijt=jpiglo-ji+2
                  pt2d(ji,ijpj+jl) = psgn * pt2d(ijt,ijpj-2-jl)
               END DO
            END DO
            DO ji = jpiglo/2+1, jpiglo
               ijt=jpiglo-ji+2
               pt2d(ji,ijpj-1) = psgn * pt2d(ijt,ijpj-1)
            END DO
         CASE ( 'U' )                                     ! U-point
            DO jl =0, ipr2dj
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji+1
                  pt2d(ji,ijpj+jl) = psgn * pt2d(iju,ijpj-2-jl)
               END DO
            END DO
            DO ji = jpiglo/2, jpiglo-1
               iju = jpiglo-ji+1
               pt2d(ji,ijpjm1) = psgn * pt2d(iju,ijpjm1)
            END DO
         CASE ( 'V' )                                     ! V-point
            DO jl =-1, ipr2dj
               DO ji = 2, jpiglo
                  ijt = jpiglo-ji+2
                  pt2d(ji,ijpj+jl) = psgn * pt2d(ijt,ijpj-3-jl)
               END DO
            END DO
         CASE ( 'F' , 'G' )                               ! F-point
            DO jl =-1, ipr2dj
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji+1
                  pt2d(ji,ijpj+jl) = psgn * pt2d(iju,ijpj-3-jl)
               END DO
            END DO
         CASE ( 'I' )                                     ! ice U-V point
! Change in I-point definition for Gelato
!            DO jl =0, ipr2dj
!               pt2d(2,ijpj+jl) = psgn * pt2d(3,ijpj-1+jl)
!               DO ji = 3, jpiglo
!                  iju = jpiglo - ji + 3
!                  pt2d(ji,ijpj+jl) = psgn * pt2d(iju,ijpj-1-jl)
!               END DO
!            END DO
            DO jl =0, ipr2dj
               pt2d(1,ijpj+jl) = psgn * pt2d(2,ijpj-1+jl)
               DO ji = 2, jpiglo
                  iju = jpiglo - ji + 2
                  pt2d(ji,ijpj+jl) = psgn * pt2d(iju,ijpj-1-jl)
               END DO
            END DO
         END SELECT

      CASE ( 5, 6 )                        ! *  North fold  F-point pivot

         SELECT CASE ( cd_type )
         CASE ( 'T' , 'W' ,'S' )                          ! T-, W-point
            DO jl = 0, ipr2dj
               DO ji = 1, jpiglo
                  ijt = jpiglo-ji+1
                  pt2d(ji,ijpj+jl) = psgn * pt2d(ijt,ijpj-1-jl)
               END DO
            END DO
         CASE ( 'U' )                                     ! U-point
            DO jl = 0, ipr2dj
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji
                  pt2d(ji,ijpj+jl) = psgn * pt2d(iju,ijpj-1-jl)
               END DO
            END DO
         CASE ( 'V' )                                     ! V-point
            DO jl = 0, ipr2dj
               DO ji = 1, jpiglo
                  ijt = jpiglo-ji+1
                  pt2d(ji,ijpj+jl) = psgn * pt2d(ijt,ijpj-2-jl)
               END DO
            END DO
            DO ji = jpiglo/2+1, jpiglo
               ijt = jpiglo-ji+1
               pt2d(ji,ijpjm1) = psgn * pt2d(ijt,ijpjm1)
            END DO
         CASE ( 'F' , 'G' )                               ! F-point
            DO jl = 0, ipr2dj
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji
                  pt2d(ji,ijpj+jl) = psgn * pt2d(iju,ijpj-2-jl)
               END DO
            END DO
            DO ji = jpiglo/2+1, jpiglo-1
               iju = jpiglo-ji
               pt2d(ji,ijpjm1) = psgn * pt2d(iju,ijpjm1)
            END DO
         CASE ( 'I' )                                  ! ice U-V point
            pt2d( 2 ,ijpj:ijpj+ipr2dj) = 0.e0
            DO jl = 0, ipr2dj
               DO ji = 2 , jpiglo-1
                  ijt = jpiglo - ji + 2
                  pt2d(ji,ijpj+jl)= 0.5 * ( pt2d(ji,ijpj-1-jl) + psgn * pt2d(ijt,ijpj-1-jl) )
               END DO
            END DO
         END SELECT

      CASE DEFAULT                           ! *  closed : the code probably never go through

         SELECT CASE ( cd_type)
         CASE ( 'T' , 'U' , 'V' , 'W' )                 ! T-, U-, V-, W-points
            pt2d(:, 1:1-ipr2dj     ) = 0.e0
            pt2d(:,ijpj:ijpj+ipr2dj) = 0.e0
         CASE ( 'F' )                                   ! F-point
            pt2d(:,ijpj:ijpj+ipr2dj) = 0.e0
         CASE ( 'I' )                                   ! ice U-V point
            pt2d(:, 1:1-ipr2dj     ) = 0.e0
            pt2d(:,ijpj:ijpj+ipr2dj) = 0.e0
         END SELECT

      END SELECT

   END SUBROUTINE lbc_nfd_2d
#endif


   !!======================================================================
END MODULE mode_glt_nemo_bound
