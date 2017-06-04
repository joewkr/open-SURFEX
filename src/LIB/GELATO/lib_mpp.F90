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
MODULE lib_mpp
   !!======================================================================
   !!                       ***  MODULE  lib_mpp  ***
   !! Ocean numerics:  massively parallel processing library
   !!=====================================================================
   !! History :  OPA  !  1994  (M. Guyon, J. Escobar, M. Imbard)  Original code
   !!            7.0  !  1997  (A.M. Treguier)  SHMEM additions
   !!            8.0  !  1998  (M. Imbard, J. Escobar, L. Colombet ) SHMEM and MPI
   !!                 !  1998  (J.M. Molines) Open boundary conditions
   !!   NEMO     1.0  !  2003  (J.-M. Molines, G. Madec)  F90, free form
   !!                 !  2003  (J.M. Molines) add mpp_ini_north(_3d,_2d)
   !!             -   !  2004  (R. Bourdalle Badie)  isend option in mpi
   !!                 !  2004  (J.M. Molines) minloc, maxloc
   !!             -   !  2005  (G. Madec, S. Masson)  npolj=5,6 F-point & ice cases
   !!             -   !  2005  (R. Redler) Replacement of MPI_COMM_WORLD except for MPI_Abort
   !!             -   !  2005  (R. Benshila, G. Madec)  add extra halo case
   !!             -   !  2008  (R. Benshila) add mpp_ini_ice
   !!            3.2  !  2009  (R. Benshila) SHMEM suppression, north fold in lbc_nfd
   !!            3.2  !  2009  (O. Marti)    add mpp_ini_znl
   !!            4.0  !  2011  (G. Madec)  move ctl_ routines from in_out_manager
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   ctl_stop   : update momentum and tracer Kz from a tke scheme
   !!   ctl_warn   : initialization, namelist read, and parameters control
   !!   ctl_opn    : Open file and check if required file is available.
   !!   get_unit    : give the index of an unused logical unit
   !!----------------------------------------------------------------------
#if   defined key_mpp_mpi || ! defined in_nemo
#if ! defined in_surfex
   !!----------------------------------------------------------------------
   !!   'key_mpp_mpi'             MPI massively parallel processing library
   !!----------------------------------------------------------------------
   !!   lib_mpp_alloc : allocate mpp arrays
   !!   mynode        : indentify the processor unit
   !!   mpp_lnk       : interface (defined in lbclnk) for message passing of 2d or 3d arrays (mpp_lnk_2d, mpp_lnk_3d)
   !!   mpp_lnk_3d_gather :  Message passing manadgement for two 3D arrays
   !!   mpp_lnk_e     : interface (defined in lbclnk) for message passing of 2d array with extra halo (mpp_lnk_2d_e)
   !!   mpprecv         :
   !!   mppsend       :   SUBROUTINE mpp_ini_znl
   !!   mppscatter    :
   !!   mppgather     :
   !!   mpp_min       : generic interface for mppmin_int , mppmin_a_int , mppmin_real, mppmin_a_real
   !!   mpp_max       : generic interface for mppmax_int , mppmax_a_int , mppmax_real, mppmax_a_real
   !!   mpp_sum       : generic interface for mppsum_int , mppsum_a_int , mppsum_real, mppsum_a_real
   !!   mpp_minloc    :
   !!   mpp_maxloc    :
   !!   mppsync       :
   !!   mppstop       :
   !!   mppobc        : variant of mpp_lnk for open boundary condition
   !!   mpp_ini_north : initialisation of north fold
   !!   mpp_lbc_north : north fold processors gathering
   !!   mpp_lbc_north_e : variant of mpp_lbc_north for extra outer halo
   !!----------------------------------------------------------------------
   USE dom_oce        ! ocean space and time domain
   USE lbcnfd         ! north fold treatment
   USE in_out_manager ! I/O manager
#if ! defined in_nemo
   USE mpi
#endif
#else
!! case: in_surfex
   USE MODI_ABOR1_SFX
   USE modd_wp
   USE PARKIND1         , ONLY : JPIB, JPRB
#ifdef SFX_MPI
   !!  Case Surfex Offline with MPI
   !!  define mpp_min, mpp_max, mpp_sum for Offline Surfex case with MPI
   USE MODD_SURFEX_MPI, ONLY : mpi_comm_opa => NCOMM
#ifdef in_arpege
   !! Case SUrfex in Arpege
   USE MPL_ALLREDUCE_MOD, ONLY : MPL_ALLREDUCE
#endif
#else
   !! Case of Offline without MPI : no call to MPI, 
   !! mpp_min, mpp_max, mpp_sum are dummies (see below)
#endif
#endif

   IMPLICIT NONE
#if ! defined in_surfex
   PRIVATE
#else
#ifdef SFX_MPI
   INCLUDE 'mpif.h'
#endif
#endif

#if ! defined in_surfex
   PUBLIC   ctl_stop, ctl_warn, get_unit, ctl_opn
   PUBLIC   mynode, mppstop, mppsync !, mpp_comm_free
   PUBLIC   mpp_ini_north, mpp_lbc_north, mpp_lbc_north_e
   PUBLIC   mpp_min, mpp_max, mpp_sum, mpp_minloc, mpp_maxloc
   PUBLIC   mpp_lnk_3d, mpp_lnk_3d_gather, mpp_lnk_2d, mpp_lnk_2d_e
   PUBLIC   mppscatter, mppgather
   PUBLIC   mppobc, mpp_ini_ice, mpp_ini_znl
   PUBLIC   mppsize, mpprank ! (PUBLIC for GELATO)
   PUBLIC   lib_mpp_alloc    ! Called in nemogcm.F90
   PUBLIC   mppsend, mpprecv ! (PUBLIC for TAM)
#endif
   !! * Interfaces
   !! define generic interface for these routine as they are called sometimes
   !! with scalar arguments instead of array arguments, which causes problems
   !! for the compilation on AIX system as well as NEC and SGI. Ok on COMPACQ
   INTERFACE mpp_min
      MODULE PROCEDURE mppmin_a_int, mppmin_int, mppmin_a_real, mppmin_real
   END INTERFACE
   INTERFACE mpp_max
      MODULE PROCEDURE mppmax_a_int, mppmax_int, mppmax_a_real, mppmax_real
   END INTERFACE
   INTERFACE mpp_sum
#if ! defined in_surfex
      MODULE PROCEDURE mppsum_a_int, mppsum_int, mppsum_a_real, mppsum_real, &
                       mppsum_realdd, mppsum_a_realdd
#else
      MODULE PROCEDURE mppsum_a_int, mppsum_int, mppsum_a_real, mppsum_real
#endif
   END INTERFACE
#if ! defined in_surfex
   INTERFACE mpp_lbc_north
      MODULE PROCEDURE mpp_lbc_north_3d, mpp_lbc_north_2d
   END INTERFACE
   INTERFACE mpp_minloc
      MODULE PROCEDURE mpp_minloc2d ,mpp_minloc3d
   END INTERFACE
   INTERFACE mpp_maxloc
      MODULE PROCEDURE mpp_maxloc2d ,mpp_maxloc3d
   END INTERFACE

   !! ========================= !!
   !!  MPI  variable definition !!
   !! ========================= !!
!$AGRIF_DO_NOT_TREAT
!   INCLUDE 'mpif.h'   ! Replaced with USE mpi
!$AGRIF_END_DO_NOT_TREAT

   LOGICAL, PUBLIC, PARAMETER ::   lk_mpp = .TRUE.    !: mpp flag

   INTEGER, PARAMETER         ::   nprocmax = 2**10   ! maximun dimension (required to be a power of 2)

   INTEGER ::   mppsize        ! number of process
   INTEGER ::   mpprank        ! process number  [ 0 - size-1 ]
!$AGRIF_DO_NOT_TREAT
   INTEGER, PUBLIC ::   mpi_comm_opa   ! opa local communicator
!$AGRIF_END_DO_NOT_TREAT

   INTEGER :: MPI_SUMDD

   ! variables used in case of sea-ice
   INTEGER, PUBLIC ::   ncomm_ice       !: communicator made by the processors with sea-ice (public so that it can be freed in limthd)
   INTEGER ::   ngrp_iworld     !  group ID for the world processors (for rheology)
   INTEGER ::   ngrp_ice        !  group ID for the ice processors (for rheology)
   INTEGER ::   ndim_rank_ice   !  number of 'ice' processors
   INTEGER ::   n_ice_root      !  number (in the comm_ice) of proc 0 in the ice comm
   INTEGER, DIMENSION(:), ALLOCATABLE, SAVE ::   nrank_ice     ! dimension ndim_rank_ice

   ! variables used for zonal integration
   INTEGER, PUBLIC ::   ncomm_znl       !: communicator made by the processors on the same zonal average
   LOGICAL, PUBLIC ::   l_znl_root      ! True on the 'left'most processor on the same row
   INTEGER ::   ngrp_znl        ! group ID for the znl processors
   INTEGER ::   ndim_rank_znl   ! number of processors on the same zonal average
   INTEGER, DIMENSION(:), ALLOCATABLE, SAVE ::   nrank_znl  ! dimension ndim_rank_znl, number of the procs into the same znl domain

   ! North fold condition in mpp_mpi with jpni > 1 (PUBLIC for TAM)
   INTEGER, PUBLIC ::   ngrp_world        ! group ID for the world processors
   INTEGER, PUBLIC ::   ngrp_opa          ! group ID for the opa processors
   INTEGER, PUBLIC ::   ngrp_north        ! group ID for the northern processors (to be fold)
   INTEGER, PUBLIC ::   ncomm_north       ! communicator made by the processors belonging to ngrp_north
   INTEGER, PUBLIC ::   ndim_rank_north   ! number of 'sea' processor in the northern line (can be /= jpni !)
   INTEGER, PUBLIC ::   njmppmax          ! value of njmpp for the processors of the northern line
   INTEGER, PUBLIC ::   north_root        ! number (in the comm_opa) of proc 0 in the northern comm
   INTEGER, DIMENSION(:), ALLOCATABLE, SAVE, PUBLIC ::   nrank_north   ! dimension ndim_rank_north

   ! Type of send : standard, buffered, immediate
   CHARACTER(len=1), PUBLIC ::   cn_mpi_send = 'S'    ! type od mpi send/recieve (S=standard, B=bsend, I=isend)
   LOGICAL, PUBLIC          ::   l_isend = .FALSE.   ! isend use indicator (T if cn_mpi_send='I')
   INTEGER, PUBLIC          ::   nn_buffer = 0       ! size of the buffer in case of mpi_bsend

   REAL(wp), DIMENSION(:), ALLOCATABLE, SAVE :: tampon  ! buffer in case of bsend

   ! message passing arrays
   REAL(wp), DIMENSION(:,:,:,:,:), ALLOCATABLE, SAVE ::   t4ns, t4sn   ! 2 x 3d for north-south & south-north
   REAL(wp), DIMENSION(:,:,:,:,:), ALLOCATABLE, SAVE ::   t4ew, t4we   ! 2 x 3d for east-west & west-east
   REAL(wp), DIMENSION(:,:,:,:,:), ALLOCATABLE, SAVE ::   t4p1, t4p2   ! 2 x 3d for north fold
   REAL(wp), DIMENSION(:,:,:,:)  , ALLOCATABLE, SAVE ::   t3ns, t3sn   ! 3d for north-south & south-north
   REAL(wp), DIMENSION(:,:,:,:)  , ALLOCATABLE, SAVE ::   t3ew, t3we   ! 3d for east-west & west-east
   REAL(wp), DIMENSION(:,:,:,:)  , ALLOCATABLE, SAVE ::   t3p1, t3p2   ! 3d for north fold
   REAL(wp), DIMENSION(:,:,:)    , ALLOCATABLE, SAVE ::   t2ns, t2sn   ! 2d for north-south & south-north
   REAL(wp), DIMENSION(:,:,:)    , ALLOCATABLE, SAVE ::   t2ew, t2we   ! 2d for east-west & west-east
   REAL(wp), DIMENSION(:,:,:)    , ALLOCATABLE, SAVE ::   t2p1, t2p2   ! 2d for north fold
   REAL(wp), DIMENSION(:,:,:)    , ALLOCATABLE, SAVE ::   tr2ns, tr2sn ! 2d for north-south & south-north + extra outer halo
   REAL(wp), DIMENSION(:,:,:)    , ALLOCATABLE, SAVE ::   tr2ew, tr2we ! 2d for east-west   & west-east   + extra outer halo

   ! Arrays used in mpp_lbc_north_3d()
   REAL(wp), DIMENSION(:,:,:)  , ALLOCATABLE, SAVE   ::   ztab, znorthloc
   REAL(wp), DIMENSION(:,:,:,:), ALLOCATABLE, SAVE   ::   znorthgloio
   REAL(wp), DIMENSION(:,:,:)  , ALLOCATABLE, SAVE   ::   zfoldwk      ! Workspace for message transfers avoiding mpi_allgather

   ! Arrays used in mpp_lbc_north_2d()
   REAL(wp), DIMENSION(:,:)  , ALLOCATABLE, SAVE    ::   ztab_2d, znorthloc_2d
   REAL(wp), DIMENSION(:,:,:), ALLOCATABLE, SAVE    ::   znorthgloio_2d
   REAL(wp), DIMENSION(:,:)  , ALLOCATABLE, SAVE    ::   zfoldwk_2d    ! Workspace for message transfers avoiding mpi_allgather

   ! Arrays used in mpp_lbc_north_e()
   REAL(wp), DIMENSION(:,:)  , ALLOCATABLE, SAVE    ::   ztab_e, znorthloc_e
   REAL(wp), DIMENSION(:,:,:), ALLOCATABLE, SAVE    ::   znorthgloio_e

   ! North fold arrays used to minimise the use of allgather operations. Set in nemo_northcomms (nemogcm) so need to be public
   INTEGER, PUBLIC,  PARAMETER :: jpmaxngh = 8                 ! Assumed maximum number of active neighbours
   INTEGER, PUBLIC,  PARAMETER :: jptyps   = 5                 ! Number of different neighbour lists to be used for northfold exchanges
   INTEGER, PUBLIC,  DIMENSION (jpmaxngh,jptyps)    ::   isendto
   INTEGER, PUBLIC,  DIMENSION (jptyps)             ::   nsndto
   LOGICAL, PUBLIC                                  ::   ln_nnogather     = .FALSE.  ! namelist control of northfold comms
   LOGICAL, PUBLIC                                  ::   l_north_nogather = .FALSE.  ! internal control of northfold comms
   INTEGER, PUBLIC                                  ::   ityp
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id$
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
#endif
CONTAINS
#if ! defined in_surfex
   INTEGER FUNCTION lib_mpp_alloc( kumout )
      !!----------------------------------------------------------------------
      !!              ***  routine lib_mpp_alloc  ***
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kumout   ! ocean.output logical unit
      !!----------------------------------------------------------------------
      !
      ALLOCATE( t4ns(jpi,jprecj,jpk,2,2) , t4sn(jpi,jprecj,jpk,2,2) ,                                            &
         &      t4ew(jpj,jpreci,jpk,2,2) , t4we(jpj,jpreci,jpk,2,2) ,                                            &
         &      t4p1(jpi,jprecj,jpk,2,2) , t4p2(jpi,jprecj,jpk,2,2) ,                                            &
         &      t3ns(jpi,jprecj,jpk,2)   , t3sn(jpi,jprecj,jpk,2)   ,                                            &
         &      t3ew(jpj,jpreci,jpk,2)   , t3we(jpj,jpreci,jpk,2)   ,                                            &
         &      t3p1(jpi,jprecj,jpk,2)   , t3p2(jpi,jprecj,jpk,2)   ,                                            &
         &      t2ns(jpi,jprecj    ,2)   , t2sn(jpi,jprecj    ,2)   ,                                            &
         &      t2ew(jpj,jpreci    ,2)   , t2we(jpj,jpreci    ,2)   ,                                            &
         &      t2p1(jpi,jprecj    ,2)   , t2p2(jpi,jprecj    ,2)   ,                                            &
         !
         &      tr2ns(1-jpr2di:jpi+jpr2di,jprecj+jpr2dj,2) ,                                                     &
         &      tr2sn(1-jpr2di:jpi+jpr2di,jprecj+jpr2dj,2) ,                                                     &
         &      tr2ew(1-jpr2dj:jpj+jpr2dj,jpreci+jpr2di,2) ,                                                     &
         &      tr2we(1-jpr2dj:jpj+jpr2dj,jpreci+jpr2di,2) ,                                                     &
         !
         &      ztab(jpiglo,4,jpk) , znorthloc(jpi,4,jpk) , znorthgloio(jpi,4,jpk,jpni) ,                        &
         &      zfoldwk(jpi,4,jpk) ,                                                                             &
         !
         &      ztab_2d(jpiglo,4)  , znorthloc_2d(jpi,4)  , znorthgloio_2d(jpi,4,jpni)  ,                        &
         &      zfoldwk_2d(jpi,4)  ,                                                                             &
         !
         &      ztab_e(jpiglo,4+2*jpr2dj) , znorthloc_e(jpi,4+2*jpr2dj) , znorthgloio_e(jpi,4+2*jpr2dj,jpni) ,   &
         !
         &      STAT=lib_mpp_alloc )
         !
      IF( lib_mpp_alloc /= 0 ) THEN
         WRITE(kumout,cform_war)
         WRITE(kumout,*) 'lib_mpp_alloc : failed to allocate arrays'
      ENDIF
      !
   END FUNCTION lib_mpp_alloc


   FUNCTION mynode( ldtxt, kumnam, kstop, localComm )
      !!----------------------------------------------------------------------
      !!                  ***  routine mynode  ***
      !!
      !! ** Purpose :   Find processor unit
      !!----------------------------------------------------------------------
      CHARACTER(len=*),DIMENSION(:), INTENT(  out) ::   ldtxt
      INTEGER                      , INTENT(in   ) ::   kumnam       ! namelist logical unit
      INTEGER                      , INTENT(inout) ::   kstop        ! stop indicator
      INTEGER, OPTIONAL            , INTENT(in   ) ::   localComm
      !
      INTEGER ::   mynode, ierr, code, ji, ii
      LOGICAL ::   mpi_was_called
      !
      NAMELIST/nammpp/ cn_mpi_send, nn_buffer, jpni, jpnj, jpnij, ln_nnogather
      !!----------------------------------------------------------------------
      !
      ii = 1
      WRITE(ldtxt(ii),*)                                                                          ;   ii = ii + 1
      WRITE(ldtxt(ii),*) 'mynode : mpi initialisation'                                            ;   ii = ii + 1
      WRITE(ldtxt(ii),*) '~~~~~~ '                                                                ;   ii = ii + 1
      !
      jpni = -1; jpnj = -1; jpnij = -1
      REWIND( kumnam )               ! Namelist namrun : parameters of the run
      READ  ( kumnam, nammpp )
      !                              ! control print
      WRITE(ldtxt(ii),*) '   Namelist nammpp'                                                     ;   ii = ii + 1
      WRITE(ldtxt(ii),*) '      mpi send type                      cn_mpi_send = ', cn_mpi_send   ;   ii = ii + 1
      WRITE(ldtxt(ii),*) '      size in bytes of exported buffer   nn_buffer   = ', nn_buffer     ;   ii = ii + 1

#if defined key_agrif
      IF( .NOT. Agrif_Root() ) THEN
         jpni  = Agrif_Parent(jpni )
         jpnj  = Agrif_Parent(jpnj )
         jpnij = Agrif_Parent(jpnij)
      ENDIF
#endif

      IF(jpnij < 1)THEN
         ! If jpnij is not specified in namelist then we calculate it - this
         ! means there will be no land cutting out.
         jpnij = jpni * jpnj
      END IF

      IF( (jpni < 1) .OR. (jpnj < 1) )THEN
         WRITE(ldtxt(ii),*) '      jpni, jpnj and jpnij will be calculated automatically'; ii = ii + 1
      ELSE
         WRITE(ldtxt(ii),*) '      processor grid extent in i         jpni = ',jpni; ii = ii + 1
         WRITE(ldtxt(ii),*) '      processor grid extent in j         jpnj = ',jpnj; ii = ii + 1
         WRITE(ldtxt(ii),*) '      number of local domains           jpnij = ',jpnij; ii = ii +1
      END IF

      WRITE(ldtxt(ii),*) '      avoid use of mpi_allgather at the north fold  ln_nnogather = ', ln_nnogather  ; ii = ii + 1

      CALL mpi_initialized ( mpi_was_called, code )
      IF( code /= MPI_SUCCESS ) THEN
         DO ji = 1, SIZE(ldtxt)
            IF( TRIM(ldtxt(ji)) /= '' )   WRITE(*,*) ldtxt(ji)      ! control print of mynode
         END DO
         WRITE(*, cform_err)
         WRITE(*, *) 'lib_mpp: Error in routine mpi_initialized'
         CALL mpi_abort( mpi_comm_world, code, ierr )
      ENDIF

      IF( mpi_was_called ) THEN
         !
         SELECT CASE ( cn_mpi_send )
         CASE ( 'S' )                ! Standard mpi send (blocking)
            WRITE(ldtxt(ii),*) '           Standard blocking mpi send (send)'                     ;   ii = ii + 1
         CASE ( 'B' )                ! Buffer mpi send (blocking)
            WRITE(ldtxt(ii),*) '           Buffer blocking mpi send (bsend)'                      ;   ii = ii + 1
            IF( Agrif_Root() )   CALL mpi_init_opa( ldtxt, ii, ierr )
         CASE ( 'I' )                ! Immediate mpi send (non-blocking send)
            WRITE(ldtxt(ii),*) '           Immediate non-blocking send (isend)'                   ;   ii = ii + 1
            l_isend = .TRUE.
         CASE DEFAULT
            WRITE(ldtxt(ii),cform_err)                                                            ;   ii = ii + 1
            WRITE(ldtxt(ii),*) '           bad value for cn_mpi_send = ', cn_mpi_send             ;   ii = ii + 1
            kstop = kstop + 1
         END SELECT
      ELSE IF ( PRESENT(localComm) .and. .not. mpi_was_called ) THEN
         WRITE(ldtxt(ii),*) ' lib_mpp: You cannot provide a local communicator '                  ;   ii = ii + 1
         WRITE(ldtxt(ii),*) '          without calling MPI_Init before ! '                        ;   ii = ii + 1
         kstop = kstop + 1
      ELSE
         SELECT CASE ( cn_mpi_send )
         CASE ( 'S' )                ! Standard mpi send (blocking)
            WRITE(ldtxt(ii),*) '           Standard blocking mpi send (send)'                     ;   ii = ii + 1
            CALL mpi_init( ierr )
         CASE ( 'B' )                ! Buffer mpi send (blocking)
            WRITE(ldtxt(ii),*) '           Buffer blocking mpi send (bsend)'                      ;   ii = ii + 1
            IF( Agrif_Root() )   CALL mpi_init_opa( ldtxt, ii, ierr )
         CASE ( 'I' )                ! Immediate mpi send (non-blocking send)
            WRITE(ldtxt(ii),*) '           Immediate non-blocking send (isend)'                   ;   ii = ii + 1
            l_isend = .TRUE.
            CALL mpi_init( ierr )
         CASE DEFAULT
            WRITE(ldtxt(ii),cform_err)                                                            ;   ii = ii + 1
            WRITE(ldtxt(ii),*) '           bad value for cn_mpi_send = ', cn_mpi_send             ;   ii = ii + 1
            kstop = kstop + 1
         END SELECT
         !
      ENDIF

      IF( PRESENT(localComm) ) THEN
         IF( Agrif_Root() ) THEN
            mpi_comm_opa = localComm
         ENDIF
      ELSE
         CALL mpi_comm_dup( mpi_comm_world, mpi_comm_opa, code)
         IF( code /= MPI_SUCCESS ) THEN
            DO ji = 1, SIZE(ldtxt)
               IF( TRIM(ldtxt(ji)) /= '' )   WRITE(*,*) ldtxt(ji)      ! control print of mynode
            END DO
            WRITE(*, cform_err)
            WRITE(*, *) ' lib_mpp: Error in routine mpi_comm_dup'
            CALL mpi_abort( mpi_comm_world, code, ierr )
         ENDIF
      ENDIF

      CALL mpi_comm_rank( mpi_comm_opa, mpprank, ierr )
      CALL mpi_comm_size( mpi_comm_opa, mppsize, ierr )
      mynode = mpprank
      !
      CALL MPI_OP_CREATE(DDPDD_MPI, .TRUE., MPI_SUMDD, ierr)
      !
   END FUNCTION mynode


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
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(inout) ::   ptab     ! 3D array on which the boundary condition is applied
      CHARACTER(len=1)                , INTENT(in   ) ::   cd_type  ! define the nature of ptab array grid-points
      !                                                             ! = T , U , V , F , W points
      REAL(wp)                        , INTENT(in   ) ::   psgn     ! =-1 the sign change across the north fold boundary
      !                                                             ! =  1. , the sign is kept
      CHARACTER(len=3), OPTIONAL      , INTENT(in   ) ::   cd_mpp   ! fill the overlap area only
      REAL(wp)        , OPTIONAL      , INTENT(in   ) ::   pval     ! background value (used at closed boundaries)
      !!
      INTEGER  ::   ji, jj, jk, jl             ! dummy loop indices
      INTEGER  ::   imigr, iihom, ijhom        ! temporary integers
      INTEGER  ::   ml_req1, ml_req2, ml_err   ! for key_mpi_isend
      REAL(wp) ::   zland
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
         CALL mpprecv( 1, t3ew(1,1,1,2), imigr, noea )
         IF(l_isend) CALL mpi_wait(ml_req1, ml_stat, ml_err)
      CASE ( 0 )
         CALL mppsend( 1, t3ew(1,1,1,1), imigr, nowe, ml_req1 )
         CALL mppsend( 2, t3we(1,1,1,1), imigr, noea, ml_req2 )
         CALL mpprecv( 1, t3ew(1,1,1,2), imigr, noea )
         CALL mpprecv( 2, t3we(1,1,1,2), imigr, nowe )
         IF(l_isend) CALL mpi_wait(ml_req1, ml_stat, ml_err)
         IF(l_isend) CALL mpi_wait(ml_req2, ml_stat, ml_err)
      CASE ( 1 )
         CALL mppsend( 1, t3ew(1,1,1,1), imigr, nowe, ml_req1 )
         CALL mpprecv( 2, t3we(1,1,1,2), imigr, nowe )
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
         CALL mpprecv( 3, t3ns(1,1,1,2), imigr, nono )
         IF(l_isend) CALL mpi_wait(ml_req1, ml_stat, ml_err)
      CASE ( 0 )
         CALL mppsend( 3, t3ns(1,1,1,1), imigr, noso, ml_req1 )
         CALL mppsend( 4, t3sn(1,1,1,1), imigr, nono, ml_req2 )
         CALL mpprecv( 3, t3ns(1,1,1,2), imigr, nono )
         CALL mpprecv( 4, t3sn(1,1,1,2), imigr, noso )
         IF(l_isend) CALL mpi_wait(ml_req1, ml_stat, ml_err)
         IF(l_isend) CALL mpi_wait(ml_req2, ml_stat, ml_err)
      CASE ( 1 )
         CALL mppsend( 3, t3ns(1,1,1,1), imigr, noso, ml_req1 )
         CALL mpprecv( 4, t3sn(1,1,1,2), imigr, noso )
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
      REAL(wp), DIMENSION(jpi,jpj), INTENT(inout) ::   pt2d     ! 2D array on which the boundary condition is applied
      CHARACTER(len=1)            , INTENT(in   ) ::   cd_type  ! define the nature of ptab array grid-points
      !                                                         ! = T , U , V , F , W and I points
      REAL(wp)                    , INTENT(in   ) ::   psgn     ! =-1 the sign change across the north fold boundary
      !                                                         ! =  1. , the sign is kept
      CHARACTER(len=3), OPTIONAL  , INTENT(in   ) ::   cd_mpp   ! fill the overlap area only
      REAL(wp)        , OPTIONAL  , INTENT(in   ) ::   pval     ! background value (used at closed boundaries)
      !!
      INTEGER  ::   ji, jj, jl   ! dummy loop indices
      INTEGER  ::   imigr, iihom, ijhom        ! temporary integers
      INTEGER  ::   ml_req1, ml_req2, ml_err   ! for key_mpi_isend
      REAL(wp) ::   zland
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
         CALL mpprecv( 1, t2ew(1,1,2), imigr, noea )
         IF(l_isend) CALL mpi_wait(ml_req1,ml_stat,ml_err)
      CASE ( 0 )
         CALL mppsend( 1, t2ew(1,1,1), imigr, nowe, ml_req1 )
         CALL mppsend( 2, t2we(1,1,1), imigr, noea, ml_req2 )
         CALL mpprecv( 1, t2ew(1,1,2), imigr, noea )
         CALL mpprecv( 2, t2we(1,1,2), imigr, nowe )
         IF(l_isend) CALL mpi_wait(ml_req1,ml_stat,ml_err)
         IF(l_isend) CALL mpi_wait(ml_req2,ml_stat,ml_err)
      CASE ( 1 )
         CALL mppsend( 1, t2ew(1,1,1), imigr, nowe, ml_req1 )
         CALL mpprecv( 2, t2we(1,1,2), imigr, nowe )
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
         CALL mpprecv( 3, t2ns(1,1,2), imigr, nono )
         IF(l_isend) CALL mpi_wait(ml_req1,ml_stat,ml_err)
      CASE ( 0 )
         CALL mppsend( 3, t2ns(1,1,1), imigr, noso, ml_req1 )
         CALL mppsend( 4, t2sn(1,1,1), imigr, nono, ml_req2 )
         CALL mpprecv( 3, t2ns(1,1,2), imigr, nono )
         CALL mpprecv( 4, t2sn(1,1,2), imigr, noso )
         IF(l_isend) CALL mpi_wait(ml_req1,ml_stat,ml_err)
         IF(l_isend) CALL mpi_wait(ml_req2,ml_stat,ml_err)
      CASE ( 1 )
         CALL mppsend( 3, t2ns(1,1,1), imigr, noso, ml_req1 )
         CALL mpprecv( 4, t2sn(1,1,2), imigr, noso )
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
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(inout) ::   ptab1     ! first and second 3D array on which
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(inout) ::   ptab2     ! the boundary condition is applied
      CHARACTER(len=1)                , INTENT(in   ) ::   cd_type1  ! nature of ptab1 and ptab2 arrays
      CHARACTER(len=1)                , INTENT(in   ) ::   cd_type2  ! i.e. grid-points = T , U , V , F or W points
      REAL(wp)                        , INTENT(in   ) ::   psgn      ! =-1 the sign change across the north fold boundary
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
         CALL mpprecv( 1, t4ew(1,1,1,1,2), imigr, noea )
         IF(l_isend) CALL mpi_wait(ml_req1, ml_stat, ml_err)
      CASE ( 0 )
         CALL mppsend( 1, t4ew(1,1,1,1,1), imigr, nowe, ml_req1 )
         CALL mppsend( 2, t4we(1,1,1,1,1), imigr, noea, ml_req2 )
         CALL mpprecv( 1, t4ew(1,1,1,1,2), imigr, noea )
         CALL mpprecv( 2, t4we(1,1,1,1,2), imigr, nowe )
         IF(l_isend) CALL mpi_wait(ml_req1, ml_stat, ml_err)
         IF(l_isend) CALL mpi_wait(ml_req2, ml_stat, ml_err)
      CASE ( 1 )
         CALL mppsend( 1, t4ew(1,1,1,1,1), imigr, nowe, ml_req1 )
         CALL mpprecv( 2, t4we(1,1,1,1,2), imigr, nowe )
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
         CALL mpprecv( 3, t4ns(1,1,1,1,2), imigr, nono )
         IF(l_isend) CALL mpi_wait(ml_req1, ml_stat, ml_err)
      CASE ( 0 )
         CALL mppsend( 3, t4ns(1,1,1,1,1), imigr, noso, ml_req1 )
         CALL mppsend( 4, t4sn(1,1,1,1,1), imigr, nono, ml_req2 )
         CALL mpprecv( 3, t4ns(1,1,1,1,2), imigr, nono )
         CALL mpprecv( 4, t4sn(1,1,1,1,2), imigr, noso )
         IF(l_isend) CALL mpi_wait(ml_req1, ml_stat, ml_err)
         IF(l_isend) CALL mpi_wait(ml_req2, ml_stat, ml_err)
      CASE ( 1 )
         CALL mppsend( 3, t4ns(1,1,1,1,1), imigr, noso, ml_req1 )
         CALL mpprecv( 4, t4sn(1,1,1,1,2), imigr, noso )
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
      REAL(wp), DIMENSION(1-jpr2di:jpi+jpr2di,1-jpr2dj:jpj+jpr2dj), INTENT(inout) ::   pt2d     ! 2D array with extra halo
      CHARACTER(len=1)                                            , INTENT(in   ) ::   cd_type  ! nature of ptab array grid-points
      !                                                                                         ! = T , U , V , F , W and I points
      REAL(wp)                                                    , INTENT(in   ) ::   psgn     ! =-1 the sign change across the
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
         CALL mpprecv( 1, tr2ew(1-jpr2dj,1,2), imigr, noea )
         IF(l_isend) CALL mpi_wait(ml_req1,ml_stat,ml_err)
      CASE ( 0 )
         CALL mppsend( 1, tr2ew(1-jpr2dj,1,1), imigr, nowe, ml_req1 )
         CALL mppsend( 2, tr2we(1-jpr2dj,1,1), imigr, noea, ml_req2 )
         CALL mpprecv( 1, tr2ew(1-jpr2dj,1,2), imigr, noea )
         CALL mpprecv( 2, tr2we(1-jpr2dj,1,2), imigr, nowe )
         IF(l_isend) CALL mpi_wait(ml_req1,ml_stat,ml_err)
         IF(l_isend) CALL mpi_wait(ml_req2,ml_stat,ml_err)
      CASE ( 1 )
         CALL mppsend( 1, tr2ew(1-jpr2dj,1,1), imigr, nowe, ml_req1 )
         CALL mpprecv( 2, tr2we(1-jpr2dj,1,2), imigr, nowe )
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
         CALL mpprecv( 3, tr2ns(1-jpr2di,1,2), imigr, nono )
         IF(l_isend) CALL mpi_wait(ml_req1,ml_stat,ml_err)
      CASE ( 0 )
         CALL mppsend( 3, tr2ns(1-jpr2di,1,1), imigr, noso, ml_req1 )
         CALL mppsend( 4, tr2sn(1-jpr2di,1,1), imigr, nono, ml_req2 )
         CALL mpprecv( 3, tr2ns(1-jpr2di,1,2), imigr, nono )
         CALL mpprecv( 4, tr2sn(1-jpr2di,1,2), imigr, noso )
         IF(l_isend) CALL mpi_wait(ml_req1,ml_stat,ml_err)
         IF(l_isend) CALL mpi_wait(ml_req2,ml_stat,ml_err)
      CASE ( 1 )
         CALL mppsend( 3, tr2ns(1-jpr2di,1,1), imigr, noso, ml_req1 )
         CALL mpprecv( 4, tr2sn(1-jpr2di,1,2), imigr, noso )
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
      REAL(wp), INTENT(inout) ::   pmess(*)   ! array of real
      INTEGER , INTENT(in   ) ::   kbytes     ! size of the array pmess
      INTEGER , INTENT(in   ) ::   kdest      ! receive process number
      INTEGER , INTENT(in   ) ::   ktyp       ! tag of the message
      INTEGER , INTENT(in   ) ::   md_req     ! argument for isend
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


   SUBROUTINE mpprecv( ktyp, pmess, kbytes, ksource )
      !!----------------------------------------------------------------------
      !!                  ***  routine mpprecv  ***
      !!
      !! ** Purpose :   Receive messag passing array
      !!
      !!----------------------------------------------------------------------
      REAL(wp), INTENT(inout) ::   pmess(*)   ! array of real
      INTEGER , INTENT(in   ) ::   kbytes     ! suze of the array pmess
      INTEGER , INTENT(in   ) ::   ktyp       ! Tag of the recevied message
      INTEGER, OPTIONAL, INTENT(in) :: ksource    ! source process number
      !!
      INTEGER :: istatus(mpi_status_size)
      INTEGER :: iflag
      INTEGER :: use_source
      !!----------------------------------------------------------------------
      !

      ! If a specific process number has been passed to the receive call,
      ! use that one. Default is to use mpi_any_source
      use_source=mpi_any_source
      if(present(ksource)) then
         use_source=ksource
      end if

      CALL mpi_recv( pmess, kbytes, mpi_double_precision, use_source, ktyp, mpi_comm_opa, istatus, iflag )
      !
   END SUBROUTINE mpprecv


   SUBROUTINE mppgather( ptab, kp, pio )
      !!----------------------------------------------------------------------
      !!                   ***  routine mppgather  ***
      !!
      !! ** Purpose :   Transfert between a local subdomain array and a work
      !!     array which is distributed following the vertical level.
      !!
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj),       INTENT(in   ) ::   ptab   ! subdomain input array
      INTEGER ,                           INTENT(in   ) ::   kp     ! record length
      REAL(wp), DIMENSION(jpi,jpj,jpnij), INTENT(  out) ::   pio    ! subdomain input array
      !!
      INTEGER :: itaille, ierror   ! temporary integer
      !!---------------------------------------------------------------------
      !
      itaille = jpi * jpj
      CALL mpi_gather( ptab, itaille, mpi_double_precision, pio, itaille     ,   &
         &                            mpi_double_precision, kp , mpi_comm_opa, ierror )
      !
   END SUBROUTINE mppgather


   SUBROUTINE mppscatter( pio, kp, ptab )
      !!----------------------------------------------------------------------
      !!                  ***  routine mppscatter  ***
      !!
      !! ** Purpose :   Transfert between awork array which is distributed
      !!      following the vertical level and the local subdomain array.
      !!
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj,jpnij)  ::  pio        ! output array
      INTEGER                             ::   kp        ! Tag (not used with MPI
      REAL(wp), DIMENSION(jpi,jpj)        ::  ptab       ! subdomain array input
      !!
      INTEGER :: itaille, ierror   ! temporary integer
      !!---------------------------------------------------------------------
      !
      itaille=jpi*jpj
      !
      CALL mpi_scatter( pio, itaille, mpi_double_precision, ptab, itaille     ,   &
         &                            mpi_double_precision, kp  , mpi_comm_opa, ierror )
      !
   END SUBROUTINE mppscatter
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
#if !defined in_arpege
!$OMP SINGLE
      CALL mpi_allreduce( ktab, iwork, kdim, mpi_integer, mpi_max, localcomm, ierror )
!$OMP END SINGLE
      !
      ktab(:) = iwork(:)
#else
      CALL abor1_sfx("lib_mpp:mmpmax_a_int : Cannot yet sum a real array in Arpege")
#endif
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
#ifdef in_arpege
      INTEGER(KIND=JPIB)       :: jb
#endif
      !!----------------------------------------------------------------------
      !
#if !defined in_surfex || defined SFX_MPI
      localcomm = mpi_comm_opa
      IF( PRESENT(kcom) )   localcomm = kcom
      !
#if !defined in_arpege
!$OMP SINGLE
      CALL mpi_allreduce( ktab, iwork, 1, mpi_integer, mpi_max, localcomm, ierror)
!$OMP END SINGLE
      !
      ktab = iwork
#else
      jb=ktab
! Commented out for now - does not work !
!      CALL MPL_ALLREDUCE(jb,'MAX')
      ktab=jb
#endif
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
#if !defined in_arpege
!$OMP SINGLE
      CALL mpi_allreduce( ktab, iwork, kdim, mpi_integer, mpi_min, localcomm, ierror )
!$OMP END SINGLE
      !
      ktab(:) = iwork(:)
#else
      CALL abor1_sfx("lib_mpp:mmpmin_a_int : Cannot yet min a real array in Arpege")
#endif
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
#ifdef in_arpege
      INTEGER(KIND=JPIB)       :: jb
#endif
      !!----------------------------------------------------------------------
      !
#if !defined in_surfex || defined SFX_MPI
      localcomm = mpi_comm_opa
      IF( PRESENT(kcom) )   localcomm = kcom
      !
#if !defined in_arpege
!$OMP SINGLE
      CALL mpi_allreduce( ktab, iwork, 1, mpi_integer, mpi_min, localcomm, ierror )
!$OMP END SINGLE
      !
      ktab = iwork
#else
      jb=ktab
! Commented out for now - does not work !
!      CALL MPL_ALLREDUCE(jb,'MIN')
      ktab=jb
#endif
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
#if !defined in_arpege
!$OMP SINGLE
      CALL mpi_allreduce( ktab, iwork, kdim, mpi_integer, mpi_sum, mpi_comm_opa, ierror )
!$OMP END SINGLE
      !
      ktab(:) = iwork(:)
#else
      CALL abor1_sfx("lib_mpp:mmpsum_a_int : Cannot yet sum a real array in Arpege")
#endif
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
#ifdef in_arpege
      INTEGER(KIND=JPIB)       :: jb
#endif
      !!----------------------------------------------------------------------
      !
#if !defined in_surfex || defined SFX_MPI
#if !defined in_arpege
!$OMP SINGLE
      CALL mpi_allreduce( ktab, iwork, 1, mpi_integer, mpi_sum, mpi_comm_opa, ierror)
!$OMP END SINGLE
      !
      ktab = iwork
#else
      jb=ktab
! Commented out for now - does not work !
!      CALL MPL_ALLREDUCE(jb,'SUM')
      ktab=jb
#endif
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
      REAL(wp), INTENT(inout), DIMENSION(kdim) ::   ptab
      INTEGER , INTENT(in   ), OPTIONAL        ::   kcom
      !!
      INTEGER :: ierror, localcomm
      REAL(wp), DIMENSION(kdim) ::  zwork
      !!----------------------------------------------------------------------
      !
#if !defined in_surfex || defined SFX_MPI
      localcomm = mpi_comm_opa
      IF( PRESENT(kcom) ) localcomm = kcom
      !
#if !defined in_arpege
!$OMP SINGLE
      CALL mpi_allreduce( ptab, zwork, kdim, mpi_double_precision, mpi_max, localcomm, ierror )
!$OMP END SINGLE
      !
      ptab(:) = zwork(:)
#else
      CALL abor1_sfx("lib_mpp:mmpmax_a_real : Cannot yet make a max on a real array in Arpege")
#endif
#endif
   END SUBROUTINE mppmax_a_real


   SUBROUTINE mppmax_real( ptab, kcom )
      !!----------------------------------------------------------------------
      !!                  ***  routine mppmax_real  ***
      !!
      !! ** Purpose :   Maximum
      !!
      !!----------------------------------------------------------------------
      REAL(wp), INTENT(inout)           ::   ptab   ! ???
      INTEGER , INTENT(in   ), OPTIONAL ::   kcom   ! ???
      !!
      INTEGER  ::   ierror, localcomm
      REAL(wp) ::   zwork
#ifdef in_arpege
      REAL(KIND=JPRB)       :: rb
#endif
      !!----------------------------------------------------------------------
      !
#if !defined in_surfex || defined SFX_MPI
      localcomm = mpi_comm_opa
      IF( PRESENT(kcom) )   localcomm = kcom
      !
#if !defined in_arpege
!$OMP SINGLE
      CALL mpi_allreduce( ptab, zwork, 1, mpi_double_precision, mpi_max, localcomm, ierror )
!$OMP END SINGLE
      !
      ptab = zwork
#else
      rb=ptab
! Commented out for now - does not work !
!      CALL MPL_ALLREDUCE(rb,'MAX',LDREPROD=.TRUE.)
      ptab=rb
#endif
#endif
   END SUBROUTINE mppmax_real


   SUBROUTINE mppmin_a_real( ptab, kdim, kcom )
      !!----------------------------------------------------------------------
      !!                 ***  routine mppmin_a_real  ***
      !!
      !! ** Purpose :   Minimum of REAL, array case
      !!
      !!-----------------------------------------------------------------------
      INTEGER , INTENT(in   )                  ::   kdim
      REAL(wp), INTENT(inout), DIMENSION(kdim) ::   ptab
      INTEGER , INTENT(in   ), OPTIONAL        ::   kcom
      !!
      INTEGER :: ierror, localcomm
      REAL(wp), DIMENSION(kdim) ::   zwork
      !!-----------------------------------------------------------------------
      !
#if !defined in_surfex || defined SFX_MPI
      localcomm = mpi_comm_opa
      IF( PRESENT(kcom) ) localcomm = kcom
      !
#if !defined in_arpege
!$OMP SINGLE
      CALL mpi_allreduce( ptab, zwork, kdim, mpi_double_precision, mpi_min, localcomm, ierror )
!$OMP END SINGLE
      !
      ptab(:) = zwork(:)
#else
      CALL abor1_sfx("lib_mpp:mmpmin_a_real : Cannot yet make a min on a real array in Arpege")
#endif
#endif
   END SUBROUTINE mppmin_a_real


   SUBROUTINE mppmin_real( ptab, kcom )
      !!----------------------------------------------------------------------
      !!                  ***  routine mppmin_real  ***
      !!
      !! ** Purpose :   minimum of REAL, scalar case
      !!
      !!-----------------------------------------------------------------------
      REAL(wp), INTENT(inout)           ::   ptab        !
      INTEGER , INTENT(in   ), OPTIONAL :: kcom
      !!
      INTEGER  ::   ierror
      REAL(wp) ::   zwork
      INTEGER :: localcomm
#ifdef in_arpege
      REAL(KIND=JPRB)       :: rb
#endif
      !!-----------------------------------------------------------------------
      !
#if !defined in_surfex || defined SFX_MPI
      localcomm = mpi_comm_opa
      IF( PRESENT(kcom) )   localcomm = kcom
      !
#if !defined in_arpege
!$OMP SINGLE
      CALL mpi_allreduce( ptab, zwork, 1, mpi_double_precision, mpi_min, localcomm, ierror )
!$OMP END SINGLE
      !
      ptab = zwork
#else
      rb=ptab
! Commented out for now - does not work !
!      CALL MPL_ALLREDUCE(rb,'MIN',LDREPROD=.TRUE.)
      ptab=rb
#endif
#endif
   END SUBROUTINE mppmin_real


   SUBROUTINE mppsum_a_real( ptab, kdim, kcom )
      !!----------------------------------------------------------------------
      !!                  ***  routine mppsum_a_real  ***
      !!
      !! ** Purpose :   global sum, REAL ARRAY argument case
      !!
      !!-----------------------------------------------------------------------
      INTEGER , INTENT( in )                     ::   kdim      ! size of ptab
      REAL(wp), DIMENSION(kdim), INTENT( inout ) ::   ptab      ! input array
      INTEGER , INTENT( in ), OPTIONAL           :: kcom
      !!
      INTEGER                   ::   ierror    ! temporary integer
      INTEGER                   ::   localcomm
      REAL(wp), DIMENSION(kdim) ::   zwork     ! temporary workspace
      !!-----------------------------------------------------------------------
      !
#if !defined in_surfex || defined SFX_MPI
      localcomm = mpi_comm_opa
      IF( PRESENT(kcom) )   localcomm = kcom
      !
#if !defined in_arpege
!$OMP SINGLE
      CALL mpi_allreduce( ptab, zwork, kdim, mpi_double_precision, mpi_sum, localcomm, ierror )
!$OMP END SINGLE
      !
      ptab(:) = zwork(:)
#else
      CALL abor1_sfx("lib_mpp:mmpsum_a_real : Cannot yet make a sum on a real array in Arpege") 
#endif
#endif
   END SUBROUTINE mppsum_a_real


   SUBROUTINE mppsum_real( ptab, kcom )
      !!----------------------------------------------------------------------
      !!                  ***  routine mppsum_real  ***
      !!
      !! ** Purpose :   global sum, SCALAR argument case
      !!
      !!-----------------------------------------------------------------------
      REAL(wp), INTENT(inout)           ::   ptab   ! input scalar
      INTEGER , INTENT(in   ), OPTIONAL ::   kcom
      !!
      INTEGER  ::   ierror, localcomm
      REAL(wp) ::   zwork
#ifdef in_arpege
      REAL(KIND=JPRB)       :: rb
#endif
      !!--------------------------------------------- -------------------------
      !
#if !defined in_surfex || defined SFX_MPI
      localcomm = mpi_comm_opa
      IF( PRESENT(kcom) ) localcomm = kcom
      !
#if !defined in_arpege
!$OMP SINGLE
      CALL mpi_allreduce( ptab, zwork, 1, mpi_double_precision, mpi_sum, localcomm, ierror )
!$OMP END SINGLE
      !
      ptab = zwork
#else
      rb=ptab
! Commented out for now - does not work !
!      CALL MPL_ALLREDUCE(rb,'SUM',LDREPROD=.TRUE.)
      ptab=rb
#endif
#endif
   END SUBROUTINE mppsum_real

#if ! defined in_surfex
   SUBROUTINE mppsum_realdd( ytab, kcom )
      !!----------------------------------------------------------------------
      !!                  ***  routine mppsum_realdd ***
      !!
      !! ** Purpose :   global sum in Massively Parallel Processing
      !!                SCALAR argument case for double-double precision
      !!
      !!-----------------------------------------------------------------------
      COMPLEX(wp), INTENT(inout)         :: ytab    ! input scalar
      INTEGER , INTENT( in  ), OPTIONAL :: kcom

      !! * Local variables   (MPI version)
      INTEGER  ::    ierror
      INTEGER  ::   localcomm
      COMPLEX(wp) :: zwork

      localcomm = mpi_comm_opa
      IF( PRESENT(kcom) ) localcomm = kcom

      ! reduce local sums into global sum
!$OMP SINGLE
      CALL MPI_ALLREDUCE (ytab, zwork, 1, MPI_DOUBLE_COMPLEX, &
                       MPI_SUMDD,localcomm,ierror)
!$OMP END SINGLE
      ytab = zwork

   END SUBROUTINE mppsum_realdd


   SUBROUTINE mppsum_a_realdd( ytab, kdim, kcom )
      !!----------------------------------------------------------------------
      !!                  ***  routine mppsum_a_realdd  ***
      !!
      !! ** Purpose :   global sum in Massively Parallel Processing
      !!                COMPLEX ARRAY case for double-double precision
      !!
      !!-----------------------------------------------------------------------
      INTEGER , INTENT( in )                        ::   kdim      ! size of ytab
      COMPLEX(wp), DIMENSION(kdim), INTENT( inout ) ::   ytab      ! input array
      INTEGER , INTENT( in  ), OPTIONAL :: kcom

      !! * Local variables   (MPI version)
      INTEGER                      :: ierror    ! temporary integer
      INTEGER                      ::   localcomm
      COMPLEX(wp), DIMENSION(kdim) :: zwork     ! temporary workspace

      localcomm = mpi_comm_opa
      IF( PRESENT(kcom) ) localcomm = kcom
!$OMP SINGLE
      CALL MPI_ALLREDUCE (ytab, zwork, kdim, MPI_DOUBLE_COMPLEX, &
                       MPI_SUMDD,localcomm,ierror)
!$OMP END SINGLE
      ytab(:) = zwork(:)

   END SUBROUTINE mppsum_a_realdd


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
      REAL(wp), DIMENSION (jpi,jpj), INTENT(in   ) ::   ptab    ! Local 2D array
      REAL(wp), DIMENSION (jpi,jpj), INTENT(in   ) ::   pmask   ! Local mask
      REAL(wp)                     , INTENT(  out) ::   pmin    ! Global minimum of ptab
      INTEGER                      , INTENT(  out) ::   ki, kj   ! index of minimum in global frame
      !!
      INTEGER , DIMENSION(2)   ::   ilocs
      INTEGER :: ierror
      REAL(wp) ::   zmin   ! local minimum
      REAL(wp), DIMENSION(2,1) ::   zain, zaout
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
      REAL(wp), DIMENSION (jpi,jpj,jpk), INTENT(in   ) ::   ptab         ! Local 2D array
      REAL(wp), DIMENSION (jpi,jpj,jpk), INTENT(in   ) ::   pmask        ! Local mask
      REAL(wp)                         , INTENT(  out) ::   pmin         ! Global minimum of ptab
      INTEGER                          , INTENT(  out) ::   ki, kj, kk   ! index of minimum in global frame
      !!
      INTEGER  ::   ierror
      REAL(wp) ::   zmin     ! local minimum
      INTEGER , DIMENSION(3)   ::   ilocs
      REAL(wp), DIMENSION(2,1) ::   zain, zaout
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
      REAL(wp), DIMENSION (jpi,jpj), INTENT(in   ) ::   ptab     ! Local 2D array
      REAL(wp), DIMENSION (jpi,jpj), INTENT(in   ) ::   pmask    ! Local mask
      REAL(wp)                     , INTENT(  out) ::   pmax     ! Global maximum of ptab
      INTEGER                      , INTENT(  out) ::   ki, kj   ! index of maximum in global frame
      !!
      INTEGER  :: ierror
      INTEGER, DIMENSION (2)   ::   ilocs
      REAL(wp) :: zmax   ! local maximum
      REAL(wp), DIMENSION(2,1) ::   zain, zaout
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
      REAL(wp), DIMENSION (jpi,jpj,jpk), INTENT(in   ) ::   ptab         ! Local 2D array
      REAL(wp), DIMENSION (jpi,jpj,jpk), INTENT(in   ) ::   pmask        ! Local mask
      REAL(wp)                         , INTENT(  out) ::   pmax         ! Global maximum of ptab
      INTEGER                          , INTENT(  out) ::   ki, kj, kk   ! index of maximum in global frame
      !!
      REAL(wp) :: zmax   ! local maximum
      REAL(wp), DIMENSION(2,1) ::   zain, zaout
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


   SUBROUTINE mppsync()
      !!----------------------------------------------------------------------
      !!                  ***  routine mppsync  ***
      !!
      !! ** Purpose :   Massively parallel processors, synchroneous
      !!
      !!-----------------------------------------------------------------------
      INTEGER :: ierror
      !!-----------------------------------------------------------------------
      !
      CALL mpi_barrier( mpi_comm_opa, ierror )
      !
   END SUBROUTINE mppsync


   SUBROUTINE mppstop
      !!----------------------------------------------------------------------
      !!                  ***  routine mppstop  ***
      !!
      !! ** purpose :   Stop massively parallel processors method
      !!
      !!----------------------------------------------------------------------
      INTEGER ::   info
      !!----------------------------------------------------------------------
      !
      CALL mppsync
      CALL mpi_finalize( info )
      !
   END SUBROUTINE mppstop


   SUBROUTINE mppobc( ptab, kd1, kd2, kl, kk, ktype, kij , kumout)
      !!----------------------------------------------------------------------
      !!                  ***  routine mppobc  ***
      !!
      !! ** Purpose :   Message passing manadgement for open boundary
      !!     conditions array
      !!
      !! ** Method  :   Use mppsend and mpprecv function for passing mask
      !!       between processors following neighboring subdomains.
      !!       domain parameters
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
      USE wrk_nemo        ! Memory allocation
      !
      INTEGER , INTENT(in   )                     ::   kd1, kd2   ! starting and ending indices
      INTEGER , INTENT(in   )                     ::   kl         ! index of open boundary
      INTEGER , INTENT(in   )                     ::   kk         ! vertical dimension
      INTEGER , INTENT(in   )                     ::   ktype      ! define north/south or east/west cdt
      !                                                           !  = 1  north/south  ;  = 2  east/west
      INTEGER , INTENT(in   )                     ::   kij        ! horizontal dimension
      INTEGER , INTENT(in   )                     ::   kumout     ! ocean.output logical unit
      REAL(wp), INTENT(inout), DIMENSION(kij,kk)  ::   ptab       ! variable array
      !
      INTEGER ::   ji, jj, jk, jl        ! dummy loop indices
      INTEGER ::   iipt0, iipt1, ilpt1   ! local integers
      INTEGER ::   ijpt0, ijpt1          !   -       -
      INTEGER ::   imigr, iihom, ijhom   !   -       -
      INTEGER ::   ml_req1, ml_req2, ml_err    ! for key_mpi_isend
      INTEGER ::   ml_stat(MPI_STATUS_SIZE)    ! for key_mpi_isend
      REAL(wp), POINTER, DIMENSION(:,:) ::   ztab   ! temporary workspace
      !!----------------------------------------------------------------------

      CALL wrk_alloc( jpi,jpj, ztab )

      ! boundary condition initialization
      ! ---------------------------------
      ztab(:,:) = 0.e0
      !
      IF( ktype==1 ) THEN                                  ! north/south boundaries
         iipt0 = MAX( 1, MIN(kd1 - nimpp+1, nlci     ) )
         iipt1 = MAX( 0, MIN(kd2 - nimpp+1, nlci - 1 ) )
         ilpt1 = MAX( 1, MIN(kd2 - nimpp+1, nlci     ) )
         ijpt0 = MAX( 1, MIN(kl  - njmpp+1, nlcj     ) )
         ijpt1 = MAX( 0, MIN(kl  - njmpp+1, nlcj - 1 ) )
      ELSEIF( ktype==2 ) THEN                              ! east/west boundaries
         iipt0 = MAX( 1, MIN(kl  - nimpp+1, nlci     ) )
         iipt1 = MAX( 0, MIN(kl  - nimpp+1, nlci - 1 ) )
         ijpt0 = MAX( 1, MIN(kd1 - njmpp+1, nlcj     ) )
         ijpt1 = MAX( 0, MIN(kd2 - njmpp+1, nlcj - 1 ) )
         ilpt1 = MAX( 1, MIN(kd2 - njmpp+1, nlcj     ) )
      ELSE
         WRITE(kumout, cform_err)
         WRITE(kumout,*) 'mppobc : bad ktype'
         CALL mppstop
      ENDIF

      ! Communication level by level
      ! ----------------------------
!!gm Remark : this is very time consumming!!!
      !                                         ! ------------------------ !
      DO jk = 1, kk                             !   Loop over the levels   !
         !                                      ! ------------------------ !
         !
         IF( ktype == 1 ) THEN                               ! north/south boundaries
            DO jj = ijpt0, ijpt1
               DO ji = iipt0, iipt1
                  ztab(ji,jj) = ptab(ji,jk)
               END DO
            END DO
         ELSEIF( ktype == 2 ) THEN                           ! east/west boundaries
            DO jj = ijpt0, ijpt1
               DO ji = iipt0, iipt1
                  ztab(ji,jj) = ptab(jj,jk)
               END DO
            END DO
         ENDIF


         ! 1. East and west directions
         ! ---------------------------
         !
         IF( nbondi /= 2 ) THEN         ! Read Dirichlet lateral conditions
            iihom = nlci-nreci
            DO jl = 1, jpreci
               t2ew(:,jl,1) = ztab(jpreci+jl,:)
               t2we(:,jl,1) = ztab(iihom +jl,:)
            END DO
         ENDIF
         !
         !                              ! Migrations
         imigr=jpreci*jpj
         !
         IF( nbondi == -1 ) THEN
            CALL mppsend( 2, t2we(1,1,1), imigr, noea, ml_req1 )
            CALL mpprecv( 1, t2ew(1,1,2), imigr, noea )
            IF(l_isend)   CALL mpi_wait( ml_req1, ml_stat, ml_err )
         ELSEIF( nbondi == 0 ) THEN
            CALL mppsend( 1, t2ew(1,1,1), imigr, nowe, ml_req1 )
            CALL mppsend( 2, t2we(1,1,1), imigr, noea, ml_req2 )
            CALL mpprecv( 1, t2ew(1,1,2), imigr, noea )
            CALL mpprecv( 2, t2we(1,1,2), imigr, nowe )
            IF(l_isend)   CALL mpi_wait( ml_req1, ml_stat, ml_err )
            IF(l_isend)   CALL mpi_wait( ml_req2, ml_stat, ml_err )
         ELSEIF( nbondi == 1 ) THEN
            CALL mppsend( 1, t2ew(1,1,1), imigr, nowe, ml_req1 )
            CALL mpprecv( 2, t2we(1,1,2), imigr, nowe )
            IF(l_isend) CALL mpi_wait( ml_req1, ml_stat, ml_err )
         ENDIF
         !
         !                              ! Write Dirichlet lateral conditions
         iihom = nlci-jpreci
         !
         IF( nbondi == 0 .OR. nbondi == 1 ) THEN
            DO jl = 1, jpreci
               ztab(jl,:) = t2we(:,jl,2)
            END DO
         ENDIF
         IF( nbondi == -1 .OR. nbondi == 0 ) THEN
            DO jl = 1, jpreci
               ztab(iihom+jl,:) = t2ew(:,jl,2)
            END DO
         ENDIF


         ! 2. North and south directions
         ! -----------------------------
         !
         IF( nbondj /= 2 ) THEN         ! Read Dirichlet lateral conditions
            ijhom = nlcj-nrecj
            DO jl = 1, jprecj
               t2sn(:,jl,1) = ztab(:,ijhom +jl)
               t2ns(:,jl,1) = ztab(:,jprecj+jl)
            END DO
         ENDIF
         !
         !                              ! Migrations
         imigr = jprecj * jpi
         !
         IF( nbondj == -1 ) THEN
            CALL mppsend( 4, t2sn(1,1,1), imigr, nono, ml_req1 )
            CALL mpprecv( 3, t2ns(1,1,2), imigr, nono )
            IF(l_isend) CALL mpi_wait( ml_req1, ml_stat, ml_err )
         ELSEIF( nbondj == 0 ) THEN
            CALL mppsend( 3, t2ns(1,1,1), imigr, noso, ml_req1 )
            CALL mppsend( 4, t2sn(1,1,1), imigr, nono, ml_req2 )
            CALL mpprecv( 3, t2ns(1,1,2), imigr, nono )
            CALL mpprecv( 4, t2sn(1,1,2), imigr, noso )
            IF( l_isend )   CALL mpi_wait( ml_req1, ml_stat, ml_err )
            IF( l_isend )   CALL mpi_wait( ml_req2, ml_stat, ml_err )
         ELSEIF( nbondj == 1 ) THEN
            CALL mppsend( 3, t2ns(1,1,1), imigr, noso, ml_req1 )
            CALL mpprecv( 4, t2sn(1,1,2), imigr, noso)
            IF( l_isend )   CALL mpi_wait( ml_req1, ml_stat, ml_err )
         ENDIF
         !
         !                              ! Write Dirichlet lateral conditions
         ijhom = nlcj - jprecj
         IF( nbondj == 0 .OR. nbondj == 1 ) THEN
            DO jl = 1, jprecj
               ztab(:,jl) = t2sn(:,jl,2)
            END DO
         ENDIF
         IF( nbondj == 0 .OR. nbondj == -1 ) THEN
            DO jl = 1, jprecj
               ztab(:,ijhom+jl) = t2ns(:,jl,2)
            END DO
         ENDIF
         IF( ktype==1 .AND. kd1 <= jpi+nimpp-1 .AND. nimpp <= kd2 ) THEN
            DO jj = ijpt0, ijpt1            ! north/south boundaries
               DO ji = iipt0,ilpt1
                  ptab(ji,jk) = ztab(ji,jj)
               END DO
            END DO
         ELSEIF( ktype==2 .AND. kd1 <= jpj+njmpp-1 .AND. njmpp <= kd2 ) THEN
            DO jj = ijpt0, ilpt1            ! east/west boundaries
               DO ji = iipt0,iipt1
                  ptab(jj,jk) = ztab(ji,jj)
               END DO
            END DO
         ENDIF
         !
      END DO
      !
      CALL wrk_dealloc( jpi,jpj, ztab )
      !
   END SUBROUTINE mppobc


!   SUBROUTINE mpp_comm_free( kcom )
!      !!----------------------------------------------------------------------
!      !!----------------------------------------------------------------------
!      INTEGER, INTENT(in) ::   kcom
!      !!
!      INTEGER :: ierr
!      !!----------------------------------------------------------------------
!      !
!      CALL MPI_COMM_FREE(kcom, ierr)
!      !
!   END SUBROUTINE mpp_comm_free


   SUBROUTINE mpp_ini_ice( pindic, kumout )
      !!----------------------------------------------------------------------
      !!               ***  routine mpp_ini_ice  ***
      !!
      !! ** Purpose :   Initialize special communicator for ice areas
      !!      condition together with global variables needed in the ddmpp folding
      !!
      !! ** Method  : - Look for ice processors in ice routines
      !!              - Put their number in nrank_ice
      !!              - Create groups for the world processors and the ice processors
      !!              - Create a communicator for ice processors
      !!
      !! ** output
      !!      njmppmax = njmpp for northern procs
      !!      ndim_rank_ice = number of processors with ice
      !!      nrank_ice (ndim_rank_ice) = ice processors
      !!      ngrp_iworld = group ID for the world processors
      !!      ngrp_ice = group ID for the ice processors
      !!      ncomm_ice = communicator for the ice procs.
      !!      n_ice_root = number (in the world) of proc 0 in the ice comm.
      !!
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   pindic
      INTEGER, INTENT(in) ::   kumout   ! ocean.output logical unit
      !!
      INTEGER :: jjproc
      INTEGER :: ii, ierr
      INTEGER, ALLOCATABLE, DIMENSION(:) ::   kice
      INTEGER, ALLOCATABLE, DIMENSION(:) ::   zwork
      !!----------------------------------------------------------------------
      !
      ! Since this is just an init routine and these arrays are of length jpnij
      ! then don't use wrk_nemo module - just allocate and deallocate.
      ALLOCATE( kice(jpnij), zwork(jpnij), STAT=ierr )
      IF( ierr /= 0 ) THEN
         WRITE(kumout, cform_err)
         WRITE(kumout,*) 'mpp_ini_ice : failed to allocate 2, 1D arrays (jpnij in length)'
         CALL mppstop
      ENDIF

      ! Look for how many procs with sea-ice
      !
      kice = 0
      DO jjproc = 1, jpnij
         IF( jjproc == narea .AND. pindic .GT. 0 )   kice(jjproc) = 1
      END DO
      !
      zwork = 0
      CALL MPI_ALLREDUCE( kice, zwork, jpnij, mpi_integer, mpi_sum, mpi_comm_opa, ierr )
      ndim_rank_ice = SUM( zwork )

      ! Allocate the right size to nrank_north
      IF( ALLOCATED ( nrank_ice ) )   DEALLOCATE( nrank_ice )
      ALLOCATE( nrank_ice(ndim_rank_ice) )
      !
      ii = 0
      nrank_ice = 0
      DO jjproc = 1, jpnij
         IF( zwork(jjproc) == 1) THEN
            ii = ii + 1
            nrank_ice(ii) = jjproc -1
         ENDIF
      END DO

      ! Create the world group
      CALL MPI_COMM_GROUP( mpi_comm_opa, ngrp_iworld, ierr )

      ! Create the ice group from the world group
      CALL MPI_GROUP_INCL( ngrp_iworld, ndim_rank_ice, nrank_ice, ngrp_ice, ierr )

      ! Create the ice communicator , ie the pool of procs with sea-ice
      CALL MPI_COMM_CREATE( mpi_comm_opa, ngrp_ice, ncomm_ice, ierr )

      ! Find proc number in the world of proc 0 in the north
      ! The following line seems to be useless, we just comment & keep it as reminder
      ! CALL MPI_GROUP_TRANSLATE_RANKS(ngrp_ice,1,0,ngrp_iworld,n_ice_root,ierr)
      !
      CALL MPI_GROUP_FREE(ngrp_ice, ierr)
      CALL MPI_GROUP_FREE(ngrp_iworld, ierr)

      DEALLOCATE(kice, zwork)
      !
   END SUBROUTINE mpp_ini_ice


   SUBROUTINE mpp_ini_znl( kumout )
      !!----------------------------------------------------------------------
      !!               ***  routine mpp_ini_znl  ***
      !!
      !! ** Purpose :   Initialize special communicator for computing zonal sum
      !!
      !! ** Method  : - Look for processors in the same row
      !!              - Put their number in nrank_znl
      !!              - Create group for the znl processors
      !!              - Create a communicator for znl processors
      !!              - Determine if processor should write znl files
      !!
      !! ** output
      !!      ndim_rank_znl = number of processors on the same row
      !!      ngrp_znl = group ID for the znl processors
      !!      ncomm_znl = communicator for the ice procs.
      !!      n_znl_root = number (in the world) of proc 0 in the ice comm.
      !!
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kumout   ! ocean.output logical units
      !
      INTEGER :: jproc      ! dummy loop integer
      INTEGER :: ierr, ii   ! local integer
      INTEGER, ALLOCATABLE, DIMENSION(:) ::   kwork
      !!----------------------------------------------------------------------
      !-$$     WRITE (numout,*) 'mpp_ini_znl ', nproc, ' - ngrp_world     : ', ngrp_world
      !-$$     WRITE (numout,*) 'mpp_ini_znl ', nproc, ' - mpi_comm_world : ', mpi_comm_world
      !-$$     WRITE (numout,*) 'mpp_ini_znl ', nproc, ' - mpi_comm_opa   : ', mpi_comm_opa
      !
      ALLOCATE( kwork(jpnij), STAT=ierr )
      IF( ierr /= 0 ) THEN
         WRITE(kumout, cform_err)
         WRITE(kumout,*) 'mpp_ini_znl : failed to allocate 1D array of length jpnij'
         CALL mppstop
      ENDIF

      IF( jpnj == 1 ) THEN
         ngrp_znl  = ngrp_world
         ncomm_znl = mpi_comm_opa
      ELSE
         !
         CALL MPI_ALLGATHER ( njmpp, 1, mpi_integer, kwork, 1, mpi_integer, mpi_comm_opa, ierr )
         !-$$        WRITE (numout,*) 'mpp_ini_znl ', nproc, ' - kwork pour njmpp : ', kwork
         !-$$        CALL flush(numout)
         !
         ! Count number of processors on the same row
         ndim_rank_znl = 0
         DO jproc=1,jpnij
            IF ( kwork(jproc) == njmpp ) THEN
               ndim_rank_znl = ndim_rank_znl + 1
            ENDIF
         END DO
         !-$$        WRITE (numout,*) 'mpp_ini_znl ', nproc, ' - ndim_rank_znl : ', ndim_rank_znl
         !-$$        CALL flush(numout)
         ! Allocate the right size to nrank_znl
         IF (ALLOCATED (nrank_znl)) DEALLOCATE(nrank_znl)
         ALLOCATE(nrank_znl(ndim_rank_znl))
         ii = 0
         nrank_znl (:) = 0
         DO jproc=1,jpnij
            IF ( kwork(jproc) == njmpp) THEN
               ii = ii + 1
               nrank_znl(ii) = jproc -1
            ENDIF
         END DO
         !-$$        WRITE (numout,*) 'mpp_ini_znl ', nproc, ' - nrank_znl : ', nrank_znl
         !-$$        CALL flush(numout)

         ! Create the opa group
         CALL MPI_COMM_GROUP(mpi_comm_opa,ngrp_opa,ierr)
         !-$$        WRITE (numout,*) 'mpp_ini_znl ', nproc, ' - ngrp_opa : ', ngrp_opa
         !-$$        CALL flush(numout)

         ! Create the znl group from the opa group
         CALL MPI_GROUP_INCL  ( ngrp_opa, ndim_rank_znl, nrank_znl, ngrp_znl, ierr )
         !-$$        WRITE (numout,*) 'mpp_ini_znl ', nproc, ' - ngrp_znl ', ngrp_znl
         !-$$        CALL flush(numout)

         ! Create the znl communicator from the opa communicator, ie the pool of procs in the same row
         CALL MPI_COMM_CREATE ( mpi_comm_opa, ngrp_znl, ncomm_znl, ierr )
         !-$$        WRITE (numout,*) 'mpp_ini_znl ', nproc, ' - ncomm_znl ', ncomm_znl
         !-$$        CALL flush(numout)
         !
      END IF

      ! Determines if processor if the first (starting from i=1) on the row
      IF ( jpni == 1 ) THEN
         l_znl_root = .TRUE.
      ELSE
         l_znl_root = .FALSE.
         kwork (1) = nimpp
         CALL mpp_min ( kwork(1), kcom = ncomm_znl)
         IF ( nimpp == kwork(1)) l_znl_root = .TRUE.
      END IF

      DEALLOCATE(kwork)

   END SUBROUTINE mpp_ini_znl


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
      !! ** output
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
      ! Note : the rank start at 0 in MPI
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
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(inout) ::   pt3d      ! 3D array on which the b.c. is applied
      CHARACTER(len=1)                , INTENT(in   ) ::   cd_type   ! nature of pt3d grid-points
      !                                                              !   = T ,  U , V , F or W  gridpoints
      REAL(wp)                        , INTENT(in   ) ::   psgn      ! = -1. the sign change across the north fold
      !!                                                             ! =  1. , the sign is kept
      INTEGER ::   ji, jj, jr
      INTEGER ::   ierr, itaille, ildi, ilei, iilb
      INTEGER ::   ijpj, ijpjm1, ij, iproc
      INTEGER, DIMENSION (jpmaxngh)          ::   ml_req_nf          ! for mpi_isend when avoiding mpi_allgather
      INTEGER                                ::   ml_err             ! for mpi_isend when avoiding mpi_allgather
      INTEGER, DIMENSION(MPI_STATUS_SIZE)    ::   ml_stat            ! for mpi_isend when avoiding mpi_allgather
      !!----------------------------------------------------------------------
      !
      ijpj   = 4
      ityp = -1
      ijpjm1 = 3
      ztab(:,:,:) = 0.e0
      !
      DO jj = nlcj - ijpj +1, nlcj          ! put in znorthloc the last 4 jlines of pt3d
         ij = jj - nlcj + ijpj
         znorthloc(:,ij,:) = pt3d(:,jj,:)
      END DO
      !
      !                                     ! Build in procs of ncomm_north the znorthgloio
      itaille = jpi * jpk * ijpj
      IF ( l_north_nogather ) THEN
         !
         ! Avoid the use of mpi_allgather by exchanging only with the processes already identified
         ! (in nemo_northcomms) as being  involved in this process' northern boundary exchange
         !
         DO jj = nlcj-ijpj+1, nlcj          ! First put local values into the global array
            ij = jj - nlcj + ijpj
            DO ji = 1, nlci
               ztab(ji+nimpp-1,ij,:) = pt3d(ji,jj,:)
            END DO
         END DO

         !
         ! Set the exchange type in order to access the correct list of active neighbours
         !
         SELECT CASE ( cd_type )
            CASE ( 'T' , 'W' )
               ityp = 1
            CASE ( 'U' )
               ityp = 2
            CASE ( 'V' )
               ityp = 3
            CASE ( 'F' )
               ityp = 4
            CASE ( 'I' )
               ityp = 5
            CASE DEFAULT
               ityp = -1                    ! Set a default value for unsupported types which
                                            ! will cause a fallback to the mpi_allgather method
         END SELECT
         IF ( ityp .gt. 0 ) THEN

            DO jr = 1,nsndto(ityp)
               CALL mppsend(5, znorthloc, itaille, isendto(jr,ityp), ml_req_nf(jr) )
            END DO
            DO jr = 1,nsndto(ityp)
               CALL mpprecv(5, zfoldwk, itaille, isendto(jr,ityp))
               iproc = isendto(jr,ityp) + 1
               ildi = nldit (iproc)
               ilei = nleit (iproc)
               iilb = nimppt(iproc)
               DO jj = 1, ijpj
                  DO ji = ildi, ilei
                     ztab(ji+iilb-1,jj,:) = zfoldwk(ji,jj,:)
                  END DO
               END DO
            END DO
            IF (l_isend) THEN
               DO jr = 1,nsndto(ityp)
                  CALL mpi_wait(ml_req_nf(jr), ml_stat, ml_err)
               END DO
            ENDIF

         ENDIF

      ENDIF

      IF ( ityp .lt. 0 ) THEN
         CALL MPI_ALLGATHER( znorthloc  , itaille, MPI_DOUBLE_PRECISION,                &
            &                znorthgloio, itaille, MPI_DOUBLE_PRECISION, ncomm_north, ierr )
         !
         DO jr = 1, ndim_rank_north         ! recover the global north array
            iproc = nrank_north(jr) + 1
            ildi  = nldit (iproc)
            ilei  = nleit (iproc)
            iilb  = nimppt(iproc)
            DO jj = 1, ijpj
               DO ji = ildi, ilei
                  ztab(ji+iilb-1,jj,:) = znorthgloio(ji,jj,:,jr)
               END DO
            END DO
         END DO
      ENDIF
      !
      ! The ztab array has been either:
      !  a. Fully populated by the mpi_allgather operation or
      !  b. Had the active points for this domain and northern neighbours populated
      !     by peer to peer exchanges
      ! Either way the array may be folded by lbc_nfd and the result for the span of
      ! this domain will be identical.
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
      REAL(wp), DIMENSION(jpi,jpj), INTENT(inout) ::   pt2d      ! 3D array on which the b.c. is applied
      CHARACTER(len=1)            , INTENT(in   ) ::   cd_type   ! nature of pt3d grid-points
      !                                                          !   = T ,  U , V , F or W  gridpoints
      REAL(wp)                    , INTENT(in   ) ::   psgn      ! = -1. the sign change across the north fold
      !!                                                             ! =  1. , the sign is kept
      INTEGER ::   ji, jj, jr
      INTEGER ::   ierr, itaille, ildi, ilei, iilb
      INTEGER ::   ijpj, ijpjm1, ij, iproc
      INTEGER, DIMENSION (jpmaxngh)      ::   ml_req_nf          ! for mpi_isend when avoiding mpi_allgather
      INTEGER                            ::   ml_err             ! for mpi_isend when avoiding mpi_allgather
      INTEGER, DIMENSION(MPI_STATUS_SIZE)::   ml_stat            ! for mpi_isend when avoiding mpi_allgather
      !!----------------------------------------------------------------------
      !
      ijpj   = 4
      ityp = -1
      ijpjm1 = 3
      ztab_2d(:,:) = 0.e0
      !
      DO jj = nlcj-ijpj+1, nlcj             ! put in znorthloc_2d the last 4 jlines of pt2d
         ij = jj - nlcj + ijpj
         znorthloc_2d(:,ij) = pt2d(:,jj)
      END DO

      !                                     ! Build in procs of ncomm_north the znorthgloio_2d
      itaille = jpi * ijpj
      IF ( l_north_nogather ) THEN
         !
         ! Avoid the use of mpi_allgather by exchanging only with the processes already identified
         ! (in nemo_northcomms) as being  involved in this process' northern boundary exchange
         !
         DO jj = nlcj-ijpj+1, nlcj          ! First put local values into the global array
            ij = jj - nlcj + ijpj
            DO ji = 1, nlci
               ztab_2d(ji+nimpp-1,ij) = pt2d(ji,jj)
            END DO
         END DO

         !
         ! Set the exchange type in order to access the correct list of active neighbours
         !
         SELECT CASE ( cd_type )
            CASE ( 'T' , 'W' )
               ityp = 1
            CASE ( 'U' )
               ityp = 2
            CASE ( 'V' )
               ityp = 3
            CASE ( 'F' )
               ityp = 4
            CASE ( 'I' )
               ityp = 5
            CASE DEFAULT
               ityp = -1                    ! Set a default value for unsupported types which
                                            ! will cause a fallback to the mpi_allgather method
         END SELECT

         IF ( ityp .gt. 0 ) THEN

            DO jr = 1,nsndto(ityp)
               CALL mppsend(5, znorthloc_2d, itaille, isendto(jr,ityp), ml_req_nf(jr) )
            END DO
            DO jr = 1,nsndto(ityp)
               CALL mpprecv(5, zfoldwk_2d, itaille, isendto(jr,ityp))
               iproc = isendto(jr,ityp) + 1
               ildi = nldit (iproc)
               ilei = nleit (iproc)
               iilb = nimppt(iproc)
               DO jj = 1, ijpj
                  DO ji = ildi, ilei
                     ztab_2d(ji+iilb-1,jj) = zfoldwk_2d(ji,jj)
                  END DO
               END DO
            END DO
            IF (l_isend) THEN
               DO jr = 1,nsndto(ityp)
                  CALL mpi_wait(ml_req_nf(jr), ml_stat, ml_err)
               END DO
            ENDIF

         ENDIF

      ENDIF

      IF ( ityp .lt. 0 ) THEN
         CALL MPI_ALLGATHER( znorthloc_2d  , itaille, MPI_DOUBLE_PRECISION,        &
            &                znorthgloio_2d, itaille, MPI_DOUBLE_PRECISION, ncomm_north, ierr )
         !
         DO jr = 1, ndim_rank_north            ! recover the global north array
            iproc = nrank_north(jr) + 1
            ildi = nldit (iproc)
            ilei = nleit (iproc)
            iilb = nimppt(iproc)
            DO jj = 1, ijpj
               DO ji = ildi, ilei
                  ztab_2d(ji+iilb-1,jj) = znorthgloio_2d(ji,jj,jr)
               END DO
            END DO
         END DO
      ENDIF
      !
      ! The ztab array has been either:
      !  a. Fully populated by the mpi_allgather operation or
      !  b. Had the active points for this domain and northern neighbours populated
      !     by peer to peer exchanges
      ! Either way the array may be folded by lbc_nfd and the result for the span of
      ! this domain will be identical.
      !
      CALL lbc_nfd( ztab_2d, cd_type, psgn )   ! North fold boundary condition
      !
      !
      DO jj = nlcj-ijpj+1, nlcj             ! Scatter back to pt2d
         ij = jj - nlcj + ijpj
         DO ji = 1, nlci
            pt2d(ji,jj) = ztab_2d(ji+nimpp-1,ij)
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
      REAL(wp), DIMENSION(1-jpr2di:jpi+jpr2di,1-jpr2dj:jpj+jpr2dj), INTENT(inout) ::   pt2d     ! 2D array with extra halo
      CHARACTER(len=1)                                            , INTENT(in   ) ::   cd_type  ! nature of pt3d grid-points
      !                                                                                         !   = T ,  U , V , F or W -points
      REAL(wp)                                                    , INTENT(in   ) ::   psgn     ! = -1. the sign change across the
      !!                                                                                        ! north fold, =  1. otherwise
      INTEGER ::   ji, jj, jr
      INTEGER ::   ierr, itaille, ildi, ilei, iilb
      INTEGER ::   ijpj, ij, iproc
      !!----------------------------------------------------------------------
      !
      ijpj=4
      ztab_e(:,:) = 0.e0

      ij=0
      ! put in znorthloc_e the last 4 jlines of pt2d
      DO jj = nlcj - ijpj + 1 - jpr2dj, nlcj +jpr2dj
         ij = ij + 1
         DO ji = 1, jpi
            znorthloc_e(ji,ij)=pt2d(ji,jj)
         END DO
      END DO
      !
      itaille = jpi * ( ijpj + 2 * jpr2dj )
      CALL MPI_ALLGATHER( znorthloc_e(1,1)  , itaille, MPI_DOUBLE_PRECISION,    &
         &                znorthgloio_e(1,1,1), itaille, MPI_DOUBLE_PRECISION, ncomm_north, ierr )
      !
      DO jr = 1, ndim_rank_north            ! recover the global north array
         iproc = nrank_north(jr) + 1
         ildi = nldit (iproc)
         ilei = nleit (iproc)
         iilb = nimppt(iproc)
         DO jj = 1, ijpj+2*jpr2dj
            DO ji = ildi, ilei
               ztab_e(ji+iilb-1,jj) = znorthgloio_e(ji,jj,jr)
            END DO
         END DO
      END DO


      ! 2. North-Fold boundary conditions
      ! ----------------------------------
      CALL lbc_nfd( ztab_e(:,:), cd_type, psgn, pr2dj = jpr2dj )

      ij = jpr2dj
      !! Scatter back to pt2d
      DO jj = nlcj - ijpj + 1 , nlcj +jpr2dj
      ij  = ij +1
         DO ji= 1, nlci
            pt2d(ji,jj) = ztab_e(ji+nimpp-1,ij)
         END DO
      END DO
      !
   END SUBROUTINE mpp_lbc_north_e


   SUBROUTINE mpi_init_opa( ldtxt, ksft, code )
      !!---------------------------------------------------------------------
      !!                   ***  routine mpp_init.opa  ***
      !!
      !! ** Purpose :: export and attach a MPI buffer for bsend
      !!
      !! ** Method  :: define buffer size in namelist, if 0 no buffer attachment
      !!            but classical mpi_init
      !!
      !! History :: 01/11 :: IDRIS initial version for IBM only
      !!            08/04 :: R. Benshila, generalisation
      !!---------------------------------------------------------------------
      CHARACTER(len=*),DIMENSION(:), INTENT(  out) ::   ldtxt
      INTEGER                      , INTENT(inout) ::   ksft
      INTEGER                      , INTENT(  out) ::   code
      INTEGER                                      ::   ierr, ji
      LOGICAL                                      ::   mpi_was_called
      !!---------------------------------------------------------------------
      !
      CALL mpi_initialized( mpi_was_called, code )      ! MPI initialization
      IF ( code /= MPI_SUCCESS ) THEN
         DO ji = 1, SIZE(ldtxt)
            IF( TRIM(ldtxt(ji)) /= '' )   WRITE(*,*) ldtxt(ji)      ! control print of mynode
         END DO
         WRITE(*, cform_err)
         WRITE(*, *) ' lib_mpp: Error in routine mpi_initialized'
         CALL mpi_abort( mpi_comm_world, code, ierr )
      ENDIF
      !
      IF( .NOT. mpi_was_called ) THEN
         CALL mpi_init( code )
         CALL mpi_comm_dup( mpi_comm_world, mpi_comm_opa, code )
         IF ( code /= MPI_SUCCESS ) THEN
            DO ji = 1, SIZE(ldtxt)
               IF( TRIM(ldtxt(ji)) /= '' )   WRITE(*,*) ldtxt(ji)      ! control print of mynode
            END DO
            WRITE(*, cform_err)
            WRITE(*, *) ' lib_mpp: Error in routine mpi_comm_dup'
            CALL mpi_abort( mpi_comm_world, code, ierr )
         ENDIF
      ENDIF
      !
      IF( nn_buffer > 0 ) THEN
         WRITE(ldtxt(ksft),*) 'mpi_bsend, buffer allocation of  : ', nn_buffer   ;   ksft = ksft + 1
         ! Buffer allocation and attachment
         ALLOCATE( tampon(nn_buffer), stat = ierr )
         IF( ierr /= 0 ) THEN
            DO ji = 1, SIZE(ldtxt)
               IF( TRIM(ldtxt(ji)) /= '' )   WRITE(*,*) ldtxt(ji)      ! control print of mynode
            END DO
            WRITE(*, cform_err)
            WRITE(*, *) ' lib_mpp: Error in ALLOCATE', ierr
            CALL mpi_abort( mpi_comm_world, code, ierr )
         END IF
         CALL mpi_buffer_attach( tampon, nn_buffer, code )
      ENDIF
      !
   END SUBROUTINE mpi_init_opa

   SUBROUTINE DDPDD_MPI (ydda, yddb, ilen, itype)
      !!---------------------------------------------------------------------
      !!   Routine DDPDD_MPI: used by reduction operator MPI_SUMDD
      !!
      !!   Modification of original codes written by David H. Bailey
      !!   This subroutine computes yddb(i) = ydda(i)+yddb(i)
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in)                         :: ilen, itype
      COMPLEX(wp), DIMENSION(ilen), INTENT(in)     :: ydda
      COMPLEX(wp), DIMENSION(ilen), INTENT(inout)  :: yddb
      !
      REAL(wp) :: zerr, zt1, zt2    ! local work variables
      INTEGER :: ji, ztmp           ! local scalar

      ztmp = itype   ! avoid compilation warning

      DO ji=1,ilen
      ! Compute ydda + yddb using Knuth's trick.
         zt1  = real(ydda(ji)) + real(yddb(ji))
         zerr = zt1 - real(ydda(ji))
         zt2  = ((real(yddb(ji)) - zerr) + (real(ydda(ji)) - (zt1 - zerr))) &
                + aimag(ydda(ji)) + aimag(yddb(ji))

         ! The result is zt1 + zt2, after normalization.
         yddb(ji) = cmplx ( zt1 + zt2, zt2 - ((zt1 + zt2) - zt1),wp )
      END DO

   END SUBROUTINE DDPDD_MPI
#endif
#else
#if ! defined in_surfex
   !!----------------------------------------------------------------------
   !!   Default case:            Dummy module        share memory computing
   !!----------------------------------------------------------------------
   USE in_out_manager

   INTERFACE mpp_sum
      MODULE PROCEDURE mpp_sum_a2s, mpp_sum_as, mpp_sum_ai, mpp_sum_s, mpp_sum_i, mppsum_realdd, mppsum_a_realdd
   END INTERFACE
   INTERFACE mpp_max
      MODULE PROCEDURE mppmax_a_int, mppmax_int, mppmax_a_real, mppmax_real
   END INTERFACE
   INTERFACE mpp_min
      MODULE PROCEDURE mppmin_a_int, mppmin_int, mppmin_a_real, mppmin_real
   END INTERFACE
   INTERFACE mppobc
      MODULE PROCEDURE mppobc_1d, mppobc_2d, mppobc_3d, mppobc_4d
   END INTERFACE
   INTERFACE mpp_minloc
      MODULE PROCEDURE mpp_minloc2d ,mpp_minloc3d
   END INTERFACE
   INTERFACE mpp_maxloc
      MODULE PROCEDURE mpp_maxloc2d ,mpp_maxloc3d
   END INTERFACE

   LOGICAL, PUBLIC, PARAMETER ::   lk_mpp = .FALSE.      !: mpp flag
   LOGICAL, PUBLIC            ::   ln_nnogather  = .FALSE.  !: namelist control of northfold comms (needed here in case "key_mpp_mpi" is not used)
   INTEGER :: ncomm_ice
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION lib_mpp_alloc(kumout)          ! Dummy function
      INTEGER, INTENT(in) ::   kumout
      lib_mpp_alloc = 0
   END FUNCTION lib_mpp_alloc

   FUNCTION mynode( ldtxt, kumnam, kstop, localComm ) RESULT (function_value)
      INTEGER, OPTIONAL            , INTENT(in   ) ::   localComm
      CHARACTER(len=*),DIMENSION(:) ::   ldtxt
      INTEGER ::   kumnam, kstop
      IF( PRESENT( localComm ) .OR. .NOT.PRESENT( localComm ) )   function_value = 0
      IF( .FALSE. )   ldtxt(:) = 'never done'
   END FUNCTION mynode

   SUBROUTINE mppsync                       ! Dummy routine
   END SUBROUTINE mppsync

   SUBROUTINE mpp_sum_as( parr, kdim, kcom )      ! Dummy routine
      REAL   , DIMENSION(:) :: parr
      INTEGER               :: kdim
      INTEGER, OPTIONAL     :: kcom
      WRITE(*,*) 'mpp_sum_as: You should not have seen this print! error?', kdim, parr(1), kcom
   END SUBROUTINE mpp_sum_as

   SUBROUTINE mpp_sum_a2s( parr, kdim, kcom )      ! Dummy routine
      REAL   , DIMENSION(:,:) :: parr
      INTEGER               :: kdim
      INTEGER, OPTIONAL     :: kcom
      WRITE(*,*) 'mpp_sum_a2s: You should not have seen this print! error?', kdim, parr(1,1), kcom
   END SUBROUTINE mpp_sum_a2s

   SUBROUTINE mpp_sum_ai( karr, kdim, kcom )      ! Dummy routine
      INTEGER, DIMENSION(:) :: karr
      INTEGER               :: kdim
      INTEGER, OPTIONAL     :: kcom
      WRITE(*,*) 'mpp_sum_ai: You should not have seen this print! error?', kdim, karr(1), kcom
   END SUBROUTINE mpp_sum_ai

   SUBROUTINE mpp_sum_s( psca, kcom )            ! Dummy routine
      REAL                  :: psca
      INTEGER, OPTIONAL     :: kcom
      WRITE(*,*) 'mpp_sum_s: You should not have seen this print! error?', psca, kcom
   END SUBROUTINE mpp_sum_s

   SUBROUTINE mpp_sum_i( kint, kcom )            ! Dummy routine
      integer               :: kint
      INTEGER, OPTIONAL     :: kcom
      WRITE(*,*) 'mpp_sum_i: You should not have seen this print! error?', kint, kcom
   END SUBROUTINE mpp_sum_i

   SUBROUTINE mppsum_realdd( ytab, kcom )
      COMPLEX(wp), INTENT(inout)         :: ytab    ! input scalar
      INTEGER , INTENT( in  ), OPTIONAL :: kcom
      WRITE(*,*) 'mppsum_realdd: You should not have seen this print! error?', ytab
   END SUBROUTINE mppsum_realdd

   SUBROUTINE mppsum_a_realdd( ytab, kdim, kcom )
      INTEGER , INTENT( in )                        ::   kdim      ! size of ytab
      COMPLEX(wp), DIMENSION(kdim), INTENT( inout ) ::   ytab      ! input array
      INTEGER , INTENT( in  ), OPTIONAL :: kcom
      WRITE(*,*) 'mppsum_a_realdd: You should not have seen this print! error?', kdim, ytab(1), kcom
   END SUBROUTINE mppsum_a_realdd

   SUBROUTINE mppmax_a_real( parr, kdim, kcom )
      REAL   , DIMENSION(:) :: parr
      INTEGER               :: kdim
      INTEGER, OPTIONAL     :: kcom
      WRITE(*,*) 'mppmax_a_real: You should not have seen this print! error?', kdim, parr(1), kcom
   END SUBROUTINE mppmax_a_real

   SUBROUTINE mppmax_real( psca, kcom )
      REAL                  :: psca
      INTEGER, OPTIONAL     :: kcom
      WRITE(*,*) 'mppmax_real: You should not have seen this print! error?', psca, kcom
   END SUBROUTINE mppmax_real

   SUBROUTINE mppmin_a_real( parr, kdim, kcom )
      REAL   , DIMENSION(:) :: parr
      INTEGER               :: kdim
      INTEGER, OPTIONAL     :: kcom
      WRITE(*,*) 'mppmin_a_real: You should not have seen this print! error?', kdim, parr(1), kcom
   END SUBROUTINE mppmin_a_real

   SUBROUTINE mppmin_real( psca, kcom )
      REAL                  :: psca
      INTEGER, OPTIONAL     :: kcom
      WRITE(*,*) 'mppmin_real: You should not have seen this print! error?', psca, kcom
   END SUBROUTINE mppmin_real

   SUBROUTINE mppmax_a_int( karr, kdim ,kcom)
      INTEGER, DIMENSION(:) :: karr
      INTEGER               :: kdim
      INTEGER, OPTIONAL     :: kcom
      WRITE(*,*) 'mppmax_a_int: You should not have seen this print! error?', kdim, karr(1), kcom
   END SUBROUTINE mppmax_a_int

   SUBROUTINE mppmax_int( kint, kcom)
      INTEGER               :: kint
      INTEGER, OPTIONAL     :: kcom
      WRITE(*,*) 'mppmax_int: You should not have seen this print! error?', kint, kcom
   END SUBROUTINE mppmax_int

   SUBROUTINE mppmin_a_int( karr, kdim, kcom )
      INTEGER, DIMENSION(:) :: karr
      INTEGER               :: kdim
      INTEGER, OPTIONAL     :: kcom
      WRITE(*,*) 'mppmin_a_int: You should not have seen this print! error?', kdim, karr(1), kcom
   END SUBROUTINE mppmin_a_int

   SUBROUTINE mppmin_int( kint, kcom )
      INTEGER               :: kint
      INTEGER, OPTIONAL     :: kcom
      WRITE(*,*) 'mppmin_int: You should not have seen this print! error?', kint, kcom
   END SUBROUTINE mppmin_int

   SUBROUTINE mppobc_1d( parr, kd1, kd2, kl, kk, ktype, kij, knum )
      INTEGER  ::   kd1, kd2, kl , kk, ktype, kij, knum
      REAL, DIMENSION(:) ::   parr           ! variable array
      WRITE(*,*) 'mppobc: You should not have seen this print! error?', parr(1), kd1, kd2, kl, kk, ktype, kij, knum
   END SUBROUTINE mppobc_1d

   SUBROUTINE mppobc_2d( parr, kd1, kd2, kl, kk, ktype, kij, knum )
      INTEGER  ::   kd1, kd2, kl , kk, ktype, kij, knum
      REAL, DIMENSION(:,:) ::   parr           ! variable array
      WRITE(*,*) 'mppobc: You should not have seen this print! error?', parr(1,1), kd1, kd2, kl, kk, ktype, kij, knum
   END SUBROUTINE mppobc_2d

   SUBROUTINE mppobc_3d( parr, kd1, kd2, kl, kk, ktype, kij, knum )
      INTEGER  ::   kd1, kd2, kl , kk, ktype, kij, knum
      REAL, DIMENSION(:,:,:) ::   parr           ! variable array
      WRITE(*,*) 'mppobc: You should not have seen this print! error?', parr(1,1,1), kd1, kd2, kl, kk, ktype, kij, knum
   END SUBROUTINE mppobc_3d

   SUBROUTINE mppobc_4d( parr, kd1, kd2, kl, kk, ktype, kij, knum )
      INTEGER  ::   kd1, kd2, kl , kk, ktype, kij, knum
      REAL, DIMENSION(:,:,:,:) ::   parr           ! variable array
      WRITE(*,*) 'mppobc: You should not have seen this print! error?', parr(1,1,1,1), kd1, kd2, kl, kk, ktype, kij, knum
   END SUBROUTINE mppobc_4d

   SUBROUTINE mpp_minloc2d( ptab, pmask, pmin, ki, kj )
      REAL                   :: pmin
      REAL , DIMENSION (:,:) :: ptab, pmask
      INTEGER :: ki, kj
      WRITE(*,*) 'mpp_minloc2d: You should not have seen this print! error?', pmin, ki, kj, ptab(1,1), pmask(1,1)
   END SUBROUTINE mpp_minloc2d

   SUBROUTINE mpp_minloc3d( ptab, pmask, pmin, ki, kj, kk )
      REAL                     :: pmin
      REAL , DIMENSION (:,:,:) :: ptab, pmask
      INTEGER :: ki, kj, kk
      WRITE(*,*) 'mpp_minloc3d: You should not have seen this print! error?', pmin, ki, kj, kk, ptab(1,1,1), pmask(1,1,1)
   END SUBROUTINE mpp_minloc3d

   SUBROUTINE mpp_maxloc2d( ptab, pmask, pmax, ki, kj )
      REAL                   :: pmax
      REAL , DIMENSION (:,:) :: ptab, pmask
      INTEGER :: ki, kj
      WRITE(*,*) 'mpp_maxloc2d: You should not have seen this print! error?', pmax, ki, kj, ptab(1,1), pmask(1,1)
   END SUBROUTINE mpp_maxloc2d

   SUBROUTINE mpp_maxloc3d( ptab, pmask, pmax, ki, kj, kk )
      REAL                     :: pmax
      REAL , DIMENSION (:,:,:) :: ptab, pmask
      INTEGER :: ki, kj, kk
      WRITE(*,*) 'mpp_maxloc3d: You should not have seen this print! error?', pmax, ki, kj, kk, ptab(1,1,1), pmask(1,1,1)
   END SUBROUTINE mpp_maxloc3d

   SUBROUTINE mppstop
      WRITE(*,*) 'mppstop: You should not have seen this print if running in mpp mode! error?...'
      WRITE(*,*) 'mppstop: ..otherwise this is a stop condition raised by ctl_stop in single processor mode'
      STOP
   END SUBROUTINE mppstop

   SUBROUTINE mpp_ini_ice( kcom, knum )
      INTEGER :: kcom, knum
      WRITE(*,*) 'mpp_ini_ice: You should not have seen this print! error?', kcom, knum
   END SUBROUTINE mpp_ini_ice

   SUBROUTINE mpp_ini_znl( knum )
      INTEGER :: knum
      WRITE(*,*) 'mpp_ini_znl: You should not have seen this print! error?', knum
   END SUBROUTINE mpp_ini_znl

   SUBROUTINE mpp_comm_free( kcom )
      INTEGER :: kcom
      WRITE(*,*) 'mpp_comm_free: You should not have seen this print! error?', kcom
   END SUBROUTINE mpp_comm_free
#endif
#endif
#if ! defined in_surfex

   !!----------------------------------------------------------------------
   !!   All cases:         ctl_stop, ctl_warn, get_unit, ctl_opn   routines
   !!----------------------------------------------------------------------

   SUBROUTINE ctl_stop( cd1, cd2, cd3, cd4, cd5 ,   &
      &                 cd6, cd7, cd8, cd9, cd10 )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE  stop_opa  ***
      !!
      !! ** Purpose :   print in ocean.outpput file a error message and
      !!                increment the error number (nstop) by one.
      !!----------------------------------------------------------------------
      CHARACTER(len=*), INTENT(in), OPTIONAL ::  cd1, cd2, cd3, cd4, cd5
      CHARACTER(len=*), INTENT(in), OPTIONAL ::  cd6, cd7, cd8, cd9, cd10
      !!----------------------------------------------------------------------
      !
      nstop = nstop + 1
      IF(lwp) THEN
         WRITE(numout,cform_err)
         IF( PRESENT(cd1 ) )   WRITE(numout,*) cd1
         IF( PRESENT(cd2 ) )   WRITE(numout,*) cd2
         IF( PRESENT(cd3 ) )   WRITE(numout,*) cd3
         IF( PRESENT(cd4 ) )   WRITE(numout,*) cd4
         IF( PRESENT(cd5 ) )   WRITE(numout,*) cd5
         IF( PRESENT(cd6 ) )   WRITE(numout,*) cd6
         IF( PRESENT(cd7 ) )   WRITE(numout,*) cd7
         IF( PRESENT(cd8 ) )   WRITE(numout,*) cd8
         IF( PRESENT(cd9 ) )   WRITE(numout,*) cd9
         IF( PRESENT(cd10) )   WRITE(numout,*) cd10
      ENDIF
                               CALL FLUSH(numout    )
      IF( numstp     /= -1 )   CALL FLUSH(numstp    )
      IF( numsol     /= -1 )   CALL FLUSH(numsol    )
      IF( numevo_ice /= -1 )   CALL FLUSH(numevo_ice)
      !
      IF( cd1 == 'STOP' ) THEN
         IF(lwp) WRITE(numout,*)  'huge E-R-R-O-R : immediate stop'
         CALL mppstop()
      ENDIF
      !
   END SUBROUTINE ctl_stop


   SUBROUTINE ctl_warn( cd1, cd2, cd3, cd4, cd5,   &
      &                 cd6, cd7, cd8, cd9, cd10 )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE  stop_warn  ***
      !!
      !! ** Purpose :   print in ocean.outpput file a error message and
      !!                increment the warning number (nwarn) by one.
      !!----------------------------------------------------------------------
      CHARACTER(len=*), INTENT(in), OPTIONAL ::  cd1, cd2, cd3, cd4, cd5
      CHARACTER(len=*), INTENT(in), OPTIONAL ::  cd6, cd7, cd8, cd9, cd10
      !!----------------------------------------------------------------------
      !
      nwarn = nwarn + 1
      IF(lwp) THEN
         WRITE(numout,cform_war)
         IF( PRESENT(cd1 ) ) WRITE(numout,*) cd1
         IF( PRESENT(cd2 ) ) WRITE(numout,*) cd2
         IF( PRESENT(cd3 ) ) WRITE(numout,*) cd3
         IF( PRESENT(cd4 ) ) WRITE(numout,*) cd4
         IF( PRESENT(cd5 ) ) WRITE(numout,*) cd5
         IF( PRESENT(cd6 ) ) WRITE(numout,*) cd6
         IF( PRESENT(cd7 ) ) WRITE(numout,*) cd7
         IF( PRESENT(cd8 ) ) WRITE(numout,*) cd8
         IF( PRESENT(cd9 ) ) WRITE(numout,*) cd9
         IF( PRESENT(cd10) ) WRITE(numout,*) cd10
      ENDIF
      CALL FLUSH(numout)
      !
   END SUBROUTINE ctl_warn


   SUBROUTINE ctl_opn( knum, cdfile, cdstat, cdform, cdacce, klengh, kout, ldwp, karea )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE ctl_opn  ***
      !!
      !! ** Purpose :   Open file and check if required file is available.
      !!
      !! ** Method  :   Fortan open
      !!----------------------------------------------------------------------
      INTEGER          , INTENT(  out) ::   knum      ! logical unit to open
      CHARACTER(len=*) , INTENT(in   ) ::   cdfile    ! file name to open
      CHARACTER(len=*) , INTENT(in   ) ::   cdstat    ! disposition specifier
      CHARACTER(len=*) , INTENT(in   ) ::   cdform    ! formatting specifier
      CHARACTER(len=*) , INTENT(in   ) ::   cdacce    ! access specifier
      INTEGER          , INTENT(in   ) ::   klengh    ! record length
      INTEGER          , INTENT(in   ) ::   kout      ! number of logical units for write
      LOGICAL          , INTENT(in   ) ::   ldwp      ! boolean term for print
      INTEGER, OPTIONAL, INTENT(in   ) ::   karea     ! proc number
      !!
      CHARACTER(len=80) ::   clfile
      INTEGER           ::   iost
      !!----------------------------------------------------------------------

      ! adapt filename
      ! ----------------
      clfile = TRIM(cdfile)
      IF( PRESENT( karea ) ) THEN
         IF( karea > 1 )   WRITE(clfile, "(a,'_',i4.4)") TRIM(clfile), karea-1
      ENDIF
#if defined key_agrif
      IF( .NOT. Agrif_Root() )   clfile = TRIM(Agrif_CFixed())//'_'//TRIM(clfile)
      knum=Agrif_Get_Unit()
#else
      knum=get_unit()
#endif

      iost=0
      IF( cdacce(1:6) == 'DIRECT' )  THEN
         OPEN( UNIT=knum, FILE=clfile, FORM=cdform, ACCESS=cdacce, STATUS=cdstat, RECL=klengh, ERR=100, IOSTAT=iost )
      ELSE
         OPEN( UNIT=knum, FILE=clfile, FORM=cdform, ACCESS=cdacce, STATUS=cdstat             , ERR=100, IOSTAT=iost )
      ENDIF
      IF( iost == 0 ) THEN
         IF(ldwp) THEN
            WRITE(kout,*) '     file   : ', clfile,' open ok'
            WRITE(kout,*) '     unit   = ', knum
            WRITE(kout,*) '     status = ', cdstat
            WRITE(kout,*) '     form   = ', cdform
            WRITE(kout,*) '     access = ', cdacce
            WRITE(kout,*)
         ENDIF
      ENDIF
100   CONTINUE
      IF( iost /= 0 ) THEN
         IF(ldwp) THEN
            WRITE(kout,*)
            WRITE(kout,*) ' ===>>>> : bad opening file: ', clfile
            WRITE(kout,*) ' =======   ===  '
            WRITE(kout,*) '           unit   = ', knum
            WRITE(kout,*) '           status = ', cdstat
            WRITE(kout,*) '           form   = ', cdform
            WRITE(kout,*) '           access = ', cdacce
            WRITE(kout,*) '           iostat = ', iost
            WRITE(kout,*) '           we stop. verify the file '
            WRITE(kout,*)
         ENDIF
         STOP 'ctl_opn bad opening'
      ENDIF

   END SUBROUTINE ctl_opn


   INTEGER FUNCTION get_unit()
      !!----------------------------------------------------------------------
      !!                  ***  FUNCTION  get_unit  ***
      !!
      !! ** Purpose :   return the index of an unused logical unit
      !!----------------------------------------------------------------------
      LOGICAL :: llopn
      !!----------------------------------------------------------------------
      !
      get_unit = 15   ! choose a unit that is big enough then it is not already used in NEMO
      llopn = .TRUE.
      DO WHILE( (get_unit < 998) .AND. llopn )
         get_unit = get_unit + 1
         INQUIRE( unit = get_unit, opened = llopn )
      END DO
      IF( (get_unit == 999) .AND. llopn ) THEN
         CALL ctl_stop( 'get_unit: All logical units until 999 are used...' )
         get_unit = -1
      ENDIF
      !
   END FUNCTION get_unit

   !!----------------------------------------------------------------------
#endif
END MODULE lib_mpp
