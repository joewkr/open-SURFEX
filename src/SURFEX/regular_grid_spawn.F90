!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ################################################################
      SUBROUTINE REGULAR_GRID_SPAWN(U,KLUOUT,                               &
                                      KL1, KIMAX1,KJMAX1,PX1,PY1,PDX1,PDY1, &
                                      KXOR, KYOR, KDXRATIO, KDYRATIO,       &
                                      KXSIZE, KYSIZE,                       &
                                      KL2, KIMAX_C_ll,KJMAX_C_ll,PX2,PY2,PDX2,PDY2  )
!     ################################################################
!
!!****  *REGULAR_GRID_SPAWN* - routine to read in namelist the horizontal grid
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004 
!!        M.Moge    04/2015  Parallelization using routines from MNH/SURCOUCHE
!!        M.Moge    06/2015  bug fix for reproductibility using UPDATE_NHALO1D
!!        M.Moge    01/2016  bug fix for parallel execution with SPLIT2
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_PAR, ONLY : NUNDEF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODI_ABOR1_SFX
#ifdef MNH_PARALLEL
USE MODE_ll
USE MODE_MODELN_HANDLER

USE MODE_SPLITTING_ll, ONLY : SPLIT2, DEF_SPLITTING2
USE MODD_VAR_ll, ONLY : NPROC, IP, YSPLITTING
USE MODD_STRUCTURE_ll, ONLY : ZONE_ll, CRSPD_ll
USE MODD_PARAMETERS, ONLY : JPHEXT
USE MODE_TOOLS_ll, ONLY : INTERSECTION
USE MODE_EXCHANGE_ll, ONLY : SEND_RECV_FIELD
USE MODI_UPDATE_NHALO1D
#endif
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
INTEGER,                      INTENT(IN)    :: KLUOUT     ! output listing logical unit
INTEGER,                      INTENT(IN)    :: KL1        ! total number of points KIMAX1 * KJMAX1
INTEGER,                      INTENT(IN)    :: KIMAX1     ! number of points in x direction
INTEGER,                      INTENT(IN)    :: KJMAX1     ! number of points in y direction
REAL, DIMENSION(KL1),         INTENT(IN)    :: PX1        ! X coordinate of all points
REAL, DIMENSION(KL1),         INTENT(IN)    :: PY1        ! Y coordinate of all points
REAL, DIMENSION(KL1),         INTENT(IN)    :: PDX1       ! X mesh size of all points
REAL, DIMENSION(KL1),         INTENT(IN)    :: PDY1       ! Y mesh size of all points
INTEGER,                      INTENT(IN)    :: KXOR       ! position of modified bottom left point
INTEGER,                      INTENT(IN)    :: KYOR       ! according to initial grid
INTEGER,                      INTENT(IN)    :: KXSIZE     ! number of grid meshes in initial grid to be
INTEGER,                      INTENT(IN)    :: KYSIZE     ! covered by the modified grid
INTEGER,                      INTENT(IN)    :: KDXRATIO   ! resolution ratio between modified grid
INTEGER,                      INTENT(IN)    :: KDYRATIO   ! and initial grid
INTEGER,                      INTENT(IN)    :: KL2        ! total number of points KIMAX_C_ll * KJMAX_C_ll
#ifdef MNH_PARALLEL
INTEGER,                      INTENT(INOUT)    :: KIMAX_C_ll     ! number of points in x direction (glb on entry, lcl on exit)
INTEGER,                      INTENT(INOUT)    :: KJMAX_C_ll     ! number of points in y direction (glb on entry, lcl on exit)
REAL, DIMENSION(:),ALLOCATABLE,         INTENT(OUT)   :: PX2        ! X coordinate of all points
REAL, DIMENSION(:),ALLOCATABLE,         INTENT(OUT)   :: PY2        ! Y coordinate of all points
REAL, DIMENSION(:),ALLOCATABLE,         INTENT(OUT)   :: PDX2       ! X mesh size of all points
REAL, DIMENSION(:),ALLOCATABLE,         INTENT(OUT)   :: PDY2       ! Y mesh size of all points
#else
INTEGER,                      INTENT(IN)    :: KIMAX_C_ll     ! number of points in x direction
INTEGER,                      INTENT(IN)    :: KJMAX_C_ll     ! number of points in y direction
REAL, DIMENSION(KL2),         INTENT(OUT)   :: PX2        ! X coordinate of all points
REAL, DIMENSION(KL2),         INTENT(OUT)   :: PY2        ! Y coordinate of all points
REAL, DIMENSION(KL2),         INTENT(OUT)   :: PDX2       ! X mesh size of all points
REAL, DIMENSION(KL2),         INTENT(OUT)   :: PDY2       ! Y mesh size of all points
#endif
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!* coarse/father grid
!
REAL, DIMENSION(:),     ALLOCATABLE :: ZXM1     ! X coordinate of center of mesh (IIMAX1   points)
REAL, DIMENSION(:),     ALLOCATABLE :: ZYM1     ! Y coordinate of center of mesh (IJMAX1   points)
REAL, DIMENSION(:),     ALLOCATABLE :: ZXHAT1   ! X coordinate of left side      (IIMAX1+1 points)
REAL, DIMENSION(:),     ALLOCATABLE :: ZYHAT1   ! Y coordinate of bottom side    (IJMAX1+1 points)
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZXHAT1_3D, ZYHAT1_3D   ! ZXHAT1 and ZXHAT1 copied in a 3D field for the communications
!
!* fine/son grid
!
REAL, DIMENSION(:),   ALLOCATABLE :: ZXHAT2   ! X coordinate of left side      (IIMAX2 points)
REAL, DIMENSION(:),   ALLOCATABLE :: ZYHAT2   ! Y coordinate of bottom side    (IJMAX2 points)
REAL, DIMENSION(:),   ALLOCATABLE :: ZXHAT2_F_TMP
REAL, DIMENSION(:),   ALLOCATABLE :: ZYHAT2_F_TMP
REAL, DIMENSION(:,:,:),   ALLOCATABLE :: ZXHAT2_F, ZYHAT2_F   ! temporary 3D fields to communicate the values on the father grid to the local son subgrid
!
!* other variables
!
INTEGER     :: JL            ! loop counter
INTEGER     :: JI,JJ         ! loop controls relatively to modified grid
#ifndef MNH_PARALLEL
INTEGER     :: JIBOX,JJBOX   ! grid mesh relatively to initial grid
REAL        :: ZCOEF         ! ponderation coefficient for linear interpolation
#endif
REAL(KIND=JPRB) :: ZHOOK_HANDLE
INTEGER :: IMI
INTEGER :: IINFO_ll
INTEGER :: IXDOMAINS, IYDOMAINS               ! number of subdomains in X and Y directions
LOGICAL :: GPREM                              ! needed for DEF_SPLITTING2, true if NPROC is a prime number
INTEGER :: IXOR_F_ll, IYOR_F_ll               ! origin of local father subdomain in global coordinates
INTEGER :: IXDIM_C, IYDIM_C                   ! size of local son subdomain (in coarse/father grid)
INTEGER :: IXOR_C_ll, IYOR_C_ll               ! origin of local son subdomain (in global fine/son grid)
INTEGER :: IXEND_C_ll, IYEND_C_ll             ! end of local son subdomain (in global fine/son grid)
INTEGER :: IXOR_C_COARSE_ll, IYOR_C_COARSE_ll ! origin of local son subdomain (in global coarse/father grid)
INTEGER :: IIMAX_C     ! number of points in x direction in local portion of son model (in fine grid)
INTEGER :: IJMAX_C     ! number of points in y direction in local portion of son model (in fine grid)
REAL, DIMENSION(KDXRATIO) :: ZCOEFX   ! ponderation coefficients for linear interpolation
REAL, DIMENSION(KDYRATIO) :: ZCOEFY   ! ponderation coefficients for linear interpolation
!
! structures for the partitionning
!
#ifdef MNH_PARALLEL
TYPE(ZONE_ll), DIMENSION(NPROC) :: TZSPLITTING_C  !splitting of child model
TYPE(ZONE_ll), ALLOCATABLE, DIMENSION(:) :: TZCOARSEFATHER ! Coarse father grid splitting
TYPE(ZONE_ll), ALLOCATABLE, DIMENSION(:) :: TZCOARSESONSPLIT   ! coarse son grid intersection with local father subdomain : coordinates in the father grid
!
! structures for the communications
!
TYPE(ZONE_ll), ALLOCATABLE, DIMENSION(:)  :: TZSEND, TZRECV
TYPE(CRSPD_ll), POINTER      :: TZCRSPDSEND, TZCRSPDRECV
TYPE(CRSPD_ll), ALLOCATABLE, DIMENSION(:), TARGET :: TZCRSPDSENDTAB, TZCRSPDRECVTAB
#endif
!
INTEGER :: J
INTEGER :: INBMSG
INTEGER :: ICARD
INTEGER :: ICARDDIF
!
!------------------------------------------------------------------------------
!
!*       1.    Coherence tests
!              ---------------
!
!* tests
!
IF (LHOOK) CALL DR_HOOK('REGULAR_GRID_SPAWN',0,ZHOOK_HANDLE)
#ifdef MNH_PARALLEL
IF ( KXOR+KXSIZE-1 > U%NIMAX_SURF_ll ) THEN
  WRITE(KLUOUT,*) 'spawned domain is not contained in the input domain'
  WRITE(KLUOUT,*) 'IXOR = ', KXOR, ' IXSIZE = ', KXSIZE,&
                    ' with NIMAX(file) = ', U%NIMAX_SURF_ll
  CALL ABOR1_SFX('REGULAR_GRID_SPAWN: (1) SPAWNED DOMAIN NOT CONTAINED IN INPUT DOMAIN')
END IF
IF ( KYOR+KYSIZE-1 > U%NJMAX_SURF_ll ) THEN
  WRITE(KLUOUT,*) 'spawned domain is not contained in the input domain'
  WRITE(KLUOUT,*) 'IYOR = ', KYOR, ' IYSIZE = ', KYSIZE,&
                    ' with NJMAX(file) = ',  U%NJMAX_SURF_ll
  CALL ABOR1_SFX('REGULAR_GRID_SPAWN: (2) SPAWNED DOMAIN NOT CONTAINED IN INPUT DOMAIN')
END IF
#else
IF ( KXOR+KXSIZE-1 > KIMAX1 ) THEN
  WRITE(KLUOUT,*) 'spawned domain is not contained in the input domain'
  WRITE(KLUOUT,*) 'IXOR = ', KXOR, ' IXSIZE = ', KXSIZE,&
                    ' with NIMAX(file) = ', KIMAX1  
  CALL ABOR1_SFX('REGULAR_GRID_SPAWN: (1) SPAWNED DOMAIN NOT CONTAINED IN INPUT DOMAIN')
END IF
IF ( KYOR+KYSIZE-1 > KJMAX1 ) THEN
  WRITE(KLUOUT,*) 'spawned domain is not contained in the input domain'
  WRITE(KLUOUT,*) 'IYOR = ', KYOR, ' IYSIZE = ', KYSIZE,&
                    ' with NJMAX(file) = ', KJMAX1  
  CALL ABOR1_SFX('REGULAR_GRID_SPAWN: (2) SPAWNED DOMAIN NOT CONTAINED IN INPUT DOMAIN')
END IF
#endif

!
!------------------------------------------------------------------------------
!
!*       2.    Partitionning of the son subdomain
!              --------------------------------------------------------------
!
#ifdef MNH_PARALLEL
! get origin of local father subdomain in global coordinates
!
CALL GET_OR_ll( "B", IXOR_F_ll, IYOR_F_ll )
!
! origin of local son subdomain in global father coordinates
!
!IXOR_C_COARSE_ll = MAX( IXOR_F_ll, KXOR+1 )
!IYOR_C_COARSE_ll = MAX( IYOR_F_ll, KYOR+1 )
IXOR_C_COARSE_ll = MAX( IXOR_F_ll-1, KXOR ) ! we have to add one point on the west and south sides -> hence the "- 1"
IYOR_C_COARSE_ll = MAX( IYOR_F_ll-1, KYOR ) ! we have to add one point on the west and south sides -> hence the "- 1"
!
ALLOCATE(TZCOARSEFATHER(NPROC))
ALLOCATE(TZCOARSESONSPLIT(NPROC))
!
! compute father partitioning
!
CALL SPLIT2( U%NIMAX_SURF_ll,  U%NJMAX_SURF_ll, 1, NPROC,TZCOARSEFATHER, YSPLITTING)
! we don't want the halo
DO J = 1, NPROC
  TZCOARSEFATHER(J)%NXOR = TZCOARSEFATHER(J)%NXOR - JPHEXT
  TZCOARSEFATHER(J)%NYOR = TZCOARSEFATHER(J)%NYOR - JPHEXT
  TZCOARSEFATHER(J)%NXEND = TZCOARSEFATHER(J)%NXEND - JPHEXT
  TZCOARSEFATHER(J)%NYEND = TZCOARSEFATHER(J)%NYEND - JPHEXT
ENDDO
!
! partition son domain on father grid (with global coordinates on father grid)
!
! we have to add one point on the west and south sides -> hence the "- 1"
! Warning : we cannot just call SPLIT2(KXSIZE, KYSIZE, 1, NPROC, TZCOARSESONSPLIT, YSPLITTING) as it would not 
!           necessarily split the son domain the same way the father domain was splitted
!           example : if father domain is 30x40 and son domain is 6x5 (in father grid dimensions) then
!                     with NPROC = 2, SPLIT2 will split father domain along Y dimension -> 30x20 local domains
!                     but SPLIT2 will split son domain along X dimension -> 3x5 local domains.
!           therefore we have to use DEF_SPLITTING2 and force the decomposition in the call to SPLIT2
! we want the same domain partitioning for the child domain and for the father domain
CALL DEF_SPLITTING2(IXDOMAINS,IYDOMAINS, U%NIMAX_SURF_ll, U%NJMAX_SURF_ll,NPROC,GPREM)
CALL SPLIT2(KXSIZE, KYSIZE, 1, NPROC, TZCOARSESONSPLIT, YSPLITTING, IXDOMAINS, IYDOMAINS)

! compute the local size of son grid
! KIMAX_C_ll, KJMAX_C_ll are the global sizes of son domain
IIMAX_C = ( TZCOARSESONSPLIT(IP)%NXEND - TZCOARSESONSPLIT(IP)%NXOR + 1 ) * KDXRATIO
IJMAX_C = ( TZCOARSESONSPLIT(IP)%NYEND - TZCOARSESONSPLIT(IP)%NYOR + 1 ) * KDYRATIO
! get the coordinates of the son domain partition on father grid
DO J = 1, NPROC
  TZCOARSESONSPLIT(J)%NXOR = TZCOARSESONSPLIT(J)%NXOR + KXOR - JPHEXT - 1
  TZCOARSESONSPLIT(J)%NYOR = TZCOARSESONSPLIT(J)%NYOR + KYOR - JPHEXT - 1
  TZCOARSESONSPLIT(J)%NXEND = TZCOARSESONSPLIT(J)%NXEND + KXOR - JPHEXT
  TZCOARSESONSPLIT(J)%NYEND = TZCOARSESONSPLIT(J)%NYEND + KYOR - JPHEXT
ENDDO
!
! compute the local size of son grid
! KIMAX_C_ll, KJMAX_C_ll are the global sizes of son domain
!
!CALL SPLIT2 ( KIMAX_C_ll, KJMAX_C_ll, 1, NPROC, TZSPLITTING_C, YSPLITTING )
!IXOR_C_ll = TZSPLITTING_C(IP)%NXOR - JPHEXT
!IXEND_C_ll = TZSPLITTING_C(IP)%NXEND - JPHEXT
!IYOR_C_ll = TZSPLITTING_C(IP)%NYOR - JPHEXT
!IYEND_C_ll = TZSPLITTING_C(IP)%NYEND - JPHEXT
!!
!IIMAX_C = IXEND_C_ll - IXOR_C_ll + 1
!IJMAX_C = IYEND_C_ll - IYOR_C_ll + 1
!IIMAX_C = ( TZCOARSESONSPLIT(IP)%NXEND - TZCOARSESONSPLIT(IP)%NXOR + 1 ) * KDXRATIO
!IJMAX_C = ( TZCOARSESONSPLIT(IP)%NYEND - TZCOARSESONSPLIT(IP)%NYOR + 1 ) * KDYRATIO
!
!------------------------------------------------------------------------------
!
!*       3.    Preparing the structures for the communications for the initialization of son fields using father fields
!              --------------------------------------------------------------
!
  !
  ! ######## initializing the structures for the SEND ########
  !
  ALLOCATE(TZSEND(NPROC))
  CALL INTERSECTION( TZCOARSESONSPLIT, NPROC, TZCOARSEFATHER(IP), TZSEND)
  ! il faut initialiser le TAG de manière a avoir un meme tag unique pour le send et le recv :
  !   on concatene le num du proc qui envoie et le num du proc qui recoit
  DO J = 1, NPROC
    IF ( TZSEND(J)%NUMBER > 0 ) THEN
      IF (TZSEND(J)%NUMBER == 1) THEN
        TZSEND(J)%MSSGTAG = IP * 10 + 1
      ELSE
        TZSEND(J)%MSSGTAG = IP * 10**(CEILING(LOG10(real(TZSEND(J)%NUMBER)))) + TZSEND(J)%NUMBER
      ENDIF
    ENDIF
  ENDDO
  ! switching to local coordinates
  DO J = 1, NPROC
    IF ( TZSEND(J)%NUMBER > 0 ) THEN
       TZSEND(J)%NXOR = TZSEND(J)%NXOR - IXOR_F_ll + 1
       TZSEND(J)%NXEND = TZSEND(J)%NXEND - IXOR_F_ll + 1
       TZSEND(J)%NYOR = TZSEND(J)%NYOR - IYOR_F_ll + 1
       TZSEND(J)%NYEND = TZSEND(J)%NYEND - IYOR_F_ll + 1
    ENDIF
  ENDDO
  ! we do not need the Z dimension
  DO J = 1, NPROC
    IF ( TZSEND(J)%NUMBER > 0 ) THEN
       TZSEND(J)%NZOR = 1
       TZSEND(J)%NZEND = 1
    ENDIF
  ENDDO
  ! switching from an array of CRSPD_ll to a CRSPD_ll pointer
  INBMSG = 0
  DO J = 1, NPROC
    IF ( TZSEND(J)%NUMBER > 0 ) THEN
      INBMSG = INBMSG+1
    ENDIF
  ENDDO
  IF ( INBMSG > 0 ) THEN
    ALLOCATE( TZCRSPDSENDTAB(INBMSG) )
    ICARD = 0
    ICARDDIF = 0
    DO J = 1, NPROC
      IF ( TZSEND(J)%NUMBER > 0 ) THEN
	ICARD = ICARD+1
	IF ( TZSEND(ICARD)%NUMBER /= IP ) THEN
	  ICARDDIF = ICARDDIF+1
	ENDIF
	TZCRSPDSENDTAB(ICARD)%TELT = TZSEND(J)
	IF ( ICARD == INBMSG ) THEN
	  TZCRSPDSENDTAB(ICARD)%TNEXT => NULL()
	ELSE
	  TZCRSPDSENDTAB(ICARD)%TNEXT => TZCRSPDSENDTAB(ICARD+1)
	ENDIF
      ENDIF
    ENDDO
    DO J = 1, ICARD
      TZCRSPDSENDTAB(J)%NCARD = ICARD
      TZCRSPDSENDTAB(J)%NCARDDIF = ICARDDIF
    ENDDO
  ELSE 
    !il faut tout de meme mettre un element de taille 0 dans TZCRSPDSENDTAB
    !sinon SEND_RECV_FIELD plante en 02
    ALLOCATE( TZCRSPDSENDTAB(1) )
    ICARD = 0
    ICARDDIF = 0
    TZCRSPDSENDTAB(1)%TELT = TZSEND(1)
    TZCRSPDSENDTAB(1)%TNEXT => NULL()
    TZCRSPDSENDTAB(1)%NCARD = 0
    TZCRSPDSENDTAB(1)%NCARDDIF = 0
  ENDIF
  IF (ICARD > 0) THEN
    TZCRSPDSEND => TZCRSPDSENDTAB(1)
  ELSE
    TZCRSPDSEND => NULL()
  ENDIF
  !
  ! ######## initializing the structures for the RECV ########
  !
  ALLOCATE(TZRECV(NPROC))
  CALL INTERSECTION( TZCOARSEFATHER, NPROC, TZCOARSESONSPLIT(IP), TZRECV )
  ! il faut initialiser le TAG de manière a avoir un meme tag unique pour le send et le recv :
  !   on concatene le num du proc qui envoie et le num du proc qui recoit
  DO J = 1, NPROC
    IF ( TZRECV(J)%NUMBER > 0 ) THEN
      IF (IP == 1) THEN
        TZRECV(J)%MSSGTAG = TZRECV(J)%NUMBER * 10 + 1
      ELSE
        TZRECV(J)%MSSGTAG = TZRECV(J)%NUMBER * 10**(CEILING(LOG10(real(IP)))) + IP
      ENDIF
    ENDIF
  ENDDO
  ! switching to local coordinates
  DO J = 1, NPROC
    IF ( TZRECV(J)%NUMBER > 0 ) THEN
       TZRECV(J)%NXOR = TZRECV(J)%NXOR - TZCOARSESONSPLIT(IP)%NXOR + 1
       TZRECV(J)%NXEND = TZRECV(J)%NXEND - TZCOARSESONSPLIT(IP)%NXOR + 1
       TZRECV(J)%NYOR = TZRECV(J)%NYOR - TZCOARSESONSPLIT(IP)%NYOR + 1
       TZRECV(J)%NYEND = TZRECV(J)%NYEND - TZCOARSESONSPLIT(IP)%NYOR + 1
    ENDIF
  ENDDO
  ! we do not need the Z dimension
  DO J = 1, NPROC
    IF ( TZRECV(J)%NUMBER > 0 ) THEN
       TZRECV(J)%NZOR = 1
       TZRECV(J)%NZEND = 1
    ENDIF
  ENDDO
  ! switching from an array of CRSPD_ll to a CRSPD_ll pointer
  INBMSG = 0
  DO J = 1, NPROC
    IF ( TZRECV(J)%NUMBER > 0 ) THEN
      INBMSG = INBMSG+1
    ENDIF
  ENDDO
  IF ( INBMSG > 0 ) THEN
    ALLOCATE( TZCRSPDRECVTAB(INBMSG) )
    ICARD = 0
    ICARDDIF = 0
    DO J = 1, NPROC
      IF ( TZRECV(J)%NUMBER > 0 ) THEN
	ICARD = ICARD+1
	IF ( TZRECV(ICARD)%NUMBER /= IP ) THEN
	  ICARDDIF = ICARDDIF+1
	ENDIF
	TZCRSPDRECVTAB(ICARD)%TELT = TZRECV(J)
	IF ( ICARD == INBMSG ) THEN
	  TZCRSPDRECVTAB(ICARD)%TNEXT => NULL()
	ELSE
	  TZCRSPDRECVTAB(ICARD)%TNEXT => TZCRSPDRECVTAB(ICARD+1)
	ENDIF
      ENDIF
    ENDDO
    DO J = 1, ICARD
      TZCRSPDRECVTAB(J)%NCARD = ICARD
      TZCRSPDRECVTAB(J)%NCARDDIF = ICARDDIF
    ENDDO
  ELSE
    !il faut tout de meme mettre un element de taille 0 dans TZCRSPDRECVTAB
    !sinon SEND_RECV_FIELD plante en 02
    ALLOCATE( TZCRSPDRECVTAB(1) )
    ICARD = 0
    ICARDDIF = 0
    TZCRSPDRECVTAB(1)%TELT = TZSEND(1)
    TZCRSPDRECVTAB(1)%TNEXT => NULL()
    TZCRSPDRECVTAB(1)%NCARD = 0
    TZCRSPDRECVTAB(1)%NCARDDIF = 0
  ENDIF
  IF (ICARD > 0) THEN
    TZCRSPDRECV => TZCRSPDRECVTAB(1)
  ELSE
    TZCRSPDRECV => NULL()
  ENDIF
#else
IIMAX_C = KIMAX_C_ll
IJMAX_C = KJMAX_C_ll
#endif
!
!------------------------------------------------------------------------------
!
!*       4.    Center of mesh coordinate arrays for each direction separately
!              --------------------------------------------------------------
!
! allocate the fields on the local son grid
!
#ifdef MNH_PARALLEL
ALLOCATE(PX2(IIMAX_C*IJMAX_C))
ALLOCATE(PY2(IIMAX_C*IJMAX_C))
ALLOCATE(PDX2(IIMAX_C*IJMAX_C))
ALLOCATE(PDY2(IIMAX_C*IJMAX_C))
#endif
ALLOCATE(ZXHAT2(IIMAX_C+1))
ALLOCATE(ZYHAT2(IJMAX_C+1))
!
! allocate the fields on the local father grid
!
ALLOCATE(ZXM1  (KIMAX1))
ALLOCATE(ZYM1  (KJMAX1))
ALLOCATE(ZXHAT1(KIMAX1+1))
ALLOCATE(ZYHAT1(KJMAX1+1))
!
ZXM1(:) = PX1(1:KIMAX1)
DO JL=1,KL1
  IF (MOD(JL,KIMAX1)==0) ZYM1(JL/KIMAX1) = PY1(JL)
END DO
!
!------------------------------------------------------------------------------
!
!*       5.    side of mesh coordinate arrays for each direction separately
!              ------------------------------------------------------------
!
!
IF (KIMAX1==1) THEN
  ZXHAT1(1) = ZXM1(1) - 0.5 * PDX1(1)
  ZXHAT1(2) = ZXM1(1) + 0.5 * PDX1(1)
ELSE
  ZXHAT1(1) = 1.5 * ZXM1(1) - 0.5 * ZXM1(2)
  DO JI=2,KIMAX1
    ZXHAT1(JI) = 0.5 * ZXM1(JI-1) + 0.5 * ZXM1(JI)
  END DO
  ZXHAT1(KIMAX1+1) = 1.5 * ZXM1(KIMAX1) - 0.5 * ZXM1(KIMAX1-1)
END IF
!
IF (KJMAX1==1) THEN
  ZYHAT1(1) = ZYM1(1) - 0.5 * PDY1(1)
  ZYHAT1(2) = ZYM1(1) + 0.5 * PDY1(1)
ELSE
  ZYHAT1(1) = 1.5 * ZYM1(1) - 0.5 * ZYM1(2)
  DO JJ=2,KJMAX1
    ZYHAT1(JJ) = 0.5 * ZYM1(JJ-1) + 0.5 * ZYM1(JJ)
  END DO
  ZYHAT1(KJMAX1+1) = 1.5 * ZYM1(KJMAX1) - 0.5 * ZYM1(KJMAX1-1)
END IF
#ifdef MNH_PARALLEL
  !
  ! do the communication
  !
  IXDIM_C = TZCOARSESONSPLIT(IP)%NXEND-TZCOARSESONSPLIT(IP)%NXOR+1
  IYDIM_C = TZCOARSESONSPLIT(IP)%NYEND-TZCOARSESONSPLIT(IP)%NYOR+1
  ALLOCATE(ZXHAT2_F(IXDIM_C,IYDIM_C,1))
  ALLOCATE(ZYHAT2_F(IXDIM_C,IYDIM_C,1))
  ALLOCATE(ZXHAT1_3D(KIMAX1,KJMAX1,1))
  ALLOCATE(ZYHAT1_3D(KIMAX1,KJMAX1,1))
  ZXHAT1_3D(:,:,:) = 0
  ZYHAT1_3D(:,:,:) = 0
  ZXHAT2_F(:,:,:) = 0
  ZYHAT2_F(:,:,:) = 0
  DO J=1, KJMAX1
    ZXHAT1_3D(:,J,1) = ZXHAT1(1:KIMAX1)
  ENDDO
  DO J=1, KIMAX1
    ZYHAT1_3D(J,:,1) = ZYHAT1(1:KJMAX1)
  ENDDO
  CALL SEND_RECV_FIELD( TZCRSPDSEND, TZCRSPDRECV, ZXHAT1_3D, ZXHAT2_F, IINFO_ll)
  CALL SEND_RECV_FIELD( TZCRSPDSEND, TZCRSPDRECV, ZYHAT1_3D, ZYHAT2_F, IINFO_ll)
!
! We have to copy the entries of ZXHAT1_3D and ZYHAT1_3D that are local to the current process,
! and that are therefore not communicated in SEND_RECV_FIELD, in ZXHAT2_F and ZYHAT2_F
!
  IF ( TZSEND(IP)%NUMBER /= 0 ) THEN  !if there are entries of ZXHAT1_3D and ZYHAT1_3D that are local to the current process
!  DO J=TZSEND(IP)%NXOR-KXOR,TZSEND(IP)%NXEND-KXOR
    ZXHAT2_F( TZRECV(IP)%NXOR:TZRECV(IP)%NXEND, 1, 1) = ZXHAT1_3D( TZSEND(IP)%NXOR:TZSEND(IP)%NXEND, 1, 1)
!  ENDDO
!  DO J=TZSEND(IP)%NYOR-KYOR,TZSEND(IP)%NYEND-KYOR
    ZYHAT2_F( 1,TZRECV(IP)%NYOR:TZRECV(IP)%NYEND, 1) = ZYHAT1_3D( 1,TZSEND(IP)%NYOR:TZSEND(IP)%NYEND, 1)
!  ENDDO
  ENDIF
  ! 
  ! We need one halo point on the east and north sides of each local subdomain to do a proper interpolation
  !
  ALLOCATE( ZXHAT2_F_TMP(IXDIM_C+1) )
  ALLOCATE( ZYHAT2_F_TMP(IYDIM_C+1) )
  ZXHAT2_F_TMP(:) = 0.
  ZYHAT2_F_TMP(:) = 0.
  ZXHAT2_F_TMP(1:IXDIM_C) = ZXHAT2_F(:,1,1)
  ZYHAT2_F_TMP(1:IYDIM_C) = ZYHAT2_F(1,:,1)
! we want the same domain partitioning for the child domain and for the father domain
  CALL DEF_SPLITTING2(IXDOMAINS,IYDOMAINS, U%NIMAX_SURF_ll, U%NJMAX_SURF_ll,NPROC,GPREM)
  CALL SPLIT2(KXSIZE, KYSIZE, 1, NPROC, TZCOARSESONSPLIT, YSPLITTING, IXDOMAINS, IYDOMAINS)
  CALL UPDATE_NHALO1D( 1, ZXHAT2_F_TMP, KXSIZE, KYSIZE,TZCOARSESONSPLIT(IP)%NXOR, &
    TZCOARSESONSPLIT(IP)%NXEND,TZCOARSESONSPLIT(IP)%NYOR,TZCOARSESONSPLIT(IP)%NYEND, 'XX    ')
  CALL UPDATE_NHALO1D( 1, ZYHAT2_F_TMP, KXSIZE, KYSIZE,TZCOARSESONSPLIT(IP)%NXOR, &
    TZCOARSESONSPLIT(IP)%NXEND,TZCOARSESONSPLIT(IP)%NYOR,TZCOARSESONSPLIT(IP)%NYEND, 'YY    ')
#endif
!
!------------------------------------------------------------------------------
!
!*       6.    Interpolation of coordinate arrays for each direction separately
!              ----------------------------------------------------------------
!
!* X coordinate array
!
#ifndef MNH_PARALLEL
DO JI=1,IIMAX_C
  JIBOX=(JI-1)/KDXRATIO + KXOR
  ZCOEF= FLOAT(MOD(JI-1,KDXRATIO))/FLOAT(KDXRATIO)
  ZXHAT2(JI)=(1.-ZCOEF)*ZXHAT1(JIBOX)+ZCOEF*ZXHAT1(JIBOX+1)
END DO
IF (IIMAX_C==1) THEN
  ZXHAT2(IIMAX_C+1) = ZXHAT2(IIMAX_C) + ZXHAT1(JIBOX+1) - ZXHAT1(JIBOX)
ELSE
  ZXHAT2(IIMAX_C+1) = 2. * ZXHAT2(IIMAX_C) - ZXHAT2(IIMAX_C-1)
END IF
#else
DO J=0,KDXRATIO-1
  ZCOEFX(J+1) = FLOAT(J)/FLOAT(KDXRATIO)
ENDDO
DO JI=1,IXDIM_C-1
  DO JJ=1,KDXRATIO
    ZXHAT2((JI-1)*KDXRATIO+JJ)=(1.-ZCOEFX(JJ))*ZXHAT2_F(JI,1,1)+ZCOEFX(JJ)*ZXHAT2_F(JI+1,1,1)
  ENDDO
ENDDO
IF (IIMAX_C==1) THEN
  ZXHAT2(IIMAX_C+1) = ZXHAT2(IIMAX_C) + ZXHAT2_F(JI,1,1) - ZXHAT2_F(JI,1,1)
ELSE
   IF ( LEAST_ll() ) THEN ! the east halo point does not have a correct value so have to do an extrapolation
      ZXHAT2(IIMAX_C+1) = 2. * ZXHAT2(IIMAX_C) - ZXHAT2(IIMAX_C-1)
   ELSE
      ZXHAT2(IIMAX_C+1)=(1.-ZCOEFX(1))*ZXHAT2_F_TMP(IXDIM_C)+ZCOEFX(1)*ZXHAT2_F_TMP(IXDIM_C+1)
   ENDIF
END IF
#endif
!
!* Y coordinate array
!
#ifndef MNH_PARALLEL
DO JJ=1,IJMAX_C
  JJBOX=(JJ-1)/KDYRATIO + KYOR
  ZCOEF= FLOAT(MOD(JJ-1,KDYRATIO))/FLOAT(KDYRATIO)
  ZYHAT2(JJ)=(1.-ZCOEF)*ZYHAT1(JJBOX)+ZCOEF*ZYHAT1(JJBOX+1)
END DO
IF (IJMAX_C==1) THEN
  ZYHAT2(IJMAX_C+1) = ZYHAT2(IJMAX_C) + ZYHAT1(JJBOX+1) - ZYHAT1(JJBOX)
ELSE
  ZYHAT2(IJMAX_C+1) = 2. * ZYHAT2(IJMAX_C) - ZYHAT2(IJMAX_C-1)
END IF
#else
DO J=0,KDYRATIO-1
  ZCOEFY(J+1) = FLOAT(J)/FLOAT(KDYRATIO)
ENDDO
DO JI=1,IYDIM_C-1
  DO JJ=1,KDYRATIO
    ZYHAT2((JI-1)*KDYRATIO+JJ)=(1.-ZCOEFY(JJ))*ZYHAT2_F(1,JI,1)+ZCOEFY(JJ)*ZYHAT2_F(1,JI+1,1)
  ENDDO
ENDDO
IF (IJMAX_C==1) THEN
  ZYHAT2(IJMAX_C+1) = ZYHAT2(IJMAX_C) + ZYHAT2_F(1,JI,1) - ZYHAT2_F(1,JI,1)
ELSE
  IF ( LNORTH_ll() ) THEN ! the east halo point does not have a correct value so have to do an extrapolation
    ZYHAT2(IJMAX_C+1) = 2. * ZYHAT2(IJMAX_C) - ZYHAT2(IJMAX_C-1)
  ELSE
    ZYHAT2(IJMAX_C+1)=(1.-ZCOEFY(1))*ZYHAT2_F_TMP(IYDIM_C)+ZCOEFY(1)*ZYHAT2_F_TMP(IYDIM_C+1)
  ENDIF
END IF
#endif
!---------------------------------------------------------------------------
DEALLOCATE(ZXM1)
DEALLOCATE(ZYM1)
DEALLOCATE(ZXHAT1)
DEALLOCATE(ZYHAT1)
#ifdef MNH_PARALLEL
DEALLOCATE(ZXHAT1_3D)
DEALLOCATE(ZYHAT1_3D)
#endif
!------------------------------------------------------------------------------
!
!*       7.    Coordinate arrays of all points
!              -------------------------------
!
DO JJ=1,IJMAX_C
  DO JI=1,IIMAX_C
    JL = (JJ-1) * IIMAX_C + JI
      PX2 (JL) = 0.5 * ZXHAT2(JI) + 0.5 * ZXHAT2(JI+1)
      PDX2(JL) = ZXHAT2(JI+1) - ZXHAT2(JI)
      PY2 (JL) = 0.5 * ZYHAT2(JJ) + 0.5 * ZYHAT2(JJ+1)
      PDY2(JL) = ZYHAT2(JJ+1) - ZYHAT2(JJ)
  END DO
END DO
!
#ifdef MNH_PARALLEL
KIMAX_C_ll = IIMAX_C
KJMAX_C_ll = IJMAX_C
#endif
!---------------------------------------------------------------------------
#ifdef MNH_PARALLEL
DEALLOCATE(ZXHAT2_F)
DEALLOCATE(ZYHAT2_F)
DEALLOCATE(TZCRSPDSENDTAB)
DEALLOCATE(TZCRSPDRECVTAB)
DEALLOCATE(TZSEND)
DEALLOCATE(TZRECV)
DEALLOCATE(TZCOARSEFATHER)
DEALLOCATE(TZCOARSESONSPLIT)
#endif
DEALLOCATE(ZXHAT2)
DEALLOCATE(ZYHAT2)
IF (LHOOK) CALL DR_HOOK('REGULAR_GRID_SPAWN',1,ZHOOK_HANDLE)
!---------------------------------------------------------------------------
!
END SUBROUTINE REGULAR_GRID_SPAWN
