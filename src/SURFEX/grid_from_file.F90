!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GRID_FROM_FILE (U,GCP,PGRID_FULL_PAR,HPROGRAM,HFILE,HFILETYPE,&
                                 OGRID,HGRID,KGRID_PAR,PGRID_PAR,KL,HDIR)
!     ##########################################################
!!
!!    PURPOSE
!!    -------
!!   Reads in namelist the grid type and parameters.
!!
!!    METHOD
!!    ------
!!   
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson                   Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original     01/2004
!!      M.Moge     02/2015  Parallelization of spawning
!!      M.Moge     04/2015  Parallelization of prep_pgd on son model
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NSIZE_TASK
!
#ifdef MNH_PARALLEL
USE MODE_TOOLS_ll, ONLY : GET_DIM_PHYS_ll
#endif
!
USE MODI_READ_NAM_GRIDTYPE
USE MODI_OPEN_AUX_IO_SURF
USE MODI_READ_GRIDTYPE
USE MODI_CLOSE_AUX_IO_SURF
USE MODI_READ_SURF
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_GRID_MODIF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_LUOUT
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
REAL, DIMENSION(:), POINTER :: PGRID_FULL_PAR
!
 CHARACTER(LEN=6),  INTENT(IN)   :: HPROGRAM   ! program calling the surface
 CHARACTER(LEN=28), INTENT(IN)   :: HFILE      ! file name
 CHARACTER(LEN=6),  INTENT(IN)   :: HFILETYPE  ! file type
LOGICAL,           INTENT(IN)   :: OGRID      ! .true. if grid is imposed by atm. model
 CHARACTER(LEN=10), INTENT(OUT)  :: HGRID      ! type of horizontal grid
INTEGER,           INTENT(OUT)  :: KGRID_PAR  ! size of PGRID_PAR
REAL, DIMENSION(:), POINTER     :: PGRID_PAR  ! parameters defining this grid
INTEGER,           INTENT(OUT)  :: KL         ! number of points on processor
 CHARACTER(LEN=1), INTENT(IN) :: HDIR
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER           :: IIMAX
INTEGER           :: IJMAX
INTEGER           :: IIMAX_LOC
INTEGER           :: IJMAX_LOC
INTEGER           :: ILUOUT ! listing  file  logical unit
INTEGER           :: ILUNAM ! namelist file  logical unit
INTEGER           :: IRESP  ! return code
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*    0.3    Declaration of namelists
!            ------------------------
!
!------------------------------------------------------------------------------
!
!*       1.    Defaults
!              --------
!
IF (LHOOK) CALL DR_HOOK('GRID_FROM_FILE',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!---------------------------------------------------------------------------
!
!*       2.    Opening of the file
!              -------------------
!
!---------------------------------------------------------------------------
!
!*       3.    Number of points in this file
!              -----------------------------
!
IF (HDIR/='H') THEN
  !
  CALL OPEN_AUX_IO_SURF(HFILE,HFILETYPE,'FULL  ',HDIR=HDIR)
  !  
  CALL READ_SURF(HFILETYPE,'DIM_FULL  ',KL,IRESP,HDIR=HDIR)
  U%NDIM_FULL = KL
  !
  !---------------------------------------------------------------------------
  !
  !*       4.    Grid type
  !              ---------
  !
   CALL READ_SURF(HFILETYPE,'GRID_TYPE',HGRID,IRESP,HDIR=HDIR)
  !
  !---------------------------------------------------------------------------
  !
  !*       5.    Reading parameters of the grid
  !              ------------------------------
  !
#ifdef MNH_PARALLEL
  CALL READ_SURF(HPROGRAM,'IMAX ',IIMAX, IRESP,HDIR='H')
  CALL READ_SURF(HPROGRAM,'JMAX ',IJMAX, IRESP,HDIR='H')
  U%NIMAX_SURF_ll = IIMAX
  U%NJMAX_SURF_ll = IJMAX
  CALL GET_DIM_PHYS_ll('B',IIMAX_LOC,IJMAX_LOC)
  U%NSIZE_FULL = IIMAX_LOC*IJMAX_LOC
  KL = U%NSIZE_FULL
  CALL READ_GRIDTYPE(HFILETYPE,HGRID,KGRID_PAR,U%NSIZE_FULL,.FALSE.,HDIR='H')
  !
  ALLOCATE(PGRID_PAR(KGRID_PAR))
  CALL READ_GRIDTYPE(HFILETYPE,HGRID,KGRID_PAR,U%NSIZE_FULL,.TRUE.,PGRID_PAR,IRESP,HDIR='H')
#else  
  CALL READ_GRIDTYPE(HFILETYPE,HGRID,KGRID_PAR,KL,.FALSE.,HDIR=HDIR)
  !
  ALLOCATE(PGRID_PAR(KGRID_PAR))
   CALL READ_GRIDTYPE(HFILETYPE,HGRID,KGRID_PAR,KL,.TRUE.,PGRID_PAR,IRESP,HDIR=HDIR)
#endif
  !
  !---------------------------------------------------------------------------
  !
  !*       6.    Closes the file
  !              ---------------
  !
  CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
  !
  !------------------------------------------------------------------------------
  !
  !*       7.    Open namelist
  !              -------------
  !
  CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
  !
  !------------------------------------------------------------------------------
  !
  !*       8.    Grid modification
  !              -----------------
  !
  IF (.NOT. OGRID) CALL GRID_MODIF(U,ILUOUT,ILUNAM,HGRID,KGRID_PAR,PGRID_PAR,KL)
  !
  CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
  !
ELSE
  !
  CALL READ_NAM_GRIDTYPE(GCP,PGRID_FULL_PAR,U%NDIM_FULL,HPROGRAM,HGRID,KGRID_PAR,PGRID_PAR,KL,HDIR)
  !
ENDIF
  !
  !------------------------------------------------------------------------------
  !
!*       9.    Close namelist
!              --------------
!
IF (LHOOK) CALL DR_HOOK('GRID_FROM_FILE',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GRID_FROM_FILE
