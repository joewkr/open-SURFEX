!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ################################################################
      SUBROUTINE GRID_MODIF_CARTESIAN(U,KLUOUT,KLUNAM,KGRID_PAR,KL,PGRID_PAR, &
                                               KGRID_PAR2,KL2,OMODIF,PGRID_PAR2      )  
!     ################################################################
!
!!****  *GRID_MODIF_CARTESIAN* - routine to read in namelist the horizontal grid
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
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004 
!!         M.Moge   06/2015 Initialization of MODD_SPAWN variables
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_SURF_PAR, ONLY : NUNDEF
#ifdef SFX_MNH
USE MODD_SPAWN, ONLY : NDXRATIO,NDYRATIO,NXSIZE,NYSIZE,NXOR,NYOR
#endif

USE MODE_POS_SURF
USE MODE_GRIDTYPE_CARTESIAN
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_REGULAR_GRID_SPAWN
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
INTEGER,                      INTENT(IN)    :: KLUOUT     ! output listing logical unit
INTEGER,                      INTENT(IN)    :: KLUNAM     ! namelist file logical unit
INTEGER,                      INTENT(IN)    :: KL         ! number of points
INTEGER,                      INTENT(IN)    :: KGRID_PAR  ! size of PGRID_PAR
REAL, DIMENSION(KGRID_PAR),   INTENT(IN)    :: PGRID_PAR  ! parameters defining the grid
INTEGER,                      INTENT(INOUT) :: KL2        ! number of points in modified grid
INTEGER,                      INTENT(INOUT) :: KGRID_PAR2 ! size of PGRID_PAR2
LOGICAL,                      INTENT(IN)    :: OMODIF     ! flag to modify the grid
REAL, DIMENSION(KGRID_PAR2),  INTENT(OUT)   :: PGRID_PAR2 ! parameters defining the modified grid
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!* initial grid
REAL                              :: ZLAT0    ! reference latitude
REAL                              :: ZLON0    ! reference longitude
INTEGER                           :: IIMAX1   ! number of points in I direction
INTEGER                           :: IJMAX1   ! number of points in J direction
REAL, DIMENSION(:),   ALLOCATABLE :: ZX1      ! X conformal coordinate of grid mesh
REAL, DIMENSION(:),   ALLOCATABLE :: ZY1      ! Y conformal coordinate of grid mesh
REAL, DIMENSION(:),   ALLOCATABLE :: ZDX1     ! X grid mesh size
REAL, DIMENSION(:),   ALLOCATABLE :: ZDY1     ! Y grid mesh size
!
!* new grid
INTEGER                           :: IIMAX2   ! number of points in I direction
INTEGER                           :: IJMAX2   ! number of points in J direction
REAL, DIMENSION(:),   ALLOCATABLE :: ZX2      ! X conformal coordinate of grid mesh
REAL, DIMENSION(:),   ALLOCATABLE :: ZY2      ! Y conformal coordinate of grid mesh
REAL, DIMENSION(:),   ALLOCATABLE :: ZDX2     ! X grid mesh size
REAL, DIMENSION(:),   ALLOCATABLE :: ZDY2     ! Y grid mesh size
!
!* other variables
LOGICAL :: GFOUND
REAL, DIMENSION(:), POINTER       :: ZGRID_PAR
!
!
!*       0.3   Declarations of namelist
!              ------------------------
!
INTEGER :: IXOR = 1            ! position of modified bottom left point
INTEGER :: IYOR = 1            ! according to initial grid
INTEGER :: IXSIZE = -999       ! number of grid meshes in initial grid to be
INTEGER :: IYSIZE = -999       ! covered by the modified grid
INTEGER :: IDXRATIO = 1        ! resolution ratio between modified grid
INTEGER :: IDYRATIO = 1        ! and initial grid
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
NAMELIST/NAM_INIFILE_CARTESIAN/IXOR,IYOR,IXSIZE,IYSIZE,IDXRATIO,IDYRATIO
!
!------------------------------------------------------------------------------
!
!*       1.    Reading of projection parameters
!              --------------------------------
!
IF (LHOOK) CALL DR_HOOK('GRID_MODIF_CARTESIAN',0,ZHOOK_HANDLE)
 CALL POSNAM(KLUNAM,'NAM_INIFILE_CARTESIAN',GFOUND,KLUOUT)
IF (GFOUND) READ(UNIT=KLUNAM,NML=NAM_INIFILE_CARTESIAN)
!
#ifdef SFX_MNH
! store the parameter in MODD_SPAWN
NXOR = IXOR
NYOR = IYOR
NXSIZE = IXSIZE
NYSIZE = IYSIZE
NDXRATIO = IDXRATIO
NDYRATIO = IDYRATIO
#endif
!
!---------------------------------------------------------------------------
!
!*       2.    All this information stored into pointer PGRID_PAR
!              --------------------------------------------------
!
ALLOCATE(ZX1 (KL))
ALLOCATE(ZY1 (KL))
ALLOCATE(ZDX1(KL))
ALLOCATE(ZDY1(KL))
!
 CALL GET_GRIDTYPE_CARTESIAN(PGRID_PAR,ZLAT0,ZLON0,               &
                              IIMAX1,IJMAX1,                       &
                              ZX1,ZY1,ZDX1,ZDY1                    )  
!
!---------------------------------------------------------------------------
!
!*       3.    Default : no modification
!              -------------------------
!
IF (IXSIZE==-999) IXSIZE=IIMAX1
IF (IYSIZE==-999) IYSIZE=IJMAX1
!
!---------------------------------------------------------------------------
!
!*       4.    Modification of the grid
!              ------------------------
!
!* number of points
!
IIMAX2=IXSIZE*IDXRATIO
IJMAX2=IYSIZE*IDYRATIO
!
KL2 = IIMAX2 * IJMAX2
!
ALLOCATE(ZX2 (IIMAX2*IJMAX2))
ALLOCATE(ZY2 (IIMAX2*IJMAX2))
ALLOCATE(ZDX2(IIMAX2*IJMAX2))
ALLOCATE(ZDY2(IIMAX2*IJMAX2))
!
 CALL REGULAR_GRID_SPAWN(U,KLUOUT,                              &
                          KL, IIMAX1,IJMAX1,ZX1,ZY1,ZDX1,ZDY1,  &
                          IXOR, IYOR, IDXRATIO, IDYRATIO,       &
                          IXSIZE, IYSIZE,                       &
                          KL2, IIMAX2,IJMAX2,ZX2,ZY2,ZDX2,ZDY2  )   
DEALLOCATE(ZX1)
DEALLOCATE(ZY1)
DEALLOCATE(ZDX1)
DEALLOCATE(ZDY1)
!---------------------------------------------------------------------------
!
!*       5.    All this information stored into pointer PGRID_PAR
!              --------------------------------------------------
!
 CALL PUT_GRIDTYPE_CARTESIAN(ZGRID_PAR,ZLAT0,ZLON0,               &
                              IIMAX2,IJMAX2,                       &
                              ZX2,ZY2,ZDX2,ZDY2                    )  
!
!---------------------------------------------------------------------------
DEALLOCATE(ZX2)
DEALLOCATE(ZY2)
DEALLOCATE(ZDX2)
DEALLOCATE(ZDY2)
!---------------------------------------------------------------------------
!
!* 1st call : initializes dimension
!
IF (KGRID_PAR2==0) THEN
  KGRID_PAR2 = SIZE(ZGRID_PAR)
!
ELSE
!
!* 2nd call : initializes grid array
!
  PGRID_PAR2(:) = 0.
  PGRID_PAR2(:) = ZGRID_PAR
END IF
!
DEALLOCATE(ZGRID_PAR)
IF (LHOOK) CALL DR_HOOK('GRID_MODIF_CARTESIAN',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------------
!
END SUBROUTINE GRID_MODIF_CARTESIAN
