!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##############################################################
      SUBROUTINE PACK_GRID_IGN(KMASK_SIZE,KMASK,KGRID_PAR1,PGRID_PAR1,KGRID_PAR2,OPACK,PGRID_PAR2)
!     ##############################################################
!
!!**** *PACK_GRID_IGN* packs the grid definition vector
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    E. Martin         Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    10/2007
!!      07/2011     add maximum domain dimension for output (B. Decharme)
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURFEX_MPI, ONLY : NRANK
!
USE MODI_PACK_SAME_RANK
USE MODE_GRIDTYPE_IGN
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
INTEGER,                        INTENT(IN)    :: KMASK_SIZE ! size of mask
INTEGER, DIMENSION(KMASK_SIZE), INTENT(IN)    :: KMASK      ! mask used
INTEGER,                        INTENT(IN)    :: KGRID_PAR1 ! size of input grid vector
REAL,    DIMENSION(KGRID_PAR1), INTENT(IN)    :: PGRID_PAR1 ! parameters of input grid
INTEGER,                        INTENT(INOUT) :: KGRID_PAR2 ! size of output grid vector
LOGICAL,                        INTENT(IN)    :: OPACK      ! flag to pack the grid vector
REAL,    DIMENSION(KGRID_PAR2), INTENT(OUT)   :: PGRID_PAR2 ! parameters of output grid
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
INTEGER                           :: ILAMBERT ! Lambert type
REAL, DIMENSION(:), ALLOCATABLE   :: ZX1      ! X conformal coordinate of grid mesh
REAL, DIMENSION(:), ALLOCATABLE   :: ZY1      ! Y conformal coordinate of grid mesh
REAL, DIMENSION(:), ALLOCATABLE   :: ZDX1     ! X grid mesh size
REAL, DIMENSION(:), ALLOCATABLE   :: ZDY1     ! Y grid mesh size
REAL, DIMENSION(:), ALLOCATABLE   :: ZX2      ! X conformal coordinate of grid mesh
REAL, DIMENSION(:), ALLOCATABLE   :: ZY2      ! Y conformal coordinate of grid mesh
REAL, DIMENSION(:), ALLOCATABLE   :: ZDX2     ! X grid mesh size
REAL, DIMENSION(:), ALLOCATABLE   :: ZDY2     ! Y grid mesh size
!
REAL, DIMENSION(:), ALLOCATABLE   :: ZXALL    ! maximum domain X coordinate of grid mesh
REAL, DIMENSION(:), ALLOCATABLE   :: ZYALL    ! maximum domain Y coordinate of grid mesh
INTEGER                           :: IDIMX    ! maximum domain length in X
INTEGER                           :: IDIMY    ! maximum domain length in Y
!
INTEGER                           :: IL1        ! number of points of input grid
REAL, DIMENSION(:), POINTER       :: ZGRID_PAR2 ! parameters of output grid
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
!*    2.     Computes grid parameters
!            ------------------------
!
IF (LHOOK) CALL DR_HOOK('PACK_GRID_IGN',0,ZHOOK_HANDLE)
 CALL GET_GRIDTYPE_IGN(PGRID_PAR1,KLAMBERT=ILAMBERT,KL=IL1)
!
ALLOCATE(ZX1 (IL1))
ALLOCATE(ZY1 (IL1))
ALLOCATE(ZDX1(IL1))
ALLOCATE(ZDY1(IL1))
!
 CALL GET_GRIDTYPE_IGN(PGRID_PAR1,PX=ZX1,PY=ZY1,PDX=ZDX1,PDY=ZDY1)
!
ALLOCATE(ZX2 (KMASK_SIZE))
ALLOCATE(ZY2 (KMASK_SIZE))
ALLOCATE(ZDX2(KMASK_SIZE))
ALLOCATE(ZDY2(KMASK_SIZE))
ALLOCATE(ZXALL(0))
ALLOCATE(ZYALL(0))
!
 CALL PACK_SAME_RANK(KMASK,ZX1 ,ZX2 )
 CALL PACK_SAME_RANK(KMASK,ZY1 ,ZY2 )
 CALL PACK_SAME_RANK(KMASK,ZDX1,ZDX2)
 CALL PACK_SAME_RANK(KMASK,ZDY1,ZDY2)
!
DEALLOCATE(ZX1 )
DEALLOCATE(ZY1 )
DEALLOCATE(ZDX1)
DEALLOCATE(ZDY1)
!
 CALL PUT_GRIDTYPE_IGN(ZGRID_PAR2,ILAMBERT,ZX2,ZY2,ZDX2,ZDY2,0,0,ZXALL,ZYALL)
!
DEALLOCATE(ZXALL)
DEALLOCATE(ZYALL)
!
IF (OPACK) THEN
  PGRID_PAR2(:) = ZGRID_PAR2(:)
ELSE
  KGRID_PAR2    = SIZE(ZGRID_PAR2(:))
END IF
!
DEALLOCATE(ZGRID_PAR2)
DEALLOCATE(ZX2 )
DEALLOCATE(ZY2 )
DEALLOCATE(ZDX2)
DEALLOCATE(ZDY2)
IF (LHOOK) CALL DR_HOOK('PACK_GRID_IGN',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE PACK_GRID_IGN
