!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #################################################################
      SUBROUTINE WRITE_GRIDTYPE_IGN (HSELECT,HPROGRAM,KLU,KGRID_PAR,PGRID_PAR,KRESP)
!     #################################################################
!
!!****  *WRITE_GRIDTYPE_IGN* - routine to write the horizontal grid
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
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODI_WRITE_SURF
!
USE MODE_GRIDTYPE_IGN
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: HSELECT
!
 CHARACTER(LEN=6),           INTENT(IN)  :: HPROGRAM   ! calling program
INTEGER,                    INTENT(IN)  :: KLU        ! number of points
INTEGER,                    INTENT(IN)  :: KGRID_PAR  ! size of PGRID_PAR
REAL, DIMENSION(KGRID_PAR), INTENT(IN)  :: PGRID_PAR  ! parameters defining this grid
INTEGER,                    INTENT(OUT) :: KRESP      ! error return code
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER                           :: ILAMBERT ! Lambert type
REAL, DIMENSION(:), ALLOCATABLE   :: ZX       ! X conformal coordinate of grid mesh (dim IIMAX)
REAL, DIMENSION(:), ALLOCATABLE   :: ZY       ! Y conformal coordinate of grid mesh (dim IJMAX)
REAL, DIMENSION(:), ALLOCATABLE   :: ZDX      ! X grid mesh size (dim IIMAX)
REAL, DIMENSION(:), ALLOCATABLE   :: ZDY      ! Y grid mesh size (dim IJMAX)
!
 CHARACTER(LEN=100)                :: YCOMMENT ! comment written in the file
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!---------------------------------------------------------------------------
!
!*       1.    Projection and 2D grid parameters
!              ---------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_GRIDTYPE_IGN',0,ZHOOK_HANDLE)
ALLOCATE(ZX (KLU))
ALLOCATE(ZY (KLU))
ALLOCATE(ZDX(KLU))
ALLOCATE(ZDY(KLU))
!
 CALL GET_GRIDTYPE_IGN(PGRID_PAR,KLAMBERT=ILAMBERT, &
                        PX=ZX,PY=ZY,PDX=ZDX,PDY=ZDY  )  
!
!---------------------------------------------------------------------------
!

!*       2.    Writing of the grid definition parameters
!              -----------------------------------------
!
SELECT CASE (ILAMBERT)
     CASE (1)
     YCOMMENT='LAMBERT 1'
     CASE (2)
     YCOMMENT='LAMBERT 2'
     CASE (3)
     YCOMMENT='LAMBERT 3'
     CASE (4)
     YCOMMENT='LAMBERT 4'
     CASE (5)
     YCOMMENT='LAMBERT 2 ETENDU'
     CASE (6)
     YCOMMENT='LAMBERT 93'
END SELECT
 CALL WRITE_SURF(HSELECT,HPROGRAM,'LAMBERT ',ILAMBERT, KRESP,YCOMMENT)
!
YCOMMENT='XX'
 CALL WRITE_SURF(HSELECT,HPROGRAM,'XX',ZX,KRESP,YCOMMENT)
!
YCOMMENT='XY'
 CALL WRITE_SURF(HSELECT,HPROGRAM,'XY',ZY,KRESP,YCOMMENT)
!
YCOMMENT='XDX'
 CALL WRITE_SURF(HSELECT,HPROGRAM,'DX',ZDX,KRESP,YCOMMENT)
!
YCOMMENT='XDY'
 CALL WRITE_SURF(HSELECT,HPROGRAM,'DY',ZDY,KRESP,YCOMMENT)
!
!---------------------------------------------------------------------------
DEALLOCATE(ZX)
DEALLOCATE(ZY)
DEALLOCATE(ZDX)
DEALLOCATE(ZDY)
IF (LHOOK) CALL DR_HOOK('WRITE_GRIDTYPE_IGN',1,ZHOOK_HANDLE)
!---------------------------------------------------------------------------
!
END SUBROUTINE WRITE_GRIDTYPE_IGN
